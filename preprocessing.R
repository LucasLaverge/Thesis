library(readr)
library(tidyverse)
library(sqldf)
library(tidymodels)
library(lubridate)
library(dplyr)



library(magrittr)

library(doParallel)
library(MLmetrics)
library(gbm)
library(PerformanceAnalytics)
library(kableExtra)
library(knitr)
library(monomvn)
library(kableExtra)
library(nnet)
library(TAF)
library(edgarWebR)

setwd("~/Desktop/NHH/BIG DATA WITH APPLICATIONS TO FINANCE/FINAL PROJECT/R")

############################################################################
########################## LOAD DATA #######################################
############################################################################
# Load and read data -------------------------------------------------------
# Compustat
compustat <- read_csv("compustat2.csv")

# Crsp_monthly
crsp <- read_csv("crsp-monthly.csv")

# Company_names
company.names.df <- read.csv(file = "names.csv") %>% 
  rename_with(tolower) %>% 
  mutate(date = lubridate::ymd(date))

# Feature_names
feature.names.df <- read.delim(file = "compustat-fields.txt") %>% 
  rename_with(tolower) 

# Industry returns
industry.returns <- read_csv("17_Industry_Portfolios_Daily.csv", skip = 9) %>% 
  rename_with(tolower) %>% 
  drop_na() %>% 
  mutate(
    "date" = lubridate::ymd(...1)
  ) %>% 
  filter(date >= "2011-01-01") %>% 
  mutate(across(.cols = c(2:18), .fns = ~.x / 100)) # divide by 100 to obtain result (instead of percentage)


############################################################################
############# MERGE CRISP AND COMPUSTAT DATASET ############################
############################################################################
crsp <- crsp[ !(crsp$RET %in% c("", "B", "C")), ]
crsp$PRC <- abs(crsp$PRC) #compute absolute value of PRC
crsp$RET <- as.numeric(crsp$RET)
crsp$RETX <- as.numeric(crsp$RETX)

to.year <- function (x) {
  x %/% 10000 - 1900
}
to.month <- function (x) {
  (x %/% 100) %% 100 - 1  
}
to.yearmon <- function (x) {
  to.year(x) * 12 + to.month(x)
}
names(compustat)[ names(compustat) == "LPERMNO" ] = "PERMNO"
dups <- duplicated(compustat[,1:3])
compustat <- compustat[!dups,]
crsp$yearmon <- to.yearmon(crsp$date)
compustat$yearmon <- to.yearmon(compustat$datadate)
crsp$MARKETCAP <- crsp$PRC * crsp$SHROUT
compustat = merge(compustat, crsp[c("PERMNO", "yearmon", "MARKETCAP")], 
                  by=c("PERMNO", "yearmon"), all.x = TRUE)
crsp$MARKETCAP <- NULL
## rename
names(compustat)[ names(compustat) == "PERMNO" ] = "PERMNO2"
names(compustat)[ names(compustat) == "yearmon" ] = "yearmon2"
merged <- sqldf("select * from crsp, compustat where crsp.PERMNO = 
compustat.PERMNO2 and crsp.yearmon - compustat.yearmon2 between 4 and 6 order by 
PERMNO, date") 

remove(to.month, to.year, to.yearmon, dups)
merged$yearmon <- NULL
merged$yearmon2 <- NULL
merged$PERMNO2 <- NULL

merged %<>% rename_with(tolower) %>% 
  mutate(date = lubridate::ymd(date))


############################################################################
########################## Data Cleaning ###################################
############################################################################
# Feature selection functions ----------------------------------------------
remove_cols_only_zero_and_NA <- function(df, print_removed_cols = F) {
  
  #' @description              Function that removes columns containing only 
  #'                           zeros and NAs
  #'
  #' @param df                 Passing a data frame
  #' @param print_removed_cols True if user want to print removed columns
  #' @return                   Data frame without columns that containing only 
  #'                           zeros and NAs
  
  cols <- df %>% 
    apply(MARGIN = 2, 
          function(x) (sum(x==0, na.rm = T) + sum(is.na(x)))/length(x))
  
  cols <- cols[cols == 1] %>% 
    as.data.frame() %>% 
    rownames() 
  
  if(print_removed_cols) cat("Columns removed: ", cols, "\n\n")
  
  return (df %>% dplyr::select(-cols))
}

remove_NA <- function(df, ratio, print_removed_cols = F){
  
  #' @description              Function that removes columns containing NAs 
  #'                           beyond a given ratio
  #'             
  #' @param df                 Passing a data frame
  #' @param ratio              Passing a upper limit NA ratio
  #' @param print_removed_cols True if user want to print removed columns
  #' @return                   Data frame without columns containing NAs 
  #'                           beyond given ratio
  
  cols <- df %>% 
    apply(MARGIN = 2, 
          function(x) sum(is.na(x))/length(x))
  
  cols <- cols[cols >= ratio] %>% 
    as.data.frame() %>% 
    rownames() 
  
  if(print_removed_cols) cat("Columns removed: ", cols, "\n\n")
  
  return(df %>% dplyr::select(-cols))
}

remove_nzv <- function(df, print_removed_cols = F){
  
  #' @description              Function that removes near zero variance 
  #'                           columns
  #'             
  #' @param df                 Passing a data frame
  #' @param print_removed_cols True if user want to print removed columns
  #' @return                   Data frame without columns near zero variance 
  #'                           columns
  
  rec <- recipe(retx ~ ., 
                data = df)
  
  cols <- (rec %>% 
             step_nzv(all_predictors()) %>% 
             prep(df) %>% 
             tidy(number = 1))$terms
  
  if(print_removed_cols) cat("Columns removed: ", cols, "\n\n")
  
  return(df %>% dplyr::select(-cols))
  
}

remove_NA_rows <- function(df) {
  #' @description Function that removes any rows with one or more NA's
  #'
  #' @param df    Passing a data frame
  #' @return      Data frame NA filtered rows
  
  return(df %>% filter(across(everything(), ~ !is.na(.x))) )
  
}

# Applying preprocessing steps
## Apply variance and correlation filter
merged %<>% remove_cols_only_zero_and_NA(print_removed_cols = T) %>% 
  remove_NA(0.2, print_removed_cols = T) %>% 
  remove_nzv(print_removed_cols = T) %>% 
  remove_NA_rows()  

# Irrelevant features ----------------------------------------------------------
# Variables that cannot be included with dependent variable RETX
excluded.variables <- c("ret",
                        "costat",
                        "datafqtr",# Remove all date related variables
                        "fyearq",
                        "datacqtr",
                        "datadate",
                        "gvkey", # Company identifier
                        "fyr",
                        "fqtr",
                        "datacqtr") 
merged <- merged %>% 
  dplyr::select(-excluded.variables)

remove(remove_cols_only_zero_and_NA, remove_NA, remove_NA_rows, remove_nzv, excluded.variables)


############################################################################
########################## Preprocessing ###################################
############################################################################
# Make a df containing all company codes in our company_names.df
unique.company.df <- company.names.df[c("permno", "hsiccd", "comnam")]
dups <- duplicated(unique.company.df[,1:2]) # exclude comnam bcs this changes over time for some companies
unique.company.df <- unique.company.df[!dups,]
remove(dups)

# Add industry code (SIC) to our merged database 
merged.industry <- merge(merged, unique.company.df[c("permno", "hsiccd")], 
                         by="permno") %>% 
  mutate (
    year = year(date),
    month = month(date)
  ) %>% 
  drop_na(hsiccd) %>% # Remove companies without industry code 
  transform(hsiccd = as.numeric(hsiccd))
# Rename hsiccd to sic
names(merged.industry)[ names(merged.industry) == "hsiccd" ] = "sic"

# Add industry names to df
merged.industry <- merged.industry %>%
  mutate(industry = case_when(sic >= 1 & sic <= 999 ~ "Agriculture",
                              sic >= 1000 & sic <= 1499 ~ "Mining",
                              sic >= 1500 & sic <= 1799 ~ "Construction",
                              sic >= 2000 & sic <= 3999 ~ "Manufacturing",
                              sic >= 4000 & sic <= 4999 ~ "Transportation",
                              sic >= 5000 & sic <= 5199 ~ "Wholesale",
                              sic >= 5200 & sic <= 5999 ~ "Retail",
                              sic >= 6000 & sic <= 6799 ~ "Finance",
                              sic >= 7000 & sic <= 8999 ~ "Services",
                              sic >= 9000 & sic <= 9999 ~ "Public",
                              TRUE ~ "Missing"))

#calculate monthly returns per industry
industry.returns.monthly <- industry.returns %>% 
  group_by(year = year(date), month = month(date)) %>% 
  summarise(
    food = prod(food + 1)-1,
    mines = prod(mines + 1)-1,
    oil = prod(oil + 1)-1,
    clths = prod(clths + 1)-1,
    durbl = prod(durbl + 1)-1,
    chems = prod(chems + 1)-1,
    cnsum = prod(cnsum + 1)-1,
    cnstr = prod(cnstr + 1)-1,
    steel = prod(steel + 1)-1,
    fabpr = prod(fabpr + 1)-1,
    machn = prod(machn + 1)-1,
    cars = prod(cars + 1)-1,
    trans = prod(trans + 1)-1,
    utils = prod(utils + 1)-1,
    rtail = prod(rtail + 1)-1,
    finan = prod(finan + 1)-1,
    other = prod(other + 1)-1,
  )
remove(industry.returns)

# Create finance df 
finance <- merged.industry %>% 
  filter(industry == "Finance") %>% 
  select(-industry)

finance <- merge(finance, industry.returns.monthly[c("year","month", "finan")],
                 by=c("year", "month")) %>% 
  select(-c(year, month, sic)) %>% 
  mutate(
    dummy = ifelse(retx > finan, 1, 0) # Create dummy
  ) %>% 
  transform(dummy = as.factor(dummy)) # Factorize dummy

summary(finance$dummy)

# Remove variables that were used to calculate dummmy
finance.final <- finance %>% 
  select(-c(retx, finan, prc)) 

unique.company.finance <- finance.final[c("permno")]
dups <- duplicated(unique.company.finance) # exclude comnam bcs this changes over time for some companies
unique.company.finance <- unique.company.finance[!dups,]


## Data splitting
training.set <- finance.final %>% 
  filter(year(date) < 2019)

validation.set <- finance.final %>% 
  filter(year(date) == 2019) 

test.set <- finance.final %>% 
  filter(year(date) == 2020)


# Create agriculture df 
agriculture <- merged.industry %>% 
  filter(industry == "Agriculture") %>%
  dplyr::select(-industry)

agriculture <- merge(agriculture, industry.returns.monthly[c("year","month", "food")],
                     by=c("year", "month")) %>% 
  dplyr::select(-c(year, month, sic)) %>% 
  mutate(
    dummy = ifelse(retx > food, 1, 0) # Create dummy
  ) %>% 
  transform(dummy = as.factor(dummy)) # Factorize dummy

summary(agriculture$dummy)

# Remove variables that were used to calculate dummmy
agriculture <- agriculture %>% 
  dplyr::select(-c(retx, food, prc))

unique.company.agriculture <- agriculture[c("permno")]
dups <- duplicated(unique.company.agriculture) # exclude comnam bcs this changes over time for some companies
unique.company.agriculture <- unique.company.agriculture[!dups,]


######################################################################
# Create transportation df 
transportation <- merged.industry %>% 
  filter(industry == "Transportation") %>%
  dplyr::select(-industry)

transportation <- merge(transportation, industry.returns.monthly[c("year","month", "trans")],
                        by=c("year", "month")) %>% 
  dplyr::select(-c(year, month, sic)) %>% 
  mutate(
    dummy = ifelse(retx > trans, 1, 0) # Create dummy
  ) %>% 
  transform(dummy = as.factor(dummy)) # Factorize dummy

summary(transportation$dummy)

# Remove variables that were used to calculate dummmy
transportation <- transportation %>% 
  dplyr::select(-c(retx, trans, prc))

unique.company.transportation <- transportation[c("permno")]
dups <- duplicated(unique.company.transportation) # exclude comnam bcs this changes over time for some companies
unique.company.transportation <- unique.company.transportation[!dups,] 








