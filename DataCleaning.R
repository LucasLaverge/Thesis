#packages
library(readr)
library(tidyverse)
library(lubridate)


setwd("~/Thesis")
############################################################################
########################## LOAD DATA #######################################
############################################################################
# Load and read data -------------------------------------------------------
  # datasets (with choice text and with numeric variables)
  df1 <- read_csv("DataSet.csv")
  df2 <- read_csv("DataSet2.csv")
  # Remove row 2 and 3 that contain import ID and question name
  df1 <- df1[-c(1,2),]
  df2 <- df2[-c(1,2),]
  # Delete columns 
  df1 <- df1[,c(1:25)] # Delete columns of last three parts in first dataset
  df2 <- df2[,-c(1:8,10:25)] # Delete all columns expect those of last three parts in second dataset
  
############################################################################
########################## MERGE DATA #######################################
############################################################################
  # Create merged df, using response id
  df <- merge(df1, df2, by = "ResponseId")
  # Remove df1 and df2
  remove(df1, df2)
  
############################################################################
########################## Data Cleaning ###################################
############################################################################
# Feature selection functions ----------------------------------------------
  # Bring variable names tolower and change character types
  df<- df %>% 
    rename_with(tolower) %>% 
    mutate(startdate = lubridate::ymd_hms(startdate),
           enddate = lubridate::ymd_hms(enddate)) %>% 
    mutate(
      q4.2 = as.numeric(q4.2), q4.3 = as.numeric(q4.3), q4.4 = as.numeric(q4.4), q4.5 = as.numeric(q4.5), 
      q4.6 = as.numeric(q4.6), q4.7 = as.numeric(q4.7), q4.8 = as.numeric(q4.8), q4.9 = as.numeric(q4.9), 
      q4.10 = as.numeric(q4.10), q4.11 = as.numeric(q4.11), q4.12 = as.numeric(q4.12), q4.13 = as.numeric(q4.13), 
      q4.14 = as.numeric(q4.14), q4.15 = as.numeric(q4.15), q4.16 = as.numeric(q4.16), q4.17 = as.numeric(q4.17), 
      q4.18 = as.numeric(q4.18), q4.19 = as.numeric(q4.19), q4.20 = as.numeric(q4.20), q4.21 = as.numeric(q4.21), 
      q4.22 = as.numeric(q4.22)
    ) %>% 
    mutate(
      q5.2 = as.numeric(q5.2), q5.3 = as.numeric(q5.3), q5.4 = as.numeric(q5.4), q5.5 = as.numeric(q5.5), 
      q5.6 = as.numeric(q5.6), q5.7 = as.numeric(q5.7), q5.8 = as.numeric(q5.8), q5.9 = as.numeric(q5.9), 
      q5.10 = as.numeric(q5.10), q5.11 = as.numeric(q5.11), q5.12 = as.numeric(q5.12), q5.13 = as.numeric(q5.13), 
      q5.14 = as.numeric(q5.14), q5.15 = as.numeric(q5.15), q5.16 = as.numeric(q5.16), q5.17 = as.numeric(q5.17), 
      q5.18 = as.numeric(q5.18), q5.19 = as.numeric(q5.19), q5.20 = as.numeric(q5.20), q5.21 = as.numeric(q5.21), 
      q5.22 = as.numeric(q5.22), q5.23 = as.numeric(q5.23), q5.24 = as.numeric(q5.24), q5.25 = as.numeric(q5.25), 
      q5.26 = as.numeric(q5.26), q5.27 = as.numeric(q5.27), q5.28 = as.numeric(q5.28)
    ) %>% 
    mutate(
      q6.2 = as.numeric(q6.2), q6.3 = as.numeric(q6.3), q6.4 = as.numeric(q6.4), q6.5 = as.numeric(q6.5),
      q6.6 = as.numeric(q6.6), q6.7 = as.numeric(q6.7), q6.8 = as.numeric(q6.8), q6.9 = as.numeric(q6.9),
      q6.10 = as.numeric(q6.10), q6.11 = as.numeric(q6.11), q6.12 = as.numeric(q6.12), q6.13 = as.numeric(q6.13)
    ) %>% 
    mutate(
      sc0 = as.numeric(sc0), sc1 = as.numeric(sc1), sc2 = as.numeric(sc2)
    )
  
# Irrelevant features ----------------------------------------------------------
  # Variables that cannot be included 
  excluded.variables <- c("startdate", 
                          "enddate", 
                          "status", 
                          "ipaddress", 
                          "duration (in seconds)", 
                          "recordeddate", 
                          "recipientlastname", 
                          "recipientfirstname", 
                          "recipientemail", 
                          "externalreference", 
                          "locationlatitude", 
                          "locationlongitude", 
                          "userlanguage", 
                          "q2.1",
                          "responseid", 
                          "finished", 
                          "distributionchannel",
                          "progress")
  df <- df %>% 
    dplyr::select(-excluded.variables)
  remove(excluded.variables)
  
# Remove NA's ----------------------------------------------------------
  remove_all_NA_rows <- function(df) {
    #' @description Function that removes any row with NA's
    #'
    #' @param df    Passing a data frame
    #' @return      Data frame NA filtered rows
    
    return(df %>% filter(rowSums(is.na(df[,8:70])) < 1 ))
  }
  
  remove_NA_rows <- function(df) {
    #' @description Function that removes any rows with 10 or more NA's
    #'
    #' @param df    Passing a data frame
    #' @return      Data frame NA filtered rows
    
    return(df %>% filter(rowSums(is.na(df[,8:70])) < 10 ))
  }

  # Apply filters to df
  # df <- df %>% remove_NA_rows() --> discuss if we want more data containing NA's or not
  df <- df %>% remove_all_NA_rows()
  # Remove function
  remove(remove_NA_rows, remove_all_NA_rows)
  
# Fill in the 'Other" with corresponding text answers ----------------------------------------------------------
  # Industries
  df$q3.1[df$q3.1 == "Other"] <- df$q3.1_7_text[df$q3.1 == "Other"]
  #Regions
  df$q3.2[df$q3.2 == "Other"] <- df$q3.2_7_text[df$q3.2 == "Other"]
    # Extra remove row containing USA and replace Leuven with flemish brabant
    df <- df[!grepl("(?i)^.*usa.*$", df$q3.2), ] #remove usa row
    df$q3.2[df$q3.2 == "Leuven"] <- "Flemish-Brabant" # Change Leuven to it's correct region
    #Remove text variables 
    excluded.variables <- c("q3.1_7_text", "q3.2_7_text")
    df <- df %>% 
      dplyr::select(-excluded.variables)
    remove(excluded.variables) 
    
# Give meaningful columnnames ----------------------------------------------------------
    # Industry
    names(df)[names(df) == "q3.1"] <- "industry"
    # Region
    names(df)[names(df) == "q3.2"] <- "region"
    # size
    names(df)[names(df) == "q3.3"] <- "size"
    # function
    names(df)[names(df) == "q3.4"] <- "function"
    # IT sector or not
    names(df)[names(df) == "q3.5"] <- "it"
    
# Reverse scores of questions with a negative connotation ----------------------------------------------------------
    df$q4.5 <- 6 - df$q4.5
    df$q4.10 <- 6 - df$q4.10
    df$q4.17 <- 6 - df$q4.17
    df$q4.21 <- 6 - df$q4.21
    df$q5.4 <- 6 - df$q5.4
    df$q5.8 <- 6 - df$q5.8
    df$q5.10 <- 6 - df$q5.10
    df$q5.16 <- 6 - df$q5.16
    
# Modify the wrong 5point likert scale to a 7 point likert scale  (q6.6) ---------------------------------------------
    df$q6.6  <- as.integer((df$q6.6 - 1) * 6 / 4 + 1)
    df$q6.6 <- as.numeric(df$q6.6) # change back to numeric

# Remove scores calculated by Qualtrics (wrong scores)--------------------------------------------- 
    #Remove text variables 
    excluded.variables <- c("sc0", "sc1", "sc2")
    df <- df %>% 
      dplyr::select(-excluded.variables)
    remove(excluded.variables)   
    
  
## things to think about  
    # if we use data were we allow some NA's, how do we handle them --> average? per section?
                                                                  # --> fill in NA with overall average
    # easier if we only use data with zero NA's















