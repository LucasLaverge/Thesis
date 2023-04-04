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
                          "distributionchannel")
  df <- df %>% 
    dplyr::select(-excluded.variables)
  remove(excluded.variables)
  
  # Remove NA's ----------------------------------------------------------
  
  remove_all_NA_rows <- function(df) {
    #' @description Function that removes any row with NA's
    #'
    #' @param df    Passing a data frame
    #' @return      Data frame NA filtered rows
    
    return(df %>% filter(rowSums(is.na(df[,9:71])) < 1 ))
  }
  
  remove_NA_rows <- function(df) {
    #' @description Function that removes any rows with 10 or more NA's
    #'
    #' @param df    Passing a data frame
    #' @return      Data frame NA filtered rows
    
    return(df %>% filter(rowSums(is.na(df[,9:71])) < 10 ))
  }

  # Apply filters to df
  df <- df %>% remove_NA_rows()
  df.all <- df %>% remove_all_NA_rows()
  # Remove function
  remove(remove_NA_rows, remove_all_NA_rows)

############################################################################
########################## ADAPT DATA #######################################
############################################################################
#bin product culture scores
df$score.bin = cut(df$sc1, breaks = c(-Inf,82,94,106,118,130,142,154,166,178,190,202,214,+Inf), 
                   labels = c("bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12", "bin13"))
 
df$innovation.culture.score = rowSums(df[,c("q4.2", "q4.3", "q4.4", "q4.5", "q4.6", "q4.7", "q4.8", "q4.9", "q4.10", 
                                      "q4.11", "q4.12", "q4.13", "q4.14", "q4.15", "q4.16", "q4.17", "q4.18", 
                                      "q4.19", "q4.20", "q4.21", "q4.22")])
df$product.culture.score = rowSums(df[,c("q5.2","q5.3","q5.4","q5.5","q5.6","q5.7","q5.8","q5.9","q5.10","q5.11",
                                         "q5.12","q5.13","q5.14","q5.15","q5.16","q5.17","q5.18","q5.19","q5.20",
                                         "q5.21","q5.22","q5.23","q5.24","q5.25","q5.26","q5.27","q5.28")])


# Load the ggplot2 package
library(ggplot2)

plot(df$innovation.culture.score, df$product.culture.score)
# Create the plot
ggplot(df, aes(innovation.culture.score, product.culture.score)) +
  geom_point() + # Add points
  scale_x_continuous(limits = c(0, 120)) + # Set the x-axis limits
  scale_y_continuous(limits = c(0, 120)) # Set the y-axis limits














