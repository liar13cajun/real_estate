# price to rent ratio 
# price to annual rent


# get rent average data for house

# get rent avergae data for flat

# each post code

# work with each post code

# get housing price history across suburb 

# Load necessary libraries
library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(dplyr)

# Convert the string to POSIXct
filter_quarter <- as.POSIXct("2024-06-01 UTC", tz = "UTC")

# Filter



# Load rent history data from Excel file
rent_history_path <- "C:/Project_R/Real_estate/real_estate/rent_history_ingested.xlsx"
df_rent_history <- read_excel(rent_history_path)

filter_quarter <-
df_rent_filter <- df_rent_history %>%  filter(postcode==5166 & quarter== filter_quarter)

df <- read_excel("D:/finance/real_estate/lsg_stats_2024_q3.xlsx")
df$PostCode <- as.character(df$PostCode)
colnames(df)[6] <- "median_value"
df$Suburb <- tolower(df$Suburb)