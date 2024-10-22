#store data in regarding 
library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(sf)


# df_March_2011 <- read_excel("D:/finance/real_estate/rent/cuserscrahirdesktopprivate-rental-report-2011-03.xls", sheet = "Postcode" , skip =19)

folder_path <- "D:/finance/real_estate/rent/"

file_names <- list.files(folder_path)
file_names_df <- data.frame(list.files(folder_path))
print(file_names)

