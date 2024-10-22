# SA housing Data
# median metro price across quarter 
# get P to R index

# in future do per suburb 

library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(sf)

df_metro_median_price<- read_excel("D:/finance/real_estate/median_hse_price_webscrape.xlsx")

colnames(df_metro_median_price) <- c("Quarter","Metro","Non_Metro")
print(df_metro_median_price)

# Convert the Quarter column to a date (assuming the first day of the quarter)
df_metro_median_price <- df_metro_median_price %>%
  mutate(Quarter = as.Date(paste("01", Quarter), format = "%d %b %Y"))

# Remove the dollar sign and commas, then convert to numeric
df_metro_median_price <- df_metro_median_price %>%
  mutate(Metro = as.numeric(gsub("[\\$,]", "", Metro)))

# Plot Metro prices over time (Quarter)
ggplot(df_metro_median_price, aes(x = Quarter, y = Metro)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Adelaide Metro Housing Prices Over Time",
       x = "Quarter",
       y = "Median Metro Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
