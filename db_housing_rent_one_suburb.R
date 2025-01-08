# Load necessary libraries
library(tidyverse)
library(readxl)

# Load the rent history data
rent_history_ingested <- "C:/Project_R/Real_estate/real_estate/rent_history_ingested.xlsx"
df_rent_history <- read_excel(rent_history_ingested)

# Filter the dataset for postcode 5166
df_postcode_5166 <- df_rent_history %>% filter(postcode == 5166)

# Convert the 'quarter' column to Date type
df_postcode_5166 <- df_postcode_5166 %>% mutate(quarter = as.Date(quarter))

# Define a unique color palette for all categories
color_palette <- c(
  "1BR Flats Median" = "blue",
  "2BR Flats Median" = "green",
  "3BR Flats Median" = "orange",
  "4BR Flats Median" = "red",
  "1BR Houses Median" = "purple",
  "2BR Houses Median" = "brown",
  "3BR Houses Median" = "pink",
  "4BR Houses Median" = "cyan"
)

# Plot median prices of flats and houses over time
ggplot(df_postcode_5166, aes(x = quarter)) +
  # Flats
  geom_point(aes(y = `1BR_flats_median`, color = "1BR Flats Median"), size = 1) +
  geom_point(aes(y = `2BR_flats_median`, color = "2BR Flats Median"), size = 1) +
  geom_point(aes(y = `3BR_flats_median`, color = "3BR Flats Median"), size = 1) +
  geom_point(aes(y = `4BR_flats_median`, color = "4BR Flats Median"), size = 1) +
  # Houses
  geom_point(aes(y = `1BR_houses_median`, color = "1BR Houses Median"), size = 1) +
  geom_point(aes(y = `2BR_houses_median`, color = "2BR Houses Median"), size = 1) +
  geom_point(aes(y = `3BR_houses_median`, color = "3BR Houses Median"), size = 1) +
  geom_point(aes(y = `4BR_houses_median`, color = "4BR Houses Median"), size = 1) +
  # Customize labels and appearance
  labs(
    title = "Median Rental Prices by Bedroom Size (Postcode 5166)",
    x = "Year",
    y = "Median Price"
  ) +
  scale_color_manual(
    values = color_palette,
    breaks = names(color_palette)
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


