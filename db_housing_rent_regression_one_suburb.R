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

# Gather the data to long format for regression plotting
df_long <- df_postcode_5166 %>%
  select(quarter, 
         `1BR_flats_median`, `2BR_flats_median`, `3BR_flats_median`, `4BR_flats_median`,
         `1BR_houses_median`, `2BR_houses_median`, `3BR_houses_median`, `4BR_houses_median`) %>%
  pivot_longer(
    cols = -quarter,
    names_to = "category",
    values_to = "median_price"
  )

# Define a unique color palette
color_palette <- c(
  "1BR_flats_median" = "blue",
  "2BR_flats_median" = "green",
  "3BR_flats_median" = "orange",
  "4BR_flats_median" = "red",
  "1BR_houses_median" = "purple",
  "2BR_houses_median" = "brown",
  "3BR_houses_median" = "pink",
  "4BR_houses_median" = "cyan"
)

# Plot regression lines for each category
ggplot(df_long, aes(x = quarter, y = median_price, color = category)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  labs(
    title = "Regression of Median Rental Prices by Bedroom Size (Postcode 5166)",
    x = "Year",
    y = "Median Price"
  ) +
  scale_color_manual(
    values = color_palette,
    labels = c(
      "1BR Flats", "2BR Flats", "3BR Flats", "4BR Flats",
      "1BR Houses", "2BR Houses", "3BR Houses", "4BR Houses"
    )
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
