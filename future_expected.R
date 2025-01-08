# Load necessary libraries
library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)

# Load rent history data from Excel file
rent_history_path <- "C:/Project_R/Real_estate/real_estate/rent_history_ingested.xlsx"
df_rent_history <- read_excel(rent_history_path)

# Specify the postcode of interest
interested_postcode <- 5043

# Filter the data for the specified postcode
df_rent_filtered <- df_rent_history %>%
  filter(postcode == interested_postcode)

# Convert `quarter` column to Date type for plotting and regression modeling
df_rent_filtered <- df_rent_filtered %>%
  mutate(quarter = as.Date(quarter))

# Preview the filtered data
head(df_rent_filtered)

##################################
# Create a plot for median rents over time for houses
ggplot(df_rent_filtered) +
  geom_point(aes(x = quarter, y = `1BR_houses_median`, color = "1BR House"), na.rm = TRUE) +
  geom_point(aes(x = quarter, y = `2BR_houses_median`, color = "2BR House"), na.rm = TRUE) +
  geom_point(aes(x = quarter, y = `3BR_houses_median`, color = "3BR Houses"), na.rm = TRUE) +
  geom_point(aes(x = quarter, y = `4BR_houses_median`, color = "4BR Houses"), na.rm = TRUE) +
  geom_point(aes(x = quarter, y = total_houses_median, color = "Total Houses Median"), na.rm = TRUE) +
  labs(
    title = paste0("Median House Rent Trends Over Time for Postcode ", interested_postcode),
    x = "Quarter",
    y = "Median Rent ($)",
    color = "Legend"
  ) +
  theme_minimal()

##################################
# Create a plot for median rents for flats (1BR, 2BR, 3BR, Total Flats)
ggplot(df_rent_filtered) +
  geom_point(aes(x = quarter, y = `1BR_flats_median`, color = "1BR Flats"), na.rm = TRUE) +
  geom_point(aes(x = quarter, y = `2BR_flats_median`, color = "2BR Flats"), na.rm = TRUE) +
  geom_point(aes(x = quarter, y = `3BR_flats_median`, color = "3BR Flats"), na.rm = TRUE) +
  geom_point(aes(x = quarter, y = total_flats_median, color = "Total Flats Median"), na.rm = TRUE) +
  labs(
    title = paste0("Median Flat Rent Trends Over Time for Postcode ", interested_postcode),
    x = "Quarter",
    y = "Median Rent ($)",
    color = "Legend"
  ) +
  theme_minimal()

##################################
# Build a linear regression model for 2BR Houses
model_2BR <- lm(`2BR_houses_median` ~ quarter, data = df_rent_filtered)
summary(model_2BR)  # Display model summary
plot(model_2BR)     # Diagnostic plots to check assumptions

# Identify and remove influential points
influential <- cooks.distance(model_2BR) > (4 / nrow(df_rent_filtered))
df_rent_filtered_cleaned <- df_rent_filtered[!influential, ]

##################################
# Visualize regression for 2BR Houses
ggplot(df_rent_filtered, aes(x = quarter, y = `2BR_houses_median`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Regression of 2BR House Median Rent Over Time",
    x = "Quarter",
    y = "Median Rent ($)"
  ) +
  theme_minimal()

##################################
# Build multiple regression model for 2BR Houses
model_mult <- lm(`2BR_houses_median` ~ quarter + `1BR_houses_median` + `3BR_houses_median`, data = df_rent_filtered)
summary(model_mult)

##################################
# Forecast future values for 2BR Houses
future_quarters <- data.frame(
  quarter = seq.Date(from = as.Date("2025-01-01"), to = as.Date("2025-12-31"), by = "quarter")
)
future_quarters$forecasted_rent <- predict(model_2BR, newdata = future_quarters)

# Combine historical and forecasted data
df_combined <- df_rent_filtered %>%
  select(quarter, `2BR_houses_median`) %>%
  rename(rent = `2BR_houses_median`) %>%
  mutate(type = "Historical") %>%
  bind_rows(
    future_quarters %>%
      rename(rent = forecasted_rent) %>%
      mutate(type = "Forecast")
  )

##################################
# Plot historical and forecasted data for 2BR Houses
ggplot(df_combined, aes(x = quarter, y = rent, color = type)) +
  geom_point() +
  geom_smooth(data = df_combined %>% filter(type == "Historical"), 
              method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "2BR House Median Rent Trends with Forecast for 2025",
    x = "Quarter",
    y = "Median Rent ($)",
    color = "Type"
  ) +
  scale_color_manual(values = c("Historical" = "blue", "Forecast" = "red")) +
  theme_minimal()

##################################

