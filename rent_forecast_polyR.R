# Load necessary libraries
library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)

# Load rent history data from Excel file
rent_history_path <- "C:/Project_R/Real_estate/real_estate/rent_history_ingested.xlsx"
df_rent_history <- read_excel(rent_history_path)

# Specify the postcode of interest
interested_postcode <- 5166

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
# 
# `1BR_flats_median`
# `2BR_flats_median`
# `3BR_flats_median`

# `1BR_houses_median`
# `2BR_houses_median`
# `3BR_houses_median`
# `4BR_houses_median`


# Define the column name for the dependent variable as a string
interested_BR <- "3BR_houses_median"

# Define the degree of the polynomial (experiment with different values)
polynomial_degree <- 3

# Create the formula for the polynomial model
model_formula <- reformulate(paste0("quarter^", polynomial_degree), response = interested_BR)

# Fit the polynomial regression model
model_BR <- lm(model_formula, data = df_rent_filtered)

# Display model summary
summary(model_BR)

# Diagnostic plots to check assumptions
plot(model_BR)

# Identify and remove influential points
influential <- cooks.distance(model_BR) > (4 / nrow(df_rent_filtered))
df_rent_filtered_cleaned <- df_rent_filtered[!influential, ]

########################
# Visualize regression for interested_BR
ggplot(df_rent_filtered, aes(x = quarter, y = interested_BR)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = paste0("Regression of ",interested_BR," Rent Over Time :",interested_postcode ),
    x = "Quarter",
    y = "Median Rent ($)"
  ) +
  theme_minimal()

##################################
# Forecast future values using the polynomial model
future_quarters <- data.frame(
  quarter = seq.Date(from = as.Date("2025-01-01"), to = as.Date("2025-12-31"), by = "quarter")
)
future_quarters$forecasted_rent <- predict(model_BR, newdata = future_quarters)

# Combine historical and forecast ed data
df_combined <- df_rent_filtered %>%
  select(quarter, interested_BR) %>%
  rename(rent = interested_BR) %>%
  mutate(type = "Historical") %>%
  bind_rows(
    future_quarters %>%
      rename(rent = forecasted_rent) %>%
      mutate(type = "Forecast")
  )

##################################
# Plot historical and forecast ed data for 2BR Houses
ggplot(df_combined, aes(x = quarter, y = rent, color = type)) +
  geom_point() +
  geom_smooth(data = df_combined %>% filter(type == "Historical"), 
              method = "lm", se = FALSE, color = "blue") +
  labs(
    title = paste0("House Median Rent Trends with Forecast for 2025 ",interested_BR," Postcode :",interested_postcode ),
    x = "Quarter",
    y = "Median Rent ($)",
    color = "Type"
  ) +
  scale_color_manual(values = c("Historical" = "blue", "Forecast" = "red")) +
  theme_minimal()


future_quarters

#############################################
# polynomail ploting 

# ... (your existing code)

# Fit the polynomial regression model (as before)
polynomial_degree <- 2 # Or whichever degree you choose
model_formula <- reformulate(paste0("poly(quarter, ", polynomial_degree, ", raw = TRUE)"), response = interested_BR)
model_BR <- lm(model_formula, data = df_rent_filtered)

# Create a sequence of x values for smoother plotting of the curve
quarter_seq <- seq(min(df_rent_filtered$quarter), max(df_rent_filtered$quarter), length.out = 100)

# Create a data frame for prediction
df_quarter_seq <- data.frame(quarter = quarter_seq)

# Predict y values based on the polynomial model
predicted_rents <- predict(model_BR, newdata = df_quarter_seq)

# Visualize regression for interested_BR with polynomial line
ggplot(df_rent_filtered, aes(x = quarter, y = interested_BR)) +
  geom_point() +
  geom_line(data = data.frame(quarter = quarter_seq, predicted_rent = predicted_rents),
            aes(x = quarter, y = predicted_rent), color = "blue", size = 1) + # Plot the polynomial line
  labs(
    title = paste0("Regression of ", interested_BR, " Rent Over Time :", interested_postcode),
    x = "Quarter",
    y = "Median Rent ($)"
  ) +
  theme_minimal()


# Forecast future values using the polynomial model (as before)
future_quarters <- data.frame(
  quarter = seq.Date(from = as.Date("2025-01-01"), to = as.Date("2025-12-31"), by = "quarter")
)

# Predict future rents
future_quarters$forecasted_rent <- predict(model_BR, newdata = future_quarters)


# Combine historical and forecasted data (with the predicted values from the sequence for the historical part)
df_combined <- df_rent_filtered %>%
  select(quarter, interested_BR) %>%
  rename(rent = interested_BR) %>%
  mutate(type = "Historical") %>%
  bind_rows(
    future_quarters %>%
      rename(rent = forecasted_rent) %>%
      mutate(type = "Forecast")
  )

# Plot historical and forecasted data with the polynomial curve and forecasts
ggplot(df_combined, aes(x = quarter, y = rent, color = type)) +
  geom_point() +
  geom_line(data = data.frame(quarter = quarter_seq, predicted_rent = predicted_rents),
            aes(x = quarter, y = predicted_rent), color = "blue", size = 1) + # Plot the polynomial curve for historical data
  labs(
    title = paste0("House Median Rent Trends with Forecast for 2025 ", interested_BR, " Postcode :", interested_postcode),
    x = "Quarter",
    y = "Median Rent ($)",
    color = "Type"
  ) +
  scale_color_manual(values = c("Historical" = "blue", "Forecast" = "red")) +
  theme_minimal()

future_quarters
