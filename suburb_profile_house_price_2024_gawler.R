library(tidyverse)
library(readxl)

# Read the data
df_2024_q1 <- read_excel("D:/finance/real_estate/lsg_stats_2024_q1.xlsx")
df_2024_q2 <- read_excel("D:/finance/real_estate/lsg_stats_2024_q2.xlsx")
df_2024_q3 <- read_excel("D:/finance/real_estate/lsg_stats_2024_q3.xlsx")
df_2024_q4 <- read_excel("D:/finance/real_estate/lsg_stats_2024_q4.xlsx")

# Select relevant columns and ensure 'Suburb' is lowercase
df_2024_q1 <- df_2024_q1 %>%
  mutate(Suburb = tolower(Suburb)) %>%
  select(City, Suburb, starts_with("Median") , -`Median Change`,starts_with("Sales"))

df_2024_q2 <- df_2024_q2 %>%
  mutate(Suburb = tolower(Suburb)) %>%
  select(City, Suburb, starts_with("Median") , -`Median Change` ,starts_with("Sales"))

df_2024_q3 <- df_2024_q3 %>%
  mutate(Suburb = tolower(Suburb)) %>%
  select(City, Suburb, starts_with("Median") , -`Median Change` ,starts_with("Sales"))

df_2024_q4 <- df_2024_q4 %>%
  mutate(Suburb = tolower(Suburb)) %>%
  select(City, Suburb, starts_with("Median") , -`Median Change` ,starts_with("Sales"))

# Merge data frames by 'Suburb' and 'City'
df_merged <- reduce(list(df_2024_q1, df_2024_q2, df_2024_q3, df_2024_q4), full_join, by = c("Suburb", "City"))

# View the result
print(df_merged)

# Filter data for Gawler
df_gawler <- df_merged %>%
  select(-starts_with("Sales")) %>% 
  filter(City == "GAWLER") %>%
  pivot_longer(cols = starts_with("Median"), names_to = "Quarter", values_to = "Median_Price")

# Clean up the quarter labels
df_gawler$Quarter <- recode(df_gawler$Quarter,
                            "Median 1Q 2023" = "Q1 2023",
                            "Median 2Q 2023" = "Q2 2023",
                            "Median 3Q 2023" = "Q3 2023",
                            "Median 4Q 2023" = "Q4 2023",
                            
                            "Median 1Q 2024" = "Q1 2024",
                            "Median 2Q 2024" = "Q2 2024",
                            "Median 3Q 2024" = "Q3 2024",
                            "Median 4Q 2024" = "Q4 2024")

# Plot the median price trend
ggplot(df_gawler, aes(x = Quarter, y = Median_Price, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = "Median Price Trend for Gawler",
       x = "Quarter",
       y = "Median Price") +
  theme_minimal() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))


# For sales data plotting, you can filter and pivot similarly, as needed:
df_gawler_sales <- df_merged %>%
  select(-starts_with("Median")) %>% 
  filter(City == "GAWLER") %>%
  pivot_longer(cols = starts_with("Sales"), names_to = "Quarter", values_to = "Sales_Amount")

# Clean up sales quarter labels similarly
df_gawler_sales$Quarter <- recode(df_gawler_sales$Quarter,
                                  "Sales 1Q 2023" = "Q1 2023",
                                  "Sales 2Q 2023" = "Q2 2023",
                                  "Sales 3Q 2023" = "Q3 2023",
                                  "Sales 4Q 2023" = "Q4 2023",
                                  "Sales 1Q 2024" = "Q1 2024",
                                  "Sales 2Q 2024" = "Q2 2024",
                                  "Sales 3Q 2024" = "Q3 2024",
                                  "Sales 4Q 2024" = "Q4 2024")

# Plot the sales trend for Gawler
ggplot(df_gawler_sales, aes(x = Quarter, y = Sales_Amount, group = 1)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "orange", size = 3) +
  labs(title = "Sales Trend for Gawler",
       x = "Quarter",
       y = "Sales Amount") +
  theme_minimal() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))

