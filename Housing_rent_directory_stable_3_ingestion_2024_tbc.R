library(tidyverse)
library(readxl)

# Path to the rent library file
rent_library_path <- "C:/Project_R/Real_estate/real_estate/rent_library.xlsx"

# Load the directory of files and corresponding quarters from the "directory_try" sheet
df_library <- read_excel(rent_library_path, sheet = "directory_try")

# Ensure the columns `Name`, `Quarter`, `Skip`, and `Sheet` exist and extract them
file_paths <- df_library$Name
quarters <- df_library$Quarter
skips <- df_library$Skip
sheet <- df_library$Sheet

# Load the column renaming map
df_col_name <- read_excel(rent_library_path, sheet = "rename_2011")

# Initialize an empty list to store the processed data frames
processed_data <- list()

# Process each file and assign its quarter
for (i in seq_along(file_paths)) {
  file_path <- file_paths[i]
  quarter <- as.Date(quarters[i])  # Convert quarter from Excel to Date format
  
  # Construct the full file path
  full_file_path <- paste0("D:/finance/real_estate/rent/", file_path)
  
  # Load the data
  df <- read_excel(full_file_path, sheet = sheet[i], skip = skips[i])
  
  # Inspect column names
  print(colnames(df))
  
  # Clean and prepare the data
  cleaned_df <- df %>%
    # Ensure the exact column name matches
    rename(`Metro/Rest of State` = `Metro/Rest of State`) %>%
    # Apply fill explicitly to the column
    fill(`Metro/Rest of State`, .direction = "down") %>%
    # Add quarter column
    mutate(Quarter = quarter)
  
  # Replace '*' with '3' and handle 'n.a.' as NA, excluding first two columns
  cleaned_df <- cleaned_df %>%
    mutate(across(where(is.character), ~ case_when(
      . == "*" ~ "3",
      . == "n.a." ~ NA_character_,
      TRUE ~ .
    ))) %>%
    # Convert character columns to numeric, excluding the first two columns
    mutate(across(
      .cols = -c(1, 2, 31),  # Exclude the first two columns
      .fns = ~ suppressWarnings(as.numeric(.))
    ))
  
  # Filter rows where 'Postcode' is numeric
  cleaned_df <- cleaned_df %>%
    filter(!is.na(as.numeric(Postcode)))
  
  # Create renaming map for this file
  col_rename_map <- df_col_name %>%
    filter(old_name %in% colnames(cleaned_df)) %>%
    deframe()
  
  # Rename columns
  cleaned_df <- cleaned_df %>%
    rename_with(~ col_rename_map[.x], .cols = names(col_rename_map))
  
  # Append to the list
  processed_data <- append(processed_data, list(cleaned_df))
}

# Combine all processed data into a single data frame
final_data <- bind_rows(processed_data)

# Display the combined data
print(final_data)


# Save this data frame to ? 



# for further filter 


# certian subrub sales 


# plot graphs

