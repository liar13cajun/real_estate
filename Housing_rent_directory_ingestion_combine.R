library(tidyverse)
library(readxl)
library(writexl)

# Path to the rent library file
rent_library_path <- "C:/Project_R/Real_estate/real_estate/rent_library.xlsx"

# Load the directory of files and corresponding quarters from the "directory_all" sheet
df_library <- read_excel(rent_library_path, sheet = "directory_all")

# Ensure the columns `Name`, `Quarter`, `Skip`, and `Sheet` exist and extract them
file_paths <- df_library$Name
quarters <- df_library$Quarter
skips <- df_library$Skip
sheet <- df_library$Sheet
rename_col <- df_library$Rename  # Add column to identify the renaming method (rename_2011 or rename_2024)

# Load the column renaming maps
df_col_name_2011 <- read_excel(rent_library_path, sheet = "rename_2011")
df_col_name_2024 <- read_excel(rent_library_path, sheet = "rename_2024")

# Initialize an empty list to store the processed data frames
processed_data <- list()

# Process each file and assign its quarter
for (i in seq_along(file_paths)) {
  file_path <- file_paths[i]
  quarter <- as.Date(quarters[i])  # Convert quarter from Excel to Date format
  rename_type <- rename_col[i]  # Determine the renaming type for the current file
  
  # Construct the full file path
  full_file_path <- paste0("D:/finance/real_estate/rent/", file_path)
  
  # Load the data
  df <- read_excel(full_file_path, sheet = sheet[i], skip = skips[i])
  
  # Inspect file paths
  print(file_paths[i])
  
  # Apply transformations based on the renaming type
  if (rename_type == "rename_2011") {
    cleaned_df <- df %>%
      # Ensure the exact column name matches
      rename(`Metro/Rest of State` = `Metro/Rest of State`) %>%
      # Apply fill explicitly to the column
      fill(`Metro/Rest of State`, .direction = "down") %>%
      # Add quarter column
      mutate(Quarter = quarter) %>%
      # Replace '*' with '3' and handle 'n.a.' as NA
      mutate(across(where(is.character), ~ case_when(
        . == "*" ~ "3",
        . == "n.a." ~ NA_character_,
        TRUE ~ .
      ))) %>%
      # Convert character columns to numeric, excluding specific columns
      mutate(across(
        .cols = -c(1, 2, 31),
        .fns = ~ suppressWarnings(as.numeric(.))
      )) %>%
      # Filter rows where 'Postcode' is numeric
      filter(!is.na(as.numeric(Postcode)))
    
    # Create renaming map for this file
    col_rename_map <- df_col_name_2011 %>%
      filter(old_name %in% colnames(cleaned_df)) %>%
      deframe()
    
    # Rename columns
    cleaned_df <- cleaned_df %>%
      rename_with(~ col_rename_map[.x], .cols = names(col_rename_map))
    
  } else if (rename_type == "rename_2024") {
    cleaned_df <- df %>%
      mutate(Quarter = quarter, `Metro/Rest of State` = NA_character_) %>%
      relocate(`Metro/Rest of State`, .before = 1) %>%
      # Replace '*' with '3' and handle 'n.a.' as NA
      mutate(across(where(is.character), ~ case_when(
        . == "*" ~ "3",
        . == "n.a." ~ NA_character_,
        TRUE ~ .
      ))) %>%
      # Convert character columns to numeric, excluding specific columns
      mutate(across(
        .cols = -c(1, 2, 33),
        .fns = ~ suppressWarnings(as.numeric(.))
      ))
    
    # Create renaming map for this file
    col_rename_map <- df_col_name_2024 %>%
      filter(old_name %in% colnames(cleaned_df)) %>%
      deframe()
    
    # Rename columns
    cleaned_df <- cleaned_df %>%
      rename_with(~ col_rename_map[.x], .cols = names(col_rename_map)) %>%
      select(-'Delete1', -'Delete2')
  }
  
  # Append the cleaned data frame to the list
  processed_data <- append(processed_data, list(cleaned_df))
}

# Combine all processed data into a single data frame
final_data <- bind_rows(processed_data)

# Display the combined data
print(final_data)

# Save this data frame to ? 
write_xlsx( 
  final_data,"C:/Project_R/Real_estate/real_estate/rent_history_ingested.xlsx", 
  col_names = TRUE, 
  format_headers = TRUE)
