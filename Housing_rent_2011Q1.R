library(tidyverse)
library(readxl)

# Original data paths
folder_path <- "D:/finance/real_estate/rent/"
file_names <- list.files(folder_path)
print(file_names)

# Load your reference and renaming sheets
df_library <- read_excel("C:/Project_R/Real_estate/real_estate/rent_library.xlsx", sheet = "directory")
df_March_2011 <- read_excel("D:/finance/real_estate/rent/cuserscrahirdesktopprivate-rental-report-2011-03.xls", sheet = "Postcode", skip = 19)
df_col_name <- read_excel("C:/Project_R/Real_estate/real_estate/rent_library.xlsx", sheet = "rename_2011")

# Clean and prepare df_March_2011
dff_march_2011 <- df_March_2011 %>%
  fill("Metro/Rest of State", .direction = "down") %>%
  mutate(Quarter = as.Date("2011-03-01"))

# Filter out rows where 'Postcode' is not numeric
dff_march_2011 <- dff_march_2011 %>%
  filter(!is.na(as.numeric(Postcode)))

# Create the renaming map, only including columns present in dff_march_2011
col_rename_map <- df_col_name %>%
  filter(old_name %in% colnames(dff_march_2011)) %>%
  deframe()

# Rename columns based on the filtered map
dff_march_2011 <- dff_march_2011 %>%
  rename_with(~ col_rename_map[.x], .cols = names(col_rename_map))

# Display the cleaned and renamed data
print(dff_march_2011)

