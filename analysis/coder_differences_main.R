# Load required libraries
library(tidyverse)
library(here)
library(openxlsx)

`%notin%` <- Negate(`%in%`) # Define a "not in" operator

# Read CSV files
d_bk <- read_csv(here('data','primary','data_bianca.csv'))
d_jb <- read_csv(here('data','primary','data_justine.csv'))

# Shared article IDs
shared <- intersect(d_bk$article_id, d_jb$article_id)

# Article IDs unique to each
unique_dbk <- setdiff(d_bk$article_id, d_jb$article_id)
unique_djb <- setdiff(d_jb$article_id, d_bk$article_id)

# combine datasets
d <- bind_rows(d_bk, d_jb)

# # eligibility
d %>%
  rowwise() %>%
  mutate(eligible = all(c_across(starts_with("eligible")) == "YES")) %>%
  ungroup() %>%
  filter(eligible == T) %>%
  count(year)

# Identify the two coders
coders <- unique(d$coder)

# Check exactly two coders
if (length(coders) != 2) {
  stop("There must be exactly two coders. Found: ", paste(coders, collapse = ", "))
}

# Create consensus rows based on JB
consensus_rows <- d %>%
  filter(coder == "JB") %>%
  mutate(
    coder = "Consensus",
    timestamp = "None"
  )

# Combine original and consensus data
d_extended <- bind_rows(d, consensus_rows)

# Set factor level for consistent ordering
d_extended$coder <- factor(d_extended$coder, levels = c(coders, "Consensus"))

# Sort rows by article ID and coder
d_extended <- d_extended %>%
  arrange(article_id, coder)

# get meta data (DOI, article title, journal)
meta_2012 <- read_csv(here('data','prepare-sample','03_extraction_ready','extraction_ready_2012.csv')) %>%
  mutate(article_id = paste0('2012_',article_id)) %>%
  rename(article_title = title)

meta_2018 <- read_csv(here('data','prepare-sample','03_extraction_ready','extraction_ready_2018.csv')) %>%
  mutate(article_id = paste0('2018_',article_id)) %>%
  rename(article_title = title)

meta_2024 <- read_csv(here('data','prepare-sample','03_extraction_ready','extraction_ready_2024.csv')) %>%
  mutate(article_id = paste0('2024_',article_id)) %>%
  rename(article_title = title)

meta_all <- bind_rows(meta_2012, meta_2018, meta_2024)

# Merge metadata with extracted data

d_extended <- left_join(d_extended, meta_all, by = 'article_id')

# Define columns to exclude from comparison (in addition to those ending with "verbatim")
exclude_cols <- c("timestamp", "coder", "notes")
info_cols <- setdiff(names(d_extended), c(exclude_cols, grep("verbatim$", names(d_extended), value = TRUE)))

# Create Excel workbook
wb <- createWorkbook()
addWorksheet(wb, "Comparison")
freezePane(wb, "Comparison", firstActiveRow = 2)

# Write column headers (once)
writeData(wb, "Comparison", d_extended[0, ], startRow = 1, colNames = TRUE)

# Start writing article data from row 2
row_idx <- 2

# Loop through each article
for (id in unique(d_extended$article_id)) {
  article_data <- d_extended %>% filter(article_id == id)
  
  # Safety check
  if (nrow(article_data) != 3) {
    warning(paste("Skipping article ID", id, "- expected 3 rows, found", nrow(article_data)))
    next
  }
  
  # Write article data
  writeData(wb, "Comparison", article_data, startRow = row_idx, colNames = FALSE)
  
  # Compare coder rows (exclude Consensus)
  coder_rows <- article_data %>% filter(coder != "Consensus")
  
  for (col_name in info_cols) {
    val1 <- coder_rows[[1, col_name]]
    val2 <- coder_rows[[2, col_name]]
    
    if (!identical(val1, val2)) {
      # Get Excel column index
      col_index <- which(names(d_extended) == col_name)
      
      # Create yellow highlight style
      highlight_style <- createStyle(fgFill = "#FFFF00")
      
      # Apply highlight to consensus row
      addStyle(wb, "Comparison", highlight_style,
               rows = row_idx + 2, cols = col_index, gridExpand = TRUE)
    }
  }
  
  # Move to next article (3 rows per article)
  row_idx <- row_idx + 3
}

# Save the workbook to specified path
output_path <- here("data", "processed", "coding_differences.xlsx")
saveWorkbook(wb, output_path, overwrite = TRUE)

cat("✅ Export complete:", output_path, "\n")
