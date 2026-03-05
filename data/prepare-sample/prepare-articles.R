library(tidyverse)
library(here)

# load the articles returned by the Web of Science (WoS) searches.
d_2012 <- read_csv(here('data', 'prepare-sample','01_wos_raw','wos_2012.csv'), show_col_types = F)
d_2018 <- read_csv(here('data', 'prepare-sample','01_wos_raw','wos_2018.csv'), show_col_types = F)
d_2024 <- read_csv(here('data', 'prepare-sample','01_wos_raw','wos_2024.csv'), show_col_types = F)

retraction_remove <- function(dataset){
  # Filter out rows that contain "retraction" (case-insensitive) in either column
  dataset_clean <- dataset %>%
    filter(
      !str_detect(str_to_lower(`All Document Types (comma-separated)`), "retracted")
    )
  
  # Report how many rows were removed
  rows_removed <- nrow(dataset) - nrow(dataset_clean)
  cat("Number of rows removed due to retraction:", rows_removed, "\n")
  return(dataset_clean)
}

d_2012 <- retraction_remove(d_2012)
d_2018 <- retraction_remove(d_2018)
d_2024 <- retraction_remove(d_2024)

randomly_select <- function(dataset){
  set.seed(42) # set the seed for reproducibility of random sampling
  dataset <- dataset %>%
    mutate(article_id = seq(1,nrow(dataset))) %>% # give every article a unique ID
    select(article_id, everything()) %>% # move article_id to first col
    slice_sample(n = 400) # randomly select 400 rows (we're doing more than the target sample of 200 to allow for exclusions)
  return(dataset)
}

d_2012_random <- randomly_select(d_2012)
d_2018_random <- randomly_select(d_2018)
d_2024_random <- randomly_select(d_2024)

# save datasets as is with all of the WOS bibliographic information
write_csv(d_2012_random, here('data','prepare-sample','02_wos_random','wos_2012_random.csv'))
write_csv(d_2018_random, here('data','prepare-sample','02_wos_random','wos_2018_random.csv'))
write_csv(d_2024_random, here('data','prepare-sample','02_wos_random','wos_2024_random.csv'))

# create slimmed down versions of the datasets ready for the coders

# Define a vector of the columns to select and rename
columns_to_select_rename <- c("article_id", "DOI", "journal_name" = "Source title", "title" = "Title") # column name mappings

# Select and rename columns
select_adjust <- function(dataset){
  dataset <- dataset %>%
    select(!!!columns_to_select_rename) %>%
    mutate(DOI = paste0('https://doi.org/',DOI)) # convert dois to links
  return(dataset)
}

d_2012_random_slim <- select_adjust(d_2012_random)
d_2018_random_slim <- select_adjust(d_2018_random)
d_2024_random_slim <- select_adjust(d_2024_random)

# save slimmed down version of datasets
write_csv(d_2012_random_slim, here('data','prepare-sample','03_extraction_ready','extraction_ready_2012.csv'))
write_csv(d_2018_random_slim, here('data','prepare-sample','03_extraction_ready','extraction_ready_2018.csv'))
write_csv(d_2024_random_slim, here('data','prepare-sample','03_extraction_ready','extraction_ready_2024.csv'))

