# nolint start
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# --- Helper: normalize a name by keeping only full words (>1 char), lowercased & sorted ---
# This way "Smith, John" and "John Smith" both become "john smith"
normalize_name <- function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, "[^a-z0-9\\s]", " ")  # strip punctuation
  words <- str_split(str_squish(x), "\\s+")
  sapply(words, function(w) {
    w <- w[nchar(w) > 1]  # drop initials (single-char tokens)
    paste(sort(w), collapse = " ")
  })
}

# --- Find the top LGBT-subject books by checkouts for each year ---

data_dir <- "GroupProject/raw_data"
years <- 2015:2020

results <- list()

for (yr in years) {
  file_path <- file.path(data_dir, paste0(yr, "_data.csv"))
  if (!file.exists(file_path)) {
    cat("Skipping", yr, "- file not found\n")
    next
  }

  cat("Processing", yr, "...\n")
  df <- read_csv(file_path, show_col_types = FALSE)

  # Keep rows where Subjects contains "LGBT", "gay", "lesbian", or "queer" but NOT "fiction" (case-insensitive)
  lgbt <- df %>%
    filter(str_detect(Subjects, regex("LGBT|gay|lesbian|queer", ignore_case = TRUE)),
           !str_detect(Subjects, regex("fiction|novel", ignore_case = TRUE)))

  if (nrow(lgbt) == 0) {
    cat("  No LGBT-subject books found in", yr, "\n")
    next
  }

  # Normalize creator for grouping ("Last, First" and "First Last" → same key)
  # Aggregate total checkouts per title + normalized creator
  top_books <- lgbt %>%
    mutate(CreatorNorm = normalize_name(replace_na(Creator, ""))) %>%
    group_by(Title, CreatorNorm) %>%
    summarise(
      TotalCheckouts = sum(Checkouts, na.rm = TRUE),
      Creator = first(Creator),  # keep one original name for display
      .groups = "drop"
    ) %>%
    arrange(desc(TotalCheckouts)) %>%
    mutate(Year = yr) %>%
    select(Year, Title, Creator, TotalCheckouts)

  results[[as.character(yr)]] <- top_books

  cat("  Found", nrow(top_books), "LGBT-subject titles;",
      "top book:", top_books$Title[1], "with", top_books$TotalCheckouts[1], "checkouts\n")
}

# Combine all years into one data frame
all_results <- bind_rows(results)

# Print top 10 per year
for (yr in years) {
  year_data <- all_results %>% filter(Year == yr)
  if (nrow(year_data) == 0) next
  cat("\n===", yr, "- Top 10 LGBT-Subject Books by Checkouts ===\n")
  print(year_data %>% head(10), n = 10)
}

# Write full results to CSV
output_path <- "GroupProject/lgbt_top_checkouts.csv"
write_csv(all_results, output_path)
cat("\nFull results written to", output_path, "\n")
