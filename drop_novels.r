# nolint start
library(readr)
library(dplyr)
library(stringr)

# --- Drop rows with "Novel" in Subjects from all raw_data CSVs ---

csv_files <- list.files("GroupProject/raw_data", pattern = "\\.csv$", full.names = TRUE)
cat("Found", length(csv_files), "CSV files in raw_data/\n")

for (csv_file in csv_files) {
  cat("\nProcessing:", csv_file, "\n")
  df <- read_csv(csv_file, show_col_types = FALSE)
  before <- nrow(df)

  df <- df %>%
    mutate(
      # Strip non-alphabetic characters (keep letters and spaces only)
      SubjectClean = str_replace_all(Subjects, "[^a-zA-Z ]", " "),
      SubjectClean = str_squish(SubjectClean)
    ) %>%
    filter(
      is.na(SubjectClean) |
      !(
        str_detect(SubjectClean, regex("\\bnovel\\b", ignore_case = TRUE)) |
        (str_detect(SubjectClean, regex("\\bfiction\\b", ignore_case = TRUE)) &
         !str_detect(SubjectClean, regex("\\bnonfiction\\b", ignore_case = TRUE)))
      )
    ) %>%
    select(-SubjectClean)

  after <- nrow(df)
  cat("  Rows before:", before, " After:", after, " Dropped:", before - after, "\n")

  # Extract year from filename (e.g., "2015_data.csv" -> "2015")
  year <- str_extract(basename(csv_file), "\\d{4}")
  out_file <- file.path("GroπupProject", paste0("filtered_", year, ".csv"))
  write_csv(df, out_file)
  cat("  Written to", out_file, "\n")
}

cat("\nDone.\n")
# nolint end
