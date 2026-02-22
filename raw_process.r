library(readr)
library(dplyr)
library(stringr)

# --- Helper: parse book list files ---
# Expected format: "Title by/- Author" per line
# We extract just the title portion (before "by" or "-" delimiter)
parse_book_titles <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[lines != ""]  # drop blank lines

  # Split on " by " or " - " to isolate the title
  titles <- str_trim(str_replace(lines, "\\s+(by|-)\\s+.*$", ""))
  titles
}

# --- Build regex pattern for matching (case-insensitive partial match) ---
build_pattern <- function(titles) {
  if (length(titles) == 0) return(NULL)
  escaped <- str_replace_all(titles, "([\\\\\\[\\](){}.*+?^$|])", "\\\\\\1")
  paste(escaped, collapse = "|")
}

# --- Load book lists ---
lgbt_titles <- parse_book_titles("GroupProject/LGBTBooks.txt")
control_titles <- parse_book_titles("GroupProject/ControlBooks.txt")
treatment_titles <- parse_book_titles("GroupProject/TreatmentBooks.txt")
all_titles <- c(lgbt_titles, control_titles, treatment_titles)

cat("LGBT titles loaded:", length(lgbt_titles), "\n")
cat("Control titles loaded:", length(control_titles), "\n")
cat("Treatment titles loaded:", length(treatment_titles), "\n")

lgbt_pattern <- build_pattern(lgbt_titles)
control_pattern <- build_pattern(control_titles)
treatment_pattern <- build_pattern(treatment_titles)
combined_pattern <- build_pattern(all_titles)

# --- Process all CSVs in raw_data ---
csv_files <- list.files("GroupProject/raw_data", pattern = "\\.csv$", full.names = TRUE)
all_filtered <- list()

for (input_file in csv_files) {
  cat("\nProcessing:", input_file, "\n")
  data <- read_csv(input_file, show_col_types = FALSE)
  cat("Rows in file:", nrow(data), "\n")

  # --- Filter to only treatment + control books ---
  if (!is.null(combined_pattern)) {
    filtered_data <- data %>%
      filter(str_detect(Title, regex(combined_pattern, ignore_case = TRUE)))
  } else {
    filtered_data <- tibble()
  }

  # --- Tag each row as LGBT, Treatment, or Control ---
  filtered_data <- filtered_data %>%
    mutate(BookGroup = case_when(
      !is.null(lgbt_pattern) & str_detect(Title, regex(lgbt_pattern, ignore_case = TRUE)) ~ "LGBT",
      !is.null(treatment_pattern) & str_detect(Title, regex(treatment_pattern, ignore_case = TRUE)) ~ "Treatment",
      !is.null(control_pattern) & str_detect(Title, regex(control_pattern, ignore_case = TRUE)) ~ "Control",
      TRUE ~ NA_character_
    ))

  cat("  LGBT:", sum(filtered_data$BookGroup == "LGBT", na.rm = TRUE),
      " Treatment:", sum(filtered_data$BookGroup == "Treatment", na.rm = TRUE),
      " Control:", sum(filtered_data$BookGroup == "Control", na.rm = TRUE), "\n")

  all_filtered <- append(all_filtered, list(filtered_data))
}

# --- Combine and write single output ---
filtered_all <- bind_rows(all_filtered)
write_csv(filtered_all, "GroupProject/filtered_books.csv")
cat("\nAll years combined:", nrow(filtered_all), "rows\n")
cat("Written to GroupProject/filtered_books.csv\n")