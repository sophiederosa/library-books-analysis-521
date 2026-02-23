# nolint start
library(readr)
library(dplyr)
library(stringr)

# --- Helper: parse book list files into title + author ---
# Format: <Title> by <Author>
parse_books <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[lines != ""]

  books <- lapply(lines, function(line) {
    parts <- str_split_fixed(line, "\\s+by\\s+", 2)
    list(title = str_trim(parts[1, 1]), author = str_trim(parts[1, 2]))
  })
  books
}

# --- Helper: extract word set from a string (lowercase, no punctuation) ---
to_word_set <- function(text) {
  text <- tolower(text)
  text <- str_replace_all(text, "[^a-z0-9\\s]", "")  # strip punctuation
  words <- str_split(str_squish(text), "\\s+")[[1]]
  words[words != ""]
}

# --- Helper: find which group a CSV row belongs to (or NA if no match) ---
find_group <- function(csv_title, csv_creator, group_lists) {
  title_words <- to_word_set(csv_title)
  creator_words <- to_word_set(csv_creator)
  for (g in names(group_lists)) {
    for (book in group_lists[[g]]) {
      tw <- to_word_set(book$title)
      if (length(tw) == 0 || !all(tw %in% title_words)) next
      aw <- to_word_set(book$author)
      if (length(aw) == 0 || all(aw %in% creator_words)) {
        return(g)
      }
    }
  }
  NA_character_
}

# --- Load book lists ---
lgbt_books <- parse_books("GroupProject/LGBTBooks.txt")
control_books <- parse_books("GroupProject/ControlBooks.txt")
treatment_books <- parse_books("GroupProject/TreatmentBooks.txt")
all_books <- c(lgbt_books, control_books, treatment_books)

cat("LGBT books loaded:", length(lgbt_books), "\n")
cat("Control books loaded:", length(control_books), "\n")
cat("Treatment books loaded:", length(treatment_books), "\n")

group_lists <- list(
  "LGBT" = lgbt_books,
  "Treatment" = treatment_books,
  "Control" = control_books
)

# --- Process existing filtered_books.csv ---
cat("\nProcessing: GroupProject/filtered_books.csv\n")
data <- read_csv("GroupProject/filtered_books.csv", show_col_types = FALSE)
cat("Rows in file:", nrow(data), "\n")

# Ensure Creator column exists (fill NA with empty string)
data <- data %>% mutate(Creator = ifelse(is.na(Creator), "", Creator))

# --- Filter and tag in one pass ---
n <- nrow(data)
book_groups <- character(n)

for (i in seq_len(n)) {
  book_groups[i] <- find_group(data$Title[i], data$Creator[i], group_lists)
  if (i %% 10000 == 0) {
    cat("  Processed", i, "of", n, "rows...\n")
  }
}
cat("  Processed", n, "of", n, "rows (done)\n")

data$BookGroup <- book_groups
filtered_data <- data %>% filter(!is.na(BookGroup))

cat("  LGBT:", sum(filtered_data$BookGroup == "LGBT", na.rm = TRUE),
    " Treatment:", sum(filtered_data$BookGroup == "Treatment", na.rm = TRUE),
    " Control:", sum(filtered_data$BookGroup == "Control", na.rm = TRUE), "\n")

# --- Write output ---
write_csv(filtered_data, "GroupProject/filtered_books_v2.csv")
cat("\nTotal filtered rows:", nrow(filtered_data), "\n")
cat("Written to GroupProject/filtered_books_v2.csv\n")

#nolint end