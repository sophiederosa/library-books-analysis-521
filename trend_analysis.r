library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)


# =============================================================================
# 1. Read 2019 and 2020 data (Jan 2019 – Dec 2020)
# =============================================================================
csv_files <- list.files(".",
                        pattern = "^filtered_books_(2019|2020)\\.csv$",
                        full.names = TRUE)
cat("Found", length(csv_files), "CSV files:\n")
cat(paste(" ", csv_files, collapse = "\n"), "\n\n")

data <- csv_files %>%
  lapply(read_csv, show_col_types = FALSE) %>%
  bind_rows()

# Build a date-like column for filtering (first of each month)
data <- data %>%
  mutate(YearMonth = make_date(CheckoutYear, CheckoutMonth, 1))

# Keep only Jan 2019 through Dec 2020
data <- data %>%
  filter(YearMonth >= as.Date("2019-01-01"),
         YearMonth <= as.Date("2020-12-01"))

cat("Total rows after date filter:", nrow(data), "\n")
cat("Month range:", format(min(data$YearMonth), "%Y-%m"),
    "to", format(max(data$YearMonth), "%Y-%m"), "\n")
cat("Book groups:", paste(unique(data$BookGroup), collapse = ", "), "\n\n")

# =============================================================================
# 2. Aggregate checkouts by month and book group
# =============================================================================
trend_data <- data %>%
  filter(!is.na(BookGroup)) %>%
  group_by(YearMonth, BookGroup) %>%
  summarise(MedianCheckouts = median(Checkouts, na.rm = TRUE), .groups = "drop") %>%
  mutate(LogMedianCheckouts = log(MedianCheckouts),
         BookGroup = factor(BookGroup, levels = c("Control", "Treatment", "LGBT")))

cat("Median checkouts summary by group:\n")
print(trend_data %>% group_by(BookGroup) %>%
        summarise(OverallMedian = median(MedianCheckouts), .groups = "drop"))
cat("\nMonthly median checkouts by group:\n")
print(as.data.frame(trend_data))

# =============================================================================
# 3. Parallel Trends Plot – month-by-month (Jan 2019 – Dec 2020)
# =============================================================================
trend_plot <- ggplot(trend_data, aes(x = YearMonth, y = LogMedianCheckouts,
                                     color = BookGroup, group = BookGroup)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = as.Date("2020-05-01"),
             linetype = "dashed", color = "gray40") +
  annotate("text", x = as.Date("2020-05-15"), y = Inf,
           label = "Treatment\n(May 2020)",
           hjust = 0, vjust = 1.2, size = 3.5, color = "gray30") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Parallel Trends: Monthly Median Checkouts (Jan 2019 \u2013 Dec 2020)",
    subtitle = "Dashed line = start of treatment (May 2020)",
    x = "Month",
    y = "Log(Median Checkouts)",
    color = "Book Group"
  ) +
  scale_color_manual(values = c("Control" = "#2A9D8F",
                                "Treatment" = "#457B9D",
                                "LGBT" = "#E63946")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("trend_plot.png", trend_plot, width = 12, height = 6, dpi = 300)
cat("\nPlot saved to trend_plot.png\n")
print(trend_plot)
