library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)


# =============================================================================
# 1. Read and combine all filtered_books_*.csv files
# =============================================================================
csv_files <- list.files(pattern = "^filtered_books_\\d{4}\\.csv$", full.names = TRUE)
cat("Found", length(csv_files), "CSV files:\n")
cat(paste(" ", csv_files, collapse = "\n"), "\n\n")

data <- csv_files %>%
  lapply(read_csv, show_col_types = FALSE) %>%
  bind_rows()

cat("Total rows:", nrow(data), "\n")
cat("Years covered:", paste(sort(unique(data$CheckoutYear)), collapse = ", "), "\n")
cat("Book groups:", paste(unique(data$BookGroup), collapse = ", "), "\n\n")

# =============================================================================
# 2. Aggregate checkouts by year and book group
# =============================================================================
trend_data <- data %>%
  filter(!is.na(BookGroup)) %>%
  group_by(CheckoutYear, BookGroup) %>%
  summarise(TotalCheckouts = sum(Checkouts, na.rm = TRUE), .groups = "drop") %>%
  mutate(BookGroup = factor(BookGroup, levels = c("Control", "Treatment", "LGBT")))

cat("Checkouts summary by group:\n")
print(trend_data %>% group_by(BookGroup) %>%
        summarise(Total = sum(TotalCheckouts), .groups = "drop"))
cat("\nYearly checkouts by group:\n")
print(as.data.frame(trend_data))

# =============================================================================
# 3. Parallel Trends Plot (raw scale)
# =============================================================================
trend_plot <- ggplot(trend_data, aes(x = CheckoutYear, y = TotalCheckouts,
                                     color = BookGroup, group = BookGroup)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2020.4, linetype = "dashed", color = "gray40") +
  annotate("text", x = 2020.5, y = Inf, label = "Treatment\n(May 2020)",
           hjust = 0, vjust = 1.2, size = 3.5, color = "gray30") +
  scale_y_log10(labels = comma) +
  labs(
    title = "Parallel Trends Analysis: Library Book Checkouts by Group",
    subtitle = "Log scale | Dashed line marks start of treatment period (May 2020)",
    x = "Year",
    y = "Total Checkouts (log scale)",
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
    panel.grid.minor = element_blank()
  )

ggsave("trend_plot.png", trend_plot, width = 10, height = 6, dpi = 300)
cat("\nPlot saved to trend_plot.png\n")
print(trend_plot)
