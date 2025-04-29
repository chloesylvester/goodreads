#' Generated with the help of ChatGPT and roxygen2
#' @title predict_next_year
#' @description 
 #' Predict next year's reading goal, average book length, and top genre preferences based on historical Goodreads data.
#' @name predict_next_year
#' @param data A data frame of Goodreads export data. Must include 'Date Read', 'Number of Pages', and 'Bookshelves'.
#' @param current_year Numeric value of the current year (e.g., 2024). Used to calculate trends.
#' @examples 
  #' data <- import_goodreads("goodreads_export.csv")
  #' predict_next_year(data, 2024)

#' @export
predict_next_year <- function(data, current_year) {
  library(dplyr)
  library(lubridate)
  library(tidyr)
  
  data <- data %>%
    mutate(
      DateRead = as.Date(`Date Read`, format = "%Y-%m-%d"),
      Year = year(DateRead),
      Pages = as.numeric(`Number of Pages`)
    ) %>%
    filter(!is.na(Year), !is.na(Pages))
  
  # Group by year
  yearly_summary <- data %>%
    group_by(Year) %>%
    summarise(
      books_read = n(),
      avg_pages = mean(Pages, na.rm = TRUE)
    ) %>%
    filter(Year <= current_year)
  
  # Linear model to predict next year
  model_books <- lm(books_read ~ Year, data = yearly_summary)
  model_pages <- lm(avg_pages ~ Year, data = yearly_summary)
  
  next_year <- current_year + 1
  predicted_books <- round(predict(model_books, newdata = data.frame(Year = next_year)))
  predicted_pages <- round(predict(model_pages, newdata = data.frame(Year = next_year)))
  
  # Genre preferences
  genre_summary <- data %>%
    filter(Year == current_year) %>%
    separate_rows(Bookshelves, sep = ",") %>%
    filter(Bookshelves != "") %>%
    count(Bookshelves, sort = TRUE) %>%
    slice_head(n = 3)
  
  # Output
  cat("=== Prediction for", next_year, "===\n")
  cat("Predicted Reading Goal (Books):", predicted_books, "\n")
  cat("Predicted Average Book Length:", predicted_pages, "pages\n")
  cat("Top Genre Preferences:\n")
  print(genre_summary)
  
  invisible(list(
    predicted_books = predicted_books,
    predicted_pages = predicted_pages,
    top_genres = genre_summary
  ))
}
