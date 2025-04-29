#' Generated with the help of ChatGPT and roxygen2 
#' @title track_reading_goals
#' @description - 
  #' Calculate and visualize progress toward a user's yearly reading goal.
  #' This code will count how many books were marked as "read" compared to "currently reading" or "want to read" within the given year and compares it to the goal set. 
#' @name track_reading_goals
#' @param data A data frame of Goodreads book data, ideally read using \code{import_goodreads()}.
#' @param year Numeric value indicating the year to track (e.g., 2023).
#' @param goal Numeric value for the target number of books to read that year.
#' @return A ggplot bar chart comparing books read to the goal.
#' @examples 
  #' data <- import_goodreads("data/goodreads_export.csv")
  #' track_reading_goals(data, year = 2023, goal = 50)
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot aes geom_col geom_hline labs theme_minimal
#' @importFrom lubridate year
#' @export

track_reading_goals <- function(data, year, goal) {
  data$ReadDate <- lubridate::year(as.Date(data$`Date Read`, format = "%Y-%m-%d"))
  yearly_data <- dplyr::filter(data, ReadDate == year)
  books_read <- nrow(yearly_data)
  
  df <- data.frame(Category = c("Goal", "Read"), Count = c(goal, books_read))
  
  ggplot2::ggplot(df, ggplot2::aes(x = Category, y = Count, fill = Category)) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = goal, linetype = "dashed", color = "red") +
    ggplot2::labs(title = paste("Reading Progress for", year),
                  y = "Number of Books",
                  x = NULL) +
    ggplot2::theme_minimal()
}
