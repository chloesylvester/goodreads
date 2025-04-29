#' Generated with the help of ChatGPT and roxygen2
#' #' @title visualize_genres
#' @description 
  #' Will go through and sort the "Bookshelves" column to help visualize the most common genres or tags associated with the book. 
  #' Helps see what types of books the user gravitates towards/ most likely to choose.
#' @name visualize_genres
#' @param data A data frame of Goodreads data, ideally imported using \code{import_goodreads()}.
#' @param top_n An integer specifying how many of the top genres to show (default is 10).
#' @examples 
  #' data <- import_goodreads("data/goodreads_export.csv")
  #' visualize_genres(data, top_n = 8)
#' @importFrom tidyr separate_rows
#' @importFrom dplyr filter group_by summarise arrange desc
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_minimal
#' @export

visualize_genres <- function(data, top_n = 10) {
  genre_data <- data |>
    tidyr::separate_rows(Bookshelves, sep = ",") |>
    dplyr::filter(Bookshelves != "") |>
    dplyr::group_by(Bookshelves) |>
    dplyr::summarise(count = n()) |>
    dplyr::arrange(dplyr::desc(count)) |>
    head(top_n)
  
  ggplot2::ggplot(genre_data, ggplot2::aes(x = reorder(Bookshelves, count), y = count)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Top Genres in Goodreads Library",
                  x = "Genre (Bookshelf)",
                  y = "Number of Books") +
    ggplot2::theme_minimal()
}
