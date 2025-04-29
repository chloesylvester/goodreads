#' Generated with the help of ChatGPT and roxygen2 
#' @title compare_users_goodreads
#' @description 
  #' The S3 method can be used to compare two users' Goodreads data on reading trends, including genres, book length, favorite authors, and reading goals.
#' @name compare_users_goodreads
#' @param user1 A data frame of Goodreads data for user 1.
#' @param user2 A data frame of Goodreads data for user 2.
#' @param user1_name A character string for the name of user 1 (e.g., "Chloe").
#' @param user2_name A character string for the name of user 2 (e.g., "Madison").
#' @param year Optional numeric value to filter books read in a given year.
#' @examples
  #' user1 <- import_goodreads("data/goodreads_library_export C.Sylvester.csv")
  #' user2 <- import_goodreads("data/goodreads_library_export M.Sylvester.csv")
  #' compare_users_goodreads(user1, user2, "Chloe", "Madison", year = 2023)

#' @export
compare_users_goodreads <- function(user1, user2, user1_name = "User1", user2_name = "User2", year = NULL) {
  structure(list(user1 = user1, user2 = user2, 
                 user1_name = user1_name, user2_name = user2_name, 
                 year = year), class = "compare_users_goodreads")
}

#' @export
print.compare_users_goodreads <- function(x, ...) {
  u1 <- x$user1
  u2 <- x$user2
  year <- x$year
  
  if (!is.null(year)) {
    u1$Year <- lubridate::year(as.Date(u1$`Date Read`, format = "%Y-%m-%d"))
    u2$Year <- lubridate::year(as.Date(u2$`Date Read`, format = "%Y-%m-%d"))
    u1 <- dplyr::filter(u1, Year == year)
    u2 <- dplyr::filter(u2, Year == year)
  }
  
  cat("=== Goodreads Comparison ===\n")
  cat(paste0("Year: ", ifelse(is.null(year), "All Years", year), "\n"))
  cat(paste0(x$user1_name, " vs ", x$user2_name, "\n\n"))
  
  # Books Read
  cat("Books Read:\n")
  cat(paste0(x$user1_name, ": ", nrow(u1), "\n"))
  cat(paste0(x$user2_name, ": ", nrow(u2), "\n\n"))
  
  # Average Page Count
  cat("Average Book Length (Pages):\n")
  cat(paste0(x$user1_name, ": ", round(mean(as.numeric(u1$`Number of Pages`), na.rm = TRUE), 1), "\n"))
  cat(paste0(x$user2_name, ": ", round(mean(as.numeric(u2$`Number of Pages`), na.rm = TRUE), 1), "\n\n"))
  
  # Top Authors
  top_authors <- function(df, name) {
    dplyr::count(df, Author) |> 
      dplyr::arrange(desc(n)) |> 
      dplyr::slice(1:5) |>
      dplyr::mutate(User = name)
  }
  authors_df <- dplyr::bind_rows(top_authors(u1, x$user1_name), top_authors(u2, x$user2_name))
  print(ggplot2::ggplot(authors_df, ggplot2::aes(x = reorder(Author, n), y = n, fill = User)) +
          ggplot2::geom_col(position = "dodge") +
          ggplot2::coord_flip() +
          ggplot2::labs(title = "Top Authors Compared", x = "Author", y = "Books") +
          ggplot2::theme_minimal())
  
  # Genres
  genre_summary <- function(df, name) {
    df |>
      tidyr::separate_rows(Bookshelves, sep = ",") |>
      dplyr::filter(Bookshelves != "") |>
      dplyr::count(Bookshelves) |>
      dplyr::top_n(5, n) |>
      dplyr::mutate(User = name)
  }
  genre_df <- dplyr::bind_rows(genre_summary(u1, x$user1_name), genre_summary(u2, x$user2_name))
  print(ggplot2::ggplot(genre_df, ggplot2::aes(x = reorder(Bookshelves, n), y = n, fill = User)) +
          ggplot2::geom_col(position = "dodge") +
          ggplot2::coord_flip() +
          ggplot2::labs(title = "Genre Preferences", x = "Genre", y = "Count") +
          ggplot2::theme_minimal())
  
  # Ratings
  ratings <- dplyr::bind_rows(
    data.frame(User = x$user1_name, Rating = as.numeric(u1$`My Rating`)),
    data.frame(User = x$user2_name, Rating = as.numeric(u2$`My Rating`))
  )
  print(ggplot2::ggplot(ratings, ggplot2::aes(x = factor(Rating), fill = User)) +
          ggplot2::geom_bar(position = "dodge") +
          ggplot2::labs(title = "Rating Distributions", x = "Rating", y = "Count") +
          ggplot2::theme_minimal())
}
