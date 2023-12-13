library("dplyr")

# Here is the editable list of movies. If you wanted to add a new movie, it would go on the end. # nolint: line_length_linter.
movie_list <- data.frame(Movies = c("Interstellar", "La La Land", "Revenge of the Sith", "The Secret Life of Walter Mitty", "John Wick", "The Social Network"), # nolint: line_length_linter.
  Ratings = c("PG-13", "PG-13", "PG-13", "PG", "R", "PG-13"),
  Genres = c("Action", "Musical", "Sci-Fi", "Adventure", "Action", "Documentary"), # nolint: line_length_linter.
  Runtimes = c(169L, 128L, 146L, 114L, 101L, 120L),
  AlreadyWatched = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  CriticScores = c(0.87, 0.94, 0.68, 0.54, 0.68, 0.95)
)

main <- function(movie_list) {
  print("Welcome to a movie database program written in the R language.")
  while (0 == 0) {
    print("There are 3 functionalities that you can choose from:")
    print("    1. Movie Recommendation")
    print("    2. Movie Statistics")
    print("    3. Average Critic Score")
    print("    4. Quit")
    print("Please enter the correlated number to run that functionality.")

    user_input <- as.integer(readline())
    switch(
      user_input,
      movie_recommendation(movie_list),
      movie_statistics(movie_list),
      test_critic_scores(),
      break
    )
  }
}

movie_recommendation <- function(movie_list) {
  print("Welcome to a movie recommendation program.")
  print("You'll be asked a series of questions that on what kind of movie you want to watch.") # nolint: line_length_linter.
  print("Enjoy!")
  print("If you don't have a preference, just hit ENTER to skip the question.")
  output <- movie_list

  # On each question, there is a correlated if statement that will slowly trim down the main list of movies until the end of the function. # nolint: line_length_linter.
  print("Would you like to watch a movie you haven't seen before?")
  already_watched <- tolower(readline())
  if (already_watched == "yes") {
    output <- dplyr::filter(output, AlreadyWatched == FALSE) # nolint
  } else if (already_watched == "no") {
    output <- dplyr::filter(output, AlreadyWatched == TRUE) # nolint
  }

  print("What rating of movie would you like to watch? (G, PG, PG-13, R)")
  rating <- toupper(readline())
  if (rating != "") {
    output <- dplyr::filter(output, Ratings == rating) # nolint
  }

  print("Is there a genre you enjoy?")
  genre <- tools::toTitleCase(readline())
  if (genre != "") {
    output <- dplyr::filter(output, Genres == genre) # nolint
  }

  print("What length of runtime would you like? (short, medium, long)")
  runtime <- tolower(readline())
  output <- switch(
    runtime,
    "short" = dplyr::filter(output, Runtimes <= 110), # nolint
    "medium" = dplyr::filter(output, between(Runtimes, 110, 145)),
    "long" = dplyr::filter(output, Runtimes >= 145),
    output
  )

  print("Here are the movies that fit your criteria:")
  print(output)
}

movie_statistics <- function(movie_list) {
  print("You can either look up a specific movie and view its individual information, or look up a genre and see general trends among them.") # nolint: line_length_linter.
  print("Type the name of a movie here to see if it is on the list. Hit ENTER to look up a genre.") # nolint: line_length_linter.

  specific_movie <- tools::toTitleCase(readline())
  if (specific_movie != "") {
    print("Here's the information we have on that movie: ")
    print(filter(movie_list, Movies == specific_movie), row.names = FALSE) # nolint
  } else {
    print("Type the name of the genre you are interested in: ")
    selected_genre <- tools::toTitleCase(readline())
    new_dataframe <- dplyr::filter(movie_list, Genres == selected_genre) # nolint
    print(paste0("Average Runtime: ", mean(new_dataframe$Runtimes, na.rm = FALSE), " minutes")) # nolint: line_length_linter.
    print(new_dataframe, row.names = FALSE)
    print(paste("Total movies: ", nrow(new_dataframe)))
  }
}

test_critic_scores <- function() {
  # For example's purposes, this has been detached from the main dataframe but could easily be grabbed from it. # nolint: line_length_linter.
  critic_scores <- list(0.87, 0.94, 0.68, 0.54, 0.68, 0.95)
  avg_score <- 0
  for (x in critic_scores) {
    avg_score <- avg_score + x
  }

  print(paste0("The average critic score of all the movies is ", round(avg_score / length(critic_scores) * 100, 0), "%.")) # nolint: line_length_linter.
}

main(movie_list)