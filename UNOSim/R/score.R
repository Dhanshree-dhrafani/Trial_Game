#' Score the UNO game
#'
#' Returns remaining card counts per player.
#'
#' @param game_state Output from `play_game()`.
#' @return A named vector of card counts.
#' @export
score_game <- function(game_state) {
  sapply(game_state$hands, nrow)
}

