#' Simulate a full UNO game
#'
#' Plays a full UNO game with random legal moves for each player.
#'
#' @param n_players Number of players (default = 4).
#' @return Final game state including winner.
#' @export
play_game <- function(n_players = 4) {
  state <- deal_hands(create_deck(), n_players)
  hands <- state$hands
  deck <- state$deck
  discard <- state$discard
  direction <- 1
  turn <- 1

  repeat {
    current_player <- paste0("Player_", turn)
    hand <- hands[[current_player]]
    top_card <- tail(discard, 1)

    # Find playable cards
    playable <- which(
      hand$color == top_card$color |
        hand$value == top_card$value |
        hand$color == "wild"
    )

    if (length(playable) == 0) {
      # Draw one card if nothing playable
      if (nrow(deck) == 0) stop("Deck exhausted!")
      drawn <- deck[1, , drop = FALSE]
      deck <- deck[-1, ]
      hand <- rbind(hand, drawn)
    } else {
      play_idx <- sample(playable, 1)
      discard <- rbind(discard, hand[play_idx, ])
      hand <- hand[-play_idx, ]
    }

    # Update hand
    hands[[current_player]] <- hand

    # Win condition
    if (nrow(hand) == 0) {
      message(current_player, " wins!")
      return(list(winner = current_player, hands = hands, discard = discard))
    }

    # Move to next player
    turn <- (turn + direction - 1) %% n_players + 1
  }
}
