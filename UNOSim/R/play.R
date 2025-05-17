#' Simulate a full UNO game
#'
#' Plays a full UNO game with random legal moves for each player.
#'
#' @param n_players Number of players (default = 4).
#' @return Final game state including winner.
#' @export
play_game <- function(n_players = 4) {
  state <- deal_hands(create_uno_deck(), n_players)
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
      played_card <- hand[play_idx, ]
      discard <- rbind(discard, played_card)
      hand <- hand[-play_idx, ]

      # Handle action/wild card effects
      if (played_card$value == "skip") {
        turn <- (turn + direction - 1) %% n_players + 1  # skip next player
      } else if (played_card$value == "reverse") {
        direction <- -direction
      } else if (played_card$value == "+2") {
        next_player <- paste0("Player_", (turn + direction - 1) %% n_players + 1)
        if (nrow(deck) >= 2) {
          hands[[next_player]] <- rbind(hands[[next_player]], deck[1:2, ])
          deck <- deck[-(1:2), ]
        }
        turn <- (turn + direction - 1) %% n_players + 1  # skip next player
      } else if (played_card$value == "wild_draw4") {
        next_player <- paste0("Player_", (turn + direction - 1) %% n_players + 1)
        if (nrow(deck) >= 4) {
          hands[[next_player]] <- rbind(hands[[next_player]], deck[1:4, ])
          deck <- deck[-(1:4), ]
        }
        turn <- (turn + direction - 1) %% n_players + 1
      }

      # For wild cards, choose random color
      if (played_card$value %in% c("wild", "wild_draw4")) {
        new_color <- sample(c("red", "blue", "green", "yellow"), 1)
        discard[nrow(discard), "color"] <- new_color
      }
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
