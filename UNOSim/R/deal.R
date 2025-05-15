#' Deal hands to players
#'
#' Deals 7 cards to each player and returns a list of hands.
#'
#' @param deck A data frame of UNO cards.
#' @param n_players Number of players.
#' @return A list of player hands and remaining deck.
#' @export
deal_hands <- function(deck, n_players = 4) {
  hands <- vector("list", n_players)
  for (i in 1:n_players) {
    hands[[i]] <- deck[1:7, ]
    deck <- deck[-(1:7), ]
  }
  names(hands) <- paste0("Player_", 1:n_players)
  list(hands = hands, deck = deck, discard = deck[1, , drop = FALSE], deck = deck[-1, ])
}


