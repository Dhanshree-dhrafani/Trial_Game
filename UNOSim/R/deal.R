deal_hands <- function(deck, n_players = 4) {
  # Shuffle the deck
  deck <- deck[sample(nrow(deck)), ]

  # Initialize empty hands
  hands <- vector("list", n_players)
  for (i in 1:n_players) {
    hands[[i]] <- deck[0, ]  # same structure, zero rows
  }
  names(hands) <- paste0("Player_", 1:n_players)

  # Deal 1 card at a time to each player for 7 rounds
  for (round in 1:7) {
    for (i in 1:n_players) {
      hands[[i]] <- rbind(hands[[i]], deck[1, ])
      deck <- deck[-1, ]
    }
  }

  # Take one card for discard
  discard <- deck[1, , drop = FALSE]
  deck <- deck[-1, ]

  return(list(
    hands = hands,
    deck = deck,
    discard = discard
  ))
}
