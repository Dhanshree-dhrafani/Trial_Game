#' Create a full UNO deck
#'
#' Generates a standard 108-card UNO deck including colors, numbers, actions, and wilds.
#'
#' @return A shuffled data frame with 108 cards.
#' @export
create_deck <- function() {
  colors <- c("red", "green", "blue", "yellow")
  numbers <- 0:9
  actions <- c("skip", "reverse", "draw2")

  base_deck <- do.call(rbind, lapply(colors, function(col) {
    num_cards <- data.frame(color = col, type = "number", value = as.character(numbers))
    action_cards <- data.frame(color = col, type = "action", value = rep(actions, each = 2))
    # Add one 0, two of each 1–9 and action cards
    rbind(num_cards[1, ], num_cards[-1, ][rep(1:nrow(num_cards[-1, ]), 2), ], action_cards)
  }))

  wilds <- data.frame(
    color = "wild",
    type = "wild",
    value = rep(c("wild", "draw4"), each = 4)
  )

  full_deck <- rbind(base_deck, wilds)
  shuffled <- full_deck[sample(nrow(full_deck)), ]
  rownames(shuffled) <- NULL
  return(shuffled)
}

