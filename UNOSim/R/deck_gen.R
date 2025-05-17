library(tidyverse)

# Define the components of the deck
colors <- c("red", "green", "blue", "yellow")
numbers <- 0:9
actions <- c("skip", "reverse", "+2")
wilds <- c("wild", "wild_draw4")

#' Create UNO Deck
#'
#' Generates the full 108-card UNO deck with number, action, and wild cards.
#' @return A data frame of UNO cards with columns: color, value, type
#' @examples
#' deck <- create_uno_deck()
#' head(deck)
#' @export
create_uno_deck <- function() {
  # Generate Number Cards
  number_cards <- expand_grid(
    color = colors,
    value = as.character(numbers)
  ) %>%
    mutate(type = "number") %>%
    group_by(value) %>%
    mutate(replication = if_else(value == "0", 1, 2)) %>%
    ungroup() %>%
    slice(rep(1:n(), replication)) %>%
    select(-replication)

  # Generate Action Cards (2 of each per color)
  action_cards <- expand_grid(
    color = colors,
    value = actions
  ) %>%
    mutate(type = "action") %>%
    slice(rep(1:n(), 2))

  #  Generate Wild Cards (4 of each, no color)
  wild_cards <- expand_grid(
    color = "wild",
    value = wilds
  ) %>%
    mutate(type = "wild") %>%
    slice(rep(1:n(), 4))

  # Combine All Cards into a Deck
  deck <- bind_rows(number_cards, action_cards, wild_cards) %>%
    arrange(color, type, value)

  return(deck)
}
