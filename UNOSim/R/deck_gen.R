library(tidyverse)

# Define the components of the deck
colors <- c("red", "green", "blue", "yellow")
numbers <- 0:9
actions <- c("skip", "reverse", "+2")
wilds <- c("wild", "wild_draw4")

# Function to create the full UNO deck
create_uno_deck <- function() {
  # 🃏 1️⃣ Generate Number Cards
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

  # 🃏 2️⃣ Generate Action Cards (2 of each per color)
  action_cards <- expand_grid(
    color = colors,
    value = actions
  ) %>%
    mutate(type = "action") %>%
    slice(rep(1:n(), 2))

  # 🃏 3️⃣ Generate Wild Cards (4 of each, no color)
  wild_cards <- expand_grid(
    color = "wild",
    value = wilds
  ) %>%
    mutate(type = "wild") %>%
    slice(rep(1:n(), 4))

  # 🃏 4️⃣ Combine All Cards into a Deck
  deck <- bind_rows(number_cards, action_cards, wild_cards) %>%
    arrange(color, type, value)

  return(deck)
}

# Create the UNO deck
uno_deck <- create_uno_deck()
uno_deck

Card Breakdown:
# Colored Cards (76 cards total):
#There are 4 colors: Red, Green, Blue, Yellow. Each color contains:
#   Number Cards (0–9):
#   0: 1 card per color → 4 cards in total.
# 1–9: 2 cards each per color → 18 cards per color.
# Total for number cards → 76 cards (19 per color × 4 colors).
#  Action Cards (24 cards total):
#   Each color also has:
#   Skip: 2 cards
# Reverse: 2 cards
# Draw Two (+2): 2 cards
# For all four colors, it sums up to:
#   6 action cards per color × 4 colors = 24 action cards
# ️ Wild Cards (8 cards total):
#   These are colorless and can be played on any color:
#   Wild: 4 cards
# Wild Draw Four: 4 cards
#  Summary of the Deck Composition:
#   Type	Count
# Number Cards (0–9)	76
# Action Cards	24
# Wild Cards	8
# Total	108

