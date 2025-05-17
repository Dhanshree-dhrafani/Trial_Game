# Load your package (after building it locally)
# devtools::load_all(".")  # If developing
# library(unoSim)          # If installed via devtools::install()

# Step 1: Create the full deck
deck <- create_uno_deck()
cat("Deck created with", nrow(deck), "cards.\n")

# Step 2: Deal cards to players
n_players <- 4
dealt <- deal_hands(deck, n_players)

# Show initial hands
for (i in seq_along(dealt$hands)) {
  cat("\n", names(dealt$hands)[i], "'s hand:\n", sep = "")
  print(dealt$hands[[i]])
}

cat("\nTop discard card:\n")
print(dealt$discard)

# Step 3: Simulate full UNO game
cat("\nPlaying a game of UNO with", n_players, "players...\n\n")
game_result <- play_game(n_players)

# Step 4: Show final result
cat("\n✅ Game over!\n")
cat("🎉 Winner:", game_result$winner, "\n")

# Step 5: Show remaining cards for each player
cat("\n📦 Final hands:\n")
print(score_game(game_result))

# Optional: View discard pile
# View(game_result$discard)

