### COMPARING METHODS OF D&D STATS GENERATION ###
# Author: Zane Billings
# Updated: 28 June, 2019
# This script will simulate several methods of generating character attribute
#  scores ("stats") for Dungeons and Dragons 5th edition characters.

# Load packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(magrittr)

# Standard array.
#   The player assigns the scores [15, 14, 13, 12, 10, 8] as they like.
generate_stats_with_standardarray <- function() {
  attributes <- c(15, 14, 13, 12, 10, 8)
}

# 3d6 method.
#   Each score is assigned by rolling three six-sided dice and taking the sum.
generate_stats_with_3d6 <- function() {
  attributes <- vector(mode = "numeric", length = 6)
  attributes <- 
    lapply(attributes,
           function(x) sum(base::sample(1:6, size = 3, replace = TRUE))
           )
  return(unlist(attributes))
}

# 3d6r1 method
#   Each score is assigned by rolling three six-sided dice and taking the sum.
#   In this method, all scores of 1 are re-rolled.
generate_stats_with_3d6r1 <- function() {
  attributes <- vector(mode = "numeric", length = 6)
  attributes <- 
    lapply(attributes,
           function(x) sum(base::sample(2:6, size = 3, replace = TRUE))
    )
  return(unlist(attributes))
}

# 2d6 + 6 method
#   Each score is assigned by rolling two six sided dice and taking the sum of
#   the results plus six.
generate_stats_with_2d6plus6 <- function() {
  attributes <- vector(mode = "numeric", length = 6)
  attributes <- 
    lapply(attributes,
           function(x) sum(base::sample(1:6, size = 2, replace = TRUE) + 6)
    )
  return(unlist(attributes))
}

# 4d6k3 method
#   Each score is assigned by rolling four six-sided dice. The lowest value is
#   removed and the remaining three results are summed.
generate_stats_with_4d6k3 <- function() {
  attributes <- vector(mode = "numeric", length = 6)
  raw <- 
    lapply(attributes,
           function(x) base::sample(1:6, size = 4, replace = TRUE)
    )
  attributes <- 
    lapply(raw,
           function(x) sum(x) - min(x)
    )
  return(unlist(attributes))
}

# 4d6k3r1 method
#   Each score is assigned by rolling four six-sided dice. The lowest value is
#   removed and the remaining three results are summed. All dice rolls of 1 are
#   rerolled before anything else is done.
generate_stats_with_4d6k3r1 <- function() {
  attributes <- vector(mode = "numeric", length = 6)
  raw <- 
    lapply(attributes,
           function(x) base::sample(2:6, size = 4, replace = TRUE)
    )
  attributes <- 
    lapply(raw,
           function(x) sum(x) - min(x)
    )
  return(unlist(attributes))
}

# 4d6k3r1 alternate method
#   Each score is assigned by rolling four six-sided dice. The lowest value is
#   removed and the remaining three results are summed. All dice rolls of 1 are
#   rerolled after removing the fourth die, but before taking the sum.
generate_stats_with_4d6k3r1_alt <- function() {
  attributes <- vector(mode = "numeric", length = 6)
  raw <- 
    lapply(attributes,
           function(x) base::sample(1:6, size = 4, replace = TRUE)
    )
  replace_ones <- 
    lapply(raw,
           function(x) {
             unlist(
               lapply(x, 
                      function(y) ifelse(y == 1, sample(1:6, size = 1), y)
                    )
               )
           }
    )
  attributes <- 
    lapply(replace_ones,
           function(x) sum(x) - min(x)
    )
  return(unlist(attributes))
}

# d20 method
#   Each score is assigned by rolling one twenty-sided die.
generate_stats_with_1d20 <- function() {
  attributes <- vector(mode = "numeric", length = 6)
  attributes <- 
    lapply(attributes,
           function(x) sum(base::sample(1:20, size = 1))
    )
  return(unlist(attributes))
}

# Strengths/weaknesses method
#   2 stats are rolled as 3d6, 2 stats are rolled as 4d6k3, and 2 stats are
#   rolled as 6d6k3.
generate_stats_with_sw_method <- function() {
  attributes <- list()
  attributes[1] <- sum(sample(1:6, size = 3, replace = TRUE))
  attributes[2] <- sum(sample(1:6, size = 3, replace = TRUE))
  attributes[3] <- base::sample(1:6, size = 4, replace = TRUE)
  attributes[3] <- sum(attributes[3]) - min(attributes(3))
}


# Playing cards method #1 ("Ti-Bob")
#   A deck is composed of 12 playing cards: one four, three fives, two sixes,
#   two sevens, two eights, and two nines. The cards are randomly grouped into
#   six groups of two cards each. The values of the two cards are added, and
#   these six sums are the ability scores.


# Playing cards method #2 ("Arial Black")
#   This method is identical to the "Ti-Bob" method, except the deck replaces
#   one of the fives with a four. The original method calls for allowing a 
#   player to swap two cards but I have no desire to attempt to implement this.


# Playing cards method #3 ("Holy Grail")
#   A deck of 18 cards is created as follows: four twos, three threes, four
#   fours, three fives, and four sixes (constructable with a "standard deck" of
#   playing cards). The deck is randomly grouped into six piles of three cards
#   each, and the sum of the values of the cards in each piles becomes an
#   attribute score. This method claims to be the closest to producing equal
#   results to the standard array.









