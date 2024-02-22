# DND death probability ####
# When your HP goes to zero, you start making death saving throws.
# You die when you get three failures, which is a roll of 10 or less. However,
# a 1 counts as two failures so the probability is slightly higher than
# a naive estimate with no extra rules.

# However, we can't just look at the probability of three rolls because it's
# possible for someone to make up to five death saving throws -- two fails and
# three successes or two successes and three fails. Or if you get a 20,
# you automatically live.

# While calculating the possibility analytically is possible, it's much easier
# to code a simulation and run it multiple times.

death_score <- function(x) {
  return(sum(x < 10) + sum(x == 1))
}

live_score <- function(x) {
  return(sum(x >= 10))
}

one_death_sim <- function(..., verbose = FALSE) {
  
  # Simulate the rolls
  saves <- sample(1:20, size = 5, replace = TRUE)
  if(isTRUE(verbose)) {message(paste(saves, collapse = ", "))}
  
  # Check the first roll for a nat 20
  if (saves[1] == 20) {return(0)}
  
  # Check the first 2 rolls -- there is only one possible outcome
  # If they got a 1 and another failure they die, so we need to check.
  if (death_score(saves[1:2]) >= 3) {return(1)} 
  if (saves[2] == 20) {return(0)}
  
  # Now check the first 3 rolls.
  if (death_score(saves[1:3]) >= 3) {return(1)}
  if (live_score(saves[1:3]) >= 3) {return(0)}
  if (saves[3] == 20) {return(0)}
  
  # Now check the first 4
  if (death_score(saves[1:4]) >= 3) {return(1)}
  if (live_score(saves[1:4]) >= 3) {return(0)}
  if (saves[4] == 20) {return(0)}
  
  # And finally all of them
  if (death_score(saves[1:5]) >= 3) {return(1)}
  if (live_score(saves[1:5]) >= 3) {return(0)}
  if (saves[5] == 20) {return(0)}
}

# Do a million of those
main <- function(S = 370, sim_N = 1e6L) {
  set.seed(S)
  res <- purrr::map_int(1:sim_N, one_death_sim, .progress = "Simulating!")
  
  # Now calculate the probability
  prob <- mean(res) * 100
  msg <- paste0(
    "Probability of dying, estimated in ", sim_N, " simulations: ≈",
    sprintf("%.2f", prob), "%."
  )
  message(msg)
}

main()

# Note: when I did the simulation I got 40.49%.

# END OF FILE ####
