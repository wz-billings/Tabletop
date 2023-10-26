num_rolls <- 100000

res <- data.frame(
  roll_one = numeric(num_rolls),
  roll_two = numeric(num_rolls),
  explode = numeric(num_rolls),
  sum = numeric(num_rolls)
)

for (i in 1:num_rolls) {
  roll = sample(1:6, size = 2, replace = TRUE)
  res$roll_one[i] <- roll[1]
  res$roll_two[i] <- roll[2]
  
  if(xor(roll[1] == 6, roll[2] == 6)) {
    res$explode[i] <- sample(1:6, size = 1)
  } else if (roll[1] == 6 & roll[2] == 6) {
    res$explode[i] <- sum(sample(1:6, size = 2, replace = TRUE))
  } else {
    res$explode[i] <- 0
  }
  
  res$sum[i] <- sum(roll) + res$explode[i]
}

res$explosions <- numeric(num_rolls)
res$explosions[res$roll_one == 6 & res$roll_two == 6] <- 2
res$explosions[xor(res$roll_one == 6, res$roll_two == 6)] <- 1
res$explosions[res$roll_one != 6 & res$roll_two != 6] <- 0

# P(no explode) = P(neither dice is 6) = (36 - 6 - 6 + 1) / 36 = 25/36.
par(mfrow = c(2,2))
barplot(table(res$sum), main = "Distribution of values")
barplot(table(res$sum[res$explosions == 2]), main = "Distribution of values given both rolls were 6")
barplot(table(res$sum[res$explosions == 1]), main = "Distribution of values given one roll was 6")
barplot(table(res$sum[res$explosions == 0]), main = "Distribution of values given neither roll was 6")
