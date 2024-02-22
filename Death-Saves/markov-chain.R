# Death save simulation: Markov chain approach ####
# We need to do matrix powers for this, so source the function.
source(here::here("Death-Saves", "mat-pow.R"))

# First write down the initial state -- this is ALWAYS the 00 state, so that
# gets a one and all the other states have zeros.
init <- matrix(c(1, rep(0, times = 11)), ncol = 1)

# Now write down the transition matrix. Each row of this refers to beginning
# in a specific state, which is identified by the comment above that row.
# Then the row of probabilities is the probability of going to each state in
# order.
states <- c("00", "01", "02", "10", "11", "12", "20", "21", "22", "1hp",
            "Stable", "Dead")
P <- matrix(
  c(
    # 00
    0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    # 01
    0.40, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    # 02
    0.05, 0.40, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    # 10
    0.50, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    # 11
    0.00, 0.50, 0.00, 0.40, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    # 12
    0.00, 0.00, 0.50, 0.05, 0.40, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    # 20
    0.00, 0.00, 0.00, 0.50, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    # 21
    0.00, 0.00, 0.00, 0.00, 0.50, 0.00, 0.40, 0.00, 0.00, 0.00, 0.00, 0.00,
    # 22
    0.00, 0.00, 0.00, 0.00, 0.00, 0.50, 0.05, 0.40, 0.00, 0.00, 0.00, 0.00,
    # 1hp
    0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 1.00, 0.00, 0.00,
    # Stable
    0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.50, 0.50, 0.50, 0.00, 1.00, 0.00,
    # Dead
    0.00, 0.05, 0.45, 0.00, 0.05, 0.45, 0.00, 0.05, 0.45, 0.00, 0.00, 1.00
  ),
  nrow = 12,
  ncol = 12
) |> t()

# Take the transpose again to get a column stochastic matrix to follow
# here https://www.math.umd.edu/~immortal/MATH401/book/ch_absorbing_markov_chains.pdf

rownames(P) <- colnames(P) <- states

# Now we can simulate runs of the Markov chain. We know that the three
# absorbing states will cause the Markov chain to reach a steady state by the
# end of five iterations, so that's all the runs we need to do.
res <-
  # Calculate the state distribution after 1 to 5 saving throws
  purrr::map(1:5, function(pow) {t(matpowfast(P, pow) %*% init)}) |>
  purrr::map(as.data.frame) |>
  purrr::map(\(d) `colnames<-`(d, states)) |>
  # Clean up the matrix for plotting
  purrr::list_rbind(names_to = ".iter") |>
  # Pivot longer for plotting
  tidyr::pivot_longer(
    cols = -.iter,
    names_to = "state",
    values_to = "prob"
  ) |>
  # Make state an ordered factor
  dplyr::mutate(
    state_f = factor(state, levels = states, ordered = TRUE)
  )

col_palette <- hcl.colors(length(states), "Dark 3")
plot(
  NULL,
  xlim = c(1, 5),
  ylim = c(0, 0.5),
  xlab = "Iteration",
  ylab = "Probability",
  axes = FALSE
)
axis(
  1,
  at = 1:5,
  labels = TRUE
)
axis(
  2,
  at = seq(0, 0.5, 0.25),
  labels = TRUE
)
for (i in 1:length(states)) {
  plot_dat <- res[res$state == states[i], ]
  lines(x = plot_dat$.iter, y = plot_dat$prob, col = col_palette[i])
}
legend("topright", legend = states, col = col_palette, lty = 1,
       ncol = 6)

# The markov chain transition matrix P (mat) is already in canonical form.
# The absorbing states are 1hp, Stable, and Dead, so we can extract the Q
# and R matrices.
absorbing_states <- c("1hp", "Stable", "Dead")
transient_states <- setdiff(states, absorbing_states)
Q <- P[transient_states, transient_states]
R <- P[absorbing_states, transient_states]

# https://en.wikipedia.org/wiki/Absorbing_Markov_chain
# Expected number of visits to transient states ####
# The actual formula for calculating N requires us to sum the powers up to
# infinity. However, for this chain we know that the steady state is reached
# # after 5 iterations, so we only need to sum up to five.
# N <-
#   purrr::map(1:5, function(pow) {matpowfast(t(Q), pow)}) |>
#   purrr::reduce(`+`)

# Because the Markov chain is absorbing, this is enough to guarantee the
# regularity condition that the column sums of Q^k will all be <1 for some k.
# So that infinite sum actually converges to (I - Q) ^ {-1}.
N <- solve(diag(ncol(Q)) - Q)


# Expected number of steps before being absorbed ####
t <- N %*% matrix(rep(1, length(transient_states)), ncol = 1)
# Interestingly, from t[00] we can see that it takes, on average, 2.63 death
# saves 

# Absorbing probabilities ####
# If you start in a given state, find the probability you end up in a given
# absorbing state
B <- R %*% N

# Transient visiting probabilities ####
# This gives the probability of visiting state j when starting from state i
N_dg <- diag(diag(N))
I_t <- diag(length(transient_states))
H <- (N - I_t) %*% solve(N_dg)

# Variance on number of transient visits ####
N_sq <- N * N # Hadamard product of N with itself
N_2 <- N %*% (2 * N_dg - I_t) - N_sq

# Variance on number of steps ####
t_2 <- (2 * N - I_t) %*% t - (t * t)

