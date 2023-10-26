
# Enumerate counts for one stat roll
one_roll_stats <-
# First enumerate all combinations of 4d6
	expand.grid(1:6, 1:6, 1:6, 1:6) |>
	# Now sort the rolls and remove the first column (hence drop the lowest of)
	# the four rolls
	apply(1, sort) |>
	t() |>
	(\(x) x[,-1])() |>
	# Now add each roll to get the distribution of stats
	apply(1, sum)

# Now we can see how complicated this is because to get the distribution of
# a single stat roll, we need to cross this six times. Which will be an
# enormous matrix. So we need to brainstorm a better way to enumerate this.
# Now we know the probability that P(roll 1 = n) for all n in the support, so
# we can say that, e.g. P(all 1's) = (that probability) ^ 6 and in general
# since the rolls are independent we can multiply them, which will still
# give us a big matrix, but it will be 16 ^ 6 elements (~17M) which we can at
# least store in memory. on my machine anyways

orp <- one_roll_stats |>
	table() |>
	prop.table() |>
	tibble::as_tibble() |>
	`colnames<-`(c('value', 'p')) |>
	dplyr::mutate(value = as.integer(value))

enum_sums <- expand.grid(3:18, 3:18, 3:18, 3:18, 3:18, 3:18)

sums_long <-
	enum_sums |>
	# We have to add an ID or tidyr will freak out when we pivot back to wide
	dplyr::mutate(
		id = dplyr::row_number(),
		roll_sum  = Var1 + Var2 + Var3 + Var4 + Var5 + Var6
	) |>
	tidyr::pivot_longer(
		cols = tidyselect::contains("Var")
	) |>
	dplyr::left_join(orp, by = "value") |>
	dplyr::select(-value) |>
	tidyr::pivot_wider(names_from = name, values_from = p) |>
	# Add total probability column (multiply for independent events!)
	dplyr::mutate(
		roll_prob = Var1 * Var2 * Var3 * Var4 * Var5 * Var6
	)

plot(
	x = 1:nrow(sums_long),
	y = cumsum(sums_long$roll_prob),
	type = "l",
	xlab = "n",
	ylab = "cumulative probability"
)

sums_probabilities <-
	sums_long |>
	dplyr::group_by(roll_sum) |>
	dplyr::summarise(sum_p = sum(roll_prob), .group = "drop")

barplot(
	sum_p ~ roll_sum,
	data = sums_probabilities,
	xlab = "Sum of stat scores",
	ylab = "Probability"
)

sorted_df <-
	enum_sums |>
	as.matrix() |>
	# This sorts each row of the matrix, IDK how it works.
	# https://stackoverflow.com/questions/9506442/fastest-way-to-sort-each-row-of-a-large-matrix-in-r
	(\(a) matrix(a[order(row(a), a)], ncol = ncol(a), byrow = TRUE))() |>
	`colnames<-`(1:6) |>
	tibble::as_tibble()

osp <-
	sorted_df |>
	dplyr::mutate(id = dplyr::row_number()) |>
	tidyr::pivot_longer(cols = -c('id')) |>
	dplyr::group_by(name, value) |>
	dplyr::count() |>
	dplyr::left_join(orp, by = "value") |>
	# Needs reweighting to deal with 4d6k3 instead of uniformly distributed
	# stat scores
	dplyr::mutate(
		w = n * p
	) |>
	dplyr::ungroup(value) |>
	dplyr::mutate(prob = w / sum(w)) |>
	dplyr::ungroup()

library(ggplot2)
ggplot(osp) +
	aes(x = value, y = prob, color = factor(name), group = factor(name)) +
	geom_line(lwd = 0.75) +
	geom_point(size = 3) +
	theme_minimal() +
	scale_x_continuous(
		breaks = seq(3, 18, 1),
		minor_breaks = c(),
		labels = seq(3, 18, 1)
	) +
	labs(x = "Stat score", y = "Probability", color = "Order stat") +
	theme(legend.position = "bottom") +
	scale_color_viridis_d(end = 0.9)
