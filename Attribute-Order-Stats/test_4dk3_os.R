
roll_4d6k3 <- function() {
	roll <- sample(1:6, size = 4, replace = TRUE)
	out <- sort(roll, decreasing = TRUE)[1:3]
	return(out)
}

roll_stats <- function(n_stats = 6L) {
	return(sapply(1:n_stats, \(x) sum(roll_4d6k3())))
}

set.seed(100)
N_sims <- 1e6L
res <- purrr::map(1:N_sims, \(x) sort(roll_stats()))
res_df <- do.call(rbind, res) |>
	`colnames<-`(paste0("stat", 1:6)) |>
	tibble::as_tibble()

res_long <-
	res_df |>
	tidyr::pivot_longer(
		cols = dplyr::everything(),
		names_to = "roll",
		names_prefix = "stat"
	)

library(ggplot2)
res_counts <-
	res_long |>
	dplyr::group_by(roll, value) |>
	dplyr::summarise(
		count = dplyr::n(),
		.groups = "drop_last"
	) |>
	dplyr::mutate(prop = count / sum(count)) |>
	dplyr::ungroup()

ggplot(res_counts) +
	aes(x = value, y = prop, group = roll, color = roll) +
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

res_df |>
	dplyr::mutate(sum = stat1 + stat2 + stat3 + stat4 + stat5 + stat6) |>
	dplyr::count(sum) |>
	dplyr::mutate(prop = n / sum(n)) |>
	ggplot() +
	aes(x = sum, y = prop) +
	geom_line(lwd = 0.75) +
	geom_point(size = 3) +
	theme_minimal() +
	labs(x = "Sum of stats", y = "Probability") +
	theme(legend.position = "bottom") +
	scale_color_viridis_d(end = 0.9)
 
