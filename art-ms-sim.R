#' ART-MS financials
#'
#' Details ...
#'
#' @param n_years `integer(1)`. The no. years to project out the simulation.
#' @param with_codirector `logical(1)`. Include a co-director. This
#'   effects the max number of students in the program, either 24 or 16.
#' @param oper_budget `numeric(1)`. The operating budget coming from CSU.
#' @param diff_amt `numeric(1)`. The differential amount per credit hour.
#' @param n_credits `numeric(1)`. The number of credit hours each student
#'   must take and therefor pay for via the differential amount.
#'
#' @return A data frame of the simulated annual revenue over the
#'   time span defined by `n_years` and a `type` variable indicating
#'   whether a co-director was hired.
#'
#' @examples
#' sim_projection()
#' sim_projection(with_codirector = FALSE)
#' @export
sim_projection <- function(with_codirector = TRUE, n_years = 10L,
                           oper_budget     = 145600.00) {

  # calc Univ tuition revenue
  calc_revenue <- function(n1, n2, base1 = 14267.18, base2 = 30955.38) {
    inf    <- runif(1, 1.01, 1.03)   # random inflation 1% - 3%
    base   <- (n1 * base1 * inf) +  (n2 * base2 * inf)
    base - oper_budget
  }

  # salaries: this comes out of the
  # operating budget; thus Univ. does
  # not care much about this vis-a-vis revenue
  # -------
  director <- 37795.00   # 3mo
  director_co <- ifelse(with_codirector, 22500.00, 0.00)  # 3mo
  tech1 <- 69830.00 # 12mo
  tech2 <- 58000.00 # 12mo
  tech3 <- 20000.00 # 12mo (Zell's old position)
  total_salaries <- sum(director, director_co, tech1, tech2, tech3)
  maintenance <- 9000.00  # equipment maintenance/yr
  supplies <- 3882.00     # per student per yr
  total_expenses <- (n_students * supplies) + maintenance + total_salaries
  diff_amt <- 250.00
  n_credits <- 30
  # -------

  n_students <- ifelse(with_codirector, 24, 16)
  n_decline  <- withr::with_seed(101, sample(1:3, 10, replace = TRUE))
  revenue <- numeric(n_years)

  for ( i in seq(n_years) ) {
    n_total <- n_students - n_decline[i]
    n_nonres <- rbinom(1L, n_total, prob = 0.7)
    n_res <- n_total - n_nonres
    revenue[i] <- calc_revenue(n_res, n_nonres)
  }

  df <- setNames(data.frame(as.list(revenue)), 1:n_years)
  cbind(type = ifelse(with_codirector, "codirector", "no_codirector"), df)
}

n <- 100L
outsim <- lapply(rep(1:0, each = n), sim_projection, n_years = 10L) |>
  do.call(what = "rbind") |>
  tibble::rowid_to_column("simulation")

plot_df <- outsim |>
  tidyr::pivot_longer(cols = c(-type, -simulation),
                      names_to = "year", values_to = "revenue") |>
  tidyr::unite("simulation", c(type, simulation), remove = FALSE) |>
  dplyr::mutate(year = as.numeric(year))

# Plot all traces
plot_df |>
  ggplot(aes(x = year, y = revenue, group = simulation, color = type)) +
    geom_line(alpha = 0.3) +
    scale_color_manual(values = c("steelblue", "#00A499")) +
    guides(color = guide_legend(override.aes = list(size = 5, alpha = 1))) +
    labs(title = "Total Revenue Comparison With and Without a Co-director",
         x = "Year", y = "Total Revenue")

split_revenue <- tapply(outsim, outsim$type, `[`, x = ncol(outsim))
means <- vapply(split_revenue, mean, 0.1)
means

data.frame(split_revenue) |>
  tidyr::gather(key = "type") |>
  ggplot(aes(x = value, color = type)) +
  geom_density(aes(fill = type), alpha = 0.3, linewidth = 0.2) +
  labs(
    title = "Total Revenue Comparison With and Without a Co-director",
    y = "Prob. Density", x = "Total Revenue (10yr)") +
  scale_fill_manual(values = c("steelblue", "#00A499")) +
  scale_color_manual(values = c("steelblue", "#00A499")) +
  scale_x_continuous(labels = scales::dollar) +
  geom_vline(xintercept = means, linetype = "dashed")
