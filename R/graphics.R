# graphics.R

make_traceplot <- function(data, beta_col) {
  na.omit(data) |>
    group_by(Chain, Method) |>
    mutate(Iteration = row_number()) |>
    ungroup() |>
    ggplot(aes(x = Iteration, y = get(beta_col))) +
    geom_line() +
    facet_wrap(~ Chain + Method, scales = 'free_y', labeller = label_wrap_gen(multi_line = FALSE)) +
    labs(y = beta_col)
}

make_posterior_plot <- function(data) {
  p <- 
    data |>
    ggplot(aes(x = Estimate, y = Method)) +
    stat_halfeye(.width = c(0.95, 0.5)) +
    labs(x = "Estimated health utility") + 
    theme(panel.spacing.x = unit(2, "lines"))
  
  if("xvar" %in% names(data)) {
    p <- p + facet_wrap(~xvar)
  }
  
  p
}

make_posteriors_table <- function(data) {
  data |>
    group_by(across(any_of(c("Method", "xvar")))) |>
    summarise(
      Mean = mean(Estimate),
      SD = sd(Estimate),
      Lower = quantile(Estimate, 0.025),
      Upper = quantile(Estimate, 0.975),
      Width = (quantile(Estimate, 0.975) - quantile(Estimate, 0.025))
    ) 
}
