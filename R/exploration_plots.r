bivariat_continuous_plot_poisson <- function() {
  list(
    geom_smooth(
      formula = y ~ s(x, bs = "ts")
      , method = "gam"
      , method.args = list(family = "poisson")
    )
    # , stat_summary_bin(
    #   geom = "pointrange"
    #   , fun.data = mean_qi
    # )
  )
}

bivariat_continuous_plot_lnorm <- function() {
  list(
    geom_smooth(
      formula = y ~ s(x, bs = "ts")
      , method = "gam"
      , method.args = list(family = "gaussian")
    )
    # , stat_summary_bin(
    #   geom = "pointrange"
    #   , fun.data = mean_se
    # )
  )
}
