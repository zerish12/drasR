test_that("plot_dras_frontier runs without error", {
  tuned <- tune_dras_policy(
    n_reps = 2,
    n_lots = 30,
    scenario = "sudden_deterioration",
    lambda_values = c(0.20, 0.40),
    warning_values = c(0.03),
    tighten_values = c(1.50),
    seed = 123
  )

  selected <- select_dras_policy(tuned)

  expect_silent(plot_dras_frontier(tuned, selected))
})
