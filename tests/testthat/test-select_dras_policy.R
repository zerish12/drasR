test_that("select_dras_policy returns three policy types", {
  tuned <- tune_dras_policy(
    n_reps = 2,
    n_lots = 30,
    scenario = "sudden_deterioration",
    lambda_values = c(0.20, 0.40),
    warning_values = c(0.03),
    tighten_values = c(1.50, 2.00),
    seed = 123
  )

  selected <- select_dras_policy(tuned)

  expect_true(is.data.frame(selected))
  expect_equal(nrow(selected), 3)
  expect_true("policy_type" %in% names(selected))
  expect_true(all(c("Low-cost DRAS", "Balanced DRAS", "High-safety DRAS") %in% selected$policy_type))
})
