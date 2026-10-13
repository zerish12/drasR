test_that("tune_dras_policy returns valid tuning table", {
  res <- tune_dras_policy(n_reps = 3, scenario = "sudden_deterioration", seed = 123)
  expect_true(is.data.frame(res))
  expect_true(all(c("lambda", "warning_threshold", "tighten_factor") %in% names(res)))
  expect_true("safety_cost_score" %in% names(res))
  expect_true(nrow(res) >= 1)
})
