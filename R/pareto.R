
#' Compute a Safety-Cost Pareto Frontier
#'
#' Identifies Pareto-efficient policies where lower inspection burden
#' and lower false acceptance are preferred.
#'
#' @param x Data frame containing policy results.
#' @param cost_col Column name for inspection burden.
#' @param safety_col Column name for false acceptance burden.
#'
#' @return Data frame of Pareto-efficient rows.
#' @export
#'
#' @examples
#' df <- data.frame(cost = c(100, 120, 150), risk = c(5, 3, 2))
#' pareto_frontier(df, cost_col = "cost", safety_col = "risk")
pareto_frontier <- function(x,
                            cost_col = "mean_avg_sample_number",
                            safety_col = "mean_false_accepts") {

  if (!all(c(cost_col, safety_col) %in% names(x))) {
    stop("cost_col and safety_col must be columns in x.")
  }

  x <- x[order(x[[cost_col]], x[[safety_col]]), , drop = FALSE]

  keep <- rep(FALSE, nrow(x))
  best_safety <- Inf

  for (i in seq_len(nrow(x))) {
    if (x[[safety_col]][i] < best_safety) {
      keep[i] <- TRUE
      best_safety <- x[[safety_col]][i]
    }
  }

  x[keep, , drop = FALSE]
}

