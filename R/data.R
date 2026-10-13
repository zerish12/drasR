#' Example Lot Inspection Data
#'
#' A simulated lot-level inspection dataset for demonstrating
#' drift-aware risk-guaranteed acceptance sampling. The data represent
#' 200 production lots under a sudden deterioration scenario, where
#' the true defect probability increases halfway through the sequence.
#'
#' @format A data frame with 200 rows and 4 variables:
#' \describe{
#'   \item{lot}{Lot index.}
#'   \item{p_true}{True lot-level defect probability used in simulation.}
#'   \item{lot_size}{Number of units in the lot.}
#'   \item{true_defects}{Simulated number of true defective units in the lot.}
#' }
#'
#' @details
#' This dataset is intended for examples, tests, and vignettes. It is not
#' a real industrial dataset. Users can apply the same functions to their
#' own lot-level inspection data containing defect rates or defect counts.
#'
#' @examples
#' data(example_lot_data)
#' head(example_lot_data)
"example_lot_data"
