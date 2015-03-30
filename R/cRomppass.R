#' cRomppass
#'
#' @name cRomppass
#' @docType package
NULL

#' Test data for comppass. Includes 27 IP's from the human interactome
#' project. The column names are as follows:
#'
#' \itemize{
#'   \item Experiment.ID A unique identifier for the AP-MS experiment
#'   \item Replicate Some label for replicates, eg "A" or "B"
#'   \item Experiment.Type This is provided for backwards compatability with other software and is ignored
#'   \item Bait The name of the bait used in the AP-MS experiment
#'   \item Prey The name of the prey found in the AP-MS experiment
#'   \item Spectral.Count Number of spectral counts for that given prey in the experiment
#' }
#'
#' @docType data
#' @keywords datasets
#' @name comppass.test.data
#' @usage data(comppass_test_data)
#' @format A data frame with 27,164 rows and 6 variables
NULL
