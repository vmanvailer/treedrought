#' Load data (local-only function)
#'
#' This function was used internally to generate the example datasets
#' included in this package. It retrieves, cleans, and merges the
#' UDEL climate data, ITRDB chronologies, and metadata used for
#' drought resilience analysis.
#'
#' @details
#' The raw source data are not publicly distributed due to size and
#' data use restrictions. The function is retained here for
#' transparency and reference only.
#'
#' @param tree_ring_data_source Character. One of
#'   \code{"Detrended imputed"} or \code{"Raw"}.
#' @return A list containing:
#'   \itemize{
#'     \item \code{climate_udel_dt} — climate dataset
#'     \item \code{chron_itrdb_dt} — tree-ring chronology dataset
#'     \item \code{chron_itrdb_meta} — metadata table
#'     \item \code{thesis_clusters} — cluster assignments
#'   }
#' @examples
#' \dontrun{
#' thesis_data <- load_thesis_data("Detrended imputed")
#' }
#' @export
load_thesis_data <- function(tree_ring_data_source = "Detrended imputed") {
  message("This function requires local datasets not included in the package.\n",
          "The processed outputs are available under inst/extdata/.")
  return(invisible(NULL))
}
