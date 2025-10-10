#' Climate data (UDEL)
#'
#' Monthly climate dataset (temperature, precipitation, latitude) used in drought analysis.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{Id}{Unique site identifier.}
#'   \item{Year}{Calendar year.}
#'   \item{Month}{Month number (1–12).}
#'   \item{TAve}{Mean monthly temperature (°C).}
#'   \item{Prec}{Total monthly precipitation (mm).}
#'   \item{Lat}{Latitude (decimal degrees).}
#' }
#' @source Derived from the UDEL climate dataset, prepared via \code{load_thesis_data()}.
#' @seealso \code{\link{load_thesis_data}}
"std_drought_clim"


#' Chronology data (ITRDB)
#'
#' Detrended tree-ring chronologies for drought resilience analysis.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{Id}{Unique site identifier.}
#'   \item{Year}{Calendar year.}
#'   \item{RES}{Standardized residual index.}
#'   \item{RWI}{Ring-width index.}
#' }
#' @source Derived from ITRDB data using \code{load_thesis_data()}.
"std_drought_chro"


#' Cluster grouping data
#'
#' Group identifiers for each chronology site, used to set \code{chron_group_col}.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{Id}{Unique site identifier.}
#'   \item{CLUSTER2}{Continuous integer numbering for all clusters in the dataset.}
#'   \item{CLUSTER3}{Continuous integer numbering for all cluster included in the analysis. i.e. CLUSTER3_STATUS == "Included"}
#'   \item{CLUSTER3_STATUS}{Character of included or not in the analysis.}
#'   \item{COLOR}{Cluster color}
#'   \item{name}{Region name for easier interpretation.}
#'   \item{group_col}{Continent delineation + cluster code.}
#' }
#' @source Derived from cluster analyses using Partition Around Medoids (PAM) done
#'         separately by each continent delineation and using elbow method for
#'         identifying ideal number clusters per continent delineation.
#'         Derived from \code{load_thesis_data()
"std_drought_clus"


#' Chronology metadata
#'
#' Metadata associated with the ITRDB chronologies.
#'
#' @format A data frame with 29 columns (including Id, site information, coordinates, species, etc.).
#' @source Extracted from the official ITRDB metadata using \code{load_thesis_data().
"std_drought_meta"


#' Sensitivity filter
#'
#' List of chronology site IDs excluded from analysis due to data quality or sensitivity thresholds.
#'
#' @format A data frame with one column:
#' \describe{
#'   \item{Id}{Unique site identifier excluded from the analysis.}
#' }
#' @source Created during sensitivity analysis as part of the thesis pipeline.
"std_drought_sens"
