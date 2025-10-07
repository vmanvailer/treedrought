run_old_pipeline <- function(raw_data) {
  # Wrap your 14 tidyverse scripts in a function
  # They can remain as scripts if you just `source()` them in order.
  source("qaqc/old_pipeline/01_load.R")
  source("qaqc/old_pipeline/02_filter.R")
  ...
  return(final_old_result)
}
