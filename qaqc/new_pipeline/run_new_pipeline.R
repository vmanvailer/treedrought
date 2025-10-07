run_new_pipeline <- function(raw_data) {
  # Orchestrates your 6 functions
  step1 <- script_01_calc(raw_data)
  step2 <- script_02_drought(step1)
  ...
  return(final_new_result)
}
