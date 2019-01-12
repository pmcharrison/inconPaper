compile_data <- function() {
  message("Counting chords...")
  count_chords()
  
  message("Adding features...")
  add_features(models = c("hutch_78_roughness",
                          "har_18_harmonicity"))
}
