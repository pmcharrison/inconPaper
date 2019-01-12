get_data <- function(opt) {
  df <- readRDS("output/composition-chord-counts-features.rds")
  for (rev in opt$reverse) 
    df[[rev]] <- - df[[rev]]
  df
}
