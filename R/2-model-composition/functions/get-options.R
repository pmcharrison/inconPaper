get_options <- function(boot_perm_imp = TRUE) {
  corpora <- c(
    Classical = "classical", 
    Popular = "popular", 
    Jazz = "jazz")
  attr(corpora, "detail") <- c(
    "Classical scores",
    "Popular transcriptions",
    "Jazz lead sheets"
  )
  features <- c(
    `Chord size` = "chord_size",
    `Spectral interference` = "hutch_78_roughness",
    `Periodicity` = "har_18_harmonicity"
  )
  reverse <- character() # c("hutch_78_roughness")
  absence <- character() # c("hutch_78_roughness")
  stopifnot(all(reverse %in% features), !anyDuplicated(reverse),
            all(absence %in% features), !anyDuplicated(absence))
  
  only_observed <- FALSE # model only observed chord types
  trim_marginals <- c(-2, 2) # trim the x-axis of the marginal plots
  
  # 10,000 takes about 4 hours
  boot_rep <- 100000
  
  degrees <- 1:4
  
  plot <- list(type = "pdf", dpi = 600)
  # plot <- list(type = "png", dpi = 600)
  
  as.list(environment())
}
