library(magrittr)
library(tidyverse)
library(memoise)
library(incon)
library(hrep)
requireNamespace("incon")
requireNamespace("inconData")

incon <- memoise(incon::incon, cache = cache_filesystem("cache/incon"))
roughness_wang <- memoise(wang13::roughness_wang, 
                          cache = cache_filesystem("cache/roughness_wang"))

compile_bowling_data <- function() {
  inconData::bowl18 %>% 
    add_main_analyses() %>% 
    add_extra_wang_analyses() %>% 
    add_toolboxes() %>% 
    add_weights() %T>% 
    saveRDS("output/perception-bowling.rds")
}

add_main_analyses <- function(x) {
  message("Running main analyses...")
  select(x, pi_chord, pi_chord_int, tuning_tonic_pc_int) %>% 
    plyr::mlply(
      function(pi_chord, pi_chord_int, tuning_tonic_pc_int) 
        incon(
          x = pi_chord[[1]],
          model = readLines("R/1-model-perception/all-incon-mods.txt"), 
          x_int = pi_chord_int[[1]],
          par = list(gill_09_harmonicity = list(tonic = tuning_tonic_pc_int))
        ), .progress = "text") %>% 
    map(as.list) %>% map(as_tibble) %>% bind_rows() %>% 
    bind_cols(x, .)
}

add_extra_wang_analyses <- function(x) {
  message("Running additional Wang analyses...")
  mutate(x, wang_13_roughness_orig = plyr::laply(
    pi_chord,
    roughness_wang, 
    include_phase_impact_factors = TRUE, 
    msg = NULL,
    .progress = "text")
  )
}

add_toolboxes <- function(x) {
  x %>% 
    add_toolbox("mir", "output/mirtoolbox/mirroughness/v1/results.csv") %>%
    add_toolbox("ess", "output/essentia/v2/results.csv")
}

add_toolbox <- function(x, label, file) {
  y <- read_csv(file, col_types = cols())
  names(y) <- c("id", paste(label, names(y)[-1], sep = "."))
  z <- left_join(x, y, by = "id")
  stopifnot(nrow(x) == nrow(z))
  z
}

add_weights <- function(x) {
  counts <- group_by(x, chord_size) %>% summarise(n = n())
  mutate(x, weight = plyr::mapvalues(x$chord_size,
                                     from = counts$chord_size,
                                     to = 1 / counts$n))
}

x <- compile_bowling_data()
