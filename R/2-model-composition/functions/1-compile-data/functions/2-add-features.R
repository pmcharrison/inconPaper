library(incon)
library(hrep)
library(tidyverse)

add_features <- function(models) {
  readRDS("output/composition-chord-counts.rds") %>% 
    add_observed() %>% 
    add_pi_chord() %>% 
    add_consonance(models) %>% 
    add_chord_size() %T>%
    saveRDS("output/composition-chord-counts-features.rds")
  NULL
}

add_observed <- function(df) {
  for (label in c("popular", "classical", "jazz")) {
    df[[paste0(label, "_observed")]] <- df[[paste0(label, "_count")]] > 0
  }
  df
}

add_pi_chord <- function(df) {
  mutate(df,
         pi_chord = map(pc_chord_type, ~ pi_chord(60L + as.integer(.))))
}

add_consonance <- function(df, models) { # wololo
  bind_cols(
    df,
    plyr::ldply(df$pi_chord, function(x) as.tibble(as.list(incon::incon(x, models))),
                .progress = "text"), 
  )
}

add_chord_size <- function(df) {
  mutate(df, chord_size = map_int(pi_chord, length))
}
