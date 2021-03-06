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
  stopifnot(all(df$pc_chord_type_id == seq_len(nrow(df))))
  mutate(df, 
         pi_chord = voicer::pc_chord_type_ideal_voicings)
}

add_consonance <- function(df, models) { # wololo
  bind_cols(
    df,
    plyr::ldply(df$pi_chord, function(x) as_tibble(as.list(incon::incon(x, models))),
                .progress = "text"), 
  )
}

add_chord_size <- function(df) {
  mutate(df, chord_size = map_int(pc_chord_type, length))
}
