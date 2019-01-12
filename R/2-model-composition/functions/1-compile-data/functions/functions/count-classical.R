count_classical <- function() {
  message("Getting classical counts...")
  classical_read_text() %>% 
    classical_collapse_over_year() %>% 
    classical_add_chord_type() %>% 
    group_by(pc_chord_type_id) %>% 
    summarise(count = sum(count))
}

classical_read_text <- function() {
  message("Reading from text file...")
  x <- readr::read_delim("input/peachnote/exact-1-grams.csv", "\t",
                         escape_double = FALSE, col_names = FALSE,
                         col_types = "cii", # wololo
                         na = "?", trim_ws = TRUE)
  names(x) <- c("chord", "year", "count")
  x
}

classical_collapse_over_year <- function(x) {
  message("Collapsing over year...")
  x %>% 
    group_by(chord) %>% 
    summarise(count = sum(count))
}

classical_add_chord_type <- function(x) {
  message("Computing chord types...")
  mutate(
    x,
    pc_chord_type_id = chord %>% 
      strsplit("_") %>% 
      plyr::laply(function(x) {
        # Quick version
        
        # midi <- cumsum(as.integer(x))
        # midi_rel <- midi - midi[1L]
        # pc_rel <- sort(unique(midi_rel %% 12L))
        # stopifnot(pc_rel[1L] == 0L)
        # str <- paste(pc_rel, collapse = " ")
        # hrep::pc_chord_alphabet$by_pc_chord[[str]]
        
        # Safe version
        as.integer(encode(pc_chord_type(pi_chord(cumsum(as.numeric(x))))))
      }, .progress = "text")
  )
}
