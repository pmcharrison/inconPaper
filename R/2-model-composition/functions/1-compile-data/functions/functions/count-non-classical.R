count_popular <- function() {
  message("Getting popular counts...")
  corpdiss::popular_1_pc_chord_type %>% 
    format_corpus_dissonance_table()
}

count_jazz <- function() {
  message("Getting jazz counts...")
  hcorp::jazz_1 %>% 
    corpdiss::corpus_dissonance_table() %>% 
    format_corpus_dissonance_table()
}

format_corpus_dissonance_table <- function(x) {
  mutate(x, 
         pc_chord_type_id = seq_along(count)) %>% 
    select(pc_chord_type_id, count)
}
