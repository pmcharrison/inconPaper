library(tidyverse)
library(hrep)

count_chords <- function() {
  classical <- count_classical()
  popular <- count_popular()
  jazz <- count_jazz()
  
  all <- tibble(pc_chord_type_id = seq_len(alphabet_size("pc_chord_type")),
                classical_count = 0L,
                popular_count = 0L,
                jazz_count = 0L)
  all$classical_count[classical$pc_chord_type_id] <- classical$count
  all$popular_count[popular$pc_chord_type_id] <- popular$count
  all$jazz_count[jazz$pc_chord_type_id] <- jazz$count
  
  all <- add_column(all, 
                    pc_chord_type = all$pc_chord_type_id %>% 
                      coded_vec("pc_chord_type") %>% 
                      decode, 
                    .before = "pc_chord_type_id")
  
  message("Saving...")
  saveRDS(all, "output/composition-chord-counts.rds")
}
