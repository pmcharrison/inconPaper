source("R/2-model-composition/functions/depend.R")

"R/2-model-composition" %>% 
  file.path("functions") %>% 
  list.files(pattern = "\\.R$", full.names = TRUE, recursive = TRUE) %>% 
  sapply(source) %>% 
  invisible
