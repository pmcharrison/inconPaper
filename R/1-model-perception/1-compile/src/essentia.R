library(withr)
library(jsonlite)
library(tidyverse)
loadNamespace("plyr")
loadNamespace("R.utils")

in_dir <- "/Users/peter/Dropbox/Academic/projects/pearce-marcus/harmony/incon-paper/input/bowling2018/materials/audio"
out_dir <- "/Users/peter/Dropbox/Academic/projects/pearce-marcus/harmony/incon-paper/output/essentia/v2"

ess <- function(x) {
  stopifnot(file.exists(x))
  x <- normalizePath(x, mustWork = TRUE)
  with_dir(tempdir(check = TRUE), {
    file.copy(x, "audio.wav")
    system("docker run --rm -v `pwd`:/essentia mtgupf/essentia essentia_streaming_extractor_music audio.wav audio.sig")
    y <- fromJSON("audio.sig")$lowlevel$dissonance$mean
    stopifnot(file.remove("audio.wav"))
    stopifnot(file.remove("audio.sig"))
    y
  })
}

res <- tibble(filename = list.files(in_dir, pattern = "\\.wav$")) %>% 
  mutate(id = strsplit(filename, split = "\\.") %>% map_chr(extract2, 1),
         path = file.path(in_dir, filename), 
         roughness = plyr::laply(path, ess, .progress = "text")) %>% 
  select(id, roughness)

R.utils::mkdirs(out_dir)
write_csv(res, file.path(out_dir, "results.csv"))
