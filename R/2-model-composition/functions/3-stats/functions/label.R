label_corpus <- function(corpus, opt, detail = FALSE) {
  plyr::mapvalues(corpus, 
                  from = opt$corpora, 
                  to = if (detail) 
                    attr(opt$corpora, "detail") else 
                      names(opt$corpora),
                  warn_missing = FALSE)
}

label_feature <- function(feature, opt, label_absence = FALSE) {
  x <- plyr::mapvalues(feature, from = opt$features, to = names(opt$features),
                       warn_missing = FALSE)
  if (label_absence) {
    x <- paste(x, ifelse(feature %in% opt$absence,
                         "\n(absence)", ""), 
               sep = "")
  }
  x
}
