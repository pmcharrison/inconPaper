label_corpus <- function(corpus, opt, detail = FALSE) {
  plyr::mapvalues(corpus, 
                  from = opt$corpora, 
                  to = if (detail) 
                    attr(opt$corpora, "detail") else 
                      names(opt$corpora),
                  warn_missing = FALSE)
}

label_feature <- function(feature, opt, reverse = FALSE) {
  x <- plyr::mapvalues(feature, from = opt$features, to = names(opt$features),
                       warn_missing = FALSE)
  if (reverse) {
    factor(x, levels = rev(names(opt$features)))
  } else {
    factor(x, levels = names(opt$features))
  }
}
