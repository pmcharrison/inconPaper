plot_lm_coef <- function(x, pred) {
  x %>% 
    filter(term %in% pred) %>% 
    mutate(label = recode_factor(
      term, 
      chord_size = "Chord size",
      har_19_corpus = "Culture",
      har_18_harmonicity = "Periodicity/harmonicity",
      hutch_78_roughness = "Interference"
    )) %>% 
    ggplot(aes(x = label, y = beta, ymin = beta.conf.low, ymax = beta.conf.high,
               fill = label)) +
    geom_bar(stat = "identity", colour = "black") + 
    geom_errorbar(width = 0.35) +
    scale_x_discrete("Predictor") + 
    scale_y_continuous("Beta") + 
    scale_fill_manual(values = c("#FFFFFF", 
                                 "#E8E410",
                                 "#11A3FF",
                                 "#B50000"),
                      guide = FALSE) +
    coord_flip()
}

fit_lm <- function(dat, pred) {
  f <- "rating ~ {paste(pred, collapse = ' + ')}" %>% 
    glue() %>% 
    as.formula()
  lm(f, dat) %>% 
    tidy(conf.int = TRUE) %>% 
    add_beta(dat) %T>%
    write_csv("output/perception-lm-coef.csv")
}

add_beta <- function(x, dat) {
  x %>% 
    select(term, estimate, conf.low, conf.high) %>% 
    pmap_dfr(function(term, estimate, conf.low, conf.high) {
      sd_pred <- sd(dat[[term]])
      sd_outcome <- sd(dat$rating)
      reverse <- estimate > 0
      f <- sd_pred * (if (reverse) 1 else - 1) * sd_outcome
      if (is.na(f)) {
        c(beta, beta.conf.low, beta.conf.high) %<-% as.numeric(c(NA, NA, NA))
      } else {
        beta <- estimate * f
        c(beta.conf.low, beta.conf.high) %<-% sort(f * c(conf.low, conf.high))
      }
      tibble(sd_pred, sd_outcome, reverse, beta, beta.conf.low, beta.conf.high)
    }) %>% 
    bind_cols(x, .)
}

get_lm_pred <- function(mod, dat) {
  dat[["(Intercept)"]] <- 1
  pred <- map2(mod$term, mod$estimate, function(term, estimate) {
    dat[[term]] * estimate
  }) %>% as.data.frame %>% rowSums
  tibble(id = dat$id,
         actual = dat$rating,
         pred = pred)
}

plot_lm_pred <- function(mod, dat) {
  pred <- get_lm_pred(mod, dat) %T>% write_csv("output/perception-lm-pred.csv")
  r_df <- data.frame(r = round(cor(pred$pred, pred$actual), 2))
  ggplot(pred, aes(x = pred, y = actual)) +
    geom_point(shape = 21) +
    scale_x_continuous("Predicted consonance") +
    scale_y_continuous("Actual consonance") + 
    annotate("text",
             label = glue("~italic(r) == {round(cor(pred$pred, pred$actual), 2)}"),
             parse = TRUE,
             x = -Inf, y = Inf,
             hjust = -0.1, vjust = 1.5) +
    # geom_text(data = r_df, 
    #           aes(label = paste("~italic(r) ==", r)), 
    #           parse = TRUE,
    #           x = -Inf, y = Inf, 
    #           hjust = -0.1, vjust = 1.5) +
    theme(aspect.ratio = 1)
}
