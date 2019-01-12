get_partial_cor <- function(model, consonance, dat) {
  if (!consonance) dat[[model]] <- - dat[[model]]
  x <- partial_out_chord_size("rating", dat)
  y <- partial_out_chord_size(model, dat)
  cor.test(x, y) %>% glance %>% 
    transmute(partial_cor.estimate = estimate,
              partial_cor.conf.low = conf.low,
              partial_cor.conf.high = conf.high)
}

partial_out_chord_size <- function(col, dat) {
  "{col} ~ as.factor(chord_size)" %>% glue %>% lm(data = dat) %>% resid
}

get_adj_r2 <- function(model, dat) {
  "rating ~ {model} + as.factor(chord_size)" %>% glue %>% 
    lm(data = dat) %>% glance %>% select(adj.r.squared) %>% as.numeric
}

plot_all_mods <- function(x) {
  file <- "output/perception-all-mod-eval"
  df <- x %>% 
    select(label, citation, class, input, starts_with("partial_cor")) %>% 
    arrange(partial_cor.estimate) %>% 
    mutate(label = factor(label, levels = label),
           class = factor(class, levels = c("Spectral interference",
                                            "Periodicity",
                                            "Culture",
                                            "Numerosity"))) %T>%
    write_csv(glue("{file}.csv"))
  p <- df %>% 
    ggplot(aes(x = label, 
               y = partial_cor.estimate, 
               ymin = partial_cor.conf.low,
               ymax = partial_cor.conf.high,
               fill = class, 
               linetype = input)) + 
    scale_x_discrete("Model", labels = df$citation) +
    scale_y_continuous("Partial correlation with consonance ratings") +
    scale_fill_manual("Theory", values = c("#FFFFFF", 
                                           "#E8E410",
                                           "#11A3FF",
                                           "#B50000") %>% rev) +
    scale_linetype_manual("Input", values = c("dotted", "solid")) +
    geom_bar(stat = "identity", colour = "black", width = 0.75) + 
    geom_errorbar(width = 0.5, linetype = "solid") +
    coord_flip() +
    guides(linetype = guide_legend(override.aes = list(fill = "white")))
  # ggsave(glue("{file}.pdf"), plot = p, width = 5.5, height = 4.5)
}

add_stats_all_mods <- function(mods, dat) {
  mods %>% 
    mutate(partial_cor = map2(label, consonance, get_partial_cor, dat),
           adj_r2 = map_dbl(label, get_adj_r2, dat)) %>% 
    unnest() %>% 
    mutate(., cocor = cocor_all_mods(., dat)) %T>%
    saveRDS("output/perception-all-mod-stats.rds")
}

all_mods_bowling <- function(dat, opt) {
  stats <- add_stats_all_mods(mods = all_mods() %>% filter(!label %in% opt$exclude), 
                     dat = dat)
  plot <- plot_all_mods(stats)
  list(stats, plot)
}

cocor_all_mods <- function(mod_perf, dat) {
  map(mod_perf$label, function(mod_1) {
    mod_perf$label %>% set_names(mod_perf$label) %>% 
      map(~ cocor_two_mods(mod_1, ., mod_perf, dat))
  })
}

cocor_two_mods <- function(mod_1, mod_2, mod_perf, dat) {
  r.jk <- mod_perf %>% filter(label == mod_1) %>% pull(partial_cor.estimate)
  r.jh <- mod_perf %>% filter(label == mod_2) %>% pull(partial_cor.estimate)
  r.kh <- cor(partial_out_chord_size(mod_1, dat),
              partial_out_chord_size(mod_2, dat))
  if (xor(mod_perf %>% filter(label == mod_1) %>% pull(consonance),
          mod_perf %>% filter(label == mod_2) %>% pull(consonance)))
    r.kh <- - r.kh
  suppressWarnings(
    cocor::cocor.dep.groups.overlap(r.jk, r.jh, r.kh, nrow(dat))
  )
}
