combine_panels <- function(p1, p2, p3, p4) {
  cowplot::plot_grid(
    cowplot::plot_grid(
      p1,
      cowplot::plot_grid(
        p3, p2, ncol = 1,
        labels = c("B", "C")
      ),
      rel_widths = c(2, 1.1),
      ncol = 2, 
      labels = c("A", "")
    ),
    p4,
    labels = c("", "D"),
    ncol = 1) %T>% ggsave("output/perception-analysis.pdf", ., width = 9, height = 9)
}

# combine_panels(p1, p2, p3, p4)
