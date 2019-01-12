source("R/2-model-composition/load-code.R")
options(warn = 2)

recompile <- TRUE # set to FALSE to skip the slow data compilation stage
boot_perm_imp <- FALSE # set to TRUE to enable (very slow) bootstrapped CIs

theme_set(theme_bw() + theme(panel.grid = element_blank(),
                             strip.background = element_blank(), 
                             axis.text.x = element_text(colour = "black"),
                             axis.text.y = element_text(colour = "black"),
                             axis.ticks = element_line(colour = "black")))

if (recompile) compile_data() # compiled data is saved to output directory
opt <- get_options(boot_perm_imp = boot_perm_imp)
df <- get_data(opt) # this data is read from the output directory
stats <- do_stats(df, opt) # saves plots to output directory

# The findings don't change substantively if we limit analysis to 
# only observed chords, but we lose some nuances in the polynomials.
