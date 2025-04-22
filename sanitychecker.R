###############################
# Alarm min scattering signal's effect on eFPF, incorporating in PDRanges
# Should be very short data (2 data points each) - just a sanity check.
# Only exporting the Overall chart because the individual ones are less useful.
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "ALARMMIN"
char_param <- "efpf_mean"
err_bars <- c(TRUE, "efpf_sd")
x_lab <- "Error Alarm Min Scattering Signal"
y_lab <- "Mean eFPF (µm)"
# PDR <- list(c(0.1, 2500), c(1.0, 100))
# 
# p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
# p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
# p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
# p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
# p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR) 
# 
# filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, "PDR", sep="-")
# 
# ((p1 | p2 | p3) /
#     (p4 | p5) +
#     plot_layout(guides = 'collect') +
#     plot_annotation(subtitle="Mean eFPF of All Snapshots for Each Experiment vs. Different Error Alarm Threshold Levels")
# ) & theme(
#   plot.subtitle=element_text(family=myFont, size=14, face="bold"),
#   axis.text.x=element_text(size=8))
# 
# ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean eFPF", 
                  x_lab, y_lab, char_param, PDRanges = PDR)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median eFPF", 
                  x_lab, "Median eFPF (µm)", "efpf_median", PDRanges = PDR)

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, "PDR", sep="-")

((p6 & theme(legend.position="none")) + p7 +
    plot_annotation(subtitle="Comparison of Mean and Median eFPF of All Snapshots for All Experiments vs. Different Error Alarm Threshold Levels")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

ymin=chr_par - chr_par_err
ymin=ifelse(chr_par - chr_par_err > 0, chr_par - chr_par_err, 0)