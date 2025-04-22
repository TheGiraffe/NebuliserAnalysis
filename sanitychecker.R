###############################
# Removed First Detectors effect on VMD, incorporating in PDRanges
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "DFIRST"
char_param <- "vmd_mean"
err_bars <- c(TRUE, "vmd_sd")
x_lab <- "Removed First Detectors"
y_lab <- "Mean VMD (µm)"
PDR <- list(c(0.1, 2500), c(1.0, 100))

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)

# filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, "PDR", sep="-")

((p1 | p2 | p3) /
    (p4 | p5) +
    plot_annotation(subtitle="Mean VMD of All Snapshots for Each Experiment vs. Number of Removed First Detectors")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean VMD", 
                  x_lab, y_lab, char_param, PDR)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median VMD", 
                  x_lab, "Median VMD (µm)", "vmd_median", PDR)

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, "PDR", sep="-")

((p6 & theme(legend.position="none")) + p7 +
    plot_annotation(subtitle="Comparison of Mean and Median VMD of All Snapshots for All Experiments vs. Number of Removed First Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

# ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")
