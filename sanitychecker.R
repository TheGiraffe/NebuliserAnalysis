###############################
# Removed Last Detectors effect on VMD
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "DDISTANCEFROMLAST"
char_param <- "vmd_mean"
err_bars <- c(TRUE, "vmd_sd")
x_lab <- "Removed Last Detectors"
y_lab <- "Mean VMD (µm)"

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab)

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, sep="-")

((p1 | p2 | p3) /
    (p4 | p5) +
    plot_annotation(subtitle="Mean VMD of All Snapshots for Each Experiment vs. Number of Removed Last Detectors")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean VMD", 
                  x_lab, y_lab, char_param)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median VMD", 
                  x_lab, "Median VMD (µm)", "vmd_median")

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, sep="-")

((p6 & theme(legend.position="none")) + p7 +
    plot_annotation(subtitle="Comparison of Mean and Median VMD of All Snapshots for All Experiments vs. Number of Removed Last Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Removed Last Detectors effect on FPF
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "DDISTANCEFROMLAST"
char_param <- "fpf_mean"
err_bars <- c(TRUE, "fpf_sd")
x_lab <- "Removed Last Detectors"
y_lab <- "Mean FPF (µm)"

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab)

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, sep="-")

((p1 | p2 | p3) /
    (p4 | p5) +
    plot_annotation(subtitle="Mean FPF of All Snapshots for Each Experiment vs. Number of Removed Last Detectors")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean FPF", 
                  x_lab, y_lab, char_param)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median FPF", 
                  x_lab, "Median FPF (µm)", "fpf_median")

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, sep="-")

((p6 & theme(legend.position="none")) + p7 +
    plot_annotation(subtitle="Comparison of Mean and Median FPF of All Snapshots for All Experiments vs. Number of Removed Last Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Removed Last Detectors effect on RF
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "DDISTANCEFROMLAST"
char_param <- "rf_mean"
err_bars <- c(TRUE, "rf_sd")
x_lab <- "Removed Last Detectors"
y_lab <- "Mean RF (µm)"

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab)

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, sep="-")

((p1 | p2 | p3) /
    (p4 | p5) +
    plot_annotation(subtitle="Mean RF of All Snapshots for Each Experiment vs. Number of Removed Last Detectors")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean RF", 
                  x_lab, y_lab, char_param)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median RF", 
                  x_lab, "Median RF (µm)", "rf_median")

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, sep="-")

((p6 & theme(legend.position="none")) + p7 +
    plot_annotation(subtitle="Comparison of Mean and Median RF of All Snapshots for All Experiments vs. Number of Removed Last Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Removed Last Detectors effect on eFPF
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "DDISTANCEFROMLAST"
char_param <- "efpf_mean"
err_bars <- c(TRUE, "efpf_sd")
x_lab <- "Removed Last Detectors"
y_lab <- "Mean eFPF (µm)"

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab)

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, sep="-")

((p1 | p2 | p3) /
    (p4 | p5) +
    plot_annotation(subtitle="Mean eFPF of All Snapshots for Each Experiment vs. Number of Removed Last Detectors")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean eFPF", 
                  x_lab, y_lab, char_param)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median eFPF", 
                  x_lab, "Median eFPF (µm)", "efpf_median")

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, sep="-")

((p6 & theme(legend.position="none")) + p7 +
    plot_annotation(subtitle="Comparison of Mean and Median eFPF of All Snapshots for All Experiments vs. Number of Removed Last Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")


