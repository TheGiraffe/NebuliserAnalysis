library(tidyverse)
library(ggplot2)
library(gganimate)
library(geomtextpath)
library(extrafont)
library(patchwork)
library(svglite)
# font_import()
loadfonts(device = "win")
windowsFonts("Century Gothic" = windowsFont("Century Gothic"))
library(scales)

rm(list = ls())
myFont = "Century Gothic"
dataPath = "Data/BatchData/FileSummaries"
exportPath = "Exports/AnalysisParameterComparison/"
ap_values = read_csv("AnalysisParametersValues.csv") %>%
  mutate(rownum = row_number())

ReadFilesAndMakeVars = function(path){
  file <- read_csv(path)
  return(file)
}

Exp20_files <- list.files(path=dataPath, pattern="^FileSummary_Exp20", full.names = TRUE)
Exp24_files <- list.files(path=dataPath, pattern="^FileSummary_Exp24", full.names = TRUE)
Exp29_files <- list.files(path=dataPath, pattern="^FileSummary_Exp29", full.names = TRUE)
Exp33_files <- list.files(path=dataPath, pattern="^FileSummary_Exp33", full.names = TRUE)
Exp39_files <- list.files(path=dataPath, pattern="^FileSummary_Exp39", full.names = TRUE)

Exp20 <- lapply(Exp20_files, ReadFilesAndMakeVars)
Exp24 <- lapply(Exp24_files, ReadFilesAndMakeVars)
Exp29 <- lapply(Exp29_files, ReadFilesAndMakeVars)
Exp33 <- lapply(Exp33_files, ReadFilesAndMakeVars)
Exp39 <- lapply(Exp39_files, ReadFilesAndMakeVars)

Exp20_df <- bind_rows(Exp20) %>%
  mutate(rownum = row_number()) %>%
  mutate(file = experiment, experiment = strsplit(experiment, "-")[[1]][[1]]) %>%
  mutate(experiment = paste0(experiment, " : 100% Water"))
Exp20_df <- left_join(Exp20_df, ap_values)

Exp24_df <- bind_rows(Exp24) %>%
  mutate(rownum = row_number()) %>%
  mutate(file = experiment, experiment = strsplit(experiment, "-")[[1]][[1]]) %>%
  mutate(experiment = paste0(experiment, " : 100% Water"))
Exp24_df <- left_join(Exp24_df, ap_values)

Exp29_df <- bind_rows(Exp29) %>%
  mutate(rownum = row_number()) %>%
  mutate(file = experiment, experiment = strsplit(experiment, "-")[[1]][[1]]) %>%
  mutate(experiment = paste0(experiment, " : 5% Ethanol"))
Exp29_df <- left_join(Exp29_df, ap_values)

Exp33_df <- bind_rows(Exp33) %>%
  mutate(rownum = row_number()) %>%
  mutate(file = experiment, experiment = strsplit(experiment, "-")[[1]][[1]]) %>%
  mutate(experiment = paste0(experiment, " : 10% Ethanol"))
Exp33_df <- left_join(Exp33_df, ap_values)

Exp39_df <- bind_rows(Exp39) %>%
  mutate(rownum = row_number()) %>%
  mutate(file = experiment, experiment = strsplit(experiment, "-")[[1]][[1]]) %>%
  mutate(experiment = paste0(experiment, " : 25% Ethanol"))
Exp39_df <- left_join(Exp39_df, ap_values)

AllExperiments <- rbind(Exp20_df, Exp24_df, Exp29_df, Exp33_df, Exp39_df)


# ChrParName = name of the characterization parameter
# characterization parameter column = ChrParCol
# list(c(0.1, 2500), c(1.0, 100))

GetVaryPlot = function(DataFrame, THINGTOVARY, PlotDescription, XLab, YLab, ChrParCol, CustomColor = c(FALSE, ""), ErrBars=c(FALSE, ""), PDRanges = list(c(0.1, 2500))){
  if (length(PDRanges) > 1){
    VARY_PDRANGE <- TRUE
  } else {
    VARY_PDRANGE <- FALSE
  }
  
  # print(VARY_PDRANGE)
   
  if (THINGTOVARY == "ALARMMIN"){
    if (!VARY_PDRANGE){
      varycond <- "VARY_ONLY_ALARM"
    } else {varycond <- "VARY_PDMINMAX_ALARM"}

  } else if (THINGTOVARY == "DFIRST"){
    if (!VARY_PDRANGE){
      varycond <- "VARY_ONLY_DETFIRST"
    } else {varycond <- "VARY_PDMINMAX_DETFIRST"}
    
  } else if (THINGTOVARY == "DDISTANCEFROMLAST"){
    if (!VARY_PDRANGE){
      varycond <- "VARY_ONLY_DETLAST"
    } else {varycond <- "VARY_PDMINMAX_DETLAST"}
  } else {
    return("error!")
  }
  
  # print(varycond)
  
  VARY_X <- DataFrame %>% 
    mutate(DFIRST_true = DFIRST, DFIRST = DFIRST - 1) %>%
    filter(eval(parse(text=varycond)) == TRUE) %>%
    mutate(chr_par = eval(parse(text=ChrParCol)))
  
  # Note, I redefined DFIRST to be the number of first detectors removed instead
  # of the first detector used, because of how I worded it in the graphs.
  # If you word it differently,  you may want to remove that mutate.

  if (VARY_PDRANGE){
    VARY_X <- VARY_X %>%
      mutate(pdrange = "")
    VARY_X$pdrange <- paste0(VARY_X$PDMIN, "-", VARY_X$PDMAX, " µm")
  }
  
  # print(VARY_X$experiment)
  
  tickint = 10
  
  if (!VARY_PDRANGE){
    # Regular Plot
    if (ErrBars[[1]] == TRUE){
      ErrCol <- ErrBars[[2]]
      
      VARY_X <- VARY_X %>%
        mutate(chr_par_err = eval(parse(text=ErrCol)))
      
      if (CustomColor[[1]] == TRUE){
        
        plot <- ggplot(VARY_X, aes(x=eval(parse(text=THINGTOVARY)), y=chr_par, group=experiment)) +
          geom_line(aes(x=eval(parse(text=THINGTOVARY)), y=chr_par, group=experiment), color=CustomColor[[2]]) +
          geom_point(aes(x=eval(parse(text=THINGTOVARY)), y=chr_par, group=experiment), color=CustomColor[[2]]) +
          geom_errorbar(aes(ymin=ifelse(chr_par - chr_par_err > 0, chr_par - chr_par_err, 0), ymax=chr_par + chr_par_err), color=CustomColor[[2]], size=0.1) +
          theme_bw() +
          theme(text=element_text(family=myFont, size=12)) +
          scale_x_continuous(breaks = seq(0, 150, by=tickint)) +
          xlab(XLab) +
          ylab(YLab) +
          ggtitle(PlotDescription)
        
      } else {
        plot <- ggplot(VARY_X, aes(x=eval(parse(text=THINGTOVARY)), y=chr_par, group=experiment, color=experiment)) +
          geom_line() +
          geom_point() +
          geom_errorbar(aes(ymin=ifelse(chr_par - chr_par_err > 0, chr_par - chr_par_err, 0), ymax=chr_par + chr_par_err), size=0.1) +
          theme_bw() +
          theme(text=element_text(family=myFont, size=12)) +
          scale_x_continuous(breaks = seq(0, 150, by=tickint)) +
          xlab(XLab) +
          ylab(YLab) +
          labs(color = "Experiment") +
          ggtitle(PlotDescription)
      }
      
    } else {
      
      if (CustomColor[[1]] == TRUE){
        
        plot <- ggplot(VARY_X, aes(x=eval(parse(text=THINGTOVARY)), y=chr_par, group=experiment)) +
          geom_line(aes(x=eval(parse(text=THINGTOVARY)), y=chr_par, group=experiment), color=CustomColor[[2]]) +
          geom_point(aes(x=eval(parse(text=THINGTOVARY)), y=chr_par, group=experiment), color=CustomColor[[2]]) +
          theme_bw() +
          theme(text=element_text(family=myFont, size=12)) +
          scale_x_continuous(breaks = seq(0, 150, by=tickint)) +
          xlab(XLab) +
          ylab(YLab) +
          ggtitle(PlotDescription)
        
      } else {
        plot <- ggplot(VARY_X, aes(x=eval(parse(text=THINGTOVARY)), y=chr_par, group=experiment, color=experiment)) +
          geom_line() +
          geom_point() +
          theme_bw() +
          theme(text=element_text(family=myFont, size=12)) +
          scale_x_continuous(breaks = seq(0, 150, by=tickint)) +
          xlab(XLab) +
          ylab(YLab) +
          labs(color = "Experiment") +
          ggtitle(PlotDescription)
      }
    }
  } else {
    # for PD ranges 
    if (ErrBars[[1]] == TRUE){
      ErrCol <- ErrBars[[2]]
      
      VARY_X <- VARY_X %>%
        mutate(chr_par_err = eval(parse(text=ErrCol)))
      
      if (CustomColor[[1]] == TRUE){
        
        plot <- ggplot(VARY_X, aes(x=eval(parse(text=THINGTOVARY)), y=chr_par, group=interaction(experiment, pdrange))) +
          geom_line(aes(x=eval(parse(text=THINGTOVARY)), y=chr_par, group=interaction(experiment, pdrange), linetype = pdrange), color=CustomColor[[2]]) +
          geom_point(aes(x=eval(parse(text=THINGTOVARY)), y=chr_par, group=interaction(experiment, pdrange)), color=CustomColor[[2]]) +
          geom_errorbar(aes(ymin=ifelse(chr_par - chr_par_err > 0, chr_par - chr_par_err, 0), ymax=chr_par + chr_par_err), color=CustomColor[[2]], size=0.1) +
          theme_bw() +
          theme(text=element_text(family=myFont, size=12)) +
          scale_x_continuous(breaks = seq(0, 150, by=tickint)) +
          xlab(XLab) +
          ylab(YLab) +
          labs(linetype = "Defined PDR") +
          ggtitle(PlotDescription)
        
      } else {
        plot <- ggplot(VARY_X, aes(x=eval(parse(text=THINGTOVARY)), y=chr_par, group=interaction(experiment, pdrange), color=experiment, linetype = pdrange)) +
          geom_line() +
          geom_point() +
          geom_errorbar(aes(ymin=ifelse(chr_par - chr_par_err > 0, chr_par - chr_par_err, 0), ymax=chr_par + chr_par_err), size=0.1) +
          theme_bw() +
          theme(text=element_text(family=myFont, size=12)) +
          scale_x_continuous(breaks = seq(0, 150, by=tickint)) +
          xlab(XLab) +
          ylab(YLab) +
          labs(color = "Experiment", linetype = "Defined PDR") +
          ggtitle(PlotDescription)
      }
      
    } else {
      
      if (CustomColor[[1]] == TRUE){
        
        plot <- ggplot(VARY_X, aes(x=eval(parse(text=THINGTOVARY)), y=chr_par, group=interaction(experiment, pdrange))) +
          geom_line(aes(x=eval(parse(text=THINGTOVARY)), y=chr_par, group=interaction(experiment, pdrange), linetype = pdrange), color=CustomColor[[2]]) +
          geom_point(aes(x=eval(parse(text=THINGTOVARY)), y=chr_par, group=interaction(experiment, pdrange)), color=CustomColor[[2]]) +
          theme_bw() +
          theme(text=element_text(family=myFont, size=12)) +
          scale_x_continuous(breaks = seq(0, 150, by=tickint)) +
          xlab(XLab) +
          ylab(YLab) +
          labs(linetype = "Defined PDR") +
          ggtitle(PlotDescription)
        
      } else {
        plot <- ggplot(VARY_X, aes(x=eval(parse(text=THINGTOVARY)), y=chr_par, group=interaction(experiment, pdrange), color=experiment, linetype = pdrange)) +
          geom_line() +
          geom_point() +
          theme_bw() +
          theme(text=element_text(family=myFont, size=12)) +
          scale_x_continuous(breaks = seq(0, 150, by=tickint)) +
          xlab(XLab) +
          ylab(YLab) +
          labs(color = "Experiment", linetype = "Defined PDR") +
          ggtitle(PlotDescription)
      }
    }
  }
  
  return(plot)
}


GetExpPlot = function(expVec, thingToVary, charParam, errBars, xLab, yLab, usrPDRange = list(c(0.1, 2500))){
  p <- GetVaryPlot(eval(parse(text = paste0(expVec[[1]], "_df"))),
                    thingToVary,
                    eval(parse(text = paste0(expVec[[1]], "_df")))$experiment[[1]],
                    xLab, yLab, charParam, 
                    ErrBars = errBars,
                    CustomColor = c(TRUE, expVec[[2]]),
                    PDRanges = usrPDRange
                   )
  return(p)
}

custom_colors <- hue_pal()(5)
exps <- list(c("Exp20", custom_colors[[1]]), c("Exp24", custom_colors[[2]]), c("Exp29", custom_colors[[3]]), c("Exp33", custom_colors[[4]]), c("Exp39", custom_colors[[5]]))

pltsnum <- 0

###############################
# Alarm min scattering signal's effect on VMD
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "ALARMMIN"
char_param <- "vmd_mean"
err_bars <- c(TRUE, "vmd_sd")
x_lab <- "Error Alarm Min Scattering Signal"
y_lab <- "Mean VMD (µm)"

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab)

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, sep="-")

((p1 | p2 | p3) /
    (p4 | p5) +
    plot_annotation(subtitle="Mean VMD of All Snapshots for Each Experiment vs. Different Error Alarm Threshold Levels")
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
  plot_annotation(subtitle="Comparison of Mean and Median VMD of All Snapshots for All Experiments vs. Different Error Alarm Threshold Levels")
  ) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
            axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")


###############################
# Alarm min scattering signal's effect on FPF
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "ALARMMIN"
char_param <- "fpf_mean"
err_bars <- c(TRUE, "fpf_sd")
x_lab <- "Error Alarm Min Scattering Signal"
y_lab <- "Mean FPF"

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab)

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, sep="-")

((p1 | p2 | p3) /
    (p4 | p5) +
    plot_annotation(subtitle="Mean FPF of All Snapshots for Each Experiment vs. Different Error Alarm Threshold Levels")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean FPF", 
                  x_lab, y_lab, char_param)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median FPF", 
                  x_lab, "Median FPF", "fpf_median")

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, sep="-")

((p6 & theme(legend.position="none")) + p7 +
    plot_annotation(subtitle="Comparison of Mean and Median FPF of All Snapshots for All Experiments vs. Different Error Alarm Threshold Levels")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Alarm min scattering signal's effect on RF
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "ALARMMIN"
char_param <- "rf_mean"
err_bars <- c(TRUE, "rf_sd")
x_lab <- "Error Alarm Min Scattering Signal"
y_lab <- "Mean RF"

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab)

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, sep="-")

((p1 | p2 | p3) /
    (p4 | p5) +
    plot_annotation(subtitle="Mean RF of All Snapshots for Each Experiment vs. Different Error Alarm Threshold Levels")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean RF", 
                  x_lab, y_lab, char_param)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median RF", 
                  x_lab, "Median RF", "rf_median")

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, sep="-")

((p6 & theme(legend.position="none")) + p7 +
    plot_annotation(subtitle="Comparison of Mean and Median RF of All Snapshots for All Experiments vs. Different Error Alarm Threshold Levels")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Alarm min scattering signal's effect on eFPF
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "ALARMMIN"
char_param <- "efpf_mean"
err_bars <- c(TRUE, "efpf_sd")
x_lab <- "Error Alarm Min Scattering Signal"
y_lab <- "Mean eFPF"

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab)

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, sep="-")

((p1 | p2 | p3) /
    (p4 | p5) +
    plot_annotation(subtitle="Mean eFPF of All Snapshots for Each Experiment vs. Different Error Alarm Threshold Levels")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean eFPF", 
                  x_lab, y_lab, char_param)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median eFPF", 
                  x_lab, "Median eFPF", "efpf_median")

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, sep="-")

((p6 & theme(legend.position="none")) + p7 +
    plot_annotation(subtitle="Comparison of Mean and Median eFPF of All Snapshots for All Experiments vs. Different Error Alarm Threshold Levels")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Removed First Detectors effect on VMD
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "DFIRST"
char_param <- "vmd_mean"
err_bars <- c(TRUE, "vmd_sd")
x_lab <- "Removed First Detectors"
y_lab <- "Mean VMD (µm)"

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab)

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, sep="-")

((p1 | p2 | p3) /
    (p4 | p5) +
    plot_annotation(subtitle="Mean VMD of All Snapshots for Each Experiment vs. Number of Removed First Detectors")
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
    plot_annotation(subtitle="Comparison of Mean and Median VMD of All Snapshots for All Experiments vs. Number of Removed First Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Removed First Detectors effect on FPF
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "DFIRST"
char_param <- "fpf_mean"
err_bars <- c(TRUE, "fpf_sd")
x_lab <- "Removed First Detectors"
y_lab <- "Mean FPF (µm)"

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab)

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, sep="-")

((p1 | p2 | p3) /
    (p4 | p5) +
    plot_annotation(subtitle="Mean FPF of All Snapshots for Each Experiment vs. Number of Removed First Detectors")
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
    plot_annotation(subtitle="Comparison of Mean and Median FPF of All Snapshots for All Experiments vs. Number of Removed First Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Removed First Detectors effect on RF
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "DFIRST"
char_param <- "rf_mean"
err_bars <- c(TRUE, "rf_sd")
x_lab <- "Removed First Detectors"
y_lab <- "Mean RF (µm)"

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab)

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, sep="-")

((p1 | p2 | p3) /
    (p4 | p5) +
    plot_annotation(subtitle="Mean RF of All Snapshots for Each Experiment vs. Number of Removed First Detectors")
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
    plot_annotation(subtitle="Comparison of Mean and Median RF of All Snapshots for All Experiments vs. Number of Removed First Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Removed First Detectors effect on eFPF
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "DFIRST"
char_param <- "efpf_mean"
err_bars <- c(TRUE, "efpf_sd")
x_lab <- "Removed First Detectors"
y_lab <- "Mean eFPF (µm)"

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab)

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, sep="-")

((p1 | p2 | p3) /
    (p4 | p5) +
    plot_annotation(subtitle="Mean eFPF of All Snapshots for Each Experiment vs. Number of Removed First Detectors")
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
    plot_annotation(subtitle="Comparison of Mean and Median eFPF of All Snapshots for All Experiments vs. Number of Removed First Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

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

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, "PDR", sep="-")

((p1 | p2 | p3 ) /
    (p4 | p5) +
    plot_layout(guides = 'collect') +
    plot_annotation(subtitle="Mean VMD of All Snapshots for Each Experiment vs. Number of Removed First Detectors")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean VMD", 
                  x_lab, y_lab, char_param, PDRanges = PDR)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median VMD", 
                  x_lab, "Median VMD (µm)", "vmd_median", PDRanges = PDR)

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, "PDR", sep="-")

(p6 + p7 +
    plot_layout(guides = 'collect') +
    plot_annotation(subtitle="Comparison of Mean and Median VMD of All Snapshots for All Experiments vs. Number of Removed First Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Removed First Detectors effect on FPF, incorporating in PDRanges
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "DFIRST"
char_param <- "fpf_mean"
err_bars <- c(TRUE, "fpf_sd")
x_lab <- "Removed First Detectors"
y_lab <- "Mean FPF (µm)"
PDR <- list(c(0.1, 2500), c(1.0, 100))

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR) 

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, "PDR", sep="-")

((p1 | p2 | p3 ) /
    (p4 | p5) +
    plot_layout(guides = 'collect') +
    plot_annotation(subtitle="Mean FPF of All Snapshots for Each Experiment vs. Number of Removed First Detectors")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean FPF", 
                  x_lab, y_lab, char_param, PDRanges = PDR)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median FPF", 
                  x_lab, "Median FPF (µm)", "fpf_median", PDRanges = PDR)

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, "PDR", sep="-")

(p6 + p7 +
    plot_layout(guides = 'collect') +
    plot_annotation(subtitle="Comparison of Mean and Median FPF of All Snapshots for All Experiments vs. Number of Removed First Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Removed First Detectors effect on RF, incorporating in PDRanges
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "DFIRST"
char_param <- "rf_mean"
err_bars <- c(TRUE, "rf_sd")
x_lab <- "Removed First Detectors"
y_lab <- "Mean RF (µm)"
PDR <- list(c(0.1, 2500), c(1.0, 100))

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR) 

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, "PDR", sep="-")

((p1 | p2 | p3 ) /
    (p4 | p5) +
    plot_layout(guides = 'collect') +
    plot_annotation(subtitle="Mean RF of All Snapshots for Each Experiment vs. Number of Removed First Detectors")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean RF", 
                  x_lab, y_lab, char_param, PDRanges = PDR)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median RF", 
                  x_lab, "Median RF (µm)", "rf_median", PDRanges = PDR)

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, "PDR", sep="-")

(p6 + p7 +
    plot_layout(guides = 'collect') +
    plot_annotation(subtitle="Comparison of Mean and Median RF of All Snapshots for All Experiments vs. Number of Removed First Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Removed First Detectors effect on eFPF, incorporating in PDRanges
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "DFIRST"
char_param <- "efpf_mean"
err_bars <- c(TRUE, "efpf_sd")
x_lab <- "Removed First Detectors"
y_lab <- "Mean eFPF (µm)"
PDR <- list(c(0.1, 2500), c(1.0, 100))

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR) 

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, "PDR", sep="-")

((p1 | p2 | p3 ) /
    (p4 | p5) +
    plot_layout(guides = 'collect') +
    plot_annotation(subtitle="Mean eFPF of All Snapshots for Each Experiment vs. Number of Removed First Detectors")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean eFPF", 
                  x_lab, y_lab, char_param, PDRanges = PDR)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median eFPF", 
                  x_lab, "Median eFPF (µm)", "efpf_median", PDRanges = PDR)

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, "PDR", sep="-")

(p6 + p7 +
    plot_layout(guides = 'collect') +
    plot_annotation(subtitle="Comparison of Mean and Median eFPF of All Snapshots for All Experiments vs. Number of Removed First Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Removed Last Detectors effect on VMD, incorporating in PDRanges
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "DDISTANCEFROMLAST"
char_param <- "vmd_mean"
err_bars <- c(TRUE, "vmd_sd")
x_lab <- "Removed Last Detectors"
y_lab <- "Mean VMD (µm)"
PDR <- list(c(0.1, 2500), c(1.0, 100))

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR) 

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, "PDR", sep="-")

((p1 | p2 | p3 ) /
    (p4 | p5) +
    plot_layout(guides = 'collect') +
    plot_annotation(subtitle="Mean VMD of All Snapshots for Each Experiment vs. Number of Removed Last Detectors")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean VMD", 
                  x_lab, y_lab, char_param, PDRanges = PDR)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median VMD", 
                  x_lab, "Median VMD (µm)", "vmd_median", PDRanges = PDR)

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, "PDR", sep="-")

(p6 + p7 +
    plot_layout(guides = 'collect') +
    plot_annotation(subtitle="Comparison of Mean and Median VMD of All Snapshots for All Experiments vs. Number of Removed Last Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Removed Last Detectors effect on FPF, incorporating in PDRanges
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "DDISTANCEFROMLAST"
char_param <- "fpf_mean"
err_bars <- c(TRUE, "fpf_sd")
x_lab <- "Removed Last Detectors"
y_lab <- "Mean FPF (µm)"
PDR <- list(c(0.1, 2500), c(1.0, 100))

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR) 

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, "PDR", sep="-")

((p1 | p2 | p3 ) /
    (p4 | p5) +
    plot_layout(guides = 'collect') +
    plot_annotation(subtitle="Mean FPF of All Snapshots for Each Experiment vs. Number of Removed Last Detectors")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean FPF", 
                  x_lab, y_lab, char_param, PDRanges = PDR)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median FPF", 
                  x_lab, "Median FPF (µm)", "fpf_median", PDRanges = PDR)

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, "PDR", sep="-")

(p6 + p7 +
    plot_layout(guides = 'collect') +
    plot_annotation(subtitle="Comparison of Mean and Median FPF of All Snapshots for All Experiments vs. Number of Removed Last Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Removed Last Detectors effect on RF, incorporating in PDRanges
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "DDISTANCEFROMLAST"
char_param <- "rf_mean"
err_bars <- c(TRUE, "rf_sd")
x_lab <- "Removed Last Detectors"
y_lab <- "Mean RF (µm)"
PDR <- list(c(0.1, 2500), c(1.0, 100))

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR) 

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, "PDR", sep="-")

((p1 | p2 | p3 ) /
    (p4 | p5) +
    plot_layout(guides = 'collect') +
    plot_annotation(subtitle="Mean RF of All Snapshots for Each Experiment vs. Number of Removed Last Detectors")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean RF", 
                  x_lab, y_lab, char_param, PDRanges = PDR)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median RF", 
                  x_lab, "Median RF (µm)", "rf_median", PDRanges = PDR)

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, "PDR", sep="-")

(p6 + p7 +
    plot_layout(guides = 'collect') +
    plot_annotation(subtitle="Comparison of Mean and Median RF of All Snapshots for All Experiments vs. Number of Removed Last Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Removed Last Detectors effect on eFPF, incorporating in PDRanges
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "DDISTANCEFROMLAST"
char_param <- "efpf_mean"
err_bars <- c(TRUE, "efpf_sd")
x_lab <- "Removed Last Detectors"
y_lab <- "Mean eFPF (µm)"
PDR <- list(c(0.1, 2500), c(1.0, 100))

p1 <- GetExpPlot(exps[[1]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p2 <- GetExpPlot(exps[[2]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p3 <- GetExpPlot(exps[[3]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p4 <- GetExpPlot(exps[[4]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR)
p5 <- GetExpPlot(exps[[5]], thing_to_vary, char_param, err_bars, x_lab, y_lab, PDR) 

filename <- paste(pltsnum, "ExpPlot", char_param, thing_to_vary, "PDR", sep="-")

((p1 | p2 | p3 ) /
    (p4 | p5) +
    plot_layout(guides = 'collect') +
    plot_annotation(subtitle="Mean eFPF of All Snapshots for Each Experiment vs. Number of Removed Last Detectors")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean eFPF", 
                  x_lab, y_lab, char_param, PDRanges = PDR)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median eFPF", 
                  x_lab, "Median eFPF (µm)", "efpf_median", PDRanges = PDR)

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, "PDR", sep="-")

(p6 + p7 +
    plot_layout(guides = 'collect') +
    plot_annotation(subtitle="Comparison of Mean and Median eFPF of All Snapshots for All Experiments vs. Number of Removed Last Detectors")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Alarm min scattering signal's effect on VMD, incorporating in PDRanges
# Should be very short data (2 data points each) - just a sanity check.
# Only exporting the Overall chart because the individual ones are less useful.
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "ALARMMIN"
char_param <- "vmd_mean"
err_bars <- c(TRUE, "vmd_sd")
x_lab <- "Error Alarm Min Scattering Signal"
y_lab <- "Mean VMD (µm)"
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
#     plot_annotation(subtitle="Mean VMD of All Snapshots for Each Experiment vs. Different Error Alarm Threshold Levels")
# ) & theme(
#   plot.subtitle=element_text(family=myFont, size=14, face="bold"),
#   axis.text.x=element_text(size=8))
# 
# ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean VMD", 
                  x_lab, y_lab, char_param, PDRanges = PDR)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median VMD", 
                  x_lab, "Median VMD (µm)", "vmd_median", PDRanges = PDR)

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, "PDR", sep="-")

((p6 & theme(legend.position="none")) + p7 +
    plot_annotation(subtitle="Comparison of Mean and Median VMD of All Snapshots for All Experiments vs. Different Error Alarm Threshold Levels")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Alarm min scattering signal's effect on FPF, incorporating in PDRanges
# Should be very short data (2 data points each) - just a sanity check.
# Only exporting the Overall chart because the individual ones are less useful.
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "ALARMMIN"
char_param <- "fpf_mean"
err_bars <- c(TRUE, "fpf_sd")
x_lab <- "Error Alarm Min Scattering Signal"
y_lab <- "Mean FPF (µm)"
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
#     plot_annotation(subtitle="Mean FPF of All Snapshots for Each Experiment vs. Different Error Alarm Threshold Levels")
# ) & theme(
#   plot.subtitle=element_text(family=myFont, size=14, face="bold"),
#   axis.text.x=element_text(size=8))
# 
# ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean FPF", 
                  x_lab, y_lab, char_param, PDRanges = PDR)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median FPF", 
                  x_lab, "Median FPF (µm)", "fpf_median", PDRanges = PDR)

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, "PDR", sep="-")

((p6 & theme(legend.position="none")) + p7 +
    plot_annotation(subtitle="Comparison of Mean and Median FPF of All Snapshots for All Experiments vs. Different Error Alarm Threshold Levels")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

###############################
# Alarm min scattering signal's effect on RF, incorporating in PDRanges
# Should be very short data (2 data points each) - just a sanity check.
# Only exporting the Overall chart because the individual ones are less useful.
###############################
pltsnum <- pltsnum + 1

thing_to_vary <- "ALARMMIN"
char_param <- "rf_mean"
err_bars <- c(TRUE, "rf_sd")
x_lab <- "Error Alarm Min Scattering Signal"
y_lab <- "Mean RF (µm)"
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
#     plot_annotation(subtitle="Mean RF of All Snapshots for Each Experiment vs. Different Error Alarm Threshold Levels")
# ) & theme(
#   plot.subtitle=element_text(family=myFont, size=14, face="bold"),
#   axis.text.x=element_text(size=8))
# 
# ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 7, units = "in")

p6 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Mean RF", 
                  x_lab, y_lab, char_param, PDRanges = PDR)

p7 <- GetVaryPlot(AllExperiments, thing_to_vary, 
                  "All Experiments - Median RF", 
                  x_lab, "Median RF (µm)", "rf_median", PDRanges = PDR)

filename <- paste(pltsnum, "OverallPlot", paste0(char_param, "_vs_median"), thing_to_vary, "PDR", sep="-")

((p6 & theme(legend.position="none")) + p7 +
    plot_annotation(subtitle="Comparison of Mean and Median RF of All Snapshots for All Experiments vs. Different Error Alarm Threshold Levels")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 12, height = 5, units = "in")

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