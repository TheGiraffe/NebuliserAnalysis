library(tidyverse)
library(ggplot2)
library(gganimate)
library(extrafont)
font_import()
loadfonts(device = "win")

rm(list = ls())

dataPath = "Data/ExportData"
dataFolder ="Exp020-automated"
filename = "Exp20-1001.txt"
fullPath = paste(dataPath, dataFolder, filename, sep="/")

data <- read_tsv(fullPath)

newBins <- which(data$`Date-Time` == "Date-Time")
data_subsets = list()

if (length(newBins) > 0){
  for (b in 1:length(newBins)){
    if (b < length(newBins)){
      if (b == 1){
        print(paste0(1, " ", newBins[[b]]))
        data_subsets[[b]] <- slice(data, 1:newBins[[b]])
      }
      print(paste0(newBins[[b]], " ", newBins[[b+1]]))
      data_subsets[[b+1]] <- slice(data, newBins[[b]]:newBins[[b+1]])
    } else {
      print(paste0(newBins[[b]], " ", nrow(data)))
      data_subsets[[b+1]] <- slice(data, newBins[[b]]:n())
    }
    newbinnames <- slice(data, newBins[[b]])
    newbinnames[,9:ncol(newbinnames)] <- lapply(newbinnames[,9:ncol(newbinnames)] , function(x) paste(x,"bin", sep="_"))
    colnames(data_subsets[[b+1]]) <- make.unique(as.character(newbinnames))
    data_subsets[[1]] <- data_subsets[[1]] %>% filter(`Date-Time` != "Date-Time")
    data_subsets[[b+1]] <- data_subsets[[b+1]] %>% filter(`Date-Time` != "Date-Time")
  }
} else {
  data_subsets[[1]] <- data
}

test <- read_tsv("Data/ExportData/test.txt")
test2 <- read_tsv("Data/ExportData/test2.txt")
t1 <- mean(t(test[1, 3:38]))
t2 <- mean(t(test2[1, 3:38]))

rawscatter <- read_tsv(rawScatterPath)
lightbg <- read_tsv(lightBgPath)

scatterdiff <- rawscatter[, 2:39] - lightbg[, 2:39]

scatterdiff <- scatterdiff %>%
  mutate(DateTime="", .before=`Sr[0]`)

scatterdiff$`Date-Time` <- rawscatter$`Date-Time`

scatterdiffThreshold = -1000

s1 <- scatterdiff %>% filter(`Sr[0]` > scatterdiffThreshold) %>% select(`Date-Time`)
s2 <- data %>% filter(`Cv(%)` <= 0) %>% select(`Date-Time`)

setdiff(s1, s2)

correctedscatter2 = read_tsv(paste(dataPath, dataFolder, "Exp20-correctedscatter2.txt", sep="/"))
correctedscatter3 = read_tsv(paste(dataPath, dataFolder, "Exp20-correctedscatter3.txt", sep="/"))

# timediff <- max(intervals$timestamp) - min(intervals$timestamp)


# barplot(height=as.numeric(test[1,]), names=colnames(test), col="green", border="pink")

# dftest <- data.frame(size=factor(colnames(test), levels=colnames(test)), vals=as.numeric(test[1,]))
# ggplot(dftest, aes(x=size, y=vals)) +
#   theme_bw() +
#   theme(axis.text.x=element_text(size=rel(0.5), angle=90)) +
#   geom_bar(stat='identity')



testFunct <- function(path){
  file <- read_tsv(path)
  date <- file$`Date-Time`[[1]]
  pathchange <- gsub("txt", "", gsub("[^a-zA-Z0-9_]", "", gsub(targetFolderPath, "", path)))
  
  expname <- gsub(".txt", "", gsub("/", "", gsub(targetFolderPath, "", path)))
  
  sillytestdata <- data.frame(
    parameters = c("sophia", "cleo", "bunbun", "thisfile", "dateofthisfile", "expname"),
    values = c("human", "cat", "plushy bunny", path, date, expname)
  )
  
  write_csv(sillytestdata, paste0(dataExportPath, "test/", pathchange, ".csv"))
}

files_list <- list.files(path=targetFolderPath, pattern="[0-9].txt$", full.names = TRUE)

d <- lapply(files_list, testFunct)

plot <- ggplot(VARY_ALARMMIN, aes(x=ALARMMIN, y=chr_par_main, group=experiment, color=experiment)) +
  geom_line() +
  geom_point() +
  # geom_errorbar(aes(ymin=chr_par_mean - chr_par_sd, ymax=chr_par_mean + chr_par_sd)) +
  theme_bw() +
  theme(text=element_text(family=myFont, size=12)) +
  scale_x_continuous(breaks = seq(0, 150, by=5))


GetVaryAlarmMinPlot = function(DataFrame, PlotDescription, XLab, YLab, ChrParCol, CustomColor = c(FALSE, ""), ErrBars=c(FALSE, "")){
  
  VARY_ALARMMIN <- DataFrame %>% filter(`VARY_ONLY_ALARM` == TRUE) %>%
    mutate(chr_par = get({{ChrParCol}}))
  tickint = 10
  
  if (ErrBars[[1]] == TRUE){
    ErrCol <- ErrBars[[2]]
    
    VARY_ALARMMIN <- VARY_ALARMMIN %>%
      mutate(chr_par_err = get({{ErrCol}}))
    
    if (CustomColor[[1]] == TRUE){
      
      plot <- ggplot(VARY_ALARMMIN, aes(x=ALARMMIN, y=chr_par, group=experiment)) +
        geom_line(aes(x=ALARMMIN, y=chr_par, group=experiment), color=CustomColor[[2]]) +
        geom_point(aes(x=ALARMMIN, y=chr_par, group=experiment), color=CustomColor[[2]]) +
        geom_errorbar(aes(ymin=chr_par - chr_par_err, ymax=chr_par + chr_par_err), color=CustomColor[[2]], size=0.1) +
        theme_bw() +
        theme(text=element_text(family=myFont, size=12)) +
        scale_x_continuous(breaks = seq(0, 150, by=tickint)) +
        xlab(XLab) +
        ylab(YLab) +
        ggtitle(PlotDescription)
      
    } else {
      plot <- ggplot(VARY_ALARMMIN, aes(x=ALARMMIN, y=chr_par, group=experiment, color=experiment)) +
        geom_line() +
        geom_point() +
        geom_errorbar(aes(ymin=chr_par - chr_par_err, ymax=chr_par + chr_par_err), size=0.1) +
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
      
      plot <- ggplot(VARY_ALARMMIN, aes(x=ALARMMIN, y=chr_par, group=experiment)) +
        geom_line(aes(x=ALARMMIN, y=chr_par, group=experiment), color=CustomColor[[2]]) +
        geom_point(aes(x=ALARMMIN, y=chr_par, group=experiment), color=CustomColor[[2]]) +
        theme_bw() +
        theme(text=element_text(family=myFont, size=12)) +
        scale_x_continuous(breaks = seq(0, 150, by=tickint)) +
        xlab(XLab) +
        ylab(YLab) +
        ggtitle(PlotDescription)
      
    } else {
      plot <- ggplot(VARY_ALARMMIN, aes(x=ALARMMIN, y=chr_par, group=experiment, color=experiment)) +
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
  return(plot)
}

# GetVaryAlarmMinPlot("Fine Particle Fraction", "Value", "fpf_mean", c(TRUE, "fpf_sd"))

# Mean VMD (µm)

custom_colors <- hue_pal()(5)
p1 <- GetVaryAlarmMinPlot(Exp20_df, 
                          Exp20_df$experiment[[1]],
                          "Error Alarm Min Scattering Signal", "Mean VMD (µm)", "vmd_mean", 
                          ErrBars = c(TRUE, "vmd_sd"),
                          CustomColor = c(TRUE, custom_colors[[1]]))

p2 <- GetVaryAlarmMinPlot(Exp24_df, 
                          Exp24_df$experiment[[1]],
                          "Error Alarm Min Scattering Signal", "Mean VMD (µm)", "vmd_mean", 
                          ErrBars = c(TRUE, "vmd_sd"),
                          CustomColor = c(TRUE, custom_colors[[2]]))

p3 <- GetVaryAlarmMinPlot(Exp29_df, 
                          Exp29_df$experiment[[1]],
                          "Error Alarm Min Scattering Signal", "Mean VMD (µm)", "vmd_mean", 
                          ErrBars = c(TRUE, "vmd_sd"),
                          CustomColor = c(TRUE, custom_colors[[3]]))

p4 <- GetVaryAlarmMinPlot(Exp33_df, 
                          Exp33_df$experiment[[1]],
                          "Error Alarm Min Scattering Signal", "Mean VMD (µm)", "vmd_mean", 
                          ErrBars = c(TRUE, "vmd_sd"),
                          CustomColor = c(TRUE, custom_colors[[4]]))

p5 <- GetVaryAlarmMinPlot(Exp39_df, 
                          Exp39_df$experiment[[1]],
                          "Error Alarm Min Scattering Signal", "Mean VMD (µm)", "vmd_mean", 
                          ErrBars = c(TRUE, "vmd_sd"),
                          CustomColor = c(TRUE, custom_colors[[5]]))

p6 <- GetVaryAlarmMinPlot(AllExperiments, "All Experiments - Mean VMD", "Error Alarm Min Scattering Signal", "Mean VMD (µm)", "vmd_mean")
p7 <- GetVaryAlarmMinPlot(AllExperiments, "All Experiments - Median VMD", "Error Alarm Min Scattering Signal", "Median VMD (µm)", "vmd_median")

filename <- "MeanVMD_Each_VARYALARMMIN"
((p1 | p2 | p3) /
    (p4 | p5) +
    plot_annotation(subtitle="Mean VMD of All Snapshots for Each Experiment vs. Different Error Alarm Threshold Levels")
) & theme(
  plot.subtitle=element_text(family=myFont, size=14, face="bold"),
  axis.text.x=element_text(size=8))
ggsave(paste0(exportPath,filename,".svg"), width = 10, height = 7, units = "in")

filename <- "MeanvsMedianVMD_All_VARYALARMMIN"
((p6 & theme(legend.position="none")) + p7 +
    plot_annotation(subtitle="Comparison of Mean and Median VMD of All Snapshots for All Experiments vs. Different Error Alarm Threshold Levels")
) & theme(plot.subtitle=element_text(family=myFont, size=14, face="bold"), 
          axis.text.x=element_text(size=8))

ggsave(paste0(exportPath,filename,".svg"), width = 10, height = 4, units = "in")

# GetVaryAlarmMinPlot("Fine Particle Fraction", "Value", "fpf_mean", c(TRUE, "fpf_sd"))

# Mean VMD (µm)


p2 <- GetVaryPlot(Exp24_df, 
                  "ALARMMIN",
                  Exp24_df$experiment[[1]],
                  "Error Alarm Min Scattering Signal", "Mean VMD (µm)", "vmd_mean", 
                  ErrBars = c(TRUE, "vmd_sd"),
                  CustomColor = c(TRUE, custom_colors[[2]]))

p3 <- GetVaryPlot(Exp29_df, 
                  "ALARMMIN",
                  Exp29_df$experiment[[1]],
                  "Error Alarm Min Scattering Signal", "Mean VMD (µm)", "vmd_mean", 
                  ErrBars = c(TRUE, "vmd_sd"),
                  CustomColor = c(TRUE, custom_colors[[3]]))

p4 <- GetVaryPlot(Exp33_df, 
                  "ALARMMIN",
                  Exp33_df$experiment[[1]],
                  "Error Alarm Min Scattering Signal", "Mean VMD (µm)", "vmd_mean", 
                  ErrBars = c(TRUE, "vmd_sd"),
                  CustomColor = c(TRUE, custom_colors[[4]]))

p5 <- GetVaryPlot(Exp39_df, 
                  "ALARMMIN",
                  Exp39_df$experiment[[1]],
                  "Error Alarm Min Scattering Signal", "Mean VMD (µm)", "vmd_mean", 
                  ErrBars = c(TRUE, "vmd_sd"),
                  CustomColor = c(TRUE, custom_colors[[5]]))


test <- Exp29_df %>% filter(VARY_ONLY_DETFIRST == TRUE)