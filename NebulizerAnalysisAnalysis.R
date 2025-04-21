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

rm(list = ls())

dataPath = "Data/SpraytecExportData"
animationExportPath = "Exports/PSDAnimations/"
boxPlotExportPath = "Exports/BoxPlots/"
dataExportPath = "Data/BatchData/"
myFont = "Century Gothic"

experiments <- c("Exp20", "Exp29", "Exp33", "Exp39", "Exp24")

experiment = experiments[[1]]
dataFolder = paste0(experiment, "-automated")

targetFolderPath <- paste0(dataPath,"/", dataFolder)

# exportDataVersion = "1042"
# 
# filename = paste0(experiment, "-", exportDataVersion, ".txt")
# fullDataPath = paste(dataPath, dataFolder, filename, sep="/")

# Fine particle fraction threshold: <= 5um according to EU Pharmacopoeia (Newman, 2022)
fpf_threshold <- 5

# "Extra-fine particle fraction" threshold can be <2um or <3um (Newman, 2022)
efpf_threshold <- 2

# It commonly appears that the respirable fraction threshold is <5um, according to
# multiple sources (Labiris & Dolovich, 2003) (Virden, 2011)
# But if we define respirable as being able to be breathed in & not caught in the
# nose and throat, and able to be deposited into the upper airways, then using
# (Nazarzadeh, 2022) article in ondrugdelivery, we can specify 10 um as our
# respirable cutoff. This way, our respirable fraction is different from our fpf.
# Respirable fraction <10um
rf_threshold <- 10

animation_fps <- 5
boxplot_boxfill = "#a5f2d6"


ProcessNebData <- function(path) {
  
  expname <- gsub(".txt", "", gsub("/", "", gsub(targetFolderPath, "", path)))
  
  correctedScatter_filename = paste0(experiment, "-correctedscatter.txt")
  correctedScatterPath = paste(dataPath, dataFolder, correctedScatter_filename, sep="/")
  
  data <- read_tsv(path) 
  
  # Keep this aside to ensure that all the bin values are the same.
  # From what I've seen, they are, but just in case!
  dts <- data %>%
    filter(`Date-Time` == "Date-Time")
  
  # Filter out all the rows that have the "Date-Time" thing going on.
  data <- data %>%
    filter(!(`Date-Time` == "Date-Time"))
  
  correctedScatter <- read_tsv(correctedScatterPath)
  
  test <- data[, 9:ncol(data)] %>%
    mutate_all(function(x) as.numeric(as.character(x)))
  
  test[which(is.na(test[,])), colnames(test)[colSums(is.na(test)) > 0]] = 0
  
  dateAndTime <- strptime(data$`Date-Time`[[1]], "%b %d %Y %H:%M:%S", tz="UTC")
  meanCorrectedScatter <- mean(t(correctedScatter[1, 3:38]))
  
  intervals <- data.frame(sizebin=factor(colnames(test), levels=colnames(test)), 
                          volumepc=as.numeric(test[1,]),
                          timestamp=rep(dateAndTime, ncol(test)),
                          mCS = rep(meanCorrectedScatter, ncol(test))
  )
  
  for (row in 2:nrow(test)){
    dateAndTime <- strptime(data$`Date-Time`[[row]], "%b %d %Y %H:%M:%S", tz="UTC")
    meanCorrectedScatter <- mean(t(correctedScatter[row, 3:38]))
    thisinterval <- data.frame(sizebin=factor(colnames(test), levels=colnames(test)), 
                               volumepc=as.numeric(test[row,]),
                               timestamp=rep(dateAndTime, ncol(test)),
                               mCS = rep(meanCorrectedScatter, ncol(test))
    )
    intervals <- rbind(intervals, thisinterval)
  }
  
  intervals <- intervals %>%
    mutate(dv50_bin = 0, vmd = 0,fpf=0, rf=0, efpf=0)
  
  all_ts <- unique(intervals$timestamp)
  
  for (t in 1:length(all_ts)){
    snapshot <- intervals %>% filter(timestamp == all_ts[[t]])
    med_vol <- 0
    cum_vol <- 0
    this_dv50 <- 0
    this_fpf <- 0
    this_rf <- 0
    this_efpf <- 0
    
    if (round(sum(snapshot$volumepc), 2) == 100){
      for (v in 1:nrow(snapshot)){
        if (cum_vol < 50){
          vol <- snapshot$volumepc[[v]]
          cum_vol <- cum_vol + vol
        } else {
          med_vol <- vol
          this_dv50 <- snapshot[which(snapshot$volumepc == med_vol), "sizebin"]
          
          getVolumes <- function(threshold){
            snapshot[which(as.numeric(as.character(snapshot$sizebin)) <= threshold), "volumepc"]
          }
          
          fpf_volumes <- getVolumes(fpf_threshold)
          this_fpf <- max(cumsum(fpf_volumes))/100
          
          rf_volumes <- getVolumes(rf_threshold)
          this_rf <- max(cumsum(rf_volumes))/100
          
          efpf_volumes <- getVolumes(efpf_threshold)
          this_efpf <- max(cumsum(efpf_volumes))/100
          
          intervals <- intervals %>%
            mutate(dv50_bin = ifelse(timestamp == all_ts[[t]], this_dv50, dv50_bin),
                   vmd = ifelse(timestamp == all_ts[[t]], as.numeric(as.character(this_dv50)), vmd),
                   fpf = ifelse(timestamp == all_ts[[t]], this_fpf, fpf),
                   rf = ifelse(timestamp == all_ts[[t]], this_rf, rf),
                   efpf = ifelse(timestamp == all_ts[[t]], this_efpf, efpf)
            )
        }
      }
    }
    print(paste(t, this_dv50, med_vol, cum_vol, this_fpf, this_rf, this_efpf, sep=" , "))
  }
  
  gradeScatter <- function(avgCS){
    if (avgCS > 250){
      grade = "A"
    } else if (avgCS > 150){
      grade = "B"
    } else if (avgCS > 75){
      grade = "C"
    } else if (avgCS > 25){
      grade = "D"
    } else {
      grade = "F"
    }
    grade
  }
  
  gradeScatter_V <- Vectorize(gradeScatter)
  
  intervals <- intervals %>%
    mutate(sGrade = gradeScatter_V(mCS))
  
  intervals_with_data <- intervals %>%
    group_by(timestamp, sGrade, mCS, vmd, fpf, rf, efpf) %>%
    tally() %>%
    select(-n) %>%
    filter(vmd != 0)
  
  # Probably will end up using the median to be a representative of the experiment,
  # but put together a summary table with the mean and standard deviation too just in case.
  
  A_intervals <- intervals_with_data %>% filter(sGrade == "A")
  B_intervals <- intervals_with_data %>% filter(sGrade == "B")
  C_intervals <- intervals_with_data %>% filter(sGrade == "C")
  D_intervals <- intervals_with_data %>% filter(sGrade == "D")
  F_intervals <- intervals_with_data %>% filter(sGrade == "F")
  
  intervals_summarized <- data.frame(experiment = expname,
                                     vmd_mean = mean(intervals_with_data$vmd),
                                     vmd_sd = sd(intervals_with_data$vmd),
                                     vmd_median = median(intervals_with_data$vmd),
                                     
                                     fpf_mean = mean(intervals_with_data$fpf),
                                     fpf_sd = sd(intervals_with_data$fpf),
                                     fpf_median = median(intervals_with_data$fpf),
                                     
                                     rf_mean = mean(intervals_with_data$rf),
                                     rf_sd = sd(intervals_with_data$rf),
                                     rf_median = median(intervals_with_data$rf),
                                     
                                     efpf_mean = mean(intervals_with_data$efpf),
                                     efpf_sd = sd(intervals_with_data$efpf),
                                     efpf_median = median(intervals_with_data$efpf),
                                     
                                     vmd_A_median = median(A_intervals$vmd),
                                     vmd_B_median = median(B_intervals$vmd),
                                     vmd_C_median = median(C_intervals$vmd),
                                     vmd_D_median = median(D_intervals$vmd),
                                     vmd_F_median = median(F_intervals$vmd),
                                     
                                     fpf_A_median = median(A_intervals$fpf),
                                     fpf_B_median = median(B_intervals$fpf),
                                     fpf_C_median = median(C_intervals$fpf),
                                     fpf_D_median = median(D_intervals$fpf),
                                     fpf_F_median = median(F_intervals$fpf),
                                     
                                     rf_A_median = median(A_intervals$rf),
                                     rf_B_median = median(B_intervals$rf),
                                     rf_C_median = median(C_intervals$rf),
                                     rf_D_median = median(D_intervals$rf),
                                     rf_F_median = median(F_intervals$rf),
                                     
                                     efpf_A_median = median(A_intervals$efpf),
                                     efpf_B_median = median(B_intervals$efpf),
                                     efpf_C_median = median(C_intervals$efpf),
                                     efpf_D_median = median(D_intervals$efpf),
                                     efpf_F_median = median(F_intervals$efpf)
  )
  
  bplotdf_fractions <- data.frame(parameter = c(
    rep("fpf", nrow(intervals_with_data)),
    rep("rf", nrow(intervals_with_data)),
    rep("efpf", nrow(intervals_with_data))),
    value = c(
      intervals_with_data$fpf,
      intervals_with_data$rf,
      intervals_with_data$efpf))
  
  bplotdf_vmd <- data.frame(parameter = c(rep("vmd", nrow(intervals_with_data))),
                            value = intervals_with_data$vmd
  )
  
  bplot_vmd <- ggplot(bplotdf_vmd, aes(x=toupper(parameter), y=value)) +
    geom_boxplot(aes(x=toupper(parameter), y=value), outlier.shape=1, fill = boxplot_boxfill) +
    stat_summary(fun=mean, geom = "point", size=2) +
    stat_boxplot(geom="errorbar") +
    theme_bw() +
    theme(text=element_text(family=myFont, size=12)) +
    xlab("Parameter") +
    ylab("Value (μm)")
  
  bplot_fractions <- ggplot(bplotdf_fractions, aes(x=toupper(parameter), y=value)) +
    geom_boxplot(aes(x=toupper(parameter), y=value), outlier.shape=1, fill = boxplot_boxfill) +
    stat_summary(fun=mean, geom = "point", size=2) +
    stat_boxplot(geom="errorbar") +
    theme_bw() +
    theme(text=element_text(family=myFont, size=12)) +
    xlab("Parameter") +
    ylab("Fraction of Total Volume")
  
  bplot_fractions + bplot_vmd + plot_annotation(
    title=paste0("Parameters for ", expname)
  ) &
    theme(text=element_text(family=myFont))
  
  ggsave(paste0(boxPlotExportPath,"BoxPlot_", expname,".svg"), width = 10, height = 4, units = "in")
  
  # plottest <- ggplot(intervals, aes(x=sizebin, y=volumepc, fill=sGrade)) +
  #   geom_bar(stat='identity') +
  #   geom_textvline(aes(xintercept = dv50_bin, label=paste0("VMD: ", as.character(vmd))), size=10, family=myFont) +
  #   ylim(0, (as.integer(max(intervals$volumepc)) + 1)) +
  #   theme_bw() +
  #   theme(axis.text.x=element_text(angle=90), axis.text = element_text(size=12), plot.title=element_text(size=40), text=element_text(family=myFont, size=15)) +
  #   labs(title = "{expname}", caption='Timestamp: {frame_time}, {format(round((frame_time - min(intervals$timestamp)), 2), nsmall = 2)} of {format(round((max(intervals$timestamp) - min(intervals$timestamp)), 2), nsmall = 2)}', x = 'Size (μm)', y = 'Volume Percentage (%)') +
  #   transition_time(timestamp) +
  #   ease_aes('linear')
  
  # animate(plottest, duration = nrow(data)/animation_fps, fps = animation_fps, width = 1920, height = 1080, renderer = gifski_renderer())
  # anim_save(paste0(animationExportPath, "PSDAnim_", expname,".gif"))
  
  write_csv(intervals, paste0(dataExportPath, "PSDs/PSDFull_", expname, ".csv"))
  write_csv(intervals_with_data, paste0(dataExportPath, "TimestampSummaries/TSSummary_", expname, ".csv"))
  write_csv(intervals_summarized, paste0(dataExportPath, "FileSummaries/FileSummary_", expname, ".csv"))
  
  return("DONE")
}
# 
# ProcessNebData(fullDataPath)

files_list <- list.files(path=targetFolderPath, pattern="[0-9].txt$", full.names = TRUE)
d1 <- lapply(files_list, ProcessNebData)

experiment = experiments[[2]]
dataFolder = paste0(experiment, "-automated")
targetFolderPath <- paste0(dataPath,"/", dataFolder)
files_list <- list.files(path=targetFolderPath, pattern="[0-9].txt$", full.names = TRUE)
d2 <- lapply(files_list, ProcessNebData)

experiment = experiments[[3]]
dataFolder = paste0(experiment, "-automated")
targetFolderPath <- paste0(dataPath,"/", dataFolder)
files_list <- list.files(path=targetFolderPath, pattern="[0-9].txt$", full.names = TRUE)
d3 <- lapply(files_list, ProcessNebData)

experiment = experiments[[4]]
dataFolder = paste0(experiment, "-automated")
targetFolderPath <- paste0(dataPath,"/", dataFolder)
files_list <- list.files(path=targetFolderPath, pattern="[0-9].txt$", full.names = TRUE)
d4 <- lapply(files_list, ProcessNebData)

experiment = experiments[[5]]
dataFolder = paste0(experiment, "-automated")
targetFolderPath <- paste0(dataPath,"/", dataFolder)
files_list <- list.files(path=targetFolderPath, pattern="[0-9].txt$", full.names = TRUE)
d5 <- lapply(files_list, ProcessNebData)
