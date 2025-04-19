library(tidyverse)
library(ggplot2)
library(gganimate)

rm(list = ls())

dataPath = "Data/ExportData"
dataFolder ="Exp033-automated"
filename = "Exp33-1001.txt"
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

test <- data[, 9:ncol(data)] %>%
  mutate_all(function(x) as.numeric(as.character(x)))

test[which(is.na(test[,])), colnames(test)[colSums(is.na(test)) > 0]] = 0

dateAndTime <- strptime(data$`Date-Time`[[1]], "%b %d %Y %H:%M:%S", tz="UTC")
intervals <- data.frame(sizebin=colnames(test), volumepc=as.numeric(test[1,]), timestamp=rep(dateAndTime, ncol(test)))

for (row in 2:nrow(test)){
  dateAndTime <- strptime(data$`Date-Time`[[row]], "%b %d %Y %H:%M:%S", tz="UTC")
  thisinterval <- data.frame(sizebin=colnames(test), volumepc=as.numeric(test[row,]), timestamp=rep(dateAndTime, ncol(test)))
  intervals <- rbind(intervals, thisinterval)
}

# plottest <- ggplot(intervals, aes(x=sizebin, y=volumepc)) +
#   geom_bar(stat='identity') +
#   ylim(0, 100)
# plottest

plottest <- ggplot(intervals, aes(x=sizebin, y=volumepc)) +
  geom_bar(stat='identity') +
  ylim(0, (as.integer(max(volumepc)) + 1)) +
  transition_time(timestamp) +
  ease_aes('linear')


animate(plottest, duration = 45.5, fps = 10, width = 1000, height = 500, renderer = gifski_renderer())
anim_save("test-plot-animation.gif")


barplot(height=as.numeric(test[5,]), names=colnames(test), col="green", border="pink")
