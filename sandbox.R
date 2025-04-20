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