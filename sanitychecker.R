Eth25_AVGS <- data.frame(name = "25% Ethanol", eth=25, n=nrow(Eth25), 
                         VMD_Mean_MEDIAN = median(Eth25$vmd_median),
                         VMD_Mean_MEAN = mean(Eth25$vmd_mean),
                         VMD_Mean_STDEV = sd(Eth25$vmd_median),
                         FPF_Mean_MEAN = mean(Eth25$fpf_mean),
                         FPF_Mean_STDEV = sd(Eth25$fpf_mean),
                         RF_Mean_MEAN = mean(Eth25$rf_mean),
                         RF_Mean_STDEV = sd(Eth25$rf_mean),
                         eFPF_Mean_MEAN = mean(Eth25$efpf_mean),
                         eFPF_Mean_STDEV = sd(Eth25$efpf_mean))