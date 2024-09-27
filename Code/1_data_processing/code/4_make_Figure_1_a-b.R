# Figure 1 - a & b
library(ggplot2)
library(reshape2)
library(vegan)

load("/Data/micro_QCed_data.RData")
load("/Data/meta_QCed_data.RData")

meta_shannon <- diversity(meta_data[,-1:-12])
micro_shannon <- diversity(micro_data[,-1:-12])
meta_shannon <- cbind(meta_shannon, meta_data[,c(2,3,4,6,7)])
micro_shannon <- cbind(micro_shannon, micro_data[,c(2,3,4,6,7)])

f1a <- ggplot(meta_shannon, aes(x = age_mos, y = meta_shannon))+
  geom_point(size = 1.5)+
  stat_smooth(method = "lm")+
  labs(title = "Metabolome", x = "Age (months)", y = "Shannon index")+
  annotate(geom="text", x=13.1, y=6.3, label=TeX("$P=0.0140$"), size=5)
f1b <- ggplot(micro_shannon, aes(x = age_mos, y = micro_shannon))+
  geom_point(size = 1.5)+
  stat_smooth(method = "lm")+
  labs(title = "Microbiome", x = "Age (months)", y = "Shannon index")+
  annotate(geom="text", x=12.7, y=6.3, label=TeX("$P<2.2e-16$"), size=5)