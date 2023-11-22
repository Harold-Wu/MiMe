# Figure 1 - a & b
library(ggplot2)
library(reshape2)
library(vegan)

load("/mnt/research/quantgen/projects/harold/InfantDev2/data/micro_filter_logit_data.RData")
load("/mnt/research/quantgen/projects/harold/InfantDev2/data/meta_filter_logit_data.RData")

meta_shannon <- diversity(meta_data[,-1:-8])
micro_shannon <- diversity(micro_data[,-1:-8])
meta_shannon <- cbind(meta_shannon, meta_data[,c(2,3,4,6,7)])
micro_shannon <- cbind(micro_shannon, micro_data[,c(2,3,4,6,7)])

p1 <- ggplot(meta_shannon, aes(x = age_mos, y = meta_shannon))+
  geom_point(size = 1.5)+
  stat_smooth(method = "lm")+
  labs(title = "Metabolome", x = "Age (months)", y = "Shannon index")
p2 <- ggplot(micro_shannon, aes(x = age_mos, y = micro_shannon))+
  geom_point(size = 1.5)+
  stat_smooth(method = "lm")+
  labs(title = "Microbiome", x = "Age (months)", y = "Shannon index")
