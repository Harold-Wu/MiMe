# SupFigure 2
library(ggplot2)
library(vegan)

load("/Data/micro_QCed_data.RData")
load("/Data/meta_QCed_data.RData")

meta_evenness <- diversity(meta_data[,-1:-8])/rowSums(meta_data[,-1:-8] != 0)
micro_evenness <- diversity(micro_data[,-1:-8])/rowSums(micro_data[,-1:-8] != 0)
meta_evenness <- cbind(meta_evenness, meta_data[,c(2,3,4,6,7)])
micro_evenness <- cbind(micro_evenness, micro_data[,c(2,3,4,6,7)])

s2a <- ggplot(meta_evenness, aes(x = age_mos, y = meta_evenness))+
  geom_point(size = 1.5)+
  stat_smooth(method = "lm")+
  labs(title = "Metabolome", x = "Age (months)", y = "Pielou's J")+
  annotate(geom="text", x=12.7, y=0.037, label=TeX("$P=1.17e-9$"), size=5)

s2b <- ggplot(micro_evenness, aes(x = age_mos, y = micro_evenness))+
  geom_point(size = 1.5)+
  stat_smooth(method = "lm")+
  labs(title = "Microbiome", x = "Age (months)", y = "Pielou's J")+
  annotate(geom="text", x=12.7, y=0.14, label=TeX("$P=1.21e-4$"), size=5)

