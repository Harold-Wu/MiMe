# SupFigure 1
library(ggplot2)
library(vegan)

load("/Data/micro_QCed_data.RData")
load("/Data/meta_QCed_data.RData")

meta_BC=rbind(data.frame('BC'=c(vegdist(meta_data[meta_data$timepoint == 2,-1:-8])), 'timepoint'=2, 'age'=colMeans(combn(meta_data[meta_data$timepoint == 2,6],2))),
              data.frame('BC'=c(vegdist(meta_data[meta_data$timepoint == 4,-1:-8])), 'timepoint'=4, 'age'=colMeans(combn(meta_data[meta_data$timepoint == 4,6],2))),
              data.frame('BC'=c(vegdist(meta_data[meta_data$timepoint == 6,-1:-8])), 'timepoint'=6, 'age'=colMeans(combn(meta_data[meta_data$timepoint == 6,6],2))),
              data.frame('BC'=c(vegdist(meta_data[meta_data$timepoint == 9,-1:-8])), 'timepoint'=9, 'age'=colMeans(combn(meta_data[meta_data$timepoint == 9,6],2))),
              data.frame('BC'=c(vegdist(meta_data[meta_data$timepoint == 12,-1:-8])), 'timepoint'=12, 'age'=colMeans(combn(meta_data[meta_data$timepoint == 12,6],2))))

micro_BC=rbind(data.frame('BC'=c(vegdist(micro_data[micro_data$timepoint == 2,-1:-8])), 'timepoint'=2, 'age'=colMeans(combn(micro_data[micro_data$timepoint == 2,6],2))),
              data.frame('BC'=c(vegdist(micro_data[micro_data$timepoint == 4,-1:-8])), 'timepoint'=4, 'age'=colMeans(combn(micro_data[micro_data$timepoint == 4,6],2))),
              data.frame('BC'=c(vegdist(micro_data[micro_data$timepoint == 6,-1:-8])), 'timepoint'=6, 'age'=colMeans(combn(micro_data[micro_data$timepoint == 6,6],2))),
              data.frame('BC'=c(vegdist(micro_data[micro_data$timepoint == 9,-1:-8])), 'timepoint'=9, 'age'=colMeans(combn(micro_data[micro_data$timepoint == 9,6],2))),
              data.frame('BC'=c(vegdist(micro_data[micro_data$timepoint == 12,-1:-8])), 'timepoint'=12, 'age'=colMeans(combn(micro_data[micro_data$timepoint == 12,6],2))))

s1a=ggplot(meta_BC) + 
  geom_violin(aes(x=timepoint, y=BC, group=timepoint))+
  geom_boxplot(aes(x=timepoint, y=BC, group=timepoint),width=0.3)+
  geom_smooth(aes(x=age, y=BC),method = "lm", linewidth=0.5, level = 0.99)+
  scale_y_continuous(limits = c(0,1))+
  labs(title = "Metabolome", y="Bray-Curtis Distance", x="Age (months)")+
  annotate(geom="text", x=12.7, y=0, label=TeX("$P<2e-16$"), size=5)

s1b=ggplot(micro_BC, aes(x=timepoint, y=BC)) + 
  geom_violin(aes(x=timepoint, y=BC, group=timepoint))+
  geom_boxplot(aes(x=timepoint, y=BC, group=timepoint),width=0.3)+
  geom_smooth(aes(x=age, y=BC),method = "lm", linewidth=0.5, level = 0.99)+
  scale_y_continuous(limits = c(0,1))+
  labs(title = "Microbiome", y="Bray-Curtis Distance", x="Age (months)")+
  annotate(geom="text", x=12.7, y=0, label=TeX("$P=0.0622$"), size=5)
