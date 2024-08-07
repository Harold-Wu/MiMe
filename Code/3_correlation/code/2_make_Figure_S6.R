# Figure S6
library(ggplot2)
library(ggpubr)
library(latex2exp)
COR=read.csv("../output/pairwise_cor.csv")
load("../output/micro_ind_expected_diff.RData")
load("../output/meta_ind_expected_diff.RData")

commonID=intersect(rownames(meta_prob_ind), rownames(micro_prob_ind))
meta_prob_ind=meta_prob_ind[commonID,]
micro_prob_ind=micro_prob_ind[commonID,]

top3=COR[order(COR$boot_cor,decreasing=T),][1:3,]
bottom3=COR[order(COR$boot_cor),][1:3,]

top3_table=meta_prob_ind[,colnames(meta_prob_ind)%in%top3$meta]
top3_table=as.data.frame(cbind(top3_table, micro_prob_ind[,colnames(micro_prob_ind)%in%top3$micro]))

bottom3_table=meta_prob_ind[,colnames(meta_prob_ind)%in%bottom3$meta]
bottom3_table=as.data.frame(cbind(bottom3_table, 'ASV_SILVA_1412'=micro_prob_ind[,colnames(micro_prob_ind)%in%bottom3$micro]))

s6a <- ggplot(top3_table, aes(x = Cluster5816, y = ASV_SILVA_1412))+
  geom_point()+
  annotate(geom="text", x=-0.565, y=0.725, label=TeX("$r=0.786$"), size=5)+

s6b <- ggplot(top3_table, aes(x = Cluster9642, y = ASV_SILVA_1412))+
  geom_point()+
  annotate(geom="text", x=-0.48, y=0.725, label=TeX("$r=0.757$"), size=5)+

s6c <- ggplot(top3_table, aes(x = Cluster3501, y = ASV_SILVA_79))+
  geom_point()+
  annotate(geom="text", x=-0.17, y=0.49, label=TeX("$r=0.668$"), size=5)+

s6d <- ggplot(bottom3_table, aes(x = Cluster5404, y = ASV_SILVA_1412))+
  geom_point()+
  annotate(geom="text", x=-0.075, y=0.63, label=TeX("$r=-0.743$"), size=5)+

s6e <- ggplot(bottom3_table, aes(x = Cluster1614, y = ASV_SILVA_1412))+
  geom_point()+
  annotate(geom="text", x=-0.05, y=0.63, label=TeX("$r=-0.716$"), size=5)+

s6f <- ggplot(bottom3_table, aes(x = Cluster4271, y = ASV_SILVA_1412))+
  geom_point()+
  annotate(geom="text", x=-0.015, y=0.63, label=TeX("$r=-0.692$"), size=5)+
