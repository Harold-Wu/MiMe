# Figure 5 - a & b
library(ggplot2)
library(ggpubr)

load("../output/pairwise_cor_signi.RData")
load("/Data/meta_taxa.RData")
load("/Data/micro_taxa.RData")
colnames(micro_taxa)[1] <- "micro"
colnames(meta_taxa)[1] <- "meta"

signi_taxa <- merge(signi_index[,-c(1,2,6)], meta_taxa, by = "meta")
signi_taxa <- merge(signi_taxa, micro_taxa, by = "micro")

family <- read.csv("../output/enrich_test/micro_family.csv")
micro_signi <- family$group[which(family$adj_pval < 0.05)]

level3 <- read.csv("../output/enrich_test/meta_NPC_level3.csv")
meta_signi <- level3$group[which(level3$adj_pval < 0.05)]

signi_final <- signi_taxa[which(signi_taxa$family %in% micro_signi & signi_taxa$NPC_level3 %in% meta_signi), c(1,2,3,4,7,17)]

load("../output/micro_pop_expected.RData")
load("../output/meta_pop_expected.RData")

micro_prob=micro_prob[rownames(micro_prob) %in% unique(signi_final$micro),]
micro_prob <- 1-micro_prob
meta_prob=meta_prob[rownames(meta_prob) %in% unique(signi_final$meta),]
meta_prob <- 1-meta_prob

micro_prob$change=micro_prob$X14-micro_prob$X2
meta_prob$change=meta_prob$X14-meta_prob$X2

micro_change=data.frame(micro=rownames(micro_prob), change=micro_prob$change, species=signi_final$family[match(rownames(micro_prob), signi_final$micro)])
meta_change=data.frame(meta=rownames(meta_prob), change=meta_prob$change, NPC_level3=signi_final$NPC_level3[match(rownames(meta_prob), signi_final$meta)])

micro_change$micro=factor(micro_change$micro, levels=micro_change$micro[order(micro_change$change)])
meta_change$meta=factor(meta_change$meta, levels=meta_change$meta[order(meta_change$change)])

f5a=ggplot(data=meta_change, aes(x=meta, y=change, fill=NPC_level3)) +
  geom_bar(stat="identity") +
  labs(x = "Metabolite", y = "Expected change of proportion of zeros in the first year of life", fill='Metabolome groups')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
        axis.text.x = element_blank(), axis.text.y=element_text(size = 18),
        legend.title=element_text(size=20), legend.text=element_text(size=18),
        legend.position = c(0.32,0.8), axis.ticks.x=element_blank())

f5b=ggplot(data=micro_change, aes(x=micro, y=change, fill=species)) +
  geom_bar(stat="identity") +
  labs(x = "ASV", y = "Expected change of proportion of zeros in the first year of life", fill='Microbial families')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
        axis.text.x = element_blank(), axis.text.y=element_text(size = 18),
        legend.title=element_text(size=20), legend.text=element_text(size=18),
        legend.position = c(0.92,0.15), axis.ticks.x=element_blank())