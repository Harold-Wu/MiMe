# Figure 4 - b
library(dplyr)
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

signi_final$genus[signi_final$genus=='Clostridium sensu stricto 1']='Clostridium sensu stricto'

signi_final=signi_final[,c(1,2,3,4,7,13:19)]

for (i in 1:nrow(signi_final)) {
  if (is.na(signi_final$species[i])) {
    signi_final$species[i]=paste0(signi_final$genus[i], ' sp.')
  }
}
signi_final$species[which(signi_final$species=='NA sp.')]=NA
for (i in 1:nrow(signi_final)) {
  if (is.na(signi_final$species[i])) {
    signi_final$species[i]=paste0(signi_final$family[i], ' family')
  }
}
signi_final$species[which(signi_final$species=='Lachnoclostridium sp.')]='Lachnospiraceae family'
signi_final=signi_final[,c(1,2,3,4,5,12)]

signi_final2=signi_final
signi_final2$NPC_level3[signi_final2$NPC_level3 %in% names(table(signi_final2$NPC_level3))[which(table(signi_final2$NPC_level3) < 35)]]='Others'

species_table=table(signi_final2$species)
species_table=species_table[order(species_table, decreasing=T)]

NPC_table=table(signi_final2$NPC_level3)
NPC_table=NPC_table[order(NPC_table, decreasing=T)]

tmp=count(signi_final2[,5:6], NPC_level3, species)
tmp=tmp[order(tmp$n,decreasing = T),]

tmp$species=factor(tmp$species, levels=names(species_table))
tmp$NPC_level3=factor(tmp$NPC_level3, , levels=c(names(NPC_table)[-which(names(NPC_table)=='Others')], 'Others'))

f4b=ggplot(data=tmp, aes(x=species, y=n, fill=NPC_level3, label=n)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust=1)) +
  labs(x = "Microbial species", y = "# metabolite with significant association", fill='Metabolome group')+
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18), axis.text.y=element_text(size = 18),
        legend.title=element_text(size=20), legend.text=element_text(size=18),
        legend.position = c(0.88, 0.83))+ scale_fill_brewer(palette="Dark2")
