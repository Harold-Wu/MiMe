# Figure 3 - a & b
library(ggplot2)
library(reshape2)

load("../output/micro_pop_expected.RData")
load("../output/meta_pop_expected.RData")
micro_popdiff <- data.frame("name"=rownames(micro_prob),
                       "prob" = micro_prob$X14 - micro_prob$X2)
micro_popdiff <- micro_popdiff[order(-micro_popdiff$prob),] 
meta_popdiff <- data.frame("name"=rownames(meta_prob),
                      "prob" = meta_prob$X14 - meta_prob$X2)
meta_popdiff <- meta_popdiff[order(-meta_popdiff$prob),] 

load("../output/micro_ind_expected_diff.RData")
load("../output/meta_ind_expected_diff.RData")
micro_prob_ind <- micro_prob_ind[,micro_popdiff$name]
meta_prob_ind <- meta_prob_ind[,meta_popdiff$name]

micro_long <- melt(micro_prob_ind)
meta_long <- melt(meta_prob_ind)


micro_long$pop <- NA
for (i in 1:ncol(micro_prob_ind)) {
    micro_long$pop[nrow(micro_prob_ind)*(i-1)+1] <- micro_popdiff$prob[which(micro_popdiff$name == micro_long$Var2[nrow(micro_prob_ind)*(i-1)+1])]
}
meta_long$pop <- NA
for (i in 1:ncol(meta_prob_ind)) {
    meta_long$pop[nrow(meta_prob_ind)*(i-1)+1] <- meta_popdiff$prob[which(meta_popdiff$name == meta_long$Var2[nrow(meta_prob_ind)*(i-1)+1])]
}

micro_long[,3:4] <- -micro_long[,3:4]
meta_long[,3:4] <- -meta_long[,3:4]

f3a <- ggplot() + 
  geom_jitter(data = meta_long, aes(x=Var2, y=value), position=position_jitter(0.2), cex=1, color="lightgray", shape = 21) +
  geom_jitter(data = meta_long[!is.na(meta_long$pop),], aes(x=Var2, y=pop, colour=group), position=position_jitter(0.2), cex=3, shape=17) +
  scale_color_manual(values=c('#56B4E9', '#E69F00', '#999999'))+
  coord_flip()+ 
  #stat_summary(fun=mean, geom="point", shape=18, size=1, color="#FC4E07")+
  labs(title = "Metabolome", x = "Metabolite", y = "Expected change of proportion of zeros in the first year of life", color='')+
  #geom_vline(xintercept = c(655.5, 656.5), linetype = "dashed", color="darkgray")+
  scale_y_continuous(limits = c(-1.05,1.05))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray")+
  geom_rect(aes(xmin = 315.5, xmax = 445.5, ymin = -1.02, ymax = 1.02), color = "purple", alpha = 0, linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18), axis.text.y=element_blank(),
        legend.title=element_text(size=20), legend.text=element_text(size=18),
        axis.ticks.y=element_blank())

f3b <- ggplot() + 
  geom_jitter(data = micro_long, aes(x=Var2, y=value), position=position_jitter(0.2), cex=1, color="lightgray", shape = 21) +
  geom_jitter(data = micro_long[!is.na(micro_long$pop),], aes(x=Var2, y=pop, colour=group), position=position_jitter(0.2), cex=3, shape=17) +
  scale_color_manual(values=c('#56B4E9', '#E69F00', '#999999'))+
  coord_flip()+ 
  #stat_summary(fun=mean, geom="point", shape=18, size=2, color="#FC4E07")+
  labs(title = "Microbiome", x = "ASV", y = "Expected change of proportion of zeros in the first year of life", color = '')+
  scale_y_continuous(limits = c(-1.05,1.05))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray")+
  geom_rect(aes(xmin = 60.5, xmax = 71.5, ymin = -1.02, ymax = 1.02), color = "purple", alpha = 0, linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18), axis.text.y=element_blank(),
        legend.title=element_text(size=20), legend.text=element_text(size=18),
        axis.ticks.y=element_blank())