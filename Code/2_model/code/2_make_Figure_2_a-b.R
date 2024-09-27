# Figure 2 - a & b
library(ggplot2)
library(reshape2)
library(latex2exp)

load("../output/micro_pop_expected.RData")
load("../output/meta_pop_expected.RData")
load("../output/micro_fixef.RData")
load("../output/meta_fixef.RData")

meta_prob <- 1-meta_prob

meta_prob$group <- ifelse(meta_prob$X1 > 0.7, "rare", "abundant")
meta_prob$group2 <- NA
for (i in 1:nrow(meta_prob)) {
  meta_prob$group2[i] <- ifelse(meta_prob$X1[i]-meta_prob$X14[i] > 0, "Decrease", "Increase")
  if (p.adjust(meta_fixef$age_p, "fdr")[which(rownames(meta_fixef) == rownames(meta_prob)[i])] > 0.05) {
    meta_prob$group2[i] <- "Not significant"
  }
}
meta_prob_exp_long <- melt(t(meta_prob[,1:23]))
meta_prob_exp_long$age <- rep(seq(1,12,0.5), times = nrow(meta_prob_exp_long)/23)
meta_prob_exp_long$group <- rep(meta_prob$group, each = 23)
meta_prob_exp_long$group2 <- rep(meta_prob$group2, each = 23)

micro_prob <- 1-micro_prob

micro_prob$group <- ifelse(micro_prob$X1 > 0.85, "rare", "abundant")
micro_prob$group2 <- NA
for (i in 1:nrow(micro_prob)) {
  micro_prob$group2[i] <- ifelse(micro_prob$X1[i]-micro_prob$X14[i] > 0, "Decrease", "Increase")
  if (p.adjust(micro_fixef$age_p, "fdr")[which(rownames(micro_fixef) == rownames(micro_prob)[i])] > 0.05) {
    micro_prob$group2[i] <- "Not significant"
  }
}
micro_prob_exp_long <- melt(t(micro_prob[,1:23]))
micro_prob_exp_long$age <- rep(seq(1,12,0.5), times = nrow(micro_prob_exp_long)/23)
micro_prob_exp_long$group <- rep(micro_prob$group, each = 23)
micro_prob_exp_long$group2 <- rep(micro_prob$group2, each = 23)

f2a <- ggplot(meta_prob_exp_long[meta_prob_exp_long$group2 != 'Not significant',], aes(x = age, y = value, group = Var2, color = group2))+
  geom_point(size = 0.5)+
  geom_line(linewidth = 0.3)+
  scale_y_continuous(limits = c(0,1))+
  labs(title = "Metabolome", x = "Age (months)", y = "Proportion of zeros", color = "")+
  scale_color_manual(values=c('#1E88E5', '#FFAC0A'))

f2b <- ggplot(micro_prob_exp_long[micro_prob_exp_long$group2 != 'Not significant',], aes(x = age, y = value, group = Var2, color = group2))+
  geom_point(size = 0.5)+
  geom_line(linewidth = 0.3)+
  scale_y_continuous(limits = c(0,1))+
  labs(title = "Microbiome", x = "Age (months)", y = "Proportion of zeros", color = "")+
  scale_color_manual(values=c('#1E88E5', '#FFAC0A'))