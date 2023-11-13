# Figure 2 - a & b & c & d
library(ggplot2)
library(reshape2)
library(latex2exp)

load("/mnt/research/quantgen/projects/harold/InfantDev2/output/logistic/micro_pop_expected.RData")
load("/mnt/research/quantgen/projects/harold/InfantDev2/output/logistic/meta_pop_expected.RData")
load("/mnt/research/quantgen/projects/harold/InfantDev2/output/logistic/micro_fixef.RData")
load("/mnt/research/quantgen/projects/harold/InfantDev2/output/logistic/meta_fixef.RData")

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

p2_1 <- ggplot(meta_prob_exp_long[which(meta_prob_exp_long$group == "rare"),], aes(x = age, y = value, group = Var2, color = group2))+
  geom_point(size = 0.5)+
  geom_line()+
  labs(subtitle = TeX("Proportion of zeros at the first time-point $\\geq$ 0.7"), x = "Age (months)", y = "Proportion of zeros", color = "")+
  scale_color_manual(values=c('#56B4E9', '#E69F00', '#999999'))
p2_2 <- ggplot(meta_prob_exp_long[which(meta_prob_exp_long$group == "abundant"),], aes(x = age, y = value, group = Var2, color = group2))+
  geom_point(size = 0.5)+
  geom_line()+
  labs(subtitle = TeX("Proportion of zeros at the first time-point $<$ 0.7"), x = "Age (months)", y = "Proportion of zeros", color = "")+
  scale_color_manual(values=c('#56B4E9', '#E69F00', '#999999'))

micro_prob <- 1-micro_prob

micro_prob$group <- ifelse(micro_prob$X1 > 0.5, "rare", "abundant")
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

p2_3 <- ggplot(micro_prob_exp_long[which(micro_prob_exp_long$group == "rare"),], aes(x = age, y = value, group = Var2, color = group2))+
  geom_point(size = 0.5)+
  geom_line()+
  labs(subtitle = TeX("Proportion of zeros at the first time-point $\\geq$ 0.5"), x = "Age (months)", y = "Proportion of zeros", color = "")+
  scale_color_manual(values=c('#56B4E9', '#E69F00', '#999999'))
p2_4 <- ggplot(micro_prob_exp_long[which(micro_prob_exp_long$group == "abundant"),], aes(x = age, y = value, group = Var2, color = group2))+
  geom_point(size = 0.5)+
  geom_line()+
  labs(subtitle = TeX("Proportion of zeros at the first time-point $<$ 0.5"), x = "Age (months)", y = "Proportion of zeros", color = "")+
  scale_color_manual(values=c('#56B4E9', '#E69F00', '#999999'))