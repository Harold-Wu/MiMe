# Figure 1 - c & d
library(ggplot2)
library(reshape2)

load("/Data/micro_QCed_data.RData")
load("/Data/meta_QCed_data.RData")

meta_prob_obs <- data.frame("name" = colnames(meta_data)[-1:-8],
                            "t2" = NA,
                            "t4" = NA,
                            "t6" = NA,
                            "t9" = NA,
                            "t12" = NA)

meta_01 <- meta_data[,-1:-8] == 0
meta_01 <- cbind("time" = meta_data[,4], meta_01)
for (i in 2:ncol(meta_01)) {
  meta_prob_obs[which(meta_prob_obs$name == colnames(meta_01)[i]),2] <- aggregate(meta_01[,i], list(meta_01[,1]), FUN=mean)[1,2]
  meta_prob_obs[which(meta_prob_obs$name == colnames(meta_01)[i]),3] <- aggregate(meta_01[,i], list(meta_01[,1]), FUN=mean)[2,2]
  meta_prob_obs[which(meta_prob_obs$name == colnames(meta_01)[i]),4] <- aggregate(meta_01[,i], list(meta_01[,1]), FUN=mean)[3,2]
  meta_prob_obs[which(meta_prob_obs$name == colnames(meta_01)[i]),5] <- aggregate(meta_01[,i], list(meta_01[,1]), FUN=mean)[4,2]
  meta_prob_obs[which(meta_prob_obs$name == colnames(meta_01)[i]),6] <- aggregate(meta_01[,i], list(meta_01[,1]), FUN=mean)[5,2]
}

micro_prob_obs <- data.frame("name" = colnames(micro_data)[-1:-8],
                             "t2" = NA,
                             "t4" = NA,
                             "t6" = NA,
                             "t9" = NA,
                             "t12" = NA)

micro_01 <- micro_data[,-1:-8] == 0
micro_01 <- cbind("time" = micro_data[,4], micro_01)
for (i in 2:ncol(micro_01)) {
  micro_prob_obs[which(micro_prob_obs$name == colnames(micro_01)[i]),2] <- aggregate(micro_01[,i], list(micro_01[,1]), FUN=mean)[1,2]
  micro_prob_obs[which(micro_prob_obs$name == colnames(micro_01)[i]),3] <- aggregate(micro_01[,i], list(micro_01[,1]), FUN=mean)[2,2]
  micro_prob_obs[which(micro_prob_obs$name == colnames(micro_01)[i]),4] <- aggregate(micro_01[,i], list(micro_01[,1]), FUN=mean)[3,2]
  micro_prob_obs[which(micro_prob_obs$name == colnames(micro_01)[i]),5] <- aggregate(micro_01[,i], list(micro_01[,1]), FUN=mean)[4,2]
  micro_prob_obs[which(micro_prob_obs$name == colnames(micro_01)[i]),6] <- aggregate(micro_01[,i], list(micro_01[,1]), FUN=mean)[5,2]
}

meta_prob_obs_long <- melt(meta_prob_obs)
micro_prob_obs_long <- melt(micro_prob_obs)
meta_prob_obs_long$variable <- rep(factor(c(2,4,6,9,12)), each = 2422)
micro_prob_obs_long$variable <- rep(factor(c(2,4,6,9,12)), each = 147)

#boxplot#
meta_quantile <- apply(meta_prob_obs[,-1], 2, FUN=function(x) quantile(x, probs = c(.1, .9)))
micro_quantile <- apply(micro_prob_obs[,-1], 2, FUN=function(x) quantile(x, probs = c(.1, .9)))

f1c <- ggplot(meta_prob_obs_long, aes(x=variable, y=value)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_segment(aes(x = 0.6, xend = 1.4, y = meta_quantile[1,1], yend = meta_quantile[1,1]), color = "dodgerblue1")+
  geom_segment(aes(x = 1.6, xend = 2.4, y = meta_quantile[1,2], yend = meta_quantile[1,2]), color = "dodgerblue1")+
  geom_segment(aes(x = 2.6, xend = 3.4, y = meta_quantile[1,3], yend = meta_quantile[1,3]), color = "dodgerblue1")+
  geom_segment(aes(x = 3.6, xend = 4.4, y = meta_quantile[1,4], yend = meta_quantile[1,4]), color = "dodgerblue1")+
  geom_segment(aes(x = 4.6, xend = 5.4, y = meta_quantile[1,5], yend = meta_quantile[1,5]), color = "dodgerblue1")+
  geom_segment(aes(x = 0.6, xend = 1.4, y = meta_quantile[2,1], yend = meta_quantile[2,1]), color = "deeppink1")+
  geom_segment(aes(x = 1.6, xend = 2.4, y = meta_quantile[2,2], yend = meta_quantile[2,2]), color = "deeppink1")+
  geom_segment(aes(x = 2.6, xend = 3.4, y = meta_quantile[2,3], yend = meta_quantile[2,3]), color = "deeppink1")+
  geom_segment(aes(x = 3.6, xend = 4.4, y = meta_quantile[2,4], yend = meta_quantile[2,4]), color = "deeppink1")+
  geom_segment(aes(x = 4.6, xend = 5.4, y = meta_quantile[2,5], yend = meta_quantile[2,5]), color = "deeppink1")+
  labs(title = "Metabolome", y="Proportion of zeros", x="Age (months)")

f1d <- ggplot(micro_prob_obs_long, aes(x=variable, y=value)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_segment(aes(x = 0.6, xend = 1.4, y = micro_quantile[1,1], yend = micro_quantile[1,1]), color = "dodgerblue1")+
  geom_segment(aes(x = 1.6, xend = 2.4, y = micro_quantile[1,2], yend = micro_quantile[1,2]), color = "dodgerblue1")+
  geom_segment(aes(x = 2.6, xend = 3.4, y = micro_quantile[1,3], yend = micro_quantile[1,3]), color = "dodgerblue1")+
  geom_segment(aes(x = 3.6, xend = 4.4, y = micro_quantile[1,4], yend = micro_quantile[1,4]), color = "dodgerblue1")+
  geom_segment(aes(x = 4.6, xend = 5.4, y = micro_quantile[1,5], yend = micro_quantile[1,5]), color = "dodgerblue1")+
  geom_segment(aes(x = 0.6, xend = 1.4, y = micro_quantile[2,1], yend = micro_quantile[2,1]), color = "deeppink1")+
  geom_segment(aes(x = 1.6, xend = 2.4, y = micro_quantile[2,2], yend = micro_quantile[2,2]), color = "deeppink1")+
  geom_segment(aes(x = 2.6, xend = 3.4, y = micro_quantile[2,3], yend = micro_quantile[2,3]), color = "deeppink1")+
  geom_segment(aes(x = 3.6, xend = 4.4, y = micro_quantile[2,4], yend = micro_quantile[2,4]), color = "deeppink1")+
  geom_segment(aes(x = 4.6, xend = 5.4, y = micro_quantile[2,5], yend = micro_quantile[2,5]), color = "deeppink1")+
  labs(title = "Microbiome", y="Proportion of zeros", x="Age (months)")
