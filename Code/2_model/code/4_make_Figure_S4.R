# Figure S4
library(ggplot2)
library(reshape2)

load("../output/micro_pop_expected.RData")
load("../output/meta_pop_expected.RData")

load("Data/micro_filter_logit_data.RData")
load("Data/meta_filter_logit_data.RData")

PoP_meta <- data.frame("name" = rownames(meta_prob),
             "t2_real" = NA,
             "t2_pred" = meta_prob[,3],
             "t4_real" = NA,
             "t4_pred" = meta_prob[,7],
             "t6_real" = NA,
             "t6_pred" = meta_prob[,11],
             "t9_real" = NA,
             "t9_pred" = meta_prob[,17],
             "t12_real" = NA,
             "t12_pred" = meta_prob[,23])
meta_01 <- meta_data[,-1:-8] != 0
meta_01 <- cbind("time" = meta_data[,4], meta_01)
for (i in 2:ncol(meta_01)) {
  PoP_meta[which(PoP_meta$name == colnames(meta_01)[i]),2] <- aggregate(meta_01[,i], list(meta_01[,1]), FUN=mean)[1,2]
  PoP_meta[which(PoP_meta$name == colnames(meta_01)[i]),4] <- aggregate(meta_01[,i], list(meta_01[,1]), FUN=mean)[2,2]
  PoP_meta[which(PoP_meta$name == colnames(meta_01)[i]),6] <- aggregate(meta_01[,i], list(meta_01[,1]), FUN=mean)[3,2]
  PoP_meta[which(PoP_meta$name == colnames(meta_01)[i]),8] <- aggregate(meta_01[,i], list(meta_01[,1]), FUN=mean)[4,2]
  PoP_meta[which(PoP_meta$name == colnames(meta_01)[i]),10] <- aggregate(meta_01[,i], list(meta_01[,1]), FUN=mean)[5,2]
}

PoP_micro <- data.frame("name" = rownames(micro_prob),
              "t2_real" = NA,
              "t2_pred" = micro_prob[,3],
              "t4_real" = NA,
              "t4_pred" = micro_prob[,7],
              "t6_real" = NA,
              "t6_pred" = micro_prob[,11],
              "t9_real" = NA,
              "t9_pred" = micro_prob[,17],
              "t12_real" = NA,
              "t12_pred" = micro_prob[,23])
micro_01 <- micro_data[,-1:-8] != 0
micro_01 <- cbind("time" = micro_data[,4], micro_01)
for (i in 2:ncol(micro_01)) {
  PoP_micro[which(PoP_micro$name == colnames(micro_01)[i]),2] <- aggregate(micro_01[,i], list(micro_01[,1]), FUN=mean)[1,2]
  PoP_micro[which(PoP_micro$name == colnames(micro_01)[i]),4] <- aggregate(micro_01[,i], list(micro_01[,1]), FUN=mean)[2,2]
  PoP_micro[which(PoP_micro$name == colnames(micro_01)[i]),6] <- aggregate(micro_01[,i], list(micro_01[,1]), FUN=mean)[3,2]
  PoP_micro[which(PoP_micro$name == colnames(micro_01)[i]),8] <- aggregate(micro_01[,i], list(micro_01[,1]), FUN=mean)[4,2]
  PoP_micro[which(PoP_micro$name == colnames(micro_01)[i]),10] <- aggregate(micro_01[,i], list(micro_01[,1]), FUN=mean)[5,2]
}


tmp1 <- melt(PoP_meta[,c(1,2,4,6,8,10)])
tmp2 <- melt(PoP_meta[,c(1,3,5,7,9,11)])
meta_long <- cbind(tmp1, tmp2)[,c(1,2,3,6)]
colnames(meta_long) <- c("name", "time", "observed", "predicted")
meta_long$time <- as.factor(rep(c(2,4,6,9,12), each = 1115))
meta_long[,3:4] <- 1-meta_long[,3:4]

s2a <- ggplot(meta_long, aes(x = observed, y = predicted, color = time))+
  geom_point(size = 0.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 0.3, color = "darkgray")+
  labs(title = "Metabolome", x = "Observed proportion of zeros", y = "Expected proportion of zeros", color = "Time (months)")

tmp1 <- melt(PoP_micro[,c(1,2,4,6,8,10)])
tmp2 <- melt(PoP_micro[,c(1,3,5,7,9,11)])
micro_long <- cbind(tmp1, tmp2)[,c(1,2,3,6)]
colnames(micro_long) <- c("name", "time", "observed", "predicted")
micro_long$time <- as.factor(rep(c(2,4,6,9,12), each = 84))
micro_long[,3:4] <- 1-micro_long[,3:4]

s2b <- ggplot(micro_long, aes(x = observed, y = predicted, color = time))+
  geom_point(size = 0.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 0.3, color = "darkgray")+
  labs(title = "Microbiome", x = "Observed proportion of zeros", y = "Expected proportion of zeros", color = "Time (months)")

