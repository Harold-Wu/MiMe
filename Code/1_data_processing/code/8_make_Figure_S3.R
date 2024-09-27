# SupFigure 3
library(ggplot2)
library(ggpubr)

load("../output/micro_noran_auc_test.RData")
load("../output/micro_noran_auc_test.RData")
load("../output/micro_auc_test.RData")
load("../output/micro_auc_test.RData")

micro <- rowMeans(micro, na.rm = T)
micro_glm <- rowMeans(micro_glm, na.rm = T)
meta <- rowMeans(meta, na.rm = T)
meta_glm <- rowMeans(meta_glm, na.rm = T)

micro_glm <- micro_glm[names(micro)]
meta_glm <- meta_glm[names(meta)]

metas <- data.frame("glm" = meta_glm, "glmer" = meta)
metas$color <- ifelse(metas$glmer > 0.65, "keep", "remove")
micros <- data.frame("glm" = micro_glm, "glmer" = micro)
micros$color <- ifelse(micros$glmer > 0.65, "keep", "remove")

s3a <- ggplot(metas, aes(x = glm, y = glmer, color = color))+
  geom_point(size = 0.5)+
  scale_y_continuous(limits = c(0.485,0.95))+
  scale_x_continuous(limits = c(0.485,0.95))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 0.3, color = "darkgray")+
  geom_hline(yintercept = 0.65, linetype = "dashed", size = 0.3, color = "red")+
  scale_color_manual(values = c(keep = "#00BA38", remove = "darkgray"))+
  labs(title = "Metabolome", x = "AUC - standard logistic regression", y = "AUC - mixed logistic regression", color = NA)+
  theme(
  plot.title = element_text(hjust = 0.5, size = 25),
  plot.subtitle = element_text(hjust = 0.5, size = 15),
  axis.title.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text.x = element_text(size = 18),
  axis.text.y = element_text(size = 18),
  legend.position="none")

s3b <- ggplot(micros, aes(x = glm, y = glmer, color = color))+
  geom_point(size = 0.5)+
  scale_y_continuous(limits = c(0.485,0.95))+
  scale_x_continuous(limits = c(0.485,0.95))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 0.3, color = "darkgray")+
  geom_hline(yintercept = 0.65, linetype = "dashed", size = 0.3, color = "red")+
  scale_color_manual(values = c(keep = "#00BA38", remove = "darkgray"))+
  labs(title = "Microbiome", x = "AUC - standard logistic regression", y = "AUC - mixed logistic regression", color = NA)+
  theme(
  plot.title = element_text(hjust = 0.5, size = 25),
  plot.subtitle = element_text(hjust = 0.5, size = 15),
  axis.title.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text.x = element_text(size = 18),
  axis.text.y = element_text(size = 18),
  legend.position="none")
