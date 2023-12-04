# Figure 4 - a
library(ggplot2)
library(reshape2)
library(ggraph)
library(igraph)
library(circlize)

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

d1 <- rbind(data.frame(from = "tmp", to = unique(signi_final$family)), data.frame(from = "tmp", to = unique(signi_final$NPC_level3)))
d2 <- rbind(unique(data.frame(from = signi_final$family, to = signi_final$micro)), unique(data.frame(from = signi_final$NPC_level3, to = signi_final$meta)))
hierarchy <- rbind(d1, d2)
vertices <- data.frame(name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to)))) 
mygraph <- graph_from_data_frame(hierarchy, vertices=vertices)

connect <- data.frame(from = signi_final$micro, to = signi_final$meta, value = signi_final$boot_cor, group = signi_final$family)
from <- match(connect$from, vertices$name)
to <- match(connect$to, vertices$name)

cols <- colorRamp2(c(-1,0,1), c("blue", "white", "red"))
library(ComplexHeatmap)
lgd_links = Legend(at = c(-1, -0.5, 0, 0.5, 1), col_fun = cols, 
             title_position = "topleft", title = "Correlation",
             labels_gp = gpar(fontsize = 18),
             title_gp = gpar(fontsize = 20, fontface = "bold"))
tmp <- data.frame('molecule' = c(signi_final$micro, signi_final$meta),
          'group' = c(signi_final$family, signi_final$NPC_level3))
tmp <- unique(tmp)
group <- tmp[,2]
names(group) <- tmp[,1]

chordDiagram(connect, group = group, col = cols, annotationTrack = c("grid"),
       order = c(unique(signi_final[order(signi_final$family),1]), unique(signi_final[order(signi_final$NPC_level3),2])),
       preAllocateTracks = list(
        track.height = mm_h(4),
        track.margin = c(mm_h(4), 0)))
highlight.sector(unique(signi_final[which(signi_final$family == "Clostridiaceae"),1]), track.index = 1, col = "plum1", 
    text = "A", cex = 1.5, text.col = "black", niceFacing = TRUE)
highlight.sector(unique(signi_final[which(signi_final$family == "Erysipelotrichaceae"),1]), track.index = 1, col = "plum1", 
    text = "B", cex = 1.5, text.col = "black", niceFacing = TRUE)
highlight.sector(unique(signi_final[which(signi_final$family == "Lachnospiraceae"),1]), track.index = 1, col = "plum1", 
    text = "C", cex = 1.5, text.col = "black", niceFacing = TRUE)
highlight.sector(unique(signi_final[which(signi_final$family == "Pasteurellaceae"),1]), track.index = 1, col = "plum1", 
    text = "D", cex = 1.5, text.col = "black", niceFacing = TRUE)
highlight.sector(unique(signi_final[which(signi_final$family == "Prevotellaceae"),1]), track.index = 1, col = "plum1", 
    text = "E", cex = 1.5, text.col = "black", niceFacing = TRUE)
highlight.sector(unique(signi_final[which(signi_final$family == "Rikenellaceae"),1]), track.index = 1, col = "plum1", 
    text = "F", cex = 1.5, text.col = "black", niceFacing = TRUE)
highlight.sector(unique(signi_final[which(signi_final$family == "Streptococcaceae"),1]), track.index = 1, col = "plum1", 
    text = "G", cex = 1.5, text.col = "black", niceFacing = TRUE)

highlight.sector(unique(signi_final[which(signi_final$NPC_level3 == "2-pyrone derivatives"),2]), track.index = 1, col = "lightblue1", 
    text = "H", cex = 1.5, text.col = "black", niceFacing = TRUE)
highlight.sector(unique(signi_final[which(signi_final$NPC_level3 == "Cholestane steroids"),2]), track.index = 1, col = "lightblue1", 
    text = "I", cex = 1.5, text.col = "black", niceFacing = TRUE)
highlight.sector(unique(signi_final[which(signi_final$NPC_level3 == "Polyamines"),2]), track.index = 1, col = "lightblue1", 
    text = "J", cex = 1.5, text.col = "black", niceFacing = TRUE)
highlight.sector(unique(signi_final[which(signi_final$NPC_level3 == "Ursane and Taraxastane triterpenoids"),2]), track.index = 1, col = "lightblue1", 
    text = "K", cex = 1.5, text.col = "black", niceFacing = TRUE)
draw(lgd_links, x = unit(1, "npc") - unit(2, "mm"), y = unit(4, "mm"), just = c("right", "bottom"))

