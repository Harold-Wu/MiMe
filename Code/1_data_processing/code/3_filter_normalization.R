load("../output/micro_auc_test.RData")
load("../output/meta_auc_test.RData")

# Filter out those with AUC < 0.65
micros <- rownames(micro)[which(rowMeans(micro, na.rm = T) >= 0.65)]
metas <- rownames(meta)[which(rowMeans(meta, na.rm = T) >= 0.65)]

load("/mnt/research/quantgen/projects/harold/InfantDev2/data/micro_QCed_data.RData")
load("/mnt/research/quantgen/projects/harold/InfantDev2/data/meta_QCed_data.RData")
micro_data <- cbind(micro_data[,1:12], micro_data[,micros])
meta_data <- cbind(meta_data[,1:12], meta_data[,metas])

# Normalization by sum
tmp <- micro_data[,-1:-12]
for (i in 1:nrow(tmp)) {
	tmp[i,] <- tmp[i,]/sum(tmp[i,])
	print(i)
}
micro_data <- cbind(micro_data[,1:12], tmp)

tmp <- meta_data[,-1:-12]
for (i in 1:nrow(tmp)) {
	tmp[i,] <- tmp[i,]/sum(tmp[i,])
	print(i)
}
meta_data <- cbind(meta_data[,1:12], tmp)

save(micro_data, file = "/Data/micro_filter_logit_data.RData")
save(meta_data, file = "/Data/meta_filter_logit_data.RData")
