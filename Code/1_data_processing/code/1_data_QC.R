load("/Data/micro_data.RData")
load("/Data/meta_data.RData")

# At least 3 samples for each subject
micro_data <- micro_data[which(micro_data$subjectID %in% names(table(micro_data$subjectID))[which(table(micro_data$subjectID) >= 3)]),] #369*20374
meta_data <- meta_data[which(meta_data$subjectID %in% names(table(meta_data$subjectID))[which(table(meta_data$subjectID) >= 3)]),] #510*8342

# Keep molecules that have non-zero proportion larger than 0.1
micro_01 <- micro_data[,-1:-12] != 0
meta_01 <- meta_data[,-1:-12] != 0
micro_data <- micro_data[, c(1:12, 12 + which(colMeans(micro_01) > 0.1))]
meta_data <- meta_data[, c(1:12, 12 + which(colMeans(meta_01) > 0.1))]
micro_data <- micro_data[which(rowSums(micro_data[,-1:-12]) != 0),]
meta_data <- meta_data[which(rowSums(meta_data[,-1:-12]) != 0),]

meta_data[,3] <- as.integer(as.character(meta_data[,3]))
meta_data[,7] <- as.character(meta_data[,7])
meta_data[,8] <- as.character(meta_data[,8])

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


save(micro_data, file = "/Data/micro_QCed_data.RData")
save(meta_data, file = "/Data/meta_QCed_data.RData")