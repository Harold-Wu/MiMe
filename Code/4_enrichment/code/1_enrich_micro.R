#!/usr/bin/env Rscript
#SBATCH --job-name=enrich1
#SBATCH --time=4:00:00
#SBATCH --mem=4G
#SBATCH --array=1-7
#SBATCH --output=../logs/%x_%A_%a
#SBATCH --constraint=[intel18|intel16]
# Usage:
#
#  sbatch 1_enrich_micro.R

load("/Data/micro_taxa.RData")
f1='../output/meta_ind_expected_diff.RData'
f2='../output/micro_ind_expected_diff.RData'
load(f1)
load(f2)
load("../output/pairwise_cor_signi.RData")

jobID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))

taxa <- micro_taxa[which(micro_taxa$name %in% colnames(micro_prob_ind)),]

test_col <- c("kindom", "phylum", "class", "order", "family", "genus", "species")[jobID]
tax_col_num <- which(colnames(taxa) == test_col)
groups <- unique(taxa[, tax_col_num][!is.na(taxa[, tax_col_num])])

out_table <- data.frame("group" = groups,
						"pval" = NA,
						"adj_pval" = NA)

test_set <- signi_index
test_set$test_group <- NA
for(i in 1:nrow(test_set)) {
	test_set$test_group[i] <- taxa[which(taxa$name == test_set$micro[i]), tax_col_num]
}

for (g in 1:length(groups)) {
	group <- groups[g]

	test_table <- matrix(nrow=2, ncol=2, NA)
	test_table[1,1] <- sum(test_set$test_group == group, na.rm = T)
	test_table[1,2] <- nrow(test_set) - test_table[1,1]
	test_table[2,1] <- ncol(meta_prob_ind)*sum(taxa[,tax_col_num] == group, na.rm = T) - test_table[1,1]
	test_table[2,2] <- ncol(meta_prob_ind)*(nrow(taxa)-sum(taxa[,tax_col_num] == group, na.rm = T)) - test_table[1,2]

	out_table[g,2] <- fisher.test(test_table, alternative="greater")$p.value
	print(paste0(g, " out of ", length(groups)))
}
out_table[,3] <- p.adjust(out_table[,2], "fdr")

write.csv(out_table, paste0("../output/enrich_test/micro_", test_col, ".csv"))
