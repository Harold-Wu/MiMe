#!/usr/bin/env Rscript
#SBATCH --job-name=un_boot
#SBATCH --time=4:00:00
#SBATCH --mem=4G
#SBATCH --array=1-188
#SBATCH --output=../logs/%x_%A_%a
#SBATCH --constraint=[intel18|intel16]
# Usage:
#
#  sbatch 1_bootstrap_unbiased_cor.R

library(data.table)

f1='../output/meta_ind_expected_diff.RData'
f2='../output/micro_ind_expected_diff.RData'
load(f1)
load(f2)

load("/Data/meta_filter_logit_data.RData")
load("/Data/micro_filter_logit_data.RData")

common_ID <- intersect(unique(meta_data$subjectID), unique(micro_data$subjectID))

meta_prob_ind <- meta_prob_ind[which(rownames(meta_prob_ind) %in% common_ID),]
micro_prob_ind <- micro_prob_ind[which(rownames(micro_prob_ind) %in% common_ID),]

all(rownames(micro_prob_ind)==rownames(meta_prob_ind))

jobID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))

jobsPerJob=500

firstJob=(jobID-1)*jobsPerJob+1
lastJob=min(firstJob+jobsPerJob-1,ncol(meta_prob_ind)*ncol(micro_prob_ind)) #93660

nBoot=100

TASKS=expand.grid(cols_meta=1:ncol(meta_prob_ind),cols_micro=1:ncol(micro_prob_ind),meta=NA,micro=NA,unbiased_cor=NA,boot_cor=NA,boot_se=NA,pval=NA)
TASKS=TASKS[firstJob:lastJob,]

if(nrow(TASKS)>0){


for(i in 1:nrow(TASKS)){

        #timeIn=proc.time()

          x=meta_prob_ind[,TASKS$cols_meta[i]]
          y=micro_prob_ind[,TASKS$cols_micro[i]]
          n=length(x)
          tmp.r=cor(x,y)
          TASKS$unbiased_cor[i]=tmp.r*(1+((1-tmp.r^2)/(2*(n-3))))
          COR=rep(NA,nBoot)
          SE=rep(NA,nBoot)

          for(j in 1:nBoot){

            tmp=sample(1:n,replace=TRUE,size=n)
            r=cor(x[tmp],y[tmp])
            COR[j]=r*(1+((1-r^2)/(2*(n-3))))
            SE[j]=sqrt((1-COR[j]^2)/(n-2))
          }

          TASKS$boot_cor[i]=mean(COR)
          TASKS$boot_se[i]=mean(SE)
          t = TASKS$boot_cor[i]/TASKS$boot_se[i]
          TASKS$pval[i] = 2 * min(pt(t, n-2),pt(t, n-2, lower.tail=FALSE))
          TASKS$meta[i] <- colnames(meta_prob_ind)[TASKS$cols_meta[i]]
          TASKS$micro[i] <- colnames(micro_prob_ind)[TASKS$cols_micro[i]]

          fwrite(TASKS[i,], file='../output/pairwise_cor.csv', append=TRUE)
        #timeOut=proc.time()
        print(i)
}
}

quit(save='no')