#!/usr/bin/env Rscript
#SBATCH --job-name=micro_pred
#SBATCH --time=4:00:00
#SBATCH --mem=4G
#SBATCH --array=1-74
#SBATCH --output=../logs/%x_%A_%a
#SBATCH --constraint=[intel18|intel16]
# Usage:
#
#  sbatch 2_auc_micro.R

jobID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))
omic <- "micro"

library(lme4)
library(MASS)
library(dplyr)
library(pROC)
library(data.table)

load(paste0("/Data/", omic, "_QCed_data.RData"))
data <- micro_data

data_t0 <- data[which(data$timepoint == 2),c(-1:-8)]
tmp <- colMeans(data_t0)

ID <- data$subjectID
age <- data$age_mos
timepoint <- data$timepoint
sex <- as.factor(data$sex)
sample <- data$sampleID
race <- data$race
Y <- data[,-c(1,2,3,4,5,6,7,8)]
bmilk=ifelse(metadata[,'bmilk']=='Yes',1,0)
bmilk[is.na(bmilk)]=0
formula=ifelse(metadata[,'formula']=='Yes',1,0)
formula[is.na(formula)]=0
cmilk=ifelse(metadata[,'cmilk']=='Yes',1,0)
cmilk[is.na(cmilk)]=0
omilk=ifelse(metadata[,'omilk']=='Yes',1,0)
omilk[is.na(omilk)]=0

start <- 2*(jobID-1)+1
end <- ifelse(jobID==74, 147, 2*jobID)

repeats <- 20

TASKS1 <- matrix(nrow = 147, ncol = repeats+1)
TASKS1[,1] <- colnames(data)[9:155]
colnames(TASKS1) <- c("rowname", 1:repeats)
rownames(TASKS1) <- colnames(data)[9:155]
TASK1 <- TASKS1[start:end,]

TASKS2 <- matrix(nrow = 147, ncol = repeats+1)
TASKS2[,1] <- colnames(data)[9:155]
colnames(TASKS2) <- c("rowname", 1:repeats)
rownames(TASKS2) <- colnames(data)[9:155]
TASK2 <- TASKS2[start:end,]

for (i in 1:nrow(TASK1)) {
  for (r in 1:repeats) {
    tryCatch({
    y <- Y[,start-1+i]
    full_table <- data.frame("y" = y,
                               "sample" = sample,
                               "ID" = ID,
                               "age" = age,
                               "sex" = factor(sex),
                               "race" = factor(race),
                               "bmilk" = factor(bmilk),
                               "formula" = factor(formula),
                               "cmilk" = factor(cmilk),
                               "omilk" = factor(omilk))
    full_table$y <- as.numeric(full_table$y != 0)
    heads <- as.data.frame(full_table %>% group_by(ID) %>% arrange(desc(age)) %>% slice_head(n=1))
    tails <- as.data.frame(full_table %>% group_by(ID) %>% arrange(desc(age)) %>% slice_tail(n=1))
    remaining <- full_table[-which(full_table$sample %in% heads$sample | full_table$sample %in% tails$sample),]
    out_table <- as.data.frame(remaining %>% group_by(ID) %>% slice_sample(n=1))
    fit_table <- full_table[-which(full_table$sample %in% out_table$sample),]
    fm1 <- glm(y ~ age+sex+race+bmilk+formula+cmilk+omilk, data = fit_table, family = "binomial")
    fm2 <- glmer(y ~ age+sex+race+bmilk+formula+cmilk+omilk+(age|ID), data = fit_table, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
    TASK1[i,r+1] <- auc(out_table$y, predict(fm1, newdata = out_table, type = "response"))
    TASK2[i,r+1] <- auc(out_table$y, predict(fm2, newdata = out_table, type = "response"))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  fwrite(TASK1[i,], file='../output/micro_auc_test.csv', append=TRUE)
  fwrite(TASK2[i,], file='../output/micro_noran_auc_test.csv', append=TRUE)
  print(i)
}

quit(save='no')