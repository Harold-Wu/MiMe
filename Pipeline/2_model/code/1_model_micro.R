#!/usr/bin/env Rscript
#SBATCH --job-name=micro
#SBATCH --time=4:00:00
#SBATCH --mem=4G
#SBATCH --array=1-17
#SBATCH --output=../logs/%x_%A_%a
#SBATCH --constraint=[intel18|intel16]
# Usage:
#
#  sbatch 1_model_micro.R

jobID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))
omic <- "micro"

library(lme4)
library(MASS)
library(data.table)

load(paste0("/Data/", omic, "_filter_logit_data.RData"))
data <- micro_data

data_t0 <- data[which(data$timepoint == 2),c(-1:-8)]
tmp <- colMeans(data_t0)

ID <- data$subjectID
age <- data$age_mos
timepoint <- data$timepoint
sex <- as.factor(data$sex)
race <- data$race
Y <- data[,-c(1,2,3,4,5,6,7,8)]

start <- 5*(jobID-1)+1
end <- ifelse(jobID==17, 84, 5*jobID)

TASKS1 <- matrix(nrow = 84, ncol = length(seq(1,14,0.5))+1)
TASKS1[,1] <- colnames(data)[9:92]
colnames(TASKS1) <- c("rowname", seq(1,14,0.5))
rownames(TASKS1) <- colnames(data)[9:92]
TASK1 <- TASKS1[start:end,]

TASKS2 <- matrix(nrow = 84, ncol = length(unique(ID))+1)
TASKS2[,1] <- colnames(data)[9:92]
colnames(TASKS2) <- c("rowname", unique(data$subjectID))
rownames(TASKS2) <- colnames(data)[9:92]
TASK2 <- TASKS2[start:end,]

TASKS3 <- matrix(nrow = 84, ncol = 7)
TASKS3[,1] <- colnames(data)[9:92]
colnames(TASKS3) <- c("rowname",
                      "Int", "Int_se", "Int_p", "age", "age_se", "age_p")
rownames(TASKS3) <- colnames(data)[9:92]
TASK3 <- TASKS3[start:end,]


for (i in 1:nrow(TASK1)) {
  tryCatch({
    y <- Y[,start-1+i]
    fit_data <- data.frame("y" = y,
                           "ID" = ID,
                           "age" = age,
                           "sex" = factor(sex),
                           "race" = factor(race))
    fit_data$y <- as.numeric(fit_data$y != 0)
    fm2 <- glmer(y ~ age+sex+race+(age|ID), data = fit_data, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
    for(j in 1:27) {
      newdata <- data.frame("ID" = NA,
                            "age" = rep(seq(1,14,0.5)[j], times = length(unique(ID))),
                            "sex" = NA,
                            "race" = NA)
      newdata[,c(1,3,4)] <- rep(unique(fit_data[,c(2,4,5)]), each = 1)
      prob <- mean(predict(fm2, newdata = newdata, type = "response", re.form = NA))
      TASK1[i,j+1] <- prob
    }
    newdata_ind <- data.frame("ID" = NA,
                              "age" = rep(c(2,14), times = length(unique(ID))),
                              "sex" = NA,
                              "race" = NA)
    newdata_ind[,c(1,3,4)] <- unique(fit_data[,c(2,4,5)])[rep(seq_len(nrow(unique(fit_data[,c(2,4,5)]))), each = 2), ]
    ind_expected <- predict(fm2, newdata = newdata_ind, type = "response")
    TASK2[i,2:ncol(TASK2)] <- matrix(ind_expected, ncol=2, byrow = T)[,2]-matrix(ind_expected, ncol=2, byrow = T)[,1]
    TASK3[i,2:7] <- c(coef(summary(fm2))[1,c(1,2,4)], coef(summary(fm2))[2,c(1,2,4)])

    fwrite(TASK1[i,], file='../output/micro_pop_expected.csv', append=TRUE)
    fwrite(TASK2[i,], file='../output/micro_ind_expected_diff.csv', append=TRUE)
    fwrite(TASK3[i,], file='../output/micro_fixef.csv', append=TRUE)

    print(i)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

quit(save='no')