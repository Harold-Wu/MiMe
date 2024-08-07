#!/usr/bin/env Rscript
#SBATCH --job-name=micro
#SBATCH --time=4:00:00
#SBATCH --mem=4G
#SBATCH --array=1-18
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
bmilk=ifelse(metadata[,'bmilk']=='Yes',1,0)
bmilk[is.na(bmilk)]=0
formula=ifelse(metadata[,'formula']=='Yes',1,0)
formula[is.na(formula)]=0
cmilk=ifelse(metadata[,'cmilk']=='Yes',1,0)
cmilk[is.na(cmilk)]=0
omilk=ifelse(metadata[,'omilk']=='Yes',1,0)
omilk[is.na(omilk)]=0

start <- 5*(jobID-1)+1
end <- ifelse(jobID==18, 86, 5*jobID)

TASKS1 <- matrix(nrow = 86, ncol = length(seq(1,14,0.5))+1)
TASKS1[,1] <- colnames(data)[9:94]
colnames(TASKS1) <- c("rowname", seq(1,14,0.5))
rownames(TASKS1) <- colnames(data)[9:94]
TASK1 <- TASKS1[start:end,]

TASKS1_2 <- matrix(nrow = 86, ncol = length(seq(1,14,0.5))+1)
TASKS1_2[,1] <- colnames(data)[9:94]
colnames(TASKS1_2) <- c("rowname", seq(1,14,0.5))
rownames(TASKS1_2) <- colnames(data)[9:94]
TASK1_2 <- TASKS1_2[start:end,]

TASKS2 <- matrix(nrow = 86, ncol = length(unique(ID))+1)
TASKS2[,1] <- colnames(data)[9:94]
colnames(TASKS2) <- c("rowname", unique(data$subjectID))
rownames(TASKS2) <- colnames(data)[9:94]
TASK2 <- TASKS2[start:end,]

TASKS3 <- matrix(nrow = 86, ncol = 7)
TASKS3[,1] <- colnames(data)[9:94]
colnames(TASKS3) <- c("rowname",
                      "Int", "Int_se", "Int_p", "age", "age_se", "age_p")
rownames(TASKS3) <- colnames(data)[9:94]
TASK3 <- TASKS3[start:end,]

TASKS4 <- as.data.frame(matrix(nrow = 86, ncol = 41))
TASKS4[,1] <- colnames(data)[9:94]
colnames(TASKS4) <- c("rowname",
                      "Int", "Int_se", "Int_Z", "Int_p", "age", "age_se", "age_Z", "age_p",
                      "sex2", "sex2_se", "sex2_Z", "sex2_p", "race2", "race2_se", "race2_Z", "race2_p",
                      "race3", "race3_se", "race3_Z", "race3_p", "race4", "race4_se", "race4_Z", "race4_p",
                      "bmilk1", "bmilk1_se", "bmilk1_Z", "bmilk1_p", "formula1", "formula1_se", "formula1_Z", "formula1_p",
                      "cmilk1", "cmilk1_se", "cmilk1_Z", "cmilk1_p", "omilk1", "omilk1_se", "omilk1_Z", "omilk1_p")
rownames(TASKS4) <- colnames(data)[9:94]
TASK4 <- TASKS4[start:end,]


for (i in 1:nrow(TASK1)) {
  tryCatch({
    y <- Y[,start-1+i]
    fit_data <- data.frame("y" = y,
                           "ID" = ID,
                           "age" = age,
                           "sex" = factor(sex),
                           "race" = factor(race),
                               "bmilk" = factor(bmilk),
                               "formula" = factor(formula),
                               "cmilk" = factor(cmilk),
                               "omilk" = factor(omilk))
    fit_data$y <- as.numeric(fit_data$y != 0)
    fm2 <- glmer(y ~ age+sex+race+bmilk+formula+cmilk+omilk+(age|ID), data = fit_data, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
    newdata <- data.frame("age" = seq(1,14,0.5),
                      "sex" = rep(as.factor(1), 27),
                      "race" = rep(as.factor(1), 27),
                      "bmilk" = rep(as.factor(0), 27),
                      "formula" = rep(as.factor(0), 27),
                      "cmilk" = rep(as.factor(0), 27),
                      "omilk" = rep(as.factor(0), 27))
    TASK1[i,2:28] <- predict(fm2, newdata = newdata, type = "response", re.form = NA)
    TASK1_2[i,2:28] <- predictSE.mer(fm2, newdata, se.fit = TRUE, type = "response", level = 0, print.matrix = FALSE)$se.fit
    
    newdata_ind <- data.frame("ID" = NA,
                              "age" = rep(c(2,14), each = length(unique(ID))),
                              "sex" = factor(NA, levels = c(1,2)),
                              "race" = factor(NA, levels = c(1,2,3,4)),
                              "bmilk" = factor(NA, levels = c(0,1)),
                               "formula" = factor(NA, levels = c(0,1)),
                               "cmilk" = factor(NA, levels = c(0,1)),
                               "omilk" = factor(NA, levels = c(0,1)))
    heads <- as.data.frame(fit_data %>% group_by(ID) %>% arrange(desc(age)) %>% slice_head(n=1))
    tails <- as.data.frame(fit_data %>% group_by(ID) %>% arrange(desc(age)) %>% slice_tail(n=1))
    newdata_ind[1:length(unique(ID)),c(1,3:8)] <- tails[,c(2,4:9)]
    newdata_ind[(length(unique(ID))+1):(2*length(unique(ID))),c(1,3:8)] <- heads[,c(2,4:9)]
    ind_expected <- predict(fm2, newdata = newdata_ind, type = "response")
    TASK2[i,2:ncol(TASK2)] <- ind_expected[(length(unique(ID))+1):(2*length(unique(ID)))]-ind_expected[1:length(unique(ID))]
    TASK3[i,2:7] <- c(coef(summary(fm2))[1,c(1,2,4)], coef(summary(fm2))[2,c(1,2,4)])
    TASK4[i,2:41] <- c(coef(summary(fm2))[1,],coef(summary(fm2))[2,],coef(summary(fm2))[3,],coef(summary(fm2))[4,],coef(summary(fm2))[5,],coef(summary(fm2))[6,],
                       coef(summary(fm2))[7,],coef(summary(fm2))[8,],coef(summary(fm2))[9,],coef(summary(fm2))[10,])

    addALine(x=TASK1[i,],fname='../output/micro_pop_expected.csv')
    addALine(x=TASK1_2[i,],fname='../output/micro_pop_expected_SE.csv')
    addALine(x=TASK2[i,],fname='../output/micro_ind_expected_diff.csv')
    addALine(x=TASK3[i,],fname='../output/micro_fixef.csv')
    addALine(x=TASK4[i,],fname='../output/micro_fixef_full.csv')

    print(i)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

quit(save='no')