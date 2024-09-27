# Figure S5
library(ggplot2)
library(reshape2)
library(lme4)

load(paste0("/Data/micro_filter_logit_data.RData"))
load(paste0("/Data/meta_filter_logit_data.RData"))
load("../output/micro_ind_expected_diff.RData")
load("../output/meta_ind_expected_diff.RData")

micro_var=apply(micro_prob_ind, 2, var)
tmp=quantile(micro_var, c(.8, .5, .2))
list_micro=c(NA, NA, NA, NA)
index_micro=c(NA, NA, NA, NA)
for (i in 1:length(tmp)) {
  list_micro[i]=names(micro_var)[which.min(abs(micro_var-tmp[i]))]
  index_micro[i]=which(colnames(micro_data) == list_micro[i])-12
}

get_prob <- function(dd = micro_data, i = 45) {
  data <- dd

  ID <- data$subjectID
  age <- data$age_mos
  timepoint <- data$timepoint
  sex <- as.factor(data$sex)
  Y <- data[,-c(1:12)]
  y <- Y[,i]

  fit_data <- data.frame("y" = y,
                         "ID" = ID,
                         "age" = age,
                         "sex" = sex)
  fit_data$y <- as.numeric(fit_data$y != 0)
  fm2 <- glmer(y ~ age+sex+(age|ID), data = fit_data, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
  pop_prob <- numeric(5)
  ind_prob <- matrix(nrow = length(unique(ID)), ncol = 5)
  colnames(ind_prob) <- c(2,4,6,9,12)
  rownames(ind_prob) <- unique(ID)

  for(j in 1:5) {
    newdata <- data.frame("ID" = NA,
                          "age" = rep(c(2,4,6,9,12)[j], times = length(unique(ID))),
                          "sex" = NA)
    newdata[,c(1,3)] <- rep(unique(cbind(ID, sex)), each = 1)
    newdata$sex <- as.factor(newdata$sex)
    pop_prob[j] <- mean(predict(fm2, newdata = newdata, type = "response", re.form = NA))
    ind_prob[,j] <- predict(fm2, newdata = newdata, type = "response")
  }
  ind_prob <- t(ind_prob)
  prob_both <- cbind("pop" = pop_prob[1:5], ind_prob[1:5,])
  prob_both
}


tmp <- get_prob(i=index_micro[1])
prob_long <- melt(tmp)
prob_long$color <- NA
for (i in 1:ncol(tmp)) {
  prob_long$color[(5*(i-1)+1):(5*i)] <- ifelse(prob_long$value[5*i] < prob_long$value[5*(i-1)+1], "Increase", "Decrease")
}
prob_long$color[1:5] <- "Population"
prob_long$value <- 1-prob_long$value

s5a <- ggplot(prob_long, aes(x = Var1, y = value, group = Var2, color = color))+
  geom_point(size = 0.5)+
  geom_line(aes(size = color))+
  scale_size_manual(values = c(Population = 3, Increase = 0.5, Decrease = 0.5),
                    labels = c(Population = "Population", Increase = "Individual Increase", Decrease = "Individual Decrease"))+
  scale_color_manual(values = c(Population = "#F8766D", Increase = "#E69F00", Decrease = "#56B4E9"),
                     labels = c(Population = "Population", Increase = "Individual Increase", Decrease = "Individual Decrease"))+
  labs(title = list_micro[1], subtitle = 'High Heterogeneity', x = "Age (months)", y = "Proportion of zeros", color = "", size = "")+

tmp <- get_prob(i=index_micro[2])
prob_long <- melt(tmp)
prob_long$color <- NA
for (i in 1:ncol(tmp)) {
  prob_long$color[(5*(i-1)+1):(5*i)] <- ifelse(prob_long$value[5*i] < prob_long$value[5*(i-1)+1], "Increase", "Decrease")
}
prob_long$color[1:5] <- "Population"
prob_long$value <- 1-prob_long$value

s5b <- ggplot(prob_long, aes(x = Var1, y = value, group = Var2, color = color))+
  geom_point(size = 0.5)+
  geom_line(aes(size = color))+
  scale_size_manual(values = c(Population = 3, Increase = 0.5, Decrease = 0.5),
                    labels = c(Population = "Population", Increase = "Individual Increase", Decrease = "Individual Decrease"))+
  scale_color_manual(values = c(Population = "#F8766D", Increase = "#E69F00", Decrease = "#56B4E9"),
                     labels = c(Population = "Population", Increase = "Individual Increase", Decrease = "Individual Decrease"))+
  labs(title = list_micro[2], subtitle = 'Moderate Heterogeneity', x = "Age (months)", y = "Proportion of zeros", color = "", size = "")+

tmp <- get_prob(i=index_micro[3])
prob_long <- melt(tmp)
prob_long$color <- NA
for (i in 1:ncol(tmp)) {
  prob_long$color[(5*(i-1)+1):(5*i)] <- ifelse(prob_long$value[5*i] < prob_long$value[5*(i-1)+1], "Increase", "Decrease")
}
prob_long$color[1:5] <- "Population"
prob_long$value <- 1-prob_long$value

s5c <- ggplot(prob_long, aes(x = Var1, y = value, group = Var2, color = color))+
  geom_point(size = 0.5)+
  geom_line(aes(size = color))+
  scale_size_manual(values = c(Population = 3, Increase = 0.5, Decrease = 0.5),
                    labels = c(Population = "Population", Increase = "Individual Increase", Decrease = "Individual Decrease"))+
  scale_color_manual(values = c(Population = "#F8766D", Increase = "#E69F00", Decrease = "#56B4E9"),
                     labels = c(Population = "Population", Increase = "Individual Increase", Decrease = "Individual Decrease"))+
  labs(title = list_micro[3], subtitle = 'Low Heterogeneity', x = "Age (months)", y = "Proportion of zeros", color = "", size = "")+
