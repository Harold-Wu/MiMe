# Figure S3
library(ggplot2)
library(reshape2)
library(lme4)

load(paste0("/mnt/research/quantgen/projects/harold/InfantDev2/data/micro_filter_logit_data.RData"))
load(paste0("/mnt/research/quantgen/projects/harold/InfantDev2/data/meta_filter_logit_data.RData"))

get_prob <- function(dd = micro_data, i = 45) {
  data <- dd

  ID <- data$subjectID
  age <- data$age_mos
  timepoint <- data$timepoint
  sex <- as.factor(data$sex)
  Y <- data[,-c(1,2,3,4,5,6,7,8)]
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

tmp <- get_prob(i=17) #
prob_long <- melt(tmp)
prob_long$color <- NA
for (i in 1:ncol(tmp)) {
  prob_long$color[(5*(i-1)+1):(5*i)] <- ifelse(prob_long$value[5*i] < prob_long$value[5*(i-1)+1], "Increase", "Decrease")
}
prob_long$color[1:5] <- "Population"
prob_long$value <- 1-prob_long$value

f3_3 <- ggplot(prob_long, aes(x = Var1, y = value, group = Var2, color = color))+
  geom_point(size = 0.5)+
  geom_line(aes(size = color))+
  scale_size_manual(values = c(Population = 3, Increase = 0.5, Decrease = 0.5),
                    labels = c(Population = "Population", Increase = "Individual Increase", Decrease = "Individual Decrease"))+
  scale_color_manual(values = c(Population = "#619CFF", Increase = "#00BA38", Decrease = "#F8766D"),
                     labels = c(Population = "Population", Increase = "Individual Increase", Decrease = "Individual Decrease"))+
  labs(title = "ASV_195", x = "Age (months)", y = "Proportion of zeros", color = "", size = "")


tmp <- get_prob(i=20) #
prob_long <- melt(tmp)
prob_long$color <- NA
for (i in 1:ncol(tmp)) {
  prob_long$color[(5*(i-1)+1):(5*i)] <- ifelse(prob_long$value[5*i] < prob_long$value[5*(i-1)+1], "Increase", "Decrease")
}
prob_long$color[1:5] <- "Population"
prob_long$value <- 1-prob_long$value

f3_4 <- ggplot(prob_long, aes(x = Var1, y = value, group = Var2, color = color))+
  geom_point(size = 0.5)+
  geom_line(aes(size = color))+
  scale_size_manual(values = c(Population = 3, Increase = 0.5, Decrease = 0.5),
                    labels = c(Population = "Population", Increase = "Individual Increase", Decrease = "Individual Decrease"))+
  scale_color_manual(values = c(Population = "#619CFF", Increase = "#00BA38", Decrease = "#F8766D"),
                     labels = c(Population = "Population", Increase = "Individual Increase", Decrease = "Individual Decrease"))+
  labs(title = "ASV_145", x = "Age (months)", y = "Proportion of zeros", color = "", size = "")


tmp <- get_prob(i=61) #
prob_long <- melt(tmp)
prob_long$color <- NA
for (i in 1:ncol(tmp)) {
  prob_long$color[(5*(i-1)+1):(5*i)] <- ifelse(prob_long$value[5*i] < prob_long$value[5*(i-1)+1], "Increase", "Decrease")
}
prob_long$color[1:5] <- "Population"
prob_long$value <- 1-prob_long$value

f3_5 <- ggplot(prob_long, aes(x = Var1, y = value, group = Var2, color = color))+
  geom_point(size = 0.5)+
  geom_line(aes(size = color))+
  scale_size_manual(values = c(Population = 3, Increase = 0.5, Decrease = 0.5),
                    labels = c(Population = "Population", Increase = "Individual Increase", Decrease = "Individual Decrease"))+
  scale_color_manual(values = c(Population = "#619CFF", Increase = "#00BA38", Decrease = "#F8766D"),
                     labels = c(Population = "Population", Increase = "Individual Increase", Decrease = "Individual Decrease"))+
  labs(title = "ASV_119", x = "Age (months)", y = "Proportion of zeros", color = "", size = "")


tmp <- get_prob(meta_data, i=689)
prob_long <- melt(tmp)
prob_long$color <- NA
for (i in 1:ncol(tmp)) {
  prob_long$color[(5*(i-1)+1):(5*i)] <- ifelse(prob_long$value[5*i] < prob_long$value[5*(i-1)+1], "Increase", "Decrease")
}
prob_long$color[1:5] <- "Population"
prob_long$value <- 1-prob_long$value

f3_6 <- ggplot(prob_long, aes(x = Var1, y = value, group = Var2, color = color))+
  geom_point(size = 0.5)+
  geom_line(aes(size = color))+
  scale_size_manual(values = c(Population = 3, Increase = 0.5, Decrease = 0.5),
                    labels = c(Population = "Population", Increase = "Individual Increase", Decrease = "Individual Decrease"))+
  scale_color_manual(values = c(Population = "#619CFF", Increase = "#00BA38", Decrease = "#F8766D"),
                     labels = c(Population = "Population", Increase = "Individual Increase", Decrease = "Individual Decrease"))+
  labs(title = "Meta_10532", x = "Age (months)", y = "Proportion of zeros", color = "", size = "")
  