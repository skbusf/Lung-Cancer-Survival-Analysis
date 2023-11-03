library(survival)

setwd("C:/Users/batch_2kjxgc7/Desktop/USF/Courses/SDM/Assignments/Survival Models/")
df = read.table("LungCancer.txt")

colnames(df) = c("treatment_type", "cell_type", "survival_days","status", "karnofsky_score", "months_from_diagnosis", "age", "prior_chemo")

View(df)

df$treatment_type = ifelse(df$treatment_type == 1, 0, 1)


df$cell_type  = ifelse(df$cell_type == 1 | df$cell_type == 3 | df$cell_type == 4 , "non_small_cell", "small_cell")
                       # ifelse(df$cell_type == 2 , "small cell",
                       #        ifelse(df$cell_type == 3, "adeno", "large")))
colSums(is.na(df))
# df$squamous  = ifelse(df$cell_type == 1, "yes", "no" )
# 
# df$smallcell = ifelse(df$cell_type == 2 , "yes", "no")
# df$adeno = ifelse(df$cell_type == 3, "yes", "no")
# 
# df$large = ifelse(df$cell_type==4, "yes", "no")

df$prior_chemo = ifelse(df$prior_chemo == 10, "Yes", "No")
str(df)
attach(df)

hist(survival_days)

# install.packages("survival")
y = Surv(survival_days,status)

km1 = survfit(y~1)
summary(km1)
plot(km1, xlab="Time", ylab="Survival Probability")

km2 = survfit(y~ treatment_type)
summary(km2)
plot(km2, xlab = "Time", ylab = "Survival Probability")

# Cox proportional hazard model - coefficients and hazard rates
cox <- coxph(y ~ treatment_type + age + cell_type + karnofsky_score + prior_chemo + months_from_diagnosis)
summary(cox)

# Exponential, Weibull, and log-logistic parametric model coefficients
exp <- survreg(y ~ treatment_type + age + cell_type + karnofsky_score  + prior_chemo + months_from_diagnosis, dist="exponential")
summary(exp)

weibull <- survreg(y ~ treatment_type + age  + cell_type + karnofsky_score + prior_chemo + months_from_diagnosis, dist="weibull")
summary(weibull)

loglogistic <- survreg(y ~ treatment_type + age  + cell_type + karnofsky_score + prior_chemo + months_from_diagnosis, dist="loglogistic")
summary(loglogistic)

library(stargazer)
stargazer(cox, exp, weibull, type="text")

