#Loading relevant packages
library(reshape2)
library(TH.data)
library(survival)
library(survminer)
library(cluster)
library(purrr)
library(survminer)

#Example of GBSG2 data set
selection <- head(GBSG2)
selection

# Create new column filled with default colour
GBSG2$Colour = "black"
GBSG2$observations = 1:686

# Set new column values to appropriate colours
GBSG2$Colour[GBSG2$cens == 0]="red"

# Plot all points at once, using newly generated colours
attach(GBSG2)
plot(observations, time, xlab="Observations", ylab= "Survival Time", pch=19, col = Colour)
segments(observations, 0, observations, time)

#Fit Weibull model to breast cancer data
wb <- survreg(Surv(time, cens) ~ 1, dist='weibull', data = GBSG2)

#Retrieve survival curve from model probabilities
surv <- seq(0.9, 0.1, by = -0.1)

#Get time, in days, for each probability
t <- predict(wb, type = "quantile", p = 1 - surv, newdata = data.frame(1))

#Create data frame with the information
surv_wb <- data.frame(time = t, surv = surv)
surv_wb
surv_wb <- data.frame(time = t, surv = surv, upper = NA, lower = NA, std.err = NA)

#Graph the survival curve estimated by the Weibull model
ggsurvplot_df(fit = surv_wb, size = 0.5, surv.geom = geom_line,  font.x = c(5, "bold"),
              font.y = c(5, "bold"), font.legend = c(5))

#Estimate the survival curves for the censored data using the Kaplan-Meier estimator
km <- survfit(Surv(time, cens) ~ 1, data = GBSG2)

#Kaplan-Meier estimate of overall survival function using shading for 95% confidence interval
ggsurvplot(km, size = 0.5, xlab = "Surivival time in days", font.x = c(12, "bold"), font.y = c(12, "bold"), risk.table = TRUE)

#Kaplan-Meier estimate of overall survival function using alpha blending
km.fit <- survfit(Surv(time,cens)~1,data=GBSG2)


res <- summary(km.fit)
cols <- lapply(c(2:6, 8:11) , function(x) res[x])
tbl <- do.call(data.frame, cols)


ggplot(data = tbl) +
  geom_step(aes(x = time, y = surv, alpha = n.risk)) +
  ylim(0, 1) + xlim(0, 2456 * 1.05) +
  labs(title = 'Estimated Survival Probability', x = 'Follow Up Time', y = 'S(t)') + theme_bw() +
  theme(text = element_text(face = 'bold', size = 14), legend.position = 'none')

#Kaplan-Meier estimate of overall cumulative hazard function
ggsurvplot(km, size = 0.5, fun = "cumhaz", xlab = "Surivival time in days", font.x = c(12, "bold"), font.y = c(12, "bold"))

#The overall distribution of age is displayed to decide on what cutoff should be chosen for the age variable
hist(GBSG2$age, xlab = "Age", main = paste("Histogram of patients age")) 

#Bi-modal distribution of age in histogram, suggests a cutoff of 50 years
GBSG2 <- GBSG2 %>% mutate(age_group = ifelse(age >=50, "old", "young"))
GBSG2$age_group <- factor(GBSG2$age_group)

#Kaplan_Meier estimate with regards the covariate horTh
km_horTh <- survfit(Surv(time, cens) ~ horTh, data = GBSG2)
ggsurvplot(km_horTh, size = 0.5, legend.title = "Kaplan-Meier estimate for horTh;",
           xlab = "Surivival time in days", font.x = c(13, "bold"), font.y = c(13, "bold"), font.legend = c(13))

#Kaplan_Meier estimate with regards the covariate menostat
km_menostat <- survfit(Surv(time, cens) ~ menostat, data = GBSG2)
ggsurvplot(km_menostat, size = 0.5, legend.title = "Kaplan-Meier estimate for menostat",
           xlab = "Surivival time in days", font.x = c(13, "bold"), font.y = c(13, "bold"), font.legend = c(13))

#Kaplan_Meier estimate with regards the covariate age group
km_age_group <- survfit(Surv(time, cens) ~ age_group, data = GBSG2)
ggsurvplot(km_age_group, size = 0.5, legend.title = "Kaplan-Meier estimate for age group",
           xlab = "Surivival time in days", font.x = c(13, "bold"), font.y = c(13, "bold"), font.legend = c(13))

#Kaplan_Meier estimate with regards the covariate tgrade
km_tgrade <- survfit(Surv(time, cens) ~ tgrade, data = GBSG2)
ggsurvplot(km_tgrade, size = 0.5, legend.title = "Kaplan-Meier estimate for tgrade",
           xlab = "Surivival time in days", font.x = c(13, "bold"), font.y = c(13, "bold"), font.legend = c(13))

#Test the difference between two or more survival curves
survtest_horTh <- survdiff(Surv(time, cens) ~ horTh , data = GBSG2, rho = 0)

#survdiff function to find log-rank test for the covariates
survtest_menostat <- survdiff(Surv(time, cens) ~ menostat , data = GBSG2, rho = 0)
survtest_age_group <- survdiff(Surv(time, cens) ~ age_group , data = GBSG2, rho = 0)
survtest_tgrade <- survdiff(Surv(time, cens) ~ tgrade , data = GBSG2, rho = 0)

#Results of the log-rank test
print(survtest_horTh)
print(survtest_menostat)
print(survtest_age_group)
print(survtest_tgrade)

#Results of log-rank test into dataframe
log_rank_test <- matrix(c(8.6, 1, 0.003, 0.3, 1, 0.6, 1.7, 1, 0.2, 21.1, 2, 0.00003),ncol=3,byrow=TRUE)
colnames(log_rank_test) <- c("Chi-square", "df", "p-value")
rownames(log_rank_test) <- c("horTh", "menostat","age group", "tgrade")
log_rank_test <- as.data.frame(log_rank_test)

log_rank_test

#Test the influence of all the covariates on the risk of death from breast cancer
cox.fit1 <- coxph(Surv(time, cens) ~ horTh + age + menostat + tsize + pnodes + progrec + estrec, data = GBSG2)
cox.fit1_d <- matrix(c(0.69, "(0.54-0.89)", 0.0046, 0.99, "(0.97-1.01)", 0.2587, 1.32, "(0.92-1.88)",
                       0.1287, 1.01, "(1-1.02)", 0.0342, 1.05,"(1.04-1.07)", .66e-11, 1, "(1-1)",
                       8.50e-06 , 1, "(1-1)", 0.7008), ncol=3, byrow=TRUE)
colnames(cox.fit1_d) <- c("HR","95% CI for", "p-value")
rownames(cox.fit1_d) <- c("horTh-yes", "age", "menostat-Post", "tsize","pnodes", "progrec", "estrec")
cox.fit1_d <- as.data.frame(cox.fit1_d)
cox.fit1_d

#Insignificant covariates, age, menostat and estrec are removed
cox.fit3 <- coxph(Surv(time, cens) ~ horTh + tsize + pnodes + progrec, data = GBSG2)
summary(cox.fit3)

cox.fit3_d <- matrix(c(0.71, "(0.56-0.91)", 0.0062, 1.01, "(1-1.02)", 0.04207, 1.05, "(1.04-1.07)",
                       1.42e-11, 1, "(1-1)", 3.73e-06), ncol=3, byrow=TRUE)
colnames(cox.fit3_d) <- c("HR", "95% CI", "p-value")
rownames(cox.fit3_d) <- c("horTh-yes", "tsize","pnodes", "progrec")
cox.fit3_d <- as.data.frame(cox.fit3_d)
cox.fit3_d

#Estimated hazard ratios
cox.fit <- coxph(Surv(time, cens) ~ horTh + tsize + pnodes + progrec, data = GBSG2)
ggforest(cox.fit, data = GBSG2)

cxmod <- coxph(Surv(time, cens) ~ horTh + pnodes + tsize + progrec, data = GBSG2)

#Decide on covariate combinations 
newdat <- expand.grid(
  horTh = levels(GBSG2$horTh),
  tsize = mean(GBSG2$tsize),
  progrec = mean(GBSG2$progrec),
  pnodes = quantile(GBSG2$pnodes, probs = c(0.25, 0.5, 0.75))
)

rownames(newdat) <- letters[1:6]

#Compute survival curves
cxsf <- survfit(cxmod, data = GBSG2, newdata = newdat)

#Create data.frame with survival curve information
surv_cxmod0 <- surv_summary(cxsf)

surv_cxmod <- cbind(surv_cxmod0,
                    newdat[as.character(surv_cxmod0$strata), ])

ggsurvplot_df(surv_cxmod, linetype = "horTh", color = "pnodes", legend.title = NULL, censor = FALSE, font.x = c(13, "bold"),
              font.y = c(13, "bold"), font.legend = c(13))

#Check for a violation of proportional hazards
cox.fit1 <- coxph(Surv(time, cens) ~ horTh + age + menostat + tsize + pnodes + progrec + estrec, data = GBSG2)
ftest <- cox.zph(cox.fit1)
ftest

#Results of schoenfield test into dataframe
schoenfield_test <- matrix(c(0.8565, 0.0549, 0.9801, 0.5558, 0.3936, 0.0493, 0.3649, 0.0134), ncol=1, byrow=TRUE)
colnames(schoenfield_test) <- c("p-value")
rownames(schoenfield_test) <- c("horTh-yes", "age", "menostat-Post", "tsize","pnodes", "progrec", "estrec", "GLOBAL")
schoenfield_test <- as.data.frame(schoenfield_test)
schoenfield_test

#Graph of Schoenfield residuals against the log transformed time
ggcoxzph(ftest, size = 0.5, font.x = c(13, "bold"), font.y = c(13, "bold"), font.legend = c(13), font.main = 4)
