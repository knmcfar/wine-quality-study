library(knitr)
library(tidyverse)
library(tableone)
library(sandwich)
library(pROC)
library(ggplot2)

#read in the data --------------------------------------------------------------
wine <- read.csv("winequality-red.csv", sep = ";")

#primary model using stepwise selection-----------------------------------------
wine1 <- wine %>% select(-quality)

#test-train split
set.seed(544)
#split the data into training/test sets with 70% and 30% observations
sample <- sample(1:1599, 1119, replace = FALSE)
training <- wine1[sample, ]
test <- wine1[-sample, ]

#fit full and intercept only binary logistic regression models
log_full <- glm(qual_bin ~ ., data = training, family = "binomial")
int_only <- glm(qual_bin ~ 1, data = training, family = "binomial")

#fit forward stepwise regression models using AIC
forward_aic <- step(int_only, 
                    direction = "forward", 
                    scope = formula(log_full),
                    trace = 0)

#sensitivity model--------------------------------------------------------------

#fit forward stepwise regression models using BIC
forward_bic <- step(int_only, 
                    direction = "forward", 
                    scope = formula(log_full),
                    trace = 0, 
                    k = log(nrow(training)))

#error rate---------------------------------------------------------------------

#fit binary logistic regression on the AIC and BIC training (subset) models
aic_mod_train <- glm(formula = qual_bin ~ alcohol + volatile.acidity + sulphates +
                       fixed.acidity + chlorides + total.sulfur.dioxide + density +
                       residual.sugar + free.sulfur.dioxide,
                     family = "binomial",
                     data = training)

bic_mod_train <- glm(formula = qual_bin ~ alcohol + volatile.acidity + sulphates +
                       fixed.acidity + chlorides, 
                     family = "binomial", 
                     data = training)

#test error for the AIC model
obs_test0 <- test[, 12]
pred.prob_test0 <- predict(forward_aic, newdata = test[, -12], type = "response")
pred.class_test0 <- ifelse(pred.prob_test0 > 0.5, 1, 0)
error_aic <- mean((obs_test0 - pred.class_test0)^2)

#test error for BIC model
pred.prob_test1 <- predict(forward_bic, newdata = test[, -12], type = "response")
pred.class_test1 <- ifelse(pred.prob_test1 > 0.5, 1, 0)
error_bic <- mean((obs_test0 - pred.class_test1)^2)

#tables and figures-------------------------------------------------------------

#table 1: descriptive statistics
wine$qual_bin <- ifelse(wine$quality < 7, 0, 1)
wine$quality <- factor(wine$qual_bin, labels = c("Poor", "Good"))

tbl1 <- CreateTableOne(data = wine, 
                       vars = colnames(wine)[-c(12,13)], 
                       strata = "quality",
                       test = FALSE, 
                       includeNA = FALSE, 
                       addOverall = TRUE)
k <- print(tbl1$ContTable)

row.names(k) <- c("n", 
                  "Fixed acidity (g(tartaric acid)/dm^3)", 
                  "Volatile acidity (g(acetic acid)/dm^3)", 
                  "Citric acid (g/dm^3)", 
                  "Residual sugar (g/dm^3)", 
                  "Chlorides (g(sodium chloride)/dm^3)", 
                  "Free sulfur dioxide (mg/dm^3)", 
                  "Total sulfur dioxide (mg/dm^3)", 
                  "Density (g/cm^3)", 
                  "pH",
                  "Sulphates (g(potassium sulphate)/dm^3)", 
                  "Alcohol (vol.%)")


kable(k[-1, ], 
      col.names = c("Overall (n=1599)", 
                             "Poor (n=1382)", 
                             "Good (n=217)"), 
      caption = "Total counts of wines and descriptive statistics (mean and standard 
      deviation) of variables stratified by wine quality and overall")

#table 2: AIC model results

#calculate robust SEs for regression coefficients
robust_se <- sqrt(diag(vcovHC(forward_aic, type = "HC0"))) 
#calculate 95% CIs based on robust SEs
robust_ci_lower <- exp(coef(forward_aic) + qnorm(0.025)*robust_se) 
robust_ci_upper <- exp(coef(forward_aic) + qnorm(0.975)*robust_se)
#create and print table 2
results1 <- data.frame(estimate = coef(forward_aic),
                       exp_estimate = exp(coef(forward_aic)),
                       robust_SE = robust_se,
                       exp_CI_lower = robust_ci_lower,
                       exp_CI_upper = robust_ci_upper,
                       p_val = summary(forward_aic)$coefficients[, 4])
results1 <- results1 %>% round(3)
results1[2,6] <- "<0.0001"
results1[c(2, 4:5),6] <- "<0.0001"
results1.1 <- format(results1[-1, ], scientific = FALSE)
results1 <- rbind(results1[1, ], results1.1)
results1[1,2] <- "9.84e+129"
results1[1,4] <- "1.12e+20"
results1[1,5] <- "8.62e+239"

rownames(results1) <- c("Intercept", 
                        "Alcohol", 
                        "Volatile Acidity", 
                        "Sulphates",
                        "Fixed Acidity", 
                        "Chlorides", 
                        "Total Sulfur Dioxide",
                        "Density", 
                        "Residual Sugar", 
                        "Free Sulfur Dioxide")

kable(results1, 
      col.names = c("Estimate", 
                              "Exponentiated Estimate",
                              "Robust SE", 
                              "95% Upper", 
                              "95% Lower", 
                              "P Value"), 
      caption = "Foward stepwise selection model estimates with AIC", 
      align = 'r')

#table 3: sensitivity analysis results

#calculate robust SEs for regression coefficients
robust_se <- sqrt(diag(vcovHC(forward_bic, type = "HC0"))) 
#calculate 95% CIs based on robust SEs
robust_ci_lower <- exp(coef(forward_bic) + qnorm(0.025)*robust_se)
robust_ci_upper <- exp(coef(forward_bic) + qnorm(0.975)*robust_se)
#create and print table 3
results2 <- data.frame(estimate = coef(forward_bic),
                       exp_estimate = exp(coef(forward_bic)),
                       robust_SE = robust_se,
                       exp_CI_lower = robust_ci_lower,
                       exp_CI_upper = robust_ci_upper,
                       p_val = summary(forward_bic)$coefficients[, 4])
results2 <- results2 %>% round(3)

results2[1:5,6] <- "<0.0001"
rownames(results2) <- c("Intercept",
                        "Alcohol", 
                        "Volatile Acidity",
                        "Sulphates",
                        "Fixed Acidity",
                        "Chlorides")
kable(results2, 
      col.names = c("Estimate",
                    "Exponentiated Estimate",
                    "Robust SE", 
                    "95% Upper", 
                    "95% Lower", 
                    "P Value"), 
      caption = "Foward stepwise selection model estimates with BIC", 
      align = 'r')


#figure 1: AIC/BIC values for both models

x <- c(1:10)
y <- c(1:6)
group <- c(rep("Forward Stepwise using AIC", 10))
group2 <- c(rep("Forward Stepwise using BIC", 6))
aic_mod <- cbind(x, forward_aic$anova[, 6], group)
bic_mod <- cbind(y, forward_bic$anova[, 6], group2)
all_mods <- rbind(aic_mod, bic_mod)
all_mods <- as.data.frame(all_mods)

all_mods %>%
  ggplot(aes(as.numeric(x), all_mods[, 2], col = as.factor(group))) +
  geom_point() + 
  geom_line(aes(group = as.factor(group))) + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  scale_color_manual(values = c("Forward Stepwise using AIC" = "#F8766D", 
                              "Forward Stepwise using BIC" = "#619CFF")) +
  xlab("Iterations") + 
  ylab("AIC/BIC Value") + 
  labs(col = "", title = "AIC/BIC values using forward stepwise selection models") +
  theme(legend.position = "bottom")

#figure 2: roc curve using BIC

roc0 <- roc(obs_test0, pred.prob_test0)
roc1 <- roc(obs_test0, pred.prob_test1)
roc.list <- roc(obs_test0 ~ pred.prob_test0 + pred.prob_test1)
ggroc(roc.list, legacy.axes = TRUE) +
  scale_colour_manual(values = c("red", "blue")) +
  geom_abline() +
  theme(legend.position = "bottom") +
  labs(x = "False Positive Rate", y = "True Positive Rate", color = "", 
       title = "ROC curves") +
  annotate("text", 
           x = 0.75, 
           y = c(0.25, 0.20), 
           label = c("AUC: 0.8532", "AUC: 0.8414"),
           size = 5, 
           color = c("#F8766D", "#619CFF")) +
  scale_color_manual(values = c("#F8766D", "#619CFF"),
                     labels = c("Forward Stepwise using AIC", 
                                "Forward Stepwise using BIC"))
