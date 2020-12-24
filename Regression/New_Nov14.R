######Logistic regression_A_112___________

library(dplyr)

A_112_Nonzerodays <- A_112_Nonzerodays %>%
  mutate(Avg_CO2 = (CO2_LR+CO2_BR1+CO2_BR2+CO2_BR3)/4)

A_112_Nonzerodays <- A_112_Nonzerodays %>%
  mutate(Avg_RH = (RH_LR+RH_BR1+RH_BR2+RH_BR3)/4)


A_112_Nonzerodays <- A_112_Nonzerodays %>%
  mutate(Tot_LP = (LP1+LP2+LP3+LP4+LP5+LP6+LP7+LP8+LP9+LP10+LP11+LP12))


Summer_112_Log_Reg <- Summer_112

Summer_112_Log_Reg$Occ_prob <- Summer_112_Log_Reg$M_tot[which(Summer_112_Log_Reg$M_tot>1)] <- 1


Summer_112_Log_Reg <- Summer_112_Log_Reg[-c(14)]

str(Summer_112_Log_Reg)

Summer_112_Log_Reg$M_tot <- as.factor(Summer_112_Log_Reg$M_tot)

#Data partition

set.seed(1234)

ind_new <- sample(2, nrow(Summer_112_Log_Reg), replace = T,  prob = c(0.75,0.25))
train_new <- Summer_112_Log_Reg[ind_new==1,]
test_new <- Summer_112_Log_Reg[ind_new==2,]


#Logistic regression model

new_glm_model <- glm(M_tot~CO2_avg+Hour, data = train_new, family = 'binomial')
summary(new_glm_model)

#Prediction

p1_new <- predict(new_glm_model, train_new, type = 'response')
head(p1_new)

summary(p1_new)
library(InformationValue)

optimalCutoff(train_new$M_tot, p1_new)[1]

#Misclassification error - train data

pred1_new <- ifelse(p1_new>0.38, 1,0)
tab1_new <- table(predicted = pred1_new, Actual = train_new$M_tot)
1-sum(diag(tab1_new))/sum(tab1_new)


#Misclassification error - test data
p2_new <- predict(new_glm_model, test_new, type = 'response')
head(p2_new)

summary(p2_new)

optimalCutoff(test_new$M_tot, p2_new)[1]

pred2_new <- ifelse(p2_new>0.5, 1,0)
tab2_new <- table(predicted = pred2_new, Actual = test_new$M_tot)
1-sum(diag(tab2_new))/sum(tab2_new)

plotROC(test_new$M_tot, p2_new)

#Goodness of fit test

with(new_glm_model, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = F))