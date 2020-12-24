# Logistics Regression
glm.fit <- glm(M_tot ~ P_tot + LP_tot, data = Cluster_1_OD_RP, family = 'binomial')

fit <- bayesglm(M_tot ~ P_tot + LP_tot, data = Cluster_1_OD_RP, family = 'binomial')

summary(fit)
glm.probs <- predict(glm.fit,type = "response")
glm.probs[1:200]
plot(glm.probs)
View(glm.fit)


plot(cluster_1_OD$M_tot, cluster_1_OD$LP_tot)
cor(cluster_1_OD$M_tot, cluster_1_OD$P_tot)

r <- lm(M_tot~LP_tot, data = cluster_1_OD)
abline(r, col='red')


plot(cluster_1_OD$M_tot, r$fitted.values)



m <- lm(M_tot~P_tot+LP_tot+Month+Hour+season, data = cluster_1_OD)
plot(predict(m), cluster_1_OD$M_tot)
abline(a=0, b=1)
summary(m)


m1 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+CO2_avg+RH_avg, data = Cluster_1_mean_profile_type_1)
summary(m1)
plot(predict(m1), Cluster_1_mean_profile_type_1$M_tot_wd)
abline(a=0, b=1)

mape((m1$fitted.values), Cluster_1_mean_profile_type_1$M_tot_wd)



m2 <- lm(M_tot_wd~CO2_avg, data = Cluster_1_mean_profile_type_1)
summary(m2)
plot(predict(m2), Cluster_1_mean_profile_type_1$M_tot_wd)
abline(a=0, b=1)

mape((m2$fitted.values), Cluster_1_mean_profile_type_1$M_tot_wd)



#logit <- glm(M_tot_wd ~ CO2_avg, data = Cluster_1_mean_profile_type_1, family = 'binomial')


library(dplyr)

#M_Cluster_3_mean_profile_hourly_analysis

Daily_data <- Type_1_original_data_With_LP_CO2_Rh %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_tot), CO2_avg = mean(CO2), RH_avg = mean(RH))

Daily_data <- Daily_data[-c(8)]



#Cluster_3_mean_profile_type_1[c(2,3,4)] <- round(Cluster_3_mean_profile_type_1[c(2,3,4)], digits = 0)


Daily_data <- Daily_data[order(Daily_data$Day),]


Daily_data[c(3,4,5,6,7)] <- round(Daily_data[c(3,4,5,6,7)], digits = 0)

#plot(x=Cluster_3_mean_profile_type_1$Hour, y=Cluster_3_mean_profile_type_1$M_tot_wd, type='l',xaxt="n", xlab='Hour', ylab="Average movements detected", main='Cluster_3')
#axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), las=2)


#######Linear regression

pairs(Daily_data[c(2,4:8)], col=Daily_data$Movements)

pairs(Cluster_1_mean_profile_type_1[2:6])

##Multiple linear regression model

#lm1 <- lm(Movements~Plugload+Lightingload, data = Daily_data)
lm2 <- lm(Movements~Lightingload, data = Daily_data)
lm1 <- lm(Movements~Plugload, data = Daily_data)
#lm4 <- lm(Movements~Lightingload+Plugload+CO2+RH+Hour, Daily_data)
lm3 <- lm(Movements~CO2, Daily_data)
#lm6 <- lm(Movements~RH, Daily_data)

#clus_1_lm <- lm(M_tot_wd~LP_tot_wd+P_tot_wd, data = Cluster_1_mean_profile_type_1)
#clus_1_lm2 <- lm(M_tot_wd~LP_tot_wd, data = Cluster_1_mean_profile_type_1)
#clus_1_lm3 <- lm(M_tot_wd~P_tot_wd, data = Cluster_1_mean_profile_type_1)



#clus_2_lm <- lm(M_tot_wd~LP_tot_wd+P_tot_wd, data = Cluster_2_mean_profile_type_1)
#clus_2_lm2 <- lm(M_tot_wd~LP_tot_wd, data = Cluster_2_mean_profile_type_1)
#clus_2_lm3 <- lm(M_tot_wd~P_tot_wd, data = Cluster_2_mean_profile_type_1)

summary(lm3)


plot(predict(lm3), Daily_data$Movements)
abline(a=0, b=1)


install.packages('Metrics')

library(Metrics)


mape((lm3$fitted.values), Daily_data$Movements)


#############Daily_data
abline(v=c(24,48,72,96,120, 144, 168), lty=2)
par(mfrow=c(3,1))

plot(Daily_data$Movements, type='l', col='green', ylim=c(0,50), lwd=3, xlab='Hour', ylab='Hourly movements detected', xaxt='n', main='Model 1')
axis(1,c(1, 24, 48, 72, 96, 120, 144, 168), las=2)
legend(
  "topleft", 
  col = c('Green', 'Red'),
  legend = c("Actual values", "Predicted values"), lty = 2, bty = 'n', cex = 1, lwd=3)
lines(lm1$fitted.values, type='l', col='red', lty=2, lwd=3)
lines(lm2$fitted.values, type='l', col='brown', lty=2, lwd=3)
lines(lm3$fitted.values, type='l', col='black', lty=2, lwd=3)
lines(lm4$fitted.values, type='l', col='blue', lty=2, lwd=3)
lines(lm5$fitted.values, type='l', col='orange', lty=2, lwd=3)

Daily_data$Id = seq(1,120,1)
cowplot::plot_grid(a,b,c,d,labels = 'Auto')

library(ggplot2)

ggplot(Daily_data, aes(x=Id, y=M_tot_wd))+geom_line()


#############Cluster_1_Data

plot(Cluster_1_mean_profile_type_1$M_tot_wd, type='l')
lines(clus_1_lm2$fitted.values,type='l', col='red')
lines(clus_1_lm3$fitted.values,type='l', col='blue')



#############Cluster_2_Data

plot(Cluster_2_mean_profile_type_1$M_tot_wd, type='l')
lines(clus_2_lm2$fitted.values,type='l', col='red')
lines(clus_2_lm3$fitted.values,type='l', col='blue')



m3 <- lm(M_tot_wd~LP_tot_wd, data = Daily_data)
summary(m3)
plot(predict(m3), Daily_data$M_tot_wd)
abline(a=0, b=1)

cor(Daily_data$M_tot_wd, Daily_data$P_tot_wd)

mape((m3$fitted.values), Daily_data$M_tot_wd)


mape((m1$fitted.values), Daily_data$M_tot_wd)

rmse((m1$fitted.values), Daily_data$M_tot_wd)



#Logistic regression 


cluster_03_logit = cluster_3_OD

cluster_03_logit$M_tot <- as.factor(cluster_03_logit$M_tot)


set.seed(123)


ind <- sample(2, nrow(cluster_03_logit), replace = T, prob = c(0.8,0.2))

train <- cluster_03_logit[ind==1,]
test <- cluster_03_logit[ind==2,]

mymodel <- glm(M_tot~LP_tot+P_tot, data = train, family = 'binomial')

#dev.off()
summary(mymodel)

plot(cluster_03_logit$M_tot, type='l')


#prediction

p1<-  predict(mymodel, train, type='response')

head(train)

head(p1)

summary(p1)


### Misclassification error - train data

pred1 <- ifelse(p1>0.56, 1, 0)

tab1 <- table(Predicted = pred1, Actual = train$M_tot)

tab1


train$m_tot_predicted <- pred1


### Misclassification error - test data
p2<-  predict(mymodel, test, type='response')

pred2 <- ifelse(p2>0.5, 1, 0)


tab2 <- table(Predicted = pred2, Actual = test$M_tot)

logit <- glm(M_tot ~ CO2, data = cluster_03_logit, family = 'binomial')
summary(logit)


pairs(Daily_data[3:7], col=Daily_data$LP_tot_wd)

plot(Daily_data[3:7])



Func.Season <- function(month){
  
  Winter <- c("1", "2","12")
  
  Spring <- c("3", "4", "5")
  
  Summer <- c("6","7","8")
  
  Autumn <- c("9","10","11")
  
  season_division <- c("Winter","Spring", "Summer", "Autumn")
  
  x <- vector()
  
  
  
  for(i in 1:length(month)){
    
    if(month[i] %in% Winter){
      
      x[i] = season_division[1]
      
    }else if(month[i] %in% Spring){
      
      x[i] = season_division[2]
      
    }else if(month[i] %in% Summer){
      
      x[i] = season_division[3]
      
    }else{
      
      x[i] = season_division[4]
      
    }
    
  }
  
  
  
  return(x)
  
}



################112 Linear regression################

library(dplyr)

A_112_Nonzerodays <- A_112_Nonzerodays %>%
  mutate(LP_avg = LP1+LP2+LP3+LP4+LP5+LP6+LP7+LP8+LP9+LP10+LP11+LP12)

A_112_Nonzerodays <- A_112_Nonzerodays %>%
  mutate(CO2_avg = (CO2_LR+CO2_BR1+CO2_BR2+CO2_BR3)/4)

A_112_Nonzerodays <- A_112_Nonzerodays %>%
  mutate(RH_avg = (RH_LR+RH_BR1+RH_BR2+RH_BR3)/4)

A_112_Nonzerodays <- A_112_Nonzerodays %>%
  mutate(Temp_avg = (temp_LR+temp_BR1+temp_BR2+temp_BR3)/4)

A_112_Nonzerodays <- A_112_Nonzerodays %>%
  mutate(Window = W1+W2+W3+W4+W5+W6+W7)

library(lubridate)
A_112_Nonzerodays$Month <- month(A_112_Nonzerodays$Day_index)

A_112_Nonzerodays$Season <- Func.Season(A_112_Nonzerodays$Month)



Summer_112$Window1 <- Summer_112$Window[which(Summer_112$Window>1)] <- 1

Summer_112 <- Summer_112[-c(14)]


######Summer_112____########

Summer_112 <- filter(Daily_Data_112, Season=='Summer')

plot(Summer_mean$M_tot_wd, type='l', col='Black', xaxt='n', lwd=3, xlab='Hour', ylab='Hourly movements detected', main='Model 2, using lighting energy consumption for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
axis(1,c(1, 24, 48, 72, 96, 120, 144, 168), cex.axis=1.5)
lines(smmr3_112$fitted.values, type='l', col='green', lty=2, lwd=3)




Summer_mean <- Summer_112 %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_avg), CO2_avg = mean(CO2_avg), RH_avg = mean(RH_avg), Temp_Avg = mean(Temp_avg), Window_avg = mean(Window))

Summer_mean[c(3:9)] <- round(Summer_mean[c(3:9)], digits = 2)

Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
             "Friday", "Saturday", "Sunday")

Summer_mean$Day <- factor(Summer_mean$Day, levels = Daylabs)

Summer_mean <- Summer_mean[order(Summer_mean$Day),] 



smmr1_112 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+Hour, data = Summer_mean)
smmr2_112 <- lm(M_tot_wd~LP_tot_wd+Hour, data = Summer_mean)
smmr3_112 <- lm(M_tot_wd~P_tot_wd+Hour, data = Summer_mean)
smmr4_112 <- lm(M_tot_wd~CO2_avg+Hour+Window_avg, data = Summer_mean)
smmr5_112 <- lm(M_tot_wd~Temp_Avg+Hour, data = Summer_mean)
smmr6_112 <- lm(M_tot_wd~RH_avg+Hour, data = Summer_mean)
smmr7_112 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+Hour+CO2_avg+Temp_Avg+RH_avg, data = Summer_mean)


summary(smmr2_112)
mape((smmr1_112$fitted.values), Summer_mean$M_tot_wd)

plot(predict(smmr1_112), Summer_112$M_tot, main='Model 1', xlab='Predicted values', ylab = 'Actual values')


#########Winter_112_-#############

Winter_112 <- filter(Daily_Data_112, Season=='Winter')

Winter_112$Window1 <- Winter_112$Window[which(Winter_112$Window>1)] <- 1

Winter_112 <- Winter_112[-c(14)]

wtr1_112 <- lm(M_tot~LP_avg+P_tot, data = Winter_112)
wtr2_112 <- lm(M_tot~LP_avg, data = Winter_112)
wtr3_112 <- lm(M_tot~P_tot, data = Winter_112)
wtr4_112 <- lm(M_tot~CO2_avg+Window, data = Winter_112)

summary(wtr4_112)



plot(Winter_mean$M_tot_wd, type='l', col='Black', lwd=3, xlab='Hour', ylab='Hourly movements detected', main='Model 2, using lighting energy consumption for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
lines(wtr2_112$fitted.values, type='l', col='green', lty=2, lwd=3)


Winter_mean <- Winter_112 %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_avg), CO2_avg = mean(CO2_avg), RH_avg = mean(RH_avg), Temp_Avg = mean(Temp_avg), Window_avg = mean(Window))

Winter_mean[c(3:9)] <- round(Winter_mean[c(3:9)], digits = 2)

Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
             "Friday", "Saturday", "Sunday")

Winter_mean$Day <- factor(Winter_mean$Day, levels = Daylabs)

Winter_mean <- Winter_mean[order(Winter_mean$Day),] 

wtr1_112 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+Hour, data = Winter_mean)
wtr2_112 <- lm(M_tot_wd~LP_tot_wd+Hour, data = Winter_mean)
wtr3_112 <- lm(M_tot_wd~P_tot_wd+Hour, data = Winter_mean)
wtr4_112 <- lm(M_tot_wd~CO2_avg+Hour, data = Winter_mean)

summary(wtr1_112)

mape((wtr4_112$fitted.values), Winter_mean$M_tot_wd)



#########Autumn_112_-#############

Autumn_112 <- filter(Daily_Data_112, Season=='Autumn')

Autumn_112$Window1 <- Autumn_112$Window[which(Autumn_112$Window>1)] <- 1

Autumn_112 <- Autumn_112[-c(14)]

atm1_112 <- lm(M_tot~LP_avg+P_tot, data = Autumn_112)
atm2_112 <- lm(M_tot~LP_avg, data = Autumn_112)
atm3_112 <- lm(M_tot~P_tot, data = Autumn_112)
atm4_112 <- lm(M_tot~CO2_avg+Window, data = Autumn_112)

summary(wtr3_112)


plot(Winter_mean$M_tot_wd, type='l', col='Black', xaxt='n', lwd=3, xlab='Hour', ylab='Hourly movements detected', main='Model 2, using lighting energy consumption for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
axis(1,c(1, 24, 48, 72, 96, 120, 144, 168), cex.axis=1.5)
lines(wtr3_112$fitted.values, type='l', col='green', lty=2, lwd=3)


Autumn_mean <- Autumn_112 %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_avg), CO2_avg = mean(CO2_avg), RH_avg = mean(RH_avg), Temp_Avg = mean(Temp_avg), Window_avg = mean(Window))

Autumn_mean[c(3:9)] <- round(Autumn_mean[c(3:9)], digits = 2)

Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
             "Friday", "Saturday", "Sunday")

Autumn_mean$Day <- factor(Autumn_mean$Day, levels = Daylabs)

Autumn_mean <- Autumn_mean[order(Autumn_mean$Day),] 

atm1_112 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+Hour, data = Autumn_mean)
atm2_112 <- lm(M_tot_wd~LP_tot_wd, data = Autumn_mean)
atm3_112 <- lm(M_tot_wd~P_tot_wd, data = Autumn_mean)
atm4_112 <- lm(M_tot_wd~CO2_avg, data = Winter_mean)

summary(atm1_112)

mape((atm3_112$fitted.values), Autumn_mean$M_tot_wd)


#############

Daily_Data_112 <- A_112_Nonzerodays[c(1:2,78:81,76,77,82:86)]

Daily_Data_112[c(10:12)] <- round(Daily_Data_112[c(10:12)], digits = 0)

#Daily_Data_112$Window1 <- Daily_Data_112$Window[which(Daily_Data_112$Window>1)] <- 1


#Daily_Data_112 <- Daily_Data_112[c(-12)]

Mean_Daily_Data_112 <- Daily_Data_112 %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_avg), CO2_avg = mean(CO2_avg), RH_avg = mean(RH_avg), Temp_Avg = mean(Temp_avg))

Mean_Daily_Data_112[c(3,4,5,6,7,8)] <- round(Mean_Daily_Data_112[c(3,4,5,6,7,8)], digits = 0)

Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
             "Friday", "Saturday", "Sunday")

Mean_Daily_Data_112$Day <- factor(Mean_Daily_Data_112$Day, levels = Daylabs)

Mean_Daily_Data_112 <- Mean_Daily_Data_112[order(Mean_Daily_Data_112$Day),] 



#######Linear regression_A112______

pairs(Pairs_graph_112[c(3:7)], col=Pairs_graph_112$`Occupant movement`, cex=1.5, cex.labels = 1.9, cex.axis=1.5)

pairs(Cluster_1_mean_profile_type_1[2:6])

##Multiple linear regression model

#Mean_Daily_Data_112 <- Mean_Daily_Data_112[order(Mean_Daily_Data_112$Day),]

lm1_112 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+CO2_avg+RH_avg+Hour, data = Mean_Daily_Data_112)
lm2_112 <- lm(M_tot_wd~LP_tot_wd+Hour, data = Mean_Daily_Data_112)
lm3_112 <- lm(M_tot_wd~P_tot_wd+Hour, data = Mean_Daily_Data_112)
lm4_112 <- lm(M_tot_wd~CO2_avg+Hour, data = Mean_Daily_Data_112)
lm5_112 <- lm(M_tot_wd~RH_avg+Hour, data = Mean_Daily_Data_112)
#lm5_112 <- lm(M_tot_wd~CO2_avg+Hour, data = Mean_Daily_Data_112)


#lm1_112 <- lm(M_tot~LP_avg+Hour, data = Daily_Data_112)

lm2_112_woh <- lm(M_tot_wd~LP_tot_wd, data = Mean_Daily_Data_112)
lm3_112 <- lm(M_tot_wd~P_tot_wd, data = Mean_Daily_Data_112)
lm4_112 <- lm(M_tot_wd~CO2_avg, data = Mean_Daily_Data_112)
lm5_112 <- lm(M_tot_wd~RH_avg, data = Mean_Daily_Data_112)


summary(lm1_112)

mape((lm1_112$fitted.values), Mean_Daily_Data_112$M_tot_wd)



#############Daily_data


par(mfrow=c(2,1))
abline(v=c(24,48,72,96,120, 144, 168), lty=2)


plot(Mean_Daily_Data_112$M_tot_wd, type='l', col='Black', ylim=c(0,50), lwd=3, xlab='Hour', ylab='Hourly movements detected', xaxt='n', main='Model 1 with hour variable', cex.lab=1.3, cex.axis=1)
axis(1,c(1, 24, 48, 72, 96, 120, 144, 168), cex.axis=1.5)
legend(
  "topright", 
  col = c('Black', 'Green'),
  legend = c("Actual values", "Predicted values"), lty = c("solid", 'dotted'), bty = 'n', cex = 1, lwd=3)
lines(lm2_112$fitted.values, type='l', col='green', lty=2, lwd=3)

plot(Mean_Daily_Data_112$M_tot_wd, type='l', col='Black', ylim=c(0,50), lwd=3, xlab='Hour', ylab='Hourly movements detected', xaxt='n', main='Model 1 without hour variable', cex.lab=1.3, cex.axis=1)
axis(1,c(1, 24, 48, 72, 96, 120, 144, 168), cex.axis=1.5)
legend(
  "topright", 
  col = c('Black', 'Red'),
  legend = c("Actual values", "Predicted values"), lty = c("solid", 'dotted'), bty = 'n', cex = 1, lwd=3)

lines(lm2_112_woh$fitted.values, type='l', col='red', lty=2, lwd=3)
lines(lm3_112$fitted.values, type='l', col='red', lty=2, lwd=3)
lines(lm4_112$fitted.values, type='l', col='blue', lty=2, lwd=3)
lines(lm5$fitted.values, type='l', col='orange', lty=2, lwd=3)

par(mfrow=c(1,2))
plot(predict(lm3_112), Mean_Daily_Data_112$M_tot_wd, main='Model 1', xlab='Predicted values', ylab = 'Actual values')
abline(a=0, b=1, col='red', lwd=2)
modsum3<- summary(lm3_112)

r2=modsum3$adj.r.squared
modsum3$coefficients
my.p = modsum3$coefficients[2,4]
mylabel = bquote(
  paste(
  italic(R)^2 == .(format(r2, digits = 3)), 
  italic(', MAPE = 0.37'), sep = ""))
text(x = 16, y = 43, labels = mylabel, cex=0.9, lwd=3)


#par(mfrow=c(1,2))
plot(predict(lm2_112), Mean_Daily_Data_112$M_tot_wd, main='Model 2', xlab='Predicted values', ylab = 'Actual values')
abline(a=0, b=1, col='red', lwd=2)
modsum4<- summary(lm2_112)

r2_lm2=modsum4$adj.r.squared
modsum4$coefficients
my.p = modsum4$coefficients[2,4]
mylabel = bquote(
  paste(
    italic(R)^2 == .(format(r2_lm2, digits = 3)), 
    italic(', MAPE = 0.23'), sep = ""))
text(x = 12, y = 43, labels = mylabel, cex = 0.9, lwd=3)


rp = vector('expression',2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]

legend('topright', legend = rp, bty = 'n')

install.packages('Metrics')

library(Metrics)


mape((lm4_112$fitted.values), Mean_Daily_Data_112$M_tot_wd)






################122 Linear regression################

A_122_Nonzerodays <- as.data.frame(A_122_Nonzerodays)

library(dplyr)

A_122_Nonzerodays <- A_122_Nonzerodays %>%
  mutate(LP_avg = LP1+LP2+LP3+LP4+LP5+LP6+LP7+LP8+LP9+LP10+LP11+LP12)

A_122_Nonzerodays <- A_122_Nonzerodays %>%
  mutate(CO2_avg = (CO2_LR+CO2_BR1+CO2_BR2+CO2_BR3)/4)

A_122_Nonzerodays <- A_122_Nonzerodays %>%
  mutate(RH_avg = (RH_LR+RH_BR1+RH_BR2+RH_BR3)/4)

A_122_Nonzerodays <- A_122_Nonzerodays %>%
  mutate(Temp_avg = (temp_LR+temp_BR1+temp_BR2+temp_BR3)/4)

A_122_Nonzerodays <- A_122_Nonzerodays %>%
  mutate(Window = W1+W2+W3+W4+W5+W6+W7)

library(lubridate)
A_122_Nonzerodays$Month <- month(A_122_Nonzerodays$Day_index)

A_122_Nonzerodays$Season <- Func.Season(A_122_Nonzerodays$Month)



Summer_122$Window1 <- Summer_122$Window[which(Summer_122$Window>1)] <- 1

Summer_122 <- Summer_122[-c(14)]


######Summer_122____########

Summer_122 <- filter(Daily_Data_122, Season=='Summer')

plot(Summer_mean$M_tot_wd, type='l', col='Red', lwd=3, xlab='Hour', ylab='Hourly movements detected', main='Model 2, using lighting energy consumption for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
lines(smmr2_122$fitted.values, type='l', col='green', lty=2, lwd=3)




Summer_mean <- Summer_122 %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_avg), CO2_avg = mean(CO2_avg), RH_avg = mean(RH_avg), Temp_Avg = mean(Temp_avg), Window_avg = mean(Window))

Summer_mean[c(3:9)] <- round(Summer_mean[c(3:9)], digits = 2)

Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
             "Friday", "Saturday", "Sunday")

Summer_mean$Day <- factor(Summer_mean$Day, levels = Daylabs)

Summer_mean <- Summer_mean[order(Summer_mean$Day),] 



smmr1_122 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+Hour, data = Summer_mean)
smmr2_122 <- lm(M_tot_wd~LP_tot_wd+Hour, data = Summer_mean)
smmr3_122 <- lm(M_tot_wd~P_tot_wd+Hour, data = Summer_mean)
smmr4_122 <- lm(M_tot_wd~CO2_avg+Hour, data = Summer_mean)
smmr5_122 <- lm(M_tot_wd~Temp_Avg+Hour, data = Summer_mean)
smmr6_122 <- lm(M_tot_wd~RH_avg+Hour, data = Summer_mean)
smmr7_122 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+Hour+CO2_avg+Temp_Avg+RH_avg, data = Summer_mean)


summary(smmr2_122)
mape((smmr1_122$fitted.values), Summer_mean$M_tot_wd)

plot(predict(smmr1_122), Summer_122$M_tot, main='Model 1', xlab='Predicted values', ylab = 'Actual values')


#########Winter_122_-#############

Winter_122 <- filter(Daily_Data_122, Season=='Winter')

Winter_122$Window1 <- Winter_122$Window[which(Winter_122$Window>1)] <- 1

Winter_122 <- Winter_122[-c(14)]

wtr1_122 <- lm(M_tot~LP_avg+P_tot, data = Winter_122)
wtr2_122 <- lm(M_tot~LP_avg, data = Winter_122)
wtr3_122 <- lm(M_tot~P_tot, data = Winter_122)
wtr4_122 <- lm(M_tot~CO2_avg+Window, data = Winter_122)

summary(wtr4_122)



plot(Winter_mean$CO2_avg, type='l', col='Black', lwd=3, xlab='Hour', ylab='Hourly movements detected', main='Model 2, using lighting energy consumption for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
lines(wtr2_122$fitted.values, type='l', col='green', lty=2, lwd=3)


Winter_mean <- Winter_122 %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_avg), CO2_avg = mean(CO2_avg), RH_avg = mean(RH_avg), Temp_Avg = mean(Temp_avg), Window_avg = mean(Window))

Winter_mean[c(3:9)] <- round(Winter_mean[c(3:9)], digits = 2)

Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
             "Friday", "Saturday", "Sunday")

Winter_mean$Day <- factor(Winter_mean$Day, levels = Daylabs)

Winter_mean <- Winter_mean[order(Winter_mean$Day),] 

wtr1_122 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+Hour, data = Winter_mean)
wtr2_122 <- lm(M_tot_wd~LP_tot_wd+Hour, data = Winter_mean)
wtr3_122 <- lm(M_tot_wd~P_tot_wd+Hour, data = Winter_mean)
wtr4_122 <- lm(M_tot_wd~CO2_avg+Hour, data = Winter_mean)

summary(wtr2_122)

mape((wtr4_122$fitted.values), Winter_mean$M_tot_wd)



#########Autumn_122_-#############

Autumn_122 <- filter(Daily_Data_122, Season=='Autumn')

Autumn_122$Window1 <- Autumn_122$Window[which(Autumn_122$Window>1)] <- 1

Autumn_122 <- Autumn_122[-c(14)]

atm1_122 <- lm(M_tot~LP_avg+P_tot, data = Autumn_122)
atm2_122 <- lm(M_tot~LP_avg, data = Autumn_122)
atm3_122 <- lm(M_tot~P_tot, data = Autumn_122)
atm4_122 <- lm(M_tot~CO2_avg+Window, data = Autumn_122)

summary(wtr3_122)


plot(Winter_mean$M_tot_wd, type='l', col='Black', lwd=3, xlab='Hour', ylab='Hourly movements detected', main='Model 2, using lighting energy consumption for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
lines(wtr3_122$fitted.values, type='l', col='green', lty=2, lwd=3)


Autumn_mean <- Autumn_122 %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_avg), CO2_avg = mean(CO2_avg), RH_avg = mean(RH_avg), Temp_Avg = mean(Temp_avg), Window_avg = mean(Window))

Autumn_mean[c(3:9)] <- round(Autumn_mean[c(3:9)], digits = 2)

Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
             "Friday", "Saturday", "Sunday")

Autumn_mean$Day <- factor(Autumn_mean$Day, levels = Daylabs)

Autumn_mean <- Autumn_mean[order(Autumn_mean$Day),] 

atm1_122 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+Hour, data = Autumn_mean)
atm2_122 <- lm(M_tot_wd~LP_tot_wd, data = Autumn_mean)
atm3_122 <- lm(M_tot_wd~P_tot_wd, data = Autumn_mean)
atm4_122 <- lm(M_tot_wd~CO2_avg, data = Winter_mean)

summary(atm1_122)

mape((atm3_122$fitted.values), Autumn_mean$M_tot_wd)


Daily_Data_122 <- A_122_Nonzerodays[c(1,2,76,77,83,84,74,75,78:82)]

Mean_Daily_Data_122 <- Daily_Data_122 %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_avg), CO2_avg = mean(CO2_avg), RH_avg = mean(RH_avg))

Mean_Daily_Data_122[c(3,4,5,6,7)] <- round(Mean_Daily_Data_122[c(3,4,5,6,7)], digits = 0)

Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
             "Friday", "Saturday", "Sunday")

Mean_Daily_Data_122$Day <- factor(Mean_Daily_Data_122$Day, levels = Daylabs)

Mean_Daily_Data_122 <- Mean_Daily_Data_122[order(Mean_Daily_Data_122$Day),] 



#######Linear regression_A122______

pairs(Mean_Daily_Data_122[c(3:6)], col=Mean_Daily_Data_122$M_tot_wd)

pairs(Cluster_1_mean_profile_type_1[2:6])

##Multiple linear regression model

#Mean_Daily_Data_122 <- Mean_Daily_Data_122[order(Mean_Daily_Data_122$Day),]

lm1_122 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+CO2_avg+RH_avg+Hour, data = Mean_Daily_Data_122)
lm2_122 <- lm(M_tot_wd~LP_tot_wd+Hour, data = Mean_Daily_Data_122)
lm3_122 <- lm(M_tot_wd~P_tot_wd+Hour, data = Mean_Daily_Data_122)
lm4_122 <- lm(M_tot_wd~CO2_avg+Hour, data = Mean_Daily_Data_122)
lm5_122 <- lm(M_tot_wd~RH_avg+Hour, data = Mean_Daily_Data_122)




lm1_122 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+Hour, data = Mean_Daily_Data_122)
lm2_122 <- lm(M_tot_wd~LP_tot_wd, data = Mean_Daily_Data_122)
lm3_122 <- lm(M_tot_wd~P_tot_wd, data = Mean_Daily_Data_122)
lm4_122 <- lm(M_tot_wd~CO2_avg, data = Mean_Daily_Data_122)
lm5_122 <- lm(M_tot_wd~RH_avg, data = Mean_Daily_Data_122)



summary(lm1_122)
mape((lm1_122$fitted.values), Mean_Daily_Data_122$M_tot_wd)

par(mfrow=c(1,2))
plot(predict(lm3_122), Mean_Daily_Data_122$M_tot_wd, main='Model 1', xlab='Predicted values', ylab = 'Actual values')
abline(a=0, b=1, col='red', lwd=2)
modsum3<- summary(lm3_122)

r2=modsum3$adj.r.squared
modsum3$coefficients
my.p = modsum3$coefficients[2,4]
mylabel = bquote(
  paste(
    italic(R)^2 == .(format(r2, digits = 3)), 
    italic(', MAPE = 0.37'), sep = ""))
text(x = 22, y = 43, labels = mylabel, cex=0.9, lwd=3)


#par(mfrow=c(1,2))
plot(predict(lm2_122), Mean_Daily_Data_122$M_tot_wd, main='Model 2', xlab='Predicted values', ylab = 'Actual values')
abline(a=0, b=1, col='red', lwd=2)
modsum4<- summary(lm2_122)

r2_lm2=modsum4$adj.r.squared
modsum4$coefficients
my.p = modsum4$coefficients[2,4]
mylabel = bquote(
  paste(
    italic(R)^2 == .(format(r2_lm2, digits = 3)), 
    italic(', MAPE = 0.23'), sep = ""))
text(x = 16, y = 43, labels = mylabel, cex = 0.9, lwd=3)


rp = vector('expression',2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]

legend('topright', legend = rp, bty = 'n')

install.packages('Metrics')

library(Metrics)


mape((lm4_122$fitted.values), Mean_Daily_Data_122$M_tot_wd)


Mean_Daily_Data_122$Hour <- as.numeric(Mean_Daily_Data_122$Hour)

plot(x=Mean_Daily_Data_122$Hour, y=Mean_Daily_Data_122$LP_tot_wd, type='l')


#############Daily_data


par(mfrow=c(2,1))
abline(v=c(24,48,72,96,120, 144, 168), lty=2)


plot(Mean_Daily_Data_122$M_tot_wd, type='l', col='Black', ylim=c(0,50), lwd=3, xlab='Hour', ylab='Hourly movements detected', xaxt='n', main='Model 2, using lighting energy consumption for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
axis(1,c(1, 24, 48, 72, 96, 120, 144, 168), cex.axis=1.5)
legend(
  "topright", 
  col = c('Black', 'Green'),
  legend = c("Actual values", "Predicted values"), lty = c("solid", 'dotted'), bty = 'n', cex = 1, lwd=3)
lines(lm1_122$fitted.values, type='l', col='red', lty=2, lwd=3)

plot(Mean_Daily_Data_122$M_tot_wd, type='l', col='Black', ylim=c(0,50), lwd=3, xlab='Hour', ylab='Hourly movements detected', xaxt='n', main='Model 1, using plugload  for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
axis(1,c(1, 24, 48, 72, 96, 120, 144, 168), cex.axis=1.5)
legend(
  "topright", 
  col = c('Black', 'Red'),
  legend = c("Actual values", "Predicted values"), lty = c("solid", 'dotted'), bty = 'n', cex = 1, lwd=3)

lines(lm2_122$fitted.values, type='l', col='green', lty=2, lwd=3)
lines(lm3_122$fitted.values, type='l', col='red', lty=2, lwd=3)
lines(lm4_122$fitted.values, type='l', col='blue', lty=2, lwd=3)
lines(lm5$fitted.values, type='l', col='orange', lty=2, lwd=3)




################142 Linear regression################

A_142_Nonzerodays<-as.data.frame(A_142_Nonzerodays)

library(dplyr)

A_142_Nonzerodays <- A_142_Nonzerodays %>%
  mutate(LP_avg = LP1+LP2+LP3+LP4+LP5+LP6+LP7+LP8+LP9+LP10+LP11+LP12)

A_142_Nonzerodays <- A_142_Nonzerodays %>%
  mutate(CO2_avg = (CO2_LR+CO2_BR1+CO2_BR2+CO2_BR3)/4)

A_142_Nonzerodays <- A_142_Nonzerodays %>%
  mutate(RH_avg = (RH_LR+RH_BR1+RH_BR2+RH_BR3)/4)

Daily_Data_142 <- A_142_Nonzerodays[c(1:3,19,34,73:75)]

Mean_Daily_Data_142 <- Daily_Data_142 %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_avg), CO2_avg = mean(CO2_avg), RH_avg = mean(RH_avg))

Mean_Daily_Data_142[c(3,4,5,6,7)] <- round(Mean_Daily_Data_142[c(3,4,5,6,7)], digits = 0)

Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
             "Friday", "Saturday", "Sunday")

Mean_Daily_Data_142$Day <- factor(Mean_Daily_Data_142$Day, levels = Daylabs)

Mean_Daily_Data_142 <- Mean_Daily_Data_142[order(Mean_Daily_Data_142$Day),] 



#######Linear regression_A142______

pairs(Mean_Daily_Data_142[c(3:6)], col=Mean_Daily_Data_142$M_tot_wd)

pairs(Cluster_1_mean_profile_type_1[2:6])

##Multiple linear regression model

#Mean_Daily_Data_142 <- Mean_Daily_Data_142[order(Mean_Daily_Data_142$Day),]

lm1_142 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+Hour, data = Mean_Daily_Data_142)
lm2_142 <- lm(M_tot_wd~LP_tot_wd+Hour, data = Mean_Daily_Data_142)
lm3_142 <- lm(M_tot_wd~P_tot_wd+Hour, data = Mean_Daily_Data_142)
lm4_142 <- lm(M_tot_wd~CO2_avg+Hour, data = Mean_Daily_Data_142)
lm5_142 <- lm(M_tot_wd~RH_avg+Hour, data = Mean_Daily_Data_142)


lm1_142 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+RH_avg+CO2_avg+Hour, data = Mean_Daily_Data_142)
lm2_142 <- lm(M_tot_wd~LP_tot_wd, data = Mean_Daily_Data_142)
lm3_142 <- lm(M_tot_wd~P_tot_wd, data = Mean_Daily_Data_142)
lm4_142 <- lm(M_tot_wd~CO2_avg, data = Mean_Daily_Data_142)
lm5_142 <- lm(M_tot_wd~RH_avg, data = Mean_Daily_Data_142)


summary(lm1_142)

mape((lm1_142$fitted.values), Mean_Daily_Data_142$M_tot_wd)

par(mfrow=c(1,2))
plot(predict(lm3_142), Mean_Daily_Data_142$M_tot_wd, main='Model 1', xlab='Predicted values', ylab = 'Actual values')
abline(a=0, b=1, col='red', lwd=2)
modsum3<- summary(lm3_142)

r2=modsum3$adj.r.squared
modsum3$coefficients
my.p = modsum3$coefficients[2,4]
mylabel = bquote(
  paste(
    italic(R)^2 == .(format(r2, digits = 3)), 
    italic(', MAPE = 0.37'), sep = ""))
text(x = 22, y = 43, labels = mylabel, cex=0.9, lwd=3)


#par(mfrow=c(1,2))
plot(predict(lm2_142), Mean_Daily_Data_142$M_tot_wd, main='Model 2', xlab='Predicted values', ylab = 'Actual values')
abline(a=0, b=1, col='red', lwd=2)
modsum4<- summary(lm2_142)

r2_lm2=modsum4$adj.r.squared
modsum4$coefficients
my.p = modsum4$coefficients[2,4]
mylabel = bquote(
  paste(
    italic(R)^2 == .(format(r2_lm2, digits = 3)), 
    italic(', MAPE = 0.23'), sep = ""))
text(x = 16, y = 43, labels = mylabel, cex = 0.9, lwd=3)


rp = vector('expression',2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]

legend('topright', legend = rp, bty = 'n')

install.packages('Metrics')

library(Metrics)


mape((lm4_142$fitted.values), Mean_Daily_Data_142$M_tot_wd)


#############Daily_data


par(mfrow=c(4,1))
abline(v=c(24,48,72,96,120, 144, 168), lty=2)


plot(Mean_Daily_Data_142$M_tot_wd, type='l', col='Black', ylim=c(0,100), lwd=3, xlab='Hour', ylab='Hourly movements detected', xaxt='n', main='Model 2, using lighting energy consumption for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
axis(1,c(1, 24, 48, 72, 96, 120, 144, 168), cex.axis=1.5)
legend(
  "topright", 
  col = c('Black', 'Green'),
  legend = c("Actual values", "Predicted values"), lty = c("solid", 'dotted'), bty = 'n', cex = 1, lwd=3)
lines(lm1_142$fitted.values, type='l', col='red', lty=2, lwd=3)

plot(Mean_Daily_Data_142$M_tot_wd, type='l', col='Black', ylim=c(0,50), lwd=3, xlab='Hour', ylab='Hourly movements detected', xaxt='n', main='Model 1, using plugload  for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
axis(1,c(1, 24, 48, 72, 96, 120, 144, 168), cex.axis=1.5)
legend(
  "topright", 
  col = c('Black', 'Red'),
  legend = c("Actual values", "Predicted values"), lty = c("solid", 'dotted'), bty = 'n', cex = 1, lwd=3)

lines(lm2_142$fitted.values, type='l', col='green', lty=2, lwd=3)
lines(lm3_142$fitted.values, type='l', col='red', lty=2, lwd=3)
lines(lm4_142$fitted.values, type='l', col='blue', lty=2, lwd=3)
lines(lm5$fitted.values, type='l', col='orange', lty=2, lwd=3)




#############################152 Linear regression################

library(dplyr)

A_152_Nonzerodays <- A_152_Nonzerodays %>%
  mutate(LP_avg = LP1+LP2+LP3+LP4+LP5+LP6+LP7+LP8+LP9+LP10+LP11+LP12+LP13+LP14+LP15)

A_152_Nonzerodays <- A_152_Nonzerodays %>%
  mutate(CO2_avg = (CO2_LR+CO2_BR1+CO2_BR2+CO2_BR3)/4)

A_152_Nonzerodays <- A_152_Nonzerodays %>%
  mutate(RH_avg = (RH_LR+RH_BR1+RH_BR2+RH_BR3)/4)

A_152_Nonzerodays <- A_152_Nonzerodays %>%
  mutate(Temp_avg = (temp_LR+temp_BR1+temp_BR2+temp_BR3)/4)

A_152_Nonzerodays <- A_152_Nonzerodays %>%
  mutate(Window = W1+W2+W3+W4+W5+W6+W7)

library(lubridate)
A_152_Nonzerodays$Month <- month(A_152_Nonzerodays$Day_index)

A_152_Nonzerodays$Season <- Func.Season(A_152_Nonzerodays$Month)


Summer_152$Window1 <- Summer_152$Window[which(Summer_152$Window>1)] <- 1

Summer_152 <- Summer_152[-c(14)]


######Summer_112____########

Summer_152 <- filter(Daily_Data_152_LP_PP_CO2_RH, Season=='Summer')

plot(Summer_mean$M_tot_wd, type='l', col='Black', lwd=3, xlab='Hour', ylab='Hourly movements detected', main='Model 2, using lighting energy consumption for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
lines(smmr3_152$fitted.values, type='l', col='green', lty=2, lwd=3)




Summer_mean <- Summer_152 %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_avg), CO2_avg = mean(CO2_avg), RH_avg = mean(RH_avg), Temp_Avg = mean(Temp_avg), Window_avg = mean(Window))

Summer_mean[c(3:9)] <- round(Summer_mean[c(3:9)], digits = 2)

Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
             "Friday", "Saturday", "Sunday")

Summer_mean$Day <- factor(Summer_mean$Day, levels = Daylabs)

Summer_mean <- Summer_mean[order(Summer_mean$Day),] 



smmr1_152 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+Hour, data = Summer_mean)
smmr2_152 <- lm(M_tot_wd~LP_tot_wd+Hour, data = Summer_mean)
smmr3_152 <- lm(M_tot_wd~P_tot_wd+Hour, data = Summer_mean)
smmr4_152 <- lm(M_tot_wd~CO2_avg+Hour+Window_avg, data = Summer_mean)
smmr5_152 <- lm(M_tot_wd~Temp_Avg+Hour, data = Summer_mean)
smmr6_152 <- lm(M_tot_wd~RH_avg+Hour, data = Summer_mean)
smmr7_152 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+Hour+CO2_avg+Temp_Avg+RH_avg, data = Summer_mean)


summary(smmr4_152)
mape((smmr7_112$fitted.values), Summer_mean$M_tot_wd)

plot(predict(smmr1_112), Summer_112$M_tot, main='Model 1', xlab='Predicted values', ylab = 'Actual values')


#########Winter_112_-#############

Winter_152 <- filter(Daily_Data_152_LP_PP_CO2_RH, Season=='Winter')

Winter_152$Window1 <- Winter_152$Window[which(Winter_152$Window>1)] <- 1

Winter_152 <- Winter_152[-c(14)]

wtr1_112 <- lm(M_tot~LP_avg+P_tot, data = Winter_112)
wtr2_112 <- lm(M_tot~LP_avg, data = Winter_112)
wtr3_112 <- lm(M_tot~P_tot, data = Winter_112)
wtr4_112 <- lm(M_tot~CO2_avg+Window, data = Winter_112)

summary(wtr3_112)


plot(Winter_mean$M_tot_wd, type='l', col='Black', lwd=3, xlab='Hour', ylab='Hourly movements detected', main='Model 2, using lighting energy consumption for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
lines(wtr3_112$fitted.values, type='l', col='green', lty=2, lwd=3)


Winter_mean <- Winter_152 %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_avg), CO2_avg = mean(CO2_avg), RH_avg = mean(RH_avg), Temp_Avg = mean(Temp_avg), Window_avg = mean(Window))

Winter_mean[c(3:9)] <- round(Winter_mean[c(3:9)], digits = 2)

Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
             "Friday", "Saturday", "Sunday")

Winter_mean$Day <- factor(Winter_mean$Day, levels = Daylabs)

Winter_mean <- Winter_mean[order(Winter_mean$Day),] 

wtr1_152 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+Hour, data = Winter_mean)
wtr2_152 <- lm(M_tot_wd~LP_tot_wd+Hour, data = Winter_mean)
wtr3_152 <- lm(M_tot_wd~P_tot_wd+Hour, data = Winter_mean)
wtr4_152 <- lm(M_tot_wd~CO2_avg+Hour, data = Winter_mean)

summary(wtr2_152)

mape((wtr4_112$fitted.values), Winter_mean$M_tot_wd)



#########Autumn_112_-#############

Autumn_152 <- filter(Daily_Data_152_LP_PP_CO2_RH, Season=='Autumn')

Autumn_152$Window1 <- Autumn_152$Window[which(Autumn_152$Window>1)] <- 1

Autumn_152 <- Autumn_152[-c(14)]

atm1_152 <- lm(M_tot~LP_avg+P_tot+Hour, data = Autumn_152)
atm2_152 <- lm(M_tot~LP_avg, data = Autumn_152)
atm3_152 <- lm(M_tot~P_tot, data = Autumn_152)
atm4_152 <- lm(M_tot~CO2_avg+Window, data = Autumn_152)

summary(atm1_152)


plot(Winter_mean$M_tot_wd, type='l', col='Black', lwd=3, xlab='Hour', ylab='Hourly movements detected', main='Model 2, using lighting energy consumption for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
lines(wtr3_112$fitted.values, type='l', col='green', lty=2, lwd=3)


Autumn_mean <- Autumn_152 %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_avg), CO2_avg = mean(CO2_avg), RH_avg = mean(RH_avg), Temp_Avg = mean(Temp_avg), Window_avg = mean(Window))

Autumn_mean[c(3:9)] <- round(Autumn_mean[c(3:9)], digits = 2)

Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
             "Friday", "Saturday", "Sunday")

Autumn_mean$Day <- factor(Autumn_mean$Day, levels = Daylabs)

Autumn_mean <- Autumn_mean[order(Autumn_mean$Day),] 

atm1_152 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+Hour, data = Autumn_mean)
atm2_152 <- lm(M_tot_wd~LP_tot_wd+Hour, data = Autumn_mean)
atm3_152 <- lm(M_tot_wd~P_tot_wd+Hour, data = Autumn_mean)
atm4_152 <- lm(M_tot_wd~CO2_avg+Hour, data = Autumn_mean)

summary(atm3_152)

mape((atm3_112$fitted.values), Autumn_mean$M_tot_wd)



Daily_Data_152_LP_PP_CO2_RH <- A_152_Nonzerodays[c(1,2,82,83,89,90,80,81,84:88)]


Mean_Daily_Data_152 <- Daily_Data_152_LP_PP_CO2_RH %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_avg), CO2_avg = mean(CO2_avg), RH_avg = mean(RH_avg))

Mean_Daily_Data_152[c(3,4,5,6,7)] <- round(Mean_Daily_Data_152[c(3,4,5,6,7)], digits = 0)


Mean_Daily_Data_152 <- Mean_Daily_Data_152[order(Mean_Daily_Data_152$Day),] 


Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
            "Friday", "Saturday", "Sunday")

Mean_Daily_Data_152$Day <- factor(Mean_Daily_Data_152$Day, levels = Daylabs)

Mean_Daily_Data_152 <- Mean_Daily_Data_152[order(Mean_Daily_Data_152$Day),] 



Mean_Daily_Data_152$Hour <- as.numeric(Mean_Daily_Data_152$Hour)
#######Linear regression

pairs(Mean_Daily_Data_152[c(3:6)], col=Mean_Daily_Data_152$M_tot_wd)

#pairs(Cluster_1_mean_profile_type_1[2:6])

##Multiple linear regression model

Mean_Daily_Data_152 <- Mean_Daily_Data_152[order(Mean_Daily_Data_152$Day),]

lm1_152 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+Hour, data = Mean_Daily_Data_152)
lm2_152 <- lm(M_tot_wd~LP_tot_wd+Hour, data = Mean_Daily_Data_152)
lm3_152 <- lm(M_tot_wd~P_tot_wd+Hour, data = Mean_Daily_Data_152)
lm4_152 <- lm(M_tot_wd~CO2_avg+Hour, data = Mean_Daily_Data_152)
lm5_152 <- lm(M_tot_wd~RH_avg+Hour, data = Mean_Daily_Data_152)


lm1_152 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+CO2_avg+RH_avg+Hour, data = Mean_Daily_Data_152)
lm2_152 <- lm(M_tot_wd~LP_tot_wd, data = Mean_Daily_Data_152)
lm3_152 <- lm(M_tot_wd~P_tot_wd, data = Mean_Daily_Data_152)
lm4_152 <- lm(M_tot_wd~CO2_avg, data = Mean_Daily_Data_152)
lm5_152 <- lm(M_tot_wd~RH_avg, data = Mean_Daily_Data_152)

summary(lm1_152)

mape((lm1_152$fitted.values), Mean_Daily_Data_152$M_tot_wd)


plot(predict(lm1_152), Mean_Daily_Data_152$M_tot_wd)
abline(a=0, b=1)


install.packages('Metrics')

library(Metrics)


mape((lm2_152$fitted.values), Mean_Daily_Data_152$M_tot_wd)


#############Daily_data
abline(v=c(24,48,72,96,120))
par(mfrow=c(2,2))
plot(Mean_Daily_Data_152$M_tot_wd, type='l', col='black', ylim=c(0,100))
lines(lm1_152$fitted.values, type='l', col='red')
lines(lm2_152$fitted.values, type='l', col='green')
lines(lm3_152$fitted.values, type='l', col='black')
lines(lm5_152$fitted.values, type='l', col='blue')


#########
par(mfrow=c(1,2))
plot(predict(lm3_152), Mean_Daily_Data_152$M_tot_wd, main='Model 1', xlab='Predicted values', ylab = 'Actual values')
abline(a=0, b=1, col='red', lwd=2)
modsum3<- summary(lm3_152)

r2=modsum3$adj.r.squared
modsum3$coefficients
my.p = modsum3$coefficients[2,4]
mylabel = bquote(
  paste(
    italic(R)^2 == .(format(r2, digits = 3)), 
    italic(', MAPE = 0.41'), sep = ""))
text(x = 18, y = 68, labels = mylabel, cex=0.9, lwd=3)


#par(mfrow=c(1,2))
plot(predict(lm2_152), Mean_Daily_Data_152$M_tot_wd, main='Model 2', xlab='Predicted values', ylab = 'Actual values')
abline(a=0, b=1, col='red', lwd=2)
modsum4<- summary(lm2_152)

r2_lm2=modsum4$adj.r.squared
modsum4$coefficients
my.p = modsum4$coefficients[2,4]
mylabel = bquote(
  paste(
    italic(R)^2 == .(format(r2_lm2, digits = 3)), 
    italic(', MAPE = 0.43'), sep = ""))
text(x = 28, y = 68, labels = mylabel, cex = 0.9, lwd=3)


################162 Linear regression################

A_162_Nonzerodays <- as.data.frame(A_162_Nonzerodays)

library(dplyr)

A_162_Nonzerodays <- A_162_Nonzerodays %>%
  mutate(LP_avg = LP1+LP2+LP3+LP4+LP5+LP6+LP7+LP8+LP9+LP10+LP11+LP12+LP13+LP14)

A_162_Nonzerodays <- A_162_Nonzerodays %>%
  mutate(CO2_avg = (CO2_LR+CO2_BR1+CO2_BR2+CO2_BR3)/4)

A_162_Nonzerodays <- A_162_Nonzerodays %>%
  mutate(RH_avg = (RH_LR+RH_BR1+RH_BR2+RH_BR3)/4)

Daily_Data_162 <- A_162_Nonzerodays[c(2:4,21,36,78:80)]

Mean_Daily_Data_162 <- Daily_Data_162 %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_avg), CO2_avg = mean(CO2_avg), RH_avg = mean(RH_avg))

Mean_Daily_Data_162[c(3,4,5,6,7)] <- round(Mean_Daily_Data_162[c(3,4,5,6,7)], digits = 0)

Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
             "Friday", "Saturday", "Sunday")

Mean_Daily_Data_162$Day <- factor(Mean_Daily_Data_162$Day, levels = Daylabs)

Mean_Daily_Data_162 <- Mean_Daily_Data_162[order(Mean_Daily_Data_162$Day),] 



#######Linear regression_A162______

pairs(Mean_Daily_Data_162[c(3:6)], col=Mean_Daily_Data_162$M_tot_wd)

pairs(Cluster_1_mean_profile_type_1[2:6])

##Multiple linear regression model

#Mean_Daily_Data_162 <- Mean_Daily_Data_162[order(Mean_Daily_Data_162$Day),]

lm1_162 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+RH_avg+CO2_avg+Hour, data = Mean_Daily_Data_162)
lm2_162 <- lm(M_tot_wd~LP_tot_wd+Hour, data = Mean_Daily_Data_162)
lm3_162 <- lm(M_tot_wd~P_tot_wd+Hour, data = Mean_Daily_Data_162)
lm4_162 <- lm(M_tot_wd~CO2_avg+Hour, data = Mean_Daily_Data_162)
lm5_162 <- lm(M_tot_wd~RH_avg+Hour, data = Mean_Daily_Data_162)


lm2_162 <- lm(M_tot_wd~LP_tot_wd, data = Mean_Daily_Data_162)
lm3_162 <- lm(M_tot_wd~P_tot_wd, data = Mean_Daily_Data_162)
lm4_162 <- lm(M_tot_wd~CO2_avg, data = Mean_Daily_Data_162)
lm5_162 <- lm(M_tot_wd~RH_avg, data = Mean_Daily_Data_162)

summary(lm1_162)

mape((lm1_162$fitted.values), Mean_Daily_Data_162$M_tot_wd)

par(mfrow=c(1,2))
plot(predict(lm3_162), Mean_Daily_Data_162$M_tot_wd, main='Model 1', xlab='Predicted values', ylab = 'Actual values')
abline(a=0, b=1, col='red', lwd=2)
modsum3<- summary(lm3_162)

r2=modsum3$adj.r.squared
modsum3$coefficients
my.p = modsum3$coefficients[2,4]
mylabel = bquote(
  paste(
    italic(R)^2 == .(format(r2, digits = 3)), 
    italic(', MAPE = 0.37'), sep = ""))
text(x = 22, y = 43, labels = mylabel, cex=0.9, lwd=3)


#par(mfrow=c(1,2))
plot(predict(lm2_162), Mean_Daily_Data_162$M_tot_wd, main='Model 2', xlab='Predicted values', ylab = 'Actual values')
abline(a=0, b=1, col='red', lwd=2)
modsum4<- summary(lm2_162)

r2_lm2=modsum4$adj.r.squared
modsum4$coefficients
my.p = modsum4$coefficients[2,4]
mylabel = bquote(
  paste(
    italic(R)^2 == .(format(r2_lm2, digits = 3)), 
    italic(', MAPE = 0.23'), sep = ""))
text(x = 16, y = 43, labels = mylabel, cex = 0.9, lwd=3)


rp = vector('expression',2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]

legend('topright', legend = rp, bty = 'n')

install.packages('Metrics')

library(Metrics)


mape((lm2_162$fitted.values), Mean_Daily_Data_162$M_tot_wd)


#############Daily_data


par(mfrow=c(4,1))
abline(v=c(24,48,72,96,120, 144, 168), lty=2)


plot(Mean_Daily_Data_162$M_tot_wd, type='l', col='Black', ylim=c(0,80), lwd=3, xlab='Hour', ylab='Hourly movements detected', xaxt='n', main='Model 2, using lighting energy consumption for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
axis(1,c(1, 24, 48, 72, 96, 120, 144, 168), cex.axis=1.5)
legend(
  "topright", 
  col = c('Black', 'Green'),
  legend = c("Actual values", "Predicted values"), lty = c("solid", 'dotted'), bty = 'n', cex = 1, lwd=3)
lines(lm1_162$fitted.values, type='l', col='red', lty=2, lwd=3)

plot(Mean_Daily_Data_162$M_tot_wd, type='l', col='Black', ylim=c(0,50), lwd=3, xlab='Hour', ylab='Hourly movements detected', xaxt='n', main='Model 1, using plugload  for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
axis(1,c(1, 24, 48, 72, 96, 120, 144, 168), cex.axis=1.5)
legend(
  "topright", 
  col = c('Black', 'Red'),
  legend = c("Actual values", "Predicted values"), lty = c("solid", 'dotted'), bty = 'n', cex = 1, lwd=3)

lines(lm2_162$fitted.values, type='l', col='green', lty=2, lwd=3)
lines(lm3_162$fitted.values, type='l', col='red', lty=2, lwd=3)
lines(lm4_162$fitted.values, type='l', col='blue', lty=2, lwd=3)
lines(lm5$fitted.values, type='l', col='orange', lty=2, lwd=3)




################172 Linear regression################

library(dplyr)

A_172_Nonzerodays <- as.data.frame(A_172_Nonzerodays)

A_172_Nonzerodays <- A_172_Nonzerodays %>%
  mutate(LP_avg = LP1+LP2+LP3+LP4+LP5+LP6+LP7+LP8+LP9+LP10+LP11+LP12)

A_172_Nonzerodays <- A_172_Nonzerodays %>%
  mutate(CO2_avg = (CO2_LR+CO2_BR1+CO2_BR2+CO2_BR3)/4)

A_172_Nonzerodays <- A_172_Nonzerodays %>%
  mutate(RH_avg = (RH_LR+RH_BR1+RH_BR2+RH_BR3)/4)

Daily_Data_172 <- A_172_Nonzerodays[c(1:3,18,32,70:72)]

Mean_Daily_Data_172 <- Daily_Data_172 %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_avg), CO2_avg = mean(CO2_avg), RH_avg = mean(RH_avg))

Mean_Daily_Data_172[c(3,4,5,6,7)] <- round(Mean_Daily_Data_172[c(3,4,5,6,7)], digits = 0)

Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
             "Friday", "Saturday", "Sunday")

Mean_Daily_Data_172$Day <- factor(Mean_Daily_Data_172$Day, levels = Daylabs)

Mean_Daily_Data_172 <- Mean_Daily_Data_172[order(Mean_Daily_Data_172$Day),] 



#######Linear regression_A172______

pairs(Mean_Daily_Data_172[c(3:6)], col=Mean_Daily_Data_172$M_tot_wd)

pairs(Cluster_1_mean_profile_type_1[2:6])

##Multiple linear regression model

#Mean_Daily_Data_172 <- Mean_Daily_Data_172[order(Mean_Daily_Data_172$Day),]

lm1_172 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+CO2_avg+RH_avg+Hour, data = Mean_Daily_Data_172)
lm2_172 <- lm(M_tot_wd~LP_tot_wd+Hour, data = Mean_Daily_Data_172)
lm3_172 <- lm(M_tot_wd~P_tot_wd+Hour, data = Mean_Daily_Data_172)
lm4_172 <- lm(M_tot_wd~CO2_avg+Hour, data = Mean_Daily_Data_172)
lm5_172 <- lm(M_tot_wd~RH_avg+Hour, data = Mean_Daily_Data_172)



lm2_172 <- lm(M_tot_wd~LP_tot_wd, data = Mean_Daily_Data_172)
lm3_172 <- lm(M_tot_wd~P_tot_wd, data = Mean_Daily_Data_172)
lm4_172 <- lm(M_tot_wd~CO2_avg, data = Mean_Daily_Data_172)
lm5_172 <- lm(M_tot_wd~RH_avg, data = Mean_Daily_Data_172)

summary(lm1_172)
mape((lm1_172$fitted.values), Mean_Daily_Data_172$M_tot_wd)

#dev.off()
#plot(lm3_172$fitted.values)
par(mfrow=c(1,2))
plot(predict(lm3_172), Mean_Daily_Data_172$M_tot_wd, main='Model 1', xlab='Predicted values', ylab = 'Actual values')
abline(a=0, b=1, col='red', lwd=2)
modsum3<- summary(lm3_172)

r2=modsum3$adj.r.squared
modsum3$coefficients
my.p = modsum3$coefficients[2,4]
mylabel = bquote(
  paste(
    italic(R)^2 == .(format(r2, digits = 3)), 
    italic(', MAPE = 0.37'), sep = ""))
text(x = 22, y = 43, labels = mylabel, cex=0.9, lwd=3)


#par(mfrow=c(1,2))
plot(predict(lm2_172), Mean_Daily_Data_172$M_tot_wd, main='Model 2', xlab='Predicted values', ylab = 'Actual values')
abline(a=0, b=1, col='red', lwd=2)
modsum4<- summary(lm2_172)

r2_lm2=modsum4$adj.r.squared
modsum4$coefficients
my.p = modsum4$coefficients[2,4]
mylabel = bquote(
  paste(
    italic(R)^2 == .(format(r2_lm2, digits = 3)), 
    italic(', MAPE = 0.23'), sep = ""))
text(x = 16, y = 43, labels = mylabel, cex = 0.9, lwd=3)


rp = vector('expression',2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]

legend('topright', legend = rp, bty = 'n')

install.packages('Metrics')

library(Metrics)


mape((lm2_172$fitted.values), Mean_Daily_Data_172$M_tot_wd)


#############Daily_data


par(mfrow=c(2,1))
abline(v=c(24,48,72,96,120, 144, 168), lty=2)


plot(Mean_Daily_Data_172$M_tot_wd, type='l', col='Black', ylim=c(0,50), lwd=3, xlab='Hour', ylab='Hourly movements detected', xaxt='n', main='Model 2, using lighting energy consumption for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
axis(1,c(1, 24, 48, 72, 96, 120, 144, 168), cex.axis=1.5)
legend(
  "topright", 
  col = c('Black', 'Green'),
  legend = c("Actual values", "Predicted values"), lty = c("solid", 'dotted'), bty = 'n', cex = 1, lwd=3)
lines(lm4_172$fitted.values, type='l', col='red', lty=2, lwd=3)

plot(Mean_Daily_Data_172$M_tot_wd, type='l', col='Black', ylim=c(0,50), lwd=3, xlab='Hour', ylab='Hourly movements detected', xaxt='n', main='Model 1, using plugload  for occupanct hourly movement prediction', cex.lab=1.3, cex.axis=1)
axis(1,c(1, 24, 48, 72, 96, 120, 144, 168), cex.axis=1.5)
legend(
  "topright", 
  col = c('Black', 'Red'),
  legend = c("Actual values", "Predicted values"), lty = c("solid", 'dotted'), bty = 'n', cex = 1, lwd=3)

lines(lm2_172$fitted.values, type='l', col='green', lty=2, lwd=3)
lines(lm3_172$fitted.values, type='l', col='red', lty=2, lwd=3)
lines(lm4_172$fitted.values, type='l', col='blue', lty=2, lwd=3)
lines(lm5$fitted.values, type='l', col='orange', lty=2, lwd=3)





#############################182 Linear regression################

library(dplyr)

#A_182_Nonzerodays <- A_182_Nonzerodays %>%
  #mutate(LP_avg = LP1+LP2+LP3+LP4+LP5+LP6+LP7+LP8+LP9+LP10+LP11+LP12+LP13+LP14+LP15)

A_182_Nonzerodays <- A_182_Nonzerodays %>%
  mutate(CO2_avg = (CO2_LR+CO2_BR1+CO2_BR2+CO2_BR3)/4)

A_182_Nonzerodays <- A_182_Nonzerodays %>%
  mutate(RH_avg = (RH_LR+RH_BR1+RH_BR2+RH_BR3)/4)

Daily_Data_182_LP_PP_CO2_RH <- A_182_Nonzerodays[c(1,2,4,44,25,60,91:99)]


Mean_Daily_Data_182 <- Daily_Data_182_LP_PP_CO2_RH %>%
  
  group_by(Hour,DayOfWeek) %>%
  
  summarise(P_tot_wd = mean(power_total_1), M_tot_wd = mean(motion_total), LP_tot_wd = mean(light_total), CO2_avg = mean(CO2_avg), RH_avg = mean(RH_avg))

Mean_Daily_Data_182[c(3,4,5,6,7)] <- round(Mean_Daily_Data_182[c(3,4,5,6,7)], digits = 0)


Mean_Daily_Data_182 <- Mean_Daily_Data_182[order(Mean_Daily_Data_182$DayOfWeek),] 


Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
             "Friday", "Saturday", "Sunday")

Mean_Daily_Data_182$DayOfWeek <- factor(Mean_Daily_Data_182$DayOfWeek, levels = Daylabs)

Mean_Daily_Data_182 <- Mean_Daily_Data_182[order(Mean_Daily_Data_182$DayOfWeek),] 



#Mean_Daily_Data_182$Hour <- as.numeric(Mean_Daily_Data_182$Hour)
#######Linear regression

pairs(Pairs_graph_182[c(3:7)], col=Pairs_graph_182$`Occupant movement`, cex=1.2, cex.labels = 1.9, cex.axis=1.5)

#pairs(Cluster_1_mean_profile_type_1[2:6])

##Multiple linear regression model

#Mean_Daily_Data_152 <- Mean_Daily_Data_152[order(Mean_Daily_Data_152$Day),]

lm1_182 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+RH_avg+CO2_avg+Hour, data = Mean_Daily_Data_182)
lm2_182 <- lm(M_tot_wd~LP_tot_wd+Hour, data = Mean_Daily_Data_182)
lm3_182 <- lm(M_tot_wd~P_tot_wd+Hour, data = Mean_Daily_Data_182)
lm4_182 <- lm(M_tot_wd~CO2_avg+Hour, data = Mean_Daily_Data_182)
lm5_182 <- lm(M_tot_wd~RH_avg+Hour, data = Mean_Daily_Data_182)



lm2_182_woh <- lm(M_tot_wd~LP_tot_wd, data = Mean_Daily_Data_182)
lm3_182 <- lm(M_tot_wd~P_tot_wd, data = Mean_Daily_Data_182)
lm4_182 <- lm(M_tot_wd~CO2_avg, data = Mean_Daily_Data_182)
lm5_182 <- lm(M_tot_wd~RH_avg, data = Mean_Daily_Data_182)

summary(lm1_182)
mape((lm1_182$fitted.values), Mean_Daily_Data_182$M_tot_wd)

plot(predict(lm2_182), Mean_Daily_Data_182$M_tot_wd)
abline(a=0, b=1)


#############Daily_data

par(mfrow=c(2,1))
abline(v=c(24,48,72,96,120, 144, 168), lty=2)


plot(Mean_Daily_Data_182$M_tot_wd, type='l', col='Black', ylim=c(0,100), lwd=3, xlab='Hour', ylab='Hourly movements detected', xaxt='n', main='Model 1 with hour variable', cex.lab=1.3, cex.axis=1)
axis(1,c(1, 24, 48, 72, 96, 120, 144, 168), cex.axis=1.5)
legend(
  "topright", 
  col = c('Black', 'Green'),
  legend = c("Actual values", "Predicted values"), lty = c("solid", 'dotted'), bty = 'n', cex = 1, lwd=3)
lines(lm2_182$fitted.values, type='l', col='green', lty=2, lwd=3)

plot(Mean_Daily_Data_182$M_tot_wd, type='l', col='Black', ylim=c(0,100), lwd=3, xlab='Hour', ylab='Hourly movements detected', xaxt='n', main='Model 1 without hour variable', cex.lab=1.3, cex.axis=1)
axis(1,c(1, 24, 48, 72, 96, 120, 144, 168), cex.axis=1.5)
legend(
  "topright", 
  col = c('Black', 'Red'),
  legend = c("Actual values", "Predicted values"), lty = c("solid", 'dotted'), bty = 'n', cex = 1, lwd=3)

lines(lm2_182_woh$fitted.values, type='l', col='red', lty=2, lwd=3)
lines(lm3_112$fitted.values, type='l', col='red', lty=2, lwd=3)
lines(lm4_112$fitted.values, type='l', col='blue', lty=2, lwd=3)
lines(lm5$fitted.values, type='l', col='orange', lty=2, lwd=3)


install.packages('Metrics')

library(Metrics)


mape((lm2_182$fitted.values), Mean_Daily_Data_182$M_tot_wd)


#############Daily_data
abline(v=c(24,48,72,96,120))
par(mfrow=c(2,2))
plot(Mean_Daily_Data_182$M_tot_wd, type='l', col='pink', ylim=c(0,100))
lines(lm1_182$fitted.values, type='l', col='red')
lines(lm2_182$fitted.values, type='l', col='green')
lines(lm3_182$fitted.values, type='l', col='black')
lines(lm5_182$fitted.values, type='l', col='blue')








###################Type 2  #################

Daily_data_type_2 <- Type_2_original_data %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_tot))

#Daily_data <- Daily_data[-c(8)]



#Cluster_3_mean_profile_type_1[c(2,3,4)] <- round(Cluster_3_mean_profile_type_1[c(2,3,4)], digits = 0)


#Daily_data <- Daily_data[order(Daily_data$Day),]


Daily_data_type_2[c(3,4,5)] <- round(Daily_data_type_2[c(3,4,5)], digits = 0)

#plot(x=Cluster_3_mean_profile_type_1$Hour, y=Cluster_3_mean_profile_type_1$M_tot_wd, type='l',xaxt="n", xlab='Hour', ylab="Average movements detected", main='Cluster_3')
#axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), las=2)


#######Linear regression

pairs(Daily_data_type_2[c(3,4)], col=Daily_data_type_2$M_tot_wd)

pairs(Cluster_1_mean_profile_type_1[2:6])

##Multiple linear regression model

Daily_data_type_2 <- Daily_data_type_2[order(Daily_data_type_2$Day),]

lm1 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd, data = Daily_data_type_2)
lm2 <- lm(M_tot_wd~LP_tot_wd, data = Daily_data_type_2)
lm3 <- lm(M_tot_wd~P_tot_wd, data = Daily_data_type_2)
lm4 <- lm(M_tot_wd~LP_tot_wd+P_tot_wd+CO2_avg+RH_avg+Hour, Daily_data_type_2)


#clus_1_lm <- lm(M_tot_wd~LP_tot_wd+P_tot_wd, data = Cluster_1_mean_profile_type_1)
#clus_1_lm2 <- lm(M_tot_wd~LP_tot_wd, data = Cluster_1_mean_profile_type_1)
#clus_1_lm3 <- lm(M_tot_wd~P_tot_wd, data = Cluster_1_mean_profile_type_1)



#clus_2_lm <- lm(M_tot_wd~LP_tot_wd+P_tot_wd, data = Cluster_2_mean_profile_type_1)
#clus_2_lm2 <- lm(M_tot_wd~LP_tot_wd, data = Cluster_2_mean_profile_type_1)
#clus_2_lm3 <- lm(M_tot_wd~P_tot_wd, data = Cluster_2_mean_profile_type_1)

summary(lm1)


plot(predict(lm1), Daily_data$M_tot_wd)
abline(a=0, b=1)


install.packages('Metrics')

library(Metrics)


mape((lm3$fitted.values), Daily_data_type_2$M_tot_wd)


#############Daily_data
abline(v=c(24,48,72,96,120))
par(mfrow=c(2,2))
plot(Daily_data_type_2$M_tot_wd, type='l', col='pink', ylim=c(0,60))
lines(lm1$fitted.values, type='l', col='red')
lines(lm2$fitted.values, type='l', col='green')
lines(lm3$fitted.values, type='l', col='black')
lines(lm4$fitted.values, type='l', col='blue')


write.csv(Daily_data, 'Daily_data.csv')


library(caTools)
split <- sample.split(Daily_data, SplitRatio = 0.75)

train_type1_dailydata <- subset(Daily_data, split=TRUE)
test_type1_dailydata <- subset(Daily_data, split=FALSE)


MOdel <- lm(M_tot_wd~P_tot_wd, data = train_type1_dailydata)
summary(MOdel)


##Prediction
pred <- predict(MOdel, test_type1_dailydata)
pred <- round(pred)

plot(test_type1_dailydata$M_tot_wd, type='l', col='red')
lines(pred, type='l')




######Logistic regression_A_112___________

library(dplyr)

A_112_Nonzerodays <- A_112_Nonzerodays %>%
  mutate(Avg_CO2 = (CO2_LR+CO2_BR1+CO2_BR2+CO2_BR3)/4)

A_112_Nonzerodays <- A_112_Nonzerodays %>%
  mutate(Avg_RH = (RH_LR+RH_BR1+RH_BR2+RH_BR3)/4)


A_112_Nonzerodays <- A_112_Nonzerodays %>%
  mutate(Tot_LP = (LP1+LP2+LP3+LP4+LP5+LP6+LP7+LP8+LP9+LP10+LP11+LP12))


Data_112_Log_Reg <- Daily_Data_112

Data_112_Log_Reg$Occ_prob <- Data_112_Log_Reg$M_tot[which(Data_112_Log_Reg$M_tot>1)] <- 1


Data_112_Log_Reg <- Data_112_Log_Reg[-c(10)]

str(Data_112_Log_Reg)

Data_112_Log_Reg$M_tot <- as.factor(Data_112_Log_Reg$M_tot)

#Data partition

set.seed(1234)

ind_new <- sample(2, nrow(Data_112_Log_Reg), replace = T,  prob = c(0.75,0.25))
train_new <- Data_112_Log_Reg[ind_new==1,]
test_new <- Data_112_Log_Reg[ind_new==2,]


#Logistic regression model

new_glm_model <- glm(M_tot~RH_avg, data = train_new, family = 'binomial')
summary(new_glm_model)

#Prediction

p1_new <- predict(new_glm_model, train_new, type = 'response')
head(p1_new)

summary(p1_new)



optimalCutoff(train_new$M_tot, p1_new)[1]

#Misclassification error - train data

pred1_new <- ifelse(p1_new>0.68, 1,0)
tab1_new <- table(predicted = pred1_new, Actual = train_new$M_tot)
1-sum(diag(tab1_new))/sum(tab1_new)


#Misclassification error - test data
p2_new <- predict(new_glm_model, test_new, type = 'response')
head(p2_new)

summary(p2_new)

optimalCutoff(test_new$M_tot, p2_new)[1]

pred2_new <- ifelse(p2_new>0.68, 1,0)
tab2_new <- table(predicted = pred2_new, Actual = test_new$M_tot)
1-sum(diag(tab2_new))/sum(tab2_new)

plotROC(test_new$M_tot, p2_new)

#Goodness of fit test

with(new_glm_model, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = F))






######Logistic regression_A_122___________

library(dplyr)

A_122_Nonzerodays <- A_122_Nonzerodays %>%
  mutate(Avg_CO2 = (CO2_LR+CO2_BR1+CO2_BR2+CO2_BR3)/4)

A_122_Nonzerodays <- A_122_Nonzerodays %>%
  mutate(Avg_RH = (RH_LR+RH_BR1+RH_BR2+RH_BR3)/4)


A_122_Nonzerodays <- A_122_Nonzerodays %>%
  mutate(Tot_LP = (LP1+LP2+LP3+LP4+LP5+LP6+LP7+LP8+LP9+LP10+LP11+LP12))


Data_122_Log_Reg <- Daily_Data_122

Data_122_Log_Reg$Occ_prob <- Data_122_Log_Reg$M_tot[which(Data_122_Log_Reg$M_tot>1)] <- 1


Data_122_Log_Reg <- Data_122_Log_Reg[-c(9)]

str(Data_122_Log_Reg)

Data_122_Log_Reg$M_tot <- as.factor(Data_122_Log_Reg$M_tot)

#Data partition

set.seed(1234)

ind_new <- sample(2, nrow(Data_122_Log_Reg), replace = T,  prob = c(0.75,0.25))
train_new <- Data_122_Log_Reg[ind_new==1,]
test_new <- Data_122_Log_Reg[ind_new==2,]


#Logistic regression model

new_glm_model <- glm(M_tot~RH_avg, data = train_new, family = 'binomial')
summary(new_glm_model)

#Prediction

p1_new <- predict(new_glm_model, train_new, type = 'response')
head(p1_new)

summary(p1_new)

optimalCutoff(train_new$M_tot, p1_new)[1]

#Misclassification error - train data

pred1_new <- ifelse(p1_new>0.75, 1,0)
tab1_new <- table(predicted = pred1_new, Actual = train_new$M_tot)
1-sum(diag(tab1_new))/sum(tab1_new)


#Misclassification error - test data
p2_new <- predict(new_glm_model, test_new, type = 'response')
head(p2_new)

summary(p2_new)

optimalCutoff(test_new$M_tot, p2_new)[1]

pred2_new <- ifelse(p2_new>0.75, 1,0)
tab2_new <- table(predicted = pred2_new, Actual = test_new$M_tot)
1-sum(diag(tab2_new))/sum(tab2_new)

plotROC(test_new$M_tot, p2_new)

#Goodness of fit test

with(new_glm_model, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = F))


######Logistic regression_A_142___________

library(dplyr)

A_142_Nonzerodays <- A_142_Nonzerodays %>%
  mutate(Avg_CO2 = (CO2_LR+CO2_BR1+CO2_BR2+CO2_BR3)/4)

A_142_Nonzerodays <- A_142_Nonzerodays %>%
  mutate(Avg_RH = (RH_LR+RH_BR1+RH_BR2+RH_BR3)/4)


A_142_Nonzerodays <- A_142_Nonzerodays %>%
  mutate(Tot_LP = (LP1+LP2+LP3+LP4+LP5+LP6+LP7+LP8+LP9+LP10+LP11+LP12))

Daily_Data_142 <- A_142_Nonzerodays[c(1:3,19,34,73:75)]
  
  
  
Data_142_Log_Reg <- Daily_Data_142

Data_142_Log_Reg$Occ_prob <- Data_142_Log_Reg$M_tot[which(Data_142_Log_Reg$M_tot>1)] <- 1


Data_142_Log_Reg <- Data_142_Log_Reg[-c(9)]

str(Data_142_Log_Reg)

Data_142_Log_Reg$M_tot <- as.factor(Data_142_Log_Reg$M_tot)

#Data partition

set.seed(1234)

ind_new <- sample(2, nrow(Data_142_Log_Reg), replace = T,  prob = c(0.75,0.25))
train_new <- Data_142_Log_Reg[ind_new==1,]
test_new <- Data_142_Log_Reg[ind_new==2,]


#Logistic regression model

new_glm_model <- glm(M_tot~Avg_RH, data = train_new, family = 'binomial')
summary(new_glm_model)

#Prediction

p1_new <- predict(new_glm_model, train_new, type = 'response')
head(p1_new)

summary(p1_new)

optimalCutoff(train_new$M_tot, p1_new)[1]

#Misclassification error - train data

pred1_new <- ifelse(p1_new>0.75, 1,0)
tab1_new <- table(predicted = pred1_new, Actual = train_new$M_tot)
1-sum(diag(tab1_new))/sum(tab1_new)


#Misclassification error - test data
p2_new <- predict(new_glm_model, test_new, type = 'response')
head(p2_new)

summary(p2_new)

optimalCutoff(test_new$M_tot, p2_new)[1]

pred2_new <- ifelse(p2_new>0.74, 1,0)
tab2_new <- table(predicted = pred2_new, Actual = test_new$M_tot)
1-sum(diag(tab2_new))/sum(tab2_new)

plotROC(test_new$M_tot, p2_new)

#Goodness of fit test

with(new_glm_model, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = F))






######Logistic regression_A_152___________

library(dplyr)

A_152_Nonzerodays <- A_152_Nonzerodays %>%
  mutate(Avg_CO2 = (CO2_LR+CO2_BR1+CO2_BR2+CO2_BR3)/4)

A_152_Nonzerodays <- A_152_Nonzerodays %>%
  mutate(Avg_RH = (RH_LR+RH_BR1+RH_BR2+RH_BR3)/4)


A_152_Nonzerodays <- A_152_Nonzerodays %>%
  mutate(Tot_LP = (LP1+LP2+LP3+LP4+LP5+LP6+LP7+LP8+LP9+LP10+LP11+LP12))



Data_152_Log_Reg <- Daily_Data_152_LP_PP_CO2_RH

Data_152_Log_Reg$Occ_prob <- Data_152_Log_Reg$M_tot[which(Data_152_Log_Reg$M_tot>1)] <- 1


Data_152_Log_Reg <- Data_152_Log_Reg[-c(9)]

str(Data_152_Log_Reg)

Data_152_Log_Reg$M_tot <- as.factor(Data_152_Log_Reg$M_tot)

#Data partition

set.seed(1234)

ind_new <- sample(2, nrow(Data_152_Log_Reg), replace = T,  prob = c(0.75,0.25))
train_new <- Data_152_Log_Reg[ind_new==1,]
test_new <- Data_152_Log_Reg[ind_new==2,]


#Logistic regression model

new_glm_model <- glm(M_tot~RH_avg, data = train_new, family = 'binomial')
summary(new_glm_model)

#Prediction

p1_new <- predict(new_glm_model, train_new, type = 'response')
head(p1_new)

summary(p1_new)

optimalCutoff(train_new$M_tot, p1_new)[1]

#Misclassification error - train data

pred1_new <- ifelse(p1_new>0.72, 1,0)
tab1_new <- table(predicted = pred1_new, Actual = train_new$M_tot)
1-sum(diag(tab1_new))/sum(tab1_new)


#Misclassification error - test data
p2_new <- predict(new_glm_model, test_new, type = 'response')
head(p2_new)

summary(p2_new)

optimalCutoff(test_new$M_tot, p2_new)[1]

pred2_new <- ifelse(p2_new>0.71, 1,0)
tab2_new <- table(predicted = pred2_new, Actual = test_new$M_tot)
1-sum(diag(tab2_new))/sum(tab2_new)

plotROC(test_new$M_tot, p2_new)

#Goodness of fit test

with(new_glm_model, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = F))



######Logistic regression_A_162___________

library(dplyr)

A_162_Nonzerodays <- A_162_Nonzerodays %>%
  mutate(Avg_CO2 = (CO2_LR+CO2_BR1+CO2_BR2+CO2_BR3)/4)

A_162_Nonzerodays <- A_162_Nonzerodays %>%
  mutate(Avg_RH = (RH_LR+RH_BR1+RH_BR2+RH_BR3)/4)


A_162_Nonzerodays <- A_162_Nonzerodays %>%
  mutate(Tot_LP = (LP1+LP2+LP3+LP4+LP5+LP6+LP7+LP8+LP9+LP10+LP11+LP12))


Data_162_Log_Reg <- Daily_Data_162

Data_162_Log_Reg$Occ_prob <- Data_162_Log_Reg$M_tot[which(Data_162_Log_Reg$M_tot>1)] <- 1


Data_162_Log_Reg <- Data_162_Log_Reg[-c(9)]

str(Data_162_Log_Reg)

Data_162_Log_Reg$M_tot <- as.factor(Data_162_Log_Reg$M_tot)

#Data partition

set.seed(1234)

ind_new <- sample(2, nrow(Data_162_Log_Reg), replace = T,  prob = c(0.75,0.25))
train_new <- Data_162_Log_Reg[ind_new==1,]
test_new <- Data_162_Log_Reg[ind_new==2,]


#Logistic regression model

new_glm_model <- glm(M_tot~RH_avg, data = train_new, family = 'binomial')
summary(new_glm_model)

#Prediction

p1_new <- predict(new_glm_model, train_new, type = 'response')
head(p1_new)

summary(p1_new)

optimalCutoff(train_new$M_tot, p1_new)[1]

#Misclassification error - train data

pred1_new <- ifelse(p1_new>0.63, 1,0)
tab1_new <- table(predicted = pred1_new, Actual = train_new$M_tot)
1-sum(diag(tab1_new))/sum(tab1_new)


#Misclassification error - test data
p2_new <- predict(new_glm_model, test_new, type = 'response')
head(p2_new)

summary(p2_new)

optimalCutoff(test_new$M_tot, p2_new)[1]

pred2_new <- ifelse(p2_new>0.62, 1,0)
tab2_new <- table(predicted = pred2_new, Actual = test_new$M_tot)
1-sum(diag(tab2_new))/sum(tab2_new)

plotROC(test_new$M_tot, p2_new)

#Goodness of fit test

with(new_glm_model, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = F))



######Logistic regression_A_172___________

library(dplyr)

A_172_Nonzerodays <- A_172_Nonzerodays %>%
  mutate(Avg_CO2 = (CO2_LR+CO2_BR1+CO2_BR2+CO2_BR3)/4)

A_172_Nonzerodays <- A_172_Nonzerodays %>%
  mutate(Avg_RH = (RH_LR+RH_BR1+RH_BR2+RH_BR3)/4)


A_172_Nonzerodays <- A_172_Nonzerodays %>%
  mutate(Tot_LP = (LP1+LP2+LP3+LP4+LP5+LP6+LP7+LP8+LP9+LP10+LP11+LP12))


Data_172_Log_Reg <- Daily_Data_172

Data_172_Log_Reg$Occ_prob <- Data_172_Log_Reg$M_tot[which(Data_172_Log_Reg$M_tot>1)] <- 1


Data_172_Log_Reg <- Data_172_Log_Reg[-c(9)]

str(Data_172_Log_Reg)

Data_172_Log_Reg$M_tot <- as.factor(Data_172_Log_Reg$M_tot)

#Data partition

set.seed(1234)

ind_new <- sample(2, nrow(Data_172_Log_Reg), replace = T,  prob = c(0.75,0.25))
train_new <- Data_172_Log_Reg[ind_new==1,]
test_new <- Data_172_Log_Reg[ind_new==2,]


#Logistic regression model

new_glm_model <- glm(M_tot~RH_avg, data = train_new, family = 'binomial')
summary(new_glm_model)

#Prediction

p1_new <- predict(new_glm_model, train_new, type = 'response')
head(p1_new)

summary(p1_new)

##3Optimal cutoff
#install.packages("InformationValue")
library(InformationValue)
optimalCutoff(train_new$M_tot, p1_new)[1]


#Misclassification error - train data

pred1_new <- ifelse(p1_new>0.80, 1,0)
tab1_new <- table(predicted = pred1_new, Actual = train_new$M_tot)
1-sum(diag(tab1_new))/sum(tab1_new)


#Misclassification error - test data
p2_new <- predict(new_glm_model, test_new, type = 'response')
head(p2_new)

summary(p2_new)

optimalCutoff(test_new$M_tot, p2_new)[1]

pred2_new <- ifelse(p2_new>0.80, 1,0)
tab2_new <- table(predicted = pred2_new, Actual = test_new$M_tot)
1-sum(diag(tab2_new))/sum(tab2_new)

#Goodness of fit test

with(new_glm_model, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = F))

install.packages("InformationValue")
library(InformationValue)
a <- optimalCutoff(test_new$M_tot, p2_new)[1]

install.packages("pROC")
library('pROC')
plotROC(test_new$M_tot, p2_new)
#dev.off()



######Logistic regression_A_182___________

library(dplyr)

A_182_Nonzerodays <- A_182_Nonzerodays %>%
  mutate(Avg_CO2 = (CO2_LR+CO2_BR1+CO2_BR2+CO2_BR3)/4)

A_182_Nonzerodays <- A_182_Nonzerodays %>%
  mutate(Avg_RH = (RH_LR+RH_BR1+RH_BR2+RH_BR3)/4)


A_182_Nonzerodays <- A_182_Nonzerodays %>%
  mutate(Tot_LP = (LP1+LP2+LP3+LP4+LP5+LP6+LP7+LP8+LP9+LP10+LP11+LP12))


Data_182_Log_Reg <- Daily_Data_182_LP_PP_CO2_RH

Data_182_Log_Reg$Occ_prob <- Data_182_Log_Reg$motion_total[which(Data_182_Log_Reg$motion_total>1)] <- 1


Data_182_Log_Reg <- Data_182_Log_Reg[-c(16)]

str(Data_182_Log_Reg)

Data_182_Log_Reg$motion_total <- as.factor(Data_182_Log_Reg$motion_total)

#Data partition

set.seed(1234)

ind_new <- sample(2, nrow(Data_182_Log_Reg), replace = T,  prob = c(0.75,0.25))
train_new <- Data_182_Log_Reg[ind_new==1,]
test_new <- Data_182_Log_Reg[ind_new==2,]


#Logistic regression model

new_glm_model <- glm(motion_total~CO2_avg, data = train_new, family = 'binomial')
summary(new_glm_model)

#Prediction

p1_new <- predict(new_glm_model, train_new, type = 'response')
head(p1_new)

summary(p1_new)

optimalCutoff(train_new$motion_total, p1_new)[1]

#Misclassification error - train data

pred1_new <- ifelse(p1_new>0.42, 1,0)
tab1_new <- table(predicted = pred1_new, Actual = train_new$motion_total)
1-sum(diag(tab1_new))/sum(tab1_new)


#Misclassification error - test data
p2_new <- predict(new_glm_model, test_new, type = 'response')
head(p2_new)

summary(p2_new)

optimalCutoff(test_new$motion_total, p2_new)[1]

pred2_new <- ifelse(p2_new>0.42, 1,0)
tab2_new <- table(predicted = pred2_new, Actual = test_new$motion_total)
print(tab2_new)

1-sum(diag(tab2_new))/sum(tab2_new)
plotROC(test_new$M_tot, p2_new)

################LP_182#############

new_glm_model1 <- glm(motion_total~light_total, data = train_new, family = 'binomial')
summary(new_glm_model)

#Prediction

p_LP_new <- predict(new_glm_model1, train_new, type = 'response')
head(p_LP_new)

summary(p_LP_new)

optimalCutoff(train_new$motion_total, p_LP_new)[1]

#Misclassification error - train data

pred_LP_new <- ifelse(p_LP_new>0.68, 1,0)
tab_LP_new <- table(predicted = pred_LP_new, Actual = train_new$motion_total)
1-sum(diag(tab_LP_new))/sum(tab_LP_new)


#Misclassification error - test data
p2_LP_new <- predict(new_glm_model1, test_new, type = 'response')
head(p2_LP_new)

summary(p2_LP_new)

optimalCutoff(test_new$motion_total, p2_LP_new)[1]

pred2_LP_new <- ifelse(p2_LP_new>0.68, 1,0)
tab2_LP_new <- table(predicted = pred2_LP_new, Actual = test_new$motion_total)
1-sum(diag(tab2_LP_new))/sum(tab2_LP_new)


################P_182#############

new_glm_model2 <- glm(motion_total~power_total_1, data = train_new, family = 'binomial')
summary(new_glm_model2)

#Prediction

p_P_new <- predict(new_glm_model2, train_new, type = 'response')
head(p_P_new)

summary(p_P_new)

optimalCutoff(train_new$motion_total, p_P_new)[1]

#Misclassification error - train data

pred_P_new <- ifelse(p_P_new>0.63, 1,0)
tab_P_new <- table(predicted = pred_P_new, Actual = train_new$motion_total)
1-sum(diag(tab_P_new))/sum(tab_P_new)


#Misclassification error - test data
p2_P_new <- predict(new_glm_model2, test_new, type = 'response')
head(p2_P_new)

summary(p2_P_new)

optimalCutoff(test_new$motion_total, p2_P_new)[1]

pred2_P_new <- ifelse(p2_P_new>0.63, 1,0)
tab2_P_new <- table(predicted = pred2_P_new, Actual = test_new$motion_total)
1-sum(diag(tab2_P_new))/sum(tab2_P_new)
print(tab2_P_new)



################RH_182#############

new_glm_model2 <- glm(motion_total~RH_avg, data = train_new, family = 'binomial')
summary(new_glm_model2)

#Prediction

p_RH_new <- predict(new_glm_model2, train_new, type = 'response')
#head(p_P_new)

#summary(p_P_new)

optimalCutoff(train_new$motion_total, p_RH_new)[1]

#Misclassification error - train data

pred_RH_new <- ifelse(p_RH_new>0.70, 1,0)
tab_RH_new <- table(predicted = pred_RH_new, Actual = train_new$motion_total)
1-sum(diag(tab_RH_new))/sum(tab_RH_new)


#Misclassification error - test data
p2_RH_new <- predict(new_glm_model2, test_new, type = 'response')
#head(p2_P_new)

#summary(p2_P_new)

optimalCutoff(test_new$motion_total, p2_RH_new)[1]

pred2_RH_new <- ifelse(p2_RH_new>0.72, 1,0)
tab2_RH_new <- table(predicted = pred2_RH_new, Actual = test_new$motion_total)
1-sum(diag(tab2_RH_new))/sum(tab2_RH_new)




par(mfrow=c(2,2))


fourfoldplot(tab2_LP_new, col=c("red", "orange"), main = 'Model1', margin = 1, space = 0.3, conf.level = 0)
fourfoldplot(tab2_P_new, col=c("red", "orange"), main = 'Model2', margin = 1, space = 0.3, conf.level = 0)
fourfoldplot(tab2_new, col=c("red", "orange"), main = 'Model3', margin = 2, space = 0.3, conf.level = 0)
fourfoldplot(tab2_RH_new, col=c("red", "orange"), main = 'Model4', margin = 2, space = 0.3, conf.level = 0)


plotROC(test_new$motion_total, p2_new)
plotROC(test_new$motion_total, p2_LP_new)
plotROC(test_new$motion_total, p2_P_new)
plotROC(test_new$motion_total, p2_RH_new)


#Goodness of fit test

with(new_glm_model, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = F))

#dev.off()



#ctable <- as.table(matrix(c(223, 135, 88, 1213), nrow = 2, byrow = TRUE))
#fourfoldplot(ctable, color = c("Red", "Black"),
    #         conf.level = 0, margin = 1, main = "Confusion Matrix - Model1 (CO2)")



#library(caret)
## 2 class example
#lvs <- c(0, 1)
#truth <- factor(rep(lvs, times = c(311, 1348)),
                #levels = rev(lvs))
#pred <- factor(
#  c(rep(lvs, times = c(1213, 135)), rep(lvs, times = c(88, 223))), levels = rev(lvs))
#xtab <- table(pred, truth)
#cm <- confusionMatrix(pred, truth)
#cm$table
fourfoldplot(tab2_new, col=c("red", "black"))
