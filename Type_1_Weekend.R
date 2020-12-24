library(TSrepr)
library(ggplot2)
library(data.table)
library(cluster)
library(clusterCrit)

library('dtwclust')

library('TSclust')

library('dplyr')

library('microbenchmark')


library('reshape2')


library(pheatmap)

library(gplots)


Type_1_Weekend_Dayindex_data_matrix <- as.matrix(Type_1_data_weekend_with_Dayindex[c(2:25)])

Type_1_weekend_original_data$daynum <- rep(1:135, each=24)


Type_1_weekend_original_data$daynum <- as.factor(Type_1_weekend_original_data$daynum)

Type_1_weekend_original_data$Hour <- as.integer(Type_1_weekend_original_data$Hour)



library(ggplot2)
ggplot(Type_1_weekend_original_data, aes(x= Hour, y=M_tot, color= daynum)) + geom_line() + labs(x = "Time of day", y= "Total number of movements")+ ggtitle("Type A - Weekends occupant movement data") + theme(legend.position = "none", panel.background = element_blank()+  theme(axis.line = element_line(color = 'black')))

clusterings_we_Type_1_c1 <- lapply(c(2:10), function(x)
  pam((as.matrix(Type_1_Weekend_Dayindex_data_matrix)), x))

DB_values_we_Type_1_c1 <- sapply(seq_along(clusterings_we_Type_1_c1), function(x) 
  intCriteria(Type_1_Weekend_Dayindex_data_matrix, as.integer(clusterings_we_Type_1_c1[[x]]$clustering),
              c("Dunn")))



ggplot(data.table(Clusters = 2:10, Dunn = unlist(DB_values_we_Type_1_c1)),
       aes(Clusters, Dunn)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_bw()


dtw_cluster_we_type_1_c1 = tsclust(Type_1_Weekend_Dayindex_data_matrix, type="partitional",k=2, preproc = zscore,seed=110,
                                   distance="sbd",centroid = "shape",trace=T, na.rm=TRUE)
plot(dtw_cluster_we_type_1_c1)

plot(dtw_cluster_we_type_1_c1, type = "series", clus = 2L)
plot(dtw_cluster_we_type_1_c1, type = "centroids", clus = 2L)


#########################

Type_1_data_weekend_with_Dayindex$cluster <- dtw_cluster_we_type_1_c1@cluster


cluster_1_we <- subset(Type_1_data_weekend_with_Dayindex, Type_1_data_weekend_with_Dayindex$cluster==1)


pheatmap::pheatmap(as.matrix(cluster_1_we[c(2:25)]), treeheight_row = 0, treeheight_col = 0,cluster_rows=F, cluster_cols=F)


cluster_2_we <- subset(Type_1_data_weekend_with_Dayindex, Type_1_data_weekend_with_Dayindex$cluster==2)


pheatmap::pheatmap(as.matrix(cluster_2_we[c(2:25)]), treeheight_row = 0, treeheight_col = 0, cluster_rows=F, cluster_cols=F)


#cluster_3_we <- subset(Type_1_data_weekend_with_Dayindex, Type_1_data_weekend_with_Dayindex$cluster==3)


#pheatmap::pheatmap(as.matrix(cluster_3_we[c(2:25)]), treeheight_row = 0, treeheight_col = 0, cluster_rows=F, cluster_cols=F)


#cluster_4_we <- subset(Type_1_data_weekend_with_Dayindex, Type_1_data_weekend_with_Dayindex$cluster==4)


#pheatmap::pheatmap(as.matrix(cluster_4_we[c(2:25)]), treeheight_row = 0, treeheight_col = 0, cluster_rows=F, cluster_cols=F)



a <- rep(Type_1_data_weekend_with_Dayindex$cluster, each=24)

Type_1_weekend_original_data$cluster <- a

cluster_1_we_OD <- subset(Type_1_weekend_original_data, Type_1_weekend_original_data$cluster==1)

cluster_2_we_OD <- subset(Type_1_weekend_original_data, Type_1_weekend_original_data$cluster==2)

#cluster_3_we_OD <- subset(Type_1_original_data, Type_1_original_data$cluster==3)


#cluster_1_we_OD$daynum <- cumsum(!duplicated(cluster_1_we_OD$Day_index))

cluster_1_we_OD$ID <- rep(1:36, each=24)


cluster_1_we_OD$ID <- as.factor(cluster_1_we_OD$ID)

cluster_1_we_OD$Hour <- as.integer(cluster_1_we_OD$Hour)


library(ggplot2)
ggplot(cluster_1_we_OD, aes(x= Hour, y=M_tot, color= ID)) + geom_line() + labs(x = "Time of day", y= "Total number of movements")+ ggtitle("Cluster_1") + theme(legend.position = "none")



#cluster_2_we_OD$daynum <- cumsum(!duplicated(cluster_2_OD$Day_index))

cluster_2_we_OD$ID <- rep(1:99, each=24)


cluster_2_we_OD$ID <- as.factor(cluster_2_we_OD$ID)

cluster_2_we_OD$Hour <- as.integer(cluster_2_we_OD$Hour)




ggplot(cluster_2_we_OD, aes(x= Hour, y=M_tot, color= ID)) + geom_line() + labs(x = "Time of day", y= "Total number of movements")+ ggtitle("Cluster_2") + theme(legend.position = "none")



#cluster_3_OD$daynum <- cumsum(!duplicated(cluster_3_OD$Day_index))


#cluster_3_OD$daynum <- as.factor(cluster_3_OD$daynum)

#cluster_3_OD$Hour <- as.integer(cluster_3_OD$Hour)

#cluster_3_OD$ID <- rep(1:116, each=24)


library(ggplot2)
#ggplot(cluster_3_OD, aes(x= Hour, y=M_tot, color= daynum)) + geom_line() + labs(x = "Time of day", y= "Total number of movements")+ ggtitle("Cluster_3") + theme(legend.position = "none")


#M_Cluster_1_mean_profile_hourly_analysis

Cluster_1_mean_profile_type_1_we <- cluster_1_we_OD %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_we = mean(LP_tot))

Cluster_1_mean_profile_type_1_we[c(2,3,4)] <- round(Cluster_1_mean_profile_type_1_we[c(2,3,4)], digits = 0)

plot(x=Cluster_1_mean_profile_type_1_we$Hour, y=Cluster_1_mean_profile_type_1_we$M_tot_wd, xaxt="n", xlab='Hour', ylab="Average movements detected", main='Cluster_1', type='l')
axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), las=2)


#a <- cpt.mean(Cluster_1_mean_profile$M_tot_wd ,penalty = "AIC", Q=3, method="BinSeg")
#cpts(a)
#plot(a, xlab="Time(min)",ylab="Occupant count", main ='Sunday', xaxt='n')
#axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)


#M_Cluster_2_mean_profile_hourly_analysis

Cluster_2_mean_profile_type_1_we <- cluster_2_we_OD %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_we = mean(LP_tot))

Cluster_2_mean_profile_type_1_we[c(2,3,4)] <- round(Cluster_2_mean_profile_type_1_we[c(2,3,4)], digits = 0)

plot(x=Cluster_2_mean_profile_type_1_we$Hour, y=Cluster_2_mean_profile_type_1_we$M_tot_wd, type='l',xaxt="n", xlab='Hour', ylab="Average movements detected", main='Cluster_2')
axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), las=2)


## Daily analysis_Cluster_1 ##

Cluster_1_we_Saturday <- filter(cluster_1_we_OD, Day == 'Saturday')

Cluster_1_we_Saturday_mean_profile <- Cluster_1_we_Saturday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_we = mean(LP_tot))

#Cluster_1_Monday_occ_prob <- Cluster_1_Monday

#Cluster_1_Monday_occ_prob$occ_prob <- Cluster_1_Monday_occ_prob$M_tot[which(Cluster_1_Monday_occ_prob$M_tot>1)] <- 1

#c <- Cluster_1_Monday_occ_prob %>%
 # group_by(Day, Hour) %>%
  #summarise(occ_count=length(which(M_tot==1)),
            #occ_prob=length(which(M_tot==1))/length(M_tot))



Cluster_1_we_Saturday_mean_profile[c(2,3,4)] <- round(Cluster_1_we_Saturday_mean_profile[c(2,3,4)], digits = 0)


#Sunday

Cluster_1_we_Sunday <- filter(cluster_1_we_OD, Day == 'Sunday')

Cluster_1_we_Sunday_mean_profile <- Cluster_1_we_Sunday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_we = mean(LP_tot))


Cluster_1_we_Sunday_mean_profile[c(2,3,4)] <- round(Cluster_1_we_Sunday_mean_profile[c(2,3,4)], digits = 0)

par(mfrow=c(3,2))

plot(Cluster_1_we_Saturday_mean_profile$M_tot_wd, type='l', col='blue', ylim=c(0, 100), ylab='Hourly movements detected', xlab='Hours', lwd=3, main='Weekend_Cluster 1')
lines(Cluster_1_we_Sunday_mean_profile$M_tot_wd, type='l', col='orange', lwd=3)

legend(
  "topleft", 
  col = c('Blue', 'Orange'),
  legend = c("Saturday", "Sunday"), lty = 1, bty = 'n', lwd=3)



#### CLUSTER2 ####

## Daily analysis_Cluster_2 ##

Cluster_2_we_Saturday <- filter(cluster_2_we_OD, Day == 'Saturday')

Cluster_2_we_Saturday_mean_profile <- Cluster_2_we_Saturday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_we = mean(LP_tot))

#Cluster_2_Monday_occ_prob <- Cluster_2_Monday

#Cluster_2_Monday_occ_prob$occ_prob <- Cluster_2_Monday_occ_prob$M_tot[which(Cluster_2_Monday_occ_prob$M_tot>1)] <- 1

#c <- Cluster_2_Monday_occ_prob %>%
# group_by(Day, Hour) %>%
#summarise(occ_count=length(which(M_tot==1)),
#occ_prob=length(which(M_tot==1))/length(M_tot))



Cluster_2_we_Saturday_mean_profile[c(2,3,4)] <- round(Cluster_2_we_Saturday_mean_profile[c(2,3,4)], digits = 0)


#Sunday

Cluster_2_we_Sunday <- filter(cluster_2_we_OD, Day == 'Sunday')

Cluster_2_we_Sunday_mean_profile <- Cluster_2_we_Sunday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_we = mean(LP_tot))


Cluster_2_we_Sunday_mean_profile[c(2,3,4)] <- round(Cluster_2_we_Sunday_mean_profile[c(2,3,4)], digits = 0)



plot(Cluster_2_we_Saturday_mean_profile$M_tot_wd, type='l', col='blue', ylim=c(0, 100), ylab='Hourly movements detected', xlab='Hours', lwd=3, main='Weekend_Cluster 2')
lines(Cluster_2_we_Sunday_mean_profile$M_tot_wd, type='l', col='orange', lwd=3)

legend(
  "topleft", 
  col = c('Blue', 'Orange'),
  legend = c("Saturday", "Sunday"), lty = 1, bty = 'n', lwd=3)


##### OCC_PROB ######

## CLUSTER_1####

cluster_1_we_OD_Occ_prob <- cluster_1_we_OD


cluster_1_we_OD_Occ_prob$occ_prob <- cluster_1_we_OD_Occ_prob$M_tot[which(cluster_1_we_OD_Occ_prob$M_tot>1)] <- 1

library(dplyr)

cluster_1_we_OD_Occ_prob_CPA <- cluster_1_we_OD_Occ_prob %>%
  group_by(Day, Hour) %>%
  summarise(occ_count=length(which(M_tot==1)),
            occ_prob=length(which(M_tot==1))/length(M_tot))

q_1 <- cluster_1_we_OD_Occ_prob_CPA[which(cluster_1_we_OD_Occ_prob_CPA$Day=='Saturday'),]
w_1 <- cluster_1_we_OD_Occ_prob_CPA[which(cluster_1_we_OD_Occ_prob_CPA$Day=='Sunday'),]


plot(q_1$occ_prob, type='l', ylim=c(0,1), col='blue')
lines(w_1$occ_prob, type='l', col='orange')

legend(
  "bottomleft", 
  col = c('Blue', 'Orange'),
  legend = c("Saturday", "Sunday"), lty = 1, bty = 'o')


#### CLUSTER_2######

cluster_2_we_OD_Occ_prob <- cluster_2_we_OD


cluster_2_we_OD_Occ_prob$occ_prob <- cluster_2_we_OD_Occ_prob$M_tot[which(cluster_2_we_OD_Occ_prob$M_tot>1)] <- 1

library(dplyr)

cluster_2_we_OD_Occ_prob_CPA <- cluster_2_we_OD_Occ_prob %>%
  group_by(Day, Hour) %>%
  summarise(occ_count=length(which(M_tot==1)),
            occ_prob=length(which(M_tot==1))/length(M_tot))

q_2 <- cluster_2_we_OD_Occ_prob_CPA[which(cluster_2_we_OD_Occ_prob_CPA$Day=='Saturday'),]
w_2 <- cluster_2_we_OD_Occ_prob_CPA[which(cluster_2_we_OD_Occ_prob_CPA$Day=='Sunday'),]


plot(q_2$occ_prob, type='l', ylim=c(0,1), col='blue')
lines(w_2$occ_prob, type='l', col='orange')

legend(
  "bottomleft", 
  col = c('Blue', 'Orange'),
  legend = c("Saturday", "Sunday"), lty = 1, bty = 'o')



with(Cluster_1_mean_profile_type_1_we, plot(Hour, M_tot_wd, type='l', col='red'))

par(new=TRUE)

with(Cluster_1_mean_profile_type_1_we, plot(Hour, P_tot_wd, type='l', pch=16, axes=F,xlab=NA, ylab=NA, cex=1.2))
axis(side=4)
mtext(side=4, line=3, 'P_Tot')
legend("topleft",
       legend=c(expression(M_tot), expression(P_Tot)),
       lty=c(1,0), pch=c(NA, 16), col=c("red", "black"))



with(Cluster_2_mean_profile_type_1_we, plot(Hour, M_tot_wd, type='l', col='red'))

par(new=TRUE)

with(Cluster_2_mean_profile_type_1_we, plot(Hour, LP_tot_we, type='l', pch=16, axes=F,xlab=NA, ylab=NA, cex=1.2))
axis(side=4)
mtext(side=4, line=3, 'P_Tot')
legend("topleft",
       legend=c(expression(M_tot), expression(P_Tot)),
       lty=c(1,0), pch=c(NA, 16), col=c("red", "black"))


##### CPA ######

library('changepoint')

### CLUSTER_1 ######
q_1_CPA <- cpt.meanvar(Type_1_WE_Cluster_1_Average_profile$Average, penalty = "AIC", Q=5, method="BinSeg")
cpts(q_1_CPA)
plot(q_1_CPA, xlab="Time(Hrs)",ylab="Occupant probability", main ='Occupancy probability_Cluster_1', xaxt='n', ylim=c(0,1))
abline(v=c(6))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), las=2)
param.est(q_1_CPA)



### CLUSTER_2 ######

q_2_CPA <- cpt.meanvar(Type_1_WE_Cluster_2_Average_profile$Average, penalty = "AIC", Q=5, method="BinSeg")
cpts(q_2_CPA)
plot(q_2_CPA, xlab="Time(Hrs)",ylab="Occupant probability", main ='Occupancy probability_Cluster_2', xaxt='n', ylim=c(0,1))
abline(v=c(4,6,11,19))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), las=2)
param.est(q_2_CPA)


#### Plug load


### CLUSTER_1 ######
PP_q_1_CPA <- cpt.mean(Cluster_1_mean_profile_type_1_we$P_tot_wd, penalty = "AIC", Q=4, method="BinSeg")
cpts(PP_q_1_CPA)
plot(PP_q_1_CPA, xlab="Time(Hrs)",ylab="Average energy consumption (Wh)", main ='Plug_load_Cluster_1', xaxt='n')
abline(v=c(7,12,14,19))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), las=2)
param.est(PP_q_1_CPA)


OCC_COUNT_q_1_CPA <- cpt.mean(Cluster_1_mean_profile_type_1_we$M_tot_wd, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_COUNT_q_1_CPA)
plot(OCC_COUNT_q_1_CPA, xlab="Time(Hrs)",ylab="Average occupancy movement count", main ='Occupancy activity_Cluster_1_WE', xaxt='n')
abline(v=c(7,9,21))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), las=2)
param.est(OCC_COUNT_q_1_CPA)

LP_q_1_CPA <- cpt.mean(Cluster_1_mean_profile_type_1_we$LP_tot_we, penalty = "AIC", Q=4, method="BinSeg")
cpts(LP_q_1_CPA)
plot(LP_q_1_CPA, xlab="Time(Hrs)",ylab="Average energy consumption (Wh)", main ='Plug_load_Cluster_1', xaxt='n')
abline(v=c(7,9,11,18))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), las=2)
param.est(LP_q_1_CPA)



### CLUSTER_2 ######

PP_q_2_CPA <- cpt.mean(Cluster_2_mean_profile_type_1_we$P_tot_wd, penalty = "AIC", Q=4, method="BinSeg")
cpts(PP_q_2_CPA)
plot(PP_q_2_CPA, xlab="Time(Hrs)",ylab="Average occupancy movement count", main ='Cluster_2', xaxt='n')
abline(v=c(7,11,19))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), las=2)
param.est(PP_q_2_CPA)

OCC_COUNT_q_2_CPA <- cpt.mean(Cluster_2_mean_profile_type_1_we$M_tot_wd, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_COUNT_q_2_CPA)
plot(OCC_COUNT_q_2_CPA, xlab="Time(Hrs)",ylab="Average occupancy movement count", main ='Occupancy activity_Cluster_2_WE', xaxt='n')
abline(v=c(7,11,18))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), las=2)
param.est(OCC_COUNT_q_2_CPA)

LP_q_2_CPA <- cpt.mean(Cluster_2_mean_profile_type_1_we$LP_tot_we, penalty = "AIC", Q=3, method="BinSeg")
cpts(LP_q_2_CPA)
plot(LP_q_2_CPA, xlab="Time(Hrs)",ylab="Average occupancy movement count", main ='Cluster_2', xaxt='n')
abline(v=c(7,11,19))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), las=2)
param.est(LP_q_2_CPA)


###########################


Type_1_weekend_original_data$Day_index <- as.character(Type_1_weekend_original_data$Day_index)

Type_1_weekend_original_data$Day_index <- as.POSIXct(Type_1_weekend_original_data$Day_index)

library('dplyr')

Type_1_weekend_original_data <- Type_1_weekend_original_data %>%
  mutate(Month = format(Type_1_weekend_original_data$Day_index, "%m"))

Type_1_weekend_original_data$season <- Func.season(Type_1_weekend_original_data$Month)




##Test the cpt_evaluation function and cpt_count function
Cluster_1_CPA_Jun <- cpt_evaluation(cluster_1_we_OD[c(10,5)], num_cpt = 3,cpt_method = 'mean', penalty_method = 'AIC')


Cluster_1_CPA_RF <- subset(Cluster_1_CPA_Jun, select = c(2:4))

#write.csv(M_112_RF, 'M_112_RF.csv')

#M_112_RF <- M_112_RF[c(-1)]

#M_112_RF <- as.matrix(M_112_RF)

rel_freq_daily <- table(Cluster_1_CPA_RF)

freq_daily_we_clus1 <- table(Cluster_1_CPA_RF)/length(Cluster_1_CPA_RF)

barplot(freq_daily_we_clus1, xlab = "Time of the day (hrs)", ylab = "Frequency", col = "orange", main = "Cluster_1 - Weekdays - Type-1", las=2) 
box()

abline(h=0.06)

freq_daily_we_clus1 <- as.data.frame(freq_daily_we_clus1)

write.csv(freq_daily_we_clus1, 'freq_daily_we_clus1.csv')

##Test the cpt_evaluation function and cpt_count function
Cluster_2_CPA_Jun <- cpt_evaluation(cluster_2_we_OD[c(10,5)], num_cpt = 3,cpt_method = 'mean', penalty_method = 'AIC')

Cluster_2_CPA_RF <- subset(Cluster_2_CPA_Jun, select = c(2:4))

#write.csv(M_112_RF, 'M_112_RF.csv')

#M_112_RF <- M_112_RF[c(-1)]

#M_112_RF <- as.matrix(M_112_RF)

rel_freq_daily_Clus_2 <- table(Cluster_2_CPA_RF)

freq_daily_we_Clus_2 <- table(Cluster_2_CPA_RF)/length(Cluster_2_CPA_RF)

barplot(freq_daily_we_Clus_2, xlab = "Time of the day (hrs)", ylab = "Frequency", col = "green", main = "Cluster_2 - Weekdays - Type-1", las=2) 
box()
abline(h=0.075)

freq_daily_we_Clus_2 <- as.data.frame(freq_daily_we_Clus_2)
write.csv(freq_daily_we_Clus_2, 'freq_daily_we_Clus_2.csv')

library(dplyr)

Daily_data_WE <- Type_1_weekend_original_data %>%
  
  group_by(Hour,Day) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_tot), CO2_avg = mean(Avg_CO2), RH_avg = mean(Avg_RH))

Daily_data_WE[c(3:7)] <- round(Daily_data_WE[c(3:7)], digits = 0)

write.csv(Daily_data_WE, 'Daily_data_WE.csv')

#Daily_data <- Daily_data[-c(8)]

#dev.off()
##########Occ_activity#################

OCC_sat_1_CPA <- cpt.mean(Cluster_1_we_Saturday_mean_profile$M_tot_wd, penalty = "AIC", Q=5, method="BinSeg")
cpts(OCC_sat_1_CPA)
plot(OCC_sat_1_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_1', xaxt='n')
abline(v=c(7,10,14,21), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_sat_1_CPA)

plot(Cluster_1_mean_profile_type_1$Norm_PP, type='l')


OCC_sun_1_CPA <- cpt.mean(Cluster_1_we_Sunday_mean_profile$M_tot_wd, penalty = "AIC", Q=6, method="BinSeg")
cpts(OCC_sun_1_CPA)
plot(OCC_sun_1_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_1', xaxt='n')
abline(v=c(6,10, 16,19, 21), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_sun_1_CPA)



OCC_sat_2_CPA <- cpt.mean(Cluster_2_we_Saturday_mean_profile$M_tot_wd, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_sat_2_CPA)
plot(OCC_sat_2_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_1', xaxt='n')
abline(v=c(7,11,18), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_sat_2_CPA)

#plot(Cluster_1_mean_profile_type_1$Norm_PP, type='l')


OCC_sun_2_CPA <- cpt.mean(Cluster_2_we_Sunday_mean_profile$M_tot_wd, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_sun_2_CPA)
plot(OCC_sun_2_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_1', xaxt='n')
abline(v=c(7,11,19), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_sun_2_CPA)


##################

par(mfrow=c(2,2))
#dev.off()
plot(x=WE_Cluster_1_Daily_occ_presence_prob_Weekend$Hrs, y=WE_Cluster_1_Daily_occ_presence_prob_Weekend$Saturday, type='l', col='blue', ylab='Normalized occupant actiity', xlab='Hours', lwd=3, ylim=c(0,1), main='Cluster 1')
lines(x=WE_Cluster_1_Daily_occ_presence_prob_Weekend$Hrs, y=WE_Cluster_1_Daily_occ_presence_prob_Weekend$Sunday, type='l', col='orange', ylab='OCC actiity', xlab='Hours', lwd=3, ylim=c(0,1),main='Tuesday')
legend(
  "topleft", 
  col = c('Blue', 'Orange'),
  legend = c("Saturday", "Saunday"), lty = 1, bty = 'n', cex = 1.0, lwd=3)




plot(x=WE_Cluster_2_Daily_occ_presence_prob_Weekend$Hrs, y=WE_Cluster_2_Daily_occ_presence_prob_Weekend$Saturday, type='l', col='blue', ylab='Normalized occupant actiity', xlab='Hours', lwd=3, ylim=c(0,1), main='Cluster 2')
lines(x=WE_Cluster_2_Daily_occ_presence_prob_Weekend$Hrs, y=WE_Cluster_2_Daily_occ_presence_prob_Weekend$Sunday, type='l', col='orange', ylab='OCC actiity', xlab='Hours', lwd=3, ylim=c(0,1),main='Tuesday')
legend(
  "topleft", 
  col = c('Blue', 'Orange'),
  legend = c("Saturday", "Saunday"), lty = 1, bty = 'n', cex = 0.9, lwd=3)




#######OCCUPANTS PROBABILITY###############

###########################

OCC_prob_Sat_1_CPA <- cpt.meanvar(q_1$occ_prob, penalty = "AIC", Q=6, method="BinSeg")
cpts(OCC_prob_Sat_1_CPA)
plot(OCC_prob_Sat_1_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_1, Monday', xaxt='n', ylim=c(0,1))
abline(v=c(12), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Sat_1_CPA)

plot(Cluster_1_mean_profile_type_1$Norm_PP, type='l')


OCC_prob_Sun_1_CPA <- cpt.meanvar(w_1$occ_prob, penalty = "AIC", Q=6, method="BinSeg")
cpts(OCC_prob_Sun_1_CPA)
plot(OCC_prob_Sun_1_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_1, Tuesday', xaxt='n', ylim=c(0,1))
abline(v=c(6,9,11,20), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Sun_1_CPA)


OCC_prob_Sat_2_CPA <- cpt.meanvar(q_2$occ_prob, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_prob_Sat_2_CPA)
plot(OCC_prob_Sat_2_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_1, Monday', xaxt='n', ylim=c(0,1))
abline(v=c(4,12,19), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Sat_2_CPA)

plot(Cluster_1_mean_profile_type_1$Norm_PP, type='l')


OCC_prob_Sun_2_CPA <- cpt.meanvar(w_2$occ_prob, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_prob_Sun_2_CPA)
plot(OCC_prob_Sun_2_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_1, Tuesday', xaxt='n', ylim=c(0,1))
abline(v=c(6,11,19), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Sun_2_CPA)



######################

par(mfrow=c(1,2))
#dev.off()
plot(x=Cluster_1_WE_presence$Hours, y=Cluster_1_WE_presence$Saturday, type='l', col='blue', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1), main='Cluster 1')
lines(x=Cluster_1_WE_presence$Hours, y=Cluster_1_WE_presence$Sunday, type='l', col='orange', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1),main='Tuesday')
legend(
  "bottomright", 
  col = c('Blue', 'Orange'),
  legend = c("Saturday", "Saunday"), lty = 1, bty = 'n', cex = 1.0, lwd=3)




plot(x=Cluster_2_WE_presence$Hours, y=Cluster_2_WE_presence$Saturday, type='l', col='blue', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1), main='Cluster 2')
lines(x=Cluster_2_WE_presence$Hours, y=Cluster_2_WE_presence$Sunday, type='l', col='orange', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1),main='Tuesday')
legend(
  "bottomleft", 
  col = c('Blue', 'Orange'),
  legend = c("Saturday", "Saunday"), lty = 1, bty = 'n', cex = 1.0, lwd=3)

