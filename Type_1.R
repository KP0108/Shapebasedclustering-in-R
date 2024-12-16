library('TSrepr')
library('ggplot2')
library('data.table')
library('cluster')
library('clusterCrit')
library('dtwclust')
library('TSclust')
library('dplyr')
library('microbenchmark')
library('reshape2')
library('pheatmap')
library('gplots')
library('ColorBrewer')


Type_1_Dayindex_data_matrix <- as.matrix(Type_1_data_with_Dayindex[c(2:25)])

clusterings_wd_Type_1_c1 <- lapply(c(2:10), function(x)
  pam((as.matrix(Type_1_Dayindex_data_matrix)), x))

Type_1_original_data$daynum <- rep(1:356, each=24)


Type_1_original_data$daynum <- as.factor(Type_1_original_data$daynum)

Type_1_original_data$Hour <- as.integer(Type_1_original_data$Hour)



library(ggplot2)
ggplot(Type_1_original_data, aes(x= Hour, y=M_tot, color= daynum)) + geom_line() + labs(x = "Time of day", y= "Total number of movements")+ ggtitle("Type A - Weekdays occupant movement data") + theme(legend.position = "none", panel.background = element_blank()+  theme(axis.line = element_line(color = 'black')))




#DB_values_wd_Type_1_c1$dunn[1] <- 0.0852093 

DB_values_wd_Type_1_c1 <- sapply(seq_along(clusterings_wd_Type_1_c1), function(x) 
  intCriteria(Type_1_Dayindex_data_matrix, as.integer(clusterings_wd_Type_1_c1[[x]]$clustering),
              c("Dunn")))



ggplot(data.table(Clusters = 2:10, Dunn_index = unlist(DB_values_wd_Type_1_c1)),
       aes(Clusters, Dunn_index)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_bw() + ggtitle("CVI")


dtw_cluster_wd_type_1_c1 = tsclust(Type_1_Dayindex_data_matrix, type="partitional",k=3, preproc = zscore,seed=110,
                                   distance="sbd",centroid = "shape",trace=T, na.rm=TRUE)
plot(dtw_cluster_wd_type_1_c1)

plot(dtw_cluster_wd_type_1_c1, type = "series", clus = 2L)
plot(dtw_cluster_wd_type_1_c1, type = "centroids", clus = 1L)+xlab('Hours')+ylab('Z-normalized value')+ggtitle("Cluster 1")
plot(dtw_cluster_wd_type_1_c1, type = "centroids", clus = 2L)+xlab('Hours')+ylab('Z-normalized value')+ggtitle("Cluster 2")
plot(dtw_cluster_wd_type_1_c1, type = "centroids", clus = 3L)+xlab('Hours')+ylab('Z-normalized value')+ggtitle("Cluster 3")

Type_1_data_with_Dayindex$cluster <- dtw_cluster_wd_type_1_c1@cluster


write.csv(Type_1_data_with_Dayindex, 'Type_1_data_with_Dayindex.csv')


cluster_1 <- subset(Type_1_data_with_Dayindex, Type_1_data_with_Dayindex$cluster==1)

cluster_1 <- cluster_1[-c(46, 51, 52), ] #(Removing occupant movements with 60 and 120)


colors<-colorRampPalette(rev(brewer.pal(n=7,name="Spectral")))(7)

#dev.off()

library(grid)

cluster_1 <- cluster_1[-c(46, 51, 52), ]
pheatmap::pheatmap(as.matrix(cluster_1[c(2:25)]), treeheight_row = 0, treeheight_col = 0,
                   cluster_rows=F, cluster_cols=F, col=colors, legend = TRUE, legend_labels = 'No.of.occupants', main = 'Cluster 1', xlab = 'Hour', cex=1.2)


#########


cluster_2 <- subset(Type_1_data_with_Dayindex, Type_1_data_with_Dayindex$cluster==2)

cluster_2 <- cluster_2[-c(127), ]


pheatmap::pheatmap(as.matrix(cluster_2[c(2:25)]), treeheight_row = 0, treeheight_col = 0, cluster_rows=F, cluster_cols=F, col=colors, main = 'Cluster 2', cex=1.2)


cluster_3 <- subset(Type_1_data_with_Dayindex, Type_1_data_with_Dayindex$cluster==3)

cluster_3 <- cluster_3[-c(85), ]


pheatmap::pheatmap(as.matrix(cluster_3[c(2:25)]), treeheight_row = 0, treeheight_col = 0, cluster_rows=F, cluster_cols=F, col=colors, main = 'Cluster 3', cex=1.2)




a <- rep(Type_1_data_with_Dayindex$cluster, each=24)

Type_1_original_data$cluster <- a

write.csv(Type_1_original_data, 'Type_1_original_data.csv')

cluster_1_OD <- subset(Type_1_original_data_With_LP_CO2_Rh, Type_1_original_data_With_LP_CO2_Rh$cluster==1)

cluster_2_OD <- subset(Type_1_original_data_With_LP_CO2_Rh, Type_1_original_data_With_LP_CO2_Rh$cluster==2)

cluster_3_OD <- subset(Type_1_original_data_With_LP_CO2_Rh, Type_1_original_data_With_LP_CO2_Rh$cluster==3)


#cluster_1_OD$daynum <- cumsum(!duplicated(cluster_1_OD$Day_index))

cluster_1_OD$ID <- rep(1:102, each=24)


cluster_1_OD$ID <- as.factor(cluster_1_OD$ID)

cluster_1_OD$Hour <- as.integer(cluster_1_OD$Hour)


library(ggplot2)
ggplot(cluster_1_OD, aes(x= Hour, y=M_tot, color= ID)) + geom_line() + labs(x = "Time of day", y= "Total number of movements")+ ggtitle("Cluster_1") + theme(legend.position = "none")



cluster_2_OD$daynum <- cumsum(!duplicated(cluster_2_OD$Day_index))


cluster_2_OD$ID <- as.factor(cluster_2_OD$ID)

cluster_2_OD$Hour <- as.integer(cluster_2_OD$Hour)


cluster_2_OD$ID <- rep(1:138, each=24)

ggplot(cluster_2_OD, aes(x= Hour, y=M_tot, color= ID)) + geom_line() + labs(x = "Time of day", y= "Total number of movements")+ ggtitle("Cluster_2") + theme(legend.position = "none")



cluster_3_OD$daynum <- cumsum(!duplicated(cluster_3_OD$Day_index))


cluster_3_OD$ID <- as.factor(cluster_3_OD$ID)

cluster_3_OD$Hour <- as.integer(cluster_3_OD$Hour)

cluster_3_OD$ID <- rep(1:116, each=24)


library(ggplot2)
ggplot(cluster_3_OD, aes(x= Hour, y=M_tot, color= ID)) + geom_line() + labs(x = "Time of day", y= "Total number of movements")+ ggtitle("Cluster_3") + theme(legend.position = "none")


#M_Cluster_1_mean_profile_hourly_analysis

Cluster_1_mean_profile_type_1 <- cluster_1_OD %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_tot), CO2_avg = mean(CO2), RH_avg = mean(RH))

Cluster_1_mean_profile_type_1[c(2,3,4,5,6)] <- round(Cluster_1_mean_profile_type_1[c(2,3,4,5,6)], digits = 0)

plot(x=Cluster_1_mean_profile_type_1$Hour, y=Cluster_1_mean_profile_type_1$CO2_avg, xaxt="n", xlab='Hour', ylab="Average movements detected", main='Cluster_1', type='l')
axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), las=2)


#a <- cpt.mean(Cluster_1_mean_profile$M_tot_wd ,penalty = "AIC", Q=3, method="BinSeg")
#cpts(a)
#plot(a, xlab="Time(min)",ylab="Occupant count", main ='Sunday', xaxt='n')
#axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)


#M_Cluster_2_mean_profile_hourly_analysis

Cluster_2_mean_profile_type_1 <- cluster_2_OD %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_tot), CO2_avg = mean(CO2), RH_avg = mean(RH))

Cluster_2_mean_profile_type_1[c(2,3,4,5,6)] <- round(Cluster_2_mean_profile_type_1[c(2,3,4,5,6)], digits = 0)

plot(x=Cluster_2_mean_profile_type_1$Hour, y=Cluster_2_mean_profile_type_1$CO2_avg, type='l',xaxt="n", xlab='Hour', ylab="Average movements detected", main='Cluster_2')
axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), las=2)


#M_Cluster_3_mean_profile_hourly_analysis

Cluster_3_mean_profile_type_1 <- cluster_3_OD %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_tot), CO2_avg = mean(CO2), RH_avg = mean(RH))

Cluster_3_mean_profile_type_1[c(2,3,4,5,6)] <- round(Cluster_3_mean_profile_type_1[c(2,3,4,5,6)], digits = 0)

plot(x=Cluster_3_mean_profile_type_1$Hour, y=Cluster_3_mean_profile_type_1$CO2_avg, type='l',xaxt="n", xlab='Hour', ylab="Average movements detected", main='Cluster_3')
axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), las=2)



## Daily analysis_Cluster_1 ##

Cluster_1_Monday <- filter(cluster_1_OD, Day == 'Monday')

Cluster_1_Monday_mean_profile <- Cluster_1_Monday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot), LP_tot_wd = mean(LP_tot), CO2_avg = mean(CO2), RH_avg = mean(RH))

#Cluster_1_Monday_occ_prob <- Cluster_1_Monday

#Cluster_1_Monday_occ_prob$occ_prob <- Cluster_1_Monday_occ_prob$M_tot[which(Cluster_1_Monday_occ_prob$M_tot>1)] <- 1

#c <- Cluster_1_Monday_occ_prob %>%
  #group_by(Day, Hour) %>%
  #summarise(occ_count=length(which(M_tot==1)),
           # occ_prob=length(which(M_tot==1))/length(M_tot))



Cluster_1_Monday_mean_profile[c(2,3,4,5,6)] <- round(Cluster_1_Monday_mean_profile[c(2,3,4,5,6)], digits = 0)

#Tuesday
Cluster_1_Tuesday <- filter(cluster_1_OD, Day == 'Tuesday')

Cluster_1_Tuesday_mean_profile <- Cluster_1_Tuesday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot), CO2_avg = mean(CO2), RH_avg = mean(RH))


Cluster_1_Tuesday_mean_profile[c(2,3,4,5,6)] <- round(Cluster_1_Tuesday_mean_profile[c(2,3,4,5,6)], digits = 0)


#Cluster_1_Tuesday_occ_prob <- Cluster_1_Tuesday

#Cluster_1_Tuesday_occ_prob$occ_prob <- Cluster_1_Tuesday_occ_prob$M_tot[which(Cluster_1_Tuesday_occ_prob$M_tot>1)] <- 1

#d <- Cluster_1_Tuesday_occ_prob %>%
#  group_by(Day, Hour) %>%
 # summarise(occ_count=length(which(M_tot==1)),
            #occ_prob=length(which(M_tot==1))/length(M_tot))



#Wednesday
Cluster_1_Wednesday <- filter(cluster_1_OD, Day == 'Wednesday')

Cluster_1_Wednesday_mean_profile <- Cluster_1_Wednesday %>%
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot), CO2_avg = mean(CO2), RH_avg = mean(RH))


Cluster_1_Wednesday_mean_profile[c(2,3,4,5,6)] <- round(Cluster_1_Wednesday_mean_profile[c(2,3,4,5,6)], digits = 0)

#Thursday
Cluster_1_Thursday <- filter(cluster_1_OD, Day == 'Thursday')

Cluster_1_Thursday_mean_profile <- Cluster_1_Thursday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot),CO2_avg = mean(CO2), RH_avg = mean(RH))


Cluster_1_Thursday_mean_profile[c(2,3,4,5,6)] <- round(Cluster_1_Thursday_mean_profile[c(2,3,4,5,6)], digits = 0)


#Friday

Cluster_1_Friday <- filter(cluster_1_OD, Day == 'Friday')

Cluster_1_Friday_mean_profile <- Cluster_1_Friday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot),CO2_avg = mean(CO2), RH_avg = mean(RH))


Cluster_1_Friday_mean_profile[c(2,3,4,5,6)] <- round(Cluster_1_Friday_mean_profile[c(2,3,4,5,6)], digits = 0)

par(mfrow=c(1,3))

plot(Cluster_1_Monday_mean_profile$P_tot_wd, type='l', col='blue', ylim=c(0,70), ylab='Occupant movements detected', xlab='Hours', lwd=3, main ='Cluster 1', cex.axis=2, cex.lab=2, cex.main=3)
#,xaxt='n')
#axis(1,c(0,2,4,6,8,10,12,14,16,18,20,22,24))
lines(Cluster_1_Tuesday_mean_profile$M_tot_wd, type='l', col='orange', lwd=3)
lines(Cluster_1_Wednesday_mean_profile$M_tot_wd, type='l', col='green', lwd=3)
lines(Cluster_1_Thursday_mean_profile$M_tot_wd, type='l', col='red', lwd=3)
lines(Cluster_1_Friday_mean_profile$M_tot_wd, type='l', col='black', lwd=3)
legend(
  "topleft", 
  col = c('Blue', 'Orange', 'Green', 'Red', 'Black'),
  legend = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), lty = 1, bty = 'n', cex = 2, lwd=3)


## Daily analysis_Cluster_2 ##

Cluster_2_Monday <- filter(cluster_2_OD, Day == 'Monday')

Cluster_2_Monday_mean_profile <- Cluster_2_Monday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_2_Monday_mean_profile[c(2,3,4)] <- round(Cluster_2_Monday_mean_profile[c(2,3,4)], digits = 0)



#Cluster_2_Monday_occ_prob <- Cluster_2_Monday


#Cluster_2_Monday_occ_prob$occ_prob <- Cluster_2_Monday_occ_prob$M_tot[which(Cluster_2_Monday_occ_prob$M_tot>1)] <- 1

#c_1 <- Cluster_2_Monday_occ_prob %>%
  #group_by(Day, Hour) %>%
  #summarise(occ_count=length(which(M_tot==1)),
            #occ_prob=length(which(M_tot==1))/length(M_tot))


#Tuesday
Cluster_2_Tuesday <- filter(cluster_2_OD, Day == 'Tuesday')

Cluster_2_Tuesday_mean_profile <- Cluster_2_Tuesday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_2_Tuesday_mean_profile[c(2,3,4)] <- round(Cluster_2_Tuesday_mean_profile[c(2,3,4)], digits = 0)

#################


#Cluster_2_Tuesday_occ_prob <- Cluster_2_Tuesday

#Cluster_2_Tuesday_occ_prob$occ_prob <- Cluster_2_Tuesday_occ_prob$M_tot[which(Cluster_2_Tuesday_occ_prob$M_tot>1)] <- 1

#d_1 <- Cluster_2_Tuesday_occ_prob %>%
 # group_by(Day, Hour) %>%
  #summarise(occ_count=length(which(M_tot==1)),
          #  occ_prob=length(which(M_tot==1))/length(M_tot))

#Wednesday
Cluster_2_Wednesday <- filter(cluster_2_OD, Day == 'Wednesday')

Cluster_2_Wednesday_mean_profile <- Cluster_2_Wednesday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_2_Wednesday_mean_profile[c(2,3,4)] <- round(Cluster_2_Wednesday_mean_profile[c(2,3,4)], digits = 0)

#Thursday
Cluster_2_Thursday <- filter(cluster_2_OD, Day == 'Thursday')

Cluster_2_Thursday_mean_profile <- Cluster_2_Thursday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_2_Thursday_mean_profile[c(2,3,4)] <- round(Cluster_2_Thursday_mean_profile[c(2,3,4)], digits = 0)


#Friday

Cluster_2_Friday <- filter(cluster_2_OD, Day == 'Friday')

Cluster_2_Friday_mean_profile <- Cluster_2_Friday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_2_Friday_mean_profile[c(2,3,4)] <- round(Cluster_2_Friday_mean_profile[c(2,3,4)], digits = 0)



plot(Cluster_2_Monday_mean_profile$M_tot_wd, type='l', col='blue', ylim=c(0, 70), ylab='Occupant movements detected', lwd=3, main='Cluster 2', xlab='Hours',cex.axis=2, cex.lab=2, cex.main=3)
#, xaxt='n')
#axis(1,c(0,2,4,6,8,10,12,14,16,18,20,22,24))
lines(Cluster_2_Tuesday_mean_profile$M_tot_wd, type='l', col='orange', lwd=3)
lines(Cluster_2_Wednesday_mean_profile$M_tot_wd, type='l', col='green', lwd=3)
lines(Cluster_2_Thursday_mean_profile$M_tot_wd, type='l', col='red', lwd=3)
lines(Cluster_2_Friday_mean_profile$M_tot_wd, type='l', col='black', lwd=3)
legend(
  "topright", 
  col = c('Blue', 'Orange', 'Green', 'Red', 'Black'),
  legend = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), lty = 1, bty = 'n', cex = 2, lwd=3)


## Daily analysis_Cluster_3 ##

Cluster_3_Monday <- filter(cluster_3_OD, Day == 'Monday')

Cluster_3_Monday_mean_profile <- Cluster_3_Monday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_3_Monday_mean_profile[c(2,3,4)] <- round(Cluster_3_Monday_mean_profile[c(2,3,4)], digits = 0)

#Tuesday
Cluster_3_Tuesday <- filter(cluster_3_OD, Day == 'Tuesday')

Cluster_3_Tuesday_mean_profile <- Cluster_3_Tuesday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_3_Tuesday_mean_profile[c(2,3,4)] <- round(Cluster_3_Tuesday_mean_profile[c(2,3,4)], digits = 0)


#Cluster_3_Tuesday_mean_profile[c(2,3)] <- round(Cluster_3_Tuesday_mean_profile[c(2,3)], digits = 0)


#Cluster_3_Tuesday_occ_prob <- Cluster_3_Tuesday

#Cluster_3_Tuesday_occ_prob$occ_prob <- Cluster_3_Tuesday_occ_prob$M_tot[which(Cluster_3_Tuesday_occ_prob$M_tot>1)] <- 1

#d_2 <- Cluster_1_Tuesday_occ_prob %>%
 # group_by(Day, Hour) %>%
  #summarise(occ_count=length(which(M_tot==1)),
            #occ_prob=length(which(M_tot==1))/length(M_tot))


#Wednesday
Cluster_3_Wednesday <- filter(cluster_3_OD, Day == 'Wednesday')

Cluster_3_Wednesday_mean_profile <- Cluster_3_Wednesday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_3_Wednesday_mean_profile[c(2,3,4)] <- round(Cluster_3_Wednesday_mean_profile[c(2,3,4)], digits = 0)

#Thursday
Cluster_3_Thursday <- filter(cluster_3_OD, Day == 'Thursday')

Cluster_3_Thursday_mean_profile <- Cluster_3_Thursday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_3_Thursday_mean_profile[c(2,3,4)] <- round(Cluster_3_Thursday_mean_profile[c(2,3,4)], digits = 0)


#Friday

Cluster_3_Friday <- filter(cluster_3_OD, Day == 'Friday')

Cluster_3_Friday_mean_profile <- Cluster_3_Friday %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot),LP_tot_wd = mean(LP_tot))


Cluster_3_Friday_mean_profile[c(2,3,4)] <- round(Cluster_3_Friday_mean_profile[c(2,3,4)], digits = 0)



plot(Cluster_3_Monday_mean_profile$M_tot_wd, type='l', col='blue', ylim=c(0, 70), ylab='Occupant movements detected', xlab='Hours', main='Cluster 3', lwd=3, cex.axis=2, cex.lab=2, cex.main=3)
#,xaxt='n')
#axis(1,c(0,2,4,6,8,10,12,14,16,18,20,22,24))
lines(Cluster_3_Tuesday_mean_profile$M_tot_wd, type='l', col='orange', lwd=3)
lines(Cluster_3_Wednesday_mean_profile$M_tot_wd, type='l', col='green', lwd=3)
lines(Cluster_3_Thursday_mean_profile$M_tot_wd, type='l', col='red', lwd=3)
lines(Cluster_3_Friday_mean_profile$M_tot_wd, type='l', col='black', lwd=3)
legend(
  "topleft", 
  col = c('Blue', 'Orange', 'Green', 'Red', 'Black'),
  legend = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), lty = 1, bty = 'n', cex = 2, lwd=3)



library(changepoint)

### CPA ####

##Test the cpt_evaluation function and cpt_count function
Cluster_1_CPA_Jun <- cpt_evaluation(cluster_1_OD[c(11,6)], num_cpt = 3,cpt_method = 'mean', penalty_method = 'AIC')


Cluster_1_CPA_RF <- subset(Cluster_1_CPA_Jun, select = c(2:4))

#write.csv(M_112_RF, 'M_112_RF.csv')

#M_112_RF <- M_112_RF[c(-1)]

#M_112_RF <- as.matrix(M_112_RF)

rel_freq_daily <- table(Cluster_1_CPA_RF)

freq_daily <- table(Cluster_1_CPA_RF)/length(Cluster_1_CPA_RF)

freq_daily <- as.data.frame(freq_daily)

barplot(freq_daily, xlab = "Time of the day (hrs)", ylab = "Frequency", col = "orange", main = "Cluster_1 - Weekdays - Type-1", las=2) 
box()

write.csv(freq_daily, 'freq_daily.csv')

##Test the cpt_evaluation function and cpt_count function
Cluster_2_CPA_Jun <- cpt_evaluation(cluster_2_OD[c(11,6)], num_cpt = 3,cpt_method = 'mean', penalty_method = 'AIC')


Cluster_2_CPA_RF <- subset(Cluster_2_CPA_Jun, select = c(2:4))

#write.csv(M_112_RF, 'M_112_RF.csv')

#M_112_RF <- M_112_RF[c(-1)]

#M_112_RF <- as.matrix(M_112_RF)

rel_freq_daily_Clus_2 <- table(Cluster_2_CPA_RF)

freq_daily_Clus_2 <- table(Cluster_2_CPA_RF)/length(Cluster_2_CPA_RF)

barplot(freq_daily_Clus_2, xlab = "Time of the day (hrs)", ylab = "Frequency", col = "green", main = "Cluster_2 - Weekdays - Type-1", las=2) 
box()

write.csv(freq_daily_Clus_2, 'freq_daily_clus_2.csv')

##Test the cpt_evaluation function and cpt_count function
Cluster_3_CPA_Jun <- cpt_evaluation(cluster_3_OD[c(11,6)], num_cpt = 3,cpt_method = 'mean', penalty_method = 'AIC')


Cluster_3_CPA_RF <- subset(Cluster_3_CPA_Jun, select = c(2:4))

#write.csv(M_112_RF, 'M_112_RF.csv')

#M_112_RF <- M_112_RF[c(-1)]

#M_112_RF <- as.matrix(M_112_RF)

rel_freq_daily_Clus_3 <- table(Cluster_3_CPA_RF)

freq_daily_Clus_3 <- table(Cluster_3_CPA_RF)/length(Cluster_3_CPA_RF)

barplot(freq_daily_Clus_3, xlab = "Time of the day (hrs)", ylab = "Frequency", col = "yellow", main = "Cluster_3 - Weekdays - Type-1", las=2) 
box()
abline(h=0.06)

write.csv(freq_daily_Clus_3, 'freq_daily_clus_3.csv')


CPA_cluster_3_mean <- cpt.mean(Cluster_3_mean_profile_type_1$M_tot_wd,penalty = "AIC", Q=6, method="BinSeg")
cpts(CPA_cluster_3_mean)
plot(CPA_cluster_3_mean, xlab="Time(min)",ylab="Occupant probability", main ='Occupancy probability Friday - MeanVariance', xaxt='n')
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
abline(v=c(6, 7,12,16,19,21))



### PLUGload analysis ####

#M_Cluster_1_mean_profile_hourly_analysis

Cluster_1_mean_profile_type_1 <- cluster_1_OD %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot))

Cluster_1_mean_profile_type_1[c(2,3)] <- round(Cluster_1_mean_profile_type_1[c(2,3)], digits = 0)

plot(x=Cluster_1_mean_profile_type_1$Hour, y=Cluster_1_mean_profile_type_1$P_tot_wd, xaxt="n", xlab='Hour', ylab="Average movements detected", main='Cluster_1', type='l')
axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), las=2)


a <- cpt.mean(Cluster_1_mean_profile$M_tot_wd ,penalty = "AIC", Q=3, method="BinSeg")
cpts(a)
plot(a, xlab="Time(min)",ylab="Occupant count", main ='Sunday', xaxt='n')
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)

with(Cluster_1_mean_profile_type_1, plot(Hour, M_tot_wd, type='l', col='red'))

par(new=TRUE)

with(Cluster_1_mean_profile_type_1, plot(Hour, P_tot_wd, type='l', pch=16, axes=F,xlab=NA, ylab=NA, cex=1.2))
axis(side=4)
mtext(side=4, line=3, 'P_Tot')
legend("topleft",
       legend=c(expression(M_tot), expression(P_Tot)),
       lty=c(1,0), pch=c(NA, 16), col=c("red", "black"))

## Cluster#2....

with(Cluster_2_mean_profile_type_1, plot(Hour, M_tot_wd, type='l', col='red'))

par(new=TRUE)

with(Cluster_2_mean_profile_type_1, plot(Hour, LP_tot_wd, type='l', pch=16, axes=F,xlab=NA, ylab=NA, cex=1.2))
axis(side=4)
mtext(side=4, line=3, 'P_Tot')
legend("topleft",
       legend=c(expression(M_tot), expression(P_Tot)),
       lty=c(1,0), pch=c(NA, 16), col=c("red", "black"))

## Cluster#3....

with(Cluster_3_mean_profile_type_1, plot(Hour, M_tot_wd, type='l', col='red'))

par(new=TRUE)

with(Cluster_3_mean_profile_type_1, plot(Hour, LP_tot_wd, type='l', pch=16, axes=F,xlab=NA, ylab=NA, cex=1.2))
axis(side=4)
mtext(side=4, line=2, 'P_Tot')
legend("topleft",
       legend=c(expression(M_tot), expression(P_Tot)),
       lty=c(1,0), pch=c(16, 19), col=c("red", "black"))

write.csv(cluster_1_OD, 'cluster_1_OD.csv')


######

cluster_1_OD_Occ_prob <- cluster_1_OD


cluster_1_OD_Occ_prob$occ_prob <- cluster_1_OD_Occ_prob$M_tot[which(cluster_1_OD_Occ_prob$M_tot>1)] <- 1

library(dplyr)

cluster_1_OD_Occ_prob_CPA <- cluster_1_OD_Occ_prob %>%
  group_by(Day, Hour) %>%
  summarise(occ_count=length(which(M_tot==1)),
            occ_prob=length(which(M_tot==1))/length(M_tot))

q_1 <- cluster_1_OD_Occ_prob_CPA[which(cluster_1_OD_Occ_prob_CPA$Day=='Monday'),]
w_1 <- cluster_1_OD_Occ_prob_CPA[which(cluster_1_OD_Occ_prob_CPA$Day=='Tuesday'),]
e_1 <- cluster_1_OD_Occ_prob_CPA[which(cluster_1_OD_Occ_prob_CPA$Day=='Wednesday'),]
r_1 <- cluster_1_OD_Occ_prob_CPA[which(cluster_1_OD_Occ_prob_CPA$Day=='Thursday'),]
t_1 <- cluster_1_OD_Occ_prob_CPA[which(cluster_1_OD_Occ_prob_CPA$Day=='Friday'),]


plot(q_1$occ_prob, type='l', ylim=c(0,1), col='red')
lines(w_1$occ_prob, type='l', col='blue')
lines(e_1$occ_prob, type='l', col='black')
lines(r_1$occ_prob, type='l', col='green')
lines(t_1$occ_prob, type='l', col='orange')

plot(Type_1_Cluster_1_Average_profile$Average, type='l')


library('changepoint')


q_1_CPA <- cpt.meanvar(Type_1_Cluster_1_Average_profile$Average, penalty = "AIC", Q=5, method="BinSeg")
cpts(q_1_CPA)
plot(q_1_CPA, xlab="Time(Hrs)",ylab="Occupant probability", main ='Occupancy probability_Cluster_1', xaxt='n', ylim=c(0,1))
abline(v=c(4,6))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), las=2)
param.est(q_1_CPA)



### CLUSTER 2####

cluster_2_OD_Occ_prob <- cluster_2_OD


cluster_2_OD_Occ_prob$occ_prob <- cluster_2_OD_Occ_prob$M_tot[which(cluster_2_OD_Occ_prob$M_tot>1)] <- 1

cluster_2_OD_Occ_prob_CPA <- cluster_2_OD_Occ_prob %>%
  group_by(Day, Hour) %>%
  summarise(occ_count=length(which(M_tot==1)),
            occ_prob=length(which(M_tot==1))/length(M_tot))


q_2 <- cluster_2_OD_Occ_prob_CPA[which(cluster_2_OD_Occ_prob_CPA$Day=='Monday'),]
w_2 <- cluster_2_OD_Occ_prob_CPA[which(cluster_2_OD_Occ_prob_CPA$Day=='Tuesday'),]
e_2 <- cluster_2_OD_Occ_prob_CPA[which(cluster_2_OD_Occ_prob_CPA$Day=='Wednesday'),]
r_2 <- cluster_2_OD_Occ_prob_CPA[which(cluster_2_OD_Occ_prob_CPA$Day=='Thursday'),]
t_2 <- cluster_2_OD_Occ_prob_CPA[which(cluster_2_OD_Occ_prob_CPA$Day=='Friday'),]


plot(q_2$occ_prob, type='l', ylim=c(0,1), col='red')
lines(w_2$occ_prob, type='l', col='blue')
lines(e_2$occ_prob, type='l', col='black')
lines(r_2$occ_prob, type='l', col='green')
lines(t_2$occ_prob, type='l', col='orange')

plot(Type_1_Cluster_2_Average_profile$Average, type='l')


library('changepoint')


q_2_CPA <- cpt.meanvar(Type_1_Cluster_2_Average_profile$Average, penalty = "AIC", Q=3, method="BinSeg")
cpts(q_2_CPA)
plot(q_2_CPA, xlab="Time(Hrs)",ylab="Occupant probability", main ='Occupancy probability_Cluster_2', xaxt='n', ylim=c(0,1))
abline(v=c(6,10,17,19))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), las=2)
param.est(q_2_CPA)

##### CLUSTER 3######

cluster_3_OD_Occ_prob <- cluster_3_OD


cluster_3_OD_Occ_prob$occ_prob <- cluster_3_OD_Occ_prob$M_tot[which(cluster_3_OD_Occ_prob$M_tot>1)] <- 1

cluster_3_OD_Occ_prob_CPA <- cluster_3_OD_Occ_prob %>%
  group_by(Day, Hour) %>%
  summarise(occ_count=length(which(M_tot==1)),
            occ_prob=length(which(M_tot==1))/length(M_tot))

q_3 <- cluster_3_OD_Occ_prob_CPA[which(cluster_3_OD_Occ_prob_CPA$Day=='Monday'),]
w_3 <- cluster_3_OD_Occ_prob_CPA[which(cluster_3_OD_Occ_prob_CPA$Day=='Tuesday'),]
e_3 <- cluster_3_OD_Occ_prob_CPA[which(cluster_3_OD_Occ_prob_CPA$Day=='Wednesday'),]
r_3 <- cluster_3_OD_Occ_prob_CPA[which(cluster_3_OD_Occ_prob_CPA$Day=='Thursday'),]
t_3 <- cluster_3_OD_Occ_prob_CPA[which(cluster_3_OD_Occ_prob_CPA$Day=='Friday'),]


plot(q_3$occ_prob, type='l', ylim=c(0,1), col='red')
lines(w_3$occ_prob, type='l', col='blue')
lines(e_3$occ_prob, type='l', col='black')
lines(r_3$occ_prob, type='l', col='green')
lines(t_3$occ_prob, type='l', col='orange')

### Change point daily basis ###

q_3_CPA <- cpt.meanvar(Type_1_Cluster_3_Average_profile$Average, penalty = "AIC", Q=4, method="BinSeg")
cpts(q_3_CPA)
plot(q_3_CPA, xlab="Time(Hrs)",ylab="Occupant probability", main ='Occupancy probability_Cluster_3', xaxt='n', ylim=c(0,1))
abline(v=c(5,12,16,19))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), las=2)
param.est(q_3_CPA)







library('changepoint')


OCC_q_1_CPA <- cpt.mean(Cluster_1_mean_profile_type_1$M_tot_wd, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_q_1_CPA)
plot(OCC_q_1_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_1', xaxt='n')
abline(v=c(7,19,21))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_q_1_CPA)

plot(Cluster_1_mean_profile_type_1$Norm_PP, type='l')

### CLUSTER 2####


library('changepoint')


OCC_q_2_CPA <- cpt.mean(Cluster_2_mean_profile_type_1$M_tot_wd, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_q_2_CPA)
plot(OCC_q_2_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_2', xaxt='n')
abline(v=c(6,11,19))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_q_2_CPA)

##### CLUSTER 3######

### Change point daily basis ###

OCC_q_3_CPA <- cpt.mean(Cluster_3_mean_profile_type_1$M_tot_wd, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_q_3_CPA)
plot(OCC_q_3_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_3', xaxt='n')
abline(v=c(7,12,19))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_q_3_CPA)



############### LIGHTING LOAD CPA #################
library('changepoint')


LP_q_1_CPA <- cpt.meanvar(Cluster_1_mean_profile_type_1$LP_tot_wd, penalty = "AIC", Q=4, method="BinSeg")
cpts(LP_q_1_CPA)
plot(LP_q_1_CPA, xlab="Time(Hrs)",ylab="Average motion detected (Wh)", main ='Occupancy Activity_Cluster_2', xaxt='n')
abline(v=c(7,10,18,21))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(LP_q_1_CPA)

plot(Cluster_1_mean_profile_type_1$Norm_PP, type='l')

### CLUSTER 2####


library('changepoint')


LP_q_2_CPA <- cpt.meanvar(Cluster_2_mean_profile_type_1$LP_tot_wd, penalty = "AIC", Q=3, method="BinSeg")
cpts(LP_q_2_CPA)
plot(LP_q_2_CPA, xlab="Time(Hrs)",ylab="Average motion detected (Wh)", main ='Occupancy Activity_Cluster_2', xaxt='n')
abline(v=c(7,10,20))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(LP_q_2_CPA)

##### CLUSTER 3######

### Change point daily basis ###

LP_q_3_CPA <- cpt.mean(Cluster_3_mean_profile_type_1$LP_tot_wd, penalty = "AIC", Q=3, method="BinSeg")
cpts(LP_q_3_CPA)
plot(LP_q_3_CPA, xlab="Time(Hrs)",ylab="Average energy consumption (Wh)", main ='Plug_Load_Cluster_3', xaxt='n')
abline(v=c(7,11,18))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(LP_q_3_CPA)



library('changepoint')


PP_q_2_CPA <- cpt.mean(Cluster_2_mean_profile_type_1$P_tot_wd, penalty = "AIC", Q=3, method="BinSeg")
cpts(PP_q_2_CPA)
plot(PP_q_2_CPA, xlab="Time(Hrs)",ylab="Average motion detected (Wh)", main ='Occupancy Activity_Cluster_2', xaxt='n')
abline(v=c(7,10,20))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(PP_q_2_CPA)



Type_1_original_data$Day_index <- as.character(Type_1_original_data$Day_index)

Type_1_original_data$Day_index <- as.POSIXct(Type_1_original_data$Day_index)

library('dplyr')

Type_1_original_data <- Type_1_original_data %>%
  mutate(Month = format(Type_1_original_data$Day_index, "%m"))

Type_1_original_data$season <- Func.season(Type_1_original_data$Month)


Type_1_original_data$Day_index <- as.character(Type_1_original_data$Day_index)

Type_1_original_data$Day_index <- as.POSIXct(Type_1_original_data$Day_index)

library('dplyr')

Type_1_data_with_Dayindex <- Type_1_data_with_Dayindex %>%
  mutate(Month = format(Type_1_data_with_Dayindex$Day_index, "%m"))

Type_1_data_with_Dayindex$season <- Func.season(Type_1_data_with_Dayindex$Month)


cluster_1_DI_OD <- subset(Type_1_data_with_Dayindex, Type_1_data_with_Dayindex$cluster==1)

cluster_2_DI_OD <- subset(Type_1_data_with_Dayindex, Type_1_data_with_Dayindex$cluster==2)

cluster_3_DI_OD <- subset(Type_1_data_with_Dayindex, Type_1_data_with_Dayindex$cluster==3)



plot(x=Cluster_1_mean_profile_type_1$Hour, y=Cluster_1_mean_profile_type_1$M_tot_wd, type='l', col='red')
lines(x=Cluster_2_mean_profile_type_1$Hour, y=Cluster_2_mean_profile_type_1$M_tot_wd, type='l', col='green')
lines(x=Cluster_3_mean_profile_type_1$Hour, y=Cluster_3_mean_profile_type_1$M_tot_wd, type='l', col='blue')


p <- ggplot(cluster_2_OD, aes(M_tot, P_tot, color=Hour))+ geom_point(size=2)+theme(panel.background=element_rect(fill = "white", colour = 'black'))+
  ggtitle('Cluster 1') + ylab('Plugpower Energy consumption (Wh)') + xlab("No. of movements")
p+scale_color_gradient(low="green", high="red")
colorbar.plot(p, limits=c(0,24))
print(p)


###

install.packages('plotrix')

library('plotrix')

slices_Type_1 <- c(102,138,116)

lbls <- c("Cluster_1", "Cluster_2", "Cluster_3")

pct <- round(slices_Type_1/sum(slices_Type_1)*100)

lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels


pie3D(slices_Type_1,labels=lbls,explode=0.1,
      main="Type_1_Weekday")



######

cluster_1_PP_dcast <- dcast(cluster_1_OD, Day_index~Hour, value.var = 'P_tot', sum)

dev.off()
boxplot(cluster_1_PP_dcast[c(2:25)], outline = FALSE, las=2, main='Cluster_1', xlab='Hours', ylab='Plugpower consumption (Wh)',border = 'red')
grid()


cluster_2_PP_dcast <- dcast(cluster_2_OD, Day_index~Hour, value.var = 'P_tot', sum)
boxplot(cluster_2_PP_dcast[c(2:25)], outline = FALSE, main='Cluster_2', xlab='Hours', ylab='Plugpower consumption (Wh)', border = 'blue', las=2)
grid()

cluster_3_PP_dcast <- dcast(cluster_3_OD, Day_index~Hour, value.var = 'P_tot', sum)
boxplot(cluster_3_PP_dcast[c(2:25)], outline = FALSE, main='Cluster_3', xlab='Hours', ylab='Plugpower consumption (Wh)', border = 'orange', las=2)
grid()

###########
cluster_1_LP_dcast <- dcast(cluster_1_OD, Day_index~Hour, value.var = 'LP_tot', sum)

boxplot(cluster_1_LP_dcast[c(2:25)], outline = FALSE, las=2, main='Cluster_1', xlab='Hours', ylab='Light energy consumption (Wh)',border = 'red')
grid()


cluster_2_LP_dcast <- dcast(cluster_2_OD, Day_index~Hour, value.var = 'LP_tot', sum)
boxplot(cluster_2_LP_dcast[c(2:25)], outline = FALSE, main='Cluster_2', xlab='Hours', ylab='Light energy consumption (Wh)', border = 'blue', las=2)
grid()

cluster_3_LP_dcast <- dcast(cluster_3_OD, Day_index~Hour, value.var = 'LP_tot', sum)
boxplot(cluster_3_LP_dcast[c(2:25)], outline = FALSE, main='Cluster_3', xlab='Hours', ylab='Light energy consumption (Wh)', border = 'orange', las=2)
grid()

##############
cluster_1_OCC_dcast <- dcast(cluster_1_OD, Day_index~Hour, value.var = 'M_tot', sum)

boxplot(cluster_1_OCC_dcast[c(2:25)], outline = FALSE, las=2, main='Cluster_1', xlab='Hours', ylab='No. of occupancy movements',border = 'red')
grid()


cluster_2_OCC_dcast <- dcast(cluster_2_OD, Day_index~Hour, value.var = 'M_tot', sum)
boxplot(cluster_2_OCC_dcast[c(2:25)], outline = FALSE, main='Cluster_2', xlab='Hours', ylab='No. of occupancy movements', border = 'blue', las=2)
grid()

cluster_3_OCC_dcast <- dcast(cluster_3_OD, Day_index~Hour, value.var = 'M_tot', sum)
boxplot(cluster_3_OCC_dcast[c(2:25)], outline = FALSE, main='Cluster_3', xlab='Hours', ylab='No. of occupancy movements', border = 'orange', las=2)
grid()



###########################

OCC_Mon_1_CPA <- cpt.mean(Cluster_1_Monday_mean_profile$M_tot_wd, penalty = "AIC", Q=5, method="BinSeg")
cpts(OCC_Mon_1_CPA)
plot(OCC_Mon_1_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_1', xaxt='n')
abline(v=c(7,10,12,18,21), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_Mon_1_CPA)

plot(Cluster_1_mean_profile_type_1$Norm_PP, type='l')


OCC_Tue_1_CPA <- cpt.mean(Cluster_1_Tuesday_mean_profile$M_tot_wd, penalty = "AIC", Q=6, method="BinSeg")
cpts(OCC_Tue_1_CPA)
plot(OCC_Tue_1_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_1', xaxt='n')
abline(v=c(6,10, 16,19, 21), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_Tue_1_CPA)



OCC_Wed_1_CPA <- cpt.mean(Cluster_1_Wednesday_mean_profile$M_tot_wd, penalty = "AIC", Q=5, method="BinSeg")
cpts(OCC_Wed_1_CPA)
plot(OCC_Wed_1_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_1', xaxt='n')
abline(v=c(6,7,10,19,21), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_Wed_1_CPA)


OCC_Thu_1_CPA <- cpt.mean(Cluster_1_Thursday_mean_profile$M_tot_wd, penalty = "AIC", Q=5, method="BinSeg")
cpts(OCC_Thu_1_CPA)
plot(OCC_Thu_1_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_1', xaxt='n')
abline(v=c(6,7,11,19,21), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_Thu_1_CPA)

OCC_Fri_1_CPA <- cpt.mean(Cluster_1_Friday_mean_profile$M_tot_wd, penalty = "AIC", Q=5, method="BinSeg")
cpts(OCC_Fri_1_CPA)
plot(OCC_Fri_1_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_1', xaxt='n')
abline(v=c(6,7,14,19,21), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_Fri_1_CPA)




################
OCC_Mon_2_CPA <- cpt.mean(Cluster_2_Monday_mean_profile$M_tot_wd, penalty = "AIC", Q=4, method="BinSeg")
cpts(OCC_Mon_2_CPA)
plot(OCC_Mon_2_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_2', xaxt='n')
abline(v=c(6,11,18,21), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_Mon_2_CPA)



OCC_Tue_2_CPA <- cpt.mean(Cluster_2_Tuesday_mean_profile$M_tot_wd, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_Tue_2_CPA)
plot(OCC_Tue_2_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_2', xaxt='n')
abline(v=c(7,10,18), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_Tue_2_CPA)



OCC_Wed_2_CPA <- cpt.mean(Cluster_2_Wednesday_mean_profile$M_tot_wd, penalty = "AIC", Q=4, method="BinSeg")
cpts(OCC_Wed_2_CPA)
plot(OCC_Wed_2_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_2', xaxt='n')
abline(v=c(6,10,17,20), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_Wed_2_CPA)


OCC_Thu_2_CPA <- cpt.mean(Cluster_2_Thursday_mean_profile$M_tot_wd, penalty = "AIC", Q=5, method="BinSeg")
cpts(OCC_Thu_2_CPA)
plot(OCC_Thu_2_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_2', xaxt='n')
abline(v=c(6,8,10,19))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_Thu_2_CPA)

OCC_Fri_2_CPA <- cpt.mean(Cluster_2_Friday_mean_profile$M_tot_wd, penalty = "AIC", Q=5, method="BinSeg")
cpts(OCC_Fri_2_CPA)
plot(OCC_Fri_2_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_2', xaxt='n')
abline(v=c(6,11,18,20))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_Fri_2_CPA)




##################
OCC_Mon_3_CPA <- cpt.mean(Cluster_3_Monday_mean_profile$M_tot_wd, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_Mon_3_CPA)
plot(OCC_Mon_3_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_3', xaxt='n')
abline(v=c(7,12,19), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_Mon_3_CPA)

plot(Cluster_3_mean_profile_type_3$Norm_PP, type='l')


OCC_Tue_3_CPA <- cpt.mean(Cluster_3_Tuesday_mean_profile$M_tot_wd, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_Tue_3_CPA)
plot(OCC_Tue_3_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_3', xaxt='n')
abline(v=c(7,10,18), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_Tue_3_CPA)



OCC_Wed_3_CPA <- cpt.mean(Cluster_3_Wednesday_mean_profile$M_tot_wd, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_Wed_3_CPA)
plot(OCC_Wed_3_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_3', xaxt='n')
abline(v=c(6,12,17))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_Wed_3_CPA)


OCC_Thu_3_CPA <- cpt.mean(Cluster_3_Thursday_mean_profile$M_tot_wd, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_Thu_3_CPA)
plot(OCC_Thu_3_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_3', xaxt='n')
abline(v=c(7,12,19))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_Thu_3_CPA)

OCC_Fri_3_CPA <- cpt.mean(Cluster_3_Friday_mean_profile$M_tot_wd, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_Fri_3_CPA)
plot(OCC_Fri_3_CPA, xlab="Time(Hrs)",ylab="No.of movements detected", main ='Occupancy Activity_Cluster_3', xaxt='n')
abline(v=c(7,11,19))
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_Fri_3_CPA)



par(mfrow=c(1,3))
#dev.off()
plot(x=Cluster_1_Daily_occ_activity_profile_Weekdays$Hrs, y=Cluster_1_Daily_occ_activity_profile_Weekdays$Mon, type='l', col='blue', ylab='Normalized occupant actiity', xlab='Hours', lwd=3, ylim=c(0,1), main='Cluster 1',cex.axis=2, cex.lab=2, cex.main=2)
lines(x=Cluster_1_Daily_occ_activity_profile_Weekdays$Hrs, y=Cluster_1_Daily_occ_activity_profile_Weekdays$Tue, type='l', col='orange', ylab='OCC actiity', xlab='Hours', lwd=3, ylim=c(0,1),main='Tuesday')
lines(x=Cluster_1_Daily_occ_activity_profile_Weekdays$Hrs, y=Cluster_1_Daily_occ_activity_profile_Weekdays$Wed, type='l', col='green', ylab='OCC actiity', xlab='Hours', lwd=3, ylim=c(0,1),main='Wednesday')
lines(x=Cluster_1_Daily_occ_activity_profile_Weekdays$Hrs, y=Cluster_1_Daily_occ_activity_profile_Weekdays$Thu, type='l', col='red', ylab='OCC actiity', xlab='Hours', lwd=3, ylim=c(0,1),main='Thursday')
lines(x=Cluster_1_Daily_occ_activity_profile_Weekdays$Hrs, y=Cluster_1_Daily_occ_activity_profile_Weekdays$Fri, type='l', ylab='OCC actiity', xlab='Hours', lwd=3, ylim=c(0,1),main='Friday')
legend(
  "bottomright", 
  col = c('Blue', 'Orange', 'Green', 'Red', 'Black'),
  legend = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), lty = 1, bty = 'n', cex = 1.75, lwd=3)




plot(x=Cluster_2_Daily_occ_activity_profile_Weekdays$Hrs, y=Cluster_2_Daily_occ_activity_profile_Weekdays$Monday, type='l', col='blue', ylab='Normalized occupant actiity', xlab='Hours', lwd=3, ylim=c(0,1), main='Cluster 2',cex.axis=2, cex.lab=2, cex.main=2)
lines(x=Cluster_2_Daily_occ_activity_profile_Weekdays$Hrs, y=Cluster_2_Daily_occ_activity_profile_Weekdays$Tuesday, type='l', col='orange', ylab='OCC actiity', xlab='Hours', lwd=3, ylim=c(0,1),main='Tuesday')
lines(x=Cluster_2_Daily_occ_activity_profile_Weekdays$Hrs, y=Cluster_2_Daily_occ_activity_profile_Weekdays$Wednesday, type='l', col='green', ylab='OCC actiity', xlab='Hours', lwd=3, ylim=c(0,1),main='Wednesday')
lines(x=Cluster_2_Daily_occ_activity_profile_Weekdays$Hrs, y=Cluster_2_Daily_occ_activity_profile_Weekdays$Thursday, type='l', col='red', ylab='OCC actiity', xlab='Hours', lwd=3, ylim=c(0,1),main='Thursday')
lines(x=Cluster_2_Daily_occ_activity_profile_Weekdays$Hrs, y=Cluster_2_Daily_occ_activity_profile_Weekdays$Friday, type='l', ylab='OCC actiity', xlab='Hours', lwd=3, ylim=c(0,1),main='Friday')
legend(
  "topright", 
  col = c('Blue', 'Orange', 'Green', 'Red', 'Black'),
  legend = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), lty = 1, bty = 'n', cex = 1.75, lwd=3)



plot(x=Cluster_3_Daily_occ_activity_profile_Weekdays$Hrs, y=Cluster_3_Daily_occ_activity_profile_Weekdays$Monday, type='l', col='blue', ylab='Normalized occupant actiity', xlab='Hours', lwd=3, ylim=c(0,1), main='Cluster 3',cex.axis=2, cex.lab=2, cex.main=2)
lines(x=Cluster_3_Daily_occ_activity_profile_Weekdays$Hrs, y=Cluster_3_Daily_occ_activity_profile_Weekdays$Tuesday, type='l', col='orange', ylab='OCC actiity', xlab='Hours', lwd=3, ylim=c(0,1),main='Tuesday')
lines(x=Cluster_3_Daily_occ_activity_profile_Weekdays$Hrs, y=Cluster_3_Daily_occ_activity_profile_Weekdays$Wednesday, type='l', col='green', ylab='OCC actiity', xlab='Hours', lwd=3, ylim=c(0,1),main='Wednesday')
lines(x=Cluster_3_Daily_occ_activity_profile_Weekdays$Hrs, y=Cluster_3_Daily_occ_activity_profile_Weekdays$Thursday, type='l', col='red', ylab='OCC actiity', xlab='Hours', lwd=3, ylim=c(0,1),main='Thursday')
lines(x=Cluster_3_Daily_occ_activity_profile_Weekdays$Hrs, y=Cluster_3_Daily_occ_activity_profile_Weekdays$Friday, type='l', ylab='OCC actiity', xlab='Hours', lwd=3, ylim=c(0,1),main='Friday')
legend(
  "topleft", 
  col = c('Blue', 'Orange', 'Green', 'Red', 'Black'),
  legend = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), lty = 1, bty = 'n', cex = 1.75, lwd=3)


#######OCCUPANTS PROBABILITY###############

###########################

OCC_prob_Mon_1_CPA <- cpt.meanvar(q_1$occ_prob, penalty = "AIC", Q=5, method="BinSeg")
cpts(OCC_prob_Mon_1_CPA)
plot(OCC_prob_Mon_1_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_1, Monday', xaxt='n', ylim=c(0,1))
abline(v=c(12), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Mon_1_CPA)

plot(Cluster_1_mean_profile_type_1$Norm_PP, type='l')


OCC_prob_Tue_1_CPA <- cpt.meanvar(w_1$occ_prob, penalty = "AIC", Q=6, method="BinSeg")
cpts(OCC_prob_Tue_1_CPA)
plot(OCC_prob_Tue_1_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_1, Tuesday', xaxt='n', ylim=c(0,1))
abline(v=c(4,20), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Tue_1_CPA)



OCC_prob_Wed_1_CPA <- cpt.meanvar(e_1$occ_prob, penalty = "AIC", Q=6, method="BinSeg")
cpts(OCC_prob_Wed_1_CPA)
plot(OCC_prob_Wed_1_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_1, Wednesday', xaxt='n', ylim=c(0,1))
abline(v=c(6,9,14), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Wed_1_CPA)


OCC_prob_Thu_1_CPA <- cpt.meanvar(r_1$occ_prob, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_prob_Thu_1_CPA)
plot(OCC_prob_Thu_1_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_1, Thursday', xaxt='n', ylim=c(0,1))
abline(v=c(3,18,20), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Thu_1_CPA)

OCC_prob_Fri_1_CPA <- cpt.meanvar(t_1$occ_prob, penalty = "AIC", Q=4, method="BinSeg")
cpts(OCC_prob_Fri_1_CPA)
plot(OCC_prob_Fri_1_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_1, Friday', xaxt='n', ylim=c(0,1))
abline(v=c(6,15,18), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Fri_1_CPA)




################
OCC_prob_Mon_2_CPA <- cpt.meanvar(q_2$occ_prob, penalty = "AIC", Q=2, method="BinSeg")
cpts(OCC_prob_Mon_2_CPA)
plot(OCC_prob_Mon_2_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_1, Monday', xaxt='n', ylim=c(0,1))
abline(v=c(11,19), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Mon_2_CPA)



OCC_prob_Tue_2_CPA <- cpt.meanvar(w_2$occ_prob, penalty = "AIC", Q=4, method="BinSeg")
cpts(OCC_prob_Tue_2_CPA)
plot(OCC_prob_Tue_2_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_2, Tuesday', xaxt='n', ylim=c(0,1))
abline(v=c(8,10, 15,21), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Tue_2_CPA)



OCC_prob_Wed_2_CPA <- cpt.meanvar(e_2$occ_prob, penalty = "AIC", Q=4, method="BinSeg")
cpts(OCC_prob_Wed_2_CPA)
plot(OCC_prob_Wed_2_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_2, Wednesday', xaxt='n', ylim=c(0,1))
abline(v=c(6,11,21), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Wed_2_CPA)


OCC_prob_Thu_2_CPA <- cpt.meanvar(r_2$occ_prob, penalty = "AIC", Q=4, method="BinSeg")
cpts(OCC_prob_Thu_2_CPA)
plot(OCC_prob_Thu_2_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_2, Thursday', ylim=c(0,1))
abline(v=c(3,11,19), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Thu_2_CPA)

OCC_prob_Fri_2_CPA <- cpt.meanvar(t_2$occ_prob, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_prob_Fri_2_CPA)
plot(OCC_prob_Fri_2_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_2, Friday', xaxt='n')
abline(v=c(9,12,21), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Fri_2_CPA)




##################
OCC_prob_Mon_3_CPA <- cpt.meanvar(q_3$occ_prob, penalty = "AIC", Q=3, method="BinSeg")
cpts(OCC_prob_Mon_3_CPA)
plot(OCC_prob_Mon_3_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_3, Monday', xaxt='n', ylim=c(0,1))
abline(v=c(7,11,19), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Mon_3_CPA)



OCC_prob_Tue_3_CPA <- cpt.meanvar(w_3$occ_prob, penalty = "AIC", Q=6, method="BinSeg")
cpts(OCC_prob_Tue_3_CPA)
plot(OCC_prob_Tue_3_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_3, Tuesday', xaxt='n')
abline(v=c(3,7, 10, 12,16), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Tue_3_CPA)



OCC_prob_Wed_3_CPA <- cpt.meanvar(e_3$occ_prob, penalty = "AIC", Q=5, method="BinSeg")
cpts(OCC_prob_Wed_3_CPA)
plot(OCC_prob_Wed_3_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_3, Wednesday', xaxt='n', ylim=c(0,1))
abline(v=c(6,10,12,16,20), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Wed_3_CPA)


OCC_prob_Thu_3_CPA <- cpt.meanvar(r_3$occ_prob, penalty = "AIC", Q=5, method="BinSeg")
cpts(OCC_prob_Thu_3_CPA)
plot(OCC_prob_Thu_3_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_3, Thursday', xaxt='n', ylim=c(0,1))
abline(v=c(4,12,16,19), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Thu_3_CPA)

OCC_prob_Fri_3_CPA <- cpt.meanvar(t_3$occ_prob, penalty = "AIC", Q=4, method="BinSeg")
cpts(OCC_prob_Fri_3_CPA)
plot(OCC_prob_Fri_3_CPA, xlab="Time(Hrs)",ylab="Presence probability", main ='Occupants presence probability_Cluster_3, Friday', xaxt='n', ylim=c(0,1))
abline(v=c(6,10,12,19), lty=2)
axis(1,c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), las=2)
param.est(OCC_prob_Fri_3_CPA)




par(mfrow=c(3,5))
#dev.off()
plot(x=Cluster_1_Daily_occ_presence_prob_Weekdays$Hours, y=Cluster_1_Daily_occ_presence_prob_Weekdays$Monday, type='l', col='blue', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1), main='Monday_Cluster1', cex.lab=1.5, cex.axis=1.25, cex.main=2)
plot(x=Cluster_1_Daily_occ_presence_prob_Weekdays$Hours, y=Cluster_1_Daily_occ_presence_prob_Weekdays$Tuesday, type='l', col='orange', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1),main='Tuesday_Cluster1', cex.lab=1.5, cex.axis=1.25, cex.main=2)
plot(x=Cluster_1_Daily_occ_presence_prob_Weekdays$Hours, y=Cluster_1_Daily_occ_presence_prob_Weekdays$Wednesday, type='l', col='green', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1),main='Wednesday_Cluster1', cex.lab=1.5, cex.axis=1.25, cex.main=2)
plot(x=Cluster_1_Daily_occ_presence_prob_Weekdays$Hours, y=Cluster_1_Daily_occ_presence_prob_Weekdays$Thursday, type='l', col='red', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1),main='Thursday_Cluster1', cex.lab=1.5, cex.axis=1.25, cex.main=2)
plot(x=Cluster_1_Daily_occ_presence_prob_Weekdays$Hours, y=Cluster_1_Daily_occ_presence_prob_Weekdays$Friday, type='l', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1),main='Friday_Cluster1', cex.lab=1.5, cex.axis=1.25, cex.main=2)
legend(
  "bottomright", 
  col = c('Blue', 'Orange', 'Green', 'Red', 'Black'),
  legend = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), lty = 1, bty = 'n', cex = 1.2, lwd=3)




plot(x=Cluster_2_Daily_occ_presence_prob_Weekdays$Hours, y=Cluster_2_Daily_occ_presence_prob_Weekdays$Monday, type='l', col='blue', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1), main='Monday_Cluster2', cex.lab=1.5, cex.axis=1.25, cex.main=2)
plot(x=Cluster_2_Daily_occ_presence_prob_Weekdays$Hours, y=Cluster_2_Daily_occ_presence_prob_Weekdays$Tuesday, type='l', col='orange', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1),main='Tuesday_Cluster2', cex.lab=1.5, cex.axis=1.25, cex.main=2)
plot(x=Cluster_2_Daily_occ_presence_prob_Weekdays$Hours, y=Cluster_2_Daily_occ_presence_prob_Weekdays$Wednesday, type='l', col='green', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1),main='Wednesday_Cluster2', cex.lab=1.5, cex.axis=1.25, cex.main=2)
plot(x=Cluster_2_Daily_occ_presence_prob_Weekdays$Hours, y=Cluster_2_Daily_occ_presence_prob_Weekdays$Thursday, type='l', col='red', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1),main='Thursday_Cluster2', cex.lab=1.5, cex.axis=1.25, cex.main=2)
plot(x=Cluster_2_Daily_occ_presence_prob_Weekdays$Hours, y=Cluster_2_Daily_occ_presence_prob_Weekdays$Friday, type='l', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1),main='Friday_Cluster2', cex.lab=1.5, cex.axis=1.25, cex.main=2)
legend(
  "topright", 
  col = c('Blue', 'Orange', 'Green', 'Red', 'Black'),
  legend = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), lty = 1, bty = 'n', cex = 0.85, lwd=3)



plot(x=Cluster_3_Daily_occ_presence_prob_Weekdays$Hours, y=Cluster_3_Daily_occ_presence_prob_Weekdays$Monday, type='l', col='blue', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1), main='Monday_Cluster3', cex.lab=1.5, cex.axis=1.25, cex.main=2)
plot(x=Cluster_3_Daily_occ_presence_prob_Weekdays$Hours, y=Cluster_3_Daily_occ_presence_prob_Weekdays$Tuesday, type='l', col='orange', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1),main='Tuesday_Cluster3', cex.lab=1.5, cex.axis=1.25, cex.main=2)
plot(x=Cluster_3_Daily_occ_presence_prob_Weekdays$Hours, y=Cluster_3_Daily_occ_presence_prob_Weekdays$Wednesday, type='l', col='green', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1),main='Wednesday_Cluster3', cex.lab=1.5, cex.axis=1.25, cex.main=2)
plot(x=Cluster_3_Daily_occ_presence_prob_Weekdays$Hours, y=Cluster_3_Daily_occ_presence_prob_Weekdays$Thursday, type='l', col='red', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1),main='Thursday_Cluster3', cex.lab=1.5, cex.axis=1.25, cex.main=2)
plot(x=Cluster_3_Daily_occ_presence_prob_Weekdays$Hours, y=Cluster_3_Daily_occ_presence_prob_Weekdays$Friday, type='l', ylab='Presence probability', xlab='Hours', lwd=3, ylim=c(0,1),main='Friday_Cluster3', cex.lab=1.5, cex.axis=1.25, cex.main=2)
legend(
  "bottomleft", 
  col = c('Blue', 'Orange', 'Green', 'Red', 'Black'),
  legend = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), lty = 1, bty = 'n', cex = 1.2, lwd=3)



##################
par(mfrow=c(1,3))
plot(Cluster_1_Monday_mean_profile$P_tot_wd, type='l', col='blue', ylim=c(0,550), ylab='Energy consumption (Wh)', xlab='Hours', lwd=3, main ='Cluster 1', cex.axis=2, cex.lab=2, cex.main=3)
#,xaxt='n')
#axis(1,c(0,2,4,6,8,10,12,14,16,18,20,22,24))
lines(Cluster_1_Tuesday_mean_profile$P_tot_wd, type='l', col='orange', lwd=3)
lines(Cluster_1_Wednesday_mean_profile$P_tot_wd, type='l', col='green', lwd=3)
lines(Cluster_1_Thursday_mean_profile$P_tot_wd, type='l', col='red', lwd=3)
lines(Cluster_1_Friday_mean_profile$P_tot_wd, type='l', col='black', lwd=3)
legend(
  "bottomright", 
  col = c('Blue', 'Orange', 'Green', 'Red', 'Black'),
  legend = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), lty = 1, bty = 'n', cex = 2, lwd=3)




plot(Cluster_2_Monday_mean_profile$P_tot_wd, type='l', col='blue', ylim=c(0, 550), ylab='Energy consumption (Wh)', lwd=3, main='Cluster 2', xlab='Hours',cex.axis=2, cex.lab=2, cex.main=3)
#, xaxt='n')
#axis(1,c(0,2,4,6,8,10,12,14,16,18,20,22,24))
lines(Cluster_2_Tuesday_mean_profile$P_tot_wd, type='l', col='orange', lwd=3)
lines(Cluster_2_Wednesday_mean_profile$P_tot_wd, type='l', col='green', lwd=3)
lines(Cluster_2_Thursday_mean_profile$P_tot_wd, type='l', col='red', lwd=3)
lines(Cluster_2_Friday_mean_profile$P_tot_wd, type='l', col='black', lwd=3)
legend(
  "topright", 
  col = c('Blue', 'Orange', 'Green', 'Red', 'Black'),
  legend = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), lty = 1, bty = 'n', cex = 2, lwd=3)



plot(Cluster_3_Monday_mean_profile$P_tot_wd, type='l', col='blue', ylim=c(0, 550), ylab='Energy consumption (Wh)', xlab='Hours', main='Cluster 3', lwd=3, cex.axis=2, cex.lab=2, cex.main=3)
#,xaxt='n')
#axis(1,c(0,2,4,6,8,10,12,14,16,18,20,22,24))
lines(Cluster_3_Tuesday_mean_profile$P_tot_wd, type='l', col='orange', lwd=3)
lines(Cluster_3_Wednesday_mean_profile$P_tot_wd, type='l', col='green', lwd=3)
lines(Cluster_3_Thursday_mean_profile$P_tot_wd, type='l', col='red', lwd=3)
lines(Cluster_3_Friday_mean_profile$P_tot_wd, type='l', col='black', lwd=3)
legend(
  "topleft", 
  col = c('Blue', 'Orange', 'Green', 'Red', 'Black'),
  legend = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), lty = 1, bty = 'n', cex = 2, lwd=3)








