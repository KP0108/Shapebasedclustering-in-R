library(data.table)
library(parallel)
library(cluster)
library(clusterCrit)
library(TSrepr)
library(OpenML)
library(ggplot2)
library(grid)
library(animation)
library(gganimate)
library(av)

data <- OpenML::getOMLDataSet(data.id = 41060)
data <- data.matrix(data$data)

#data <- London_5months_5066ID

data <- data.matrix(data$data)

data_cons <- data[1:1000,]
period <- 24 # frequency of time series, every day 48 measurements are gathered


ggplot(data.table(Time = 1:ncol(data_cons),
                  Value = data_cons[200,])) +
  geom_line(aes(Time, Value)) +
  labs(y = "Consumption (kWh)") +
  theme_bw()


ggplot(data.table(Time = 1:period,
                  Value = repr_seas_profile(data_cons[200,],
                                            freq = period,
                                            func = mean))) +
  geom_line(aes(Time, Value)) +
  geom_point(aes(Time, Value), alpha = 0.8, size = 2.5) +
  scale_x_continuous(breaks = seq(1, 48, 6),
                     labels = paste(seq(0, 23, by = 3), "h", sep = "")) +
  labs(y = "Consumption (kWh)") +
  theme_bw()


data_ave_prof <- repr_matrix(data_cons,
                             func = repr_seas_profile,
                             args = list(freq = period,
                                         func = median),
                             normalise = TRUE,
                             func_norm = norm_z)

###########


####### kmeans clustering #########

#  Occupancy
#Elbow Method
k.max <- 10
data <- A_112_weekday_cluster_data[c(2:25)]
wss1 <- sapply(1:k.max, 
               function(k){kmeans(data, k, nstart=50,iter.max = 100 )$tot.withinss})
wss1
plot(1:k.max, wss1,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main = "Occupancy movement_Morning period")
box()

set.seed(123)


# Validation
#Compute clValid

library(clValid)

hourly_data_152 <- as.data.frame(hourly_data_152)

data <- A_112_weekday_cluster_data[c(2:25)]
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(data, nClust = 2:6,
                  clMethods = clmethods, validation = "internal")
summary(intern)

plot(intern)

data_ave_prof <- repr_matrix(A_112_weekday_cluster_data[c(2:25)],
                             func = repr_seas_profile,
                             args = list(freq = period,
                                         func = median),
                             normalise = TRUE,
                             func_norm = norm_z)




res_clust <- kmeans(data_ave_prof, 10, nstart = 20)

data_plot <- data.table(melt(data_ave_prof))
data_plot[, Clust := rep(res_clust$cluster, ncol(data_ave_prof))]

data_centroid <- data.table(melt(res_clust$centers))
data_centroid[, Clust := Var1]


ggplot(data_plot) +
  facet_wrap(~Clust, scales = "free", ncol = 3) +
  geom_line(aes(Var2, value, group = Var1), alpha = 0.7) +
  geom_line(data = data_centroid, aes(Var2, value, group = Var1),
            alpha = 0.8, color = "orange", size = 1.2) +
  #scale_x_continuous(breaks = c(1,seq(1, 24, 1)),
                    # labels = paste(seq(0, 24, by = 1), "h", sep = "")) +
  theme_bw()


data_cons <- hourly_data_152_weekdays[c(1,2,3,4,22)]

period <- 24

data_cons$num <- seq(1,5472,1)

plot(data_cons$P_tot, type='l')


ggplot(data.table(Time = 1:period,
                  Value = repr_seas_profile(hourly_data_152$P_tot,
                                            freq = period,
                                            func = mean))) +
  geom_line(aes(Time, Value)) +
  geom_point(aes(Time, Value), alpha = 0.8, size = 2.5) +
  scale_x_continuous(breaks = seq(1, 24, 2),
                     labels = paste(seq(0, 23, by = 2), "h", sep = "")) +
  labs(y = "Consumption (Wh)")+
  theme_bw()



ggplot(data.table(Time = 1:(period*7),
                  Value = repr_seas_profile(data_cons$P_tot,
                                            freq = period*7,
                                            func = mean))) +
  geom_line(aes(Time, Value), size = 0.8) +
  geom_vline(xintercept = (0:7)*period, color = "dodgerblue2",
             size = 1.2, alpha = 0.7, linetype = 2) +
  labs(y = "Consumption (Wh)") +
  theme_bw()

remove(data_ave_prof)

data_ave_prof <- repr_matrix(data_cons$P_tot,
                             func = repr_seas_profile,
                             args = list(freq = period,
                                         func = median),
                             normalise = TRUE,
                             func_norm = norm_z)
