install.packages('ggmap')
library(raster)
library(ggmap)
library(ggplot2)
register_stadiamaps(key = 'c070816e-74f5-4939-8b49-a0e919de5792')

# Define the bounding box for the area of interest
#Full Image
bbox <- c(left = -180, bottom = 10, right = -60, top = 72)

#Only Mainland America
bbox <- c(left = -130, bottom = 20, right = -60, top = 50)
# Download Stamen map tiles
stamen_tiles <- get_stadiamap(bbox, zoom = 4,labels = FALSE,maptype = "stamen_terrain_background")
# Plot the Stamen map tiles
?get_stadiamap()

#Init_Cluster
load('output_weatherdatafinal_GMM_AR1_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G = 4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$init_cluster==g)] = colors[g]  
}

dev.new(width = 2000, height = 500)
ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#GMM_AR1_Cluster
load('output_weatherdatafinal_GMM_AR1_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#GMM_AR1_Sine_Cluster
load('output_weatherdatafinal_GMM_AR1_Sine_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G = 4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 



#SubsetData
#Init_Cluster
load('output_weatherdatasubsetfinal_GMM_AR1_G6_conv1e_14.RData')
loc = read.csv('Subsetted_location_info.csv')
size = rep(0,600)
size[which((1-apply(WeatherDataSubset_GMM_AR1_G6$probability,1,max))<0.01)] = 1.5
size[which((1-apply(WeatherDataSubset_GMM_AR1_G6$probability,1,max))>0.01)] = 3
shape = rep(0,600)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_G6$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_G6$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,600)
G = 6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_G6$init_cluster==g)] = colors[g]  
}

dev.new(width = 2000, height = 500)
ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#GMM_AR1_Cluster
load('output_weatherdatasubsetfinal_GMM_AR1_G6_conv1e_14.RData')
loc = read.csv('Subsetted_location_info.csv')
size = rep(0,600)
size[which((1-apply(WeatherDataSubset_GMM_AR1_G6$probability,1,max))<0.01)] = 1.5
size[which((1-apply(WeatherDataSubset_GMM_AR1_G6$probability,1,max))>0.01)] = 3
shape = rep(0,600)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_G6$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_G6$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,600)
G = 6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_G6$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#GMM_AR1_Sine_Cluster
load('output_weatherdatasubsetfinal_GMM_AR1_Sine_G6_conv1e_14.RData')
loc = read.csv('Subsetted_location_info.csv')
size = rep(0,600)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 1.5
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 3
shape = rep(0,600)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,600)
G = 6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 



#Mean Estimation plots
load('output_weatherdatafinal_GMM_AR1_Sine_G6_conv1e_14.RData')
load('Manly_Transformed_TestData.RData')
P = 3
G = 6
mu = array(dim = c(P,840,G))
for(g in 1:G){
  for(p in 1:P){
    mu[p,,g] = apply(t(test_data[p,,which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))]),2,mean)
    
  }
}
dev.new()
k = 5
P = 3
names = c('TMIN_Sine','TMAX_Sine','PRCP_Sine')
par(mfrow = c(P,1))
for(p in 1:P){
  if(p != P){
    par(mar=c(2,4,2,1))
    plot(c(1:6), WeatherDataSubset_GMM_AR1_Sine$mu_sine_init_pars[p,k,], col = 'green',ylab = names[p], xaxt = 'n', ylim = c(0.95,1.05),pch = 10)
    points(c(1:6), WeatherDataSubset_GMM_AR1_Sine$mu_sine_pars[p,k,],col='red',pch = 10)
  }
  else{
    par(mar=c(4,4,0.1,1))
    plot(c(1:6), WeatherDataSubset_GMM_AR1_Sine$mu_sine_init_pars[p,k,], col = 'green',ylab = names[p], ylim = c(0.95,1.05),pch = 10,xlab = 'g')
    points(c(1:6), WeatherDataSubset_GMM_AR1_Sine$mu_sine_pars[p,k,],col='red',pch = 10)
  }
  if(p == 1){
    title('f')
  }
}

WeatherDataSubset_GMM_AR1_Sine$mu_sine_pars

plot_g = 4
x <- 1:t
par(mfrow = c(3,1))
par(mar=c(0.1,4,3,1))
plot(x, mu[1,,plot_g], type = 'l', col = 'green',ylab = 'TMIN', xaxt = 'n')
#points(x, mean_original_y[1,], type = "l",col='blue')
abline(h = 0, lwd = 0.2)
y.seq <- sine_estimate(x, W_pars[1,1,plot_g], W_pars[1,2,plot_g], W_pars[1,3,plot_g], W_pars[1,4,plot_g], W_pars[1,5,plot_g], W_pars[1,6,plot_g])
points(x, y.seq, type = "l",col='red')
title('Cluster 21')

par(mar=c(0.1,4,0.1,1))
plot(x, mu[2,,plot_g], type = 'l', col = 'green',ylab = 'TMAX', xaxt = 'n')
#points(x, mean_original_y[1,], type = "l",col='blue')
abline(h = 0, lwd = 0.2)
y.seq <- sine_estimate(x, W_pars[2,1,plot_g], W_pars[2,2,plot_g], W_pars[2,3,plot_g], W_pars[2,4,plot_g], W_pars[2,5,plot_g], W_pars[2,6,plot_g])
points(x, y.seq, type = "l",col='red')

par(mar=c(4,4,0.1,1))
plot(x, mu[3,,plot_g], type = 'l', col = 'green',ylab = 'PRCP', xaxt = 'n')
#points(x, mean_original_y[1,], type = "l",col='blue')
abline(h = 0, lwd = 0.2)
y.seq <- sine_estimate(x, W_pars[3,1,plot_g], W_pars[3,2,plot_g], W_pars[3,3,plot_g], W_pars[3,4,plot_g], W_pars[3,5,plot_g], W_pars[3,6,plot_g])
points(x, y.seq, type = "l",col='red')


WeatherDataSubset_GMM_AR1_Sine$mu_sine_init_pars
WeatherDataSubset_GMM_AR1_Sine$mu_sine_pars


#G = 10
#Init_Cluster
load('output_weatherdatafinal_GMM_AR1_G10_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G = 10
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$init_cluster==g)] = colors[g]  
}

dev.new(width = 2000, height = 500)
ggmap(stamen_tiles)+ geom_point(data = loc[c(1361,1705),], aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors[c(1361,1705)],size = size[c(1361,1705)], shape = shape[c(1361,1705)]) + theme(legend.position="none") 

#GMM_AR1_Cluster
load('output_weatherdatafinal_GMM_AR1_G10_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =10
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
ggmap(stamen_tiles)+ geom_point(data = loc[c(1361,1705),], aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors[c(1361,1705)],size = size[c(1361,1705)], shape = shape[c(1361,1705)]) + theme(legend.position="none") 

#GMM_AR1_Sine_Cluster
load('output_weatherdatafinal_GMM_AR1_Sine_G10_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G = 10
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
ggmap(stamen_tiles)+ geom_point(data = loc[c(1361,1705),], aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors[c(1361,1705)],size = size[c(1361,1705)], shape = shape[c(1361,1705)]) + theme(legend.position="none") 

#G = 20
#Init_Cluster
load('output_weatherdatafinal_GMM_AR1_G10_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G = 20
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$init_cluster==g)] = colors[g]  
}

dev.new(width = 2000, height = 500)
ggmap(stamen_tiles)+ geom_point(data = loc[c(1361,1705),], aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors[c(1361,1705)],size = size[c(1361,1705)], shape = shape[c(1361,1705)]) + theme(legend.position="none") 

#GMM_AR1_Cluster
load('output_weatherdatafinal_GMM_AR1_G20_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =20
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
ggmap(stamen_tiles)+ geom_point(data = loc[c(1361,1705),], aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors[c(1361,1705)],size = size[c(1361,1705)], shape = shape[c(1361,1705)]) + theme(legend.position="none") 

#GMM_AR1_Sine_Cluster
load('output_weatherdatafinal_GMM_AR1_Sine_G20_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G = 20
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
ggmap(stamen_tiles)+ geom_point(data = loc[c(1361,1705),], aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors[c(1361,1705)],size = size[c(1361,1705)], shape = shape[c(1361,1705)]) + theme(legend.position="none") 



load('Manly_Transformed_TestData.RData')
dim(test_data)
new_test_data = test_data[,,c(1361,1705)]
dim(new_test_data)
WeatherDataSubset_GMM_AR1$predicted_cluster[1361]
unique(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster)
#Plotting the Actual Data
par(mfrow = c(3,1))
par(mar=c(0.1,4,3,1))
plot(c(1:840),new_test_data[1,,1],type = 'l',col = 'green',lwd = 4, lty = 1, ylab = 'TMIN', xlab = 'Months',xaxt='n',ylim = c(-10,20))
lines(c(1:840),new_test_data[1,,2],col = 'red',lwd = 2, lty = 1)
title('TRUE_DATA_VALUES')
par(mar=c(0.1,4,0.1,1))
plot(c(1:840),new_test_data[2,,1],type = 'l',col = 'green',lwd = 4, lty = 1, ylab = 'TMAX', xlab = 'Months',xaxt='n',ylim = c(0,40))
lines(c(1:840),new_test_data[2,,2],col = 'red',lwd = 2, lty = 1)
par(mar=c(4,4,0.1,1))
plot(c(1:840),new_test_data[3,,1],type = 'l',col = 'green',lwd = 4, lty = 1, ylab = 'PRCP', xlab = 'Months')
lines(c(1:840),new_test_data[3,,2],col = 'red',lwd = 2, lty = 1)

#Plotting the Actual Data
par(mfrow = c(3,1))
par(mar=c(0.1,4,3,1))
plot(c(1:100),new_test_data[1,1:100,1],type = 'l',col = 'green',lwd = 4, lty = 1, ylab = 'TMIN', xlab = 'Months',xaxt='n',ylim = c(-10,20))
lines(c(1:100),new_test_data[1,1:100,2],col = 'red',lwd = 2, lty = 1)
title('TRUE_DATA_VALUES')
par(mar=c(0.1,4,0.1,1))
plot(c(1:100),new_test_data[2,1:100,1],type = 'l',col = 'green',lwd = 4, lty = 1, ylab = 'TMAX', xlab = 'Months',xaxt='n',ylim = c(0,40))
lines(c(1:100),new_test_data[2,1:100,2],col = 'red',lwd = 2, lty = 1)
par(mar=c(4,4,0.1,1))
plot(c(1:100),new_test_data[3,1:100,1],type = 'l',col = 'green',lwd = 4, lty = 1, ylab = 'PRCP', xlab = 'Months')
lines(c(1:100),new_test_data[3,1:100,2],col = 'red',lwd = 2, lty = 1)

WeatherDataSubset_GMM_AR1$probability[1361,]
WeatherDataSubset_GMM_AR1$predicted_cluster[1705]
WeatherDataSubset_GMM_AR1_Sine$predicted_cluster[1705]

#Plotting the Actual Data with estimates
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,15))
plot(c(1:840),new_test_data[1,,1],type = 'l',col = 'green',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',xaxt='n',ylim = c(-10,15))
lines(c(1:840),WeatherDataSubset_GMM_AR1$mu[1,,5],col = 'brown', lwd = 2,lty = 5)
legend(x = 860,y = 10,legend = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col =c('green','red','brown','purple','blue'))
title('TMIN_Values_Vs_EstimatedClusterMean')
par(mar=c(0.1,4,0.1,15))
plot(c(1:840),new_test_data[1,,2],type = 'l',col = 'red',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',xaxt='n',ylim = c(-5,21))
lines(c(1:840),WeatherDataSubset_GMM_AR1$mu[1,,2],col = 'purple', lwd = 2,lty = 5)
par(mar=c(4,4,0.1,15))
plot(c(1:840),new_test_data[1,,1],type = 'l',col = 'green',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',xaxt='n',ylim = c(-5,20))
lines(c(1:840),new_test_data[1,,2],col = 'red',lwd = 2, lty = 1)
lines(c(1:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,,5],col = 'blue', lwd = 2,lty = 2)


#Plotting the Actual Data with estimates
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,15))
plot(c(1:840),new_test_data[2,,1],type = 'l',col = 'green',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',xaxt='n',ylim = c(-2,50))
lines(c(1:840),WeatherDataSubset_GMM_AR1$mu[2,,5],col = 'brown', lwd = 2,lty = 5)
legend(x = 860,y = 40,legend = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col =c('green','red','brown','purple','blue'))
title('TMAX_Values_Vs_EstimatedClusterMean')
par(mar=c(0.1,4,0.1,15))
plot(c(1:840),new_test_data[2,,2],type = 'l',col = 'red',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',xaxt='n',ylim = c(10,50))
lines(c(1:840),WeatherDataSubset_GMM_AR1$mu[2,,2],col = 'purple', lwd = 2,lty = 5)
par(mar=c(4,4,0.1,15))
plot(c(1:840),new_test_data[2,,1],type = 'l',col = 'green',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-2,40))
lines(c(1:840),new_test_data[2,,2],col = 'red',lwd = 2, lty = 1)
lines(c(1:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,,5],col = 'blue', lwd = 2,lty = 2)

#Plotting the Actual Data with estimates
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,15))
plot(c(1:840),new_test_data[3,,1],type = 'l',col = 'green',lwd = 2, lty = 1, ylab = 'PRCP', xlab = 'Months',xaxt='n',ylim = c(0,100))
lines(c(1:840),WeatherDataSubset_GMM_AR1$mu[3,,5],col = 'brown', lwd = 2,lty = 5)
legend(x = 860,y = 80,legend = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col =c('green','red','brown','purple','blue'))
title('PRCP_Values_Vs_EstimatedClusterMean')
par(mar=c(0.1,4,0.1,15))
plot(c(1:840),new_test_data[3,,2],type = 'l',col = 'red',lwd = 2, lty = 1, ylab = 'PRCP', xlab = 'Months',xaxt='n',ylim = c(0,110))
lines(c(1:840),WeatherDataSubset_GMM_AR1$mu[3,,2],col = 'purple', lwd = 2,lty = 5)
par(mar=c(4,4,0.1,15))
plot(c(1:840),new_test_data[3,,1],type = 'l',col = 'green',lwd = 2, lty = 1, ylab = 'PRCP', xlab = 'Months',ylim = c(0,110))
lines(c(1:840),new_test_data[3,,2],col = 'red',lwd = 2, lty = 1)
lines(c(1:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,,5],col = 'blue', lwd = 2,lty = 2)



#Plotting the Actual Data with estimates
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,20))
plot(c(600:840),new_test_data[1,600:840,1],type = 'l',col = 'green',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',xaxt='n',ylim = c(-10,15))
lines(c(600:840),WeatherDataSubset_GMM_AR1$mu[1,600:840,5],col = 'brown', lwd = 2,lty = 5)
legend(x = 860,y = 10,legend = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col =c('green','red','brown','purple','blue'))
title('TMIN_Values_Vs_EstimatedClusterMean')
par(mar=c(0.1,4,0.1,20))
plot(c(600:840),new_test_data[1,600:840,2],type = 'l',col = 'red',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',xaxt='n',ylim = c(-5,21))
lines(c(600:840),WeatherDataSubset_GMM_AR1$mu[1,600:840,2],col = 'purple', lwd = 2,lty = 5)
par(mar=c(4,4,0.1,20))
plot(c(600:840),new_test_data[1,600:840,1],type = 'l',col = 'green',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',xaxt='n',ylim = c(-5,20))
lines(c(600:840),new_test_data[1,600:840,2],col = 'red',lwd = 2, lty = 1)
lines(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,600:840,5],col = 'blue', lwd = 2,lty = 2)


#Plotting the Actual Data with estimates
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,20))
plot(c(600:840),new_test_data[2,600:840,1],type = 'l',col = 'green',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',xaxt='n',ylim = c(-2,50))
lines(c(600:840),WeatherDataSubset_GMM_AR1$mu[2,600:840,5],col = 'brown', lwd = 2,lty = 5)
legend(x = 860,y = 40,legend = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col =c('green','red','brown','purple','blue'))
title('TMAX_Values_Vs_EstimatedClusterMean')
par(mar=c(0.1,4,0.1,20))
plot(c(600:840),new_test_data[2,600:840,2],type = 'l',col = 'red',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',xaxt='n',ylim = c(10,50))
lines(c(600:840),WeatherDataSubset_GMM_AR1$mu[2,600:840,2],col = 'purple', lwd = 2,lty = 5)
par(mar=c(4,4,0.1,20))
plot(c(600:840),new_test_data[2,600:840,1],type = 'l',col = 'green',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-2,40))
lines(c(600:840),new_test_data[2,600:840,2],col = 'red',lwd = 2, lty = 1)
lines(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,600:840,5],col = 'blue', lwd = 2,lty = 2)

#Plotting the Actual Data with estimates
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,20))
plot(c(600:840),new_test_data[3,600:840,1],type = 'l',col = 'green',lwd = 2, lty = 1, ylab = 'PRCP', xlab = 'Months',xaxt='n',ylim = c(0,100))
lines(c(600:840),WeatherDataSubset_GMM_AR1$mu[3,600:840,5],col = 'brown', lwd = 2,lty = 5)
legend(x = 860,y = 80,legend = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col =c('green','red','brown','purple','blue'))
title('PRCP_Values_Vs_EstimatedClusterMean')
par(mar=c(0.1,4,0.1,20))
plot(c(600:840),new_test_data[3,600:840,2],type = 'l',col = 'red',lwd = 2, lty = 1, ylab = 'PRCP', xlab = 'Months',xaxt='n',ylim = c(0,110))
lines(c(600:840),WeatherDataSubset_GMM_AR1$mu[3,600:840,2],col = 'purple', lwd = 2,lty = 5)
par(mar=c(4,4,0.1,20))
plot(c(600:840),new_test_data[3,600:840,1],type = 'l',col = 'green',lwd = 2, lty = 1, ylab = 'PRCP', xlab = 'Months',ylim = c(0,110))
lines(c(600:840),new_test_data[3,600:840,2],col = 'red',lwd = 2, lty = 1)
lines(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,600:840,5],col = 'blue', lwd = 2,lty = 2)


#Plotting Mean Estimates
g = 1
par(mfrow = c(3,1))
par(mar=c(0.1,4,3,1))
plot(c(1:100),WeatherDataSubset_GMM_AR1_Sine$mu[1,1:100,1],type = 'l',col = 'green',lwd = 4, lty = 2, ylab = 'TMIN', xlab = 'Months',xaxt='n')
points(c(1:100),SimulatedWeatherData_GMM_AR1$mu[1,1:100,1],col = 'red',pch = 5,type = 'b',lwd = 2, lty = 2)
points(c(1:100),SimulatedWeatherData_GMM_AR1_Sine$mu[1,1:100,1],col = 'blue',pch = 7,type = 'b',lwd = 1, lty = 2)
title('Cluster_1')
par(mar=c(0.1,4,0.1,1))
plot(c(1:100),WeatherDataSubset_GMM_AR1_Sine$mu[2,1:100,1],type = 'l',col = 'green',lwd = 4, lty = 2, ylab = 'TMAX', xlab = 'Months',xaxt ='n')
points(c(1:100),SimulatedWeatherData_GMM_AR1$mu[2,1:100,1],col = 'red',pch = 5,type = 'b',lwd = 2, lty = 2)
points(c(1:100),SimulatedWeatherData_GMM_AR1_Sine$mu[2,1:100,1],col = 'blue',pch = 7,type = 'b',lwd = 1, lty = 2)
par(mar=c(4,4,0.1,1))
plot(c(1:100),WeatherDataSubset_GMM_AR1_Sine$mu[2,1:100,1],type = 'l',col = 'green',lwd = 4, lty = 2, ylab = 'PRCP', xlab = 'Months')
points(c(1:100),SimulatedWeatherData_GMM_AR1$mu[2,1:100,1],col = 'red',pch = 5,type = 'b',lwd = 2, lty = 2)
points(c(1:100),SimulatedWeatherData_GMM_AR1_Sine$mu[2,1:100,1],col = 'blue',pch = 7,type = 'b',lwd = 1, lty = 2)


#Log-Likelihood Plots
load('output_weatherdatafinal_GMM_AR1_Sine_3_G6_conv1e_14.RData')
par(mfrow = c(1,1))
par(mar=c(4,4,3,1))
plot(WeatherDataSubset_GMM_AR1_Sine_3$log_likelihood_list, ylab = 'Log_Likelihood',xlab ='Iterations')
title('Matrix_GMM_AR1_Sine_Rerun_with_Matrix_GMM_AR1_InitParameters')
library(mclust)
load('output_weatherdatafinal_GMM_AR1_Sine_G6_conv1e_14.RData')
adjustedRandIndex(WeatherDataSubset_GMM_AR1_Sine_3$predicted_cluster,WeatherDataSubset_GMM_AR1_Sine_3$init_values)
adjustedRandIndex(WeatherDataSubset_GMM_AR1_Sine_3$predicted_cluster,WeatherDataSubset_GMM_AR1_Sine$predicted_cluster)


#GMM_AR1_Cluster
load('output_weatherdatafinal_GMM_AR1_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#GMM_AR1_Sine_Cluster
load('output_weatherdatafinal_GMM_AR1_Sine_2_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_2$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_2$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_2$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_2$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G = 4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine_2$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map2 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

install.packages('gridExtra')
library(gridExtra)
dev.new(width = 2000, height = 500)
grid.arrange(map1,map2, ncol = 2)


#GMM_AR1_Cluster
load('output_weatherdatafinal_GMM_AR1_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#GMM_AR1_Sine_Cluster
load('output_weatherdatafinal_GMM_AR1_Sine_3_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_3$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_3$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_3$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_3$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G = 4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine_3$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map2 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

install.packages('gridExtra')
library(gridExtra)
dev.new(width = 2000, height = 500)
grid.arrange(map1,map2, ncol = 2)


#GMM_AR1_Cluster
load('output_weatherdatafinal_GMM_AR1_G6_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#GMM_AR1_Sine_Cluster
load('output_weatherdatafinal_GMM_AR1_Sine_3_G6_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_3$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_3$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_3$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_3$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G = 6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine_3$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map2 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

install.packages('gridExtra')
library(gridExtra)
dev.new(width = 2000, height = 500)
grid.arrange(map1,map2, ncol = 2)


#GMM_AR1_Sine_Cluster
load('output_weatherdatafinal_GMM_AR1_Sine_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G = 4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#GMM_AR1_Sine_Cluster_Rerun
load('output_weatherdatafinal_GMM_AR1_Sine_3_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_3$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_3$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_3$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_3$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G = 4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine_3$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map2 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

dev.new(width = 2000, height = 500)
grid.arrange(map1,map2, ncol = 2)


#GMM_AR1_Sine_Cluster
load('output_weatherdatafinal_GMM_AR1_Sine_G6_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G = 6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#GMM_AR1_Sine_Cluster_Rerun
load('output_weatherdatafinal_GMM_AR1_Sine_3_G6_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_3$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_3$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_3$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine_3$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G = 6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine_3$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map2 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

dev.new(width = 2000, height = 500)
grid.arrange(map1,map2, ncol = 2)


#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1$mu[1,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:4)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1$mu[1,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1$mu[2,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:4)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1$mu[2,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1$mu[3,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:4)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1$mu[3,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)

#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1$mu[1,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:4)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1$mu[1,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1$mu[2,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:4)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1$mu[2,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1$mu[3,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:4)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1$mu[3,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)


#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1$mu[1,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:4)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1$mu[1,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1$mu[2,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:4)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1$mu[2,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1$mu[3,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:4)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1$mu[3,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)



#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_G6_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1$mu[1,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:G)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1$mu[1,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1$mu[2,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:G)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1$mu[2,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1$mu[3,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:G)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1$mu[3,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)

#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_G6_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1$mu[1,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:G)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1$mu[1,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1$mu[2,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:G)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1$mu[2,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1$mu[3,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:G)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1$mu[3,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)

#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_G6_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1$mu[1,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:G)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1$mu[1,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1$mu[2,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:G)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1$mu[2,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1$mu[3,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:G)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1$mu[3,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)



#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_G10_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =10
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1$mu[1,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:G)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1$mu[1,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1$mu[2,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:G)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1$mu[2,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1$mu[3,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:G)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1$mu[3,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)

#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_G10_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =10
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1$mu[1,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:G)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1$mu[1,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1$mu[2,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:G)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1$mu[2,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1$mu[3,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:G)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1$mu[3,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)


#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_G10_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =10
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1$mu[1,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:G)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1$mu[1,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1$mu[2,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:G)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1$mu[2,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1$mu[3,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:G)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1$mu[3,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)



#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_Sine_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:4)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:4)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:4)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)

#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_Sine_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:4)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:4)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:4)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)


#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_Sine_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:4)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:4)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:4)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)



#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_Sine_G6_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:G)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:G)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:G)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)

#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_Sine_G6_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:G)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:G)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:G)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)

#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_Sine_G6_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:G)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:G)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:G)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)



#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_Sine_G10_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =10
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:G)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:G)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,600:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:G)){
  lines(c(600:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,600:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)

#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_Sine_G10_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =10
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:G)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:G)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,750:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:G)){
  lines(c(750:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,750:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)


#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_Sine_G10_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =10
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none") 

#Plotting the Actual Data with estimates
plot.new()
par(mfrow = c(3,1),xpd = TRUE)
#legend('topright',labels = c('North West','South West','GMM_AR1_Cluster1','GMM_AR1_Cluster_2','GMM_AR1_Sine_Cluster'),lty = c(1,1,5,5,2),col = 'green','red','brown','purple','blue')
par(mar=c(0.1,4,3,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMIN', xlab = 'Months',ylim = c(-20,25),xaxt ='n')
for(g in c(2:G)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[1,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(0.1,4,0.1,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(-5,50),xaxt ='n')
for(g in c(2:G)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[2,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
par(mar=c(4,4,0.1,1))
plot(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,800:840,1],type = 'l',col = '#00008B',lwd = 2, lty = 1, ylab = 'TMAX', xlab = 'Months',ylim = c(0,90))
for(g in c(2:G)){
  lines(c(800:840),WeatherDataSubset_GMM_AR1_Sine$mu[3,800:840,g],col = colors[g], lwd = 2,lty = 1)
}
map2 = recordPlot()
dev.new(width = 2000, height = 500)
plot_grid(map2,map1,ncol = 2)








#Plotting two cluster results together
#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map2 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none")+ggtitle('Matrix_GMM_AR1')

#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_Sine_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none")+ggtitle('Matrix_GMM_AR1_Sine')

load('output_weatherdatafinal_GMM_AR1_new_Sine_G4_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_new_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_new_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_new_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_new_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =4
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_new_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map3 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none")+ggtitle('Matrix_GMM_AR1_new_Sine')


dev.new(width = 2000, height = 500)
grid.arrange(map1,map3, ncol = 2)

#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_G6_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map2 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none")+ggtitle('Matrix_GMM_AR1')


#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_Sine_G6_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none")+ggtitle('Matrix_GMM_AR1_Sine')
dev.new(width = 2000, height = 500)
grid.arrange(map2,map1, ncol = 2)


#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_G10_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =10
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map2 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none")+ggtitle('Matrix_GMM_AR1')


#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_Sine_G10_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =10
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none")+ggtitle('Matrix_GMM_AR1_Sine')
dev.new(width = 2000, height = 500)
grid.arrange(map2,map1, ncol = 2)



load('output_weatherdatafinal_GMM_AR1_G6_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map2 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none")+ggtitle('Matrix_GMM_AR1')

#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_Sine_G6_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none")+ggtitle('Matrix_GMM_AR1_Sine')


dev.new(width = 2000, height = 500)
grid.arrange(map2,map1, ncol = 2)

load('output_weatherdatafinal_GMM_AR1_new_Sine_G6_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_new_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_new_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_new_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_new_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =6
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_new_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map3 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none")+ggtitle('Matrix_GMM_AR1_new_Sine')
dev.new(width = 2000, height = 500)
grid.arrange(map2,map3, ncol = 2)

dev.new(width = 2000, height = 500)
grid.arrange(map1,map3, ncol = 2)


load('output_weatherdatafinal_GMM_AR1_G10_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =10
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map2 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none")+ggtitle('Matrix_GMM_AR1')

#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_Sine_G10_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =10
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none")+ggtitle('Matrix_GMM_AR1_Sine')


dev.new(width = 2000, height = 500)
grid.arrange(map2,map1, ncol = 2)

load('output_weatherdatafinal_GMM_AR1_new_Sine_G10_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_new_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_new_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_new_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_new_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =10
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_new_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map3 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none")+ggtitle('Matrix_GMM_AR1_new_Sine')
dev.new(width = 2000, height = 500)
grid.arrange(map2,map3, ncol = 2)

dev.new(width = 2000, height = 500)
grid.arrange(map1,map3, ncol = 2)


load('output_weatherdatafinal_GMM_AR1_G14_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =14
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map2 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none")+ggtitle('Matrix_GMM_AR1')

#Plotting Mean Parameters With Maps
load('output_weatherdatafinal_GMM_AR1_Sine_G14_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =14
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none")+ggtitle('Matrix_GMM_AR1_Sine')


dev.new(width = 2000, height = 500)
grid.arrange(map2,map1, ncol = 2)

load('output_weatherdatafinal_GMM_AR1_new_Sine_G14_conv1e_14.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
size = rep(0,1726)
size[which((1-apply(WeatherDataSubset_GMM_AR1_new_Sine$probability,1,max))<0.01)] = 0.8
size[which((1-apply(WeatherDataSubset_GMM_AR1_new_Sine$probability,1,max))>0.01)] = 2
shape = rep(0,1726)
shape[which((1-apply(WeatherDataSubset_GMM_AR1_new_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(WeatherDataSubset_GMM_AR1_new_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,1726)
G =14
for(g in 1:G){
  cluster_colors[which(WeatherDataSubset_GMM_AR1_new_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map3 = ggmap(stamen_tiles)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none")+ggtitle('Matrix_GMM_AR1_new_Sine')
dev.new(width = 2000, height = 500)
grid.arrange(map2,map3, ncol = 2)

dev.new(width = 2000, height = 500)
grid.arrange(map1,map3, ncol = 2)

lists = data.frame()
n = 1
for(g in c(4,6,8,10,12,14)){
  loc = paste('output_weatherdatafinal_GMM_AR1_new_Sine_G',as.character(g),'_conv1e_14.RData',sep = '')
  load(loc)
  for(i in 1:g){
    for(k in 1:5){
      for(p in 1:3){
        if(k==1){
          first = 'a'
        }
        if(k==2){
          first = 'b'
        }
        if(k==3){
          first = 'd'
        }
        if(k==4){
          first = 'e'
        }
        if(k==5){
          first = 'f'
        }
        if(p==1){
          second = 'TMIN'
        }
        if(p == 2){
          second = 'TMAX'
        }
        if(p == 3){
          second = 'PRCP'
        }
        lists = rbind(lists,c(g,i, first, second, WeatherDataSubset_GMM_AR1_Sine$mu_sine_pars[,,g][p,k]))
        n = n + 1
      }
    }
  }
}
lists$X..0.00016978950357913.
Clean_Data_10_0 = lists[lists$X.a.=='a',]
Clean_Data_10_0$X.TMIN. = factor(Clean_Data_10_0$X.TMIN.,levels = c('TMIN','TMAX','PRCP'))
Y1 = ggplot(Clean_Data_10_0, aes(x = X.TMIN., y = as.numeric(X..0.00016978950357913.))) +
  geom_boxplot()+ylab('a_par_values')+xlab('')
Y1
Clean_Data_10_0 = lists[lists$X.a.=='b',]
Clean_Data_10_0$X.TMIN. = factor(Clean_Data_10_0$X.TMIN.,levels = c('TMIN','TMAX','PRCP'))
Y2 = ggplot(Clean_Data_10_0, aes(x = X.TMIN., y = as.numeric(X..0.00016978950357913.))) +
  geom_boxplot()+ylab('b_par_values')+xlab('')
Y2

Clean_Data_10_0 = lists[lists$X.a.=='d',]
Clean_Data_10_0$X.TMIN. = factor(Clean_Data_10_0$X.TMIN.,levels = c('TMIN','TMAX','PRCP'))
Y3 = ggplot(Clean_Data_10_0, aes(x = X.TMIN., y = as.numeric(X..0.00016978950357913.))) +
  geom_boxplot()+ylab('d_par_values')+xlab('')
Y3

Clean_Data_10_0 = lists[lists$X.a.=='e',]
Clean_Data_10_0$X.TMIN. = factor(Clean_Data_10_0$X.TMIN.,levels = c('TMIN','TMAX','PRCP'))
Y4 = ggplot(Clean_Data_10_0, aes(x = X.TMIN., y = as.numeric(X..0.00016978950357913.))) +
  geom_boxplot()+ylab('e_par_values')+xlab('')
Y4

Clean_Data_10_0 = lists[lists$X.a.=='f',]
Clean_Data_10_0$X.TMIN. = factor(Clean_Data_10_0$X.TMIN.,levels = c('TMIN','TMAX','PRCP'))
Y5 = ggplot(Clean_Data_10_0, aes(x = X.TMIN., y = as.numeric(X..0.00016978950357913.))) +
  geom_boxplot()+ylab('f_par_values')+xlab('')
Y5

dev.new(width = 2000, height = 500)
grid.arrange(Y1,Y2,Y3,Y4,Y5, ncol = 2,nrow = 3)
load('output_weatherdatafinal_GMM_AR1_Sine_G4_conv1e_14.RData')
WeatherDataSubset_GMM_AR1_Sine$mu_sine_pars[,,1]

load('Matrix_Weather_Data_Final.RData')
dim(Matrix_Data)
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
load('Manly_Transformed_TestData.RData')
dim(test_data)
loc
A <- loc[(loc$LATITUDE>32)&(loc$LATITUDE<42.5),]
B<-(A[(A$LONGITUDE<= -115)&(A$LONGITUDE> -125),])
B$X
california_data = Matrix_Data[,,B$X]
california_manly_transformed_data = test_data[,,B$X]
dim(california_data)
save(california_data,file = 'california_region_original_data.RData')
save(california_manly_transformed_data,file='california_manly_transformed_data.RData')
write.csv(B,file = 'california_data_locations.csv')
dim(california_data)
dim(california_manly_transformed_data)
max(B$LONGITUDE)
min(B$LONGITUDE)
max(B$LATITUDE)
min(B$LATITUDE)

install.packages('ggmap')
library(raster)
library(ggmap)
library(ggplot2)
register_stadiamaps(key = 'c070816e-74f5-4939-8b49-a0e919de5792')

# Define the bounding box for the area of interest
#Full Image
bbox <- c(left = -180, bottom = 10, right = -60, top = 72)

#Only Mainland America
bbox <- c(left = -125, bottom = 32, right = -114, top = 43)
# Download Stamen map tiles
stamen_tiles <- get_stadiamap(bbox, zoom = 4,labels = FALSE,maptype = "stamen_terrain_background")
# Plot the Stamen map tiles
load('california_manly_transformed_AR1_11.RData')
loc = read.csv('california_data_locations.csv')
size = rep(0,144)
size[which((1-apply(california_manly_transformed_AR1$probability,1,max))<0.01)] = 1.5
size[which((1-apply(california_manly_transformed_AR1$probability,1,max))>0.01)] = 4
shape = rep(0,144)
shape[which((1-apply(california_manly_transformed_AR1$probability,1,max))<0.01)] = 17
shape[which((1-apply(california_manly_transformed_AR1$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,144)
G =11
for(g in 1:G){
  cluster_colors[which(california_manly_transformed_AR1$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ggtitle('AR1_G2')+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')
map2 = ggmap(stamen_tiles)+ggtitle('AR1_G4')+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')
map3 = ggmap(stamen_tiles)+ggtitle('AR1_G6')+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')
map4 = ggmap(stamen_tiles)+ggtitle('AR1_G8')+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')
map5 = ggmap(stamen_tiles)+ggtitle('AR1_G10')+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')
map6 = ggmap(stamen_tiles)+ggtitle('AR1_G11')+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')


load('california_manly_transformed_AR1_Sine_11.RData')
loc = read.csv('california_data_locations.csv')
size = rep(0,144)
size[which((1-apply(california_manly_transformed_AR1_Sine$probability,1,max))<0.01)] = 1.5
size[which((1-apply(california_manly_transformed_AR1_Sine$probability,1,max))>0.01)] = 4
shape = rep(0,144)
shape[which((1-apply(california_manly_transformed_AR1_Sine$probability,1,max))<0.01)] = 17
shape[which((1-apply(california_manly_transformed_AR1_Sine$probability,1,max))>0.01)] = 19
colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,144)
G =11
for(g in 1:G){
  cluster_colors[which(california_manly_transformed_AR1_Sine$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map_sine_1 = ggmap(stamen_tiles)+ggtitle('AR1_Sine_G2')+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')
map_sine_2 = ggmap(stamen_tiles)+ggtitle('AR1_Sine_G4')+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')
map_sine_3 = ggmap(stamen_tiles)+ggtitle('AR1_Sine_G6')+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')
map_sine_4 = ggmap(stamen_tiles)+ggtitle('AR1_Sine_G8')+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')
map_sine_5 = ggmap(stamen_tiles)+ggtitle('AR1_Sine_G10')+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')
map_sine_6 = ggmap(stamen_tiles)+ggtitle('AR1_Sine_G11')+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')
library(gridExtra)
dev.new(width = 2000, height = 500)
grid.arrange(map1,map2,map3,map4,map5,map6,map_sine_1,map_sine_2,map_sine_3,map_sine_4,map_sine_5,map_sine_6, ncol = 6)
library(mclust)
adj_rand = c()
for(g in c(2:11)){
  loc = paste('california_manly_transformed_AR1_',as.character(g),'.RData',sep = '')
  load(loc)
  loc = paste('california_manly_transformed_AR1_Sine_',as.character(g),'.RData',sep = '')
  load(loc)
  adj_rand = c(adj_rand,adjustedRandIndex(california_manly_transformed_AR1$predicted_cluster,california_manly_transformed_AR1_Sine$predicted_cluster))
}
plot(c(2:11),adj_rand,col = 'red',xlab = 'G',ylab = 'ARI')
title('AR1 Vs AR1_Sine ARI')
adj_rand
map_sine_3


# California new sine cluster

# Plot the Stamen map tiles
load('california_AR1_G2.RData')
load('california_AR1_OldSine_G2.RData')
load('california_AR1_NewSine_G2.RData')
loc = read.csv('california_data_locations.csv')
size = rep(0,144)
size[which((1-apply(california_AR1_Sine_G2_faster$probability,1,max))<0.01)] = 1.5
size[which((1-apply(california_AR1_Sine_G2_faster$probability,1,max))>0.01)] = 4
shape = rep(0,144)
shape[which((1-apply(california_AR1_Sine_G2_faster$probability,1,max))<0.01)] = 17
shape[which((1-apply(california_AR1_Sine_G2_faster$probability,1,max))>0.01)] = 19
colors = c('#D2691E','#00008B','#0000FF','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
cluster_colors = rep(0,144)
G =2
for(g in 1:G){
  cluster_colors[which(california_AR1_Sine_G2_faster$predicted_cluster==paste('X',as.character(g),sep = ''))] = colors[g]  
}
dev.new(width = 2000, height = 500)
map1 = ggmap(stamen_tiles)+ggtitle('AR1_G2')+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')
map2 = ggmap(stamen_tiles)+ggtitle('AR1_Sine_G2')+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')
map3 = ggmap(stamen_tiles)+ggtitle('AR1_Sine_LAT_LONG_G2')+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')
map3
library(gridExtra)
dev.new(width = 500, height = 500)
grid.arrange(map1,map2,map3,ncol = 3)
map2

load('california_AR1_G1.RData')
load('california_AR1_OldSine_G1.RData')
load('california_AR1_NewSine_G1.RData')

BIC_AR1 = c(california_AR1_G1$BIC,california_AR1_G2$BIC)
BIC_Sine = c(california_AR1_Old_Sine_G1$BIC,california_AR1_Old_Sine_G2$BIC)
BIC_Sine_LAT_LONG = c(california_AR1_Sine_G1$BIC,california_AR1_Sine_G2_faster$BIC)
plot(c(1,2),BIC_AR1,type = 'p', col = 'red', ylim = c(2000000,2300000),xlab ='G', ylab ='BIC', xaxt ='n')
axis(side=1, at=c(1, 2))
lines(BIC_Sine,type ='p', col = 'blue')
lines(BIC_Sine_LAT_LONG,type ='p', col = 'green')
legend('bottomleft',legend = c('AR1','AR1_Sine','AR1_Sine_LAT_LONG'),col = c('red','blue','green'), pch = c(1,1,1))


table(california_AR1_G2$predicted_cluster,california_AR1_Old_Sine_G2$predicted_cluster)
table(california_AR1_G2$predicted_cluster,california_AR1_Sine_G2_faster$predicted_cluster)
california_AR1_Sine_G2_faster$mu_sine_pars[,,1]
california_AR1_Old_Sine_G2$mu_sine_pars[,,1]


# Plotting
# Plot the Stamen map tiles
for(g in c(2)){
  loc1 = paste('california_AR1_Old_Sine_G',as.character(g),'.csv',sep = '')
  Data = read.csv(loc1)
  loc = read.csv('california_data_locations.csv')
  size = rep(0,144)
  size[which(Data$X.1<0.01)] = 3
  size[which(Data$X.1>0.01)] = 4
  shape = rep(0,144)
  shape[which(Data$X.1<0.01)] = 17
  shape[which(Data$X.1>0.01)] = 19
  colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
  cluster_colors = rep(0,144)
  G = g
  for(k in 1:G){
    cluster_colors[which(Data$predicted_cluster==paste('X',as.character(k),sep = ''))] = colors[k]  
  }
  loc2 = paste('AR1_Sine_G',as.character(g),sep = '')
  map1 = ggmap(stamen_tiles)+ggtitle(loc2)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')
  #loc3 = paste('AR1_Sine_G',as.character(g),'.pdf',sep = '')
  #print(loc3)
  pdf(paste('AR1_Sine_G',as.character(g),'.pdf',sep = ''),10,10)
  map1
  dev.off()
}
dev.off()
library(mclust)
adjustedRandIndex(Data$predicted_cluster,Data2$predicted_cluster)
table(Data$predicted_cluster,Data2$predicted_cluster)

jpeg('temp.jpg',quality = 10000)
map1
dev.off()

map1
loc3 = paste('temp',as.character(2),'.pdf',sep = '')
pdf(file = loc3,10,10)
map1
dev.off()

loc2 = paste('AR1_Sine_G',as.character(2),sep = '')
map1 = ggmap(stamen_tiles)+ggtitle(loc2)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + xlab('') + ylab('')
map1

g = 26
loc1 = paste('california_AR1_Old_Sine_G',as.character(g),'.RData',sep = '')
load(loc1)
table(out$model$predicted_cluster)
plot(out$model$mu_sine_pars[2,5,])
loc2 = paste('california_AR1_Old_Sine_G',as.character(g),'.csv',sep = '')
Data = read.csv(loc2)
loc = read.csv('california_data_locations.csv')
order(table(Data$predicted_cluster))
table(Data$predicted_cluster)
size = rep(0,144)
size[which(Data$X.1<0.01)] = 1
size[which(Data$X.1>0.01)] = 4
shape = rep(0,144)
shape[which(Data$X.1<0.01)] = 17
shape[which(Data$X.1>0.01)] = 19
#colors = c('#00008B','#0000FF','#D2691E','#8B2323','#8B7D6B','#8B008B','#00FFFF','#008B8B','#8B6508','#FFB90F','#FF8C00','#556B2F','#68228B','#2F4F4F','#8B0A50','#FF1493','#00BFFF','#FF0000','#838B8B','#458B74')
colors = c(rep('white',3),'purple',rep('white',3),'magenta',rep('white',8),'grey',rep('white',5),'brown',rep('white',3))
cluster_colors = rep(0,144)
G = g
for(k in 1:G){
  cluster_colors[which(Data$predicted_cluster==paste('X',as.character(k),sep = ''))] = colors[k]  
}
loc2 = paste('AR1_Sine_G',as.character(g),sep = '')
map1 = ggmap(stamen_tiles)+ggtitle(loc2)+ geom_point(data = loc, aes(x = LONGITUDE, y = LATITUDE),color = cluster_colors,size = size, shape = shape) + theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),plot.title = element_text(size = 40, face = "bold")) + xlab('') + ylab('')

map1#loc3 = paste('AR1_Sine_G',as.character(g),'.pdf',sep = '')
#print(loc3)
pdf(paste('AR1_Sine_G',as.character(g),'.pdf',sep = ''),10,10)
map1
dev.off()
theme()
ARI_Sine_Vs_Sine_no_f = c()
for(g in c(2,4,6,8,10,12,14,16,18,20,22,24,26)){
  loc1 = paste('california_AR1_Old_Sine_G',as.character(g),'.RData',sep = '')
  load(loc1)
  data_predicted_cluster = out$model$predicted_cluster
  loc2 = paste('california_AR1_Old_Sine_no_f_G',as.character(g),'.RData',sep = '')
  load(loc2)
  data2_predicted_cluster = out$model$predicted_cluster
  ARI_Sine_Vs_Sine_no_f = c(ARI_Sine_Vs_Sine_no_f,adjustedRandIndex(data_predicted_cluster,data2_predicted_cluster))
}

plot(c(2,4,6,8,10,12,14,16,18,20,22,24,26),ARI_Sine_Vs_Sine_no_f,type = 'p', col = 'red', xlab = 'G', ylab = 'Adjusted Rand index')
title('AR1_Sine_Vs_AR1_Sine_no_f')
warning()
plot(c(2,4,6,8,10,12,14,16,18,20,22,24,26),BIC_AR1_Sine,type = 'p', col = 'red', xlab = 'G', ylab = 'BIC', ylim = c(2120000,2260000))
points(c(2,4,6,8,10,12,14,16,18,20,22,24,26),BIC_AR1_Sine_no_f,col = 'blue')
legend('topright',legend = c('AR1_Sine','AR1_Sine_no_f'),col = c('red','blue'),lty = c(-1,-1),pch = c(1,1))
load('california_AR1_Old_Sine_G10.RData')
out$model$mu_sine_pars

for(g in c(2,4,6,8,10,12,14,16,18,20,22,24,26)){
  loc1 = paste('california_AR1_Old_Sine_G',as.character(g),'.RData',sep = '')
  load(loc1)
  data_predicted_cluster = out$model$predicted_cluster
  loc2 = paste('california_AR1_Old_Sine_no_f_G',as.character(g),'.RData',sep = '')
  load(loc2)
  data2_predicted_cluster = out$model$predicted_cluster
  ARI_Sine_Vs_Sine_no_f = c(ARI_Sine_Vs_Sine_no_f,adjustedRandIndex(data_predicted_cluster,data2_predicted_cluster))
}

table(out$model$predicted_cluster)

plot(out$model$mu_sine_pars[3,5,],col = 'red', ylab = 'PRCP_f', xlab = 'G')


load('california_manly_transformed_data.RData')
plot(california_manly_transformed_data[2,1:840,1])
dim(california_manly_transformed_data)

sum(out$model$mu[1,,21])
which(out$model$predicted_cluster=='X21')
california_manly_transformed_data[1,,91]


plot(out$model$mu[1,,21],type = 'l', col = 'blue')
lines(california_manly_transformed_data[1,,91],col = 'red')

out$model$sigma[,,21]
plot(out$model$probability[,21])
sum()

install.packages('kgc')
library(kgc)

?kgc::CZUncertainty()

california_data = data.frame(num = c(1:144), Longitude = loc$LONGITUDE, Latitude = loc$LATITUDE)

california_data_2 = data.frame(california_data, rndCoord.lon = RoundCoordinates(california_data$Longitude), rndCoord.lat = RoundCoordinates(california_data$Latitude))

california_data_3 = data.frame(california_data_2,ClimateZ = LookupCZ(california_data_2))

california_data_4 = data.frame(california_data_3,CZUncertainty(california_data_3))


table(california_data_3$ClimateZ)
california_data_4$possible.cz



