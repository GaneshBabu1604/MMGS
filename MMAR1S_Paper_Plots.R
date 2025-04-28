library(raster)
library(ggmap)
library(ggplot2)
library(cowplot)

BIC_AR1 = c()
for(g in c(1:25)){
  loc = paste('US_Full_RawData_AR1_Old_Sine_rerun_v2_maxit5_optim_perparam_faster_G',as.character(g),'.RData',sep = '')
  load(loc)
  BIC_AR1 = c(BIC_AR1,out$model$BIC)
}

data = data.frame(x = c(1:25),y = BIC_AR1)
data
dev.new(width = 2000, height = 500)
ggplot(data, aes(x = x, y = y, color = 'red')) +
  geom_point(size = 3,shape = 17) +  # Color gradient
  labs(
    title = "",
    x = "G",
    y = "BIC"
  ) + theme(legend.position = 'None',
    plot.margin = margin(0,0,0,0),panel.background = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 24),
    legend.title = element_text(size = 21),    # Set legend title font size
    legend.text = element_text(size = 20),      # Set legend text font size
    legend.background = element_rect(fill = 'white',color = NA),
    panel.grid.major = element_line(color = "gray80"),                  # Major grid lines in light gray
    panel.grid.minor = element_line(color = "gray90"),                  # Minor grid lines in lighter gray
    panel.border = element_rect(color = "gray50", fill = NA, size = 1) ,panel.spacing = unit(0, "lines")
  ) 

?pch
library(kgc)
#Koppen climate zones

load('US_Full_Data_koppen_classification_Final.RData')
load('US_Full_Data_koppen_classification_uncertainty_Final.RData')
US_Koppen_Classification = as.factor(US_Full_Data_Final$ClimateZ)
US_Koppen_Classification_Types = unique(US_Full_Data_Final$ClimateZ)
shapes = c(rep(15,9),rep(23,9),rep(21,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','chartreuse','orange','deeppink','darkgreen','yellow'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)
loc$Cluster
load('stamen_tiles_mainlandamerica_v2.RData')
load('stamen_tiles_alaska.RData')
A <- ggmap(stamen_tiles_mainlandamerica) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = Cluster, shape = Cluster)) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(title = "", 
       x = "Longitude", 
       y = "Latitude",color = "KGC", shape = "KGC") +  # Add labels and title
  theme(
    legend.position = "right",                 # Position the legend
    legend.title = element_text(size = 16,),    # Set legend title font size
    legend.text = element_text(size = 14),      # Set legend text font size
    plot.background = element_rect(fill = NA, color = NA),  axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16), 
    axis.ticks.length = unit(0.3, "cm")
  )

A
B <- ggmap(stamen_tiles_alaska) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = Cluster, shape = Cluster)) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "") +  # Add labels and title
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA) )

combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.08, y = 0.3, width = 0.3, height = 0.5)  # Adjust the inset position and size

dev.new(width = 2000, height = 500)
combined_plot
US_Koppen_Classification

?pch

# Matrix AR1 Sine G = 20, backward search best model
load('Matrix_AR1_Sine_reduced_parameters_best_model.RData')
current_best_model = current_best_model
#Individual cluster analysis
predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
#old_predicted_cluster = colnames(data.frame(previous_best_model$z_hat))[max.col(data.frame(previous_best_model$z_hat), ties.method = "first")]
US_Koppen_Classification = as.factor(gsub('X','',predicted_cluster))
US_Koppen_Classification_Types = unique(gsub('X','',predicted_cluster))
?pch

shapes = c(rep(15,9),rep(17,9),rep(18,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','chartreuse','orange','deeppink','darkgreen','brown'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)
write.csv(loc,file = 'location_cluster.csv')

US_Koppen_Classification_Types_v2 = c()
for(g in US_Koppen_Classification_Types){
  US_Koppen_Classification_Types_v2 = c(US_Koppen_Classification_Types_v2,gsub('X','',g))
}
loc$Cluster
load('stamen_tiles_mainlandamerica_v2.RData')
load('stamen_tiles_alaska.RData')
A <- ggmap(stamen_tiles_mainlandamerica) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = Cluster, shape = Cluster)) +
  scale_color_manual(values = custom_color, breaks = sort(as.numeric(US_Koppen_Classification_Types))) +    # Apply custom colors
  scale_shape_manual(values = custom_shape, breaks = sort(as.numeric(US_Koppen_Classification_Types))) +    # Apply custom shapes
  labs(title = "", 
       x = "Longitude", 
       y = "Latitude",color = "Cluster", shape = "Cluster") +  # Add labels and title
  theme(
    legend.position = "right",                 # Position the legend
    legend.title = element_text(size = 16,),    # Set legend title font size
    legend.text = element_text(size = 14),      # Set legend text font size
    plot.background = element_rect(fill = NA, color = NA),  axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16), 
    axis.ticks.length = unit(0.3, "cm")
  )

A
library(ggplot2)
library(ggspatial)

B <- ggplot() +
  layer_spatial(alaska_map) +
  coord_sf() +
  theme_minimal() +
  labs(title = "Alaska Map with Latitude and Longitude")

B <- ggmap(stamen_tiles_alaska) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = Cluster, shape = Cluster)) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "") +  # Add labels and title
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA) )

combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.08, y = 0.3, width = 0.3, height = 0.5)  # Adjust the inset position and size

dev.new(width = 2000, height = 500)
combined_plot


# Matrix AR1 Sine G = 20, backward search best model
load('US_Full_RawData_Matrix_AR1_G20.RData')
out$model$predicted_cluster
current_best_model = current_best_model
#Individual cluster analysis
predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
#old_predicted_cluster = colnames(data.frame(previous_best_model$z_hat))[max.col(data.frame(previous_best_model$z_hat), ties.method = "first")]
predicted_cluster = out$model$predicted_cluster
US_Koppen_Classification = as.factor(gsub('X','',predicted_cluster))
US_Koppen_Classification_Types = unique(gsub('X','',predicted_cluster))

shapes = c(rep(15,9),rep(17,9),rep(21,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','chartreuse','orange','deeppink','darkgreen','yellow'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)
US_Koppen_Classification_Types_v2 = c()
for(g in US_Koppen_Classification_Types){
  US_Koppen_Classification_Types_v2 = c(US_Koppen_Classification_Types_v2,gsub('X','',g))
}
loc$Cluster
load('stamen_tiles_mainlandamerica_v2.RData')
load('stamen_tiles_alaska.RData')
A <- ggmap(stamen_tiles_mainlandamerica) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = Cluster, shape = Cluster)) +
  scale_color_manual(values = custom_color, breaks = sort(as.numeric(US_Koppen_Classification_Types))) +    # Apply custom colors
  scale_shape_manual(values = custom_shape, breaks = sort(as.numeric(US_Koppen_Classification_Types))) +    # Apply custom shapes
  labs(title = "", 
       x = "Longitude", 
       y = "Latitude",color = "Cluster", shape = "Cluster") +  # Add labels and title
  theme(
    legend.position = "right",                 # Position the legend
    legend.title = element_text(size = 16,),    # Set legend title font size
    legend.text = element_text(size = 14),      # Set legend text font size
    plot.background = element_rect(fill = NA, color = NA),  axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16), 
    axis.ticks.length = unit(0.3, "cm")
  )

A
B <- ggmap(stamen_tiles_alaska) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = Cluster, shape = Cluster)) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "") +  # Add labels and title
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA) )

combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.08, y = 0.3, width = 0.3, height = 0.5)  # Adjust the inset position and size

dev.new(width = 2000, height = 500)
combined_plot

# Matrix AR1 Sine G = 20, backward search best model
load('US_Full_RawData_Matrix_AR1_G20.RData')
out$model$predicted_cluster
current_best_model = current_best_model
#Individual cluster analysis
#predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
#old_predicted_cluster = colnames(data.frame(previous_best_model$z_hat))[max.col(data.frame(previous_best_model$z_hat), ties.method = "first")]
mclust_cluster = out$model$init_cluster
matrix_AR1_cluster = out$model$predicted_cluster
matrix_AR1_sine_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
koppen = US_Koppen_Classification

US_Koppen_Classification = as.factor(gsub('X','',predicted_cluster))
US_Koppen_Classification_Types = unique(gsub('X','',predicted_cluster))

shapes = c(rep(15,9),rep(17,9),rep(21,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','chartreuse','orange','deeppink','darkgreen','yellow'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)
US_Koppen_Classification_Types_v2 = c()
for(g in US_Koppen_Classification_Types){
  US_Koppen_Classification_Types_v2 = c(US_Koppen_Classification_Types_v2,gsub('X','',g))
}
loc$Cluster
load('stamen_tiles_mainlandamerica_v2.RData')
load('stamen_tiles_alaska.RData')
A <- ggmap(stamen_tiles_mainlandamerica) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = Cluster, shape = Cluster)) +
  scale_color_manual(values = custom_color, breaks = sort(as.numeric(US_Koppen_Classification_Types))) +    # Apply custom colors
  scale_shape_manual(values = custom_shape, breaks = sort(as.numeric(US_Koppen_Classification_Types))) +    # Apply custom shapes
  labs(title = "", 
       x = "Longitude", 
       y = "Latitude",color = "Cluster", shape = "Cluster") +  # Add labels and title
  theme(
    legend.position = "right",                 # Position the legend
    legend.title = element_text(size = 16,),    # Set legend title font size
    legend.text = element_text(size = 14),      # Set legend text font size
    plot.background = element_rect(fill = NA, color = NA),  axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16), 
    axis.ticks.length = unit(0.3, "cm")
  )

A
B <- ggmap(stamen_tiles_alaska) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = Cluster, shape = Cluster)) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "") +  # Add labels and title
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA) )

combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.08, y = 0.3, width = 0.3, height = 0.5)  # Adjust the inset position and size

dev.new(width = 2000, height = 500)
combined_plot



#Amplitude Change in Minimum Temperature
predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
US_Koppen_Classification = as.factor(predicted_cluster)
US_Koppen_Classification_Types = unique(predicted_cluster)
shapes = c(rep(15,9),rep(17,9),rep(19,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','chartreuse','orange','deeppink','darkgreen','yellow'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)

brightness_min_temp = rep(0,1726)
for(g in c(1:20)){
  brightness_min_temp[which(loc$Cluster == paste('X',as.character(g),sep = ''))] = current_best_model$mu_sine_pars[1,1,g]
}


loc$value = brightness_min_temp

A <- ggmap(stamen_tiles_mainlandamerica) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = value),shape = 17, size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(title = "", 
       x = "Longitude", 
       y = "Latitude",color = 'Minimum Temperature (a)') +  # Add labels and title
  theme(
    legend.position = "right",                 # Position the legend
    legend.title = element_text(size = 12),    # Set legend title font size
    legend.text = element_text(size = 10),      # Set legend text font size
    plot.background = element_rect(fill = NA, color = NA),  axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16), 
    axis.ticks.length = unit(0.3, "cm")
  ) 


B <- ggmap(stamen_tiles_alaska) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = value),shape = 17, size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "") +  # Add labels and title
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA) )

combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.4, width = 0.3, height = 0.4)  # Adjust the inset position and size


dev.new(width = 2000, height = 500)
combined_plot

#Amplitude Change in Maximum Temperature
predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
US_Koppen_Classification = as.factor(predicted_cluster)
US_Koppen_Classification_Types = unique(predicted_cluster)
shapes = c(rep(15,9),rep(17,9),rep(19,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','chartreuse','orange','deeppink','darkgreen','yellow'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)


brightness_max_temp = rep(0,1726)
for(g in c(1:20)){
  brightness_max_temp[which(loc$Cluster == paste('X',as.character(g),sep = ''))] = current_best_model$mu_sine_pars[2,1,g]
}


loc$value = brightness_max_temp

A <- ggmap(stamen_tiles_mainlandamerica) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = value),shape = 17, size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(title = "", 
       x = "Longitude", 
       y = "Latitude",color = 'Maximum Temperature (a)') +  # Add labels and title
  theme(
    legend.position = "right",                 # Position the legend
    legend.title = element_text(size = 12),    # Set legend title font size
    legend.text = element_text(size = 10),      # Set legend text font size
    plot.background = element_rect(fill = NA, color = NA),  axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16), 
    axis.ticks.length = unit(0.3, "cm")
  ) 


B <- ggmap(stamen_tiles_alaska) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = value),shape = 17, size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "") +  # Add labels and title
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA) )

combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.4, width = 0.3, height = 0.4)  # Adjust the inset position and size


dev.new(width = 2000, height = 500)
combined_plot


#Amplitude Change in Minimum Temperature
predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
US_Koppen_Classification = as.factor(predicted_cluster)
US_Koppen_Classification_Types = unique(predicted_cluster)
shapes = c(rep(15,9),rep(17,9),rep(19,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','chartreuse','orange','deeppink','darkgreen','yellow'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)

brightness_prcp = rep(0,1726)
for(g in c(1:20)){
  brightness_prcp[which(loc$Cluster == paste('X',as.character(g),sep = ''))] = current_best_model$mu_sine_pars[3,1,g]
}


loc$value = brightness_prcp

A <- ggmap(stamen_tiles_mainlandamerica) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = value),shape = 17, size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(title = "", 
       x = "Longitude", 
       y = "Latitude",color = 'Precipitation (a)') +  # Add labels and title
  theme(
    legend.position = "right",                 # Position the legend
    legend.title = element_text(size = 12),    # Set legend title font size
    legend.text = element_text(size = 10),      # Set legend text font size
    plot.background = element_rect(fill = NA, color = NA),  axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16), 
    axis.ticks.length = unit(0.3, "cm")
  ) 


B <- ggmap(stamen_tiles_alaska) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = value),shape = 17, size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "") +  # Add labels and title
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA) )

combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.4, width = 0.3, height = 0.4)  # Adjust the inset position and size


dev.new(width = 2000, height = 500)
combined_plot


#Vertical Change in Minimum Temperature
predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
US_Koppen_Classification = as.factor(predicted_cluster)
US_Koppen_Classification_Types = unique(predicted_cluster)
shapes = c(rep(15,9),rep(17,9),rep(19,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','chartreuse','orange','deeppink','darkgreen','yellow'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)

brightness_min_temp = rep(0,1726)
for(g in c(1:20)){
  brightness_min_temp[which(loc$Cluster == paste('X',as.character(g),sep = ''))] = current_best_model$mu_sine_pars[1,5,g]
}


loc$value = brightness_min_temp

A <- ggmap(stamen_tiles_mainlandamerica) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = value),shape = 17, size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(title = "", 
       x = "Longitude", 
       y = "Latitude",color = 'Minimum Temperature (e)') +  # Add labels and title
  theme(
    legend.position = "right",                 # Position the legend
    legend.title = element_text(size = 12),    # Set legend title font size
    legend.text = element_text(size = 10),      # Set legend text font size
    plot.background = element_rect(fill = NA, color = NA),  axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16), 
    axis.ticks.length = unit(0.3, "cm")
  ) 


B <- ggmap(stamen_tiles_alaska) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = value),shape = 17, size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "") +  # Add labels and title
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA) )

combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.4, width = 0.3, height = 0.4)  # Adjust the inset position and size


dev.new(width = 2000, height = 500)
combined_plot

#Vertical Change in Maximum Temperature
predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
US_Koppen_Classification = as.factor(predicted_cluster)
US_Koppen_Classification_Types = unique(predicted_cluster)
shapes = c(rep(15,9),rep(17,9),rep(19,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','black','orange','deeppink','darkgreen','yellow'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)


brightness_max_temp = rep(0,1726)
for(g in c(1:20)){
  brightness_max_temp[which(loc$Cluster == paste('X',as.character(g),sep = ''))] = current_best_model$mu_sine_pars[2,5,g]
}


loc$value = brightness_max_temp

A <- ggmap(stamen_tiles_mainlandamerica) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = value),shape = 17, size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(title = "", 
       x = "Longitude", 
       y = "Latitude",color = 'Maximum Temperature (e)') +  # Add labels and title
  theme(
    legend.position = "right",                 # Position the legend
    legend.title = element_text(size = 12),    # Set legend title font size
    legend.text = element_text(size = 10),      # Set legend text font size
    plot.background = element_rect(fill = NA, color = NA),  axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16), 
    axis.ticks.length = unit(0.3, "cm")
  ) 


B <- ggmap(stamen_tiles_alaska) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = value),shape = 17, size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "") +  # Add labels and title
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA) )

combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.4, width = 0.3, height = 0.4)  # Adjust the inset position and size


dev.new(width = 2000, height = 500)
combined_plot


#Vertical Change in Precipiation
predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
US_Koppen_Classification = as.factor(predicted_cluster)
US_Koppen_Classification_Types = unique(predicted_cluster)
shapes = c(rep(15,9),rep(17,9),rep(19,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','chartreuse','orange','deeppink','darkgreen','yellow'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)

brightness_prcp = rep(0,1726)
for(g in c(1:20)){
  brightness_prcp[which(loc$Cluster == paste('X',as.character(g),sep = ''))] = current_best_model$mu_sine_pars[3,5,g]
}


loc$value = brightness_prcp

A <- ggmap(stamen_tiles_mainlandamerica) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = value),shape = 17, size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(title = "", 
       x = "Longitude", 
       y = "Latitude",color = 'Precipitation (e)') +  # Add labels and title
  theme(
    legend.position = "right",                 # Position the legend
    legend.title = element_text(size = 12),    # Set legend title font size
    legend.text = element_text(size = 10),      # Set legend text font size
    plot.background = element_rect(fill = NA, color = NA),  axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16), 
    axis.ticks.length = unit(0.3, "cm")
  ) 


B <- ggmap(stamen_tiles_alaska) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = value),shape = 17, size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "") +  # Add labels and title
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA) )

combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.4, width = 0.3, height = 0.4)  # Adjust the inset position and size


dev.new(width = 2000, height = 500)
combined_plot


library(mclust)
adjustedRandIndex(koppen,mclust_cluster)
adjustedRandIndex(koppen,matrix_AR1_cluster)
adjustedRandIndex(koppen,matrix_AR1_sine_cluster)


adjustedRandIndex(matrix_AR1_sine_cluster,mclust_cluster)
adjustedRandIndex(matrix_AR1_sine_cluster,matrix_AR1_cluster)


min(loc[loc$Cluster == '14',4])
max(loc[loc$Cluster == '14',4])
min(loc[loc$Cluster == '14',5])
max(loc[loc$Cluster == '14',5])


#Cluster Level Plots

#One Year
library(reshape2)
g = 20
data <- data.frame(
  x1 = c(1:12),
  '1946-47' = current_best_model$mu[1,1:12,g],
  x2 = c(409:420),
  '1969-70' = current_best_model$mu[1,409:420,g],
  x3 = c(829:840),
  '2014-15' = current_best_model$mu[1,829:840,g]
)


data_long_1 <- melt(data, id.vars = c("x1", "x2", "x3"), variable.name = "Year", value.name = "y")


data <- data.frame(
  x1 = c(1:12),
  '1946-47' = current_best_model$mu[2,1:12,g],
  x2 = c(409:420),
  '1969-70' = current_best_model$mu[2,409:420,g],
  x3 = c(829:840),
  '2014-15' = current_best_model$mu[2,829:840,g]
)


data_long_2 <- melt(data, id.vars = c("x1", "x2", "x3"), variable.name = "Year", value.name = "y")

data <- data.frame(
  x1 = c(1:12),
  '1946-47' = current_best_model$mu[3,1:12,g],
  x2 = c(409:420),
  '1969-70' = current_best_model$mu[3,409:420,g],
  x3 = c(829:840),
  '2014-15' = current_best_model$mu[3,829:840,g]
)


data_long_3 <- melt(data, id.vars = c("x1", "x2", "x3"), variable.name = "Year", value.name = "y")



A = ggplot(data_long_1, aes(x = x1, y = y, color = Year)) +
  scale_color_manual(values = c('red','blue','green'),name = 'Year', labels = c('1946','1970','2015'))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
  geom_line() +
  labs(title = paste("Cluster ",as.character(g),sep =''), x = "", y = "Minimum Temperature(°C)") + theme(
    axis.ticks.x = element_blank(),  # Removes x-axis ticks
    axis.text.x = element_blank(),
    plot.margin = margin(t = 0, b = 0)
  )

B = ggplot(data_long_2, aes(x = x1, y = y, color = Year)) +
  scale_color_manual(values = c('red','blue','green'),name = 'Year', labels = c('1946','1970','2015'))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
  geom_line() +
  labs(title = '', x = "", y = "Maximum Temperature(°C)") + theme(
    axis.ticks.x = element_blank(),  # Removes x-axis ticks
    axis.text.x = element_blank(),
    plot.margin = margin(t = 0, b = 0)
  )

C = ggplot(data_long_3, aes(x = x1, y = y, color = Year)) +
  scale_color_manual(values = c('red','blue','green'),name = 'Year', labels = c('1946','1970','2015'))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
  geom_line() +
  labs(title = '', x = "", y = "Precipitation(mm)")+ theme(
    plot.margin = margin(t = 0)
  )

library(agricolae)
library(patchwork)

D = A/B/C + plot_layout(heights = c(7,7,5))
dev.new(width = 2000, height = 500)
D


(data_long_1[25:36,'y']-data_long_1[1:12,'y'])[1:3]
(data_long_1[25:36,'y']-data_long_1[1:12,'y'])[4:6]
(data_long_1[25:36,'y']-data_long_1[1:12,'y'])[7:9]
(data_long_1[25:36,'y']-data_long_1[1:12,'y'])[10:12]

(data_long_2[25:36,'y']-data_long_2[1:12,'y'])[1:3]
(data_long_2[25:36,'y']-data_long_2[1:12,'y'])[4:6]
(data_long_2[25:36,'y']-data_long_2[1:12,'y'])[7:9]
(data_long_2[25:36,'y']-data_long_2[1:12,'y'])[10:12]

(data_long_3[25:36,'y']-data_long_3[1:12,'y'])[1:3]
(data_long_3[25:36,'y']-data_long_3[1:12,'y'])[4:6]
(data_long_3[25:36,'y']-data_long_3[1:12,'y'])[7:9]
(data_long_3[25:36,'y']-data_long_3[1:12,'y'])[10:12]


sum(data_long_3[c(28:33),'y']-data_long_3[c(4:9),'y'])/sum(data_long_3[c(4:9),'y'])


ggmap::register_google('AIzaSyAe8ti_rtLNY4hZDSgop5GF2C_TbrMEcOc')

# Load required libraries
library(terra)

# Load the PNG image without georeferencing info
alaska_image <- rast("alaska.png")

# Set the extent manually to approximate bounds of Alaska
ext(alaska_image) <- c(-172, -130, 54, 71)  # xmin, xmax, ymin, ymax



# Define CRS (Coordinate Reference System) for WGS84
crs(alaska_image) <- "EPSG:4326"  # WGS84

# Plot the map with geographic referencing
plot(alaska_image)

loc_new = loc[which(loc$LONGITUDE<-129&loc$LATITUDE>50),]

# Install and load required libraries
install.packages("png")
install.packages("grid")

library(png)
library(grid)

# Load the PNG image (replace with the actual path to the file)
img <- readPNG("alaska.png")


# Plot the image
grid.raster(img)

# Install and load the raster package
install.packages("raster")
library(raster)

# Load the GeoTIFF map (replace with the actual file path)
raster_map <- raster("alaska.tif")
library(tiff)
img <- readTIFF("alaska.tif", as.is = TRUE)
extent(raster_map) <- c(-172, -130, 54, 71)
crs(raster_map) <- "EPSG:4326"  # WGS84
# Plot the raster map
plot(raster_map$alaska_1)

ext(alaska_image) <- c(-172, -130, 54, 71)
B <- ggplot() +
  annotation_raster(img, xmin = lon_min, xmax = lon_max, ymin = lat_min, ymax = lat_max)+
  geom_point(data = loc_new, 
             aes(x = LONGITUDE, y = LATITUDE, color = Cluster, shape = Cluster)) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "") +  # Add labels and title
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA) )


B
B <- ggmap(stamen_tiles_alaska) +
  geom_point(data = loc, 
             aes(x = LONGITUDE, y = LATITUDE, color = Cluster, shape = Cluster)) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "") +  # Add labels and title
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA) )


# Load required libraries
library(ggplot2)
library(png)
library(grid)

lat_lon_to_mercator <- function(lon, lat) {
  print(c(lat,lon))
  # Convert latitude and longitude to radians
  lat_rad <- lat * pi / 180
  lon_rad <- lon * pi / 180
  
  # Earth's radius in meters (Web Mercator)
  R <- 6378137
  
  # Mercator Projection (x, y)
  x <- lon_rad * R
  y <- log(tan(pi / 4 + lat_rad / 2)) * R
  
  return(c(x, y))
}

# Earth's radius for Web Mercator projection in meters
earth_radius <- 6378137

# Function to convert Mercator x, y to latitude and longitude
mercator_to_latlon <- function(x, y) {
  # Longitude
  lon <- x / earth_radius
  
  # Latitude
  lat <- atan(sinh(y / earth_radius))
  
  # Convert radians to degrees
  lon_deg <- lon * (180 / pi)
  lat_deg <- lat * (180 / pi)
  
  return(c(lon_deg, lat_deg))
}

# Example usage
# Mercator coordinates (x, y) in meters
mercator_to_latlon(0, 0)              # Expected: c(0, 0) at the equator and prime meridian
mercator_to_latlon(1000000, 1000000) # Example Mercator coordinates


#Matrix_AR1_Sine
load('Matrix_AR1_Sine_reduced_parameters_best_model.RData')
current_best_model = current_best_model
#Individual cluster analysis
predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
#old_predicted_cluster = colnames(data.frame(previous_best_model$z_hat))[max.col(data.frame(previous_best_model$z_hat), ties.method = "first")]
US_Koppen_Classification = as.factor(gsub('X','',predicted_cluster))
US_Koppen_Classification_Types = unique(gsub('X','',predicted_cluster))
shapes = c(rep(15,9),rep(17,9),rep(19,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','black','orange','deeppink','darkgreen','brown'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)
head(loc)
# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[5][1]), as.numeric(x[4][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

# Load your PNG image of Alaska
img <- readPNG("Alaska.png")  # Replace with the path to your image file

loc_new = loc[which(loc$LONGITUDE<-130&loc$LATITUDE>50),]
lat_lon_to_mercator(71.5,-129.7)

B <- ggplot() +
  annotation_raster(img, xmin = -19169216, xmax = -14438138, ymin = 7170156, ymax = 11575440)+
  geom_point(data = loc_new, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = Cluster, shape = Cluster),size = 2) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0, l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(67*pi/180))+ coord_cartesian(ylim = c(7170156, 11575440), xlim = c(-19169216, -14438138))
dev.new(width = 2000, height = 500)
B


mercator_to_latlon(-19169216,11575440)
lat_lon_to_mercator(-129.7,54)

# Load your PNG image of Alaska
img <- readPNG("hawaii.png")  # Replace with the path to your image file

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)
# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[5][1]), as.numeric(x[4][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x
lat_lon_to_mercator(22.3,-154.75)
loc_new = loc[which(loc$LONGITUDE<-154.75&loc$LATITUDE<22.3),]

C <- ggplot() +
  annotation_raster(img, xmin = -17794421, xmax = -17226691, ymin = 2137284, ymax = 2547582)+
  geom_point(data = loc_new, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = Cluster, shape = Cluster),size = 2) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0,l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(20.8*pi/180))+ coord_cartesian(ylim = c(2137284, 2547582), xlim = c(-17794421, -17226691))     # Set the y-axis limits
dev.new(width = 2000, height = 500)
C

mercator_to_latlon(-17226691,2547582)
lat_lon_to_mercator(-129.7,54)

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)

# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[5][1]), as.numeric(x[4][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

# Load your PNG image of Alaska
img <- readPNG("usa.png")  # Replace with the path to your image file

loc_new = loc[which(loc$LONGITUDE>-125&loc$LATITUDE<50),]
A <- ggplot() +
  annotation_raster(img, ymin = 2863467,ymax = 6360131, xmin = -16419625, xmax = -7436142)+
  geom_point(data = loc_new, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = Cluster, shape = Cluster),size = 2) +
  scale_color_manual(values = custom_color, breaks = sort(as.numeric(US_Koppen_Classification_Types))) +    # Apply custom colors
  scale_shape_manual(values = custom_shape, breaks = sort(as.numeric(US_Koppen_Classification_Types))) +    # Apply custom shapes
  labs(x = "", 
       y = "")+ theme_void()+ theme(  legend.position = "right",                 # Position the legend
                                       legend.title = element_text(size = 26),    # Set legend title font size
                                       legend.text = element_text(size = 22),      # Set legend text font size
         plot.margin = margin(t = 0, b = 0,l = 0, r = 0), legend.margin = margin(t = 0, r = 0, b = 0, l = -45),  # Adjusts padding within the legend box
         legend.spacing = unit(0, "cm")    
       ) + coord_equal(ratio = 1 )+ coord_cartesian(ylim = c(2863467, 6360131), xlim = c(-16419625, -7436142))     # Set the y-axis limits
dev.new(width = 2000, height = 500)
A
mercator_to_latlon(-7436142,6360131)
lat_lon_to_mercator(-129.7,54)

combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.5, width = 0.25, height = 0.4)+
  draw_plot(C,x = 0.07,y = 0.2, width = 0.15, height = 0.2)
dev.new(width = 2000, height = 500)
combined_plot

#Mclust
load('US_Full_RawData_Matrix_AR1_G20.RData')
out$model$predicted_cluster
current_best_model = current_best_model
#Individual cluster analysis
#predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
#old_predicted_cluster = colnames(data.frame(previous_best_model$z_hat))[max.col(data.frame(previous_best_model$z_hat), ties.method = "first")]
mclust_cluster = out$model$init_cluster
matrix_AR1_cluster = out$model$predicted_cluster
koppen = US_Koppen_Classification

US_Koppen_Classification = as.factor(gsub('X','',mclust_cluster))
US_Koppen_Classification_Types = unique(gsub('X','',mclust_cluster))

shapes = c(rep(15,9),rep(17,9),rep(19,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','black','orange','deeppink','darkgreen','brown'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[4][1]), as.numeric(x[5][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

# Load your PNG image of Alaska
img <- readPNG("Alaska.png")  # Replace with the path to your image file

B <- ggplot() +
  annotation_raster(img, xmin = -19169216, xmax = -14438138, ymin = 7170156, ymax = 11575440)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = Cluster, shape = Cluster),size = 2) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0, l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(67*pi/180))+ coord_cartesian(ylim = c(7170156, 11575440), xlim = c(-19169216, -14438138))
dev.new(width = 2000, height = 500)
B


# Load your PNG image of Alaska
img <- readPNG("hawaii.png")  # Replace with the path to your image file

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)
# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[4][1]), as.numeric(x[5][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

C <- ggplot() +
  annotation_raster(img, xmin = -17794421, xmax = -17226691, ymin = 2137284, ymax = 2547582)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = Cluster, shape = Cluster),size = 2) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0,l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(20.8*pi/180))+ coord_cartesian(ylim = c(2137284, 2547582), xlim = c(-17794421, -17226691))     # Set the y-axis limits
dev.new(width = 2000, height = 500)
C

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)

# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[4][1]), as.numeric(x[5][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

# Load your PNG image of Alaska
img <- readPNG("usa.png")  # Replace with the path to your image file

A <- ggplot() +
  annotation_raster(img, ymin = 2863467,ymax = 6360131, xmin = -16419625, xmax = -7436142)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = Cluster, shape = Cluster),size = 2) +
  scale_color_manual(values = custom_color, breaks = sort(as.numeric(US_Koppen_Classification_Types))) +    # Apply custom colors
  scale_shape_manual(values = custom_shape, breaks = sort(as.numeric(US_Koppen_Classification_Types))) +    # Apply custom shapes
  labs(x = "", 
       y = "")+ theme_void()+ theme(  legend.position = "right",                 # Position the legend
                                      legend.title = element_text(size = 26),    # Set legend title font size
                                      legend.text = element_text(size = 22),      # Set legend text font size
                                      plot.margin = margin(t = 0, b = 0,l = 0, r = 0), legend.margin = margin(t = 0, r = 0, b = 0, l = -45),  # Adjusts padding within the legend box
                                      legend.spacing = unit(0, "cm")    
       ) + coord_equal(ratio = 1 )+ coord_cartesian(ylim = c(2863467, 6360131), xlim = c(-16419625, -7436142))     # Set the y-axis limits
dev.new(width = 2000, height = 500)
A
combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.5, width = 0.25, height = 0.4)+
  draw_plot(C,x = 0.07,y = 0.2, width = 0.15, height = 0.2)
dev.new(width = 2000, height = 500)
combined_plot

#Matrix_AR1
load('US_Full_RawData_Matrix_AR1_G20.RData')
out$model$predicted_cluster
current_best_model = current_best_model
#Individual cluster analysis
#predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
#old_predicted_cluster = colnames(data.frame(previous_best_model$z_hat))[max.col(data.frame(previous_best_model$z_hat), ties.method = "first")]
mclust_cluster = out$model$init_cluster
matrix_AR1_cluster = out$model$predicted_cluster
koppen = US_Koppen_Classification

US_Koppen_Classification = as.factor(gsub('X','',matrix_AR1_cluster))
US_Koppen_Classification_Types = unique(gsub('X','',matrix_AR1_cluster))

shapes = c(rep(15,9),rep(17,9),rep(19,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','black','orange','deeppink','darkgreen','brown'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[4][1]), as.numeric(x[5][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

# Load your PNG image of Alaska
img <- readPNG("Alaska.png")  # Replace with the path to your image file

B <- ggplot() +
  annotation_raster(img, xmin = -19169216, xmax = -14438138, ymin = 7170156, ymax = 11575440)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = Cluster, shape = Cluster),size = 2) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0, l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(67*pi/180))+ coord_cartesian(ylim = c(7170156, 11575440), xlim = c(-19169216, -14438138))
dev.new(width = 2000, height = 500)
B


# Load your PNG image of Alaska
img <- readPNG("hawaii.png")  # Replace with the path to your image file

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)
# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[4][1]), as.numeric(x[5][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

C <- ggplot() +
  annotation_raster(img, xmin = -17794421, xmax = -17226691, ymin = 2137284, ymax = 2547582)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = Cluster, shape = Cluster),size = 2) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0,l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(20.8*pi/180))+ coord_cartesian(ylim = c(2137284, 2547582), xlim = c(-17794421, -17226691))     # Set the y-axis limits
dev.new(width = 2000, height = 500)
C

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)

# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[4][1]), as.numeric(x[5][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

# Load your PNG image of Alaska
img <- readPNG("usa.png")  # Replace with the path to your image file

A <- ggplot() +
  annotation_raster(img, ymin = 2863467,ymax = 6360131, xmin = -16419625, xmax = -7436142)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = Cluster, shape = Cluster),size = 2) +
  scale_color_manual(values = custom_color, breaks = sort(as.numeric(US_Koppen_Classification_Types))) +    # Apply custom colors
  scale_shape_manual(values = custom_shape, breaks = sort(as.numeric(US_Koppen_Classification_Types))) +    # Apply custom shapes
  labs(x = "", 
       y = "")+ theme_void()+ theme(  legend.position = "right",                 # Position the legend
                                      legend.title = element_text(size = 26),    # Set legend title font size
                                      legend.text = element_text(size = 22),      # Set legend text font size
                                      plot.margin = margin(t = 0, b = 0,l = 0, r = 0), legend.margin = margin(t = 0, r = 0, b = 0, l = -45),  # Adjusts padding within the legend box
                                      legend.spacing = unit(0, "cm")    
       ) + coord_equal(ratio = 1 )+ coord_cartesian(ylim = c(2863467, 6360131), xlim = c(-16419625, -7436142))     # Set the y-axis limits
dev.new(width = 2000, height = 500)
A
combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.5, width = 0.25, height = 0.4)+
  draw_plot(C,x = 0.07,y = 0.2, width = 0.15, height = 0.2)
dev.new(width = 2000, height = 500)
combined_plot

#Koppen climate zones
load('US_Full_Data_koppen_classification_Final.RData')
load('US_Full_Data_koppen_classification_uncertainty_Final.RData')
US_Koppen_Classification = as.factor(US_Full_Data_Final$ClimateZ)
US_Koppen_Classification_Types = unique(US_Full_Data_Final$ClimateZ)
shapes = c(rep(15,9),rep(17,9),rep(19,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','black','orange','deeppink','darkgreen','brown'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[4][1]), as.numeric(x[5][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

# Load your PNG image of Alaska
img <- readPNG("Alaska.png")  # Replace with the path to your image file

B <- ggplot() +
  annotation_raster(img, xmin = -19169216, xmax = -14438138, ymin = 7170156, ymax = 11575440)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = Cluster, shape = Cluster),size = 2) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0, l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(67*pi/180))+ coord_cartesian(ylim = c(7170156, 11575440), xlim = c(-19169216, -14438138))
dev.new(width = 2000, height = 500)
B


# Load your PNG image of Alaska
img <- readPNG("hawaii.png")  # Replace with the path to your image file

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)
# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[4][1]), as.numeric(x[5][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

C <- ggplot() +
  annotation_raster(img, xmin = -17794421, xmax = -17226691, ymin = 2137284, ymax = 2547582)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = Cluster, shape = Cluster),size = 2) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0,l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(20.8*pi/180))+ coord_cartesian(ylim = c(2137284, 2547582), xlim = c(-17794421, -17226691))     # Set the y-axis limits
dev.new(width = 2000, height = 500)
C

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)

# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[4][1]), as.numeric(x[5][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

# Load your PNG image of Alaska
img <- readPNG("usa.png")  # Replace with the path to your image file

A <- ggplot() +
  annotation_raster(img, ymin = 2863467,ymax = 6360131, xmin = -16419625, xmax = -7436142)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = Cluster, shape = Cluster),size = 2) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes   # Apply custom shapes
  labs(x = "", 
       y = "",color = "KGC zone", shape = "KGC zone")+ theme_void()+ theme(  legend.position = "right",                 # Position the legend
                                      legend.title = element_text(size = 26),    # Set legend title font size
                                      legend.text = element_text(size = 22),      # Set legend text font size
                                      plot.margin = margin(t = 0, b = 0,l = 0, r = 0), legend.margin = margin(t = 0, r = 0, b = 0, l = -45),  # Adjusts padding within the legend box
                                      legend.spacing = unit(0, "cm")    
       ) + coord_equal(ratio = 1 )+ coord_cartesian(ylim = c(2863467, 6360131), xlim = c(-16419625, -7436142))     # Set the y-axis limits
dev.new(width = 2000, height = 500)
A
combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.5, width = 0.25, height = 0.4)+
  draw_plot(C,x = 0.07,y = 0.2, width = 0.15, height = 0.2)
dev.new(width = 2000, height = 500)
combined_plot

#Weather Stations
load('US_Full_Data_koppen_classification_Final.RData')
load('US_Full_Data_koppen_classification_uncertainty_Final.RData')
US_Koppen_Classification = as.factor(US_Full_Data_Final$ClimateZ)
US_Koppen_Classification_Types = unique(US_Full_Data_Final$ClimateZ)
shapes = c(rep(17,9),rep(17,9),rep(17,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('black','black','black','black','black','black','black','black','black','black'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[4][1]), as.numeric(x[5][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

# Load your PNG image of Alaska
img <- readPNG("Alaska.png")  # Replace with the path to your image file

B <- ggplot() +
  annotation_raster(img, xmin = -19169216, xmax = -14438138, ymin = 7170156, ymax = 11575440)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = Cluster, shape = Cluster),size = 2) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0, l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(67*pi/180))+ coord_cartesian(ylim = c(7170156, 11575440), xlim = c(-19169216, -14438138))
dev.new(width = 2000, height = 500)
B
C <- ggplot() +
  annotation_raster(img, xmin = -17794421, xmax = -17226691, ymin = 2137284, ymax = 2547582)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = Cluster, shape = Cluster),size = 2) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0,l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(20.8*pi/180))+ coord_cartesian(ylim = c(2137284, 2547582), xlim = c(-17794421, -17226691))     # Set the y-axis limits
dev.new(width = 2000, height = 500)
C

# Load your PNG image of Alaska
img <- readPNG("hawaii.png")  # Replace with the path to your image file

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)
# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[4][1]), as.numeric(x[5][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

C <- ggplot() +
  annotation_raster(img, xmin = -17794421, xmax = -17226691, ymin = 2137284, ymax = 2547582)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = Cluster, shape = Cluster),size = 2) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0,l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(20.8*pi/180))+ coord_cartesian(ylim = c(2137284, 2547582), xlim = c(-17794421, -17226691))     # Set the y-axis limits
dev.new(width = 2000, height = 500)
C

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)

# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[4][1]), as.numeric(x[5][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

# Load your PNG image of Alaska
img <- readPNG("usa.png")  # Replace with the path to your image file

A <- ggplot() +
  annotation_raster(img, ymin = 2863467,ymax = 6360131, xmin = -16419625, xmax = -7436142)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = Cluster, shape = Cluster),size = 2) +
  scale_color_manual(values = custom_color) +    # Apply custom colors
  scale_shape_manual(values = custom_shape) +    # Apply custom shapes   # Apply custom shapes
  labs(x = "", 
       y = "",color = "KGC", shape = "KGC")+ theme_void()+ theme(  legend.position = "none",                 # Position the legend
                                                                   legend.title = element_text(size = 26),    # Set legend title font size
                                                                   legend.text = element_text(size = 22),      # Set legend text font size
                                                                   plot.margin = margin(t = 0, b = 0,l = 0, r = 0), legend.margin = margin(t = 0, r = 0, b = 0, l = -45),  # Adjusts padding within the legend box
                                                                   legend.spacing = unit(0, "cm")    
       ) + coord_equal(ratio = 1 )+ coord_cartesian(ylim = c(2863467, 6360131), xlim = c(-16419625, -7436142))     # Set the y-axis limits
dev.new(width = 2000, height = 500)
A
combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.5, width = 0.25, height = 0.4)+
  draw_plot(C,x = 0.07,y = 0.2, width = 0.15, height = 0.2)
dev.new(width = 2000, height = 500)
combined_plot
load('Matrix_AR1_Sine_reduced_parameters_best_model.RData')
which(c(current_best_model$mu_sine_pars[,c(5:5),c(1:20)])==0)

#Amplitude Change in Minimum Temperature
load('Matrix_AR1_Sine_reduced_parameters_best_model.RData')
current_best_model = final_best_model
predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
US_Koppen_Classification = as.factor(predicted_cluster)
US_Koppen_Classification_Types = unique(predicted_cluster)
shapes = c(rep(15,9),rep(17,9),rep(19,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','chartreuse','orange','deeppink','darkgreen','yellow'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)

# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[5][1]), as.numeric(x[4][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

brightness_min_temp = rep(0,1726)
for(g in c(1:20)){
  brightness_min_temp[which(loc$Cluster == paste('X',as.character(g),sep = ''))] = current_best_model$mu_sine_pars[1,1,g]
}


loc$value = brightness_min_temp

# Load your PNG image of Alaska
img <- readPNG("usa_Gray.png")  # Replace with the path to your image file
A <- ggplot() +
  annotation_raster(img, ymin = 2863467,ymax = 6360131, xmin = -16419625, xmax = -7436142)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color =  value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "",color = '')+ theme_void()+ theme(  legend.position = "right",                 # Position the legend
                                                 legend.title = element_text(size = 26),    # Set legend title font size
                                                 legend.text = element_text(size = 22),      # Set legend text font size
                                                 plot.margin = margin(t = 0, b = 0,l = 0, r = 0), legend.margin = margin(t = 0, r = 0, b = 0, l = -45),  # Adjusts padding within the legend box
                                                 legend.spacing = unit(0, "cm")    
       ) + coord_equal(ratio = 1)+ coord_cartesian(ylim = c(2863467, 6360131), xlim = c(-16419625, -7436142))     # Set the y-axis limits

img <- readPNG("Alaska_Gray.png")  # Replace with the path to your image file

B <- ggplot() +
  annotation_raster(img, xmin = -19169216, xmax = -14438138, ymin = 7170156, ymax = 11575440)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0, l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(67*pi/180))+ coord_cartesian(ylim = c(7170156, 11575440), xlim = c(-19169216, -14438138))

img <- readPNG("hawaii_Gray.png")  # Replace with the path to your image file

C <- ggplot() +
  annotation_raster(img, xmin = -17794421, xmax = -17226691, ymin = 2137284, ymax = 2547582)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0,l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(20.8*pi/180))+ coord_cartesian(ylim = c(2137284, 2547582), xlim = c(-17794421, -17226691))     # Set the y-axis limits

img = readPNG('a_hat_img.png')

g <- rasterGrob(img, interpolate=TRUE)
g
D = ggplot() +
  annotation_custom(g, xmin=0, xmax=0.1, ymin=0, ymax=0.1) +
  geom_point()+xlim(0,0.1)+ylim(0,0.1)+theme_void() + theme(
    legend.position = "none",
    plot.margin = margin(t = 0, b = 0,l = 0, r = 0),panel.border = element_rect(color = "white", fill = NA, linewidth = 0.01)
  ) 

combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.5, width = 0.25, height = 0.4)+
  draw_plot(C,x = 0.07,y = 0.2, width = 0.15, height = 0.2)+
  draw_plot(D,x = 0.878,y = 0.55, width = 0.075, height = 0.075)
dev.new(width = 2000, height = 500)
combined_plot


#Amplitude Change in Maximum Temperature
predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
US_Koppen_Classification = as.factor(predicted_cluster)
US_Koppen_Classification_Types = unique(predicted_cluster)
shapes = c(rep(15,9),rep(17,9),rep(19,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','chartreuse','orange','deeppink','darkgreen','yellow'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)

# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[5][1]), as.numeric(x[4][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

brightness_max_temp = rep(0,1726)
for(g in c(1:20)){
  brightness_max_temp[which(loc$Cluster == paste('X',as.character(g),sep = ''))] = current_best_model$mu_sine_pars[2,1,g]
}


loc$value = brightness_max_temp


# Load your PNG image of Alaska
img <- readPNG("usa_Gray.png")  # Replace with the path to your image file
A <- ggplot() +
  annotation_raster(img, ymin = 2863467,ymax = 6360131, xmin = -16419625, xmax = -7436142)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color =  value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "",color = '')+ theme_void()+ theme(  legend.position = "right",                 # Position the legend
                                                 legend.title = element_text(size = 26),    # Set legend title font size
                                                 legend.text = element_text(size = 22),      # Set legend text font size
                                                 plot.margin = margin(t = 0, b = 0,l = 0, r = 0), legend.margin = margin(t = 0, r = 0, b = 0, l = -45),  # Adjusts padding within the legend box
                                                 legend.spacing = unit(0, "cm")    
       ) + coord_equal(ratio = 1)+ coord_cartesian(ylim = c(2863467, 6360131), xlim = c(-16419625, -7436142))     # Set the y-axis limits

img <- readPNG("Alaska_Gray.png")  # Replace with the path to your image file

B <- ggplot() +
  annotation_raster(img, xmin = -19169216, xmax = -14438138, ymin = 7170156, ymax = 11575440)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0, l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(67*pi/180))+ coord_cartesian(ylim = c(7170156, 11575440), xlim = c(-19169216, -14438138))

img <- readPNG("hawaii_Gray.png")  # Replace with the path to your image file

C <- ggplot() +
  annotation_raster(img, xmin = -17794421, xmax = -17226691, ymin = 2137284, ymax = 2547582)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0,l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(20.8*pi/180))+ coord_cartesian(ylim = c(2137284, 2547582), xlim = c(-17794421, -17226691))     # Set the y-axis limits

img = readPNG('a_hat_img.png')

g <- rasterGrob(img, interpolate=TRUE)
g
D = ggplot() +
  annotation_custom(g, xmin=0, xmax=0.1, ymin=0, ymax=0.1) +
  geom_point()+xlim(0,0.1)+ylim(0,0.1)+theme_void() + theme(
    legend.position = "none",
    plot.margin = margin(t = 0, b = 0,l = 0, r = 0),panel.border = element_rect(color = "white", fill = NA, linewidth = 0.01)
  ) 

combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.5, width = 0.25, height = 0.4)+
  draw_plot(C,x = 0.07,y = 0.2, width = 0.15, height = 0.2)+
  draw_plot(D,x = 0.883,y = 0.55, width = 0.075, height = 0.075)
dev.new(width = 2000, height = 500)
combined_plot


#Amplitude Change in Precipitation
predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
US_Koppen_Classification = as.factor(predicted_cluster)
US_Koppen_Classification_Types = unique(predicted_cluster)
shapes = c(rep(15,9),rep(17,9),rep(19,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','chartreuse','orange','deeppink','darkgreen','yellow'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)

# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[5][1]), as.numeric(x[4][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

brightness_prcp = rep(0,1726)
for(g in c(1:20)){
  brightness_prcp[which(loc$Cluster == paste('X',as.character(g),sep = ''))] = current_best_model$mu_sine_pars[3,1,g]
}


loc$value = brightness_prcp


# Load your PNG image of Alaska
img <- readPNG("usa_Gray.png")  # Replace with the path to your image file
A <- ggplot() +
  annotation_raster(img, ymin = 2863467,ymax = 6360131, xmin = -16419625, xmax = -7436142)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color =  value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "",color = '')+ theme_void()+ theme(  legend.position = "right",                 # Position the legend
                                                 legend.title = element_text(size = 26),    # Set legend title font size
                                                 legend.text = element_text(size = 22),      # Set legend text font size
                                                 plot.margin = margin(t = 0, b = 0,l = 0, r = 0), legend.margin = margin(t = 0, r = 0, b = 0, l = -45),  # Adjusts padding within the legend box
                                                 legend.spacing = unit(0, "cm")    
       ) + coord_equal(ratio = 1)+ coord_cartesian(ylim = c(2863467, 6360131), xlim = c(-16419625, -7436142))     # Set the y-axis limits

img <- readPNG("Alaska_Gray.png")  # Replace with the path to your image file

B <- ggplot() +
  annotation_raster(img, xmin = -19169216, xmax = -14438138, ymin = 7170156, ymax = 11575440)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0, l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(67*pi/180))+ coord_cartesian(ylim = c(7170156, 11575440), xlim = c(-19169216, -14438138))

img <- readPNG("hawaii_Gray.png")  # Replace with the path to your image file

C <- ggplot() +
  annotation_raster(img, xmin = -17794421, xmax = -17226691, ymin = 2137284, ymax = 2547582)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0,l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(20.8*pi/180))+ coord_cartesian(ylim = c(2137284, 2547582), xlim = c(-17794421, -17226691))     # Set the y-axis limits

img = readPNG('a_hat_img.png')

g <- rasterGrob(img, interpolate=TRUE)
g
D = ggplot() +
  annotation_custom(g, xmin=0, xmax=0.1, ymin=0, ymax=0.1) +
  geom_point()+xlim(0,0.1)+ylim(0,0.1)+theme_void() + theme(
    legend.position = "none",
    plot.margin = margin(t = 0, b = 0,l = 0, r = 0),panel.border = element_rect(color = "white", fill = NA, linewidth = 0.01)
  ) 

combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.5, width = 0.25, height = 0.4)+
  draw_plot(C,x = 0.07,y = 0.2, width = 0.15, height = 0.2)+
  draw_plot(D,x = 0.883,y = 0.55, width = 0.075, height = 0.075)
dev.new(width = 2000, height = 500)
combined_plot

#Vertical Change in Minimum Temperature
load('Matrix_AR1_Sine_reduced_parameters_best_model.RData')
current_best_model = final_best_model
predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
US_Koppen_Classification = as.factor(predicted_cluster)
US_Koppen_Classification_Types = unique(predicted_cluster)
shapes = c(rep(15,9),rep(17,9),rep(19,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','chartreuse','orange','deeppink','darkgreen','yellow'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)

# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[5][1]), as.numeric(x[4][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

brightness_min_temp = rep(0,1726)
for(g in c(1:20)){
  brightness_min_temp[which(loc$Cluster == paste('X',as.character(g),sep = ''))] = current_best_model$mu_sine_pars[1,5,g]
}

loc$value = brightness_min_temp


# Load your PNG image of Alaska
img <- readPNG("usa_Gray.png")  # Replace with the path to your image file
A <- ggplot() +
  annotation_raster(img, ymin = 2863467,ymax = 6360131, xmin = -16419625, xmax = -7436142)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color =  value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "",color = '')+ theme_void()+ theme(  legend.position = "right",                 # Position the legend
                                                 legend.title = element_text(size = 26),    # Set legend title font size
                                                 legend.text = element_text(size = 22),      # Set legend text font size
                                                 plot.margin = margin(t = 0, b = 0,l = 0, r = 0), legend.margin = margin(t = 0, r = 0, b = 0, l = -45),  # Adjusts padding within the legend box
                                                 legend.spacing = unit(0, "cm")    
       ) + coord_equal(ratio = 1)+ coord_cartesian(ylim = c(2863467, 6360131), xlim = c(-16419625, -7436142))     # Set the y-axis limits

img <- readPNG("Alaska_Gray.png")  # Replace with the path to your image file

B <- ggplot() +
  annotation_raster(img, xmin = -19169216, xmax = -14438138, ymin = 7170156, ymax = 11575440)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0, l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(67*pi/180))+ coord_cartesian(ylim = c(7170156, 11575440), xlim = c(-19169216, -14438138))

img <- readPNG("hawaii_Gray.png")  # Replace with the path to your image file

C <- ggplot() +
  annotation_raster(img, xmin = -17794421, xmax = -17226691, ymin = 2137284, ymax = 2547582)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0,l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(20.8*pi/180))+ coord_cartesian(ylim = c(2137284, 2547582), xlim = c(-17794421, -17226691))     # Set the y-axis limits

img = readPNG('e_hat_img.png')

g <- rasterGrob(img, interpolate=TRUE)
g
D = ggplot() +
  annotation_custom(g, xmin=0, xmax=0.1, ymin=0, ymax=0.1) +
  geom_point()+xlim(0,0.1)+ylim(0,0.1)+theme_void() + theme(
    legend.position = "none",
    plot.margin = margin(t = 0, b = 0,l = 0, r = 0),panel.border = element_rect(color = "white", fill = NA, linewidth = 0.01)
  ) 
combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.5, width = 0.25, height = 0.4)+
  draw_plot(C,x = 0.07,y = 0.2, width = 0.15, height = 0.2)+
  draw_plot(D,x = 0.888,y = 0.55, width = 0.065, height = 0.065)
dev.new(width = 2000, height = 500)
combined_plot

#Vertical Change in Maximum Temperature
predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
US_Koppen_Classification = as.factor(predicted_cluster)
US_Koppen_Classification_Types = unique(predicted_cluster)
shapes = c(rep(15,9),rep(17,9),rep(19,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','chartreuse','orange','deeppink','darkgreen','yellow'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)

# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[5][1]), as.numeric(x[4][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x


brightness_max_temp = rep(0,1726)
for(g in c(1:20)){
  brightness_max_temp[which(loc$Cluster == paste('X',as.character(g),sep = ''))] = current_best_model$mu_sine_pars[2,5,g]
}


loc$value = brightness_max_temp


# Load your PNG image of Alaska
img <- readPNG("usa_Gray.png")  # Replace with the path to your image file
A <- ggplot() +
  annotation_raster(img, ymin = 2863467,ymax = 6360131, xmin = -16419625, xmax = -7436142)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color =  value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "",color = '')+ theme_void()+ theme(  legend.position = "right",                 # Position the legend
                                                 legend.title = element_text(size = 26),    # Set legend title font size
                                                 legend.text = element_text(size = 22),      # Set legend text font size
                                                 plot.margin = margin(t = 0, b = 0,l = 0, r = 0), legend.margin = margin(t = 0, r = 0, b = 0, l = -45),  # Adjusts padding within the legend box
                                                 legend.spacing = unit(0, "cm")    
       ) + coord_equal(ratio = 1)+ coord_cartesian(ylim = c(2863467, 6360131), xlim = c(-16419625, -7436142))     # Set the y-axis limits

img <- readPNG("Alaska_Gray.png")  # Replace with the path to your image file

B <- ggplot() +
  annotation_raster(img, xmin = -19169216, xmax = -14438138, ymin = 7170156, ymax = 11575440)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0, l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(67*pi/180))+ coord_cartesian(ylim = c(7170156, 11575440), xlim = c(-19169216, -14438138))

img <- readPNG("hawaii_Gray.png")  # Replace with the path to your image file

C <- ggplot() +
  annotation_raster(img, xmin = -17794421, xmax = -17226691, ymin = 2137284, ymax = 2547582)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0,l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(20.8*pi/180))+ coord_cartesian(ylim = c(2137284, 2547582), xlim = c(-17794421, -17226691))     # Set the y-axis limits

combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.5, width = 0.25, height = 0.4)+
  draw_plot(C,x = 0.07,y = 0.2, width = 0.15, height = 0.2)+
  draw_plot(D,x = 0.883,y = 0.55, width = 0.065, height = 0.065)
dev.new(width = 2000, height = 500)
combined_plot


#Vertical Change in Precipitation
predicted_cluster = colnames(data.frame(current_best_model$z_hat))[max.col(data.frame(current_best_model$z_hat), ties.method = "first")]
US_Koppen_Classification = as.factor(predicted_cluster)
US_Koppen_Classification_Types = unique(predicted_cluster)
shapes = c(rep(15,9),rep(17,9),rep(19,10),rep(13,10),rep(9,10),rep(16,10),rep(11,10),rep(18,10))
custom_shape = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_shape[US_Koppen_Classification_Types[g]] = shapes[g]
}
color_manual = c(rep(c('red','blue','purple','magenta','cyan','chartreuse','orange','deeppink','darkgreen','yellow'),10))
custom_color = c()
for(g in 1:length(US_Koppen_Classification_Types)){
  custom_color[US_Koppen_Classification_Types[g]] = color_manual[g]
}

loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
loc = read.csv('Matrix_Weather_Stations_Final_Locations_Altitude.csv')
loc$Cluster = factor(US_Koppen_Classification,levels = US_Koppen_Classification_Types)

# Apply Mercator transformation to each location
locations_mercator <- t(apply(loc, 1, function(x) lat_lon_to_mercator(as.numeric(x[5][1]), as.numeric(x[4][1]))))
locations_mercator <- as.data.frame(locations_mercator)
colnames(locations_mercator) <- c("x", "y")
loc$LATITUDE_Mercator = locations_mercator$y
loc$LONGITUDE_Mercator = locations_mercator$x

brightness_prcp = rep(0,1726)
for(g in c(1:20)){
  brightness_prcp[which(loc$Cluster == paste('X',as.character(g),sep = ''))] = current_best_model$mu_sine_pars[3,5,g]
}


loc$value = brightness_prcp


# Load your PNG image of Alaska
img <- readPNG("usa_Gray.png")  # Replace with the path to your image file
A <- ggplot() +
  annotation_raster(img, ymin = 2863467,ymax = 6360131, xmin = -16419625, xmax = -7436142)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color =  value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "",color = '')+ theme_void()+ theme(  legend.position = "right",                 # Position the legend
                                                 legend.title = element_text(size = 26),    # Set legend title font size
                                                 legend.text = element_text(size = 22),      # Set legend text font size
                                                 plot.margin = margin(t = 0, b = 0,l = 0, r = 0), legend.margin = margin(t = 0, r = 0, b = 0, l = -45),  # Adjusts padding within the legend box
                                                 legend.spacing = unit(0, "cm")    
       ) + coord_equal(ratio = 1)+ coord_cartesian(ylim = c(2863467, 6360131), xlim = c(-16419625, -7436142))     # Set the y-axis limits

img <- readPNG("Alaska_Gray.png")  # Replace with the path to your image file

B <- ggplot() +
  annotation_raster(img, xmin = -19169216, xmax = -14438138, ymin = 7170156, ymax = 11575440)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0, l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(67*pi/180))+ coord_cartesian(ylim = c(7170156, 11575440), xlim = c(-19169216, -14438138))

img <- readPNG("hawaii_Gray.png")  # Replace with the path to your image file

C <- ggplot() +
  annotation_raster(img, xmin = -17794421, xmax = -17226691, ymin = 2137284, ymax = 2547582)+
  geom_point(data = loc, 
             aes(x = LONGITUDE_Mercator, y = LATITUDE_Mercator, color = value),shape = 17,size = 2) +
  scale_color_gradient2(low = "blue", high = "red",mid = 'white',midpoint = 0) +
  labs(x = "", 
       y = "")+ theme_void() + theme(
         legend.position = "none",
         plot.margin = margin(t = 0, b = 0,l = 0, r = 0),panel.border = element_rect(color = "lightgray", fill = NA, linewidth = 0.01)
       ) + coord_equal(ratio = 1/cos(20.8*pi/180))+ coord_cartesian(ylim = c(2137284, 2547582), xlim = c(-17794421, -17226691))     # Set the y-axis limits

combined_plot <- ggdraw() +
  draw_plot(A, 0, 0, 1, 1) +  # Main map fills the entire space
  draw_plot(B, x = 0.02, y = 0.5, width = 0.25, height = 0.4)+
  draw_plot(C,x = 0.07,y = 0.2, width = 0.15, height = 0.2)+
  draw_plot(D,x = 0.90,y = 0.55, width = 0.065, height = 0.065)
dev.new(width = 2000, height = 500)
combined_plot

library(extrafont)
font_import(pattern = "DejaVu")  # Import DejaVu fonts
loadfonts(device = "pdf")       # Load fonts for the PDF device

install.packages('showtext')

library(showtext)
font_add("DejaVu Sans", "DejaVuSans.ttf") # Provide the path to the TTF file
showtext_auto()

pdf("temp_output.pdf",encoding = "UTF-8")
combined_plot
dev.off()

pdf("temp_output.pdf")
plot(1:10, main = "Mathematical Italic Small 'a'")
legend("topright", legend = c("\U0001D44E"))  # Unicode for mathematical italic small 'a'
dev.off()


# List all available fonts
ZZ = system_fonts()

print(n = 100)
#Cluster Level Plots

sine_estimator <- function(t, a, b, d, e, f){
  #(a*t+b) * sin((2 * pi / c) *t+d) + (e*f^t)
  (a*t+b) * sin((2 * pi / 12) *t+d) + (e+f*t)
}

load('Matrix_AR1_Sine_reduced_parameters_best_model.RData')

final_best_model = current_best_model

for(g in c(1:20)){
  for(p in c(1:3)){
    if(final_best_model$mu_sine_pars[p,2,g]<0){
      final_best_model$mu_sine_pars[p,1,g] = -1*final_best_model$mu_sine_pars[p,1,g]
      final_best_model$mu_sine_pars[p,2,g] = -1*final_best_model$mu_sine_pars[p,2,g]
      final_best_model$mu_sine_pars[p,3,g] = final_best_model$mu_sine_pars[p,3,g]-pi
    }   
  }
}

final_best_model$mu_sine_pars[,,1]
current_best_model$mu_sine_pars[,,1]

mu = array(0,dim = c(3,840500,20))
for(g in c(1:20)){
  for(p in c(1:3)){
    mu[p,,g] = sine_estimator(seq(0.001,840.5,0.001),current_best_model$mu_sine_pars[p,1,g],current_best_model$mu_sine_pars[p,2,g],current_best_model$mu_sine_pars[p,3,g],current_best_model$mu_sine_pars[p,4,g],current_best_model$mu_sine_pars[p,5,g])
  }
}

mu = array(0,dim = c(3,840500,20))
for(g in c(1:20)){
  for(p in c(1:3)){
    mu[p,,g] = sine_estimator(seq(0.001,840.5,0.001),final_best_model$mu_sine_pars[p,1,g],final_best_model$mu_sine_pars[p,2,g],final_best_model$mu_sine_pars[p,3,g],final_best_model$mu_sine_pars[p,4,g],final_best_model$mu_sine_pars[p,5,g])
  }
}
count = 0
for(g in c(1:20)){
  for(p in c(1:3)){
    if(current_best_model$mu_sine_pars[p,1,g]>=0 & current_best_model$mu_sine_pars[p,2,g]<0){
      count = count+1
    }
    else if(current_best_model$mu_sine_pars[p,1,g]<=0 & current_best_model$mu_sine_pars[p,2,g]>0){
      count = count+1
    }
  }
}
count
current_best_model$mu_sine_pars[,,10]

length(seq(0.001,840,0.001))
plot(mu[1,,2])
abline(h = 0)
abline(h = 20)
#One Year
library(reshape2)
g = 9
data <- data.frame(
  x1 = c(501:12500),
  '1946-47' = mu[1,501:12500,g],
  x2 = c(408501:420500),
  '1979-80' = mu[1,408501:420500,g],
  x3 = c(828501:840500),
  '2014-15' = mu[1,828501:840500,g]
)

data_long_1 <- melt(data, id.vars = c("x1", "x2", "x3"), variable.name = "Year", value.name = "y")


data <- data.frame(
  x1 = c(501:12500),
  '1946-47' = mu[2,501:12500,g],
  x2 = c(408501:420500),
  '1979-80' = mu[2,408501:420500,g],
  x3 = c(828501:840500),
  '2014-15' = mu[2,828501:840500,g]
)


data_long_2 <- melt(data, id.vars = c("x1", "x2", "x3"), variable.name = "Year", value.name = "y")

data <- data.frame(
  x1 = c(501:12500),
  '1946-47' = mu[3,501:12500,g],
  x2 = c(408501:420500),
  '1979-80' = mu[3,408501:420500,g],
  x3 = c(828501:840500),
  '2014-15' = mu[3,828501:840500,g]
)

data_long_3 <- melt(data, id.vars = c("x1", "x2", "x3"), variable.name = "Year", value.name = "y")

A = ggplot(data_long_1, aes(x = x1, y = y, color = Year)) + 
  scale_color_manual(values = c('red','blue','green'),name = 'Year', labels = c('1946','1980','2015'))+
  scale_x_continuous(lim = c(251,12750),breaks = c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000),labels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
  geom_line() +
  labs(title = paste("Cluster ",as.character(g),sep =''), x = "", y = "TMIN(°C)") + theme(
    plot.title = element_text(size = 21),
    axis.ticks.x = element_blank(),  # Removes x-axis ticks
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 21),    # Set legend title font size
    legend.text = element_text(size = 20),      # Set legend text font size
    plot.margin = margin(0,0,0,0),panel.background = element_rect(fill = "white", color = NA),panel.grid.major = element_line(color = "gray80"),                  # Major grid lines in light gray
    panel.grid.minor = element_line(color = "gray90"),                  # Minor grid lines in lighter gray
    panel.border = element_rect(color = "gray50", fill = NA, size = 1),panel.spacing = unit(0, "lines")  
  )

B = ggplot(data_long_2, aes(x = x1, y = y, color = Year)) +
  scale_color_manual(values = c('red','blue','green'),name = 'Year', labels = c('1946','1980','2015'))+
  scale_x_continuous(lim = c(251,12750),breaks = c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000),labels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
  geom_line() +
  labs(title = '', x = "", y = "TMAX(°C)") + theme(
    plot.title = element_text(size = 21),
    axis.ticks.x = element_blank(),  # Removes x-axis ticks
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 21),    # Set legend title font size
    legend.text = element_text(size = 20),      # Set legend text font size
    plot.margin = margin(0,0,0,0),panel.background = element_rect(fill = "white", color = NA),panel.grid.major = element_line(color = "gray80"),                  # Major grid lines in light gray
    panel.grid.minor = element_line(color = "gray90"),                  # Minor grid lines in lighter gray
    panel.border = element_rect(color = "gray50", fill = NA, size = 1),panel.spacing = unit(0, "lines")  
  )

C = ggplot(data_long_3, aes(x = x1, y = y, color = Year)) +
  scale_color_manual(values = c('red','blue','green'),name = 'Year', labels = c('1946','1980','2015'))+
  scale_x_continuous(lim = c(251,12750),breaks = c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000),labels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
  geom_line() +
  labs(title = '', x = "", y = "PRCP(mm)")+ theme(
    plot.margin = margin(0,0,0,0),panel.background = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    legend.title = element_text(size = 21),    # Set legend title font size
    legend.text = element_text(size = 20),      # Set legend text font size
    legend.background = element_rect(fill = 'white',color = NA),
    panel.grid.major = element_line(color = "gray80"),                  # Major grid lines in light gray
    panel.grid.minor = element_line(color = "gray90"),                  # Minor grid lines in lighter gray
    panel.border = element_rect(color = "gray50", fill = NA, size = 1) ,panel.spacing = unit(0, "lines")
  )

library(agricolae)
library(patchwork)

D = A/B/C + plot_layout(heights = c(8,8,8))&theme(plot.margin = margin(0, 0, 0, 0))
dev.new(width = 2000, height = 500)
D


(data_long_1[25:36,'y']-data_long_1[1:12,'y'])[1:3]
(data_long_1[25:36,'y']-data_long_1[1:12,'y'])[4:6]
(data_long_1[25:36,'y']-data_long_1[1:12,'y'])[7:9]
(data_long_1[25:36,'y']-data_long_1[1:12,'y'])[10:12]

(data_long_2[25:36,'y']-data_long_2[1:12,'y'])[1:3]
(data_long_2[25:36,'y']-data_long_2[1:12,'y'])[4:6]
(data_long_2[25:36,'y']-data_long_2[1:12,'y'])[7:9]
(data_long_2[25:36,'y']-data_long_2[1:12,'y'])[10:12]

(data_long_3[25:36,'y']-data_long_3[1:12,'y'])[1:3]
(data_long_3[25:36,'y']-data_long_3[1:12,'y'])[4:6]
(data_long_3[25:36,'y']-data_long_3[1:12,'y'])[7:9]
(data_long_3[25:36,'y']-data_long_3[1:12,'y'])[10:12]


sum(data_long_3[c(28:33),'y']-data_long_3[c(4:9),'y'])/sum(data_long_3[c(4:9),'y'])

install.packages('Cairo')
# Load necessary libraries
library(Cairo)
library(ggplot2)

# Unicode character for the Greek letter "α"
unicode_text <- "This is a Unicode character: expression(plain(\"\U0001D452\")^"^")"  # Greek alpha (α)

# Create a simple ggplot
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  ggtitle(expression(plain("\U0001D452")))+
  theme(text = element_text(family = "DejaVu Sans"))

# Save the plot to a PDF using Cairo for proper Unicode support
CairoPDF("plot_with_unicode.pdf", width = 7, height = 5)
print(p)
dev.off()


# Install necessary packages if not installed
install.packages("ggplot2")
install.packages("Cairo")
install.packages("showtext")

# Load the packages
library(ggplot2)
library(Cairo)
library(showtext)

# Use the STIX font family (ensure STIX fonts are installed on your system)
# Using showtext to load STIX font
font_add("STIX", regular = "STIXTwoText-Italic.ttf")  # Modify path as necessary
showtext_auto()

# Create a plot with Unicode character \U0001D452 (𝑒 - Mathematical Italic Small 'e')
unicode_text <- hat("\U0001D452^")
unicode_text
# Create a basic plot with ggplot2
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  ggtitle(expression(hat(plain("\U0001D452"))))+theme(family = 'STIX')

# Save the plot as PDF using Cairo
CairoPDF("unicode_stix_plot.pdf", width = 7, height = 5)
print(p)
dev.off()
paste0('\U0001D452','^')


# Plot with title and legend showing Mathematical Italic Small 'a' with hat
pdf("temp_output.pdf")  
plot(1:10, main = expression(hat(plain(italic(a)))))  # Main title with hat on 'a'
legend("topright", legend = c(expression(hat(plain("\U0001D452")))))  # Hat in legend
dev.off()


# Load necessary libraries
library(ggplot2)
library(png)
library(grid)

# Read the image (e.g., a PNG file)
img_path <- "a_hat_img.png"  # Set your image path here
img <- readPNG(img_path)

# Create a ggplot object
p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  labs(title = "Plot with Image in Title") +
  theme(legend.title = element_text(face = "bold"))

# Save the plot first as a normal plot
ggsave("basic_plot.png", plot = p)

# Open the plot in a new device
grid.newpage()
pushViewport(viewport())

# Add the image to the plot
rasterImage(img, 0.9, 0.9, 0.95, 1)  # Adjust coordinates to position the image

# Recreate the ggplot and plot it with the image close to the legend
print(p)

plot(img)

# Load necessary libraries
library(ggplot2)
library(png)
library(grid)

# Read the image (e.g., a PNG file)
img_path <- "a_hat_img.png"  # Set your image path here
img <- readPNG(img_path)

# Create a ggplot object
p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  labs(title = "Plot with Image in Title") +
  theme(legend.title = element_text(face = "bold"))

# Save the plot first as a normal plot
ggsave("basic_plot.png", plot = p)

# Open the plot in a new device
grid.newpage()
pushViewport(viewport())

# Add the image to the plot
rasterImage(img, 4, 5, 15, 20)  # Adjust coordinates to position the image

# Recreate the ggplot and plot it with the image close to the legend
print(p)


# Load necessary libraries
library(ggplot2)
library(png)
library(grid)

# Read the image (e.g., a PNG file)
img_path <- "a_hat_img.png"  # Set your image path here
img <- readPNG(img_path)

# Create a ggplot object
p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  labs(title = "Plot with Image Outside") +
  theme(
    legend.title = element_text(face = "bold"),
    plot.margin = margin(2, 2, 2, 10)  # Increase the right margin to make space
  ) +
  annotation_raster(
    img, 
    xmin = 5, xmax = 6,   # Specify the range of the x-axis outside the plot
    ymin = 25, ymax = 30  # Specify the range of the y-axis outside the plot
  )

# Print the plot
print(p)

current_best_model$mu_sine_pars[,,3]
final_best_model$mu_sine_pars[,,3]

mu[2,c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,12000),19]

zz = mu[2,c(829000,830000,831000,832000,833000,834000,835000,836000,837000,838000,839000,840000),19]-mu[2,c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000),19]

sum(zz[c(5:8)])/4

load('US_Full_Data_koppen_classification_Final.RData')
table(US_Full_Data_Final$ClimateZ)
