library(VFS)
library(stringr)
A = list.files(pattern='*.dly')
B = A[grepl('US',A)]
location_list = read.csv('locations.csv')
for(i in B[65000:71309]){
  print(i)
  name = paste('ghcnd_all/',i,sep = '')
  daily_data = read.dly(name)
  st_name = str_replace(i,'.dly','')
  loc = location_list[location_list$STATIONS==st_name,]
  saver = data.frame()
  for(y in 1946:2015){
    for(m in 1:12){
      temp_data = daily_data[daily_data$YEAR==y & daily_data$MONTH==m,]
      EMNT = NA
      EMXT = NA
      EMXP = NA
      TMIN = NA
      TMAX = NA
      TAVG = NA
      PRCP = NA
      if(length(temp_data)>0){
        min_temp = na.omit(temp_data$TMIN.VALUE)
        max_temp = na.omit(temp_data$TMAX.VALUE)
        prcp = na.omit(temp_data$PRCP.VALUE)
        if(length(min_temp)>0){
          EMNT = min(min_temp)
          TMIN = mean(min_temp)
        }
        if(length(max_temp)>0){
          EMXT = max(max_temp)
          TMAX = mean(max_temp)
        }
        if(length(prcp)>0){
          EMXP = max(prcp)
          PRCP = sum(prcp)
        }
        if(is.na(TMIN)==FALSE & is.na(TMAX)==FALSE){
          TAVG = (TMIN+TMAX)/2
        }
      }
      saver = rbind(saver,c(loc$STATIONS,loc$LATITUDE,loc$LONGITUDE,y,m,EMNT,EMXT,EMXP,TMIN,TMAX,TAVG,PRCP))
    }
  }
  colnames(saver) = c('STATION ID','LATITUDE','LONGITUDE','YEAR','MONTH','EMNT','EMXT','EMXP','TMIN','TMAX','TAVG','PRCP')
  name2 = paste('monthly_averaged_data_13_03_2024/',st_name,'.csv',sep = '')
  write.csv(saver,file = name2)
}

A = list.files(pattern='*.csv')
monthly_averaged_data_list = data.frame()
for(i in A){
  print(i)
  data = read.csv(i)
  data$X<-NULL
  new_data = data[complete.cases(data[,c('EMNT','EMXT','EMXP','TMIN','TMAX','TAVG','PRCP')]),]
  print(new_data)
  monthly_averaged_data_list = rbind(monthly_averaged_data_list,c(data$STATION.ID[1],data$LATITUDE[1],data$LONGITUDE[1],nrow(new_data)))
}
write.csv(monthly_averaged_data_list,file = 'monthly_averaged_data_list.csv')


#Climate data
#Monthly averaged data cluster
station_list = read.csv('monthly_averaged_data_list.csv')
station_list = station_list[as.integer(station_list$COUNTS)>780,]

#Actual Matrix with subset variables
Matrix_Data_B = array(0,dim = c(3,840,3000))
Location_List = data.frame(matrix(ncol = 3, nrow = 0))
colnames(Location_List) = c('STATION','LATITUDE','LONGITUDE')
counter = 0
counter_2 = 0
for(i in station_list$STATION){
  data = read.csv(paste(i,'.csv',sep = ''))  
  data = data[,c('YEAR','MONTH','TMIN','TMAX','PRCP')]
  for(j in c(1:nrow(data))){
    for(k in c('TMIN','TMAX','PRCP')){
      if(is.na(data[j,k])==TRUE){
        if(j-12<1){
          data[j,k] = data[j+12,k]
        }
        else if(j+12>nrow(data)){
          data[j,k] = data[j-12,k]
        }
        else{
          data[j,k] = mean(c(data[j-12,k],data[j+12,k]),na.rm = TRUE)
        }
      }      
    }
  }
  if(nrow(data[complete.cases(data),])<840){
    print(data[!complete.cases(data),])
    print(i)
    counter = counter + 1
  }
  else{
    if(any(data['TMAX']>100)|any(data['PRCP']>1000)){
      next
    }
    else{
      counter_2 = counter_2 + 1
      Matrix_Data_B[,,counter_2] = t(as.matrix(data[,c('TMIN','TMAX','PRCP')]))
      print(station_list[station_list$STATION_ID==i,])
      Location_List = rbind(Location_List,c(station_list[station_list$STATION==i,c('STATION','LATITUDE','LONGITUDE')]))
    }
  }
}

Matrix_Data = Matrix_Data_B[,,1:1726]
save(Matrix_Data,file = 'Matrix_Weather_Data_Final.RData')
write.csv(Location_List,file = 'Matrix_Weather_Data_Final_Locations.csv')
Subsetted_location_info = read.csv('Subsetted_location_info.csv')
library(sf)
library(ggplot2)
world = map_data('world')
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.05
  ) +
  geom_point(
    data = Subsetted_location_info,
    aes(LONGITUDE, LATITUDE),
    alpha = 0.5,size = 0.05,
  ) + coord_sf (xlim=c (-180, -50),ylim=c (0,80))+theme(legend.position = "none")


table(my_result_AR1_4_SubsetData$predicted_cluster)
table(my_result_AR1_NLS_4_SubsetData$predicted_cluster)
table(my_result_AR1_NLS_OptimLeastSquares_4_SubsetData$predicted_cluster)


table(my_result_AR1_10_FinalData$predicted_cluster)
table(my_result_AR1_NLS_10_FinalData$predicted_cluster)
table(my_result_AR1_NLS_OptimLeastSquares_10_FinalData$predicted_cluster)

load('originalricevismclust_pc_as_v_1_spatialexpertPGMM_epoch_1600.RData')
load('originalricevismclust_pc_as_v_1_spatialexpertPGMM_epoch_4735.RData')
load('originalricevismclust_pc_as_v_1_spatialexpertPGMM_epoch_6365.RData')
final_prediction=data.frame(temp_out_saver$z_hat)
colnames(final_prediction) = c(1:3)
predicted_cluster = colnames(final_prediction)[max.col(final_prediction, ties.method = "first")]
predicted_cluster_2 = colnames(final_prediction)[max.col(final_prediction, ties.method = "first")]
predicted_cluster_3 = colnames(final_prediction)[max.col(final_prediction, ties.method = "first")]
table(predicted_cluster,predicted_cluster_2)
table(predicted_cluster,predicted_cluster_3)



