



rm(list=ls())
library(lmomco)
#library(doParallel)
#library(foreach)


threshold_for_records<- 10


limit_values_of_lmoment<- lmrdia()
tow3andtow4_Limits<- limit_values_of_lmoment$limits
tow3andtow4_AEP4<- limit_values_of_lmoment$aep4
tow3andtow4_GEV<- limit_values_of_lmoment$gev
tow3andtow4_GLO<- limit_values_of_lmoment$glo
tow3andtow4_GNO<- limit_values_of_lmoment$gno
tow3andtow4_GOV<- limit_values_of_lmoment$gov
tow3andtow4_GPA<- limit_values_of_lmoment$gpa
tow3andtow4_PE3<- limit_values_of_lmoment$pe3
tow3andtow4_PDQ3<- limit_values_of_lmoment$pdq3
tow3andtow4_WEIB<- limit_values_of_lmoment$wei
tow3andtow4_EXP<- limit_values_of_lmoment$exp
tow3andtow4_NOR<- limit_values_of_lmoment$nor
tow3andtow4_GUM<- limit_values_of_lmoment$gum
tow3andtow4_RAY<- limit_values_of_lmoment$ray
tow3andtow4_UNI<- limit_values_of_lmoment$uniform
tow3andtow4_slas<- limit_values_of_lmoment$sla
tow3andtow4_cauchy<- limit_values_of_lmoment$cau





## Related to import of data
#mag_diff_RB_event<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/data_with_gaps/days_gap_7/mag_dif_7day_Max_gap7days.csv")
#mag_diff_RB_event<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/discus_Prof_G/max_3_day/dayGAP/days_gap_3/mag_dif_3day_Max_gap3days.csv")
mag_diff_RB_event<- read.csv(file = "D:/Kalai_IITR_office_pc/data_prepared/block_Maxima/mag_dif_block_maxima.csv")

mag_diff_RB_event<- mag_diff_RB_event[,-1]


#time_diff_RB_event<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/data_with_gaps/days_gap_7/time_dif_7day_Max_gap7days.csv")
#time_diff_RB_event<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/discus_Prof_G/max_3_day/dayGAP/days_gap_3/time_dif_3day_Max_gap3days.csv")
time_diff_RB_event<- read.csv(file = "D:/Kalai_IITR_office_pc/data_prepared/block_Maxima/time_dif_block_maxima.csv")


time_diff_RB_event<-time_diff_RB_event[,-1]



# Calculate _number of records ___________________________
num_rec_data<- matrix(nrow = ncol(mag_diff_RB_event), ncol = 1)
for (i in 1:ncol(mag_diff_RB_event)){
  
  temp_data<- mag_diff_RB_event[,i]
  temp_data_na_rm<- temp_data[!is.na(temp_data)]
  
  num_rec_data[i,1]<- length(temp_data_na_rm)
  
  cat(i)
}




number_of_records<- num_rec_data #Number of rec calculated from data
#number_of_records<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/number_times_record_broke.csv")
input_lat_long<- read.csv(file = "D:/Kalai_IITR_office_pc/data_prepared/IMD_data_Long_lat_removing_NA.csv")
input_lat_long<- t(input_lat_long)
lat_long_data<- input_lat_long[-1,]
num_rec_lat_long<- cbind(lat_long_data,number_of_records)
#number_rec_lat_long<- data.frame(num_rec_lat_long[,-3])
number_rec_lat_long<- data.frame(num_rec_lat_long)
names(number_rec_lat_long)<- c("lon","lat","num_rec")









## _________________________________ Plot for the number of records ____________________
library(raster)
###Spatial plot_________________________
library(maptools)
india_watershed_di<- readShapeSpatial("D:/Kalai_IITR_office_pc/india_shapefile/india_India_Country_Boundary_MAPOG/india_India_Country_Boundary.shp")

library(mapproj)
#as_map<- map(NW_US_di)

#library(gridExtra)

library(plotrix)

library(sf)
#library(rnaturalearth)
#library(rnaturalearthdata)
library(geodata)
library(RColorBrewer)


#setwd("G:/Kalai/Circular_statistics/Results/figures")
#tiff('seasonality_in_NW_US_211_sites.tif')
#plot(NW_US_di,lwd=2)

min_long<- extent(india_watershed_di)[1]
max_long<- extent(india_watershed_di)[2]
min_lat<- extent(india_watershed_di)[3]
max_lat<- extent(india_watershed_di)[4]

#gaps_long<- 5
long_at<- c(70,75,80,85,90,95)
lat_at<- c(10,15,20,25,30,35)




png(filename="D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/number_of_RB_events.png")
map(india_watershed_di,xlim=c(67.5,98), ylim=c(6,38),lwd=2)
title(main="Number of RB events")



map.axes(cex.axis=1.2,lwd=3,at=c(long_at,lat_at),
         labels=c('70°E','75°E','80°E','85°E','90°E','95°E','10°N','15°N','20°N','25°N',"30°N",'35°N'))


north(xy=cbind(95,34),type=2)

rec_vec_range<- c(5,10,15,20)#,25)#,30,35,40,45,50,55)
records_length_vec<- c(0,5,10,15,20)#,25)#,30,35,40,45,50,55)

#rec_vec_range<- c(9,15,20)#,25)#,30,35,40,45,50,55)
#records_length_vec<- c(0,9,15,20)#,25)#,30,35,40,45,50,55)

values_range_rec<- matrix(nrow = (length(records_length_vec) - 1) ,ncol = 2)
for (i in 1:(length(records_length_vec) - 1)){
  
  values_range_rec[i,1]<- records_length_vec[i]
  values_range_rec[i,2]<- records_length_vec[i+1]
  
}


names_colours_rl<- rev(brewer.pal(n = length(records_length_vec)-1, name = "Spectral"))
#names_colours_rl<- brewer.pal(n = length(records_length_vec), name = "Spectral")
ranges_values<- data.frame(values_range_rec)

ranges_values_char<- ranges_values
ranges_values_char[,1]<- as.character(ranges_values[,1])
ranges_values_char[,2]<- as.character(ranges_values[,2])

for (i in 1:nrow(number_rec_lat_long)){
  
  temp_lat<- number_rec_lat_long[i,2]
  temp_long<- number_rec_lat_long[i,1]
  
  
  if (number_rec_lat_long[i,3] > values_range_rec[1,1] & number_rec_lat_long[i,3] <= values_range_rec[1,2]){
    col_pnt<- names_colours_rl[1]
  }else if (number_rec_lat_long[i,3] > values_range_rec[2,1] & number_rec_lat_long[i,3] <= values_range_rec[2,2]){
    col_pnt<- names_colours_rl[2]
  }else if (number_rec_lat_long[i,3] > values_range_rec[3,1] & number_rec_lat_long[i,3] <= values_range_rec[3,2]){
    col_pnt<- names_colours_rl[3]
  }else if (number_rec_lat_long[i,3] > values_range_rec[4,1] & number_rec_lat_long[i,3] <= values_range_rec[4,2]){
    col_pnt<- names_colours_rl[4]
  }#else if (number_rec_lat_long[i,3] > values_range_rec[5,1] & number_rec_lat_long[i,3] <= values_range_rec[5,2]){
    #col_pnt<- names_colours_rl[5]
  #}
  
  # }else if (number_rec_lat_long[i,3] > values_range_rec[6,1] & number_rec_lat_long[i,3] <= values_range_rec[6,2]){
  #   col_pnt<- names_colours_rl[6]
  # }else if (number_rec_lat_long[i,3] > values_range_rec[7,1] & number_rec_lat_long[i,3] <= values_range_rec[7,2]){
  #   col_pnt<- names_colours_rl[7]
  # }else if (number_rec_lat_long[i,3] > values_range_rec[8,1] & number_rec_lat_long[i,3] <= values_range_rec[8,2]){
  #   col_pnt<- names_colours_rl[8]
  # }else if (number_rec_lat_long[i,3] > values_range_rec[9,1] & number_rec_lat_long[i,3] <= values_range_rec[9,2]){
  #   col_pnt<- names_colours_rl[9]
  # }else if (number_rec_lat_long[i,3] > values_range_rec[10,1] & number_rec_lat_long[i,3] <= values_range_rec[10,2]){
  #   col_pnt<- names_colours_rl[10]
  # }else if (number_rec_lat_long[i,3] > values_range_rec[11,1] & number_rec_lat_long[i,3] <= values_range_rec[11,2]){
  #   col_pnt<- names_colours_rl[11]
  # }
  
  
  
  points(temp_long,temp_lat,col = col_pnt, cex = 1,pch=16)
  
  
  print(paste(i," Done.....","To be done till.....      ",nrow(number_rec_lat_long),sep = ""))
}


i<- 2799
#lat_imd<- all_lat_serially_IMD[i]
#long_imd<- all_long_serially_IMD[i]
lat_imd<- number_rec_lat_long[i,2]
long_imd<- number_rec_lat_long[i,1]
points(long_imd,lat_imd, cex = 2,pch=16,col="pink3",lwd = 3)



points(85,10, cex = 2,pch=16,col="pink3",lwd = 3) #for legend


dif_lat_ind<- 1
lat_st_ind<- 17.5

for (i in 1:length(names_colours_rl)){
  lat_pnt<- lat_st_ind - (dif_lat_ind*i)
  points(85,lat_pnt,col = names_colours_rl[i], cex = 1,pch=15)
  #text(87.5, lat_pnt, as.character(samp_rl[i]),cex=0.7)
  if (i == 1){
    first_rng<- as.character(round(values_range_rec[i,1]+1,0))
  }else{
    first_rng<- as.character(round((values_range_rec[i,1] + 1),0))
  }
  
  last_rng<- as.character(round(values_range_rec[i,2],0))
  
  text(87.5, (lat_pnt+0.2), paste(first_rng,"-",last_rng,sep=""),cex=0.8)
}


dev.off()










## ______________________Related to lmoment ratios

th_rec_length<- 10 #-1 is added to take care of the differences

l_mom_mag_lamb1_tou2_tou3_tou4_tow5_all<- matrix(nrow = nrow(number_rec_lat_long), ncol = 5)
l_mom_time_lamb1_tou2_tou3_tou4_tow5_all<- matrix(nrow = nrow(number_rec_lat_long), ncol = 5)
for (i in 1:ncol(time_diff_RB_event)){
  
  temp_mag_diff<- mag_diff_RB_event[,i]
  temp_time_diff<- time_diff_RB_event[,i]
  
  temp_mag_diff_na_rm<- temp_mag_diff[!is.na(temp_mag_diff)]
  temp_time_diff_na_rm<- temp_time_diff[!is.na(temp_time_diff)]
  
  if (length(temp_mag_diff_na_rm) >= (th_rec_length)){ 
    mag_lmoments<- lmoms(temp_mag_diff_na_rm)
    time_lmoments<- lmoms(temp_time_diff_na_rm)
    
    l_mom_mag_lamb1_tou2_tou3_tou4_tow5_all[i,1]<- mag_lmoments$lambdas[1]
    l_mom_mag_lamb1_tou2_tou3_tou4_tow5_all[i,2]<- mag_lmoments$ratios[2]
    l_mom_mag_lamb1_tou2_tou3_tou4_tow5_all[i,3]<- mag_lmoments$ratios[3]
    l_mom_mag_lamb1_tou2_tou3_tou4_tow5_all[i,4]<- mag_lmoments$ratios[4]
    l_mom_mag_lamb1_tou2_tou3_tou4_tow5_all[i,5]<- mag_lmoments$ratios[5]
    
    l_mom_time_lamb1_tou2_tou3_tou4_tow5_all[i,1]<- time_lmoments$lambdas[1]
    l_mom_time_lamb1_tou2_tou3_tou4_tow5_all[i,2]<- time_lmoments$ratios[2]
    l_mom_time_lamb1_tou2_tou3_tou4_tow5_all[i,3]<- time_lmoments$ratios[3]
    l_mom_time_lamb1_tou2_tou3_tou4_tow5_all[i,4]<- time_lmoments$ratios[4]
    l_mom_time_lamb1_tou2_tou3_tou4_tow5_all[i,5]<- time_lmoments$ratios[5]
    
    
  }else{
    
    l_mom_mag_lamb1_tou2_tou3_tou4_tow5_all[i,]<- NA
    
    l_mom_time_lamb1_tou2_tou3_tou4_tow5_all[i,]<- NA
    
    
  }
  
  
  print(paste(i,"    .....DONE.......     ......    To done till    ....   ",ncol(time_diff_RB_event),sep = ""))
}







file_dir<- "D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/record_length/"

## Plot the lmoment ratios

#list_of_thres_hold_record<- c(10,15,20,25,30,35,40,45,50) #-1 is added to incorporate the differences
list_of_thres_hold_record<- c(10,15)#,20)#,25,30,35,40,45,50)

l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise<- matrix(nrow = length(list_of_thres_hold_record), ncol = 5)
l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise<- matrix(nrow = length(list_of_thres_hold_record), ncol = 5)
for (r in 1:length(list_of_thres_hold_record)){
  
  temp_th_rec_length<- list_of_thres_hold_record[r]
  
  ind_beyond_thres<- which((number_rec_lat_long[,3]) >= temp_th_rec_length)
  l_mom_mag_lamb1_tou2_tou3_tou4_tow5_th_rec<- l_mom_mag_lamb1_tou2_tou3_tou4_tow5_all[ind_beyond_thres,]
  l_mom_time_lamb1_tou2_tou3_tou4_tow5_th_rec<- l_mom_time_lamb1_tou2_tou3_tou4_tow5_all[ind_beyond_thres,]
  
  # Average l_mom
  l_mom_mag_lamb1_tou2_tou3_tou4_tow5_ave<- apply(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_th_rec,2,mean)
  l_mom_time_lamb1_tou2_tou3_tou4_tow5_ave<- apply(l_mom_time_lamb1_tou2_tou3_tou4_tow5_th_rec,2,mean)
  
  ## Save the average l momentratio values____________
  l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise[r,]<- l_mom_mag_lamb1_tou2_tou3_tou4_tow5_ave
  l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise[r,]<- l_mom_time_lamb1_tou2_tou3_tou4_tow5_ave
  
  #png(paste(file_dir,"record_len_","legend",".png",sep = ))
  png(paste(file_dir,"record_len_",list_of_thres_hold_record[r],".png",sep = ))
  #png(paste(file_dir,"record_len_leg",list_of_thres_hold_record[r],".png",sep = ))
  xlim_l<- -0.2
  xlim_u<- 1
  ylim_l<- -0.3
  ylim_u<- 1 + 0.1
  
  ## Automation plot
  #plotlmrdia(lmrdia(),lwd.cex = 2,autolegend=T, xleg="topright", legendcex=0.9)
  op <- par(mar = c(7,6,4,2) + 0.4)
  #plot(tow3andtow4_Limits[,1], tow3andtow4_Limits[,2],main =paste("Record Length = ",list_of_thres_hold_record[r],sep=""), type = "l",lty=1, lwd=4, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="grey",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  #par(new=T)
  
  par(new=T)
  plot(tow3andtow4_GEV[,1], tow3andtow4_GEV[,2],main =paste("Record Length = ",list_of_thres_hold_record[r],sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="darkred",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GLO[,1], tow3andtow4_GLO[,2],main =paste("Record Length = ",list_of_thres_hold_record[r],sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="green",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GNO[,1], tow3andtow4_GNO[,2],main =paste("Record Length = ",list_of_thres_hold_record[r],sep=""), type = "l",lty=2, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="blue",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GPA[,1], tow3andtow4_GPA[,2],main =paste("Record Length = ",list_of_thres_hold_record[r],sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="blue",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_PE3[,1], tow3andtow4_PE3[,2],main =paste("Record Length = ",list_of_thres_hold_record[r],sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="purple",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  
  
  for (i in 1:nrow(number_rec_lat_long)){
    
    if (number_rec_lat_long[i,3] >= temp_th_rec_length){
      temp_mag_lskew<- l_mom_mag_lamb1_tou2_tou3_tou4_tow5_all[i,3]
      temp_mag_lkurt<- l_mom_mag_lamb1_tou2_tou3_tou4_tow5_all[i,4]
      points(temp_mag_lskew,temp_mag_lkurt, pch=0, col="orange",cex=1.5)
      
      temp_time_lskew<- l_mom_time_lamb1_tou2_tou3_tou4_tow5_all[i,3]
      temp_time_lkurt<- l_mom_time_lamb1_tou2_tou3_tou4_tow5_all[i,4]
      points(temp_time_lskew,temp_time_lkurt, pch=24, col="black",cex=1.5)
      
    }
    
    
    
  }
  
  
  # par(new=T)
  # plotlmrdia(lmrdia(),lwd.cex = 2,#autolegend=T, xleg="topright", legendcex=0.9,
  #            xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u),
  #            #xlab = "",ylab = "",
  #            xlab =  expression(paste("L-Skewness (",tau[3],")")), 
  #            ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")),
  #            bty="n",
  #            xaxt="n",yaxt="n",axes = FALSE)
  
  
  
  axis(1, lwd = 2,cex.lab=1.5, cex.axis=1.5,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2,las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = seq(ylim_l, ylim_u,by=0.2))
  
  par(new=T)
  plot(tow3andtow4_GEV[,1], tow3andtow4_GEV[,2],main =paste("Record Length = ",list_of_thres_hold_record[r],sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="darkred",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GLO[,1], tow3andtow4_GLO[,2],main =paste("Record Length = ",list_of_thres_hold_record[r],sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="green",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GNO[,1], tow3andtow4_GNO[,2],main =paste("Record Length = ",list_of_thres_hold_record[r],sep=""), type = "l",lty=2, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="blue",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GPA[,1], tow3andtow4_GPA[,2],main =paste("Record Length = ",list_of_thres_hold_record[r],sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="blue",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_PE3[,1], tow3andtow4_PE3[,2],main =paste("Record Length = ",list_of_thres_hold_record[r],sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="purple",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  # Plot the average
  points(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_ave[3],
         l_mom_mag_lamb1_tou2_tou3_tou4_tow5_ave[4], pch=15, col="cyan",cex=2.5)
  points(l_mom_time_lamb1_tou2_tou3_tou4_tow5_ave[3],
         l_mom_time_lamb1_tou2_tou3_tou4_tow5_ave[4], pch=17, col="pink",cex=2.5)
  
  
  # legend("topright",lty= c(#1,
  #                          1,1,2,1,1,
  #                          NA,NA,NA,NA),
  #   lwd = c(#4,
  #           2,2,2,2,2,
  #           NA,NA,NA,NA),
  #   pch = c(#NA,
  #           NA,NA,NA,NA,NA,
  #           0,24,15,17),#,13,10),
  #   col = c(#"grey40",
  #           "darkred","green","blue","blue","purple"
  #           ,"orange","black","cyan","pink"),
  #   text.font = 2, cex = 0.75,
  #   legend = c(#"Limits",
  #              "GEV","GLO","GNO","GPA","PE3",
  #              "R-MAG","R-Time","AR-MAG","AR-Time")
  #   )
  
# 
#   legend("topright", lty = c(1,4,1,1,2,2,1,1,2,
#                              #1,# To remove weibul
#                              NA,NA,NA,NA,NA,
#                              NA,NA,NA,NA),#,NA,NA),
#          lwd = c(4,2,2,2,2,2,2,2,2,
#                  #2,# To remove weibul
#                  NA,NA,NA,NA,NA,
#                  NA,NA,NA,NA),#,NA,NA),
#          pch = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,
#                  #NA,# To remove weibul
#                  16,15,17,18,12,
#                  0,24,15,17),#,13,10),
#          col = c("grey40","red","darkred","green","blue","magenta","blue","purple","darkgreen",
#                  #"darkorange",# To remove weibul
#                  "red","red","red","red","red",
#                  "orange","black","cyan","pink"),#,"turquoise4","turquoise4"),
#          text.font = 2, cex = 0.75,
#          legend = c("Limits","AEP4","GEV","GLO","GNO","GOV","GPA","PE3","PDQ3",
#                     #"WEI",# To remove weibul
#                     "EXP","NOR","GUM","RAY","UNI",
#                     "R-MAG","R-Time","AR-MAG","AR-Time"))#,"CAU","SLA"))
  
  par(new=F)
  dev.off()
  
  
  print(paste(r,"  -of-   ",i,"    .....DONE.......     ......    To done till    ....   ",nrow(number_rec_lat_long),"  - of -  ",length(list_of_thres_hold_record),sep = ""))
}


# Zoom plot can be done after this immediately



ind_rec_l<- which(list_of_thres_hold_record == threshold_for_records)
## Considering the median value of the average of record length 
##___________________________________which is corresponding to 30 years record length
vec_lmom_mag<- c(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,1],
                 l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,2],
                 l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,3],
                 l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,4],
                 l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,5])

mag_lmom<- vec2lmom(vec_lmom_mag,lscale = F)



vec_lmom_time<- c(l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,1],
                  l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,2],
                  l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,3],
                  l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,4],
                  l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,5])

time_lmom<- vec2lmom(vec_lmom_time,lscale = F)






## Simulation experiment______________________________________

#pool_of_distributions<- c("AEP4","GEV","GLO","GNO","GOV","GPA","PE3","PDQ3")
pool_of_distributions<- c("GEV","GLO","GNO","GPA","PE3")


pool_of_record_length<- c(10,20,25,30,35,40,45,50,70,100)
number_of_simulation<- 1000



for (r in 1:length(pool_of_record_length)){
  
  temp_rec_len<- pool_of_record_length[r]
  
  tr<- 0 # to keep track
  
  
  
  euc_dist_mag_samp<- matrix(nrow = number_of_simulation, ncol = length(pool_of_distributions))
  #euc_dist_mag_samp_per<- matrix(nrow = number_of_simulation, ncol = length(pool_of_record_length))
  euc_dist_time_samp<- matrix(nrow = number_of_simulation, ncol = length(pool_of_distributions))
  #euc_dist_time_samp_per<- matrix(nrow = number_of_simulation, ncol = length(pool_of_record_length))
  for (i in 1:length(pool_of_distributions)){
    
    temp_dist<- pool_of_distributions[i]
    
    
    
    if (temp_dist == "AEP4"){
      para_mag<- paraep4(mag_lmom,kapapproved = F,snap.tau4=T)
      para_time<- paraep4(time_lmom,kapapproved = F,snap.tau4=T)
    }else if (temp_dist == "GEV"){
      para_mag<- pargev(mag_lmom)
      para_time<- pargev(time_lmom)
    }else if (temp_dist == "GLO"){
      para_mag<- parglo(mag_lmom)
      para_time<- parglo(time_lmom)
    }else if (temp_dist == "GNO"){
      para_mag<- pargno(mag_lmom)
      para_time<- pargno(time_lmom)
    }else if (temp_dist == "GOV"){
      para_mag<- pargov(mag_lmom)
      para_time<- pargov(time_lmom)
    }else if (temp_dist == "GPA"){
      para_mag<- pargpa(mag_lmom)
      para_time<- pargpa(time_lmom)
    }else if (temp_dist == "PE3"){
      para_mag<- parpe3(mag_lmom,checklmom = T)
      para_time<- parpe3(time_lmom,checklmom = T)
    }else if (temp_dist == "PDQ3"){
      para_mag<- parpdq3(mag_lmom,checklmom = T)
      para_time<- parpdq3(time_lmom,checklmom = T)
    }
    
    
    
    
    
    
    
    for (s in 1:number_of_simulation){
      
      tr<- tr + 1
      
      number_rec<- temp_rec_len
      unif_numb<- runif(number_rec)
      
      
      if (temp_dist == "AEP4"){
        generated_records_mag<- quaaep4(unif_numb,para_mag)
        generated_records_time<- quaaep4(unif_numb,para_time)
      }else if (temp_dist == "GEV"){
        generated_records_mag<- quagev(unif_numb,para_mag)
        generated_records_time<- quagev(unif_numb,para_time)
      }else if (temp_dist == "GLO"){
        generated_records_mag<- quaglo(unif_numb,para_mag)
        generated_records_time<- quaglo(unif_numb,para_time)
      }else if (temp_dist == "GNO"){
        generated_records_mag<- quagno(unif_numb,para_mag)
        generated_records_time<- quagno(unif_numb,para_time)
      }else if (temp_dist == "GOV"){
        generated_records_mag<- quagov(unif_numb,para_mag)
        generated_records_time<- quagov(unif_numb,para_time)
      }else if (temp_dist == "GPA"){
        generated_records_mag<- quagpa(unif_numb,para_mag)
        generated_records_time<- quagpa(unif_numb,para_time)
      }else if (temp_dist == "PE3"){
        generated_records_mag<- quape3(unif_numb,para_mag)
        generated_records_time<- quape3(unif_numb,para_time)
      }else if (temp_dist == "PDQ3"){
        generated_records_mag<- quapdq3(unif_numb,para_mag)
        generated_records_time<- quapdq3(unif_numb,para_time)
      }
      
      
      
      mag_lmoments_gen<- lmoms(generated_records_mag)
      time_lmoments_gen<- lmoms(generated_records_time)
      
      mag_error_in_t3_and_t4<- ((((mag_lmom$ratios[3])-(mag_lmoments_gen$ratios[3]))^2)+(((mag_lmom$ratios[4])-(mag_lmoments_gen$ratios[4]))^2))^(0.5)
      time_error_in_t3_and_t4<- ((((time_lmom$ratios[3])-(time_lmoments_gen$ratios[3]))^2)+(((time_lmom$ratios[4])-(time_lmoments_gen$ratios[4]))^2))^(0.5)
      
      mag_error_in_t3_and_t4_per<- ((((((mag_lmom$ratios[3])-(mag_lmoments_gen$ratios[3]))/(mag_lmoments_gen$ratios[3]))^2)+
                                       ((((mag_lmom$ratios[4])-(mag_lmoments_gen$ratios[4]))/(mag_lmoments_gen$ratios[4]))^2))^(0.5))*100
      time_error_in_t3_and_t4_per<- ((((((time_lmom$ratios[3])-(time_lmoments_gen$ratios[3]))/(time_lmoments_gen$ratios[3]))^2)+
                                        ((((time_lmom$ratios[4])-(time_lmoments_gen$ratios[4]))/(time_lmoments_gen$ratios[4]))^2))^(0.5))*100
      
      
      
      
      ## Saving______________
      euc_dist_mag_samp[s,i]<- mag_error_in_t3_and_t4
      #euc_dist_mag_samp_per[s,r]<- mag_error_in_t3_and_t4_per
      
      euc_dist_time_samp[s,i]<- time_error_in_t3_and_t4
      #euc_dist_time_samp_per[s,r]<- time_error_in_t3_and_t4_per
      
      
      
      
      print(paste(temp_rec_len," rec leng - ",temp_dist,"    ....   ",tr," ...To be done till........",number_of_simulation*length(pool_of_distributions),sep = ""))
    }
    
    
  }
  
  
  names_rl<- as.character(pool_of_distributions)
  
  euc_dist_mag_samp_df<- data.frame(euc_dist_mag_samp)
  names(euc_dist_mag_samp_df)<- c(names_rl[1:length(names_rl)])
  
  png(paste("D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/dist_record_len/diff_plot_forLength/","mag_",temp_rec_len,"_record_length.png",sep = ""))
  op <- par(mar = c(5,4.5,4,2) + 0.4)
  boxplot(euc_dist_mag_samp_df, names = c(names_rl),xlab="",ylab="Euclidean Distance",main=paste("Mag - ",temp_rec_len,"Record length",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  #boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
  box(lwd=2)
  dev.off()
  
  euc_dist_time_samp_df<- data.frame(euc_dist_time_samp)
  names(euc_dist_time_samp_df)<- c(names_rl[1:length(names_rl)])
  
  png(paste("D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/dist_record_len/diff_plot_forLength/","time_",temp_rec_len,"_record_length.png",sep = ""))
  op <- par(mar = c(5,4.5,4,2) + 0.4)
  boxplot(euc_dist_time_samp_df, names = c(names_rl),xlab="",ylab="Euclidean Distance",main=paste("Time - ",temp_rec_len,"Record length",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1.5),las=2)
  #boxplot(euc_dist_time_samp_df, main=paste(temp_dist," - Time",sep = ""))
  box(lwd=2)
  dev.off()
  
}











## ____________________________________ 

#th_rec_length_best_dist<- 30
#th_rec_length_best_dist<- 25
th_rec_length_best_dist<- 10

ind_beyond_thres<- which((number_rec_lat_long[,3]) >= th_rec_length_best_dist)
l_mom_mag_lamb1_tou2_tou3_tou4_tow5_bestDist<- l_mom_mag_lamb1_tou2_tou3_tou4_tow5_all[ind_beyond_thres,]
l_mom_time_lamb1_tou2_tou3_tou4_tow5_bestDist<- l_mom_time_lamb1_tou2_tou3_tou4_tow5_all[ind_beyond_thres,]

nrow(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_bestDist)# Numver of records



euc_distnce_mag_fordistribution<- matrix(nrow = nrow(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_bestDist), ncol=length(pool_of_distributions))
#rank_eucli_mag_small_larg
euc_distnce_time_fordistribution<- matrix(nrow = nrow(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_bestDist), ncol=length(pool_of_distributions))
#rank_eucli_time_small_larg

for (d in 1:length(pool_of_distributions)){
  
  
  temp_dist<- pool_of_distributions[d]
  
  
  if (temp_dist == "AEP4"){
    tow3_bestDist<- matrix(tow3andtow4_AEP4[,1], ncol = 1)
    tow4_bestDist<- matrix(tow3andtow4_AEP4[,2], ncol = 1)
  }else if (temp_dist == "GEV"){
    tow3_bestDist<- matrix(tow3andtow4_GEV[,1], ncol = 1)
    tow4_bestDist<- matrix(tow3andtow4_GEV[,2], ncol = 1)
  }else if (temp_dist == "GLO"){
    tow3_bestDist<- matrix(tow3andtow4_GLO[,1], ncol = 1)
    tow4_bestDist<- matrix(tow3andtow4_GLO[,2], ncol = 1)
  }else if (temp_dist == "GNO"){
    tow3_bestDist<- matrix(tow3andtow4_GNO[,1], ncol = 1)
    tow4_bestDist<- matrix(tow3andtow4_GNO[,2], ncol = 1)
  }else if (temp_dist == "GOV"){
    tow3_bestDist<- matrix(tow3andtow4_GOV[,1], ncol = 1)
    tow4_bestDist<- matrix(tow3andtow4_GOV[,2], ncol = 1)
  }else if (temp_dist == "GPA"){
    tow3_bestDist<- matrix(tow3andtow4_GPA[,1], ncol = 1)
    tow4_bestDist<- matrix(tow3andtow4_GPA[,2], ncol = 1)
  }else if (temp_dist == "PE3"){
    tow3_bestDist<- matrix(tow3andtow4_PE3[,1], ncol = 1)
    tow4_bestDist<- matrix(tow3andtow4_PE3[,2], ncol = 1)
  }else if (temp_dist == "PDQ3"){
    tow3_bestDist<- matrix(tow3andtow4_PDQ3[,1], ncol = 1)
    tow4_bestDist<- matrix(tow3andtow4_PDQ3[,2], ncol = 1)
  }
  
  
  #tow3_bestDist_mag<- matrix(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_bestDist[,1], ncol = 1)
  #tow4_bestDist_mag<- matrix(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_bestDist[,2], ncol = 1)
  
  #tow3_bestDist_time<- matrix(l_mom_time_lamb1_tou2_tou3_tou4_tow5_bestDist[,1], ncol = 1)
  #tow4_bestDist_time<- matrix(l_mom_time_lamb1_tou2_tou3_tou4_tow5_bestDist[,2], ncol = 1)
  
  mini_eucli_across_all_mag<- matrix(nrow = nrow(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_bestDist), ncol = 1)
  mini_eucli_across_all_time<- matrix(nrow = nrow(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_bestDist), ncol = 1)
  for (i in 1:nrow(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_bestDist)){
    
    temp_tow3_and_tow4_for_site_mag<- l_mom_mag_lamb1_tou2_tou3_tou4_tow5_bestDist[i,3:4]
    temp_tow3_and_tow4_for_site_time<- l_mom_time_lamb1_tou2_tou3_tou4_tow5_bestDist[i,3:4]
    
    
    eucli_dist_mag<- (((tow3_bestDist - temp_tow3_and_tow4_for_site_mag[1])^2)+((tow4_bestDist - temp_tow3_and_tow4_for_site_mag[2])^2))^(0.5)
    eucli_dist_time<- (((tow3_bestDist - temp_tow3_and_tow4_for_site_time[1])^2)+((tow4_bestDist - temp_tow3_and_tow4_for_site_time[2])^2))^(0.5)
    
    mini_eucli_across_all_mag[i]<- min(eucli_dist_mag)
    mini_eucli_across_all_time[i]<- min(eucli_dist_time)
    
  }
  
  euc_distnce_mag_fordistribution[,d]<- mini_eucli_across_all_mag
  euc_distnce_time_fordistribution[,d]<- mini_eucli_across_all_time
  
  cat(d)
}







rank_eucli_mag_small_larg<- matrix(nrow = nrow(euc_distnce_mag_fordistribution), ncol = ncol(euc_distnce_mag_fordistribution))
rank_eucli_time_small_larg<- matrix(nrow = nrow(euc_distnce_mag_fordistribution), ncol = ncol(euc_distnce_mag_fordistribution))
for (i in 1:nrow(euc_distnce_mag_fordistribution)){
  
  temp_eucl_dist_mag<- euc_distnce_mag_fordistribution[i,]
  sort_temp_eucl_dist_mag<- sort(temp_eucl_dist_mag)
  
  # Rank for mag
  for (d in 1:length(sort_temp_eucl_dist_mag)){
    
    temp_eu_dist_mag<- sort_temp_eucl_dist_mag[d]
    
    ind_rank_mag<- which(temp_eucl_dist_mag == temp_eu_dist_mag)
    
    rank_eucli_mag_small_larg[i,ind_rank_mag]<- d
  }
  
  
  
  
  
  
  temp_eucl_dist_time<- euc_distnce_time_fordistribution[i,]
  sort_temp_eucl_dist_time<- sort(temp_eucl_dist_time)
  
  # Rank for time
  for (t in 1:length(sort_temp_eucl_dist_time)){
    
    temp_eu_dist_time<- sort_temp_eucl_dist_time[t]
    
    ind_rank_time<- which(temp_eucl_dist_time == temp_eu_dist_time)
    
    rank_eucli_time_small_larg[i,ind_rank_time]<- t
  }
  
  cat(i)
}






## Calculate the priority____________________percentage

number_prior_distribution_mag<- matrix(nrow = ncol(rank_eucli_mag_small_larg), ncol = ncol(rank_eucli_mag_small_larg))
number_prior_distribution_time<- matrix(nrow = ncol(rank_eucli_mag_small_larg), ncol = ncol(rank_eucli_mag_small_larg))
for (i in 1:ncol(rank_eucli_mag_small_larg)){
  
  temp_dist_col_mag<- rank_eucli_mag_small_larg[,i]
  temp_dist_col_time<- rank_eucli_time_small_larg[,i]
  
  for (j in 1:ncol(rank_eucli_mag_small_larg)){
    
    number_prior_distribution_mag[j,i]<- length(which(temp_dist_col_mag == j))
    number_prior_distribution_time[j,i]<- length(which(temp_dist_col_time == j))
  }
  
  
}







# Percentage ________________

percentage_distribn_mag<- ((number_prior_distribution_mag[1,])/(sum(number_prior_distribution_mag[1,])))*100
percentage_distribn_time<- ((number_prior_distribution_time[1,])/(sum(number_prior_distribution_time[1,])))*100


percentage_distbn_mag_time<- rbind(matrix(percentage_distribn_mag, nrow = 1),matrix(percentage_distribn_time, nrow = 1))



colors = c("orange", "black")
dist_names<- c(pool_of_distributions)
names_bars<- c("R-Mag", "R-Time")

png(paste("D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/","suitable_distribution",".png",sep = ""))
op <- par(mar = c(5,4.5,4,2) + 0.4)
barplot(percentage_distbn_mag_time, main = "Percentage of Prefered Distribution",
        names.arg = dist_names,
        xlab = "", ylab = "Percentage (%)",
        col = colors, beside = TRUE,lwd = 2,cex.lab=1.5,cex.axis=1.4,ylim=c(0,80),las=2,cex.names=1.4)

# Add the legend to the chart
legend("topright", names_bars, cex = 1.3, fill = colors,box.lwd = 2)
dev.off()








## Plot ___________________________ for each record_length zoom plot
png(paste("D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/","l_ration_suitable_distbn_mag",".png",sep = ""))
xlim_l<- 0.18
xlim_u<- 0.33
ylim_l<- -0.2
ylim_u<- 0.5
op <- par(mar = c(5,4.5,4,2) + 0.4)
plotlmrdia(lmrdia(),lwd.cex = 2,#autolegend=T, xleg="topright", legendcex=0.9,
           main="Magnitude",
           xlim = c(xlim_l, xlim_u), ylim = c(ylim_l, ylim_u),
           #xlab = "",ylab = "",
           xlab =  expression(paste("L-Skewness (",tau[3],")")), 
           ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")),
           bty="n",
           xaxt="n",yaxt="n",axes = FALSE)
axis(1, lwd = 2,cex.lab=1.5, cex.axis=1.5,at = seq(xlim_l, xlim_u,by=0.05))
#axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
axis(2,las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = seq(ylim_l, ylim_u,by=0.1))

legend("topright", lty = c(1,4,1,1,2,2,1,1,2,
                           #1,# To remove weibul
                           NA,NA,NA,NA,NA,NA,NA,NA,NA),#,NA,NA),
       lwd = c(4,2,2,2,2,2,2,2,2,
               #2,# To remove weibul
               NA,NA,NA,NA,NA,NA,NA,NA,NA),#,NA,NA), 
       pch = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,
               #NA,# To remove weibul
               15,15,15,15,15,15,15,15,15),#,13,10),
       col = c("grey40","red","darkred","green","blue","magenta","blue","purple","darkgreen",
               #"darkorange",# To remove weibul
               "red","blue","grey"),#"peru","cyan","purple","darkgreen"),
       #,"yellow3","pink3"),#,"turquoise4","turquoise4"),
       text.font = 2, cex = 1.1,
       legend = c("Limits","AEP4","GEV","GLO","GNO","GOV","GPA","PE3","PDQ3",
                  #"WEI",# To remove weibul
                  "10","15","20"))#,"25","30","35","40","45","50"))#,"CAU","SLA"))

for (i in 1:nrow(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise)){
  
  if (i == 1){
    col_point<- "red"
  }else if (i == 2){
    col_point<- "blue"
  }else if (i == 3){
    col_point<- "grey"
  }
  # else if (i == 4){
  #   col_point<- "peru"
  # }else if (i == 5){
  #   col_point<- "cyan"
  # }else if (i == 6){
  #   col_point<- "purple"
  # }else if (i == 7){
  #   col_point<- "darkgreen"}
  # }else if (i == 8){
  #   col_point<- "yellow3"
  # }else if (i == 9){
  #   col_point<- "pink3"
  # }
  
  points(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise[i,3],l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise[i,4], pch=15,cex=2,col=col_point)
  
  
}

dev.off()






## For time_____

png(paste("D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/","l_ration_suitable_distbn_time",".png",sep = ""))
xlim_l<- 0.40
xlim_u<- 0.72
ylim_l<- -0.1
ylim_u<- 0.40
op <- par(mar = c(5,4.5,4,2) + 0.4)
plotlmrdia(lmrdia(),lwd.cex = 2,#autolegend=T, xleg="topright", legendcex=0.9,
           main="Time",
           xlim = c(xlim_l, xlim_u), ylim = c(ylim_l, ylim_u),
           #xlab = "",ylab = "",
           xlab =  expression(paste("L-Skewness (",tau[3],")")), 
           ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")),
           bty="n",
           xaxt="n",yaxt="n",axes = FALSE)
axis(1, lwd = 2,cex.lab=1.3, cex.axis=1.3,at = seq(xlim_l, xlim_u,by=0.04))
#axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
axis(2,las=1, lwd = 2,cex.lab=1.3,cex.axis=1.3, at = seq(ylim_l, ylim_u,by=0.05))


legend("topright", lty = c(1,4,1,1,2,2,1,1,2,
                           #1,# To remove weibul
                           NA,NA,NA),#NA,NA,NA,NA,NA,NA),#,NA,NA),
       lwd = c(4,2,2,2,2,2,2,2,2,
               #2,# To remove weibul
               NA,NA,NA),#NA,NA,NA,NA,NA,NA),#,NA,NA), 
       pch = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,
               #NA,# To remove weibul
               17,17,17),#17,17,17,17,17,17),#,13,10),
       col = c("grey40","red","darkred","green","blue","magenta","blue","purple","darkgreen",
               #"darkorange",# To remove weibul
               "red","blue","grey"),#"peru","cyan","purple","darkgreen"),
       #,"yellow3","pink3"),#,"turquoise4","turquoise4"),
       text.font = 2, cex = 1.1,
       legend = c("Limits","AEP4","GEV","GLO","GNO","GOV","GPA","PE3","PDQ3",
                  #"WEI",# To remove weibul
                  "10","15","20"#,"25","30","35","40"
                  #,"45","50"
       ))#,"CAU","SLA"))



for (i in 1:nrow(l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise)){
  
  if (i == 1){
    col_point<- "red"
  }else if (i == 2){
    col_point<- "blue"
  }else if (i == 3){
    col_point<- "grey"
  }
  # else if (i == 4){
  #   col_point<- "peru"
  # }else if (i == 5){
  #   col_point<- "cyan"
  # }else if (i == 6){
  #   col_point<- "purple"
  # }else if (i == 7){
  #   col_point<- "darkgreen"}
  # }else if (i == 8){
  #   col_point<- "yellow3"
  # }else if (i == 9){
  #   col_point<- "pink3"
  # }
  
  points(l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise[i,3],l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise[i,4], pch=17,cex=2,col=col_point)
  
  
}

dev.off()


#points(l_mom_time_lamb1_tou2_tou3_tou4_tow5[4],l_mom_time_lamb1_tou2_tou3_tou4_tow5[5], pch=17,cex=2,col="blue")











ind_rec_l<- which(list_of_thres_hold_record == threshold_for_records)


## Simulation of the data using the best fit distributions_________________

# For "Magnitude" three distribution PE3, AEP, and GPA_________________

vec_lmom_mag<- c(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,1],
                 l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,2],
                 l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,3],
                 l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,4],
                 l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,5])

mag_lmom<- vec2lmom(vec_lmom_mag,lscale = F)



vec_lmom_time<- c(l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,1],
                  l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,2],
                  l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,3],
                  l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,4],
                  l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,5])

time_lmom<- vec2lmom(vec_lmom_time,lscale = F)




# Selection of the sample size and record length
file_dir_sim_mag<- "D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/simulation_plot/mag/"


#temp_rec_len<- 30
#temp_rec_len<- 25
#temp_rec_len<- 20
temp_rec_len<- 10

numb_simul_gen_rec<- length(which(number_of_records[,1] >= temp_rec_len))


#Distribution for Magnitude ___________________________________
pool_of_distributions_mag<- c("GPA","PE3","GEV")

tr<- 0

for (d in 1:length(pool_of_distributions_mag)){
  
  temp_dist<- pool_of_distributions_mag[d]
  
  if (temp_dist == "AEP4"){
    para_mag<- paraep4(mag_lmom,kapapproved = F,snap.tau4=T)
    #para_time<- paraep4(time_lmom,kapapproved = F,snap.tau4=T)
  }else if (temp_dist == "GPA"){
    para_mag<- pargpa(mag_lmom)
    #para_time<- pargpa(time_lmom)
  }else if (temp_dist == "PE3"){
    para_mag<- parpe3(mag_lmom,checklmom = T)
    #para_time<- parpe3(time_lmom,checklmom = T)
  }else if (temp_dist == "GOV"){
    para_mag<- pargov(mag_lmom)
    #para_time<- parpe3(time_lmom,checklmom = T)
  }else if (temp_dist == "GEV"){
    para_mag<- pargev(mag_lmom)
    #para_time<- pargev(time_lmom)
  }
  
  
  l_mom_mag_lamb1_tou2_tou3_tou4_tow5_sim<- matrix(nrow = numb_simul_gen_rec, ncol = 5)
  for (s in 1:numb_simul_gen_rec){
    
    tr<- tr + 1
    
    number_rec<- temp_rec_len
    unif_numb<- runif(number_rec)
    
    
    if (temp_dist == "AEP4"){
      generated_records_mag<- quaaep4(unif_numb,para_mag)
      #generated_records_time<- quaaep4(unif_numb,para_time)
    }else if (temp_dist == "GPA"){
      generated_records_mag<- quagpa(unif_numb,para_mag)
      #generated_records_time<- quagpa(unif_numb,para_time)
    }else if (temp_dist == "PE3"){
      generated_records_mag<- quape3(unif_numb,para_mag)
      #generated_records_time<- quape3(unif_numb,para_time)
    }else if (temp_dist == "GOV"){
      generated_records_mag<- quagov(unif_numb,para_mag)
      #generated_records_time<- quape3(unif_numb,para_time)
    }else if (temp_dist == "GEV"){
      generated_records_mag<- quagev(unif_numb,para_mag)
      #generated_records_time<- quagev(unif_numb,para_time)
    }
    
    
    mag_lmoments_gen<- lmoms(generated_records_mag)
    #time_lmoments_gen<- lmoms(generated_records_mag)
    
    l_mom_mag_lamb1_tou2_tou3_tou4_tow5_sim[s,1]<- mag_lmoments_gen$lambdas[1]
    l_mom_mag_lamb1_tou2_tou3_tou4_tow5_sim[s,2]<- mag_lmoments_gen$ratios[2]
    l_mom_mag_lamb1_tou2_tou3_tou4_tow5_sim[s,3]<- mag_lmoments_gen$ratios[3]
    l_mom_mag_lamb1_tou2_tou3_tou4_tow5_sim[s,4]<- mag_lmoments_gen$ratios[4]
    l_mom_mag_lamb1_tou2_tou3_tou4_tow5_sim[s,5]<- mag_lmoments_gen$ratios[5]
    
    
    print(paste(temp_dist,"    ....   ",tr," ...To be done till........",numb_simul_gen_rec*length(pool_of_distributions_mag),sep = ""))
    
  }
  
  
  #png(paste(file_dir_sim_mag,"mag_diff",temp_dist,"_",temp_rec_len,"_legend.png",sep = ))
  png(paste(file_dir_sim_mag,"mag_diff",temp_dist,"_",temp_rec_len,".png",sep = ))
  
  xlim_l<- -0.2
  xlim_u<- 1
  ylim_l<- -0.3
  ylim_u<- 1 + 0.1
  
  ## Automation plot
  #plotlmrdia(lmrdia(),lwd.cex = 2,autolegend=T, xleg="topright", legendcex=0.9)
  op <- par(mar = c(7,6,4,2) + 0.4)
  #plot(tow3andtow4_Limits[,1], tow3andtow4_Limits[,2],main =paste("Record Length = ",temp_rec_len," (",temp_dist,")",sep=""), type = "l",lty=1, lwd=4, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="grey",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  #par(new=T)
  
  #par(new=T)
  plot(tow3andtow4_GEV[,1], tow3andtow4_GEV[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="darkred",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GLO[,1], tow3andtow4_GLO[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="green",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GNO[,1], tow3andtow4_GNO[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=2, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="blue",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GPA[,1], tow3andtow4_GPA[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="blue",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_PE3[,1], tow3andtow4_PE3[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="purple",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  
  for (i in 1:nrow(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_sim)){
    
    temp_mag_lskew<- l_mom_mag_lamb1_tou2_tou3_tou4_tow5_sim[i,3]
    temp_mag_lkurt<- l_mom_mag_lamb1_tou2_tou3_tou4_tow5_sim[i,4]
    points(temp_mag_lskew,temp_mag_lkurt, pch=0, col="orange",cex=1.5)
    
    
  }
  
  
  
  # par(new=T)
  # plotlmrdia(lmrdia(),lwd.cex = 2,#autolegend=T, xleg="topright", legendcex=0.9,
  #            xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u),
  #            #xlab = "",ylab = "",
  #            xlab =  expression(paste("L-Skewness (",tau[3],")")), 
  #            ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")),
  #            bty="n",
  #            xaxt="n",yaxt="n",axes = FALSE)
  axis(1, lwd = 2,cex.lab=1.5, cex.axis=1.5,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2,las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = seq(ylim_l, ylim_u,by=0.2))
  
  par(new=T)
  plot(tow3andtow4_GEV[,1], tow3andtow4_GEV[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="darkred",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GLO[,1], tow3andtow4_GLO[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="green",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GNO[,1], tow3andtow4_GNO[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=2, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="blue",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GPA[,1], tow3andtow4_GPA[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="blue",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_PE3[,1], tow3andtow4_PE3[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="purple",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  
  l_mom_mag_lamb1_tou2_tou3_tou4_tow5_sim_ave<- apply(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_sim, 2, mean)
  
  # Plot the averages
  
  points(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,3],
         l_mom_mag_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,4], pch=15, col="cyan",cex=2.5)
  
  points(l_mom_mag_lamb1_tou2_tou3_tou4_tow5_sim_ave[3],
         l_mom_mag_lamb1_tou2_tou3_tou4_tow5_sim_ave[4], pch=15, col="black",cex=2.5)
  
  
  # legend("topright",lty= c(#1,
  #   1,1,2,1,1,
  #   NA,NA,NA),
  #   lwd = c(#4,
  #     2,2,2,2,2,
  #     NA,NA,NA),
  #   pch = c(#NA,
  #           NA,NA,NA,NA,NA,
  #           0,15,15),
  #   col = c(#"grey40",
  #     "darkred","green","blue","blue","purple",
  #     "orange","cyan","black"),
  #   text.font = 2, cex = 0.75,
  #   legend = c(#"Limits",
  #     "GEV","GLO","GNO","GPA","PE3",
  #     "SR-MAG","AR-MAG","ASR-Mag"))
  
  
  # legend("topright", lty = c(1,4,1,1,2,2,1,1,2,
  #                            #1,# To remove weibul
  #                            NA,NA,NA,NA,NA,
  #                            NA,NA,NA),#,NA,NA),
  #        lwd = c(4,2,2,2,2,2,2,2,2,
  #                #2,# To remove weibul
  #                NA,NA,NA,NA,NA,
  #                NA,NA,NA),#,NA,NA),
  #        pch = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,
  #                #NA,# To remove weibul
  #                16,15,17,18,12,
  #                0,15,15),#,13,10),
  #        col = c("grey40","red","darkred","green","blue","magenta","blue","purple","darkgreen",
  #                #"darkorange",# To remove weibul
  #                "red","red","red","red","red",
  #                "orange","cyan","black"),#,"turquoise4","turquoise4"),
  #        text.font = 2, cex = 0.75,
  #        legend = c("Limits","AEP4","GEV","GLO","GNO","GOV","GPA","PE3","PDQ3",
  #                   #"WEI",# To remove weibul
  #                   "EXP","NOR","GUM","RAY","UNI",
  #                   "SR-MAG","AR-MAG","ASR-Mag"))#,"CAU","SLA"))
  
  
  par(new=F)
  dev.off()
  
  
}







#Distribution for Time ___________________________________
# For "Time" two distribution PE3 and GOV _________________
file_dir_sim_time<- "D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/simulation_plot/time/"

#pool_of_distributions_time<- c("GOV","PE3")

#pool_of_distributions_time<- c("AEP4","GOV","PE3")
pool_of_distributions_time<- c("GEV","GPA","PE3")




tr<- 0

for (d in 1:length(pool_of_distributions_time)){
  
  temp_dist<- pool_of_distributions_time[d]
  
  if (temp_dist == "AEP4"){
    #para_mag<- paraep4(mag_lmom,kapapproved = F,snap.tau4=T)
    para_time<- paraep4(time_lmom,kapapproved = F,snap.tau4=T)
  }else if (temp_dist == "GOV"){
    #para_mag<- pargov(mag_lmom)
    para_time<- pargov(time_lmom)
  }else if (temp_dist == "PE3"){
    #para_mag<- parpe3(mag_lmom,checklmom = T)
    para_time<- parpe3(time_lmom,checklmom = T)
  }else if (temp_dist == "GEV"){
    #para_mag<- pargev(mag_lmom)
    para_time<- pargev(time_lmom)
  }else if (temp_dist == "GPA"){
    #para_mag<- pargpa(mag_lmom)
    para_time<- pargpa(time_lmom)
  }
  
  
  l_mom_time_lamb1_tou2_tou3_tou4_tow5_sim<- matrix(nrow = numb_simul_gen_rec, ncol = 5)
  for (s in 1:numb_simul_gen_rec){
    
    tr<- tr + 1
    
    number_rec<- temp_rec_len
    unif_numb<- runif(number_rec)
    
    if (temp_dist == "AEP4"){
      #generated_records_mag<- quaaep4(unif_numb,para_mag)
      generated_records_time<- quaaep4(unif_numb,para_time)
    }else if (temp_dist == "GOV"){
      #generated_records_mag<- quagov(unif_numb,para_mag)
      generated_records_time<- quagov(unif_numb,para_time)
    }else if (temp_dist == "PE3"){
      #generated_records_mag<- quape3(unif_numb,para_mag)
      generated_records_time<- quape3(unif_numb,para_time)
    }else if (temp_dist == "GEV"){
      #generated_records_mag<- quagev(unif_numb,para_mag)
      generated_records_time<- quagev(unif_numb,para_time)
    }else if (temp_dist == "GPA"){
      #generated_records_mag<- quagpa(unif_numb,para_mag)
      generated_records_time<- quagpa(unif_numb,para_time)
    }
    
    
    #mag_lmoments_gen<- lmoms(generated_records_mag)
    time_lmoments_gen<- lmoms(generated_records_time)
    
    l_mom_time_lamb1_tou2_tou3_tou4_tow5_sim[s,1]<- time_lmoments_gen$lambdas[1]
    l_mom_time_lamb1_tou2_tou3_tou4_tow5_sim[s,2]<- time_lmoments_gen$ratios[2]
    l_mom_time_lamb1_tou2_tou3_tou4_tow5_sim[s,3]<- time_lmoments_gen$ratios[3]
    l_mom_time_lamb1_tou2_tou3_tou4_tow5_sim[s,4]<- time_lmoments_gen$ratios[4]
    l_mom_time_lamb1_tou2_tou3_tou4_tow5_sim[s,5]<- time_lmoments_gen$ratios[5]
    
    
    print(paste(temp_dist,"    ....   ",tr," ...To be done till........",numb_simul_gen_rec*length(pool_of_distributions_time),sep = ""))
    
  }
  
  
  #png(paste(file_dir_sim_time,"time_diff",temp_dist,"_",temp_rec_len,"_legend.png",sep = ))
  png(paste(file_dir_sim_time,"time_diff",temp_dist,"_",temp_rec_len,".png",sep = ))
  xlim_l<- -0.2
  xlim_u<- 1
  ylim_l<- -0.3
  ylim_u<- 1 + 0.1
  
  ## Automation plot
  #plotlmrdia(lmrdia(),lwd.cex = 2,autolegend=T, xleg="topright", legendcex=0.9)
  op <- par(mar = c(7,6,4,2) + 0.4)
  #plot(tow3andtow4_Limits[,1], tow3andtow4_Limits[,2],main =paste("Record Length = ",temp_rec_len," (",temp_dist,")",sep=""), type = "l",lty=1, lwd=4, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="grey",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  #par(new=T)
  
  #par(new=T)
  plot(tow3andtow4_GEV[,1], tow3andtow4_GEV[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="darkred",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GLO[,1], tow3andtow4_GLO[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="green",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GNO[,1], tow3andtow4_GNO[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=2, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="blue",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GPA[,1], tow3andtow4_GPA[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="blue",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_PE3[,1], tow3andtow4_PE3[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="purple",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  
  par(new=T)
  
  for (i in 1:nrow(l_mom_time_lamb1_tou2_tou3_tou4_tow5_sim)){
    
    temp_time_lskew<- l_mom_time_lamb1_tou2_tou3_tou4_tow5_sim[i,3]
    temp_time_lkurt<- l_mom_time_lamb1_tou2_tou3_tou4_tow5_sim[i,4]
    points(temp_time_lskew,temp_time_lkurt, pch=2, col="black",cex=1.5)
    
    
  }
  
  
  par(new=T)
  # plotlmrdia(lmrdia(),lwd.cex = 2,#autolegend=T, xleg="topright", legendcex=0.9,
  #            xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u),
  #            #xlab = "",ylab = "",
  #            xlab =  expression(paste("L-Skewness (",tau[3],")")), 
  #            ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")),
  #            bty="n",
  #            xaxt="n",yaxt="n",axes = FALSE)
  axis(1, lwd = 2,cex.lab=1.5, cex.axis=1.5,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2,las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = seq(ylim_l, ylim_u,by=0.2))
  
  
  l_mom_time_lamb1_tou2_tou3_tou4_tow5_sim_ave<- apply(l_mom_time_lamb1_tou2_tou3_tou4_tow5_sim, 2, mean)
  
  
  par(new=T)
  # Plot the averages
  
  plot(tow3andtow4_GEV[,1], tow3andtow4_GEV[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="darkred",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GLO[,1], tow3andtow4_GLO[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="green",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GNO[,1], tow3andtow4_GNO[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=2, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="blue",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_GPA[,1], tow3andtow4_GPA[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="blue",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  par(new=T)
  plot(tow3andtow4_PE3[,1], tow3andtow4_PE3[,2],main =paste("Record Length = ",temp_rec_len,sep=""), type = "l",lty=1, lwd=2,
       xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  expression(paste("L-Skewness (",tau[3],")")), 
       ylab = expression(paste("L-Kurtosis (",tau[4],")", sep = "")), col="purple",cex.lab=1,cex.axis=1,cex.main=1.5,
       bty="n",xaxt="n",yaxt="n",axes = FALSE)
  
  
  par(new=T)
  
  
  points(l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,3],
         l_mom_time_lamb1_tou2_tou3_tou4_tow5_average_recLwise[ind_rec_l,4], pch=17, col="pink",cex=2.5)
  
  points(l_mom_time_lamb1_tou2_tou3_tou4_tow5_sim_ave[3],
         l_mom_time_lamb1_tou2_tou3_tou4_tow5_sim_ave[4], pch=17, col="blue",cex=2.5)
  
  par(new=T)
  # legend("topright",lty= c(#1,
  #   1,1,2,1,1,
  #   NA,NA,NA),
  #   lwd = c(#4,
  #     2,2,2,2,2,
  #     NA,NA,NA),
  #   pch = c(#NA,
  #     NA,NA,NA,NA,NA,
  #     2,17,17),
  #   col = c(#"grey40",
  #     "darkred","green","blue","blue","purple",
  #     "black","pink","blue"),
  #   text.font = 2, cex = 0.75,
  #   legend = c(#"Limits",
  #     "GEV","GLO","GNO","GPA","PE3",
  #     "SR-Time","AR-Time","ASR-Time"))
  
  # legend("topright", lty = c(1,4,1,1,2,2,1,1,2,
  #                            #1,# To remove weibul
  #                            NA,NA,NA,NA,NA,
  #                            NA,NA,NA),#,NA,NA),
  #        lwd = c(4,2,2,2,2,2,2,2,2,
  #                #2,# To remove weibul
  #                NA,NA,NA,NA,NA,
  #                NA,NA,NA),#,NA,NA),
  #        pch = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,
  #                #NA,# To remove weibul
  #                16,15,17,18,12,
  #                2,17,17),#,13,10),
  #        col = c("grey40","red","darkred","green","blue","magenta","blue","purple","darkgreen",
  #                #"darkorange",# To remove weibul
  #                "red","red","red","red","red",
  #                "black","pink","blue"),#,"turquoise4","turquoise4"),
  #        text.font = 2, cex = 0.75,
  #        legend = c("Limits","AEP4","GEV","GLO","GNO","GOV","GPA","PE3","PDQ3",
  #                   #"WEI",# To remove weibul
  #                   "EXP","NOR","GUM","RAY","UNI",
  #                   "SR-Time","AR-Time","ASR-Time"))#,"CAU","SLA"))
  
  par(new=F)
  dev.off()
  
  
}











total_year<- 123



# Estimation of the quantiles for certain probability ____________________________
# For mag difference _________________________________


#th_rec_len<- 30 #-1 is added to take care of the differences
#th_rec_len<- 25
th_rec_len<- 10


samples_of_return_period<- c(10,50,100)

#mag_quantile_forProb_for_record_length_all_AEP4<- matrix(nrow = nrow(number_rec_lat_long), ncol = length(samples_of_return_period))
mag_quantile_forProb_for_record_length_all_EMPI<- matrix(nrow = nrow(number_rec_lat_long), ncol = length(samples_of_return_period))
mag_quantile_forProb_for_record_length_all_PE3<- matrix(nrow = nrow(number_rec_lat_long), ncol = length(samples_of_return_period))
mag_quantile_forProb_for_record_length_all_GPA<- matrix(nrow = nrow(number_rec_lat_long), ncol = length(samples_of_return_period))
mag_quantile_forProb_for_record_length_all_GEV<- matrix(nrow = nrow(number_rec_lat_long), ncol = length(samples_of_return_period))
mag_prob_for_RP_record_length_all<- matrix(nrow = nrow(number_rec_lat_long), ncol = length(samples_of_return_period))

mag_quantile_forProb_for_record_length_all_exp<- matrix(nrow = nrow(number_rec_lat_long), ncol = length(samples_of_return_period))



time_prob_for_diffRP_record_length_all_EMPI<- matrix(nrow = nrow(number_rec_lat_long), ncol = length(samples_of_return_period))
time_prob_for_diffRP_record_length_all_PE3<- matrix(nrow = nrow(number_rec_lat_long), ncol = length(samples_of_return_period))
time_prob_for_diffRP_record_length_all_GPA<- matrix(nrow = nrow(number_rec_lat_long), ncol = length(samples_of_return_period))
time_prob_for_diffRP_record_length_all_GEV<- matrix(nrow = nrow(number_rec_lat_long), ncol = length(samples_of_return_period))

time_prob_for_diffRP_record_length_all_exp<- matrix(nrow = nrow(number_rec_lat_long), ncol = length(samples_of_return_period))



#time_prob_for_diffRP_record_length_all_GOV<- matrix(nrow = nrow(number_rec_lat_long), ncol = length(samples_of_return_period))
#time_prob_for_diffRP_record_length_all_AEP4<- matrix(nrow = nrow(number_rec_lat_long), ncol = length(samples_of_return_period))

for (i in 1:ncol(time_diff_RB_event)){
  
  th_rec_length<- th_rec_len - 1
  
  temp_mag_diff<- mag_diff_RB_event[,i]
  temp_time_diff<- time_diff_RB_event[,i]
  
  temp_mag_diff_na_rm<- temp_mag_diff[!is.na(temp_mag_diff)]
  temp_time_diff_na_rm<- temp_time_diff[!is.na(temp_time_diff)]
  
  if (length(temp_mag_diff_na_rm) >= (th_rec_length)){ 
    mag_lmoments<- lmoms(temp_mag_diff_na_rm)
    time_lmoments<- lmoms(temp_time_diff_na_rm)
    
    #para_mag_aep4<- paraep4(mag_lmoments,kapapproved = F,snap.tau4=T)
    para_mag_gpa<- pargpa(mag_lmoments,checklmom = F)
    para_mag_pe3<- parpe3(mag_lmoments,checklmom = F)
    para_mag_gev<- pargev(mag_lmoments,checklmom = F)
    
    para_mag_exp<- parexp(mag_lmoments,checklmom = F)
    
    
    
    #para_time_gov<- pargov(time_lmoments,checklmom = F)
    para_time_pe3<- parpe3(time_lmoments,checklmom = F)
    para_time_gpa<- pargpa(time_lmoments,checklmom = F)
    para_time_gev<- pargev(time_lmoments,checklmom = F)
    
    para_time_exp<- parexp(time_lmoments,checklmom = F)
    #para_time_aep4<- paraep4(time_lmoments,kapapproved = F,snap.tau4=T)
    
    # cap_para_tim_aep4<- capture.output(para_time_aep4)
    # 
    # 
    # if (cap_para_tim_aep4[1] == "NULL"){
    #   next
    # }
    # 
    
    #### To compute Empirical Prob____________________________________
    sorted_mag_diff<- sort(temp_mag_diff_na_rm)
    prob_sorted_mag_diff<- matrix(nrow = length(sorted_mag_diff), ncol = 1)
    for (e in 1:length(sorted_mag_diff)){
      
      prob_sorted_mag_diff[e]<- e/((length(sorted_mag_diff))+1)
      
    }
    
    sorted_mag_diff_new<- c(0,sorted_mag_diff,max(sorted_mag_diff) + (max(sorted_mag_diff) - sorted_mag_diff[length(sorted_mag_diff) - 5]))
    prob_sorted_mag_diff_new<- c(0,prob_sorted_mag_diff,0.9999999)
    
    
    lm_prob_mag<- lm(sorted_mag_diff ~ prob_sorted_mag_diff)
    coef_mag<- lm_prob_mag$coefficients
    #predict_mag<- (lm_prob_mag$coefficients[1]) + (lm_prob_mag$coefficients[2])*0.9
    
    
    
    
    
    sorted_time_diff<- sort(temp_time_diff_na_rm)
    prob_sorted_time_diff<- matrix(nrow = length(sorted_time_diff), ncol = 1)
    for (e in 1:length(sorted_time_diff)){
      
      prob_sorted_time_diff[e]<- e/((length(sorted_time_diff))+1)
      
    }
    
    sorted_time_diff_new<- c(0,sorted_time_diff,max(sorted_time_diff) + (max(sorted_time_diff) - sorted_time_diff[length(sorted_time_diff) - 5]))
    prob_sorted_time_diff_new<- c(0,prob_sorted_time_diff,0.9999999)
    
    lm_prob_time<- lm(prob_sorted_time_diff ~ sorted_time_diff)
    coef_time<- lm_prob_time$coefficients
    
    
    ## Estimation of quantile ____________________________
    for (p in 1:length(samples_of_return_period)){
      
      temp_rp<- samples_of_return_period[p]
      
      #temp_prob<- (1-(1/temp_rp))*(length(temp_mag_diff_na_rm)/total_year)
      temp_prob<- (1-(1/temp_rp))*(length(temp_mag_diff_na_rm)/total_year)
      
      
      mag_prob_for_RP_record_length_all[i,p]<- temp_prob
      
      # Mag diff using distribution
      #aep4_generated_records_mag<- quaaep4(temp_prob,para_mag_aep4)
      gpa_generated_records_mag<- quagpa(temp_prob,para_mag_gpa)
      pe3_generated_records_mag<- quape3(temp_prob,para_mag_pe3)
      gev_generated_records_mag<- quagev(temp_prob,para_mag_gev)
      
      exp_generated_records_mag<- quaexp(temp_prob,para_mag_exp)
      
      # Mag diff Empirical
      ind_les_temp_prob_mag<- which(prob_sorted_mag_diff_new < temp_prob)
      ind_grt_temp_prob_mag<- which(prob_sorted_mag_diff_new > temp_prob)
      low_prob_mag_diff<- prob_sorted_mag_diff_new[ind_les_temp_prob_mag[length(ind_les_temp_prob_mag)]]
      up_prob_mag_diff<- prob_sorted_mag_diff_new[ind_grt_temp_prob_mag[1]]
      low_prob_mag_diff_qua<- sorted_mag_diff_new[ind_les_temp_prob_mag[length(ind_les_temp_prob_mag)]]
      if (length(low_prob_mag_diff_qua) == 0){
        low_prob_mag_diff_qua<- 0
        low_prob_mag_diff<- 0
      }
      up_prob_mag_diff_qua<- sorted_mag_diff_new[ind_grt_temp_prob_mag[1]]
      emp_generated_records_mag<- (((temp_prob - low_prob_mag_diff)/(up_prob_mag_diff - low_prob_mag_diff))*(up_prob_mag_diff_qua - low_prob_mag_diff_qua)) + low_prob_mag_diff_qua
      
      #emp_generated_records_mag<- (coef_mag[1] + (coef_mag[2])*temp_prob)
      
      #mag_quantile_forProb_for_record_length_all_AEP4[i,p]<- aep4_generated_records_mag
      mag_quantile_forProb_for_record_length_all_EMPI[i,p]<- emp_generated_records_mag
      mag_quantile_forProb_for_record_length_all_PE3[i,p]<- pe3_generated_records_mag
      mag_quantile_forProb_for_record_length_all_GPA[i,p]<- gpa_generated_records_mag
      mag_quantile_forProb_for_record_length_all_GEV[i,p]<- gev_generated_records_mag
      
      mag_quantile_forProb_for_record_length_all_exp[i,p]<- exp_generated_records_mag
      
      
      
      
      
      
      #Time diff ___________ using distribution 
      #gov_generated_records_time<- quagov(temp_prob,para_time_gov)
      #pe3_generated_records_time<- quape3(temp_prob,para_time_pe3)
      #gov_generated_prob_time<- par2cdf(temp_rp,para_time_gov)
      pe3_generated_prob_time<- par2cdf(temp_rp,para_time_pe3)
      gpa_generated_prob_time<- par2cdf(temp_rp,para_time_gpa)
      gev_generated_prob_time<- par2cdf(temp_rp,para_time_gev)
      
      exp_generated_prob_time<- par2cdf(temp_rp,para_time_exp)
      #aep4_generated_prob_time<- par2cdf(temp_rp,para_time_aep4)
      
      # Time diff Empirical prob computation
      ind_les_temp_rp_time<- which(sorted_time_diff_new < temp_rp)
      ind_grt_temp_rp_time<- which(sorted_time_diff_new > temp_rp)
      low_rp_time_diff<- sorted_time_diff_new[ind_les_temp_rp_time[length(ind_les_temp_rp_time)]]
      up_rp_time_diff<- sorted_time_diff_new[ind_grt_temp_rp_time[1]]
      low_prob_time_diff_rp<- prob_sorted_time_diff_new[ind_les_temp_rp_time[length(ind_les_temp_rp_time)]]
      up_prob_time_diff_rp<- prob_sorted_time_diff_new[ind_grt_temp_rp_time[1]]
      
      #emp_generated_records_time<- (((temp_prob - low_prob_time_diff)/(up_prob_time_diff - low_prob_time_diff))*(up_prob_time_diff_qua - low_prob_time_diff_qua)) + low_prob_time_diff_qua
      
      emp_generated_prob_diff_time<- (((temp_rp - low_rp_time_diff)/(up_rp_time_diff - low_rp_time_diff))*(up_prob_time_diff_rp - low_prob_time_diff_rp)) + low_prob_time_diff_rp
      #emp_generated_prob_diff_time<- coef_time[1] + ((coef_time[2])*temp_rp)
      
      
      time_prob_for_diffRP_record_length_all_EMPI[i,p]<- emp_generated_prob_diff_time 
      time_prob_for_diffRP_record_length_all_PE3[i,p]<- pe3_generated_prob_time
      time_prob_for_diffRP_record_length_all_GPA[i,p]<- gpa_generated_prob_time
      time_prob_for_diffRP_record_length_all_GEV[i,p]<- gev_generated_prob_time
      
      time_prob_for_diffRP_record_length_all_exp[i,p]<- exp_generated_prob_time
      #time_prob_for_diffRP_record_length_all_GOV[i,p]<- gov_generated_prob_time
      
      #time_prob_for_diffRP_record_length_all_AEP4[i,p]<- aep4_generated_prob_time
      
      cat(p)
    }
    
    
    
    
    
    
    
    
    
  }else{
    
    #mag_quantile_forProb_for_record_length_all_AEP4[i,p]<- NA
    #mag_quantile_forProb_for_record_length_all_EMPI[i,p]<- NA
    #mag_quantile_forProb_for_record_length_all_PE3[i,p]<- NA
    #mag_quantile_forProb_for_record_length_all_GPA[i,p]<- NA
    
    
    #time_prob_for_diffRP_record_length_all_EMPI[i,p]<- NA
    #time_prob_for_diffRP_record_length_all_PE3[i,p]<- NA
    #time_prob_for_diffRP_record_length_all_GOV[i,p]<- NA
    
    #time_quantile_forProb_for_record_length_all_EMPI[i,p]<- NA
    #time_quantile_forProb_for_record_length_all_PE3[i,p]<- NA
    #time_quantile_forProb_for_record_length_all_GOV[i,p]<- NA
    
    
    
  }
  
  print(paste(i,"  ......Done.....  ","To be DONE till    ......", ncol(time_diff_RB_event),sep = ""))
}







## 1-to-1 plot on 45 degree line ____________________________________
## Mag ______________________________________________________________

file_dir_rp_1_to_1_mag<- "D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/return_period_based_qq_or_pp/mag/"


for (i in 1:length(samples_of_return_period)){
  
  # Mag plots_____________________________________________________________________
  temp_emp_quan_mag<- mag_quantile_forProb_for_record_length_all_EMPI[,i]
  #temp_aep4_quan_mag<- mag_quantile_forProb_for_record_length_all_AEP4[,i]
  temp_gpa_quan_mag<- mag_quantile_forProb_for_record_length_all_GPA[,i]
  temp_pe3_quan_mag<- mag_quantile_forProb_for_record_length_all_PE3[,i]
  temp_gev_quan_mag<- mag_quantile_forProb_for_record_length_all_GEV[,i]
  
  temp_exp_quan_mag<- mag_quantile_forProb_for_record_length_all_exp[,i]
  
  xlim_l<- 0
  ylim_l<- 0
  
  up_ylim_l<- max(max(temp_emp_quan_mag[!is.na(temp_emp_quan_mag)]),
                  max(temp_gpa_quan_mag[!is.na(temp_gpa_quan_mag)]),
                  max(temp_pe3_quan_mag[!is.na(temp_pe3_quan_mag)]),
                  max(temp_gev_quan_mag[!is.na(temp_gev_quan_mag)]),
                  max(temp_exp_quan_mag[!is.na(temp_exp_quan_mag)]))
  
  xlim_u<- up_ylim_l
  ylim_u<- up_ylim_l
  
  
  # png(paste(file_dir_rp_1_to_1_mag,"AEP4","_mag_diff","_RP_",samples_of_return_period[i],".png",sep = ))
  # op <- par(mar = c(7,6,4,2) + 0.4)
  # plot(temp_emp_quan_mag, temp_aep4_quan_mag,main =paste("AEP4: RP (Mag) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical quantile (mm)",sep = ""), ylab = paste("Model quantile (mm)", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  # axis(1, lwd = 2,cex.lab=1,cex.axis=1,at = seq(xlim_l, xlim_u,by=0.5))
  # #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  # axis(2, las=1, lwd = 2,cex.lab=1,cex.axis=1, at = seq(ylim_l, ylim_u,by=0.5))
  # abline(0,1,lwd=2)
  # dev.off()
  
  
  
  png(paste(file_dir_rp_1_to_1_mag,"GPA","_mag_diff","_RP_",samples_of_return_period[i],".png",sep = ))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(temp_emp_quan_mag, temp_gpa_quan_mag,main =paste("GPA: RP (Mag) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical quantile (mm)",sep = ""), ylab = paste("Model quantile (mm)", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1,cex.axis=1,at = seq(xlim_l, xlim_u,by=5))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1,cex.axis=1, at = seq(ylim_l, ylim_u,by=5))
  lm_for_rsq<- lm(temp_gpa_quan_mag ~ temp_emp_quan_mag) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  png(paste(file_dir_rp_1_to_1_mag,"GEV","_mag_diff","_RP_",samples_of_return_period[i],".png",sep = ))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(temp_emp_quan_mag, temp_gev_quan_mag,main =paste("GEV: RP (Mag) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical quantile (mm)",sep = ""), ylab = paste("Model quantile (mm)", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1,cex.axis=1,at = seq(xlim_l, xlim_u,by=5))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1,cex.axis=1, at = seq(ylim_l, ylim_u,by=5))
  lm_for_rsq<- lm(temp_gev_quan_mag ~ temp_emp_quan_mag) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  png(paste(file_dir_rp_1_to_1_mag,"PE3","_mag_diff","_RP_",samples_of_return_period[i],".png",sep = ))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(temp_emp_quan_mag, temp_pe3_quan_mag,main =paste("PE3: RP (Mag) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical quantile (mm)",sep = ""), ylab = paste("Model quantile (mm)", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1,cex.axis=1,at = seq(xlim_l, xlim_u,by=5))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1,cex.axis=1, at = seq(ylim_l, ylim_u,by=5))
  lm_for_rsq<- lm(temp_pe3_quan_mag ~ temp_emp_quan_mag) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  
  png(paste(file_dir_rp_1_to_1_mag,"exp","_mag_diff","_RP_",samples_of_return_period[i],".png",sep = ))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(temp_emp_quan_mag, temp_exp_quan_mag,main =paste("exp: RP (Mag) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical quantile (mm)",sep = ""), ylab = paste("Model quantile (mm)", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1,cex.axis=1,at = seq(xlim_l, xlim_u,by=5))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1,cex.axis=1, at = seq(ylim_l, ylim_u,by=5))
  lm_for_rsq<- lm(temp_exp_quan_mag ~ temp_emp_quan_mag) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  
}






file_dir_rp_1_to_1_time<- "D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/return_period_based_qq_or_pp/time/"

for (i in 1:length(samples_of_return_period)){
  
  # Time plots_____________________________________________________________________
  temp_emp_quan_time<- time_prob_for_diffRP_record_length_all_EMPI[,i]
  #temp_gov_quan_time<- time_prob_for_diffRP_record_length_all_GOV[,i]
  temp_pe3_quan_time<- time_prob_for_diffRP_record_length_all_PE3[,i]
  #temp_aep4_quan_time<- time_prob_for_diffRP_record_length_all_AEP4[,i]
  temp_gpa_quan_time<- time_prob_for_diffRP_record_length_all_GPA[,i]
  temp_gev_quan_time<- time_prob_for_diffRP_record_length_all_GEV[,i]
  
  temp_exp_quan_time<- time_prob_for_diffRP_record_length_all_exp[,i]
  
  xlim_l<- 0
  ylim_l<- 0
  
  #up_ylim_l<- max(max(temp_emp_quan_mag[!is.na(temp_emp_quan_mag)]),
  #max(temp_aep4_quan_mag[!is.na(temp_aep4_quan_mag)]))
  
  #xlim_u<- up_ylim_l
  #ylim_u<- up_ylim_l
  
  xlim_u<- 1
  ylim_u<- 1
  
  
  # png(paste(file_dir_rp_1_to_1_time,"GOV","_time_diff","_RP_",samples_of_return_period[i],".png",sep = ))
  # op <- par(mar = c(7,6,4,2) + 0.4)
  # plot(temp_emp_quan_time, temp_gov_quan_time,main =paste("GOV: RP (Time) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  # axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.05))
  # #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  # axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.05))
  # abline(0,1,lwd=2)
  # dev.off()
  
  png(paste(file_dir_rp_1_to_1_time,"GPA","_time_diff","_RP_",samples_of_return_period[i],".png",sep = ))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(temp_emp_quan_time, temp_gpa_quan_time,main =paste("GPA: RP (Time) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(temp_gpa_quan_time ~ temp_emp_quan_time) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  # Boxplot _________________
  png(paste(file_dir_rp_1_to_1_time,"boxplot/","boxplot_probability_","GPA",samples_of_return_period[i],".png",sep = ""))
  op <- par(mar = c(7,4.5,4,2) + 0.4)
  #boxplot(per_abs_bias_dataEXCProb, names = c(""),xlab="",ylab="Percentage A-Bias (%)",main=paste("Percentage A-Bias of Joint Prob",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  boxplot(temp_gpa_quan_time, names = c(""),xlab="",ylab="Probability",main=paste("Probability (GPA)",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  #boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
  box(lwd=2)
  abline(h=0.5,lwd=2)
  dev.off()
  
  
  
  
  
  
  
  png(paste(file_dir_rp_1_to_1_time,"GEV","_time_diff","_RP_",samples_of_return_period[i],".png",sep = ))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(temp_emp_quan_time, temp_gev_quan_time,main =paste("GEV: RP (Time) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(temp_gev_quan_time ~ temp_emp_quan_time) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  # Boxplot _________________
  png(paste(file_dir_rp_1_to_1_time,"boxplot/","boxplot_probability_","GEV",samples_of_return_period[i],".png",sep = ""))
  op <- par(mar = c(7,4.5,4,2) + 0.4)
  #boxplot(per_abs_bias_dataEXCProb, names = c(""),xlab="",ylab="Percentage A-Bias (%)",main=paste("Percentage A-Bias of Joint Prob",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  boxplot(temp_gev_quan_time, names = c(""),xlab="",ylab="Probability",main=paste("Probability (GEV)",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  #boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
  box(lwd=2)
  abline(h=0.5,lwd=2)
  dev.off()
  
  
  
  
  png(paste(file_dir_rp_1_to_1_time,"PE3","_time_diff","_RP_",samples_of_return_period[i],".png",sep = ))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(temp_emp_quan_time, temp_pe3_quan_time,main =paste("PE3: RP (Time) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(temp_pe3_quan_time ~ temp_emp_quan_time) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  # Boxplot _________________
  png(paste(file_dir_rp_1_to_1_time,"boxplot/","boxplot_probability_","PE3",samples_of_return_period[i],".png",sep = ""))
  op <- par(mar = c(7,4.5,4,2) + 0.4)
  #boxplot(per_abs_bias_dataEXCProb, names = c(""),xlab="",ylab="Percentage A-Bias (%)",main=paste("Percentage A-Bias of Joint Prob",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  boxplot(temp_pe3_quan_time, names = c(""),xlab="",ylab="Probability",main=paste("Probability (PE3)",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  #boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
  box(lwd=2)
  abline(h=0.5,lwd=2)
  dev.off()
  
  
  
  
  
  png(paste(file_dir_rp_1_to_1_time,"exp","_time_diff","_RP_",samples_of_return_period[i],".png",sep = ))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(temp_emp_quan_time, temp_exp_quan_time,main =paste("exp: RP (Time) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(temp_exp_quan_time ~ temp_emp_quan_time) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  # png(paste(file_dir_rp_1_to_1_time,"AEP4","_time_diff","_RP_",samples_of_return_period[i],".png",sep = ))
  # op <- par(mar = c(7,6,4,2) + 0.4)
  # plot(temp_emp_quan_time, temp_aep4_quan_time,main =paste("AEP4: RP (Time) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  # axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.05))
  # #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  # axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.05))
  # abline(0,1,lwd=2)
  # dev.off()
  
  ### Plot for probability of excedence______________
  exc_temp_emp_quan_time<- 1 - temp_emp_quan_time
  #exc_temp_gov_quan_time<- 1 - temp_gov_quan_time
  exc_temp_pe3_quan_time<- 1 - temp_pe3_quan_time
  exc_temp_gpa_quan_time<- 1 - temp_gpa_quan_time
  exc_temp_gev_quan_time<- 1 - temp_gev_quan_time
  exc_temp_exp_quan_time<- 1 - temp_exp_quan_time
  
  #exc_temp_aep4_quan_time<- 1 - temp_aep4_quan_time
  
  xlim_l<- 0
  ylim_l<- 0
  
  xlim_u<- 1
  ylim_u<- 1
  
  # png(paste(file_dir_rp_1_to_1_time,"exc_","GOV","_time_diff","_RP_",samples_of_return_period[i],".png",sep = ""))
  # op <- par(mar = c(7,6,4,2) + 0.4)
  # plot(exc_temp_emp_quan_time, exc_temp_gov_quan_time,main =paste("exc_","GOV: RP (Time) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  # axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.05))
  # #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  # axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.05))
  # abline(0,1,lwd=2)
  # dev.off()
  
  png(paste(file_dir_rp_1_to_1_time,"exc_","GPA","_time_diff","_RP_",samples_of_return_period[i],".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(exc_temp_emp_quan_time, exc_temp_gpa_quan_time,main =paste("exc_","GPA: RP (Time) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(exc_temp_gpa_quan_time ~ exc_temp_emp_quan_time) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  png(paste(file_dir_rp_1_to_1_time,"exc_","GEV","_time_diff","_RP_",samples_of_return_period[i],".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(exc_temp_emp_quan_time, exc_temp_gev_quan_time,main =paste("exc_","GEV: RP (Time) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(exc_temp_gev_quan_time ~ exc_temp_emp_quan_time) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  
  png(paste(file_dir_rp_1_to_1_time,"exc_","PE3","_time_diff","_RP_",samples_of_return_period[i],".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(exc_temp_emp_quan_time, exc_temp_exp_quan_time,main =paste("exc_","PE3: RP (Time) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(exc_temp_pe3_quan_time ~ exc_temp_emp_quan_time) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  
  png(paste(file_dir_rp_1_to_1_time,"exc_","exp","_time_diff","_RP_",samples_of_return_period[i],".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(exc_temp_emp_quan_time, exc_temp_exp_quan_time,main =paste("exc_","exp: RP (Time) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(exc_temp_exp_quan_time ~ exc_temp_emp_quan_time) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  # png(paste(file_dir_rp_1_to_1_time,"exc_","AEP4","_time_diff","_RP_",samples_of_return_period[i],".png",sep = ""))
  # op <- par(mar = c(7,6,4,2) + 0.4)
  # plot(exc_temp_emp_quan_time, exc_temp_aep4_quan_time,main =paste("exc_","AEP4: RP (Time) = ",samples_of_return_period[i],sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  # axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.05))
  # #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  # axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.05))
  # abline(0,1,lwd=2)
  # dev.off()
  
}




## Boxplot ________________________________________________________________
## Absolute bias ___________________________________________________

#mag_quantile_forProb_for_record_length_all_EMPI

#abs_bias_mag_AEP4<- ((abs(mag_quantile_forProb_for_record_length_all_AEP4 - mag_quantile_forProb_for_record_length_all_EMPI))/mag_quantile_forProb_for_record_length_all_EMPI)*100
abs_bias_mag_GPA<- ((abs(mag_quantile_forProb_for_record_length_all_GPA - mag_quantile_forProb_for_record_length_all_EMPI))/mag_quantile_forProb_for_record_length_all_EMPI)*100
abs_bias_mag_PE3<- ((abs(mag_quantile_forProb_for_record_length_all_PE3 - mag_quantile_forProb_for_record_length_all_EMPI))/mag_quantile_forProb_for_record_length_all_EMPI)*100
abs_bias_mag_GEV<- ((abs(mag_quantile_forProb_for_record_length_all_GEV - mag_quantile_forProb_for_record_length_all_EMPI))/mag_quantile_forProb_for_record_length_all_EMPI)*100


#time_prob_for_diffRP_record_length_all_EMPI
#abs_bias_time_GOV<- ((abs(time_prob_for_diffRP_record_length_all_GOV - time_prob_for_diffRP_record_length_all_EMPI))/time_prob_for_diffRP_record_length_all_EMPI)*100
abs_bias_time_PE3<- ((abs(time_prob_for_diffRP_record_length_all_PE3 - time_prob_for_diffRP_record_length_all_EMPI))/time_prob_for_diffRP_record_length_all_EMPI)*100
abs_bias_time_GPA<- ((abs(time_prob_for_diffRP_record_length_all_GPA - time_prob_for_diffRP_record_length_all_EMPI))/time_prob_for_diffRP_record_length_all_EMPI)*100
abs_bias_time_GEV<- ((abs(time_prob_for_diffRP_record_length_all_GEV - time_prob_for_diffRP_record_length_all_EMPI))/time_prob_for_diffRP_record_length_all_EMPI)*100




# library(ggplot2)
# 
# abs_bias_mag_df<- data.frame(cbind(abs_bias_mag_AEP4,abs_bias_mag_GPA,abs_bias_mag_PE3))
# names(abs_bias_mag_df)<- c("AEP4-10","AEP4-50","AEP4-100",
#                            "GPA-10","GPA-50","GPA-100",
#                            "PE3-10","PE3-50","PE3-100")
# 
# 
# names_distbn_mag<- data.frame("AEP4","AEP4","AEP4",
#                               "GPA","GPA","GPA",
#                               "PE3","PE3","PE3")
# names(names_distbn_mag)<- c("AEP4-10","AEP4-50","AEP4-100",
#                            "GPA-10","GPA-50","GPA-100",
#                            "PE3-10","PE3-50","PE3-100")
# 
# 
# names_return_period_mag<- data.frame("10","50","100",
#                                      "10","50","100",
#                                      "10","50","100")
# names(names_return_period_mag)<- c("AEP4-10","AEP4-50","AEP4-100",
#                            "GPA-10","GPA-50","GPA-100",
#                            "PE3-10","PE3-50","PE3-100")
# 
# 
# 
# abs_bias_mag_df_with_NMs<- rbind(names_distbn_mag,names_return_period_mag,abs_bias_mag_df)
# 
# 
# 
# # grouped boxplot
# ggplot(abs_bias_mag_df_with_NMs, aes(x=abs_bias_mag_df, y=names_distbn_mag, fill=names_return_period_mag)) +
#   geom_boxplot()



#A10<- abs_bias_mag_AEP4[,1]
#A50<- abs_bias_mag_AEP4[,2]
#A100<- abs_bias_mag_AEP4[,3]
GP10<- abs_bias_mag_GPA[,1]
GP50<- abs_bias_mag_GPA[,2]
GP100<- abs_bias_mag_GPA[,3]
PE10<- abs_bias_mag_PE3[,1]
PE50<- abs_bias_mag_PE3[,2]
PE100<- abs_bias_mag_PE3[,3]
GEV10<- abs_bias_mag_GEV[,1]
GEV50<- abs_bias_mag_GEV[,2]
GEV100<- abs_bias_mag_GEV[,3]


DF2 <- data.frame(
  #x = c(c(A10, A50, A100), c(GP10, GP50, GP100),c(PE10,PE50,PE100)),
  #y = rep(c("AEP4", "GPA", "PE3"), each = (length(samples_of_return_period))*(nrow(abs_bias_mag_AEP4))),
  x = c(c(GP10, GP50, GP100),c(PE10,PE50,PE100),c(GEV10, GEV50, GEV100)),
  y = rep(c("GPA", "PE3","GEV"), each = (length(samples_of_return_period))*(nrow(abs_bias_mag_GPA))),
  z = rep(rep(c("10","50","100"), each=nrow(abs_bias_mag_GPA)), 3*length(samples_of_return_period)),
  stringsAsFactors = FALSE
)

# cols <- rainbow(3, s = 0.5)
# boxplot(x ~ z + y, data = DF2,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,100),
#         at = c(1:3, 5:7, 10:12), col = cols,
#         names = c("", "AEP4", "","", "GPA", "", "", "PE3", ""), xaxs = FALSE)
# legend("topleft", fill = cols, legend = c(1,2,3), horiz = T)
# 

file_dir_rp_boxplot<- "D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/return_period_based_qq_or_pp/"


png(paste(file_dir_rp_boxplot,"error_mag_diff_leg",".png",sep = ))
#png(paste(file_dir_rp_boxplot,"error_mag_diff",".png",sep = ))
cols <- rainbow(3, s = 0.5)
op <- par(mar = c(7,6,4,2) + 0.4)
boxplot(x ~ z + y, data = DF2,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=2,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,100),
        at = c(1:3, 6:8, 11:13), col = cols,xlab = "",ylab = "A-bias (%)",
        names = c("", "GPA", "", "", "PE3", "", "", "GEV", ""), xaxs = FALSE,bty="n",xaxt="n",yaxt="n",main="Mag Diff")

axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = c(2,7,12),labels = c("GPA", "PE3","GEV"))
#axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(0,100,by=20))
box(lwd=2)
#legend("topleft", fill = cols, legend = c("RP = 10","RP = 50","RP = 100"), horiz = T)
legend("topright", fill = cols, legend = c("RP = 10","RP = 50","RP = 100"), horiz = F)
dev.off()






## Boxplot for time diff

#GOV10<- abs_bias_time_GOV[,1]
#GOV50<- abs_bias_time_GOV[,2]
#GOV100<- abs_bias_time_GOV[,3]
GP10_time<- abs_bias_time_GPA[,1]
GP50_time<- abs_bias_time_GPA[,2]
GP100_time<- abs_bias_time_GPA[,3]
PE10_time<- abs_bias_time_PE3[,1]
PE50_time<- abs_bias_time_PE3[,2]
PE100_time<- abs_bias_time_PE3[,3]
GEV10_time<- abs_bias_time_GEV[,1]
GEV50_time<- abs_bias_time_GEV[,2]
GEV100_time<- abs_bias_time_GEV[,3]

DF2_time <- data.frame(
  x = c(c(GP10_time, GP50_time, GP100_time), c(PE10_time,PE50_time,PE100_time),c(GEV10_time, GEV50_time, GEV100_time)),
  y = rep(c("GPA", "PE3", "GEV"), each = (length(samples_of_return_period))*(nrow(abs_bias_time_GPA))),
  z = rep(rep(c("10","50","100"), each=nrow(abs_bias_time_GPA)), 3*length(samples_of_return_period)),
  stringsAsFactors = FALSE
)

png(paste(file_dir_rp_boxplot,"error_time_diff_leg",".png",sep = ))
#png(paste(file_dir_rp_boxplot,"error_time_diff",".png",sep = ))
cols <- rainbow(3, s = 0.5)
op <- par(mar = c(7,6,4,2) + 0.4)
boxplot(x ~ z + y, data = DF2_time,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=2,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,100),
        at = c(1:3, 6:8, 11:13), col = cols,xlab = "",ylab = "A-bias (%)",
        names = c("", "GPA", "", "", "PE3", "","", "GEV", ""), xaxs = FALSE,bty="n",xaxt="n",yaxt="n",main="Time Diff")

axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = c(2,7,12),labels = c("GPA", "PE3","GEV"))
#axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(0,100,by=20))
box(lwd=2)
#legend("topleft", fill = cols, legend = c("RP = 10","RP = 50","RP = 100"), horiz = T)
legend("topright", fill = cols, legend = c("RP = 10","RP = 50","RP = 100"), horiz = F)
dev.off()









