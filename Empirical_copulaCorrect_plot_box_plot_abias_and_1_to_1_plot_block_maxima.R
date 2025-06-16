




rm(list=ls())
library(copula)
library(lmomco)



## Related to import of data
#mag_diff_RB_event<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/discus_Prof_G/max_2_day/no_dayGAP/mag_difference_record_breaking_event_0_days_gap.csv")
mag_diff_RB_event<- read.csv(file = "D:/Kalai_IITR_office_pc/data_prepared/block_Maxima/mag_dif_block_maxima.csv")
mag_diff_RB_event<- mag_diff_RB_event[,-1]


#time_diff_RB_event<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/discus_Prof_G/max_2_day/no_dayGAP/time_difference_record_breaking_event_0_days_gap.csv")
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
#input_lat_long<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/IMD_data_Long_lat_removing_NA.csv")
input_lat_long<- read.csv(file = "D:/Kalai_IITR_office_pc/data_prepared/IMD_data_Long_lat_removing_NA.csv")
input_lat_long<- t(input_lat_long)
lat_long_data<- input_lat_long[-1,]
num_rec_lat_long<- cbind(lat_long_data,number_of_records)
#number_rec_lat_long<- data.frame(num_rec_lat_long[,-3])
number_rec_lat_long<- data.frame(num_rec_lat_long)
names(number_rec_lat_long)<- c("lon","lat","num_rec")





# number_of_records<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/number_times_record_broke.csv")
# input_lat_long<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/IMD_data_Long_lat_removing_NA.csv")
# input_lat_long<- t(input_lat_long)
# lat_long_data<- input_lat_long[-1,]
# num_rec_lat_long<- cbind(lat_long_data,number_of_records)
# number_rec_lat_long<- data.frame(num_rec_lat_long[,-3])
# names(number_rec_lat_long)<- c("lon","lat","num_rec")




## ______________________Related to lmoment ratios
#mag_diff_RB_event<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/mag_diff_record_breaking_event.csv")
#mag_diff_RB_event<- mag_diff_RB_event[,-1]


#time_diff_RB_event<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/time_diff_in_years_record_breaking_event.csv")
#time_diff_RB_event<-time_diff_RB_event[,-1]


mag_difference_record_breaking_event<- mag_diff_RB_event
time_diff_record_breaking_event<- time_diff_RB_event




threshold_for_records<- 10


# Identify the sites where the correlation is high _____ betwn the mag exceedence and time diff
# ________________________________________________________________________________________

correlation_mag_diff_and_time_diff<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))
for (i in 1:ncol(time_diff_record_breaking_event)){
  
  if (number_of_records[i,1] >= threshold_for_records){
    temp_time_diff<- time_diff_record_breaking_event[,i]
    temp_mag_diff<- mag_difference_record_breaking_event[,i]
    
    na_rm_time_diff<- temp_time_diff[!is.na(temp_time_diff)]
    na_rm_mag_diff<- temp_mag_diff[!is.na(temp_mag_diff)]
    
    
    correlation_mag_diff_and_time_diff[1,i]<- cor(na_rm_time_diff, na_rm_mag_diff)
    
  }
  cat(i)
}





## _________________________________ Spatial plot of correlation ____________________
library(raster)
###Spatial plot_________________________
library(maptools)
#india_watershed_di<- readShapeSpatial("D:/Research_IIT_Roorkee/India_shapefile/india_India_Country_Boundary_MAPOG/india_India_Country_Boundary.shp")
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




#png(filename="D:/Research_IIT_Roorkee/Record_breaking_events/plots_daysgap/discus_Prof_G/max_2_day/no_dayGAP/cor_mag_time_diff_20_yers.png")
png(filename="D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/cor_mag_time_diff_10_yers.png")
map(india_watershed_di,xlim=c(67.5,98), ylim=c(6,38),lwd=2)
title(main="Correlation (Diff in Mag and Time)")



map.axes(cex.axis=1.2,lwd=3,at=c(long_at,lat_at),
         labels=c('70°E','75°E','80°E','85°E','90°E','95°E','10°N','15°N','20°N','25°N',"30°N",'35°N'))


north(xy=cbind(95,34),type=2)



ranges_of_corr<- c(-0.3,-0.1,0.1,0.3,0.5,0.7,0.9,1.0)

records_length_vec<- ranges_of_corr

values_range_rec<- matrix(nrow = (length(records_length_vec) - 1) ,ncol = 2)
for (i in 1:(length(records_length_vec) - 1)){
  
  values_range_rec[i,1]<- records_length_vec[i]+0.01
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
  
  temp_cor<- correlation_mag_diff_and_time_diff[1,i]
  
  if (is.na(temp_cor) == T){
    col_pnt<- "black"
  }else if (temp_cor >= values_range_rec[1,1] & temp_cor <= values_range_rec[1,2]){
    col_pnt<- names_colours_rl[1]
  }else if (temp_cor >= values_range_rec[2,1] & temp_cor <= values_range_rec[2,2]){
    col_pnt<- names_colours_rl[2]
  }else if (temp_cor >= values_range_rec[3,1] & temp_cor <= values_range_rec[3,2]){
    col_pnt<- names_colours_rl[3]
  }else if (temp_cor >= values_range_rec[4,1] & temp_cor <= values_range_rec[4,2]){
    col_pnt<- names_colours_rl[4]
  }else if (temp_cor >= values_range_rec[5,1] & temp_cor <= values_range_rec[5,2]){
    col_pnt<- names_colours_rl[5]
  }else if (temp_cor >= values_range_rec[6,1] & temp_cor <= values_range_rec[6,2]){
    col_pnt<- names_colours_rl[6]
  }else if (temp_cor >= values_range_rec[7,1] & temp_cor <= values_range_rec[7,2]){
    col_pnt<- names_colours_rl[7]
  }
  
  
  
  points(temp_long,temp_lat,col = col_pnt, cex = 1.5,pch=16)
  
  
  print(paste(i," Done.....","To be done till.....      ",nrow(number_rec_lat_long),sep = ""))
}


dif_lat_ind<- 1
lat_st_ind<- 16.5

for (i in 1:length(names_colours_rl)){
  lat_pnt<- lat_st_ind - (dif_lat_ind*i)
  points(85,lat_pnt,col = names_colours_rl[i], cex = 1,pch=15)
  #text(87.5, lat_pnt, as.character(samp_rl[i]),cex=0.7)
  #if (i == 1){
  #first_rng<- as.character(round(values_range_rec[i,1],0))
  #}else{
  #first_rng<- as.character(round((values_range_rec[i,1]),0))
  #}
  
  #last_rng<- as.character(round(values_range_rec[i,2],0))
  
  first_rng<- as.character(values_range_rec[i,1])
  last_rng<- as.character(values_range_rec[i,2])
  
  text(88.5, (lat_pnt+0.2), paste("  ",first_rng,"   :   ",last_rng,sep=""),cex=0.8)
}

points(85,lat_pnt-dif_lat_ind,col = "black", cex = 1.5,pch=15)
text(87.5, (lat_pnt-dif_lat_ind-0.02), paste("     < 10  yr RL",sep=""),cex=0.8)

dev.off()







# Identify the best RB mag diff for 1 year __________________________________
##___________________________________________________________________________
diff_mag_for1_year_above_threshold<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))
for (i in 1:ncol(time_diff_record_breaking_event)){
  
  if (number_of_records[i,1] >= threshold_for_records){
    temp_time_diff<- time_diff_record_breaking_event[,i]
    temp_mag_diff<- mag_difference_record_breaking_event[,i]
    
    na_rm_time_diff<- temp_time_diff[!is.na(temp_time_diff)]
    na_rm_mag_diff<- temp_mag_diff[!is.na(temp_mag_diff)]
    
    
    dif_from_1<- abs(na_rm_time_diff - 1)
    ind_time_close_to_1_YR<- which(dif_from_1 == min(dif_from_1))
    
    
    diff_mag_for1_year_above_threshold[1,i]<- na_rm_mag_diff[ind_time_close_to_1_YR[1]]
    
  }
  cat(i)
}


ave_mag_RB_for_1yr<- mean(diff_mag_for1_year_above_threshold[!is.na(diff_mag_for1_year_above_threshold)])







## Identify the total absolute bias ________________________________________--

#diff_mag_for1_year_above_threshold<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))

total_abs_bias_normal<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))
total_abs_bias_gumbel<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))
total_abs_bias_clayton<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))
total_abs_bias_frank<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))

for (i in 1:ncol(time_diff_record_breaking_event)){
  
  if (number_of_records[i,1] >= threshold_for_records){
    temp_time_diff<- time_diff_record_breaking_event[,i]
    temp_mag_diff<- mag_difference_record_breaking_event[,i]
    
    na_rm_time_diff<- temp_time_diff[!is.na(temp_time_diff)]
    na_rm_mag_diff<- temp_mag_diff[!is.na(temp_mag_diff)]
    
    
    kendall_tou<- corKendall(cbind(matrix(na_rm_mag_diff,ncol = 1),matrix(na_rm_time_diff,ncol = 1)))
    
    data_dif_mag_time<- cbind(matrix(na_rm_mag_diff,ncol = 1),matrix(na_rm_time_diff,ncol = 1))
    ori_prob_mag<- 1 - pobs(na_rm_mag_diff)
    ori_prob_time<- pobs(na_rm_time_diff)
    probs_dif_mag_time<- cbind(matrix(ori_prob_mag,ncol = 1),matrix(ori_prob_time,ncol = 1))
    
    ## Fit different copula _____________________________________________________-
    normal.cop<- normalCopula(iTau(normalCopula(), kendall_tou[1,2]))
    gumbel.cop <- gumbelCopula (iTau(gumbelCopula(), kendall_tou[1,2]))
    clayton.cop<- claytonCopula (iTau(claytonCopula(), kendall_tou[1,2]))
    frank.cop<- frankCopula (iTau(claytonCopula(), kendall_tou[1,2]))
    
    
    # Estimate probs  _________________________________________
    prob_normal<- pCopula(probs_dif_mag_time, normal.cop)
    prob_gumbel<- pCopula(probs_dif_mag_time, gumbel.cop)
    prob_clayton<- pCopula(probs_dif_mag_time, clayton.cop)
    prob_frank<- pCopula(probs_dif_mag_time, frank.cop)
    
    
    
    ## Empirical copula ___________________________________________________
    
    emp_cupula_manual<- matrix(nrow = length(na_rm_time_diff), ncol = 1) # Prob of non exceedences
    emp_cupula_manual_corecT<- matrix(nrow = length(na_rm_time_diff), ncol = 1)
    #prob_exceedence_emp<- matrix(nrow = length(na_rm_time_diff), ncol = 1)
    emp_frequ_manual<- matrix(nrow = length(na_rm_time_diff), ncol = 1)
    emp_frequ_manual_corecT<- matrix(nrow = length(na_rm_time_diff), ncol = 1)
    #length_inter<- matrix(nrow = length(na_rm_time_diff), ncol = 1)
    for (j in 1:length(na_rm_time_diff)){
      
      temp_timDif<- na_rm_time_diff[j]
      temp_magDif<- na_rm_mag_diff[j]
      
      ind_lesE_sub_tim_dif<- which(na_rm_time_diff <= temp_timDif)
      
      
      ind_lesE_sub_mag_dif<- which(na_rm_mag_diff <= temp_magDif)
      ind_gretE_sub_mag_dif<- which(na_rm_mag_diff >= temp_magDif)
      
      
      intersec_ind<- intersect(ind_lesE_sub_mag_dif, ind_lesE_sub_tim_dif)
      intersec_ind_gretE<- intersect(ind_gretE_sub_mag_dif, ind_lesE_sub_tim_dif)
      
      total_ind_length<- length(intersec_ind)
      total_ind_length_gretE<- length(intersec_ind_gretE)
      #length_inter[i,1]<- total_ind_length
      
      # Gringorten plotting position ________________________________________
      #emp_cupula_manual[j,1]<- (total_ind_length - 0.44)/((length(na_rm_time_diff))+0.12)
      
      # Weibul plotting position ____________________________________________
      emp_cupula_manual[j,1]<- (total_ind_length)/((length(na_rm_time_diff)))
      emp_cupula_manual_corecT[j,1]<- (total_ind_length_gretE)/((length(na_rm_time_diff)))
      
      
      #Frequency ____________________________________________________________
      emp_frequ_manual[j,1]<- (total_ind_length)/((length(na_rm_time_diff)))
      emp_frequ_manual_corecT[j,1]<- (total_ind_length_gretE)/((length(na_rm_time_diff)))
      # prob of exceedences _________________________________________________
      #prob_exceedence_emp[i,1]<- 1 - ((total_ind_length - 0.44)/((length(na_rm_time_diff))+0.12))
    }
    
    
    abs_bias_normal<- abs(prob_normal - emp_cupula_manual_corecT[,1])
    abs_bias_gumbel<- abs(prob_gumbel - emp_cupula_manual_corecT[,1])
    abs_bias_clayton<- abs(prob_clayton - emp_cupula_manual_corecT[,1])
    abs_bias_frank<- abs(prob_frank - emp_cupula_manual_corecT[,1])
    
    
    #diff_mag_for1_year_above_threshold[1,i]<- na_rm_mag_diff[ind_time_close_to_1_YR[1]]
    
    
    
    total_abs_bias_normal[1,i]<- ((sum(abs_bias_normal))/(sum(emp_cupula_manual[,1])))*100
    total_abs_bias_gumbel[1,i]<- ((sum(abs_bias_gumbel))/(sum(emp_cupula_manual[,1])))*100
    total_abs_bias_clayton[1,i]<- ((sum(abs_bias_clayton))/(sum(emp_cupula_manual[,1])))*100
    total_abs_bias_frank[1,i]<- ((sum(abs_bias_frank))/(sum(emp_cupula_manual[,1])))*100
    
    
  }
  cat(i)
}



ind_na_normal<- which(is.na(total_abs_bias_normal) == T)
length(ind_na_normal)

ind_na_gumbel<- which(is.na(total_abs_bias_gumbel) == T)
length(ind_na_gumbel)

ind_na_clayton<- which(is.na(total_abs_bias_clayton) == T)
length(ind_na_clayton)

ind_na_frank<- which(is.na(total_abs_bias_frank) == T)
length(ind_na_frank)



## Combine all the percent abs bias ___________________

a_bias_percent<- cbind(matrix(total_abs_bias_normal[,-ind_na_normal], ncol = 1),
                       matrix(total_abs_bias_gumbel[,-ind_na_normal], ncol = 1),
                       matrix(total_abs_bias_clayton[,-ind_na_normal], ncol = 1),
                       matrix(total_abs_bias_frank[,-ind_na_normal], ncol = 1))


a_bias_percent_df<- data.frame(a_bias_percent)
names(a_bias_percent_df)<- c("Gaussian","Gumbel","Clayton","Frank")
names_copula<- c("Gaussian","Gumbel","Clayton","Frank")

#boxplot(a_bias_percent_df)

#png(paste("D:/Research_IIT_Roorkee/Record_breaking_events/plots_daysgap/discus_Prof_G/max_2_day/no_dayGAP/copula/","boxplot_ABiasPercentage.png",sep = ""))
png(paste("D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/copula_correct/","boxplot_ABiasPercentage.png",sep = ""))
op <- par(mar = c(7,4.5,4,2) + 0.4)
boxplot(a_bias_percent_df, names = c(names_copula),xlab="",ylab="Percentage A-Bias (%)",main=paste("Percentage A-Bias of the SUM ",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,40),las=2)
#boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
box(lwd=2)
dev.off()
















## Plot 1-to-1 plot for empirical copula vs gaussian for 1 year and 3 mm difference in RB
## ______________________________________________________________________________________

sample_rb_mag<- c(10,20,30,50)
for (m in 1:length(sample_rb_mag)){
  
  time_gap_RB<- 20
  #mag_gap_RB<- 3
  mag_gap_RB<- sample_rb_mag[m]
  
  probability_for_mag_and_time_empirical<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magG_and_timeL_empirical<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_mag_and_time_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magG_and_timeL_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  
  
  
  # Distribution based ___________________________________
  probability_for_magGPA_and_timeGPA_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magGEV_and_timeGEV_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magPE3_and_timePE3_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  
  probability_for_magGPA_and_timeGEV_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magGPA_and_timePE3_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magGEV_and_timeGPA_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magGEV_and_timePE3_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magPE3_and_timeGPA_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magPE3_and_timeGEV_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  
  probability_for_magGEV_and_timeGPA_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magPE3_and_timeGPA_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magGPA_and_timeGEV_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magPE3_and_timeGEV_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magGPA_and_timePE3_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magGEV_and_timePE3_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  
  
  
  
  
  
  probability_for_mag_and_time_clayton<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_mag_and_time_frank<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  
  
  for (i in 1:ncol(time_diff_record_breaking_event)){
    
    if (number_of_records[i,1] >= threshold_for_records){
      temp_time_diff<- time_diff_record_breaking_event[,i]
      temp_mag_diff<- mag_difference_record_breaking_event[,i]
      
      na_rm_time_diff<- temp_time_diff[!is.na(temp_time_diff)]
      na_rm_mag_diff<- temp_mag_diff[!is.na(temp_mag_diff)]
      
      
      
      
      
      
      
      kendall_tou<- corKendall(cbind(matrix(na_rm_mag_diff,ncol = 1),matrix(na_rm_time_diff,ncol = 1)))
      
      data_dif_mag_time<- cbind(matrix(na_rm_mag_diff,ncol = 1),matrix(na_rm_time_diff,ncol = 1))
      ori_prob_mag<- 1 - pobs(na_rm_mag_diff)
      ori_prob_time<- pobs(na_rm_time_diff)
      probs_dif_mag_time<- cbind(matrix(ori_prob_mag,ncol = 1),matrix(ori_prob_time,ncol = 1))
      
      ## Fit different copula _____________________________________________________-
      normal.cop<- normalCopula(iTau(normalCopula(), kendall_tou[1,2]))
      #gumbel.cop <- gumbelCopula (iTau(gumbelCopula(), kendall_tou[1,2]))
      clayton.cop<- claytonCopula (iTau(claytonCopula(), kendall_tou[1,2]))
      frank.cop<- frankCopula (iTau(claytonCopula(), kendall_tou[1,2]))
      
      
      # Identify the prob corresponding to 1 year and 3 mm rainfall_____________________
      # Prob of 1 year _________________________
      sort_time_dif<- sort(na_rm_time_diff)
      sort_time_prob<- sort(ori_prob_time)
      ind_gr_1_tim_dif<- which(sort_time_dif > time_gap_RB)
      low_time_val<- sort_time_dif[(ind_gr_1_tim_dif[1]) - 1]
      upp_time_val<- sort_time_dif[ind_gr_1_tim_dif[1]]
      low_time_prob<- sort_time_prob[(ind_gr_1_tim_dif[1]) - 1]
      upp_time_prob<- sort_time_prob[ind_gr_1_tim_dif[1]]
      
      
      if (length(low_time_val) == 0){
        low_time_val<- min(sort_time_dif) - 0.1
        low_time_prob<- min(sort_time_prob) - 0.00001
      }
      
      if (length(upp_time_val) == 0){
        upp_time_val<- max(sort_time_dif) + 1
        upp_time_prob<- max(sort_time_prob) + 0.00001
      }
      
      
      
      prob_time_RB<- (((time_gap_RB - low_time_val)/(upp_time_val - low_time_val))*(upp_time_prob - low_time_prob)) + low_time_prob
      
      
      
      
      
      
      # # Prob of >= 3 mm _________________________
      # sort_mag_dif<- sort(na_rm_mag_diff)
      # sort_mag_prob<- 1 - sort(ori_prob_mag)
      # ind_gr_1_mag_dif<- which(sort_mag_dif < mag_gap_RB)
      # low_mag_val<- sort_mag_dif[(ind_gr_1_mag_dif[1]) - 1]
      # upp_mag_val<- sort_mag_dif[ind_gr_1_mag_dif[1]]
      # low_mag_prob<- sort_mag_prob[(ind_gr_1_mag_dif[1]) - 1]
      # upp_mag_prob<- sort_mag_prob[ind_gr_1_mag_dif[1]]
      # 
      # if (length(low_mag_val) == 0){
      #   low_mag_val<- min(sort_mag_dif) - (sort_mag_dif[2] - sort_mag_dif[1])
      #   low_mag_prob<- max(sort_mag_prob) + 0.00001
      # }
      # 
      # if (length(upp_mag_val) == 0){
      #   upp_mag_val<- max(sort_mag_dif) + 1
      #   upp_mag_prob<- min(sort_mag_prob) - 0.00001
      # }
      # 
      # 
      # prob_mag_RB<- (((mag_gap_RB - low_mag_val)/(upp_mag_val - low_mag_val))*(upp_mag_prob - low_mag_prob)) + low_mag_prob
      # 
      
      
      
      # Individual Prob merged______________
      #prob_intpolated_mag_time_dif<- matrix(c(prob_mag_RB,prob_time_RB), ncol = 2)
      
      
      
      ## Either previous OR the next
      
      
      # Prob of 3 mm _________________________
      sort_mag_dif<- sort(na_rm_mag_diff)
      sort_mag_prob<- sort(ori_prob_mag)
      ind_gr_1_mag_dif<- which(sort_mag_dif > mag_gap_RB)
      low_mag_val<- sort_mag_dif[(ind_gr_1_mag_dif[1]) - 1]
      upp_mag_val<- sort_mag_dif[ind_gr_1_mag_dif[1]]
      low_mag_prob<- sort_mag_prob[(ind_gr_1_mag_dif[1]) - 1]
      upp_mag_prob<- sort_mag_prob[ind_gr_1_mag_dif[1]]
      
      if (length(low_mag_val) == 0){
        low_mag_val<- min(sort_mag_dif) - 0.001
        low_mag_prob<- min(sort_mag_prob) - 0.00001
      }
      
      if (length(upp_mag_val) == 0){
        upp_mag_val<- max(sort_mag_dif) + 1
        upp_mag_prob<- max(sort_mag_prob) + 0.00001
      }
      
      
      prob_mag_RB<- (((mag_gap_RB - low_mag_val)/(upp_mag_val - low_mag_val))*(upp_mag_prob - low_mag_prob)) + low_mag_prob
      
      # Individual Prob merged______________
      prob_intpolated_mag_time_dif<- matrix(c(1 - prob_mag_RB,prob_time_RB), ncol = 2)
      
      
      
      
      # Estimate probs  _________________________________________
      prob_normal<- pCopula(prob_intpolated_mag_time_dif, normal.cop)
      #prob_gumbel<- pCopula(probs_dif_mag_time, gumbel.cop)
      prob_clayton<- pCopula(prob_intpolated_mag_time_dif, clayton.cop)
      prob_frank<- pCopula(prob_intpolated_mag_time_dif, frank.cop)
      
      
      
      ## Empirical copula ___________________________________________________
      
      emp_cupula_manual<- matrix(nrow = length(na_rm_time_diff), ncol = 1) # Prob of non exceedences
      emp_cupula_manual_corecT<- matrix(nrow = length(na_rm_time_diff), ncol = 1)
      #prob_exceedence_emp<- matrix(nrow = length(na_rm_time_diff), ncol = 1)
      emp_frequ_manual<- matrix(nrow = length(na_rm_time_diff), ncol = 1)
      emp_frequ_manual_corecT<- matrix(nrow = length(na_rm_time_diff), ncol = 1)
      #length_inter<- matrix(nrow = length(na_rm_time_diff), ncol = 1)
      for (j in 1:length(na_rm_time_diff)){
        
        temp_timDif<- na_rm_time_diff[j]
        temp_magDif<- na_rm_mag_diff[j]
        
        ind_lesE_sub_tim_dif<- which(na_rm_time_diff <= temp_timDif)
        
        
        ind_lesE_sub_mag_dif<- which(na_rm_mag_diff <= temp_magDif)
        ind_gretE_sub_mag_dif<- which(na_rm_mag_diff >= temp_magDif)
        
        
        intersec_ind<- intersect(ind_lesE_sub_mag_dif, ind_lesE_sub_tim_dif)
        intersec_ind_gretE<- intersect(ind_gretE_sub_mag_dif, ind_lesE_sub_tim_dif)
        
        total_ind_length<- length(intersec_ind)
        total_ind_length_gretE<- length(intersec_ind_gretE)
        #length_inter[i,1]<- total_ind_length
        
        # Gringorten plotting position ________________________________________
        #emp_cupula_manual[j,1]<- (total_ind_length - 0.44)/((length(na_rm_time_diff))+0.12)
        
        # Weibul plotting position ____________________________________________
        emp_cupula_manual[j,1]<- (total_ind_length)/((length(na_rm_time_diff)))
        emp_cupula_manual_corecT[j,1]<- (total_ind_length_gretE)/((length(na_rm_time_diff)))
        
        
        #Frequency ____________________________________________________________
        emp_frequ_manual[j,1]<- (total_ind_length)/((length(na_rm_time_diff)))
        emp_frequ_manual_corecT[j,1]<- (total_ind_length_gretE)/((length(na_rm_time_diff)))
        # prob of exceedences _________________________________________________
        #prob_exceedence_emp[i,1]<- 1 - ((total_ind_length - 0.44)/((length(na_rm_time_diff))+0.12))
      }
      
      
      
      
      
      ## Find the co-ordinates near to the design mag and time ______________________________
      ind_lesE_sub_tim_dif<- which(na_rm_time_diff < time_gap_RB)
      ind_lesE_sub_mag_dif<- which(na_rm_mag_diff < mag_gap_RB)
      
      ind_gret_sub_tim_dif<- which(na_rm_time_diff > time_gap_RB)
      ind_gret_sub_mag_dif<- which(na_rm_mag_diff > mag_gap_RB)
      
      ind_intsec_mag_and_time_less<- intersect(ind_lesE_sub_tim_dif,ind_lesE_sub_mag_dif) #Diagonal
      ind_intsec_mag_and_time_gret<- intersect(ind_gret_sub_tim_dif,ind_gret_sub_mag_dif) #Diagonal
      ind_intsec_mag_gret_and_time_les<- intersect(ind_lesE_sub_tim_dif,ind_gret_sub_mag_dif)
      ind_intsec_mag_les_and_time_gret<- intersect(ind_gret_sub_tim_dif,ind_lesE_sub_mag_dif)
      
      vec_length_ind<- c(length(ind_intsec_mag_and_time_less),
                         length(ind_intsec_mag_and_time_gret),
                         length(ind_intsec_mag_les_and_time_gret),
                         length(ind_intsec_mag_gret_and_time_les))
      
      
      ind_not_zero<- which(vec_length_ind != 0)
      
      index_points_neighbour<- matrix(nrow = 1, ncol = 1)
      for (n in 1:length(ind_not_zero)){
        
        temp_ind<- ind_not_zero[n]
        
        if (temp_ind == 1){
          abs_dif_mag<- (na_rm_mag_diff[ind_intsec_mag_and_time_less] - mag_gap_RB)^2
          abs_dif_time<- (na_rm_time_diff[ind_intsec_mag_and_time_less] - time_gap_RB)^2
          
          ecu_dist<- (abs_dif_mag + abs_dif_time)^(0.5)
          ind_min_euc<- which(ecu_dist == min(ecu_dist))
          
          ind_consid<- ind_intsec_mag_and_time_less[ind_min_euc]
          
        }else if (temp_ind == 2){
          abs_dif_mag<- (na_rm_mag_diff[ind_intsec_mag_and_time_gret] - mag_gap_RB)^2
          abs_dif_time<- (na_rm_time_diff[ind_intsec_mag_and_time_gret] - time_gap_RB)^2
          
          ecu_dist<- (abs_dif_mag + abs_dif_time)^(0.5)
          ind_min_euc<- which(ecu_dist == min(ecu_dist))
          
          ind_consid<- ind_intsec_mag_and_time_gret[ind_min_euc]
          
        }else if (temp_ind == 3){
          abs_dif_mag<- (na_rm_mag_diff[ind_intsec_mag_les_and_time_gret] - mag_gap_RB)^2
          abs_dif_time<- (na_rm_time_diff[ind_intsec_mag_les_and_time_gret] - time_gap_RB)^2
          
          ecu_dist<- (abs_dif_mag + abs_dif_time)^(0.5)
          ind_min_euc<- which(ecu_dist == min(ecu_dist))
          
          ind_consid<- ind_intsec_mag_les_and_time_gret[ind_min_euc]
          
        }else if (temp_ind == 4){
          abs_dif_mag<- (na_rm_mag_diff[ind_intsec_mag_gret_and_time_les] - mag_gap_RB)^2
          abs_dif_time<- (na_rm_time_diff[ind_intsec_mag_gret_and_time_les] - time_gap_RB)^2
          
          ecu_dist<- (abs_dif_mag + abs_dif_time)^(0.5)
          ind_min_euc<- which(ecu_dist == min(ecu_dist))
          
          ind_consid<- ind_intsec_mag_gret_and_time_les[ind_min_euc]
          
        }
        
        index_points_neighbour<- rbind(index_points_neighbour,ind_consid)
        
      }
      
      index_points_neighbour<- matrix(index_points_neighbour[-1,], ncol = 1)
      
      
      # For inverse distance weighted __________________
      distance_of_points<- matrix(nrow = nrow(index_points_neighbour), ncol = 1)
      prob_of_points<- matrix(nrow = nrow(index_points_neighbour), ncol = 1)
      for (k in 1:nrow(index_points_neighbour)){
        
        abs_dif_mag<- (na_rm_mag_diff[index_points_neighbour[k]] - mag_gap_RB)^2
        abs_dif_time<- (na_rm_time_diff[index_points_neighbour[k]] - time_gap_RB)^2
        ecu_dist<- (abs_dif_mag + abs_dif_time)^(0.5)
        
        distance_of_points[k]<- ecu_dist
        
        #prob_of_points[k]<- emp_cupula_manual[index_points_neighbour[k],1]
        prob_of_points[k]<- emp_cupula_manual_corecT[index_points_neighbour[k],1]
      }
      
      
      
      
      
      ## Decide the weights _____________________________________
      
      if (nrow(distance_of_points) == 4){
        
        inv_dist<- 1/(distance_of_points[,1])
        weights_pnt<- inv_dist/(sum(inv_dist))
        
        prob_emp_est<- sum(weights_pnt*prob_of_points[,1])
        
      }else if (nrow(distance_of_points) == 3){
        
        inv_dist<- 1/(distance_of_points[,1])
        weights_pnt<- inv_dist/(sum(inv_dist))
        prob_emp_est<- sum(weights_pnt*prob_of_points[,1])
      }else if (nrow(distance_of_points) == 2){
        
        inv_dist<- 1/(distance_of_points[,1])
        weights_pnt<- inv_dist/(sum(inv_dist))
        prob_emp_est<- sum(weights_pnt*prob_of_points[,1])
      }else if (nrow(distance_of_points) == 1){
        
        
        
        
        
        if (vec_length_ind[1] != 0){
          mag_lim_1<- 0
          time_lim_1<- 0
        }else if (vec_length_ind[2] != 0){
          mag_lim_1<- max(sort(na_rm_mag_diff)) + 1
          time_lim_1<- max(sort(na_rm_time_diff)) + 1
        }
        
        
        abs_dif_mag_ext<- (mag_lim_1 - mag_gap_RB)^2
        abs_dif_time_ext<- (time_lim_1 - time_gap_RB)^2
        ecu_dist_ext<- (abs_dif_mag_ext + abs_dif_time_ext)^(0.5)
        
        #distance_of_points_new<- distance_of_points
        #prob_of_points_new<- prob_of_points
        
        distance_of_points_new<- rbind(distance_of_points,matrix(c(ecu_dist_ext), nrow = 1))
        prob_of_points_new<- rbind(prob_of_points,matrix(c(0.999), nrow = 1))
        
        
        
        
        inv_dist<- 1/(distance_of_points_new[,1])
        weights_pnt<- inv_dist/(sum(inv_dist))
        prob_emp_est<- sum(weights_pnt*prob_of_points_new[,1])
        
      }
      
      
      probability_for_mag_and_time_empirical[1,i]<- prob_emp_est
      probability_for_mag_and_time_normal[1,i]<- prob_normal
      
      probability_for_mag_and_time_clayton[1,i]<- prob_clayton
      probability_for_mag_and_time_frank[1,i]<- prob_frank
      
      
      
      
      ### Distributions   _________________________________________________________________________________________
      mag_lmoments<- lmoms(na_rm_mag_diff)
      time_lmoments<- lmoms(na_rm_time_diff)
      
      para_mag_gpa<- pargpa(mag_lmoments,checklmom = T)
      para_time_gpa<- pargpa(time_lmoments,checklmom = T)
      cdf_mag_gpa<- cdfgpa(mag_gap_RB, para_mag_gpa)
      cdf_time_gpa<- cdfgpa(time_gap_RB, para_mag_gpa)
      
      para_mag_gev<- pargev(mag_lmoments,checklmom = T)
      para_time_gev<- pargev(time_lmoments,checklmom = T)
      cdf_mag_gev<- cdfgev(mag_gap_RB, para_mag_gev)
      cdf_time_gev<- cdfgev(time_gap_RB, para_mag_gev)
      
      para_mag_pe3<- parpe3(mag_lmoments,checklmom = T)
      para_time_pe3<- parpe3(time_lmoments,checklmom = T)
      cdf_mag_pe3<- cdfpe3(mag_gap_RB, para_mag_pe3)
      cdf_time_pe3<- cdfpe3(time_gap_RB, para_mag_pe3)
      
      
      #prob_intpolated_mag_time_dif<- matrix(c(1 - prob_mag_RB,prob_time_RB), ncol = 2)
      # Estimate probs  _________________________________________
      #prob_normal<- pCopula(prob_intpolated_mag_time_dif, normal.cop)
      probability_for_magGPA_and_timeGPA_normal[1,i]<- pCopula(matrix(c(1 - cdf_mag_gpa,cdf_time_gpa), ncol = 2), normal.cop)
      probability_for_magGEV_and_timeGEV_normal[1,i]<- pCopula(matrix(c(1 - cdf_mag_gev,cdf_time_gev), ncol = 2), normal.cop)
      probability_for_magPE3_and_timePE3_normal[1,i]<- pCopula(matrix(c(1 - cdf_mag_pe3,cdf_time_pe3), ncol = 2), normal.cop)
        
      probability_for_magGPA_and_timeGEV_normal[1,i]<- pCopula(matrix(c(1 - cdf_mag_gpa,cdf_time_gev), ncol = 2), normal.cop)
      probability_for_magGPA_and_timePE3_normal[1,i]<- pCopula(matrix(c(1 - cdf_mag_gpa,cdf_time_pe3), ncol = 2), normal.cop)
      probability_for_magGEV_and_timeGPA_normal[1,i]<- pCopula(matrix(c(1 - cdf_mag_gev,cdf_time_gpa), ncol = 2), normal.cop)
      probability_for_magGEV_and_timePE3_normal[1,i]<- pCopula(matrix(c(1 - cdf_mag_gev,cdf_time_pe3), ncol = 2), normal.cop)
      probability_for_magPE3_and_timeGPA_normal[1,i]<- pCopula(matrix(c(1 - cdf_mag_pe3,cdf_time_gpa), ncol = 2), normal.cop)
      probability_for_magPE3_and_timeGEV_normal[1,i]<- pCopula(matrix(c(1 - cdf_mag_pe3,cdf_time_gev), ncol = 2), normal.cop)
      
      probability_for_magGEV_and_timeGPA_normal[1,i]<- pCopula(matrix(c(1 - cdf_mag_gev,cdf_time_gpa), ncol = 2), normal.cop)
      probability_for_magPE3_and_timeGPA_normal[1,i]<- pCopula(matrix(c(1 - cdf_mag_pe3,cdf_time_gpa), ncol = 2), normal.cop)
      probability_for_magGPA_and_timeGEV_normal[1,i]<- pCopula(matrix(c(1 - cdf_mag_gpa,cdf_time_gev), ncol = 2), normal.cop)
      probability_for_magPE3_and_timeGEV_normal[1,i]<- pCopula(matrix(c(1 - cdf_mag_pe3,cdf_time_gev), ncol = 2), normal.cop)
      probability_for_magGPA_and_timePE3_normal[1,i]<- pCopula(matrix(c(1 - cdf_mag_gpa,cdf_time_pe3), ncol = 2), normal.cop)
      probability_for_magGEV_and_timePE3_normal[1,i]<- pCopula(matrix(c(1 - cdf_mag_gev,cdf_time_pe3), ncol = 2), normal.cop)
      
      
      
      
}
    
    
    cat(i)
    
  }
  
  
  
  ind_with_data<- which(is.na(probability_for_mag_and_time_empirical[1,]) == F)
  
  emp_prob_mag_time_diff<- probability_for_mag_and_time_empirical[,ind_with_data]
  est_normal_prob_mag_time_diff<- probability_for_mag_and_time_normal[,ind_with_data]
  
  est_clayton_prob_mag_time_diff<- probability_for_mag_and_time_clayton[,ind_with_data]
  est_frank_prob_mag_time_diff<- probability_for_mag_and_time_frank[,ind_with_data]
  
  
  
  
  
  xlim_l<- 0
  ylim_l<- 0
  
  
  xlim_u<- 1
  ylim_u<- 1
  
  
  #file_dir_cop<- "D:/Research_IIT_Roorkee/Record_breaking_events/plots_daysgap/discus_Prof_G/max_2_day/no_dayGAP/copula/"
  file_dir_cop<- "D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/copula_correct/"
  
  
  png(paste(file_dir_cop,"normal","_mag_",mag_gap_RB,"mm_and_time_",time_gap_RB,"_yr",".png",sep = ))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_mag_time_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_mag_time_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_clayton_prob_mag_time_diff,main =paste("Clayton Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_clayton_prob_mag_time_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_frank_prob_mag_time_diff,main =paste("Frank Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_frank_prob_mag_time_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  
  
  # Box plot of bias __________________________________________________________________________
  spat_plt_exc_emp_prob_mag_time_diff<- probability_for_mag_and_time_empirical
  spat_plt_exc_est_normal_prob_mag_time_diff<- probability_for_mag_and_time_normal
  
  #est_clayton_prob_mag_time_diff<- probability_for_mag_and_time_clayton[,ind_with_data]
  #est_frank_prob_mag_time_diff<- probability_for_mag_and_time_frank[,ind_with_data]
  
  per_absolute_bias_exc_prob_joint<- (abs((spat_plt_exc_est_normal_prob_mag_time_diff - spat_plt_exc_emp_prob_mag_time_diff)))#/(spat_plt_exc_emp_prob_mag_time_diff))*100 
  per_bias_exc_prob_joint<- (((spat_plt_exc_est_normal_prob_mag_time_diff - spat_plt_exc_emp_prob_mag_time_diff)))#/(spat_plt_exc_emp_prob_mag_time_diff))*100 
  
  
  per_abs_bias_dataEXCProb<- per_absolute_bias_exc_prob_joint[,ind_with_data]
  per_bias_dataEXCProb<- per_bias_exc_prob_joint[,ind_with_data]
  
  computed_normal_prob<- spat_plt_exc_est_normal_prob_mag_time_diff[,ind_with_data]
  png(paste(file_dir_cop,"probability_","normal","_mag_",mag_gap_RB,"mm_and_time_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,4.5,4,2) + 0.4)
  #boxplot(per_abs_bias_dataEXCProb, names = c(""),xlab="",ylab="Percentage A-Bias (%)",main=paste("Percentage A-Bias of Joint Prob",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  boxplot(computed_normal_prob, names = c(""),xlab="",ylab="Probability",main=paste("Probability",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  
  #boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
  box(lwd=2)
  abline(h=0.4235969, lwd=2, col="red")# at 50% of quantile
  dev.off()
  
  quantile(computed_normal_prob[!is.na(computed_normal_prob)],c(0.05,0.25,0.5,0.75,0.95))
  
  
  
  
  
  # Box plot of absolute bias __________for exc joint prob
  png(paste(file_dir_cop,"JointProb_ABias_","normal","_mag_",mag_gap_RB,"mm_and_time_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,4.5,4,2) + 0.4)
  #boxplot(per_abs_bias_dataEXCProb, names = c(""),xlab="",ylab="Percentage A-Bias (%)",main=paste("Percentage A-Bias of Joint Prob",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  boxplot(per_abs_bias_dataEXCProb, names = c(""),xlab="",ylab="A-Bias of probability",main=paste("A-Bias of the probability",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  
  #boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
  box(lwd=2)
  dev.off()
  
  quantile(per_abs_bias_dataEXCProb[!is.na(per_abs_bias_dataEXCProb)],c(0.05,0.25,0.5,0.75,0.95))
  
  
  
  
  
  
  
  
  ## Plot for the distribution______________________________________________________________
  est_normal_prob_magGPA_timeGPA_diff<- probability_for_magGPA_and_timeGPA_normal[,ind_with_data]
  est_normal_prob_magGEV_timeGEV_diff<- probability_for_magGEV_and_timeGEV_normal[,ind_with_data]
  est_normal_prob_magPE3_timePE3_diff<- probability_for_magPE3_and_timePE3_normal[,ind_with_data]
  
  png(paste(file_dir_cop,"distribution/normal","_magGPA_",mag_gap_RB,"mm_and_timeGPA_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGPA_timeGPA_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGPA_timeGPA_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  png(paste(file_dir_cop,"distribution/normal","_magGEV_",mag_gap_RB,"mm_and_timeGEV_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGEV_timeGEV_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGEV_timeGEV_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  png(paste(file_dir_cop,"distribution/normal","_magPE3_",mag_gap_RB,"mm_and_timePE3_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magPE3_timePE3_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magPE3_timePE3_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  
  est_normal_prob_magGPA_timeGEV_diff<- probability_for_magGPA_and_timeGEV_normal[,ind_with_data]
  est_normal_prob_magGPA_timePE3_diff<- probability_for_magGPA_and_timePE3_normal[,ind_with_data]
  est_normal_prob_magGEV_timeGPA_diff<- probability_for_magGEV_and_timeGPA_normal[,ind_with_data]
  est_normal_prob_magGEV_timePE3_diff<- probability_for_magGEV_and_timePE3_normal[,ind_with_data]
  est_normal_prob_magPE3_timeGPA_diff<- probability_for_magPE3_and_timeGPA_normal[,ind_with_data]
  est_normal_prob_magPE3_timeGEV_diff<- probability_for_magPE3_and_timeGEV_normal[,ind_with_data]
  
  png(paste(file_dir_cop,"distribution/normal","_magGPA_",mag_gap_RB,"mm_and_timeGEV_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGPA_timeGEV_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGPA_timeGEV_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  
  png(paste(file_dir_cop,"distribution/normal","_magGPA_",mag_gap_RB,"mm_and_timePE3_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGPA_timePE3_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGPA_timePE3_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  png(paste(file_dir_cop,"distribution/normal","_magGEV_",mag_gap_RB,"mm_and_timeGPA_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGEV_timeGPA_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGEV_timeGPA_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  png(paste(file_dir_cop,"distribution/normal","_magGEV_",mag_gap_RB,"mm_and_timePE3_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGEV_timePE3_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGEV_timePE3_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  
  png(paste(file_dir_cop,"distribution/normal","_magPE3_",mag_gap_RB,"mm_and_timeGPA_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magPE3_timeGPA_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magPE3_timeGPA_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  
  png(paste(file_dir_cop,"distribution/normal","_magPE3_",mag_gap_RB,"mm_and_timeGEV_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magPE3_timeGEV_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magPE3_timeGEV_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  
  ##
  est_normal_prob_magGEV_timeGPA_diff<- probability_for_magGEV_and_timeGPA_normal[,ind_with_data]
  est_normal_prob_magPE3_timeGPA_diff<- probability_for_magPE3_and_timeGPA_normal[,ind_with_data]
  est_normal_prob_magGPA_timeGEV_diff<- probability_for_magGPA_and_timeGEV_normal[,ind_with_data]
  est_normal_prob_magPE3_timeGEV_diff<- probability_for_magPE3_and_timeGEV_normal[,ind_with_data]
  est_normal_prob_magGPA_timePE3_diff<- probability_for_magGPA_and_timePE3_normal[,ind_with_data]
  est_normal_prob_magGEV_timePE3_diff<- probability_for_magGEV_and_timePE3_normal[,ind_with_data]
  
  
  png(paste(file_dir_cop,"distribution/normal","_magGEV_",mag_gap_RB,"mm_and_timeGPA_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGEV_timeGPA_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGEV_timeGPA_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  
  png(paste(file_dir_cop,"distribution/normal","_magPE3_",mag_gap_RB,"mm_and_timeGPA_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magPE3_timeGPA_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magPE3_timeGPA_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  png(paste(file_dir_cop,"distribution/normal","_magGPA_",mag_gap_RB,"mm_and_timeGEV_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGPA_timeGEV_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGPA_timeGEV_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  png(paste(file_dir_cop,"distribution/normal","_magPE3_",mag_gap_RB,"mm_and_timeGEV_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magPE3_timeGEV_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magPE3_timeGEV_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  
  png(paste(file_dir_cop,"distribution/normal","_magGPA_",mag_gap_RB,"mm_and_timePE3_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGPA_timePE3_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGPA_timePE3_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  
  png(paste(file_dir_cop,"distribution/normal","_magGEV_",mag_gap_RB,"mm_and_timePE3_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGEV_timePE3_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGEV_timePE3_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  
  #probability_for_magGPA_and_timeGPA_normal<- pCopula(matrix(c(1 - cdf_mag_gpa,cdf_time_gpa), ncol = 2), normal.cop)
  #probability_for_magGEV_and_timeGEV_normal<- pCopula(matrix(c(1 - cdf_mag_gev,cdf_time_gev), ncol = 2), normal.cop)
  #probability_for_magPE3_and_timePE3_normal<- pCopula(matrix(c(1 - cdf_mag_pe3,cdf_time_pe3), ncol = 2), normal.cop)
  
  #probability_for_magGPA_and_timeGEV_normal<- pCopula(matrix(c(1 - cdf_mag_gpa,cdf_time_gev), ncol = 2), normal.cop)
  #probability_for_magGPA_and_timePE3_normal<- pCopula(matrix(c(1 - cdf_mag_gpa,cdf_time_pe3), ncol = 2), normal.cop)
  #probability_for_magGEV_and_timeGPA_normal<- pCopula(matrix(c(1 - cdf_mag_gev,cdf_time_gpa), ncol = 2), normal.cop)
  #probability_for_magGEV_and_timePE3_normal<- pCopula(matrix(c(1 - cdf_mag_gev,cdf_time_pe3), ncol = 2), normal.cop)
  #probability_for_magPE3_and_timeGPA_normal<- pCopula(matrix(c(1 - cdf_mag_pe3,cdf_time_gpa), ncol = 2), normal.cop)
  #probability_for_magPE3_and_timeGEV_normal<- pCopula(matrix(c(1 - cdf_mag_pe3,cdf_time_gev), ncol = 2), normal.cop)
  
  #probability_for_magGEV_and_timeGPA_normal<- pCopula(matrix(c(1 - cdf_mag_gev,cdf_time_gpa), ncol = 2), normal.cop)
  #probability_for_magPE3_and_timeGPA_normal<- pCopula(matrix(c(1 - cdf_mag_pe3,cdf_time_gpa), ncol = 2), normal.cop)
  #probability_for_magGPA_and_timeGEV_normal<- pCopula(matrix(c(1 - cdf_mag_gpa,cdf_time_gev), ncol = 2), normal.cop)
  #probability_for_magPE3_and_timeGEV_normal<- pCopula(matrix(c(1 - cdf_mag_pe3,cdf_time_gev), ncol = 2), normal.cop)
  #probability_for_magGPA_and_timePE3_normal<- pCopula(matrix(c(1 - cdf_mag_gpa,cdf_time_pe3), ncol = 2), normal.cop)
  #probability_for_magGEV_and_timePE3_normal<- pCopula(matrix(c(1 - cdf_mag_gev,cdf_time_pe3), ncol = 2), normal.cop)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## Probability of excedence ____________________________________
  excd_emp_prob_mag_time_diff<- 1 - emp_prob_mag_time_diff
  excd_est_normal_prob_mag_time_diff<- 1 - est_normal_prob_mag_time_diff
  
  excd_est_clayton_prob_mag_time_diff<- 1 - est_clayton_prob_mag_time_diff
  excd_est_frank_prob_mag_time_diff<- 1 - est_frank_prob_mag_time_diff
  
  
  png(paste(file_dir_cop,"prob_excedance/","normal","_mag_",mag_gap_RB,"mm_and_time_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(excd_emp_prob_mag_time_diff, excd_est_normal_prob_mag_time_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(excd_est_normal_prob_mag_time_diff ~ excd_emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(excd_emp_prob_mag_time_diff, excd_est_clayton_prob_mag_time_diff,main =paste("Clayton Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(excd_est_clayton_prob_mag_time_diff ~ excd_emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(excd_emp_prob_mag_time_diff, excd_est_frank_prob_mag_time_diff,main =paste("Frank Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(excd_est_frank_prob_mag_time_diff ~ excd_emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  
  
  
  #### Box_plot of Absolute bias
  spat_plt_exc_emp_prob_mag_time_diff<- 1 - probability_for_mag_and_time_empirical
  spat_plt_exc_est_normal_prob_mag_time_diff<- 1 - probability_for_mag_and_time_normal
  
  #est_clayton_prob_mag_time_diff<- probability_for_mag_and_time_clayton[,ind_with_data]
  #est_frank_prob_mag_time_diff<- probability_for_mag_and_time_frank[,ind_with_data]
  
  per_absolute_bias_exc_prob_joint<- (abs((spat_plt_exc_est_normal_prob_mag_time_diff - spat_plt_exc_emp_prob_mag_time_diff))/(spat_plt_exc_emp_prob_mag_time_diff))*100 
  per_bias_exc_prob_joint<- (((spat_plt_exc_est_normal_prob_mag_time_diff - spat_plt_exc_emp_prob_mag_time_diff))/(spat_plt_exc_emp_prob_mag_time_diff))*100 
  
  
  per_abs_bias_dataEXCProb<- per_absolute_bias_exc_prob_joint[,ind_with_data]
  per_bias_dataEXCProb<- per_bias_exc_prob_joint[,ind_with_data]
  
  # Box plot of absolute bias __________for exc joint prob
  png(paste(file_dir_cop,"prob_excedance/","excJointProb_ABiasPerc_","normal","_mag_",mag_gap_RB,"mm_and_time_",time_gap_RB,"_yr.png",sep = ""))
  op <- par(mar = c(7,4.5,4,2) + 0.4)
  boxplot(per_abs_bias_dataEXCProb, names = c(""),xlab="",ylab="Percentage A-Bias (%)",main=paste("Percentage A-Bias of Joint Prob. Exc ",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,100),las=2)
  #boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
  box(lwd=2)
  dev.off()
  
  
  
  
}


# For a-bias_run the itteration of m individually and plot abias IMPORTANT

## Spatial plot of excdeence_______________________________________________________________________
##_________________________________________________________________________________________________
lat_long_site<- number_rec_lat_long[,1:2]


spat_plt_exc_emp_prob_mag_time_diff<- 1 - probability_for_mag_and_time_empirical
spat_plt_exc_est_normal_prob_mag_time_diff<- 1 - probability_for_mag_and_time_normal

#est_clayton_prob_mag_time_diff<- probability_for_mag_and_time_clayton[,ind_with_data]
#est_frank_prob_mag_time_diff<- probability_for_mag_and_time_frank[,ind_with_data]

per_absolute_bias_exc_prob_joint<- (abs((spat_plt_exc_est_normal_prob_mag_time_diff - spat_plt_exc_emp_prob_mag_time_diff))/(spat_plt_exc_emp_prob_mag_time_diff))*100 
per_bias_exc_prob_joint<- (((spat_plt_exc_est_normal_prob_mag_time_diff - spat_plt_exc_emp_prob_mag_time_diff))/(spat_plt_exc_emp_prob_mag_time_diff))*100 


per_abs_bias_dataEXCProb<- per_absolute_bias_exc_prob_joint[,ind_with_data]
per_bias_dataEXCProb<- per_bias_exc_prob_joint[,ind_with_data]

# Box plot of absolute bias __________for exc joint prob
#png(paste(file_dir_cop,"prob_excedance/","excJointProb_boxplot_ABiasPercentage.png",sep = ""))
#op <- par(mar = c(7,4.5,4,2) + 0.4)
#boxplot(per_abs_bias_dataEXCProb, names = c(""),xlab="",ylab="Percentage A-Bias (%)",main=paste("Percentage A-Bias of Joint Prob. Exc ",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,40),las=2)
#boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = "")) #Not this to be activated
#box(lwd=2)
#dev.off()





##___________________________________
##Spatial Plot ____________ Exc. Prob.
#

#png(filename="D:/Research_IIT_Roorkee/Record_breaking_events/plots_daysgap/discus_Prof_G/max_2_day/no_dayGAP/copula/prob_excedance/exc_JointProb_spatial_plot.png")
png(filename="D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/copula_correct/prob_excedance/exc_JointProb_spatial_plot.png")

map(india_watershed_di,xlim=c(67.5,98), ylim=c(6,38),lwd=2)
title(main="Prob Exc. Diff in Mag and Time)")



map.axes(cex.axis=1.2,lwd=3,at=c(long_at,lat_at),
         labels=c('70°E','75°E','80°E','85°E','90°E','95°E','10°N','15°N','20°N','25°N',"30°N",'35°N'))


north(xy=cbind(95,34),type=2)


#These are not cor range these are prob exc range _____ Just replacing in cor code
ranges_of_corr<- c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)

records_length_vec<- ranges_of_corr

values_range_rec<- matrix(nrow = (length(records_length_vec) - 1) ,ncol = 2)
for (i in 1:(length(records_length_vec) - 1)){
  
  values_range_rec[i,1]<- records_length_vec[i]+0.01
  values_range_rec[i,2]<- records_length_vec[i+1]
  
}


#names_colours_rl<- rev(brewer.pal(n = length(records_length_vec)-1, name = "Spectral"))
names_colours_rl<- brewer.pal(n = length(records_length_vec)-1, name = "YlOrRd")

#names_colours_rl<- brewer.pal(n = length(records_length_vec), name = "Spectral")
ranges_values<- data.frame(values_range_rec)

ranges_values_char<- ranges_values
ranges_values_char[,1]<- as.character(ranges_values[,1])
ranges_values_char[,2]<- as.character(ranges_values[,2])



for (i in 1:nrow(lat_long_site)){
  
  temp_lat<- lat_long_site[i,2]
  temp_long<- lat_long_site[i,1]
  
  temp_cor<- spat_plt_exc_emp_prob_mag_time_diff[1,i]
  #temp_cor<- spat_plt_exc_est_normal_prob_mag_time_diff[1,i] # For prob exc estimated by normal(Gaussian) copula
  
  
  if (is.na(temp_cor) == T){
    col_pnt<- "black"
  }else if (temp_cor >= values_range_rec[1,1] & temp_cor <= values_range_rec[1,2]){
    col_pnt<- names_colours_rl[1]
  }else if (temp_cor >= values_range_rec[2,1] & temp_cor <= values_range_rec[2,2]){
    col_pnt<- names_colours_rl[2]
  }else if (temp_cor >= values_range_rec[3,1] & temp_cor <= values_range_rec[3,2]){
    col_pnt<- names_colours_rl[3]
  }else if (temp_cor >= values_range_rec[4,1] & temp_cor <= values_range_rec[4,2]){
    col_pnt<- names_colours_rl[4]
  }else if (temp_cor >= values_range_rec[5,1] & temp_cor <= values_range_rec[5,2]){
    col_pnt<- names_colours_rl[5]
  }else if (temp_cor >= values_range_rec[6,1] & temp_cor <= values_range_rec[6,2]){
    col_pnt<- names_colours_rl[6]
  }else if (temp_cor >= values_range_rec[7,1] & temp_cor <= values_range_rec[7,2]){
    col_pnt<- names_colours_rl[7]
  }
  
  points(temp_long,temp_lat,col = col_pnt, cex = 1.5,pch=16)
  
  
  print(paste(i," Done.....","To be done till.....      ",nrow(lat_long_site),sep = ""))
}

dif_lat_ind<- 1
lat_st_ind<- 16.5

for (i in 1:length(names_colours_rl)){
  lat_pnt<- lat_st_ind - (dif_lat_ind*i)
  points(85,lat_pnt,col = names_colours_rl[i], cex = 1,pch=15)
  #text(87.5, lat_pnt, as.character(samp_rl[i]),cex=0.7)
  #if (i == 1){
  #first_rng<- as.character(round(values_range_rec[i,1],0))
  #}else{
  #first_rng<- as.character(round((values_range_rec[i,1]),0))
  #}
  
  #last_rng<- as.character(round(values_range_rec[i,2],0))
  
  first_rng<- as.character(values_range_rec[i,1])
  last_rng<- as.character(values_range_rec[i,2])
  
  text(88.5, (lat_pnt+0.2), paste("  ",first_rng,"   :   ",last_rng,sep=""),cex=0.8)
}
points(85,lat_pnt-dif_lat_ind,col = "black", cex = 1.5,pch=15)
text(87.5, (lat_pnt-dif_lat_ind-0.02), paste("    < 10 yr RL",sep=""),cex=0.8)
dev.off()















##Spatial Plot ____________ Exc. Prob.
# Greater or less than certain probability

#
threshold_prob<- 0.7


#png(filename=paste("D:/Research_IIT_Roorkee/Record_breaking_events/plots_daysgap/discus_Prof_G/max_2_day/no_dayGAP/copula/prob_excedance/exc_JointProb_spatial_plot_gretLess_","threshold",".png",sep = ""))
png(filename=paste("D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/copula_correct/prob_excedance/exc_JointProb_spatial_plot_gretLess_","threshold",".png",sep = ""))

map(india_watershed_di,xlim=c(67.5,98), ylim=c(6,38),lwd=2)
title(main=paste("Prob Exc. Diff Mag and Time (GretLess_prob_",threshold_prob,")",sep = ""))



map.axes(cex.axis=1.2,lwd=3,at=c(long_at,lat_at),
         labels=c('70°E','75°E','80°E','85°E','90°E','95°E','10°N','15°N','20°N','25°N',"30°N",'35°N'))


north(xy=cbind(95,34),type=2)


# #These are not cor range these are prob exc range _____ Just replacing in cor code
# ranges_of_corr<- c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
# 
# records_length_vec<- ranges_of_corr
# 
# values_range_rec<- matrix(nrow = (length(records_length_vec) - 1) ,ncol = 2)
# for (i in 1:(length(records_length_vec) - 1)){
#   
#   values_range_rec[i,1]<- records_length_vec[i]+0.01
#   values_range_rec[i,2]<- records_length_vec[i+1]
#   
# }
# 
# 
# #names_colours_rl<- rev(brewer.pal(n = length(records_length_vec)-1, name = "Spectral"))
# names_colours_rl<- brewer.pal(n = length(records_length_vec)-1, name = "YlOrRd")
# 
# #names_colours_rl<- brewer.pal(n = length(records_length_vec), name = "Spectral")
# ranges_values<- data.frame(values_range_rec)
# 
# ranges_values_char<- ranges_values
# ranges_values_char[,1]<- as.character(ranges_values[,1])
# ranges_values_char[,2]<- as.character(ranges_values[,2])



for (i in 1:nrow(lat_long_site)){
  
  temp_lat<- lat_long_site[i,2]
  temp_long<- lat_long_site[i,1]
  
  temp_cor<- spat_plt_exc_emp_prob_mag_time_diff[1,i]
  #temp_cor<- spat_plt_exc_est_normal_prob_mag_time_diff[1,i] # For prob exc estimated by normal(Gaussian) copula
  
  
  if (is.na(temp_cor) == T){
    col_pnt<- "black"
  }else if (temp_cor < threshold_prob){
    col_pnt<- "yellow"
  }else{
    col_pnt<- "red"
  }
  
  
  points(temp_long,temp_lat,col = col_pnt, cex = 1.5,pch=16)
  
  
  print(paste(i," Done.....","To be done till.....      ",nrow(lat_long_site),sep = ""))
}

dif_lat_ind<- 1
lat_st_ind<- 15.5

#for (i in 1:length(names_colours_rl)){
for (i in 1:2){
  lat_pnt<- lat_st_ind - (dif_lat_ind*i)
  
  if (i == 1){
    points(85,lat_pnt,col = "yellow", cex = 1,pch=15)
    first_rng<- as.character(values_range_rec[i,1])
    #last_rng<- as.character(values_range_rec[i,2])
    text(87.5, (lat_pnt+0.2), paste(" < ",threshold_prob,sep=""),cex=0.8)
  }else{
    points(85,lat_pnt,col = "red", cex = 1,pch=15)
    first_rng<- as.character(values_range_rec[i,1])
    #last_rng<- as.character(values_range_rec[i,2])
    text(87.5, (lat_pnt+0.2), paste(" >= ",threshold_prob,sep=""),cex=0.8)
  }
  #expression("T ">="5")
  
  
  
}
points(85,lat_pnt-dif_lat_ind,col = "black", cex = 1.5,pch=15)
text(87.5, (lat_pnt-dif_lat_ind-0.02), paste("    < 20 yr RL",sep=""),cex=0.8)
dev.off()








## For one site 3D plot of joint distribution ______________________________
## _________________________________________________________________________

## Joint distribution ________________________________________

ind_cor_gret_9_mor30yrData<- which(correlation_mag_diff_and_time_diff > 0.7 & number_of_records[,1] >= 10)
#correlation_mag_diff_and_time_diff[ind_cor_gret_9_mor30yrData[1]]
#number_of_records[ind_cor_gret_9_mor30yrData[1],2]

#i<- ind_max_cor
i<- ind_cor_gret_9_mor30yrData[38]# index =2799

temp_time_diff<- time_diff_record_breaking_event[,i]
temp_mag_diff<- mag_difference_record_breaking_event[,i]

na_rm_time_diff<- temp_time_diff[!is.na(temp_time_diff)]
na_rm_mag_diff<- temp_mag_diff[!is.na(temp_mag_diff)]

ori_prob_mag<- pobs(na_rm_mag_diff)
ori_prob_time<- pobs(na_rm_time_diff)

## Empirical copula ___________________________________________________

emp_cupula_manual<- matrix(nrow = length(na_rm_time_diff), ncol = 1) # Prob of non exceedences
emp_cupula_manual_corecT<- matrix(nrow = length(na_rm_time_diff), ncol = 1)
#prob_exceedence_emp<- matrix(nrow = length(na_rm_time_diff), ncol = 1)
emp_frequ_manual<- matrix(nrow = length(na_rm_time_diff), ncol = 1)
emp_frequ_manual_corecT<- matrix(nrow = length(na_rm_time_diff), ncol = 1)
#length_inter<- matrix(nrow = length(na_rm_time_diff), ncol = 1)
for (j in 1:length(na_rm_time_diff)){
  
  temp_timDif<- na_rm_time_diff[j]
  temp_magDif<- na_rm_mag_diff[j]
  
  ind_lesE_sub_tim_dif<- which(na_rm_time_diff <= temp_timDif)
  
  
  ind_lesE_sub_mag_dif<- which(na_rm_mag_diff <= temp_magDif)
  ind_gretE_sub_mag_dif<- which(na_rm_mag_diff >= temp_magDif)
  
  
  intersec_ind<- intersect(ind_lesE_sub_mag_dif, ind_lesE_sub_tim_dif)
  intersec_ind_gretE<- intersect(ind_gretE_sub_mag_dif, ind_lesE_sub_tim_dif)
  
  total_ind_length<- length(intersec_ind)
  total_ind_length_gretE<- length(intersec_ind_gretE)
  #length_inter[i,1]<- total_ind_length
  
  # Gringorten plotting position ________________________________________
  #emp_cupula_manual[j,1]<- (total_ind_length - 0.44)/((length(na_rm_time_diff))+0.12)
  
  # Weibul plotting position ____________________________________________
  emp_cupula_manual[j,1]<- (total_ind_length)/((length(na_rm_time_diff)))
  emp_cupula_manual_corecT[j,1]<- (total_ind_length_gretE)/((length(na_rm_time_diff)))
  
  
  #Frequency ____________________________________________________________
  emp_frequ_manual[j,1]<- (total_ind_length)/((length(na_rm_time_diff)))
  emp_frequ_manual_corecT[j,1]<- (total_ind_length_gretE)/((length(na_rm_time_diff)))
  # prob of exceedences _________________________________________________
  #prob_exceedence_emp[i,1]<- 1 - ((total_ind_length - 0.44)/((length(na_rm_time_diff))+0.12))
}






max_time_diff<- max(na_rm_time_diff)
max_mag_diff<- max(na_rm_mag_diff)

max_time_diff_round<- round(max(na_rm_time_diff),0)
max_mag_diff_round<- round(max(na_rm_mag_diff),0)


time_diff_scale<- round(((max_time_diff_round+11)/11),0)*(1:11)
mag_diff_scale<- round(((max_mag_diff_round+11)/11),0)*(1:11)

time_diff_scale<- c(1,time_diff_scale)
mag_diff_scale<- c(1,mag_diff_scale)

copula_prob_for_plotting<- matrix(nrow = length(time_diff_scale), ncol = length(mag_diff_scale))
#copula_prob_for_plotting_emp<- matrix(nrow = length(time_diff_scale), ncol = length(mag_diff_scale))
copula_prob_for_plotting_2d<- matrix(nrow = 1, ncol = 3)

copula_density_for_plotting<- matrix(nrow = length(time_diff_scale), ncol = length(mag_diff_scale))
for (t in 1:length(time_diff_scale)){
  
  for (j in 1:length(mag_diff_scale)){
    
    time_gap_RB<- time_diff_scale[t]
    mag_gap_RB<- mag_diff_scale[j]
    
    # Prob of 1 year _________________________
    sort_time_dif<- sort(na_rm_time_diff)
    sort_time_prob<- sort(ori_prob_time)
    ind_gr_1_tim_dif<- which(sort_time_dif > time_gap_RB)
    low_time_val<- sort_time_dif[(ind_gr_1_tim_dif[1]) - 1]
    upp_time_val<- sort_time_dif[ind_gr_1_tim_dif[1]]
    low_time_prob<- sort_time_prob[(ind_gr_1_tim_dif[1]) - 1]
    upp_time_prob<- sort_time_prob[ind_gr_1_tim_dif[1]]
    
    prob_time_RB<- (((time_gap_RB - low_time_val)/(upp_time_val - low_time_val))*(upp_time_prob - low_time_prob)) + low_time_prob
    
    # Prob of 3 mm _________________________
    sort_mag_dif<- sort(na_rm_mag_diff)
    sort_mag_prob<- sort(ori_prob_mag)
    ind_gr_1_mag_dif<- which(sort_mag_dif > mag_gap_RB)
    low_mag_val<- sort_mag_dif[(ind_gr_1_mag_dif[1]) - 1]
    upp_mag_val<- sort_mag_dif[ind_gr_1_mag_dif[1]]
    low_mag_prob<- sort_mag_prob[(ind_gr_1_mag_dif[1]) - 1]
    upp_mag_prob<- sort_mag_prob[ind_gr_1_mag_dif[1]]
    
    prob_mag_RB<- (((mag_gap_RB - low_mag_val)/(upp_mag_val - low_mag_val))*(upp_mag_prob - low_mag_prob)) + low_mag_prob
    
    
    
    prob_intpolated_mag_time_dif<- matrix(c(1 - prob_mag_RB,prob_time_RB), ncol = 2)
    
    # Estimate probs  _________________________________________
    prob_normal<- pCopula(prob_intpolated_mag_time_dif, normal.cop)
    densty_normal<- dCopula(prob_intpolated_mag_time_dif, normal.cop)
    
    
    
    
    copula_prob_for_plotting_2d<- rbind(copula_prob_for_plotting_2d,
                                        matrix(c(time_gap_RB,mag_gap_RB,prob_normal),nrow = 1))
    
    
    
    
    
    
    # ##_____________________________________________________________________
    # 
    # ## Find the co-ordinates near to the design mag and time ______________________________
    # ind_lesE_sub_tim_dif<- which(na_rm_time_diff < time_gap_RB)
    # ind_lesE_sub_mag_dif<- which(na_rm_mag_diff < mag_gap_RB)
    # 
    # ind_gret_sub_tim_dif<- which(na_rm_time_diff > time_gap_RB)
    # ind_gret_sub_mag_dif<- which(na_rm_mag_diff > mag_gap_RB)
    # 
    # ind_intsec_mag_and_time_less<- intersect(ind_lesE_sub_tim_dif,ind_lesE_sub_mag_dif) #Diagonal
    # ind_intsec_mag_and_time_gret<- intersect(ind_gret_sub_tim_dif,ind_gret_sub_mag_dif) #Diagonal
    # ind_intsec_mag_gret_and_time_les<- intersect(ind_lesE_sub_tim_dif,ind_gret_sub_mag_dif)
    # ind_intsec_mag_les_and_time_gret<- intersect(ind_gret_sub_tim_dif,ind_lesE_sub_mag_dif)
    # 
    # vec_length_ind<- c(length(ind_intsec_mag_and_time_less),
    #                    length(ind_intsec_mag_and_time_gret),
    #                    length(ind_intsec_mag_les_and_time_gret),
    #                    length(ind_intsec_mag_gret_and_time_les))
    # 
    # 
    # ind_not_zero<- which(vec_length_ind != 0)
    # 
    # index_points_neighbour<- matrix(nrow = 1, ncol = 1)
    # for (n in 1:length(ind_not_zero)){
    #   
    #   temp_ind<- ind_not_zero[n]
    #   
    #   if (temp_ind == 1){
    #     abs_dif_mag<- (na_rm_mag_diff[ind_intsec_mag_and_time_less] - mag_gap_RB)^2
    #     abs_dif_time<- (na_rm_time_diff[ind_intsec_mag_and_time_less] - time_gap_RB)^2
    #     
    #     ecu_dist<- (abs_dif_mag + abs_dif_time)^(0.5)
    #     ind_min_euc<- which(ecu_dist == min(ecu_dist))
    #     
    #     ind_consid<- ind_intsec_mag_and_time_less[ind_min_euc]
    #     
    #   }else if (temp_ind == 2){
    #     abs_dif_mag<- (na_rm_mag_diff[ind_intsec_mag_and_time_gret] - mag_gap_RB)^2
    #     abs_dif_time<- (na_rm_time_diff[ind_intsec_mag_and_time_gret] - time_gap_RB)^2
    #     
    #     ecu_dist<- (abs_dif_mag + abs_dif_time)^(0.5)
    #     ind_min_euc<- which(ecu_dist == min(ecu_dist))
    #     
    #     ind_consid<- ind_intsec_mag_and_time_gret[ind_min_euc]
    #     
    #   }else if (temp_ind == 3){
    #     abs_dif_mag<- (na_rm_mag_diff[ind_intsec_mag_les_and_time_gret] - mag_gap_RB)^2
    #     abs_dif_time<- (na_rm_time_diff[ind_intsec_mag_les_and_time_gret] - time_gap_RB)^2
    #     
    #     ecu_dist<- (abs_dif_mag + abs_dif_time)^(0.5)
    #     ind_min_euc<- which(ecu_dist == min(ecu_dist))
    #     
    #     ind_consid<- ind_intsec_mag_les_and_time_gret[ind_min_euc]
    #     
    #   }else if (temp_ind == 4){
    #     abs_dif_mag<- (na_rm_mag_diff[ind_intsec_mag_gret_and_time_les] - mag_gap_RB)^2
    #     abs_dif_time<- (na_rm_time_diff[ind_intsec_mag_gret_and_time_les] - time_gap_RB)^2
    #     
    #     ecu_dist<- (abs_dif_mag + abs_dif_time)^(0.5)
    #     ind_min_euc<- which(ecu_dist == min(ecu_dist))
    #     
    #     ind_consid<- ind_intsec_mag_gret_and_time_les[ind_min_euc]
    #     
    #   }
    #   
    #   index_points_neighbour<- rbind(index_points_neighbour,ind_consid)
    #   
    # }
    # 
    # index_points_neighbour<- matrix(index_points_neighbour[-1,], ncol = 1)
    # 
    # 
    # # For inverse distance weighted __________________
    # distance_of_points<- matrix(nrow = nrow(index_points_neighbour), ncol = 1)
    # prob_of_points<- matrix(nrow = nrow(index_points_neighbour), ncol = 1)
    # for (k in 1:nrow(index_points_neighbour)){
    #   
    #   abs_dif_mag<- (na_rm_mag_diff[index_points_neighbour[k]] - mag_gap_RB)^2
    #   abs_dif_time<- (na_rm_time_diff[index_points_neighbour[k]] - time_gap_RB)^2
    #   ecu_dist<- (abs_dif_mag + abs_dif_time)^(0.5)
    #   
    #   distance_of_points[k]<- ecu_dist
    #   
    #   prob_of_points[k]<- emp_cupula_manual[index_points_neighbour[k],1]
    # }
    # 
    # 
    # 
    # 
    # 
    # ## Decide the weights _____________________________________
    # 
    # if (nrow(distance_of_points) == 4){
    #   
    #   inv_dist<- 1/(distance_of_points[,1])
    #   weights_pnt<- inv_dist/(sum(inv_dist))
    #   
    #   prob_emp_est<- sum(weights_pnt*prob_of_points[,1])
    #   
    # }else if (nrow(distance_of_points) == 3){
    #   
    #   inv_dist<- 1/(distance_of_points[,1])
    #   weights_pnt<- inv_dist/(sum(inv_dist))
    #   prob_emp_est<- sum(weights_pnt*prob_of_points[,1])
    # }else if (nrow(distance_of_points) == 2){
    #   
    #   inv_dist<- 1/(distance_of_points[,1])
    #   weights_pnt<- inv_dist/(sum(inv_dist))
    #   prob_emp_est<- sum(weights_pnt*prob_of_points[,1])
    # }else if (nrow(distance_of_points) == 1){
    #   
    #   
    #   
    #   
    #   
    #   if (vec_length_ind[1] != 0){
    #     mag_lim_1<- 0
    #     time_lim_1<- 0
    #   }else if (vec_length_ind[2] != 0){
    #     mag_lim_1<- max(sort(na_rm_mag_diff)) + 1
    #     time_lim_1<- max(sort(na_rm_time_diff)) + 1
    #   }
    #   
    #   
    #   abs_dif_mag_ext<- (mag_lim_1 - mag_gap_RB)^2
    #   abs_dif_time_ext<- (time_lim_1 - time_gap_RB)^2
    #   ecu_dist_ext<- (abs_dif_mag_ext + abs_dif_time_ext)^(0.5)
    #   
    #   #distance_of_points_new<- distance_of_points
    #   #prob_of_points_new<- prob_of_points
    #   
    #   distance_of_points_new<- rbind(distance_of_points,matrix(c(ecu_dist_ext), nrow = 1))
    #   prob_of_points_new<- rbind(prob_of_points,matrix(c(0.999), nrow = 1))
    #   
    #   
    #   
    #   
    #   inv_dist<- 1/(distance_of_points_new[,1])
    #   weights_pnt<- inv_dist/(sum(inv_dist))
    #   prob_emp_est<- sum(weights_pnt*prob_of_points_new[,1])
    #   
    # }
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # copula_prob_for_plotting_emp[t,j]<- prob_emp_est
    
    copula_prob_for_plotting[t,j]<- prob_normal
    copula_density_for_plotting[t,j]<- densty_normal
    
    print(paste("t= ",t," Time -- To be till ",length(time_diff_scale),"  AND     ","j= ",j," Mag -- To be till ",length(mag_diff_scale),sep = ""))
    
  }
  
  
  
  
}






## 3D plotting for the density and probability __________________________
library(plotly)

x = c(mag_diff_scale) # Magnitude as colms
y = c(time_diff_scale) #Time as rows
z = copula_prob_for_plotting
#z = copula_prob_for_plotting_emp

fig <- plot_ly(
  type = 'surface',
  contours = list(
    #x = list(show = TRUE, start = 1.5, end = 2, size = 0.04, color = 'black'),
    z = list(show = TRUE, start = 0, end = 1, size = 0.005)),
  x = ~x,
  y = ~y,
  z = ~z)
fig









##____ Contour plot with 2D data
library(metR)
copula_prob_for_plotting_2d_new<- copula_prob_for_plotting_2d[-1,]

copula_prob_for_plotting_2d_new_df<- data.frame(copula_prob_for_plotting_2d_new)
names(copula_prob_for_plotting_2d_new_df)<- c("time","mag","cdf")



# Create new variables swapping x and y
df_rotated <- copula_prob_for_plotting_2d_new_df
df_rotated$x_rot <- df_rotated$time    # swap: new x is original y
df_rotated$y_rot <- df_rotated$mag     # new y is original x
ggplot(df_rotated, aes(x = x_rot, y = y_rot, z = cdf)) +
  geom_contour(aes(color = ..level..), breaks = seq(0.1, 0.9, by = 0.1)) +
  # geom_text_contour(
  #   breaks = seq(0.1, 0.9, by = 0.1),
  #   size = 4,
  #   color = "black",
  #   stroke = 0.2,
  #   check_overlap = TRUE,              # ✅ avoid repeated labels
  #   label.placer = label_placer_flattest(),  # ✅ one label at flatter point
  #   skip = 0                           # include all lines
  # ) +
  geom_text_contour(
    breaks = seq(0.1, 0.9, by = 0.1),
    size = 5.5,
    color = "black",
    stroke = 0.2,
    check_overlap = TRUE,
    label.placer = label_placer_flattest(),
    skip = 0,
    nudge_y = 1.5   # 🔼 slight upward shift
  )+
  scale_color_viridis_c(name = "Probability") +
  guides(color = guide_colorbar(
    barwidth = 1.5, barheight = 10,
    title.theme = element_text(size = 18, face = "bold"),
    label.theme = element_text(size = 16)
  )) +
  theme(
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16)
  )+
  scale_y_reverse() +
  coord_fixed(ratio = 1/2) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold", color = "black", size = 16),
    axis.text = element_text(face = "bold", color = "black", size = 14),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5)
  ) +
  labs(
    title = "Smoothed Joint CDF Contour Plot (One Label per Line)",
    x = "Time Interval", y = "Incremental Magnitude"
  )


















## Contour plots _________________________


library(ggplot2)

# Simulate data
#set.seed(123)
#n <- 500
#x <- rnorm(n)
#y <- x + rnorm(n, sd = 0.5)

# Grid for evaluation
xgrid <- seq(1, max(max_mag_diff), length.out = 100)
ygrid <- seq(1, max(max_time_diff), length.out = 100)
grid <- expand.grid(x = xgrid, y = ygrid)


# Estimate joint CDF at each grid point
#joint_cdf <- function(x0, y0, x, y) {
  #mean(x <= x0 & y <= y0)
#}

#time_diff_scale<- c(1,time_diff_scale)
#mag_diff_scale<- c(1,mag_diff_scale)

#copula_prob_for_plotting<- matrix(nrow = length(time_diff_scale), ncol = length(mag_diff_scale))
copula_prob_for_plotting_cdf<- matrix(nrow = 1, ncol = 1)



# Prob of 1 year _________________________
sort_time_dif<- sort(na_rm_time_diff)
sort_time_prob<- sort(ori_prob_time)
sort_mag_dif<- sort(na_rm_mag_diff)
sort_mag_prob<- sort(ori_prob_mag)

sort_time_dif_new<- c(0,sort_time_dif,max(sort_time_dif)+1)
sort_time_prob_new<- c(0,sort_time_prob,max(sort_time_prob)+0.0001)

sort_mag_dif_new<- c(0,sort_mag_dif,max(sort_mag_dif)+1)
sort_mag_prob_new<- c(0,sort_mag_prob,max(sort_mag_prob)+0.0001)


#copula_density_for_plotting<- matrix(nrow = length(time_diff_scale), ncol = length(mag_diff_scale))
for (t in 1:length(xgrid)){
  
  for (j in 1:length(ygrid)){
    
    time_gap_RB<- ygrid[j]
    mag_gap_RB<- xgrid[t]
    
    
    # Prob of 1 year _________________________
    #sort_time_dif<- sort(na_rm_time_diff)
    #sort_time_prob<- sort(ori_prob_time)
    ind_gr_1_tim_dif<- which(sort_time_dif_new > time_gap_RB)
    low_time_val<- sort_time_dif_new[(ind_gr_1_tim_dif[1]) - 1]
    upp_time_val<- sort_time_dif_new[ind_gr_1_tim_dif[1]]
    low_time_prob<- sort_time_prob_new[(ind_gr_1_tim_dif[1]) - 1]
    upp_time_prob<- sort_time_prob_new[ind_gr_1_tim_dif[1]]
    
    prob_time_RB<- (((time_gap_RB - low_time_val)/(upp_time_val - low_time_val))*(upp_time_prob - low_time_prob)) + low_time_prob
    
    # Prob of 3 mm _________________________
    #sort_mag_dif<- sort(na_rm_mag_diff)
    #sort_mag_prob<- sort(ori_prob_mag)
    ind_gr_1_mag_dif<- which(sort_mag_dif_new > mag_gap_RB)
    low_mag_val<- sort_mag_dif_new[(ind_gr_1_mag_dif[1]) - 1]
    upp_mag_val<- sort_mag_dif_new[ind_gr_1_mag_dif[1]]
    low_mag_prob<- sort_mag_prob_new[(ind_gr_1_mag_dif[1]) - 1]
    upp_mag_prob<- sort_mag_prob_new[ind_gr_1_mag_dif[1]]
    
    prob_mag_RB<- (((mag_gap_RB - low_mag_val)/(upp_mag_val - low_mag_val))*(upp_mag_prob - low_mag_prob)) + low_mag_prob
    
    
    
    prob_intpolated_mag_time_dif<- matrix(c(1 - prob_mag_RB,prob_time_RB), ncol = 2)
    
    # Estimate probs  _________________________________________
    prob_normal<- pCopula(prob_intpolated_mag_time_dif, normal.cop)
    densty_normal<- dCopula(prob_intpolated_mag_time_dif, normal.cop)
    
    if (is.na(prob_normal) == T){
      print(paste("ERROR _________________ t = ",t,"and j=",j,sep = ""))
      #prob_normal<- 0
    }
    
    
    copula_prob_for_plotting_cdf<- rbind(copula_prob_for_plotting_cdf,prob_normal)
    #copula_density_for_plotting[t,j]<- densty_normal
    
    print(paste("t= ",t," Time -- To be till ",length(time_diff_scale),"  AND     ","j= ",j," Mag -- To be till ",length(mag_diff_scale),sep = ""))
    
  }
  
  
  
  
}







copula_prob_for_plotting_cdf<- copula_prob_for_plotting_cdf[-1,]



# Evaluate CDF over the grid
#z <- mapply(joint_cdf, grid$x, grid$y, MoreArgs = list(x = x, y = y))
#z <- mapply(joint_cdf, grid$x, grid$y, MoreArgs = list(x = x, y = y))
#grid$cdf <- z
grid$cdf <- copula_prob_for_plotting_cdf

# Plot with clean colorbar matching contours




library(ggplot2)

### Another flip but not in code

ggplot(grid, aes(x = x, y = y, z = cdf)) +
  geom_contour(aes(color = ..level..), breaks = seq(0.1, 0.9, by = 0.1)) +
  scale_color_viridis_c(name = "CDF Level") +
  #scale_y_reverse() +  # 🔁 Flip vertically
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold", color = "black", size = 16),
    axis.text = element_text(face = "bold", color = "black", size = 14),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5)
  ) +
  labs(
    title = "Joint CDF Contour Plot ",
    x = "X", y = "Y"
  )




















