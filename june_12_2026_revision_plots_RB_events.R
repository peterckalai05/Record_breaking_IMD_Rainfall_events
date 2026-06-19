


### _______________________________________________Showing false trend detection
library(magrittr)
library(multipanelfigure)
library(extRemes)
library(Kendall)
library(ggplot2)
library(gridExtra)
library(lmomco)

#20,21,25,27

set.seed(25)
sample_length<- 500
record_sample<- c(10,15,25,50,75,100,125,150,175,200)
p_value_mu_sigma<- matrix(nrow = sample_length,ncol = 1)
for (r in 1:length(record_sample)){
  
  
  mk_mu_trend<- matrix(nrow = sample_length, ncol = 1)
  mk_sd_trend<- matrix(nrow = sample_length, ncol = 1)
  for (s in 1:sample_length){
    record_length<- record_sample[r]
    window<- 2
    mu_0<- 10
    #mu_1<- (mu_0/100)*percentage_increase[i]
    sigma_0<- 5
    #sigma_1<- (sigma_0/100)*percentage_increase[i]
    xi_0<- 0.1
    
    #rnorm(3, mean=10, sd=2)
    time_series<- matrix(nrow = record_length, ncol = 1)
    for (t in 1:record_length){
      
      temp_mu<- mu_0 #+ (mu_1*t)
      
      temp_sigma<- sigma_0 #+ (sigma_1*t)
      
      time_series[t,1]<- revd(1, loc=temp_mu, scale=temp_sigma,shape = xi_0)
      #time_series[t,1]<- rnorm(1, mean=temp_mu, sd=temp_sigma)
    }
    
    MK_test<- MannKendall(time_series)
    #source("D:/Test_scale/ptr/st_dev_ser.R")
    # source("F:/research_work/Kalai_IITR_office_pc/trend_in_variability/ptr/abs_diff_ser.R")
    # 
    # if (MK_test$sl < 0.1){
    #   num_ser<- 1:nrow(time_series)
    #   
    #   lm_model<- lm(time_series ~ num_ser)
    #   
    #   detrend_ser<- time_series - (num_ser*lm_model$coefficients[2])
    #   #plot(num_ser,time_series)
    #   #plot(num_ser,detrend_ser)
    #   #std_series<- st_dev_ser(detrend_ser,window)
    #   
    #   std_series<- abs_diff_ser(detrend_ser,window)
    #   mk_test_sd<- MannKendall(std_series)
    # }#else {
      #std_series<- st_dev_ser(time_series,window)
      #std_series<- abs_diff_ser(time_series,window)
      #mk_test_sd<- MannKendall(std_series)
    #}
    
    
    
    
    
    #plot(1:nrow(std_series),std_series)
    #MK_test
    #mk_test_sd
    mk_mu_trend[s,1]<- MK_test$sl
    #mk_sd_trend[s,1]<- mk_test_sd$sl
    
    
    
    
    cat(s)
  }
  
  p_value_mu_sigma<- cbind(p_value_mu_sigma,mk_mu_trend)
  #p_value_mu_sigma<- cbind(p_value_mu_sigma,mk_sd_trend)
  
  
}

p_value_mu_sigma<- p_value_mu_sigma[,-1]


#Convert to percentage
signific_lim<- 0.05
percentage_values_pval<- matrix(nrow = 1, ncol = ncol(p_value_mu_sigma))
for (c in 1:ncol(p_value_mu_sigma)){
  
  percentage_values_pval[1,c]<- (length(which(p_value_mu_sigma[,c] < signific_lim))/nrow(p_value_mu_sigma))*100
  
}

percentage_values_pval<- data.frame(t(matrix(percentage_values_pval, ncol = 1)))


#percentage_values_pval

png("F:/research_work/Kalai_IITR_office_pc/RB_events/discus_Prof_G/block_Maxima/ASCE_JHE/first_round_review/plots/MK_false_Trend_Percentage_95conf.png",
    width = 2400,
    height = 1800,
    res = 300)

par(mar = c(6, 6, 4, 2))

bp<- barplot(
  height = as.numeric(percentage_values_pval[1, ]),
  names.arg = FALSE,   # suppress default x labels
  col = "lightblue",
  border = "black",
  xlab = "Record Length",
  ylab = "Percentage Identified with False Trend (%)",
  main = "Percentage of Significant Trends at 0.1 significance",
  cex.axis = 1.5,      # y-axis tick labels
  cex.lab = 1.5,
  cex.main = 1.8,
  font.axis = 1.5,
  font.lab = 1.5,
  font.main = 1.5,
  ylim = c(0, 30)
)

axis(
  side = 1,
  at = bp,
  labels = record_sample,
  cex.axis = 1.4,      # larger x-axis labels
  font = 1.4             # bold
)

dev.off()





#### ____________________________________________________________________________ Stationarity Proof ___________



## Related to import of data
#mag_diff_RB_event<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/discus_Prof_G/max_2_day/no_dayGAP/mag_difference_record_breaking_event_0_days_gap.csv")
mag_diff_RB_event<- read.csv(file = "F:/research_work/Kalai_IITR_office_pc/data_prepared/block_Maxima/mag_dif_block_maxima.csv")
mag_diff_RB_event<- mag_diff_RB_event[,-1]


#time_diff_RB_event<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/discus_Prof_G/max_2_day/no_dayGAP/time_difference_record_breaking_event_0_days_gap.csv")
time_diff_RB_event<- read.csv(file = "F:/research_work/Kalai_IITR_office_pc/data_prepared/block_Maxima/time_dif_block_maxima.csv")
time_diff_RB_event<-time_diff_RB_event[,-1]

i<- 2799
i<- 4560
mag_RB_sample_site<- mag_diff_RB_event[,i]
time_RB_sample_site<- time_diff_RB_event[,i]

mag_RB_sample_site_na_rm<- mag_RB_sample_site[!is.na(mag_RB_sample_site)]
time_RB_sample_site_na_rm<- time_RB_sample_site[!is.na(time_RB_sample_site)]

# # Regression __________________________________________________________________________
# # _____________________________________________________________________________________
# # 
# 
# 
# lm_fit_mag_diff<- lm(mag_RB_sample_site_na_rm ~ c(1:length(mag_RB_sample_site_na_rm)))
# 
# summary(lm_fit_mag_diff)
# 
# #The significance of the trend is given by the p-value of the slope coefficient:
#   
# summary(lm_fit_mag_diff)$coefficients
# 
# #or directly:
#   
# p_value <- summary(lm_fit_mag_diff)$coefficients[2,4]
# p_value






## Check the barplot for percentage RB events with trends ______________
## ______________ ______________ ______________ ______________ ______________

#identify the site with longest records __________
number_of_RB_events<- matrix(nrow = 1, ncol = 1)
for (i in 1:ncol(mag_diff_RB_event)){
  
  temp_site_mag_diff_RB<- mag_diff_RB_event[,i] 
  
  number_of_RB_events<- rbind(number_of_RB_events, length(temp_site_mag_diff_RB[!is.na(temp_site_mag_diff_RB)]))
  
  
}

number_of_RB_evnt<- number_of_RB_events[-1,]

#index_max_RB_events<- which(number_of_RB_evnt == max(number_of_RB_evnt))

index_max_RB_events<- which(number_of_RB_evnt >=10)



mk_mag_diff_RB<- matrix(nrow = length(index_max_RB_events), ncol = 1)
mk_time_diff_RB<- matrix(nrow = length(index_max_RB_events), ncol = 1)
for (i in 1:length(index_max_RB_events)){
  
  temp_mag_diffRB_series<- mag_diff_RB_event[index_max_RB_events[i]]
  temp_time_diffRB_series<- time_diff_RB_event[index_max_RB_events[i]]
  
  temp_mag_diffRB_series_na_rm<- temp_mag_diffRB_series[!is.na(temp_mag_diffRB_series)]
  temp_time_diffRB_series_na_rm<- temp_time_diffRB_series[!is.na(temp_time_diffRB_series)]
  
  
  mk_test_mag_diff_RB<- MannKendall(temp_mag_diffRB_series_na_rm)
  mk_test_time_diff_RB<- MannKendall(temp_time_diffRB_series_na_rm)
  
  mk_mag_diff_RB[i,1]<- mk_test_mag_diff_RB$sl
  mk_time_diff_RB[i,1]<- mk_test_time_diff_RB$sl
  
}


original_index_mag_diff_with_no_trend<- index_max_RB_events[which(mk_mag_diff_RB > 0.001)]
original_index_time_diff_with_no_trend<- index_max_RB_events[which(mk_time_diff_RB > 0.001)]


#intersect(original_index_mag_diff_with_no_trend,original_index_time_diff_with_no_trend)

#max(number_of_RB_evnt[intersect(original_index_mag_diff_with_no_trend,original_index_time_diff_with_no_trend)])



percentage_with_no_trend_mag_diff_RB<- (length(original_index_mag_diff_with_no_trend)/length(index_max_RB_events))*100
percentage_with_no_trend_time_diff_RB<- (length(original_index_time_diff_with_no_trend)/length(index_max_RB_events))*100




# Data __________________________________________________________Plot the percentage with 99% confidence
trend_percent <- c(round(percentage_with_no_trend_mag_diff_RB,0), round(percentage_with_no_trend_time_diff_RB,0))

png("F:/research_work/Kalai_IITR_office_pc/RB_events/discus_Prof_G/block_Maxima/ASCE_JHE/first_round_review/plots/Percentage_RB_events_with_No_Trend99point9.png",
    width = 2400,
    height = 1800,
    res = 300)

par(mar = c(6, 6, 4, 2))

#trend_percent <- c(65.8, 87.8)

bp <- barplot(
  height = trend_percent,
  width = 0.5,              # narrower bars
  names.arg = FALSE,
  col = "lightblue",
  border = "black",
  xlab = "",
  ylab = "Percentage of RB eventswith No Trend (%)",
  main = "Percentage of RB Events with No Trend at 99.9% conficence",
  cex.axis = 1.5,
  cex.lab = 1.5,
  cex.main = 1.5,
  font.axis = 2,
  font.lab = 2,
  font.main = 2,
  ylim = c(0, 110)
)

axis(
  side = 1,
  at = bp,
  labels = c("Incremental Magnitude of RB events", "Time between RB events"),
  cex.axis = 1.1,
  font = 2
)

# Add percentage labels above bars
text(
  x = bp,
  y = trend_percent + 3,
  labels = paste0(round(trend_percent, 1), "%"),
  cex = 1.4,
  font = 2
)

dev.off()


















## Insert the old code to verify _________________
library(ncdf4)
library(terra)
library(lmomco)
library(Kendall)
library(ggplot2)
library(devtools)
library(plotly)
library(doParallel)
library(foreach)

#library(devtools) # To install copula from git hub
#install_github("mmaechler/copula")
library(copula) #install.packages("copula",dependencies = TRUE)

## Function to check leap year
# Function to check for a leap year
is_leap_year <- function(year) {
  if ((year %% 4 == 0 && year %% 100 != 0) || year %% 400 == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}




list_of_imd_data_files<- list.files("F:/research_work/Kalai_IITR_office_pc/IMD_data/Indian_Rainfall_0_pnt_25")

# Find the years of records of IMD
years_of_record<- matrix(nrow = length(list_of_imd_data_files), ncol = 1)
number_of_days_each_year<- matrix(nrow = length(list_of_imd_data_files), ncol = 1)
for (i in 1:length(list_of_imd_data_files)){
  
  temp_list_file<- list_of_imd_data_files[i]
  years_of_record[i]<- as.numeric(substr(temp_list_file, 9,12))
  
  
  if (is_leap_year(as.numeric(substr(temp_list_file, 9,12))) ==T){
    number_of_days_each_year[i,1]<- 366
  }else{
    number_of_days_each_year[i,1]<- 365
  }
  
  
  
}









path_IMD_ls<- paste("F:/research_work/Kalai_IITR_office_pc/IMD_data/Indian_Rainfall_0_pnt_25/",list_of_imd_data_files[1],sep = "")
nc_IMD_ls<- nc_open(path_IMD_ls)
name_var<- attributes(nc_IMD_ls$var)
dim_lon_IMD<- ncvar_get(nc_IMD_ls, attributes(nc_IMD_ls$dim)$names[1])
dim_lat_IMD<- ncvar_get(nc_IMD_ls, attributes(nc_IMD_ls$dim)$names[2])

IMD_data<- ncvar_get(nc_IMD_ls, "RAINFALL")

nc_close(nc_IMD_ls)


if (is_leap_year(as.numeric(substr(path_IMD_ls,81,84))) == T){
  n_days<- 366
}else{
  n_days<- 365
}



## Identify all the 
all_long_serially_IMD<- matrix(nrow = 1, ncol=1)
all_lat_serially_IMD<- matrix(nrow = 1, ncol=1)
for (i in 1:length(dim_lon_IMD)){
  
  for (j in 1:length(dim_lat_IMD)){
    
    temp_IMD_long<- dim_lon_IMD[i]
    temp_IMD_lat<- dim_lat_IMD[j]
    
    IMD_data_one_grid<- IMD_data[i,j,]
    
    length_na_IMD<- length(which(is.na(IMD_data_one_grid)==T))
    
    if (length_na_IMD != n_days){
      all_long_serially_IMD<- rbind(all_long_serially_IMD,temp_IMD_long)
      all_lat_serially_IMD<- rbind(all_lat_serially_IMD,temp_IMD_lat)
      
    }
    
    
  }
  
}


all_long_serially_IMD<- all_long_serially_IMD[-1,]
all_lat_serially_IMD<- all_lat_serially_IMD[-1,]





## Extract the IMD data for the grids without NAs___________________________________

IMD_data_removing_na<- matrix(nrow = 1, ncol = length(all_long_serially_IMD))
for (f in 1:length(list_of_imd_data_files)){
  
  path_IMD<- paste("F:/research_work/Kalai_IITR_office_pc/IMD_data/Indian_Rainfall_0_pnt_25/",list_of_imd_data_files[f],sep = "")
  nc_IMD<- nc_open(path_IMD)
  name_var<- attributes(nc_IMD$var)
  dim_lon_IMD<- ncvar_get(nc_IMD, attributes(nc_IMD$dim)$names[1])
  dim_lat_IMD<- ncvar_get(nc_IMD, attributes(nc_IMD$dim)$names[2])
  
  IMD_data<- ncvar_get(nc_IMD, "RAINFALL")
  nc_close(nc_IMD_ls)
  
  numb_days<- number_of_days_each_year[f,1]
  
  data_IMD_all_grid_for_a_year<- matrix(nrow = numb_days, ncol = length(all_long_serially_IMD))
  for (i in 1:length(all_long_serially_IMD)){
    ind_lon<- which(dim_lon_IMD == all_long_serially_IMD[i])
    ind_lat<- which(dim_lat_IMD == all_lat_serially_IMD[i])
    
    IMD_data_for_a_grid<- IMD_data[ind_lon,ind_lat,]
    
    data_IMD_all_grid_for_a_year[,i]<- IMD_data_for_a_grid
    
    #cat(i)
  }
  
  
  IMD_data_removing_na<- rbind(IMD_data_removing_na,data_IMD_all_grid_for_a_year)
  
  print(paste("Data extracted for year ",years_of_record[f,1],"-----------To be extracted till ",years_of_record[nrow(years_of_record),1],sep = ""))
  
}

IMD_data_removing_na<- IMD_data_removing_na[-1,]





IMD_data_removing_na_7Days_movngAVG<- IMD_data_removing_na
IMD_data_removing_na_7Days_movngAVG[44845,]<- 0 # To replace NA

## Extract the block maximas ___________________________________________________

start_ind_data_extract<- matrix(nrow = nrow(number_of_days_each_year), ncol = 1)
end_ind_data_extract<- matrix(nrow = nrow(number_of_days_each_year), ncol = 1)
for (i in 1:nrow(number_of_days_each_year)){
  
  end_ind<- sum(number_of_days_each_year[1:i,1])
  start_ind<- end_ind - (number_of_days_each_year[i,1]) + 1
  
  start_ind_data_extract[i,1]<- start_ind
  end_ind_data_extract[i,1]<- end_ind
  cat(i)
}


# Extract _maxima __________________________________________________________________
block_maximas<- matrix(nrow = nrow(years_of_record), ncol = ncol(IMD_data_removing_na_7Days_movngAVG))
block_maximas_julian_date<- matrix(nrow = nrow(years_of_record), ncol = ncol(IMD_data_removing_na_7Days_movngAVG))

for (i in 1:ncol(IMD_data_removing_na_7Days_movngAVG)){
  
  temp_data_grid<- IMD_data_removing_na_7Days_movngAVG[,i]
  
  for (j in 1:nrow(start_ind_data_extract)){
    
    temp_data_for_a_year<- temp_data_grid[(start_ind_data_extract[j]):(end_ind_data_extract[j])]
    
    temp_data_for_a_year<- temp_data_for_a_year[!is.na(temp_data_for_a_year)]
    
    max_data_year<- max(temp_data_for_a_year)
    
    ind_max<- which(temp_data_for_a_year== max_data_year)
    
    block_maximas[j,i]<- max_data_year
    block_maximas_julian_date[j,i]<- ind_max[1]
  }
  
  
  cat(i)
}





RB_bMaxima<- matrix(nrow = nrow(block_maximas), ncol = ncol(IMD_data_removing_na_7Days_movngAVG))
time_RB_bMaxima<- matrix(nrow = nrow(block_maximas), ncol = ncol(IMD_data_removing_na_7Days_movngAVG))

#bMaxima<- matrix(nrow = nrow(block_maximas), ncol = ncol(IMD_data_removing_na_7Days_movngAVG))
time_bMaxima<- matrix(nrow = nrow(block_maximas), ncol = ncol(IMD_data_removing_na_7Days_movngAVG))

for (i in 1:ncol(block_maximas)){
  
  temp_data<- block_maximas[,i]
  temp_data_time<- block_maximas_julian_date[,i]
  
  temp_record<- temp_data[1]
  temp_record_time<- temp_data_time[1]
  temp_record_time_bM<- temp_data_time[1]
  
  #if (j == 1){
  RB_bMaxima[1,i]<- temp_data[1]
  time_RB_bMaxima[1,i]<- 0
  time_bMaxima[1,i]<- 0
  #}
  
  
  for (j in 2:length(temp_data)){
    
    temp_next_record<- temp_data[j]
    
    
    time_first_bM<- temp_record_time_bM
    time_last_bM<- sum(number_of_days_each_year[1:(j-1),1]) + temp_data_time[j]
    time_dif_days_bM<- time_last_bM - time_first_bM
    
    #time_diff_RB_bMaxima[j,i]<- time_dif_days
    time_bMaxima[j,i]<- time_last_bM/365
    
    temp_record_bM<- temp_next_record
    temp_record_time_bM<- time_last_bM
    
    
    
    ## For record breaking events____________________________________________
    if (temp_next_record > temp_record){
      
      
      
      #mag_difference_RB_bMaxima[j,i]<- temp_next_record - temp_record
      RB_bMaxima[j,i]<- temp_next_record
      
      time_first<- temp_record_time
      time_last<- sum(number_of_days_each_year[1:(j-1),1]) + temp_data_time[j]
      time_dif_days<- time_last - time_first
      
      #time_diff_RB_bMaxima[j,i]<- time_dif_days
      time_RB_bMaxima[j,i]<- time_last/365
      
      temp_record<- temp_next_record
      temp_record_time<- time_last
      
      
    }
    
    
    
  }
  
  cat(i)
}



i<- 2799

temp_grid_bMaxima<- block_maximas[,i]
temp_grid_time_bMaxima<- time_bMaxima[,i]

temp_grid_RB_bMaxima<- RB_bMaxima[,i]
temp_grid_RB_bMaxima_time<- time_RB_bMaxima[,i]



mag_RB_events<- temp_grid_RB_bMaxima[!is.na(temp_grid_RB_bMaxima)]
time_RB_events<- temp_grid_RB_bMaxima_time[!is.na(temp_grid_RB_bMaxima_time)]


## incremental values____
incre_mag_RB<- matrix(nrow = 1, ncol = 1)
incre_time_RB<- matrix(nrow = 1, ncol = 1)
for (i in 1:(length(mag_RB_events) - 1)){
  
  temp_event_mag_dif<- mag_RB_events[i+1] - mag_RB_events[i]
  incre_mag_RB<- rbind(incre_mag_RB, temp_event_mag_dif)
  
  temp_event_time_dif<- time_RB_events[i+1] - time_RB_events[i]
  incre_time_RB<- rbind(incre_time_RB, temp_event_time_dif)
}

incre_mag_RB<- incre_mag_RB[-1,]
incre_time_RB<- incre_time_RB[-1,]
























































































## Simulation experiment______________________________________

## _______________________________ _______________________________ _______________________________ _______________________________


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
input_lat_long<- read.csv(file = "F:/research_work/Kalai_IITR_office_pc/data_prepared/IMD_data_Long_lat_removing_NA.csv")
input_lat_long<- t(input_lat_long)
lat_long_data<- input_lat_long[-1,]
num_rec_lat_long<- cbind(lat_long_data,number_of_records)
#number_rec_lat_long<- data.frame(num_rec_lat_long[,-3])
number_rec_lat_long<- data.frame(num_rec_lat_long)
names(number_rec_lat_long)<- c("lon","lat","num_rec")








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







#file_dir<- "D:/Kalai_IITR_office_pc/discus_Prof_G/block_Maxima/record_length/"

## Plot the lmoment ratios

#list_of_thres_hold_record<- c(10,15,20,25,30,35,40,45,50) #-1 is added to incorporate the differences
list_of_thres_hold_record<- c(10,15,20)#,25,30,35,40,45,50)

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
}







threshold_for_records<- 10


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












#pool_of_distributions<- c("AEP4","GEV","GLO","GNO","GOV","GPA","PE3","PDQ3")
pool_of_distributions<- c("GEV","GLO","GNO","GPA","PE3")


pool_of_record_length<- c(10,15,20)#,25,30,35,40,45,50,70,100)
number_of_simulation<- 1000

equli_record_length_10_15_20_mean_mag_diff<- matrix(nrow = length(pool_of_record_length), ncol = length(pool_of_distributions))
equli_record_length_10_15_20_median_mag_diff<- matrix(nrow = length(pool_of_record_length), ncol = length(pool_of_distributions))

equli_record_length_10_15_20_mean_time_diff<- matrix(nrow = length(pool_of_record_length), ncol = length(pool_of_distributions))
equli_record_length_10_15_20_median_time_diff<- matrix(nrow = length(pool_of_record_length), ncol = length(pool_of_distributions))


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
  
  png(paste("F:/research_work/Kalai_IITR_office_pc/RB_events/discus_Prof_G/block_Maxima/ASCE_JHE/first_round_review/plots/dist_record_len/diff_plot_forLength/","mag_",temp_rec_len,"_record_length.png",sep = ""))
  op <- par(mar = c(5,4.5,4,2) + 0.4)
  boxplot(euc_dist_mag_samp_df, names = c(names_rl),xlab="",ylab="Euclidean Distance",main=paste("Mag - ",temp_rec_len,"Record length",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  #boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
  box(lwd=2)
  dev.off()
  
  equli_record_length_10_15_20_mean_mag_diff[r,]<- apply(euc_dist_mag_samp_df, 2, mean)
  equli_record_length_10_15_20_median_mag_diff[r,]<- apply(euc_dist_mag_samp_df, 2, median)
  #apply(euc_dist_mag_samp_df, 2, quantile, prob=0.25, na.rm=T)
  
  
  
  
  
  euc_dist_time_samp_df<- data.frame(euc_dist_time_samp)
  names(euc_dist_time_samp_df)<- c(names_rl[1:length(names_rl)])
  
  png(paste("F:/research_work/Kalai_IITR_office_pc/RB_events/discus_Prof_G/block_Maxima/ASCE_JHE/first_round_review/plots/dist_record_len/diff_plot_forLength/","time_",temp_rec_len,"_record_length.png",sep = ""))
  op <- par(mar = c(5,4.5,4,2) + 0.4)
  boxplot(euc_dist_time_samp_df, names = c(names_rl),xlab="",ylab="Euclidean Distance",main=paste("Time - ",temp_rec_len,"Record length",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1.5),las=2)
  #boxplot(euc_dist_time_samp_df, main=paste(temp_dist," - Time",sep = ""))
  box(lwd=2)
  dev.off()
  
  equli_record_length_10_15_20_mean_time_diff[r,]<- apply(euc_dist_time_samp_df, 2, mean)
  equli_record_length_10_15_20_median_time_diff[r,]<- apply(euc_dist_time_samp_df, 2, median)
  
}





round_equli_record_length_10_15_20_median_mag_diff<- round(equli_record_length_10_15_20_median_mag_diff,2)
abs(round_equli_record_length_10_15_20_median_mag_diff[1,] - round_equli_record_length_10_15_20_median_mag_diff[1,])
abs(round_equli_record_length_10_15_20_median_mag_diff[1,] - round_equli_record_length_10_15_20_median_mag_diff[3,])


round_equli_record_length_10_15_20_median_time_diff<- round(equli_record_length_10_15_20_median_time_diff,2)
abs(round_equli_record_length_10_15_20_median_time_diff[1,] - round_equli_record_length_10_15_20_median_time_diff[2,])
abs(round_equli_record_length_10_15_20_median_time_diff[1,] - round_equli_record_length_10_15_20_median_time_diff[3,])





























## Make AIC plot to _________________________
## _______________________________________________________________________________________________________________________

library(copula)
library(lmomco)



## Related to import of data
#mag_diff_RB_event<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/discus_Prof_G/max_2_day/no_dayGAP/mag_difference_record_breaking_event_0_days_gap.csv")
mag_diff_RB_event<- read.csv(file = "F:/research_work/Kalai_IITR_office_pc/data_prepared/block_Maxima/mag_dif_block_maxima.csv")
mag_diff_RB_event<- mag_diff_RB_event[,-1]


#time_diff_RB_event<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/discus_Prof_G/max_2_day/no_dayGAP/time_difference_record_breaking_event_0_days_gap.csv")
time_diff_RB_event<- read.csv(file = "F:/research_work/Kalai_IITR_office_pc/data_prepared/block_Maxima/time_dif_block_maxima.csv")
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
input_lat_long<- read.csv(file = "F:/research_work/Kalai_IITR_office_pc/data_prepared/IMD_data_Long_lat_removing_NA.csv")
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



aic_normal<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))
aic_gumbel<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))
aic_clayton<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))
aic_frank<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))






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
    
    
    ## Likelihood and AIC ___________________
    logLik_normal  <- sum(log(dCopula(probs_dif_mag_time, normal.cop)))
    logLik_gumbel  <- sum(log(dCopula(probs_dif_mag_time, gumbel.cop)))
    logLik_clayton <- sum(log(dCopula(probs_dif_mag_time, clayton.cop)))
    logLik_frank   <- sum(log(dCopula(probs_dif_mag_time, frank.cop)))
    
    
    AIC_normal<- 2 - 2*logLik_normal
    AIC_gumbel<- 2 - 2*logLik_gumbel
    AIC_clayton<- 2 - 2*logLik_clayton
    AIC_frank<- 2 - 2*logLik_frank
    
    
    
    aic_normal[1,i]<- AIC_normal
    aic_gumbel[1,i]<- AIC_gumbel
    aic_clayton[1,i]<- AIC_clayton
    aic_frank[1,i]<- AIC_frank
    
    
    
    
    
    
    
    
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
    
    
    
    total_abs_bias_normal[1,i]<- ((sum(abs_bias_normal))/(sum(emp_cupula_manual[,1])))#*100
    total_abs_bias_gumbel[1,i]<- ((sum(abs_bias_gumbel))/(sum(emp_cupula_manual[,1])))#*100
    total_abs_bias_clayton[1,i]<- ((sum(abs_bias_clayton))/(sum(emp_cupula_manual[,1])))#*100
    total_abs_bias_frank[1,i]<- ((sum(abs_bias_frank))/(sum(emp_cupula_manual[,1])))#*100
    
    
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
png(paste("F:/research_work/Kalai_IITR_office_pc/RB_events/discus_Prof_G/block_Maxima/ASCE_JHE/first_round_review/plots/","boxplot_ABias.png",sep = ""))
op <- par(mar = c(7,4.5,4,2) + 0.4)
boxplot(a_bias_percent_df, names = c(names_copula),xlab="",ylab="A-Bias of joint Probability",main=paste("Percentage A-Bias of the SUM ",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
#boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
box(lwd=2)
dev.off()








### _______________ ______________________ ___________________________ AIC

ind_na_normal_aic<- which(is.na(aic_normal) == T)
length(ind_na_normal_aic)

ind_na_gumbel_aic<- which(is.na(aic_gumbel) == T)
length(ind_na_gumbel_aic)

ind_na_clayton_aic<- which(is.na(aic_clayton) == T)
length(ind_na_clayton_aic)

ind_na_frank_aic<- which(is.na(aic_frank) == T)
length(ind_na_frank_aic)



## Combine all the percent abs bias ___________________

aic_all<- cbind(matrix(aic_normal[,-ind_na_normal_aic], ncol = 1),
                       matrix(aic_gumbel[,-ind_na_normal_aic], ncol = 1),
                       matrix(aic_clayton[,-ind_na_normal_aic], ncol = 1),
                       matrix(aic_frank[,-ind_na_normal_aic], ncol = 1))




aic_df<- data.frame(aic_all)
names(aic_df)<- c("Gaussian","Gumbel","Clayton","Frank")
names_copula<- c("Gaussian","Gumbel","Clayton","Frank")

#boxplot(a_bias_percent_df)

#png(paste("D:/Research_IIT_Roorkee/Record_breaking_events/plots_daysgap/discus_Prof_G/max_2_day/no_dayGAP/copula/","boxplot_ABiasPercentage.png",sep = ""))
png(paste("F:/research_work/Kalai_IITR_office_pc/RB_events/discus_Prof_G/block_Maxima/ASCE_JHE/first_round_review/plots/","aic_fitted_copulas.png",sep = ""))
op <- par(mar = c(7,4.5,4,2) + 0.4)
boxplot(aic_df, names = c(names_copula),xlab="",ylab="AIC values",main=paste("AIC for the fitted copulas ",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,20),las=2)
#boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
box(lwd=2)
dev.off()

































#____________________________________________________________________________________________________________



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
  
  
  
  
  
  # Distribution based ___________________________________
  probability_for_magGPA_and_timeGPA_frank<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magGEV_and_timeGEV_frank<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magPE3_and_timePE3_frank<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  
  probability_for_magGPA_and_timeGEV_frank<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magGPA_and_timePE3_frank<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magGEV_and_timeGPA_frank<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magGEV_and_timePE3_frank<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magPE3_and_timeGPA_frank<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magPE3_and_timeGEV_normal<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  
  probability_for_magGEV_and_timeGPA_frank<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magPE3_and_timeGPA_frank<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magGPA_and_timeGEV_frank<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magPE3_and_timeGEV_frank<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magGPA_and_timePE3_frank<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  probability_for_magGEV_and_timePE3_frank<- matrix(nrow = 1, ncol = ncol(time_diff_record_breaking_event))
  
  
  
  
  
  
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
      
      
      
      
      
      
      # Save Frank Copula
      probability_for_magGPA_and_timeGPA_frank[1,i]<- pCopula(matrix(c(1 - cdf_mag_gpa,cdf_time_gpa), ncol = 2), frank.cop)
      probability_for_magGEV_and_timeGEV_frank[1,i]<- pCopula(matrix(c(1 - cdf_mag_gev,cdf_time_gev), ncol = 2), frank.cop)
      probability_for_magPE3_and_timePE3_frank[1,i]<- pCopula(matrix(c(1 - cdf_mag_pe3,cdf_time_pe3), ncol = 2), frank.cop)
      
      probability_for_magGPA_and_timeGEV_frank[1,i]<- pCopula(matrix(c(1 - cdf_mag_gpa,cdf_time_gev), ncol = 2), frank.cop)
      probability_for_magGPA_and_timePE3_frank[1,i]<- pCopula(matrix(c(1 - cdf_mag_gpa,cdf_time_pe3), ncol = 2), frank.cop)
      probability_for_magGEV_and_timeGPA_frank[1,i]<- pCopula(matrix(c(1 - cdf_mag_gev,cdf_time_gpa), ncol = 2), frank.cop)
      probability_for_magGEV_and_timePE3_frank[1,i]<- pCopula(matrix(c(1 - cdf_mag_gev,cdf_time_pe3), ncol = 2), frank.cop)
      probability_for_magPE3_and_timeGPA_frank[1,i]<- pCopula(matrix(c(1 - cdf_mag_pe3,cdf_time_gpa), ncol = 2), frank.cop)
      probability_for_magPE3_and_timeGEV_frank[1,i]<- pCopula(matrix(c(1 - cdf_mag_pe3,cdf_time_gev), ncol = 2), frank.cop)
      
      probability_for_magGEV_and_timeGPA_frank[1,i]<- pCopula(matrix(c(1 - cdf_mag_gev,cdf_time_gpa), ncol = 2), frank.cop)
      probability_for_magPE3_and_timeGPA_frank[1,i]<- pCopula(matrix(c(1 - cdf_mag_pe3,cdf_time_gpa), ncol = 2), frank.cop)
      probability_for_magGPA_and_timeGEV_frank[1,i]<- pCopula(matrix(c(1 - cdf_mag_gpa,cdf_time_gev), ncol = 2), frank.cop)
      probability_for_magPE3_and_timeGEV_frank[1,i]<- pCopula(matrix(c(1 - cdf_mag_pe3,cdf_time_gev), ncol = 2), frank.cop)
      probability_for_magGPA_and_timePE3_frank[1,i]<- pCopula(matrix(c(1 - cdf_mag_gpa,cdf_time_pe3), ncol = 2), frank.cop)
      probability_for_magGEV_and_timePE3_frank[1,i]<- pCopula(matrix(c(1 - cdf_mag_gev,cdf_time_pe3), ncol = 2), frank.cop)
      
      
      
      
      
      
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
  file_dir_cop<- "F:/research_work/Kalai_IITR_office_pc/RB_events/discus_Prof_G/block_Maxima/ASCE_JHE/first_round_review/plots/copula/"
  
  
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
  plot(emp_prob_mag_time_diff, est_clayton_prob_mag_time_diff,main =paste("Clayton Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Clayton"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_clayton_prob_mag_time_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  
  png(paste(file_dir_cop,"frank","_mag_",mag_gap_RB,"mm_and_time_",time_gap_RB,"_yr",".png",sep = ))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_frank_prob_mag_time_diff,main =paste("Frank Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Frank"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_frank_prob_mag_time_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  dev.off()
  
  
  
  # Box plot of bias __________________________________________________________________________
  spat_plt_exc_emp_prob_mag_time_diff<- probability_for_mag_and_time_empirical
  spat_plt_exc_est_normal_prob_mag_time_diff<- probability_for_mag_and_time_normal
  spat_plt_exc_est_frank_prob_mag_time_diff<- probability_for_mag_and_time_frank
  
  #est_clayton_prob_mag_time_diff<- probability_for_mag_and_time_clayton[,ind_with_data]
  #est_frank_prob_mag_time_diff<- probability_for_mag_and_time_frank[,ind_with_data]
  
  per_absolute_bias_exc_prob_joint<- (abs((spat_plt_exc_est_normal_prob_mag_time_diff - spat_plt_exc_emp_prob_mag_time_diff)))#/(spat_plt_exc_emp_prob_mag_time_diff))*100 
  per_absolute_bias_exc_prob_joint_frank<- (abs((spat_plt_exc_est_frank_prob_mag_time_diff - spat_plt_exc_emp_prob_mag_time_diff)))#/(spat_plt_exc_emp_prob_mag_time_diff))*100 
  
  per_bias_exc_prob_joint<- (((spat_plt_exc_est_normal_prob_mag_time_diff - spat_plt_exc_emp_prob_mag_time_diff)))#/(spat_plt_exc_emp_prob_mag_time_diff))*100 
  per_bias_exc_prob_joint_frank<- (((spat_plt_exc_est_frank_prob_mag_time_diff - spat_plt_exc_emp_prob_mag_time_diff)))#/(spat_plt_exc_emp_prob_mag_time_diff))*100 
  
  
  per_abs_bias_dataEXCProb<- per_absolute_bias_exc_prob_joint[,ind_with_data]
  per_abs_bias_dataEXCProb_frank<- per_absolute_bias_exc_prob_joint_frank[,ind_with_data]
  per_bias_dataEXCProb<- per_bias_exc_prob_joint[,ind_with_data]
  per_bias_dataEXCProb_frank<- per_bias_exc_prob_joint_frank[,ind_with_data]
  
  computed_normal_prob<- spat_plt_exc_est_normal_prob_mag_time_diff[,ind_with_data]
  png(paste(file_dir_cop,"probability_","normal","_mag_",mag_gap_RB,"mm_and_time_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,4.5,4,2) + 0.4)
  #boxplot(per_abs_bias_dataEXCProb, names = c(""),xlab="",ylab="Percentage A-Bias (%)",main=paste("Percentage A-Bias of Joint Prob",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  boxplot(computed_normal_prob, names = c(""),xlab="",ylab="Probability",main=paste("Probability",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  
  #boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
  box(lwd=2)
  abline(h=0.4235969, lwd=2, col="red")# at 50% of quantile
  dev.off()
  
  #quantile(computed_normal_prob[!is.na(computed_normal_prob)],c(0.05,0.25,0.5,0.75,0.95))
  
  
  ## Frank _______________________
  #______________________________
  computed_frank_prob<- spat_plt_exc_est_frank_prob_mag_time_diff[,ind_with_data]
  png(paste(file_dir_cop,"probability_","frank","_mag_",mag_gap_RB,"mm_and_time_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,4.5,4,2) + 0.4)
  #boxplot(per_abs_bias_dataEXCProb, names = c(""),xlab="",ylab="Percentage A-Bias (%)",main=paste("Percentage A-Bias of Joint Prob",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  boxplot(computed_frank_prob, names = c(""),xlab="",ylab="Probability",main=paste("Probability",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  
  #boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
  box(lwd=2)
  abline(h=0.4235969, lwd=2, col="red")# at 50% of quantile
  dev.off()
  
  
  
  
  
  
  
  
  
  
  ### Probability of exceedence of above mag in < time ________________________________________
  ####-_________________________________________________________________________________________
  
  # Box plot of absolute bias __________for exc joint prob
  png(paste(file_dir_cop,"JointProb_ABias_","normal","_mag_",mag_gap_RB,"mm_and_time_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,4.5,4,2) + 0.4)
  #boxplot(per_abs_bias_dataEXCProb, names = c(""),xlab="",ylab="Percentage A-Bias (%)",main=paste("Percentage A-Bias of Joint Prob",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  boxplot(per_abs_bias_dataEXCProb, names = c(""),xlab="",ylab="A-Bias of probability",main=paste("A-Bias of the probability",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  
  #boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
  box(lwd=2)
  dev.off()
  
  quantile(per_abs_bias_dataEXCProb[!is.na(per_abs_bias_dataEXCProb)],c(0.05,0.25,0.5,0.75,0.95))
  quantile(per_abs_bias_dataEXCProb[!is.na(per_abs_bias_dataEXCProb)], 0.75)
  
  
  
  ## Frank
  png(paste(file_dir_cop,"JointProb_ABias_","frank","_mag_",mag_gap_RB,"mm_and_time_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,4.5,4,2) + 0.4)
  #boxplot(per_abs_bias_dataEXCProb, names = c(""),xlab="",ylab="Percentage A-Bias (%)",main=paste("Percentage A-Bias of Joint Prob",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  boxplot(per_abs_bias_dataEXCProb_frank, names = c(""),xlab="",ylab="A-Bias of probability",main=paste("A-Bias of the probability",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  
  #boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
  box(lwd=2)
  dev.off()
  
  quantile(per_abs_bias_dataEXCProb_frank[!is.na(per_abs_bias_dataEXCProb_frank)], 0.75)
  ####-_________________________________________________________________________________________
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## Plot for the distribution______________________________________________________________
  est_normal_prob_magGPA_timeGPA_diff<- probability_for_magGPA_and_timeGPA_normal[,ind_with_data]
  est_normal_prob_magGEV_timeGEV_diff<- probability_for_magGEV_and_timeGEV_normal[,ind_with_data]
  est_normal_prob_magPE3_timePE3_diff<- probability_for_magPE3_and_timePE3_normal[,ind_with_data]
  
  #png(paste(file_dir_cop,"distribution/normal","_magGPA_",mag_gap_RB,"mm_and_timeGPA_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGPA_timeGPA_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGPA_timeGPA_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  #dev.off()
  
  #png(paste(file_dir_cop,"distribution/normal","_magGEV_",mag_gap_RB,"mm_and_timeGEV_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGEV_timeGEV_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGEV_timeGEV_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  #dev.off()
  
  #png(paste(file_dir_cop,"distribution/normal","_magPE3_",mag_gap_RB,"mm_and_timePE3_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magPE3_timePE3_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magPE3_timePE3_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  #dev.off()
  
  
  est_normal_prob_magGPA_timeGEV_diff<- probability_for_magGPA_and_timeGEV_normal[,ind_with_data]
  est_normal_prob_magGPA_timePE3_diff<- probability_for_magGPA_and_timePE3_normal[,ind_with_data]
  est_normal_prob_magGEV_timeGPA_diff<- probability_for_magGEV_and_timeGPA_normal[,ind_with_data]
  est_normal_prob_magGEV_timePE3_diff<- probability_for_magGEV_and_timePE3_normal[,ind_with_data]
  est_normal_prob_magPE3_timeGPA_diff<- probability_for_magPE3_and_timeGPA_normal[,ind_with_data]
  est_normal_prob_magPE3_timeGEV_diff<- probability_for_magPE3_and_timeGEV_normal[,ind_with_data]
  
  #png(paste(file_dir_cop,"distribution/normal","_magGPA_",mag_gap_RB,"mm_and_timeGEV_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGPA_timeGEV_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGPA_timeGEV_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  #dev.off()
  
  
  #png(paste(file_dir_cop,"distribution/normal","_magGPA_",mag_gap_RB,"mm_and_timePE3_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGPA_timePE3_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGPA_timePE3_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  #dev.off()
  
  #png(paste(file_dir_cop,"distribution/normal","_magGEV_",mag_gap_RB,"mm_and_timeGPA_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGEV_timeGPA_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGEV_timeGPA_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  #dev.off()
  
  #png(paste(file_dir_cop,"distribution/normal","_magGEV_",mag_gap_RB,"mm_and_timePE3_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGEV_timePE3_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGEV_timePE3_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  #dev.off()
  
  
  #png(paste(file_dir_cop,"distribution/normal","_magPE3_",mag_gap_RB,"mm_and_timeGPA_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magPE3_timeGPA_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magPE3_timeGPA_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  #dev.off()
  
  
  #png(paste(file_dir_cop,"distribution/normal","_magPE3_",mag_gap_RB,"mm_and_timeGEV_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magPE3_timeGEV_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magPE3_timeGEV_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  #dev.off()
  
  
  ##
  est_normal_prob_magGEV_timeGPA_diff<- probability_for_magGEV_and_timeGPA_normal[,ind_with_data]
  est_normal_prob_magPE3_timeGPA_diff<- probability_for_magPE3_and_timeGPA_normal[,ind_with_data]
  est_normal_prob_magGPA_timeGEV_diff<- probability_for_magGPA_and_timeGEV_normal[,ind_with_data]
  est_normal_prob_magPE3_timeGEV_diff<- probability_for_magPE3_and_timeGEV_normal[,ind_with_data]
  est_normal_prob_magGPA_timePE3_diff<- probability_for_magGPA_and_timePE3_normal[,ind_with_data]
  est_normal_prob_magGEV_timePE3_diff<- probability_for_magGEV_and_timePE3_normal[,ind_with_data]
  
  
  #png(paste(file_dir_cop,"distribution/normal","_magGEV_",mag_gap_RB,"mm_and_timeGPA_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGEV_timeGPA_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGEV_timeGPA_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  #dev.off()
  
  
  #png(paste(file_dir_cop,"distribution/normal","_magPE3_",mag_gap_RB,"mm_and_timeGPA_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magPE3_timeGPA_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magPE3_timeGPA_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  #dev.off()
  
  #png(paste(file_dir_cop,"distribution/normal","_magGPA_",mag_gap_RB,"mm_and_timeGEV_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGPA_timeGEV_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGPA_timeGEV_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  #dev.off()
  
  #png(paste(file_dir_cop,"distribution/normal","_magPE3_",mag_gap_RB,"mm_and_timeGEV_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magPE3_timeGEV_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magPE3_timeGEV_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  #dev.off()
  
  
  #png(paste(file_dir_cop,"distribution/normal","_magGPA_",mag_gap_RB,"mm_and_timePE3_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGPA_timePE3_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGPA_timePE3_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  #dev.off()
  
  
  #png(paste(file_dir_cop,"distribution/normal","_magGEV_",mag_gap_RB,"mm_and_timePE3_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(emp_prob_mag_time_diff, est_normal_prob_magGEV_timePE3_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(est_normal_prob_magGEV_timePE3_diff ~ emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  #dev.off()
  
  
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
  
  
  #png(paste(file_dir_cop,"prob_excedance/","normal","_mag_",mag_gap_RB,"mm_and_time_",time_gap_RB,"_yr",".png",sep = ""))
  op <- par(mar = c(7,6,4,2) + 0.4)
  plot(excd_emp_prob_mag_time_diff, excd_est_normal_prob_mag_time_diff,main =paste("Normal Mag(",mag_gap_RB,"mm) and Time(",time_gap_RB,"yr)",sep=""), type = "p",pch=1, xlim = c(xlim_l,xlim_u), ylim = c(ylim_l,ylim_u), xlab =  paste("Emperical probability",sep = ""), ylab = paste("Model ","Normal"," probability", sep = ""), col="black",cex.lab=1,cex.axis=1,cex.main=1.5,bty="n",xaxt="n",yaxt="n")
  axis(1, lwd = 2,cex.lab=1.2,cex.axis=1.2,at = seq(xlim_l, xlim_u,by=0.2))
  #axis(2, las=1, lwd = 2,cex.lab=1.5,cex.axis=1.5, at = c(points_in_Y_axis),labels=c("1-Apr","1-May","1-Jun","1-Jul","1-Aug","1-Sep","1-Oct","1-Nov","1-Dec","1-Jan","1-Feb","1-Mar","1-Apr"))
  axis(2, las=1, lwd = 2,cex.lab=1.2,cex.axis=1.2, at = seq(ylim_l, ylim_u,by=0.2))
  lm_for_rsq<- lm(excd_est_normal_prob_mag_time_diff ~ excd_emp_prob_mag_time_diff) 
  mtext(paste("R-sq=",round(summary(lm_for_rsq)$r.squared,2),sep = ""), side = 3, line = 0, col = "red", cex = 1.7)
  abline(0,1,lwd=2)
  #dev.off()
  
  
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
  spat_plt_exc_est_frank_prob_mag_time_diff<- 1 - probability_for_mag_and_time_frank
  
  #est_clayton_prob_mag_time_diff<- probability_for_mag_and_time_clayton[,ind_with_data]
  #est_frank_prob_mag_time_diff<- probability_for_mag_and_time_frank[,ind_with_data]
  
  per_absolute_bias_exc_prob_joint<- (abs((spat_plt_exc_est_normal_prob_mag_time_diff - spat_plt_exc_emp_prob_mag_time_diff))/(spat_plt_exc_emp_prob_mag_time_diff))#*100 
  per_absolute_bias_exc_prob_joint_frank<- (abs((spat_plt_exc_est_frank_prob_mag_time_diff - spat_plt_exc_emp_prob_mag_time_diff))/(spat_plt_exc_emp_prob_mag_time_diff))#*100 
  
  per_bias_exc_prob_joint<- (((spat_plt_exc_est_normal_prob_mag_time_diff - spat_plt_exc_emp_prob_mag_time_diff))/(spat_plt_exc_emp_prob_mag_time_diff))#*100 
  per_bias_exc_prob_joint_frank<- (((spat_plt_exc_est_frank_prob_mag_time_diff - spat_plt_exc_emp_prob_mag_time_diff))/(spat_plt_exc_emp_prob_mag_time_diff))#*100 
  
  
  per_abs_bias_dataEXCProb<- per_absolute_bias_exc_prob_joint[,ind_with_data]
  per_abs_bias_dataEXCProb_frank<- per_absolute_bias_exc_prob_joint_frank[,ind_with_data]
  per_bias_dataEXCProb<- per_bias_exc_prob_joint[,ind_with_data]
  per_bias_dataEXCProb_frank<- per_bias_exc_prob_joint_frank[,ind_with_data]
  
  # Box plot of absolute bias __________for exc joint prob
  png(paste(file_dir_cop,"excJointProb_ABiasPerc_","normal","_mag_",mag_gap_RB,"mm_and_time_",time_gap_RB,"_yr.png",sep = ""))
  op <- par(mar = c(7,4.5,4,2) + 0.4)
  boxplot(per_abs_bias_dataEXCProb*100, names = c(""),xlab="",ylab="Percentage A-Bias (%)",main=paste("Percentage A-Bias of Joint Prob. Exc ",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,100),las=2)
  #boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
  box(lwd=2)
  dev.off()
  
  
  # Box plot of absolute bias __________for exc joint prob
  png(paste(file_dir_cop,"excJointProb_ABiasPerc_","frank","_mag_",mag_gap_RB,"mm_and_time_",time_gap_RB,"_yr.png",sep = ""))
  op <- par(mar = c(7,4.5,4,2) + 0.4)
  boxplot(per_abs_bias_dataEXCProb_frank*100, names = c(""),xlab="",ylab="Percentage A-Bias (%)",main=paste("Percentage A-Bias of Joint Prob. Exc. (Frank)",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,100),las=2)
  #boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
  box(lwd=2)
  dev.off()
  
  
  
  
  
  
  
  
  # Same plot__________ no percentage
  png(paste(file_dir_cop,"No_PER_excJointProb_ABiasPerc_","normal","_mag_",mag_gap_RB,"mm_and_time_",time_gap_RB,"_yr.png",sep = ""))
  op <- par(mar = c(7,4.5,4,2) + 0.4)
  boxplot(per_abs_bias_dataEXCProb, names = c(""),xlab="",ylab="A-Bias",main=paste("A-Bias of Joint Prob. Exc ",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  #boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
  box(lwd=2)
  dev.off()
  
  
  # Box plot of absolute bias __________for exc joint prob
  png(paste(file_dir_cop,"No_PER_excJointProb_ABiasPerc_","frank","_mag_",mag_gap_RB,"mm_and_time_",time_gap_RB,"_yr.png",sep = ""))
  op <- par(mar = c(7,4.5,4,2) + 0.4)
  boxplot(per_abs_bias_dataEXCProb_frank, names = c(""),xlab="",ylab="A-Bias (%)",main=paste("A-Bias of Joint Prob. Exc. (Frank)",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
  #boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
  box(lwd=2)
  dev.off()
}













## For one site 3D plot of joint distribution ______________________________
## _________________________________________________________________________

## Joint distribution ________________________________________ Normal

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






## 3D plotting for the density and probability __________________________ Normal
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

fig <- fig %>% layout(
  title = list(
    text = "<b>Gaussian Copula</b>",
    x = 0.5,
    xanchor = "center"
  )
)

fig


##____ Contour plot with 2D data _____________ Normal
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










## For one site 3D plot of joint distribution ______________________________
## _________________________________________________________________________

## Joint distribution ________________________________________ Frank

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
    prob_frank<- pCopula(prob_intpolated_mag_time_dif, frank.cop)
    densty_frank<- dCopula(prob_intpolated_mag_time_dif, frank.cop)
    
    
    
    
    copula_prob_for_plotting_2d<- rbind(copula_prob_for_plotting_2d,
                                        matrix(c(time_gap_RB,mag_gap_RB,prob_frank),nrow = 1))
    
    
    
    
    
    
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
    
    copula_prob_for_plotting[t,j]<- prob_frank
    copula_density_for_plotting[t,j]<- densty_frank
    
    print(paste("t= ",t," Time -- To be till ",length(time_diff_scale),"  AND     ","j= ",j," Mag -- To be till ",length(mag_diff_scale),sep = ""))
    
  }
  
  
  
  
}






## 3D plotting for the density and probability __________________________ Frank
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

fig <- fig %>% layout(
  title = list(
    text = "<b>Frank Copula</b>",
    x = 0.5,
    xanchor = "center"
  )
)

fig






##____ Contour plot with 2D data _____________ Frank
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
    title = "Smoothed Joint CDF Contour Plot (One Label per Line) Frank",
    x = "Time Interval", y = "Incremental Magnitude"
  )







































###________________________________________________________________ POT ___________________________________________________________

library(ncdf4)
library(terra)
library(lmomco)
library(Kendall)
library(ggplot2)
library(devtools)
library(plotly)
library(doParallel)
library(foreach)

#library(devtools) # To install copula from git hub
#install_github("mmaechler/copula")
library(copula) #install.packages("copula",dependencies = TRUE)

## Function to check leap year
# Function to check for a leap year
is_leap_year <- function(year) {
  if ((year %% 4 == 0 && year %% 100 != 0) || year %% 400 == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}





list_of_imd_data_files<- list.files("F:/research_work/Kalai_IITR_office_pc/IMD_data/Indian_Rainfall_0_pnt_25")

# Find the years of records of IMD
years_of_record<- matrix(nrow = length(list_of_imd_data_files), ncol = 1)
number_of_days_each_year<- matrix(nrow = length(list_of_imd_data_files), ncol = 1)
for (i in 1:length(list_of_imd_data_files)){
  
  temp_list_file<- list_of_imd_data_files[i]
  years_of_record[i]<- as.numeric(substr(temp_list_file, 9,12))
  
  
  if (is_leap_year(as.numeric(substr(temp_list_file, 9,12))) ==T){
    number_of_days_each_year[i,1]<- 366
  }else{
    number_of_days_each_year[i,1]<- 365
  }
  
  
  
}









path_IMD_ls<- paste("F:/research_work/Kalai_IITR_office_pc/IMD_data/Indian_Rainfall_0_pnt_25/",list_of_imd_data_files[1],sep = "")
nc_IMD_ls<- nc_open(path_IMD_ls)
name_var<- attributes(nc_IMD_ls$var)
dim_lon_IMD<- ncvar_get(nc_IMD_ls, attributes(nc_IMD_ls$dim)$names[1])
dim_lat_IMD<- ncvar_get(nc_IMD_ls, attributes(nc_IMD_ls$dim)$names[2])

IMD_data<- ncvar_get(nc_IMD_ls, "RAINFALL")

nc_close(nc_IMD_ls)


if (is_leap_year(as.numeric(substr(path_IMD_ls,81,84))) == T){
  n_days<- 366
}else{
  n_days<- 365
}



## Identify all the 
all_long_serially_IMD<- matrix(nrow = 1, ncol=1)
all_lat_serially_IMD<- matrix(nrow = 1, ncol=1)
for (i in 1:length(dim_lon_IMD)){
  
  for (j in 1:length(dim_lat_IMD)){
    
    temp_IMD_long<- dim_lon_IMD[i]
    temp_IMD_lat<- dim_lat_IMD[j]
    
    IMD_data_one_grid<- IMD_data[i,j,]
    
    length_na_IMD<- length(which(is.na(IMD_data_one_grid)==T))
    
    if (length_na_IMD != n_days){
      all_long_serially_IMD<- rbind(all_long_serially_IMD,temp_IMD_long)
      all_lat_serially_IMD<- rbind(all_lat_serially_IMD,temp_IMD_lat)
      
    }
    
    
  }
  
}


all_long_serially_IMD<- all_long_serially_IMD[-1,]
all_lat_serially_IMD<- all_lat_serially_IMD[-1,]





## Extract the IMD data for the grids without NAs___________________________________

IMD_data_removing_na<- matrix(nrow = 1, ncol = length(all_long_serially_IMD))
for (f in 1:length(list_of_imd_data_files)){
  
  path_IMD<- paste("F:/research_work/Kalai_IITR_office_pc/IMD_data/Indian_Rainfall_0_pnt_25/",list_of_imd_data_files[f],sep = "")
  nc_IMD<- nc_open(path_IMD)
  name_var<- attributes(nc_IMD$var)
  dim_lon_IMD<- ncvar_get(nc_IMD, attributes(nc_IMD$dim)$names[1])
  dim_lat_IMD<- ncvar_get(nc_IMD, attributes(nc_IMD$dim)$names[2])
  
  IMD_data<- ncvar_get(nc_IMD, "RAINFALL")
  nc_close(nc_IMD_ls)
  
  numb_days<- number_of_days_each_year[f,1]
  
  data_IMD_all_grid_for_a_year<- matrix(nrow = numb_days, ncol = length(all_long_serially_IMD))
  for (i in 1:length(all_long_serially_IMD)){
    ind_lon<- which(dim_lon_IMD == all_long_serially_IMD[i])
    ind_lat<- which(dim_lat_IMD == all_lat_serially_IMD[i])
    
    IMD_data_for_a_grid<- IMD_data[ind_lon,ind_lat,]
    
    data_IMD_all_grid_for_a_year[,i]<- IMD_data_for_a_grid
    
    #cat(i)
  }
  
  
  IMD_data_removing_na<- rbind(IMD_data_removing_na,data_IMD_all_grid_for_a_year)
  
  print(paste("Data extracted for year ",years_of_record[f,1],"-----------To be extracted till ",years_of_record[nrow(years_of_record),1],sep = ""))
  
}

IMD_data_removing_na<- IMD_data_removing_na[-1,]





IMD_data_removing_na_7Days_movngAVG<- IMD_data_removing_na
IMD_data_removing_na_7Days_movngAVG[44845,]<- 0 # To replace NA


## Extract the block maximas ___________________________________________________

start_ind_data_extract<- matrix(nrow = nrow(number_of_days_each_year), ncol = 1)
end_ind_data_extract<- matrix(nrow = nrow(number_of_days_each_year), ncol = 1)
for (i in 1:nrow(number_of_days_each_year)){
  
  end_ind<- sum(number_of_days_each_year[1:i,1])
  start_ind<- end_ind - (number_of_days_each_year[i,1]) + 1
  
  start_ind_data_extract[i,1]<- start_ind
  end_ind_data_extract[i,1]<- end_ind
  cat(i)
}


# Extract _maxima __________________________________________________________________
block_maximas<- matrix(nrow = nrow(years_of_record), ncol = ncol(IMD_data_removing_na_7Days_movngAVG))
block_maximas_julian_date<- matrix(nrow = nrow(years_of_record), ncol = ncol(IMD_data_removing_na_7Days_movngAVG))

for (i in 1:ncol(IMD_data_removing_na_7Days_movngAVG)){
  
  temp_data_grid<- IMD_data_removing_na_7Days_movngAVG[,i]
  
  for (j in 1:nrow(start_ind_data_extract)){
    
    temp_data_for_a_year<- temp_data_grid[(start_ind_data_extract[j]):(end_ind_data_extract[j])]
    
    temp_data_for_a_year<- temp_data_for_a_year[!is.na(temp_data_for_a_year)]
    
    max_data_year<- max(temp_data_for_a_year)
    
    ind_max<- which(temp_data_for_a_year== max_data_year)
    
    block_maximas[j,i]<- max_data_year
    block_maximas_julian_date[j,i]<- ind_max[1]
  }
  
  
  cat(i)
}





RB_bMaxima<- matrix(nrow = nrow(block_maximas), ncol = ncol(IMD_data_removing_na_7Days_movngAVG))
time_RB_bMaxima<- matrix(nrow = nrow(block_maximas), ncol = ncol(IMD_data_removing_na_7Days_movngAVG))

#bMaxima<- matrix(nrow = nrow(block_maximas), ncol = ncol(IMD_data_removing_na_7Days_movngAVG))
time_bMaxima<- matrix(nrow = nrow(block_maximas), ncol = ncol(IMD_data_removing_na_7Days_movngAVG))

for (i in 1:ncol(block_maximas)){
  
  temp_data<- block_maximas[,i]
  temp_data_time<- block_maximas_julian_date[,i]
  
  temp_record<- temp_data[1]
  temp_record_time<- temp_data_time[1]
  temp_record_time_bM<- temp_data_time[1]
  
  #if (j == 1){
  RB_bMaxima[1,i]<- temp_data[1]
  time_RB_bMaxima[1,i]<- 0
  time_bMaxima[1,i]<- 0
  #}
  
  
  for (j in 2:length(temp_data)){
    
    temp_next_record<- temp_data[j]
    
    
    time_first_bM<- temp_record_time_bM
    time_last_bM<- sum(number_of_days_each_year[1:(j-1),1]) + temp_data_time[j]
    time_dif_days_bM<- time_last_bM - time_first_bM
    
    #time_diff_RB_bMaxima[j,i]<- time_dif_days
    time_bMaxima[j,i]<- time_last_bM/365
    
    temp_record_bM<- temp_next_record
    temp_record_time_bM<- time_last_bM
    
    
    
    ## For record breaking events____________________________________________
    if (temp_next_record > temp_record){
      
      
      
      #mag_difference_RB_bMaxima[j,i]<- temp_next_record - temp_record
      RB_bMaxima[j,i]<- temp_next_record
      
      time_first<- temp_record_time
      time_last<- sum(number_of_days_each_year[1:(j-1),1]) + temp_data_time[j]
      time_dif_days<- time_last - time_first
      
      #time_diff_RB_bMaxima[j,i]<- time_dif_days
      time_RB_bMaxima[j,i]<- time_last/365
      
      temp_record<- temp_next_record
      temp_record_time<- time_last
      
      
    }
    
    
    
  }
  
  cat(i)
}













## Plot of the time series _______________________________

##__________________ POT ___________________________________








i<- 2799


data_for_grid<- IMD_data_removing_na_7Days_movngAVG[,i]

index_zeros<- which(data_for_grid == 0.0000000)

data_for_grid_rm_zeros<- data_for_grid[-index_zeros]


quantile_95th<- quantile(data_for_grid_rm_zeros, prob=0.95)
thres_hold<- quantile_95th



# considering only the RB events more than THRESHOLD ________________________

index_above_threshold_start<- min(which(data_for_grid >= thres_hold))


RB_events_mag_POT<- matrix(data_for_grid[index_above_threshold_start],nrow = 1, ncol = 1)
RB_events_time_POT<- matrix(index_above_threshold_start,nrow = 1, ncol = 1)

RB_mag<- data_for_grid[index_above_threshold_start] # Setting RB event

for (j in (index_above_threshold_start+1):length(data_for_grid)){
  
  temp_record<- data_for_grid[j]
  
  if (temp_record > RB_mag){
    RB_events_mag_POT<- rbind(RB_events_mag_POT,temp_record)
    RB_events_time_POT<- rbind(RB_events_time_POT,j)
    
    RB_mag <- temp_record
  }
  
  print(paste("Now doing: ",i,".        To be done till",length(data_for_grid), sep = ""))
}



## POT _plot _____________________________________
temp_grid_bMaxima<- data_for_grid
temp_grid_time_bMaxima<- 1:length(data_for_grid)

temp_grid_RB_bMaxima<- RB_events_mag_POT
temp_grid_RB_bMaxima_time<- RB_events_time_POT


mag_max_y<- max(temp_grid_bMaxima)
time_max_x<- max(temp_grid_time_bMaxima)

lat_imd<- all_lat_serially_IMD[i]
long_imd<- all_long_serially_IMD[i]






par(tck = -0.03,  # Length of ticks
    lwd = 5)       # Width of the ticks

#png(paste("D:/Kalai_IITR_office_pc/discus_Prof_G/display_site/site",i,".png",sep = ""), width = 400, height = 300) 
png(paste("F:/research_work/Kalai_IITR_office_pc/RB_events/discus_Prof_G/block_Maxima/ASCE_JHE/first_round_review/plots/POT_site",i,"_limit_20_x_lim1050_to_1100",".png",sep = ""), width = 400, height = 300) 
plot(temp_grid_time_bMaxima,temp_grid_bMaxima,main = paste("POT - ","Long:",long_imd,";","Lat:",lat_imd,sep = ""), 
     ylim=c(0,400),
     #ylim=c(0,mag_max_y),
     #xlim=c(0,length(data_for_grid)),
     xlim=c(1050,1100),
     #xlim=c(0,time_max_x),
     pch=16,col="black",
     xlab="Time of events (yr)", ylab="Magnitude of rainfall events (mm)")
par(new=T)
plot(temp_grid_RB_bMaxima_time,temp_grid_RB_bMaxima,
     ylim=c(0,400),
     #ylim=c(0,mag_max_y),
     #xlim=c(0,length(data_for_grid)),
     xlim=c(1050,1100),
     #xlim=c(0,time_max_x),
     pch=16,col="red",
     xlab="Time of events (yr)", ylab="Magnitude of rainfall events (mm)")

legend("topleft", legend=c("Rainfall", "Record Events"), 
       #fill = c("blue","red"))
       pch=c(16,16), col = c("black","red"), box.lwd = 2)
box(lwd = 4) 

#legend("topright", legend=c(paste("p-value=",round(mk_mag$sl,5),sep = ""), paste("p-value=",round(mk_time$sl,5),sep = "")), 
#fill = c("blue","red"))
#pch=c(), text.col = c("blue","red"), box.lwd = 2)
#box(lwd = 4) 
abline(h=thres_hold, col='yellow3', lwd=2)
par(new=F)

dev.off()



























i<- 2799
### Block maxima

temp_grid_bMaxima<- block_maximas[,i]
temp_grid_time_bMaxima<- time_bMaxima[,i]

temp_grid_RB_bMaxima<- RB_bMaxima[,i]
temp_grid_RB_bMaxima_time<- time_RB_bMaxima[,i]


mag_max_y<- max(temp_grid_bMaxima)
time_max_x<- max(temp_grid_time_bMaxima)

lat_imd<- all_lat_serially_IMD[i]
long_imd<- all_long_serially_IMD[i]






par(tck = -0.03,  # Length of ticks
    lwd = 5)       # Width of the ticks

#png(paste("D:/Kalai_IITR_office_pc/discus_Prof_G/display_site/site",i,".png",sep = ""), width = 400, height = 300) 
png(paste("F:/research_work/Kalai_IITR_office_pc/RB_events/discus_Prof_G/block_Maxima/ASCE_JHE/first_round_review/plots/AMS_site",i,"_limit_20",".png",sep = ""), width = 400, height = 300) 
plot(temp_grid_time_bMaxima,temp_grid_bMaxima,main = paste("Long:",long_imd,";","Lat:",lat_imd,sep = ""), 
     ylim=c(0,400),
     #ylim=c(0,mag_max_y),
     xlim=c(0,20),
     #xlim=c(0,time_max_x),
     pch=16,col="black",
     xlab="Time of events (yr)", ylab="Magnitude of rainfall events (mm)")
par(new=T)
plot(temp_grid_RB_bMaxima_time,temp_grid_RB_bMaxima,
     ylim=c(0,400),
     #ylim=c(0,mag_max_y),
     xlim=c(0,20),
     #xlim=c(0,time_max_x),
     pch=16,col="red",
     xlab="Time of events (yr)", ylab="Magnitude of rainfall events (mm)")

legend("topleft", legend=c("Annual Maximum", "Record Events"), 
       #fill = c("blue","red"))
       pch=c(16,16), col = c("black","red"), box.lwd = 2)
box(lwd = 4) 

#legend("topright", legend=c(paste("p-value=",round(mk_mag$sl,5),sep = ""), paste("p-value=",round(mk_time$sl,5),sep = "")), 
#fill = c("blue","red"))
#pch=c(), text.col = c("blue","red"), box.lwd = 2)
#box(lwd = 4) 

par(new=F)

dev.off()





























###-_______________________________________________________________________ Shorten the length _________________AIC plot



library(copula)
library(lmomco)



## Related to import of data
#mag_diff_RB_event<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/discus_Prof_G/max_2_day/no_dayGAP/mag_difference_record_breaking_event_0_days_gap.csv")
mag_diff_RB_event<- read.csv(file = "F:/research_work/Kalai_IITR_office_pc/data_prepared/block_Maxima/mag_dif_block_maxima.csv")
mag_diff_RB_event<- mag_diff_RB_event[,-1]


#time_diff_RB_event<- read.csv(file = "D:/Research_IIT_Roorkee/Record_breaking_events/data_generated/discus_Prof_G/max_2_day/no_dayGAP/time_difference_record_breaking_event_0_days_gap.csv")
time_diff_RB_event<- read.csv(file = "F:/research_work/Kalai_IITR_office_pc/data_prepared/block_Maxima/time_dif_block_maxima.csv")
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
input_lat_long<- read.csv(file = "F:/research_work/Kalai_IITR_office_pc/data_prepared/IMD_data_Long_lat_removing_NA.csv")
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



## Identify the total absolute bias ________________________________________--

#diff_mag_for1_year_above_threshold<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))

total_abs_bias_normal<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))
total_abs_bias_gumbel<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))
total_abs_bias_clayton<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))
total_abs_bias_frank<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))



aic_normal<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))
aic_gumbel<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))
aic_clayton<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))
aic_frank<- matrix(nrow=1, ncol = ncol(time_diff_record_breaking_event))






for (i in 1:ncol(time_diff_record_breaking_event)){
  
  if (number_of_records[i,1] >= threshold_for_records){
    temp_time_diff<- time_diff_record_breaking_event[,i]
    temp_mag_diff<- mag_difference_record_breaking_event[,i]
    
    na_rm_time_diff<- temp_time_diff[!is.na(temp_time_diff)]
    na_rm_mag_diff<- temp_mag_diff[!is.na(temp_mag_diff)]
    
    
    na_rm_mag_diff<- na_rm_mag_diff[1:(threshold_for_records-2)]
    na_rm_time_diff<- na_rm_time_diff[1:(threshold_for_records-2)]
    
    
    
    
    
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
    
    
    ## Likelihood and AIC ___________________
    logLik_normal  <- sum(log(dCopula(probs_dif_mag_time, normal.cop)))
    logLik_gumbel  <- sum(log(dCopula(probs_dif_mag_time, gumbel.cop)))
    logLik_clayton <- sum(log(dCopula(probs_dif_mag_time, clayton.cop)))
    logLik_frank   <- sum(log(dCopula(probs_dif_mag_time, frank.cop)))
    
    
    AIC_normal<- 2 - 2*logLik_normal
    AIC_gumbel<- 2 - 2*logLik_gumbel
    AIC_clayton<- 2 - 2*logLik_clayton
    AIC_frank<- 2 - 2*logLik_frank
    
    
    
    aic_normal[1,i]<- AIC_normal
    aic_gumbel[1,i]<- AIC_gumbel
    aic_clayton[1,i]<- AIC_clayton
    aic_frank[1,i]<- AIC_frank
    
    
    
    
    
    
    
    
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
    
    
    
    total_abs_bias_normal[1,i]<- ((sum(abs_bias_normal))/(sum(emp_cupula_manual[,1])))#*100
    total_abs_bias_gumbel[1,i]<- ((sum(abs_bias_gumbel))/(sum(emp_cupula_manual[,1])))#*100
    total_abs_bias_clayton[1,i]<- ((sum(abs_bias_clayton))/(sum(emp_cupula_manual[,1])))#*100
    total_abs_bias_frank[1,i]<- ((sum(abs_bias_frank))/(sum(emp_cupula_manual[,1])))#*100
    
    
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
png(paste("F:/research_work/Kalai_IITR_office_pc/RB_events/discus_Prof_G/block_Maxima/ASCE_JHE/first_round_review/plots/copula_shorter_length/","boxplot_ABias_8RB.png",sep = ""))
op <- par(mar = c(7,4.5,4,2) + 0.4)
boxplot(a_bias_percent_df, names = c(names_copula),xlab="",ylab="A-Bias of joint Probability",main=paste("Percentage A-Bias of the SUM ",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,1),las=2)
#boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
box(lwd=2)
dev.off()








### _______________ ______________________ ___________________________ AIC

ind_na_normal_aic<- which(is.na(aic_normal) == T)
length(ind_na_normal_aic)

ind_na_gumbel_aic<- which(is.na(aic_gumbel) == T)
length(ind_na_gumbel_aic)

ind_na_clayton_aic<- which(is.na(aic_clayton) == T)
length(ind_na_clayton_aic)

ind_na_frank_aic<- which(is.na(aic_frank) == T)
length(ind_na_frank_aic)



## Combine all the percent abs bias ___________________

aic_all<- cbind(matrix(aic_normal[,-ind_na_normal_aic], ncol = 1),
                matrix(aic_gumbel[,-ind_na_normal_aic], ncol = 1),
                matrix(aic_clayton[,-ind_na_normal_aic], ncol = 1),
                matrix(aic_frank[,-ind_na_normal_aic], ncol = 1))




aic_df<- data.frame(aic_all)
names(aic_df)<- c("Gaussian","Gumbel","Clayton","Frank")
names_copula<- c("Gaussian","Gumbel","Clayton","Frank")

#boxplot(a_bias_percent_df)

#png(paste("D:/Research_IIT_Roorkee/Record_breaking_events/plots_daysgap/discus_Prof_G/max_2_day/no_dayGAP/copula/","boxplot_ABiasPercentage.png",sep = ""))
png(paste("F:/research_work/Kalai_IITR_office_pc/RB_events/discus_Prof_G/block_Maxima/ASCE_JHE/first_round_review/plots/copula_shorter_length/","aic_fitted_copulas_8RB.png",sep = ""))
op <- par(mar = c(7,4.5,4,2) + 0.4)
boxplot(aic_df, names = c(names_copula),xlab="",ylab="AIC values",main=paste("AIC for the fitted copulas ",sep = ""),boxlty = 1,cex.lab=1.5,cex.axis=1.5,boxwex=0.4,boxlwd=1,staplewex = 0.5,staplelwd=2,whisklty = 1,whisklwd=2,outpch = 1,outcex = 1,cex.main=1.5,bty="n",outline = F,ylim=c(0,20),las=2)
#boxplot(euc_dist_mag_samp_df, main=paste(temp_dist," - Mag",sep = ""))
box(lwd=2)
dev.off()






