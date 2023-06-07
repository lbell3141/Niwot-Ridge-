#Lindsey Bell 
#Workflow functions used on Niwot Ridge Data

#8 zscore plots for the annual average of the growing season against the long-term annual average
workflow_8basic_varplots = function(dat_file, time_stamp_col, date_format) {
  #loading libraries  
  library(lubridate)
  library(dplyr)
  
  #loading in data and manipulating timestamps  
  dat_file = read.csv(dat_file, header = TRUE, na.strings= "-9999", skip=0, sep = ",")
  dat_file[[time_stamp_col]] = date_format(dat_file[[time_stamp_col]])
  dat_file$Months = months(dat_file[[time_stamp_col]])
  dat_file$years = format(dat_file[[time_stamp_col]], "%Y")
  
  #creating a df with variables of interest for summer growing season
  dat_file_fr = (data.frame(yr = dat_file$years,
                            mon = dat_file$Months,
                            swc = dat_file$SWC_F_MDS_1,
                            gpp = dat_file$GPP_DT_VUT_25,
                            nee = dat_file$NEE_VUT_25,
                            temp_atmos = dat_file$TA_F,
                            temp_soil = dat_file$TS_F_MDS_1,
                            precip = dat_file$P_F,
                            ppfd_in = dat_file$PPFD_IN,
                            ppfd_out = dat_file$PPFD_OUT) )
  dat_file_fr = dat_file_fr[dat_file_fr$mon %in% c('July','August','September','October','November'),]
  
  #finding long-term annual average and standard deviation by year (all values for a given column are the same but unique years are preserved to join with annual averages below)
  cols = 3:10
  og_col_names = colnames(dat_file_fr)[cols]
  col_mean = colMeans(dat_file_fr[, cols], na.rm = TRUE)
  unique_years = unique(dat_file_fr$yr)
  lt_dat = data.frame(matrix(NA, nrow = length(unique_years), ncol = length(cols) + 1))
  colnames(lt_dat) = c("yr", paste0("lt_mn_", og_col_names))
  lt_dat[, 1] = unique_years
  
  col_sd = apply(dat_file_fr[, cols], 2, sd, na.rm = TRUE)
  for (i in seq_along(cols)){
    col_name_mean = paste0("lt_mn_", og_col_names[i])
    col_name_sd = paste0("lt_sdev_", og_col_names[i])
    dat_file_fr[[col_name_mean]] = col_mean[i]
    dat_file_fr[[col_name_sd]] = col_sd[i]
    lt_dat[[col_name_mean]] = col_mean[i]
    lt_dat[[col_name_sd]] = col_sd[i]
  }
  
  #finding the annual mean for each variable
  columns = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip", "ppfd_in", "ppfd_out")
  annual_means = dat_file_fr %>%
    group_by(yr) %>%
    summarise(across(all_of(columns), mean, na.rm = TRUE, .names = "st_mn_{.col}")) %>%
    distinct()
  
  #joining annual and long-term annual averages to create a new data frame
  joined_dat = left_join(lt_dat, annual_means, by = "yr")
  
  #calculating z_score for each variable and placing in new columns in the data frame
  variables = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip", "ppfd_in", "ppfd_out")
  for (variable in variables){
    st_mean_col = paste0("st_mn_", variable)
    lt_mean_col = paste0("lt_mn_", variable)
    lt_sdev_col = paste0("lt_sdev_", variable)
    z_score_col = paste0("z_score_", variable)
    
    joined_dat = joined_dat%>%
      group_by(yr)%>%
      mutate(!!z_score_col := (.data[[st_mean_col]]-.data[[lt_mean_col]])/.data[[lt_sdev_col]])
  }
  
  #plotting graphs for each variable in one window 
  par(mfrow = c(3,3))
  z_score_vars = paste0("z_score_", variables)
  plots = lapply(z_score_vars, function(z_var){ 
    plot(joined_dat$yr, joined_dat[[z_var]],
         main = z_var, 
         xlab = "Year",
         ylab = "Z-Score",
         type = 'l', 
         lwd = 1,
    )
    points(joined_dat$yr, joined_dat[[z_var]])
    lines(joined_dat$yr, rep(0, length(joined_dat$yr)), lty = 2)
    grid()
  })
  
}


#monthly average for each month in the growing season for each year for 8 basic variables 
workflow_monavg = function(dat_file, time_stamp_col, date_format) {
  #loading libraries  
  library(lubridate)
  library(dplyr)
  
  dat_file = read.csv(dat_file, header = TRUE, na.strings= "-9999", skip=0, sep = ",")
  dat_file[[time_stamp_col]] = date_format(dat_file[[time_stamp_col]])
  dat_file$day = format(dat_file[[time_stamp_col]], "%d")
  dat_file$Months = months(dat_file[[time_stamp_col]])
  dat_file$years = format(dat_file[[time_stamp_col]], "%Y")
  
  #creating a df with variables of interest for summer growing season
  dat_file_fr = (data.frame(yr = dat_file$years,
                            mon = dat_file$Months,
                            swc = dat_file$SWC_F_MDS_1,
                            gpp = dat_file$GPP_DT_VUT_25,
                            nee = dat_file$NEE_VUT_25,
                            temp_atmos = dat_file$TA_F,
                            temp_soil = dat_file$TS_F_MDS_1,
                            precip = dat_file$P_F,
                            ppfd_in = dat_file$PPFD_IN,
                            ppfd_out = dat_file$PPFD_OUT) )
  dat_file_fr = dat_file_fr[dat_file_fr$mon %in% c('July','August','September','October','November'),]
  
  #finding the monthly mean for each year for each variable
  columns = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip", "ppfd_in", "ppfd_out")
  annual_monthly_means = dat_file_fr %>%
    group_by(yr, mon) %>%
    summarise(across(all_of(columns), mean, na.rm = TRUE, .names = "mn_{.col}")) %>%
    distinct()
}


#daily data by month for one year of interest (yoi)
daily_zscore_8var_yoi = function(dat_file, yoi, time_stamp_col, date_format) {
 
   #loading libraries  
  library(lubridate)
  library(dplyr)
  
  #loading in data and manipulating timestamps  
  dat_file = read.csv(dat_file, header = TRUE, na.strings= "-9999", skip=0, sep = ",")
  dat_file[[time_stamp_col]] = date_format(dat_file[[time_stamp_col]])
  dat_file$day = format(dat_file[[time_stamp_col]], "%d")
  dat_file$Months = months(dat_file[[time_stamp_col]])
  dat_file$years = format(dat_file[[time_stamp_col]], "%Y")
  
  #creating a df with variables of interest for summer growing season
  dat_file_fr = (data.frame(yr = dat_file$years,
                            mon = dat_file$Months,
                            day = dat_file$day,
                            swc = dat_file$SWC_F_MDS_1,
                            gpp = dat_file$GPP_DT_VUT_25,
                            nee = dat_file$NEE_VUT_25,
                            temp_atmos = dat_file$TA_F,
                            temp_soil = dat_file$TS_F_MDS_1,
                            precip = dat_file$P_F,
                            ppfd_in = dat_file$PPFD_IN,
                            ppfd_out = dat_file$PPFD_OUT) )
  dat_file_fr = dat_file_fr[dat_file_fr$mon %in% c('July','August','September','October','November'),]
  

  #finding the annual mean for each variable
  columns = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip", "ppfd_in", "ppfd_out")
  annual_means = dat_file_fr %>%
    group_by(yr) %>%
    summarise(across(all_of(columns), mean, na.rm = TRUE, .names = "st_mn_{.col}")) 
  
  #joining annual and long-term annual averages to create a new data frame
  joined_dat = left_join(dat_file_fr, annual_means, by = "yr")
  
  #calculating z_score for each variable and placing in new columns in the data frame
  variables = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip", "ppfd_in", "ppfd_out")
  for (variable in variables){
    st_mean_col = paste0("st_mn_", variable)
    z_score_col = paste0("z_score_", variable)
    
    joined_dat = joined_dat%>%
      group_by(yr)%>%
      mutate(!!z_score_col := (.data[[variable]]-.data[[st_mean_col]])/.data[[st_mean_col]])
  }
  
  yr_yoi = filter(joined_dat, yr == yoi)
  
  yr_yoi$day = as.numeric(as.character(yr_yoi$day))
  z_score_vars = paste0("z_score_", variables)
  plot_daily_zscore_yoi = 
    lapply(z_score_vars, function(z_var) {
      yr_yoi = filter(joined_dat, yr == yoi)
    ggplot(data = yr_yoi, mapping = aes(x=day, y=!!sym(z_var), group=1))+
    geom_point()+
      geom_hline(yintercept = 0)+
    facet_wrap(~ mon)+
      scale_x_discrete(breaks = seq(0,31, by = 5))
    
    plot_daily_zscore_yoi})
}

#plotting only gpp and ppfd with daily data against monthly mean for one year of interest (yoi)
daily_gpp_ppfd_yoi = function(yoi, dat_file, time_stamp_col, date_format) {
 
      #loading libraries  
      library(lubridate)
      library(dplyr)
      
      #loading in data and manipulating timestamps  
      dat_file = read.csv(dat_file, header = TRUE, na.strings= "-9999", skip=0, sep = ",")
      dat_file[[time_stamp_col]] = date_format(dat_file[[time_stamp_col]])
      dat_file$day = format(dat_file[[time_stamp_col]], "%d")
      dat_file$Months = months(dat_file[[time_stamp_col]])
      dat_file$years = format(dat_file[[time_stamp_col]], "%Y")
      
      #creating a df with variables of interest for summer growing season
      dat_file_fr = (data.frame(yr = dat_file$years,
                                mon = dat_file$Months,
                                day = dat_file$day,
                                swc = dat_file$SWC_F_MDS_1,
                                gpp = dat_file$GPP_DT_VUT_25,
                                nee = dat_file$NEE_VUT_25,
                                temp_atmos = dat_file$TA_F,
                                temp_soil = dat_file$TS_F_MDS_1,
                                precip = dat_file$P_F,
                                ppfd_in = dat_file$PPFD_IN,
                                ppfd_out = dat_file$PPFD_OUT) )
      dat_file_fr = dat_file_fr[dat_file_fr$mon %in% c('July','August','September','October','November'),]
      
      
      #finding the annual mean for each variable
      columns = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip", "ppfd_in", "ppfd_out")
      annual_means = dat_file_fr %>%
        group_by(yr) %>%
        summarise(across(all_of(columns), mean, na.rm = TRUE, .names = "st_mn_{.col}")) 
      
      #joining annual and long-term annual averages to create a new data frame
      joined_dat = left_join(dat_file_fr, annual_means, by = "yr")
      
      #calculating z_score for each variable and placing in new columns in the data frame
      variables = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip", "ppfd_in", "ppfd_out")
      for (variable in variables){
        st_mean_col = paste0("st_mn_", variable)
        z_score_col = paste0("z_score_", variable)
        
        joined_dat = joined_dat%>%
          group_by(yr)%>%
          mutate(!!z_score_col := (.data[[variable]]-.data[[st_mean_col]])/.data[[st_mean_col]])
      }
      
    z_score_vars = paste0("z_score_", variables)
  plot_daily_zscore_yoi = 
    lapply(z_score_vars, function(z_var) {
      
      yr_yoi = filter(joined_dat, yr == yoi)
      
      yr_yoi$day = as.numeric(as.character(yr_yoi$day))
      plot_daily_zscore_yoi_gpp_ppfd = 
        ggplot(data = yr_yoi, 
               mapping = aes(day)) +
        geom_point(aes(y = z_score_gpp, color = "z_score_gpp"))+
        geom_point(aes(y = z_score_ppfd_in, color = "z_score_ppfd_in"))+
        facet_wrap(~mon)+
        geom_hline(yintercept = 0)+
        scale_x_continuous(breaks = seq(0,31, by = 5))
      
      plot_daily_zscore_yoi_gpp_ppfd  
      
      })
  plot_daily_zscore_yoi_gpp_ppfd  
  }
  
#plotting only gpp, ppfd, and swc with daily data against monthly mean for one year of interest (yoi)
daily_gpp_ppfd_swc_yoi = function(yoi, dat_file, time_stamp_col, date_format) {

  #loading libraries  
  library(lubridate)
  library(dplyr)
  
  #loading in data and manipulating timestamps  
  dat_file = read.csv(dat_file, header = TRUE, na.strings= "-9999", skip=0, sep = ",")
  dat_file[[time_stamp_col]] = date_format(dat_file[[time_stamp_col]])
  dat_file$day = format(dat_file[[time_stamp_col]], "%d")
  dat_file$Months = months(dat_file[[time_stamp_col]])
  dat_file$years = format(dat_file[[time_stamp_col]], "%Y")
  
  #creating a df with variables of interest for summer growing season
  dat_file_fr = (data.frame(yr = dat_file$years,
                            mon = dat_file$Months,
                            day = dat_file$day,
                            swc = dat_file$SWC_F_MDS_1,
                            gpp = dat_file$GPP_DT_VUT_25,
                            nee = dat_file$NEE_VUT_25,
                            temp_atmos = dat_file$TA_F,
                            temp_soil = dat_file$TS_F_MDS_1,
                            precip = dat_file$P_F,
                            ppfd_in = dat_file$PPFD_IN,
                            ppfd_out = dat_file$PPFD_OUT) )
  dat_file_fr = dat_file_fr[dat_file_fr$mon %in% c('July','August','September','October','November'),]
  
  
  #finding the annual mean for each variable
  columns = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip", "ppfd_in", "ppfd_out")
  annual_means = dat_file_fr %>%
    group_by(yr) %>%
    summarise(across(all_of(columns), mean, na.rm = TRUE, .names = "st_mn_{.col}")) 
  
  #joining annual and long-term annual averages to create a new data frame
  joined_dat = left_join(dat_file_fr, annual_means, by = "yr")
  
  #calculating z_score for each variable and placing in new columns in the data frame
  variables = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip", "ppfd_in", "ppfd_out")
  for (variable in variables){
    st_mean_col = paste0("st_mn_", variable)
    z_score_col = paste0("z_score_", variable)
    
    joined_dat = joined_dat%>%
      group_by(yr)%>%
      mutate(!!z_score_col := (.data[[variable]]-.data[[st_mean_col]])/.data[[st_mean_col]])
  }
  
       yr_yoi = filter(joined_dat, yr == yoi)
      
      yr_yoi$day = as.numeric(as.character(yr_yoi$day))
      plot_daily_zscore_yoi_4var = 
        ggplot(data = yr_yoi, 
               mapping = aes(day)) +
        geom_point(aes(y = z_score_gpp, color = "z_score_gpp")) +
        geom_point(aes(y = z_score_ppfd_in, color = "z_score_ppfd_in")) +
        geom_point(aes(y = z_score_swc, color = "z_score_swc")) +
        geom_point(aes(y = z_score_temp_atmos, color = "z_score_temp_atmos")) +
        facet_wrap(~mon)+
        geom_hline(yintercept = 0)+
        scale_x_continuous(breaks = seq(0,31, by = 5))
      
      plot_daily_zscore_yoi_4var  
      
 }


  

daily_gpp_ppfd_swc_yoi = function(yoi, dat_file, time_stamp_col, date_format) {
  
  #loading libraries  
  library(lubridate)
  library(dplyr)
  
  #loading in data and manipulating timestamps  
  dat_file = read.csv(dat_file, header = TRUE, na.strings= "-9999", skip=0, sep = ",")
  dat_file[[time_stamp_col]] = date_format(dat_file[[time_stamp_col]])
  dat_file$day = format(dat_file[[time_stamp_col]], "%d")
  dat_file$Months = months(dat_file[[time_stamp_col]])
  dat_file$years = format(dat_file[[time_stamp_col]], "%Y")
  
  #creating a df with variables of interest for summer growing season
  dat_file_fr = (data.frame(yr = dat_file$years,
                            mon = dat_file$Months,
                            day = dat_file$day,
                            swc = dat_file$SWC_F_MDS_1,
                            gpp = dat_file$GPP_DT_VUT_25,
                            nee = dat_file$NEE_VUT_25,
                            temp_atmos = dat_file$TA_F,
                            temp_soil = dat_file$TS_F_MDS_1,
                            precip = dat_file$P_F,
                            ppfd_in = dat_file$PPFD_IN,
                            ppfd_out = dat_file$PPFD_OUT) )
  dat_file_fr = dat_file_fr[dat_file_fr$mon %in% c('July','August','September','October','November'),]
  
  
  #finding the annual mean for each variable
  columns = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip", "ppfd_in", "ppfd_out")
  annual_means = dat_file_fr %>%
    group_by(yr) %>%
    summarise(across(all_of(columns), mean, na.rm = TRUE, .names = "st_mn_{.col}")) 
  
  #joining annual and long-term annual averages to create a new data frame
  joined_dat = left_join(dat_file_fr, annual_means, by = "yr")
  
  #calculating z_score for each variable and placing in new columns in the data frame
  variables = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip", "ppfd_in", "ppfd_out")
  for (variable in variables){
    st_mean_col = paste0("st_mn_", variable)
    z_score_col = paste0("z_score_", variable)
    
    joined_dat = joined_dat%>%
      group_by(yr)%>%
      mutate(!!z_score_col := (.data[[variable]]-.data[[st_mean_col]])/.data[[st_mean_col]])
  }
  
  yr_yoi = filter(joined_dat, yr == yoi)
  
  yr_yoi$day = as.numeric(as.character(yr_yoi$day))
  plot_daily_zscore_yoi_4var = 
    ggplot(data = yr_yoi, 
           mapping = aes(day)) +
    geom_line(aes(y = z_score_gpp, color = "z_score_gpp")) +
    geom_line(aes(y = z_score_ppfd_in, color = "z_score_ppfd_in")) +
    geom_line(aes(y = z_score_swc, color = "z_score_swc")) +
    geom_line(aes(y = z_score_temp_atmos, color = "z_score_temp_atmos")) +
    facet_wrap(~mon)+
    geom_hline(yintercept = 0)+
    scale_x_continuous(breaks = seq(0,31, by = 5))
  
  plot_daily_zscore_yoi_4var  
  
}

