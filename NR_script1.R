#Lindsey Bell
#5/25/2023
#AmeriFlux FluxNet data: Niwot Ridge, CO

#for function use: 
dat_file = "AMF_US-NR1_FLUXNET_SUBSET_DD_1998-2016_3-5.csv"
time_stamp_col = "TIMESTAMP"
date_format = ymd

#Libraries:
library(ggplot2)

dat_NR = read.csv("AMF_US-NR1_FLUXNET_SUBSET_DD_1998-2016_3-5.csv", header = TRUE, na.strings = "-9999", skip = 0, sep = ",")

 

#z score plots for swc, GPP, NEE, Atmos temp, Soil temp, precip, ppfd
workflow_8plots("AMF_US-NR1_FLUXNET_SUBSET_DD_1998-2016_3-5.csv", "TIMESTAMP", ymd)


#loading in data and manipulating timestamps  
workflow_monavg("AMF_US-NR1_FLUXNET_SUBSET_DD_1998-2016_3-5.csv", "TIMESTAMP", ymd)

#plotting ppfd and gpp (using monthly means for each month in the growing season for all years)- very linear 
plot_ppfd_gpp = ggplot(data= annual_monthly_means,
                        mapping = aes(x=mn_gpp, y=mn_ppfd_in, group=1))+
  geom_point()
plot_ppfd_gpp





daily_gpp_ppfd_swc_yoi("2014", "AMF_US-NR1_FLUXNET_SUBSET_DD_1998-2016_3-5.csv", "TIMESTAMP", ymd)

daily_gpp_ppfd_swc_yoi("2008", "AMF_US-NR1_FLUXNET_SUBSET_DD_1998-2016_3-5.csv", "TIMESTAMP", ymd)
