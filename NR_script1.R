#Lindsey Bell
#5/25/2023
#AmeriFlux FluxNet data: Niwot Ridge, CO
library(ggplot2)

dat_NR = read.csv("AMF_US-NR1_FLUXNET_SUBSET_DD_1998-2016_3-5.csv", header = TRUE, na.strings = "-9999", skip = 0, sep = ",")

 

#z score plots for swc, GPP, NEE, Atmos temp, Soil temp, precip, ppfd
workflow_8plots("AMF_US-NR1_FLUXNET_SUBSET_DD_1998-2016_3-5.csv", "TIMESTAMP", ymd)


dat_file = "AMF_US-NR1_FLUXNET_SUBSET_DD_1998-2016_3-5.csv"
time_stamp_col = "TIMESTAMP"
date_format = ymd

#loading in data and manipulating timestamps  
workflow_monavg("AMF_US-NR1_FLUXNET_SUBSET_DD_1998-2016_3-5.csv", "TIMESTAMP", ymd)


plot_ppfd_gpp = ggplot(data= annual_monthly_means,
                        mapping = aes(x=mn_gpp, y=mn_ppfd_in, group=1))+
  geom_point()+
  facet_wrap(~ mon)
plot_ppfd_gpp



#plotting graphs for each variable in one window 
par(mfrow = c(1,2))
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


