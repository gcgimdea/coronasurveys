library(dplyr)
library(BBmisc) # maybe for more compact normalization later
library(zoo) # to use rollmean

dataumd <- read.csv("../../data/estimates-symptom-survey/PlotData/PT-estimate.csv")
dataumd$date <- as.Date(dataumd$date)

#plot(data$pct_cli_smooth)

dataccfr <- read.csv("../../data/estimates-ccfr-based/PlotData/PT-estimate.csv")
dataccfr$date <- as.Date(dataccfr$date)
pop <- dataccfr$population[1]

# Combine series by date and select fields of interest
joined <- dataccfr %>% dplyr::select(date, cases, cases_daily) %>% inner_join( dataumd %>% dplyr::select(date, p_cli, p_cli_smooth,p_cli_smooth_slope) %>% mutate(date=date+1), by="date")
#joined <- dataccfr %>% dplyr::select(date, cases, cases_daily) %>% inner_join( dataumd %>% dplyr::select(date, p_cli, p_cli_smooth,p_cli_smooth_slope) , by="date")


# Cooy series to normalize
c <- rollmean(joined$cases,7)
r <- joined$p_cli_smooth
rr <- rollmean(joined$p_cli,7)
rs <- joined$p_cli_smooth_slope
rs[is.na(rs)] <- 0

#Normalize each to 0-1 scale
c <- (c-min(c))/(max(c)-min(c))
r <- (r-min(r))/(max(r)-min(r))
rr <- (rr-min(rr))/(max(rr)-min(rr))
rs <- (rs-min(rs))/(max(rs)-min(rs))

plot(c,col="black")
lines(r,col="blue")
lines(rs,col="red")
lines(rr,col="green")

print(min(joined$date))
print(max(joined$date))
print(max(dataumd$date))


