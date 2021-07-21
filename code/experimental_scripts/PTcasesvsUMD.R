library(dplyr)
library(BBmisc) # maybe for more compact normalization later
library(zoo) # to use rollmean
#library(naniar) # replace_with_na_at

dataumd <- read.csv("../../data/estimates-symptom-survey/PlotData/IR-estimate.csv")
dataumd$date <- as.Date(dataumd$date)

# Prune days
#for (i in seq(422,429))
#  dataumd$p_cli[i] <- NA

#plot(data$pct_cli_smooth)

dataccfr <- read.csv("../../data/estimates-ccfr-based/PlotData/IR-estimate.csv")
dataccfr$date <- as.Date(dataccfr$date)
pop <- dataccfr$population[1]

# Combine series by date and select fields of interest
joined <- dataccfr %>% dplyr::select(date, cases, cases_daily) %>% inner_join( dataumd %>% dplyr::select(date, p_cli, p_cli_smooth,p_cli_smooth_slope) %>% mutate(date=date+1), by="date")
#joined <- dataccfr %>% dplyr::select(date, cases, cases_daily) %>% inner_join( dataumd %>% dplyr::select(date, p_cli, p_cli_smooth,p_cli_smooth_slope) , by="date")



# Copy series to normalize
c <- rollmean(joined$cases,1)
#r <- joined$p_cli_smooth
#rr <- rollmean(joined$p_cli,7)
#rs <- joined$p_cli_smooth_slope
cli_ks <- ksmooth(joined$date,joined$p_cli, "normal", bandwidth = 30)
cases_ks <- ksmooth(joined$date,joined$cases, "normal", bandwidth = 20)
#rs[is.na(rs)] <- 0



#Normalize each to 0-1 scale
#c <- (c-min(c))/(max(c)-min(c))
#r <- (r-min(r))/(max(r)-min(r))
#rr <- (rr-min(rr))/(max(rr)-min(rr))
#rs <- (rs-min(rs))/(max(rs)-min(rs))
#rks <- (rks$y-min(rks$y))/(max(rks$y)-min(rks$y))
c <- normalize(c,method="range")
cli_ks <- normalize(cli_ks$y, method="range")
cases_ks <- normalize(cases_ks$y, method="range")

# Make 1st derivative
cli_ksd <- diff(cli_ks,lag=4)
cases_ksd <- diff(cases_ks,lag=4)
cli_ksd <- normalize(cli_ksd, method="range")
cases_ksd <- normalize(cases_ksd, method="range")

plot(c,col="black",type="p")
lines(cases_ksd,col="gray")
lines(cli_ksd,col="light blue")
#lines(rr,col="green")
lines(cli_ks,col="blue")
#lines(rksd,col="orange")
lines(cases_ks,col="black")

print(min(joined$date))
print(max(joined$date))
print(max(dataumd$date))

#plot(joined$date,joined$p_cli)
#lines(ksmooth(joined$date,joined$p_cli, "normal", bandwidth = 14), col = 2)


