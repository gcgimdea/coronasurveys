library(dplyr)
library(readxl)

country_codes_file <- "../data/common-data/wikipedia-iso-country-codes.xlsx"
aggregates_path <- "../data/aggregate/"
ranking_path <- "../data/participation/"

# country_codes_file <- "../coronasurveys/data/common-data/wikipedia-iso-country-codes.xlsx"
# aggregates_path <- "../coronasurveys/data/aggregate/"
# ranking_path <- "./participation/"

compute_ranking <- function(country_geoid){
  cat(country_geoid, "\n")
  file_name <- paste0(aggregates_path, country_geoid, "-aggregate.csv")
  if (file.exists(file_name)) {
    dt <- read.csv(file_name, as.is = T, encoding = "UTF-8") %>% select("Timestamp","ISO.3166.2","Cookie") %>% 
      filter(Timestamp > "2019/12/31") %>%  filter(Cookie != "")
    if (nrow(dt)>0) {
    dt$Timestamp<-as.Date(dt$Timestamp)
    dt <- unique.data.frame(dt)
    dt <- dt %>% group_by(Cookie) %>% summarise(Count=n()) %>% arrange(desc(Count))
    dt = subset(dt, select = c(Cookie,Count) )
    write.csv(dt,paste0(ranking_path, country_geoid, "-ranking.csv"),row.names = FALSE)
    }
    }
}

data_country_code <- read_excel(country_codes_file)
names(data_country_code) <- c("English.short.name.lower.case", "Alpha.2.code",
                              "Alpha.3.code", "Numeric.code", "ISO.3166.2")
all_geo_ids <- unique(data_country_code$Alpha.2.code)
go <- sapply(all_geo_ids, compute_ranking)

