##
library(xlsx)
library(dplyr)
library(tidyr)
library(RCurl)
## years of WPP
years <- c(1976, 1986, 1996, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015)

###############################################################################
# Download data --------DOWNLOADING - ONE OFF----------------------------------
###############################################################################
# 1. POLICY DATA
# ## 
# ## list of urls and new file names
# urls <- paste0("https://esa.un.org/poppolicy/WPPDatasets/",
#                years, "_WPPDataset_AllVariables.xls")
# 
# file.names <- paste0("data/wpp",years , ".xls")
# 
# ## download all 11 files
# sapply(1:length(years), function(x) 
#   download.file(urls[x], destfile = file.names[x], mode = "wb"))
# function for importing all 11 files 
# but also removing missin grows where country == NA
FunImport <- function(x) {
  # imports each excel file
  assign(paste0("df.", x), read.xlsx(paste0("data/wpp",x , ".xls"), 
                                     sheetIndex = 1, header = TRUE, 
                                     startRow= 2), envir = .GlobalEnv)
  # remove extra rows we don't need
  assign(paste0("df.", x), eval(parse(text = paste0("df.", x)))[!is.na(eval(
    parse(text = paste0("df.", x)))$Country.code),], 
    envir = .GlobalEnv)
  
  # rename columns we don't need
  assign(paste0("df.", x), select(eval(parse(text = paste0("df.", x))), 
                                  Country..name, Country.code, Policy.on.growth,
                                  Policy.on.fertility.level,
                                  Government.support.for.family.planning),   
         envir = .GlobalEnv)
  return(NULL)
}

# import all 11 files as dataframes
sapply(years, function(x) FunImport(x))       

# merge together all 11 Policy tables and rename columns so the year is in the name
full <- cbind(eval(parse(text = paste0("df.", years[1]))), year = years[1])
for (i in 2:length(years)) {
  new <- cbind(eval(parse(text = paste0("df.", years[i]))), year = years[i])
  full <- rbind(full, new)
}
rm(df.1976, df.1986, df.1996, df.2001, df.2003, df.2005, df.2007, df.2009, 
   df.2011, df.2013, df.2015, new, FunImport )

###############################################################################
##  2. POPULATION SIZE 
## UN POpulaiton division
## Total Population - Both Sexes. De facto population in a country, area or 
## region as of 1 July of the year indicated. Figures are presented in thousands.
# url <- paste0("https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20",
#                 "(Standard)/EXCEL_FILES/1_Population/WPP2015_POP_F01_1_",
#                 "TOTAL_POPULATION_BOTH_SEXES.XLS")
# download.file(url, destfile = "data/wpp.total.pop.xls", mode = "wb")

# import data
pop.size <- read.xlsx("data/wpp.total.pop.xls", sheetIndex = 1, header = TRUE, 
          startRow= 17) 
# tidy up 
pop.size <- gather(pop.size, year, population, 6:71)
pop.size$year <- as.numeric(gsub("^.*?X","", pop.size$year))
pop.size <- select(pop.size, -Variant, - Notes, -Major.area..region..country.or.area.., -Index)
# merge with full policy table
full <- left_join(full, pop.size)

###############################################################################
##  3. NATURAL INCREASE RATE 
## UN POpulaiton division
## Crude birth rate minus the crude death rate. Represents the portion of population growth 
# (or decline) determined exclusively by births and deaths. It is expressed per 1,000 population annually.
url <- paste0("https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20(Standard)",
              "/EXCEL_FILES/1_Population/WPP2015_POP_F03_RATE_OF_NATURAL_INCREASE.XLS")
download.file(url, destfile = "data/wpp.nat.inc.rate.xls", mode = "wb")

# import data
nat.inc.rate <- read.xlsx("data/wpp.nat.inc.rate.xls", sheetIndex = 1, header = TRUE, 
                      startRow= 17) 
nat.inc.rate <- gather(nat.inc.rate, period, nat.increase.rate, 6:18)

# add annual rows for 5 year periods
nat.inc.rate <- mutate(nat.inc.rate, year1 = as.numeric(substr(period, 2,5))+ 1,
                       year2 = year1 + 1, year3 = year2+1, year4 = year3+1,
                       year5 = as.numeric(substr(period, 7,10)))
nat.inc.rate <- gather(nat.inc.rate, key, year, 8:12)

#remove everythign that is unnecessary
nat.inc.rate <- select(nat.inc.rate, c(5,7,9))

# merge with full policy table
full <- left_join(full, nat.inc.rate)

###############################################################################
##  4. NATURAL INCREASE RATE 
## UN POpulaiton division
## Average exponential rate of growth of the population over a given period.  It is 
## calculated as ln(Pt/P0)/t where t is the length of the period. It is 
## expressed as a percentage.
url <- paste0("https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20(Standard)",
              "/EXCEL_FILES/1_Population/WPP2015_POP_F02_POPULATION_GROWTH_RATE.XLS
")
download.file(url, destfile = "data/wpp.growth.rate.xls", mode = "wb")

# import data
growth.rate <- read.xlsx("data/wpp.growth.rate.xls", sheetIndex = 1, header = TRUE, 
                          startRow= 17) 
growth.rate <- gather(growth.rate, period, growth.rate, 6:18)

# add annual rows for 5 year periods
growth.rate <- mutate(growth.rate, year1 = as.numeric(substr(period, 2,5))+ 1,
                       year2 = year1 + 1, year3 = year2+1, year4 = year3+1,
                       year5 = as.numeric(substr(period, 7,10)))
growth.rate <- gather(growth.rate, key, year, 8:12)

#remove everythign that is unnecessary
growth.rate <- select(growth.rate, c(5,7,9))

# merge with full policy table
full <- left_join(full, growth.rate)




###############################################################################
## 5. regional codes 
## dowlnoad 
url <- paste0("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-",
              "Regional-Codes/master/all/all.csv")
download.file(url, destfile = "data/regional.codes.csv")
codes <- read.csv("data/regional.codes.csv")
codes <- rename(codes, Country.code = country.code)
codes <- select(codes, Country.code, region, sub.region)

# merge with full policy table
full <- left_join(full, codes)

write.table(full, file = "data/complete.csv")

filter(full, region == "Africa")
