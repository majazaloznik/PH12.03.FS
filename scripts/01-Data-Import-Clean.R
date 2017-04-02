###############################################################################
## preliminaries
###############################################################################
library(xlsx)
library(dplyr)
library(tidyr)
library(RCurl)
options(stringsAsFactors = FALSE)

## years of WPP
years <- c(1976, 1986, 1996, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015)

###############################################################################
#'  Download data --------DOWNLOADING - ONE OFF--------------------------------
###############################################################################
## 1. POLICY DATA
##
## list of urls and new file names
# urls <- paste0("https://esa.un.org/poppolicy/WPPDatasets/",
#                years, "_WPPDataset_AllVariables.xls")

file.names <- paste0("data/wpp",years , ".xls")

# ## download all 11 files - only once
# sapply(1:length(years), function(x)
#   download.file(urls[x], destfile = file.names[x], mode = "wb"))
# function for importing all 11 files
# but also removing missin grows where country == NA
FunImport <- function(x) {
  # imports each excel file
  assign(paste0("df.", x), xlsx::read.xlsx(paste0("data/wpp",x , ".xls"),
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
policy <- cbind(eval(parse(text = paste0("df.", years[1]))), year = years[1],  stringsAsFactors = FALSE)
for (i in 2:length(years)) {
  new <- cbind(eval(parse(text = paste0("df.", years[i]))), year = years[i])
  policy <- rbind(policy, new)
}

rm(df.1976, df.1986, df.1996, df.2001, df.2003, df.2005, df.2007, df.2009,
   df.2011, df.2013, df.2015, new, FunImport, i, years, file.names )


# ###############################################################################
## 1.5. regional codes and threeletter alpha codes 
## dowlnoad from this guy on github - haven't checked though
# url <- paste0("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-",
#               "Regional-Codes/master/all/all.csv")
# download.file(url, destfile = "data/regional.codes.csv")
# codes <- read.csv("data/regional.codes.csv", stringsAsFactors = FALSE)
# codes copy pasted from https://unstats.un.org/unsd/methodology/m49/overview/
codes <- read.csv("data/UNcodes.csv", stringsAsFactors = FALSE)

codes <- rename(codes, Country.code = M49.Code)
codes <- rename(codes, Development = Developed...Developing.Countries)
codes <- select(codes, Country.code, Region.Name, Sub.region.Name, ISO.alpha3.Code, Development)

# # add sudan pre split: 736
# codes <- rbind(codes, list(736, "Africa", "Northern Africa", "SDN"))
# # add Serbia and montenegro pre split: 891
# codes <- rbind(codes, list(891, "Europe", "Southern Europe", "SCG"))

# # merge with full policy table
full <- left_join(policy, codes)
rm(codes,  policy)

# # REPEAT SUDAN BEFORE 2011 AS 728 AND 729
# # take out a copy of pre 2011 sudan
# repeat.sudan <- filter(full, Country.code == 736) 
# # Make it regular Sudan
# repeat.sudan$Country.code <- rep(729,8)
# repeat.sudan$Country..name <- rep("Sudan",8)
# 
# # the same pre 2011 sudan in the table, change to south sudan
# full <-  mutate(full, Country..name = ifelse(Country.code == 736,  "South Sudan (Sudan until 2011)", Country..name))
# full <- mutate(full, Country.code=ifelse(Country..name == "South Sudan (Sudan until 2011)",  728,  Country.code))
# full <- mutate(full, sub.region=ifelse(Country..name == "South Sudan (Sudan until 2011)",  "Eastern Africa", sub.region))
# full <- mutate(full, alpha.3=ifelse(Country..name == "South Sudan (Sudan until 2011)",  "SSD", alpha.3))
# full <- rbind(full, repeat.sudan)


# ###############################################################################
## ADD PADDING FOR MISSING YEARS 

# first add all missing years - combo with Afghanistan
temp <- data.frame(Country..name = "Afghanistan", year = 1976:2015)
full <- full_join(full, temp)
rm(temp)

# now expand to get all country and year combos:
full <- tidyr::complete(full, Country..name, year)

# now order them correctly and within each year, fill missing 
full <- arrange(full, Country..name, year) %>%
  group_by(Country..name) %>%
  fill(Country.code, Policy.on.growth, Policy.on.fertility.level,
       Government.support.for.family.planning, Region.Name, Sub.region.Name, ISO.alpha3.Code, Development )


###############################################################################
## 2. POPULATION SIZE
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
rm(pop.size)

#  4.5 annual growth rates from world bank
## manual download
annual.growth.rate <- read.csv(file = paste0("data/Data_Extract_From_World",
                                             "_Development_Indicators/ffce9940-1ec3",
                                             "-495a-bb4a-ec5e4e007564_Data.csv"),
                               stringsAsFactors = FALSE)
annual.growth.rate <- gather(annual.growth.rate, year, growth.rate, 5:54)
annual.growth.rate <- mutate(annual.growth.rate, year = as.numeric(substr(year, 10,13)))
annual.growth.rate <- rename(annual.growth.rate, ISO.alpha3.Code = Country.Code)
annual.growth.rate <- select(annual.growth.rate, -1, -2)

# remove countries that are not in policy list and years before 1976
annual.growth.rate <- filter(annual.growth.rate, year >= 1976 &  year < 2016)


# 5. TFR rates
# 
# url <- paste0("http://www.un.org/en/development/desa/population/publications/",
#               "dataset/fertility/wfd2015/UNPD_WFD_2015_FERTILITY.xlsx")
# download.file(url, destfile = "data/wpp.total.fert.xlsx", mode = "wb")
# 
# # import data
library("openxlsx")
tfr <- read.xlsx("data/wpp.total.fert.xlsx", sheet = 3,
                      startRow= 3)


# merge
full <- left_join(full, annual.growth.rate)
rm(annual.growth.rate)
# make policies factors again
full[4:6] <- lapply(full[4:6] , factor)


## select subset - Developing countries that have a policy to Raise fert level in 2015
###############################################################################

full %>% 
  group_by(Country..name) %>% 
  filter(year == 2015,
         Policy.on.fertility.level == "Raise" | Policy.on.growth == "Raise",
         Development == "Developing")  %>% 
  filter(!is.na(Country.Name)) %>% 
  ungroup() %>% 
  select(Country..name) %>% unlist() -> selection

full %>% 
  filter(Country..name %in% selection) %>% 
  mutate(Policy.on.fertility.level = as.factor(Policy.on.fertility.level),
         Policy.on.growth = as.factor(Policy.on.growth))-> working.df

#rename DPRK koreas
tfr$Country.or.area[tfr$Country.or.area == "Dem. People's Republic of Korea"] <-
  "Democratic People's Republic of Korea"  


## select subset - of TFR data as well
###############################################################################
tfr %>%  filter(Country.or.area %in% selection) %>% 
  filter(Indicator == "TFR") -> working.tfr


save(working.df, working.tfr, file = "data/imported.RData")
