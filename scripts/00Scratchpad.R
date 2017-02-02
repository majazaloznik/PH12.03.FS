
library(dplyr)
library(tidyr)
filter(full, Country.code == 4)

filter(full, Country..name == "Iran (Islamic Republic of)")
plot(filter(full, Country..name == "Iraq")$year,
     filter(full, Country..name == "Iraq")$growth.rate, type = "b",
     col=filter(full, Country..name == "Iraq")$Policy.on.growth)
points(filter(full, Country..name == "Iraq")[[6]],
     filter(full, Country..name == "Iraq")[[10]],
     col=filter(full, Country..name == "Iraq")[[3]] )

unique(full[full$region == "Africa",][[3]])
unique(full$Policy.on.fertility.level)

x <- 854
plot(filter(full, Country.code == x)$year,
     filter(full, Country.code == x)$growth.rate, type = "b",
     col=filter(full, Country.code == x)$Policy.on.fertility.level,
     ylim = c(-3,7),
     main = paste("Policy.on.fertility.level in", full$Country..name[match(x,full$Country.code)]))

