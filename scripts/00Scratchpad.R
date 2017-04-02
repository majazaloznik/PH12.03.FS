
full <- read.table("data/complete.csv", stringsAsFactors = TRUE)


library(dplyr)
library(tidyr)

working.df

FunSubset <- function(x) {
  working.df %>% 
    filter(Country.Name == selection[x])
}

FunPlot <- function(x) {
  plot(x$year,
       x$growth.rate, type = "b",
       col=x$Policy.on.fertility.level,
       ylim = min.max,
       main = paste("Policy on fertility level in", unique(x$Country..name)),
       xlab = "Year",
       ylab = "Population Growth Rate")
}



years <- c(1976, 1986, 1996, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2016)


FunPlot2 <- function(x, policy = "growth"){
  min.max <- c(-3,7)
  #min.max <- ifelse(policy == "growth", c(-3,7), c(0,8))
  plot(NA,
       xlim = c(min(years), max(years)),
       ylim = min.max,
       xlab = "Year",
       ylab = "Population Growth Rate",
       bty = "n",
       axes = FALSE,
       main = unique(x$Country.Name))
  axis(1, at = years)
  palette <- c("violetred3",
               "goldenrod2",
               "seashell4",
               "darkolivegreen3")
  col <- if (policy == "growth") x$Policy.on.growth[x$year %in% years] else
    x$Policy.on.fertility.level[x$year %in% years]

  rect(years[1:11], rep(min.max[1],11),
       years[2:12], rep(min.max[2],11),
       border = "gray", lty = 3,
       col =    palette[col])
  rect(years[1:11], rep(min.max[1],11),
       years[2:12], rep(min.max[2],11),
       col = "white", density = 20, lwd = 2)
  points(x$year, x$growth.rate,       pch = 16)
}


FunPlot2(FunSubset(7), policy = "growth")
FunPlot2(FunSubset(7), policy = "fertility")
