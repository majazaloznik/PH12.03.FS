load("data/imported.RData")

library(dplyr)
library(tidyr)


FunSubsetDF <- function(x) {
  working.df %>% 
    filter(Country..name == unique(working.df$Country..name)[x])
}

FunSubsetTFR <- function(x) {
  working.tfr %>% 
    filter(Country.or.area == unique(working.df$Country..name)[x])
}

years <- c(1976, 1986, 1996, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2016)


FunPlot2 <- function(i, policy = "growth"){
  x <- FunSubsetDF(i)
  x.t <- FunSubsetTFR(i)
  min.max <- if (policy == "growth")  c(-3,7) else c(0,9)
  if (policy == "growth"){
    par(mar = c(2,3,3,1))
  plot(NA,
       xlim = c(min(years), max(years)),
       ylim = min.max,
       xlab = "Year",
       ylab = "",
       bty = "n",
       axes = FALSE,
       main = unique(x$Country..name))} else {
         par(mar = c(3,3,2,1))
         plot(NA,
              xlim = c(min(years), max(years)),
              ylim = min.max,
              xlab = "Year",
              ylab = "",
              bty = "n",
              axes = FALSE)
         axis(1, at = years)
       }

  axis(2, las = 2)
  palette <- c("violetred3",
               "goldenrod2",
               "seashell4",
               "blue",
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
  if (policy == "growth") {
    lines(c(1976, 2016), c(0,0), col = "white", lwd = 2)
    points(x$year, x$growth.rate,  pch = 16)
    } else {
      lines(c(1976, 2016), c(2.05,2.05), col = "white", lwd = 2)
      points(x.t$TimeMid, x.t$DataValue,  pch = 17)
  }
  
}

x = 16
par(mfrow = c(2,1), xpd = TRUE)
FunPlot2(x, policy = "growth")
FunPlot2(x, policy = "fertility")

unique(working.df$Country..name)

[1] "Armenia"                               "Barbados"                             
[3] "Chile"                                 "China"                                
[5] "Cuba"                                  "Cyprus"                               
[7] "Democratic People's Republic of Korea" "Gabon"                                
[9] "Georgia"                               "Iran (Islamic Republic of)"           
[11] "Israel"                                "Kazakhstan"                           
[13] "Kuwait"                                "Mauritius"                            
[15] "Mongolia"                              "Qatar"                                
[17] "Republic of Korea"                     "Saudi Arabia"                         
[19] "Singapore"                             "Thailand"                             
[21] "Turkey"                                "Turkmenistan"                         
[23] "United Arab Emirates"                  "Uruguay"  

