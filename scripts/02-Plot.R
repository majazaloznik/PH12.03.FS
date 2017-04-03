###############################################################################
## plots
###############################################################################



## preliminaries
###############################################################################
load("data/imported.RData")
library(dplyr)
library(tidyr)


## functions 
###############################################################################

FunSubsetDF <- function(x) {
  working.df %>% 
    filter(Country..name == unique(working.df$Country..name)[x])
}

FunSubsetTFR <- function(x) {
  working.tfr %>% 
    filter(Country.or.area == unique(working.df$Country..name)[x])
}


FunPlot2 <- function(i, policy = "growth"){
    years <- c(1976, 1986, 1996, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2016)
  x <- FunSubsetDF(i)
  x.t <- FunSubsetTFR(i)
  min.max <- if (policy == "growth")  c(-3,7) else c(0,9)
  if (policy == "growth"){
    par(mar = c(1,4,1,1))
    plot(NA,
         xlim = c(min(years), max(years)),
         ylim = min.max,
         xlab = "",
         ylab = "",
         bty = "n",
         axes = FALSE,xaxs="r", yaxs="r")} else {
           par(mar = c(2,4,0,1))
           plot(NA,
                xlim = c(min(years), max(years)),
                ylim = min.max,
                xlab = "",
                ylab = "",
                bty = "n",
                axes = FALSE)
           if (i %in% c(1,9,12,22)) {axis(1, at = years[3:12])}else{
             axis(1, at = years)}
         }

  palette <- c("lightpink",
               "goldenrod2",
               "seashell4",
               "white",
               "darkolivegreen3")
  col <- if (policy == "growth") x$Policy.on.growth[x$year %in% years] else
    x$Policy.on.fertility.level[x$year %in% years]
  
  rect(years[1:11], rep(min.max[1],11),
       years[2:12], rep(min.max[2],11),
       border = "gray", lty = 3,
       col =    palette[col])
  rect(years[1:11], rep(min.max[1],11),
       years[2:12], rep(min.max[2],11),
       col = "white", density = 12, angle = 30, lwd = 3)
  if (policy == "growth") {
    lines(c(1976, 2016), c(0,0), col = "white", lwd = 3)
    points(x$year, x$growth.rate,  pch = 16)
  } else {
    lines(c(1976, 2016), c(2.05,2.05), col = "white", lwd = 2)
    points(x.t$TimeMid, x.t$DataValue,  pch = 21, cex = 1.3, bg = "white", lwd = 2)
  }
  if (i %in% c(1,9,12,22)) {axis(2, las = 2, pos = 1995)}else{
    axis(2, las = 2, pos = 1975 )}
  if (i %in% c(1,9,12,22)) {poz <- -17} else {poz <- 2}
  if (policy == "growth"){ mtext("PGR", side = 2, line = poz )}else{
    mtext("TFR", side = 2, line = poz )}
}

FunPlots <- function(x){
  par(mfrow = c(2,1), xpd = TRUE)
  FunPlot2(x, policy = "growth")
  FunPlot2(x, policy = "fertility")
}



# latin american and carribean
FunPlots(2)
dev.copy2eps(file="../figures/barbados.eps", height=6, width=10)
FunPlots(3)
dev.copy2eps(file="../figures/chile.eps", height=6, width=10)
FunPlots(5)
dev.copy2eps(file="../figures/cuba.eps", height=6, width=10)
FunPlots(24)
dev.copy2eps(file="../figures/uruguay.eps", height=6, width=10)


# ex soviet:
FunPlots(1)
dev.copy2eps(file="../figures/armenia.eps", height=6, width=10)
FunPlots(9)
dev.copy2eps(file="../figures/georgia.eps", height=6, width=10)
FunPlots(12)
dev.copy2eps(file="../figures/kazakhstan.eps", height=6, width=10)
FunPlots(22)
dev.copy2eps(file="../figures/turkmenistan.eps", height=6, width=10)



exsov <- c(1,9,12,22)
lapply(exsov, FunPlots)


# east asian
east <- c(4, 7, 15, 17, 19, 20)
lapply(east, FunPlots)

# arab and perisan
arab <- c(10, 13, 16, 18, 21, 23)
lapply(arab, FunPlots)

# Mediteranean
med <- c(6, 11)

lapply(med, FunPlots)
# Africa 
africa <- c(8,14)
lapply(africa, FunPlots)

