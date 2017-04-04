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
       border = "white", lty = 3,
       col =    palette[col])
  rect(years[1:11], rep(min.max[1],11),
       years[2:12], rep(min.max[2],11),
       col = "white", density = 12, angle = 30, lwd = 3)
  if (policy == "growth") {
    lines(c(1976, 2016), c(0,0), col = "white", lwd = 3)
    points(x$year, x$growth.rate,  pch = 16, cex = 1.3)
  } else {
    lines(c(1976, 2016), c(2.1,2.1), col = "white", lwd = 3)
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


## plots
###############################################################################

height = 5.5 
width = 10 
# latin american and carribean
FunPlots(2)
dev.copy2eps(file="../figures/barbados.eps", height=height, width=width)
FunPlots(3)
dev.copy2eps(file="../figures/chile.eps", height=height, width=width)
FunPlots(5)
dev.copy2eps(file="../figures/cuba.eps", height=height, width=width)
FunPlots(24)
dev.copy2eps(file="../figures/uruguay.eps", height=height, width=width)

# ex soviet:
FunPlots(1)
dev.copy2eps(file="../figures/armenia.eps", height=height, width=width)
FunPlots(9)
dev.copy2eps(file="../figures/georgia.eps", height=height, width=width)
FunPlots(12)
dev.copy2eps(file="../figures/kazakhstan.eps", height=height, width=width)
FunPlots(22)
dev.copy2eps(file="../figures/turkmenistan.eps", height=height, width=width)

# arab and perisan
FunPlots(10)
dev.copy2eps(file="../figures/iran.eps", height=height, width=width)
FunPlots(13)
dev.copy2eps(file="../figures/kuwait.eps", height=height, width=width)
FunPlots(16)
dev.copy2eps(file="../figures/qatar.eps", height=height, width=width)
FunPlots(18)
dev.copy2eps(file="../figures/saudi.arabia.eps", height=height, width=width)
FunPlots(21)
dev.copy2eps(file="../figures/turkey.eps", height=height, width=width)
FunPlots(23)
dev.copy2eps(file="../figures/uae.eps", height=height, width=width)

# east asian
FunPlots(4)
dev.copy2eps(file="../figures/china.eps", height=height, width=width)
FunPlots(7)
dev.copy2eps(file="../figures/dprk.eps", height=height, width=width)
FunPlots(15)
dev.copy2eps(file="../figures/mongolia.eps", height=height, width=width)
FunPlots(17)
dev.copy2eps(file="../figures/korea.eps", height=height, width=width)
FunPlots(19)
dev.copy2eps(file="../figures/singapore.eps", height=height, width=width)
FunPlots(20)
dev.copy2eps(file="../figures/thailand.eps", height=height, width=width)

# med
FunPlots(6)
dev.copy2eps(file="../figures/cyprus.eps", height=height, width=width)
FunPlots(11)
dev.copy2eps(file="../figures/israel.eps", height=height, width=width)

# africa
FunPlots(8)
dev.copy2eps(file="../figures/gabon.eps", height=height, width=width)
FunPlots(14)
dev.copy2eps(file="../figures/mauritius.eps", height=height, width=width)

## LEGEND
#################################
palette <- c(  "seashell4",
               "lightpink",
             "goldenrod2",
             "darkolivegreen3")


par(mfrow = c(1,1))
par(mar = c(15,1,1,20))
plot(0,0, ylim = c(0,1), xlim = c(0,4),type = "n", bty = "n", axes = FALSE, xlab = "", ylab = "")
rect(0:3, rep(0, 4), 1:4,rep(1,4),
     col =    palette, border = "white")
rect(0:3, rep(0, 4), 1:4,rep(1,4),
     col = "white", density = 12, angle = 30, lwd = 3)
text(letters[4:1],
     x = (0:3)+0.5, y = -0.1,  srt=90)
lines(c(0,4), c(0.5, 0.5), col = "white", lwd = 3)
points(seq(0.5,3.5, length = 10),rep(0.75, 10),  pch = 16, cex = 1.3)

points(seq(0.5,3.5, length = 10),rep(0.25, 10),  pch = 21, cex = 1.3, bg = "white", lwd = 2)

text(LETTERS[1:2],
     x = 4.3, y = c(0.25, 0.75))
dev.copy2eps(file="../figures/ledge.eps", height=height, width=width)

