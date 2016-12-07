years <- c(1976, 1986, 1996, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015)


urls <- sapply(years, function(x) paste0("https://esa.un.org/poppolicy/WPPDatasets/",
              x, "_WPPDataset_AllVariables.xls"))

file.names <- sapply(years, function(x) paste0("data/wpp", years, ".xls"))

sapply(1:length(years), function(x) 
       download.file(urls[x], destfile = file.names[x], mode = "wb"))
