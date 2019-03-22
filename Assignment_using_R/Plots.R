library(readr)
library(ggplot2)

#1.1

dub_emp_trend <- read_delim('dublin employment trends.txt', delim = ':')
View(dub_emp_trend)
qplot(Time, Employment, data = dub_emp_trend, xlab = "Quarterly Figures", ylab = "Trend", geom = 'line', 
      main = "Dublin Employment Trends Per Sector: 2006 - 2016") + facet_grid(. ~ Sector)

#1.2
dub_prop_trend <- read_tsv('dublin property trends.txt')
View(dub_prop_trend)
qplot(Time, Trend, data = dub_prop_trend, colour = Category, geom = 'line', 
      main = "Dublin Property Trends: 2007 - 2016")

