library(data.table)
library(dplyr)

NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")


# Plot 1
x <- group_by(NEI, year) %>%
        summarise(pm25 = sum(Emissions)/1e6)

plot(x,
     las=1,
     main='Total PM2.5 emissions in United States by year',
     xlab='Year',
     ylab='Total PM2.5 emissions (millions tons)'
    )

# Plot 2

x <- group_by(NEI, year) %>%
        filter(fips=='24510') %>%
        summarise(pm25 = sum(Emissions)/1e3)

plot(x,
     las=1,
     main='Total PM2.5 emissions in Baltimore City by year',
     xlab='Year',
     ylab='Total PM2.5 emissions (thousands tons)'
    )

# Plot 3
library(ggplot2)

x <- group_by(NEI, year, type) %>%
        filter(fips=='24510') %>%
        summarise(pm25 = sum(Emissions)/1e3)

# Facets helps split out the data
# DON'T use linear fit because the question is which
# type has increase. 
qplot(year, pm25, data=x,
      facets = . ~ type,
      color=type,
      geom=c("point", "line")
      )

,
      method="lm")
