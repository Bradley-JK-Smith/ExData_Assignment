library(data.table)
library(dplyr)

NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")


# Plot 1
x <- group_by(NEI, year) %>%
        summarise(pm25 = sum(Emissions)/1e6)

plot(x,
     las  = 1,
     pch  = 19,
     type = 'b',
     main = 'Total PM2.5 emissions in United States by year',
     xlab = 'Year',
     ylab = 'Total PM2.5 emissions (millions tons)'
    )

# Plot 2

x <- group_by(NEI, year) %>%
        filter(fips=='24510') %>%
        summarise(pm25 = sum(Emissions)/1e3)

plot(x,
     las  = 1,
     pch  = 19,
     type = 'b',
     main = 'Total PM2.5 emissions in Baltimore City by year',
     xlab = 'Year',
     ylab = 'Total PM2.5 emissions (thousands tons)'
    )

# Plot 3
library(ggplot2)

x <- group_by(NEI, year, type) %>%
        filter(fips=='24510') %>%
        summarise(pm25 = sum(Emissions)/1e3)

# Facets helps split out the data
# DON'T use linear fit because the question is which
# type has increase. 
qplot(year,
      pm25,
      data   = x,
      facets = . ~ type,
      geom   = c("point", "line"),
      main   = 'Total PM2.5 Emissions in Baltimore City by year',
      ylab   = 'Total PM2.5 (thousand tons)'
      )

# Plot 4

idx <- grepl('combustion', SCC$SCC.Level.Four, ignore.case=T)
idy <- grepl('coal', SCC$SCC.Level.Four, ignore.case=T)
unique(SCC[idx&idy,'SCC.Level.Four'])

x <- group_by(NEI[idx&idy,], year) %>%
        summarise(pm25 = sum(Emissions)/1e3)

plot(x,
     las  = 1,
     pch  = 19,
     type = 'b',
     main = 'Total PM2.5 emissions in United States from coal combustion sources by year',
     xlab = 'Year',
     ylab = 'Total PM2.5 emissions (thousands tons)'
    )
  