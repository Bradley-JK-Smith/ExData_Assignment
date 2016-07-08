source('sourceList.R')

NEI <- readData('summarySCC_PM25')
SCC <- readData('Source_Classification_Code')

# Plot 1
x <- group_by(NEI, year) %>%
        summarise(pm25 = sum(Emissions)/1e6)

plot(x,
     las  = 1,
     pch  = 19,
     type = 'b',
     main = expression('Total PM'[2.5]*' emissions in United States by year')),
     xlab = 'Year',
     ylab = expression('Total PM'[2.5]*' emissions (millions tons))'
    )

# Plot 2

x <- group_by(NEI, year) %>%
        filter(fips=='24510') %>%
        summarise(pm25 = sum(Emissions)/1e3)

plot(x,
     las  = 1,
     pch  = 19,
     type = 'b',
     main = expression('Total PM'[2.5]*' emissions in Baltimore City by year'),
     xlab = 'Year',
     ylab = expression('Total PM'[2.5]*' emissions (thousands tons)')
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
      main   = expression('Total PM'[2.5]*' emissions in Baltimore City by year'),
      ylab   = expression('Total PM'[2.5]*' (thousand tons)')
      )

# Plot 4

# Need to check these
# Also used for Plot 5

ids <- grepl('Fuel Comb.*Coal', SCC$EI.Sector)
idx <- unique(SCC[ids,'SCC'])

idy <- NEI$SCC %in% idx

x <- group_by(NEI[idy,], year) %>%
        summarise(pm25 = sum(Emissions)/1e3)


plot(x,
     las  = 1,
     pch  = 19,
     type = 'b',
     main = expression('Total PM'[2.5]*' emissions in United States from coal combustion sources by year'),
     xlab = 'Year',
     ylab = expression('Total PM'[2.5]*' emissions (thousands tons)')
    )
  
# Plot 5

# Need to get 'Motor Vehicles' subset

# First attempt
# Use grepl('On-Road.*Vehicles', SCC$EI.Sector) which gives 1138 SCC categories

# Second attempt
# Use SCC$Data.category == 'Onroad' which gives 1137 SCC categories
# (Only difference with first attempt is a border crossings entry)
# Matching back to NEI data gives 3,183,599 rows

# Third attempt
# Use NEI$type == 'ON-ROAD'
# This gives 3,183,602 rows (ie. an extra 3) which is the same result
# as using the first attempt and matching back

x <- group_by(NEI[idy,], year) %>%
        filter(fips=='24510',type=='ON-ROAD') %>%
        summarise(pm25 = sum(Emissions))      

plot(x,
     las  = 1,
     pch  = 19,
     type = 'b',
     main = expression('Total PM'[2.5]*' emissions in Baltimore City from motor vehicles sources by year'),
     xlab = 'Year',
     ylab = expression('Total PM'[2.5]*' emissions (tons)')
)

# Plot 6

library(ggplot2)

x <- group_by(NEI, year, fips) %>%
        filter(fips=='24510' | fips=='06037', ,type=='ON-ROAD') %>%
        summarise(pm25 = sum(Emissions)/1e3)

# Facets helps split out the data
# DON'T use linear fit because the question is which
# type has increase. 
qplot(year,
      pm25,
      data   = x,
      facets = . ~ fips,
      geom   = c("point", "line"),
      main   = expression('Total PM'[2.5]*' emissions by year',
      ylab   = expression('Total PM'[2.5]*' emissions (thousand tons)'
      )
