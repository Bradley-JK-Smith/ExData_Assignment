library(data.table)
library(dplyr)
library(ggplot2)

readData <- function(fileName) {
        fileList <- dir(path='.', pattern=paste0('^',fileName,'$'))
        if( length(fileList) != 1) {
                stop(paste0('Cannot find ', fileName))
        }
        readRDS(fileName)
}

NEI <- readData('summarySCC_PM25.rds')
#SCC <- readData('Source_Classification_Code.rds')

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

x <- filter(NEI, fips=='24510',type=='ON-ROAD') %>%
        group_by(year) %>%
        summarise(pm25 = sum(Emissions))      

png('plot5.png')

qplot(year,
      pm25,
      data = x,
      geom = c("point", "line"),
      ylim = c(0, 350),
      main = expression('Total PM'[2.5]*' emissions in Baltimore City from motor vehicles sources by year'),
      xlab = 'Year',
      ylab = expression('PM'[2.5]*' emissions (tons)')
)

dev.off()