library(data.table)
library(dplyr)

readData <- function(fileName) {
        fileList <- dir(path='.', pattern=paste0('^',fileName,'$'))
        if( length(fileList) != 1) {
                stop(paste0('Cannot find ', fileName))
        }
        readRDS(fileName)
}

NEI <- readData('summarySCC_PM25.rds')

# Plot 1
x <- group_by(NEI, year) %>%
        summarise(pm25 = sum(Emissions)/1e6)

png('plot1.png')

plot(x,
     las  = 1,
     pch  = 19,
     type = 'b',
     main = expression('Total PM'[2.5]*' emissions in United States by year'),
     xlab = 'Year',
     ylab = expression('PM'[2.5]*' emissions (millions of tons)')
)

dev.off()