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

x <- filter(NEI, fips=='24510') %>%
        group_by(year) %>%
        summarise(pm25 = sum(Emissions))

y <- filter(x, year==1999) %>%
        rename(pm25_1999 = pm25)

x <- mutate(x, pm25_delta = pm25 - y$pm25_1999)

png('plot2.png')

plot(x$year,
     x$pm25_delta,
     las  = 1,
     pch  = 19,
     type = 'b',
     main = expression('Annual PM'[2.5]*' emissions in Baltimore City baselined at 1999'),
     xlab = 'Year',
     ylab = expression('PM'[2.5]*' emissions (tons)')
)

dev.off()