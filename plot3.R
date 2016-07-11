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

x <- filter(NEI, fips=='24510') %>%
        group_by(year, type) %>%
        summarise(pm25 = sum(Emissions))

y <- filter(x, year==1999) %>%
        rename(baseline_year = year) %>%
        rename(pm25_1999 = pm25)

z <- as.data.table(merge(x, y, by='type')) %>%
        mutate(pm25_delta = pm25 - pm25_1999)

# Facets helps split out the data
# DON'T use linear fit because the question is which
# type has increase. 

png('plot3.png')

qplot(year,
      pm25_delta,
      data   = z,
      facets = . ~ type,
      geom   = c("point", "line"),
      main   = expression('Annual PM'[2.5]*' emissions in Baltimore City by type baselined at 1999'),
      ylab   = expression('PM'[2.5]*' (tons)')
)

dev.off()