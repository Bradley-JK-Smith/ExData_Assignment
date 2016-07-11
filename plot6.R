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

x <- filter(NEI, fips=='24510' | fips=='06037',type=='ON-ROAD') %>%
        group_by(year, fips) %>%
        summarise(pm25 = sum(Emissions))

y <- filter(x, year==1999) %>%
        rename(baseline_year = year) %>%
        rename(pm25_1999 = pm25)

z <- as.data.table(merge(x, y, by='fips')) %>%
        mutate(pm25_delta = pm25 - pm25_1999) %>%
        mutate(pm25_pc = 100*pm25_delta/pm25_1999)

z[z$fips=='06037', 'fips'] <- 'Los Angeles County'
z[z$fips=='24510', 'fips'] <- 'Baltimore City'

png('plot6.png')

qplot(year,
      pm25_pc,
      data   = z,
      facets = . ~ fips,
      geom   = c("point", "line"),
      main   = expression('% change in PM'[2.5]*' emissions compared to 1999 levels'),
      ylab   = 'Percentage'
)

dev.off()