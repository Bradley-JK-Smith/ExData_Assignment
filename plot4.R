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
SCC <- readData('Source_Classification_Code.rds')

# These 2 steps are effectively a merge between NEI & SCC
# Use this because merging directly that took a really long time
idx <- grepl('Fuel Comb.*Coal', SCC$EI.Sector)
idy <- NEI$SCC %in% SCC[idx,'SCC']

x <- group_by(NEI[idy,], year) %>%
        summarise(pm25 = sum(Emissions)/1e3)

png('plot4.png')

qplot(year,
      pm25,
      data = x,
      geom = c("point", "line"),
      ylim = c(0, 600),
      main = expression('Total PM'[2.5]*' emissions in United States from coal combustion sources by year'),
      xlab = 'Year',
      ylab = expression('PM'[2.5]*' emissions (thousands of tons)')
)

dev.off()