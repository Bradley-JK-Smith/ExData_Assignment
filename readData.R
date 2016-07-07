# Function to read in the PM25 Data
#
# Inputs:
#         fileName - name of RDS file (without extension) in data directory 
#

readData <- function(fileName) {
        fullName <- paste0(fileName, '.rds')
        fileList <- dir(path='.', pattern=paste0('^',fullName,'$'))
        if( length(fileList) != 1) {
                stop(paste0('Cannot find ', fullName))
        }
        cat0('\nReading from', fullName)
        res <- readRDS(fullName)
        cat0('\nRows read in:', nrow(res))
        return(res)
}
