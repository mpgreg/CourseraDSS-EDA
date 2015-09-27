## CDSS Exploratory Data Analysis
## Project 2 - Plot 3

## Written by: Michael Gregory
## Date: 25-Sep-2015

## Original data description at http://www3.epa.gov/ttn/chief/eiinformation.html
##For each year and for each type of PM source, the NEI records how many tons of PM2.5 were emitted from that 
## source over the course of the entire year. The data that you will use for this assignment are for 1999, 2002, 2005, and 2008.
##  and https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip

##Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources 
##have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the 
##ggplot2 plotting system to make a plot answer this question.

library(ggplot2)


outputFile <- "plot3.png"
desiredFIPS <- "24510"

fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
trueFileMD5 <- "b5f11f80e171a7148029b7f367b3667d"

##setwd("~/Documents/School/coursera/data science/exploratory data analysis/project2/")
baseDir <- getwd()

zipFile <- paste(baseDir,"/exdata-data-NEI_data.zip", sep="")
summaryFile <- "summarySCC_PM25.rds"
sccFile <- "Source_Classification_Code.rds"
dateDownloaded <- NULL

##If the file doesn't already exist in baseDir download it and check md5
if(!file.exists(zipFile)) {
        cat(sprintf("Downloading file %s \n\t to location %s.\n", fileURL, zipFile))
        download.file(fileURL, destfile=zipFile, method="curl",quiet=TRUE)
        dateDownloaded <- date()
} else cat(sprintf("Zipped data file already exists in specified location. \n\t Using File: %s \n",zipFile))

zipFileMD5 <- digest::digest(algo = "md5", file=zipFile)
if(zipFileMD5 != trueFileMD5) {
        warning("Downloaded file has changed from original source for this script.  
                        This script may not work as originally intended.")
}        

##Unzip the files and load the data frames
if(!file.exists(summaryFile)) {
        cat(sprintf("Unzipping files %s \n\t ", summaryFile))
        unzip(zipFile, files=summaryFile)
} else {
        cat(sprintf("Unzipped data file already exists. \n\t Using data file: %s \n for NEI Summary Data.", summaryFile))
}
NEI <- readRDS(summaryFile)

if(!file.exists(sccFile)) {
        cat(sprintf("Unzipping files %s \n\t ", sccFile))
        unzip(zipFile, files=sccFile)
} else {
        cat(sprintf("Unzipped data file already exists. \n\t Using data file: %s \n for SCC Data.\n", sccFile))
}
SCC <- readRDS(sccFile)

##Set the column classes correctly
NEI$fips <- as.factor(NEI$fips)
NEI$SCC <- as.factor(NEI$SCC)
NEI$Pollutant <- as.factor(NEI$Pollutant)
NEI$type <- as.factor(NEI$type)
NEI$year <- as.factor(NEI$year)

if(anyNA(NEI)) cat(sprintf("WARNING: NA missing values in summary Data.\n"))
if(anyNA(SCC)) cat(sprintf("WARNING: NA missing values in summary Data.\n"))


##Open png file 
cat(sprintf("Opening output file: \n\t%s\n", outputFile))
png(filename = outputFile, bg = "transparent", height = 960)

g <- ggplot(subset(NEI, fips==desiredFIPS), aes(year,Emissions)) + 
        facet_grid(type ~.) + 
        geom_bar(stat = 'identity') + 
        ggtitle(as.character(sprintf("Total Emissions by Year for FIPS %s for each collector type.", desiredFIPS))) +
        xlab("Year") + 
        ylab("Total Emissions(Tons)")
print(g)

##Save/close the image
dev.off()
