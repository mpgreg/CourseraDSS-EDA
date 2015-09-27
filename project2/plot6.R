## CDSS Exploratory Data Analysis
## Project 2 - Plot 6

## Written by: Michael Gregory
## Date: 27-Sep-2015

## Original data description at http://www3.epa.gov/ttn/chief/eiinformation.html
##For each year and for each type of PM source, the NEI records how many tons of PM2.5 were emitted from that 
## source over the course of the entire year. The data that you will use for this assignment are for 1999, 2002, 2005, and 2008.
##  and https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip

##Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle 
##sources in Los Angeles County, California (fips == 06037). Which city has seen greater changes 
##over time in motor vehicle emissions?

library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)

outputFile <- "plot6.png"

##Set to "ALL" unless specifice value(s) are desired
desiredFIPS <- c("24510","06037")

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
        NEI <- readRDS(summaryFile)
} else {
        cat(sprintf("Unzipped data file already exists. \n\t Using data file: %s \n for NEI Summary Data.", summaryFile))
        NEI <- readRDS(summaryFile)
}

if(!file.exists(sccFile)) {
        cat(sprintf("Unzipping files %s \n\t ", sccFile))
        unzip(zipFile, files=sccFile)
        SCC <- readRDS(sccFile)
} else {
        cat(sprintf("Unzipped data file already exists. \n\t Using data file: %s \n for SCC Data.\n", sccFile))
        SCC <- readRDS(sccFile)
}

##creaete smaller dataset if possilbe
if(length(desiredFIPS) == 2) {
        cat(sprintf("Subsetting NEI data for FIPS: %s.\n", desiredFIPS))
        NEI <- subset(NEI, fips %in% desiredFIPS)
} else warning("Less than two FIPS codes specified. Nothing to compare.  Exiting!")

##Set the column classes correctly
NEI$fips <- as.factor(NEI$fips)
NEI$SCC <- as.factor(NEI$SCC)
NEI$Pollutant <- as.factor(NEI$Pollutant)
NEI$type <- as.factor(NEI$type)
NEI$year <- as.factor(NEI$year)

if(anyNA(NEI)) cat(sprintf("WARNING: NA missing values in summary Data.\n"))
if(anyNA(SCC)) cat(sprintf("WARNING: NA missing values in SCC Data.\n"))

## What fields should we search on?  List all fields that have "vehicle".
## for(i in as.numeric(1:ncol(SCC))) {print(names(SCC)[i]); print(unique(grep("vehicle", SCC[,i], value=TRUE, ignore.case = TRUE)))}

##Caputre a list of all motor vehicle sources.  Could also search EI.Sector for search but that doesn't include ie. motorcycles.
searchSCCs <- SCC[grepl("vehicle", SCC$SCC.Level.Two, ignore.case = TRUE),]$SCC

##Subset to just the desired rows
NEI <- subset(NEI, NEI$SCC %in% searchSCCs)

##calculate crosstabs and percentage change for each desiredFIPS
pChange<-list()
for(i in desiredFIPS)
{
        nameTemp <- i  ##paste("FIPS", i, sep=" ")
        
        ##calculate cross tabulation and save it as a matrix
        totalEmissions <- as.matrix(xtabs(Emissions ~ year, data = NEI, subset = NEI$fips==i))
        ##calculate year-over-year percentage changes
        pChange[[nameTemp]] <- (totalEmissions[-1,] - totalEmissions[-nrow(totalEmissions),]) / totalEmissions[-nrow(totalEmissions),]
}

pChange <- as.data.frame(pChange, optional = TRUE)
pChange <- cbind(Year = rownames(pChange), pChange)
pChange <- melt(pChange, id.vars='Year')
colnames(pChange) <- c("Year","FIPS", "Emissions")


##Open png file 
cat(sprintf("Opening output file: \n\t%s\n", outputFile))
png(filename = outputFile, bg = "transparent", height = 960, width=960)

print(ggplot(pChange, aes(Year, Emissions)) +   
        geom_bar(aes(fill = FIPS), position = "dodge", stat="identity") +
        ggtitle(paste("Year-over-Year Change in Motor Vehicle Emissions \n (1999 to 2008) in FIPS: ", toString(desiredFIPS), sep = " ")) +
        xlab("Year") + 
        ylab("Percent Change in Total Emissions (%)") +
        theme(plot.margin=unit(c(1,1,1,1), "cm")) + 
        geom_text(aes(label = sprintf("%.f%%", Emissions*100))))
        
        
##Save/close the image
dev.off()