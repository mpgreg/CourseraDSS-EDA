## CDSS Exploratory Data Analysis
## Project 1 - Plot 4

## Written by: Michael Gregory
## Date: 13-Sep-2015

## Set some general variables
## Original data description at http://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption
##  and http://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip

## Based on fork/clone from https://github.com/rdpeng/ExData_Plotting1

library(sqldf)

fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
trueFileMD5 <- "41f51806846b6b567b8ae701a300a3de"

##setwd("~/Documents/School/coursera/data science/exploratory data analysis/project1/ExData_Plotting1/")
baseDir <- getwd()

zipFile <- paste(baseDir,"/household_power_consumption.zip", sep="")
dataFile <- "household_power_consumption.txt"
dateDownloaded <- NULL

outputFile <- paste(baseDir,"/plot4.png", sep="")

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

if(!file.exists(dataFile)) {
        cat(sprintf("Unzipping file %s \n\t ", dataFile))
        unzip(zipFile, files=dataFile)
} else cat(sprintf("Unzipped data file already exists. \n\t Using data file: %s \n",dataFile))


## Load the data from the dates 2007-02-01 and 2007-02-02. 
cat(sprintf("Reading in dataset from file: \n\t%s\n", dataFile))
zipFileCon <- file(dataFile)
powerConsDF<-sqldf("select * from zipFileCon where Date='1/2/2007' or Date='2/2/2007'",file.format = list(header=TRUE,sep=";"))
close(zipFileCon)

##Alternative non-portable option
##powerConsDF <- read.table(pipe('grep "^[1-2]/2/2007\\|^Date" "household_power_consumption.txt"'), sep=";", na.strings="?", header=TRUE)

##  Convert the Date and Time variables to Date/Time classes
powerConsDF$Date <- as.Date(powerConsDF$Date,format='%d/%m/%Y')
powerConsDF$DateTime <- paste(powerConsDF$Date, powerConsDF$Time)
powerConsDF$DateTime<-strptime(powerConsDF$DateTime, format = "%Y-%m-%d %H:%M:%S",tz="UTC")

##Making Plots
##  Examine how household energy usage varies over a 2-day period in February, 2007. 

##Open png file 
cat(sprintf("Opening output file: \n\t%s\n", outputFile))
png(filename = outputFile, bg = "transparent")

##Create the image framing 2x2
par(mfrow = c(2,2))

##Insert top/left
plot(powerConsDF$DateTime, powerConsDF$Global_active_power, 
     ylab = "Global Active Power",
     xlab = "",
     type="l")

##Insert top/right
plot(powerConsDF$DateTime, powerConsDF$Voltage, 
     ylab = "Voltage",
     xlab = "datetime",
     type="l")

##Insert bottom/left
plot(powerConsDF$DateTime, powerConsDF$Sub_metering_1, 
     ylab = "Engergy sub metering", 
     xlab = "", 
     type="l", 
     col = "black")
lines(powerConsDF$DateTime, powerConsDF$Sub_metering_2, 
      type="l", 
      col = "red")
lines(powerConsDF$DateTime, powerConsDF$Sub_metering_3, 
      type="l", 
      col = "blue")
legend("topright", lty=1, 
       col = c("black", "red", "blue"), 
       legend = names(powerConsDF[7:9]),
       bty = "n")

##Insert bottom/right
plot(powerConsDF$DateTime, powerConsDF$Global_reactive_power, 
     ylab = "Global_reactive_power",
     xlab = "datetime",
     type="l")

##Save/close the image
dev.off()
