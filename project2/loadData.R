## CDSS Exploratory Data Analysis
## Project 2 - Data Load function

## Written by: Michael Gregory
## Date: 23-Sep-2015

## Set some general variables
## Original data description at http://www3.epa.gov/ttn/chief/eiinformation.html
##For each year and for each type of PM source, the NEI records how many tons of PM2.5 were emitted from that 
## source over the course of the entire year. The data that you will use for this assignment are for 1999, 2002, 2005, and 2008.
##  and https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip

loadData <- function() {
        
        fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        trueFileMD5 <- "b5f11f80e171a7148029b7f367b3667d"
        
        ##setwd("~/Documents/School/coursera/data science/exploratory data analysis/project2/")
        baseDir <- getwd()
        
        zipFile <- paste(baseDir,"/exdata-data-NEI_data.zip", sep="")
        summaryFile <- paste(baseDir, "/summarySCC_PM25.rds", sep="")
        sccFile <- paste(baseDir, "/Source_Classification_Code.rds", sep="")
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
                NEI <- readRDS(summaryFile)
        }
        
        if(!file.exists(sccFile)) {
                cat(sprintf("Unzipping files %s \n\t ", sccFile))
                unzip(zipFile, files=sccFile)
        } else {
                cat(sprintf("Unzipped data file already exists. \n\t Using data file: %s \n for SCC Data.", sccFile))
                SCC <- readRDS(sccFile)
        }
        
}