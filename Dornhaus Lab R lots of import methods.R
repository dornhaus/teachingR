# Anna Dornhaus & Lab
# R script for data analysis and figures for 
# just learning to do it in R

# Paper reference: NONE

### Libraries ------------------
#library(lme4) #needed for GLMMs
#library(lmerTest) #needed for obtaining p-values in lmm
#library(emmeans) #post-hoc comparisons

# General data handling
library(tidyverse) # for bind.rows
# File access
library(readxl)
library(googlesheets4) # for working with Google Sheets
library(googledrive) # for drive_download
# Colors
library(scales)  # for number_format & color scales
library(viridis)
# Making output tables for statistical models
library(sjPlot) # for linear models output tables



#### ACTUALLY READ THE INSTRUCTIONS !!! --------------------------

# IMPORT ----------------------------------
# This section has several methods for importing. Don't feel like you need to master
# all of them at once - just pick whichever one is most important for you right 
# now. 

# Setting directory path correctly! -----------------------
# A common problem when reading any files is that you must make sure
# that R uses the folder you want as 'working directory'. You can
# pick the working directory from the 'Session' menu above; or 
# you can write the path here in the code, e.g.:
setwd("C:/Users/dornh/Dropbox/Github/teachingR")
# But note that this is a little problematic as this detailed path
# would never work on someone else's computer. Instead,
# you can also use
setwd("../teachingR")
# or similar ... the ".." means 'go up one directory level' and then
# it will go into the folder to the right of the "/". 

# Import from a local folder ------------------------
MyDataLocalFolder <- read.table("./example_data/bb col data.csv"
                     , header=T
                     , row.names=1
                     , sep = ","
                     , dec = "."
                     )

# Or directly from a Google Sheet: -----------------
# Note that you need to modify the path to match this one from the last /
# onwards!
MyDatafromGoogleSheets <- read.csv("https://docs.google.com/spreadsheets/d/1K2D2rH770iDfZzlwakHK89bwvoPqWDLaHbVNsanJFX8/gviz/tq?tqx=out:csv")
# Or an existing .csv file on Google Drive: ----------------
MyDatafromGoogleSheets <- read.csv("https://drive.google.com/uc?export=download&id=1woJYnpsfMBPCC3kPSErcydgDCduEKjzy")
# Note that in both cases you are not just pasting the entire link from your browser, you are modifying it to include 
# the file id and some other stuff. Pattern match here!!!!!!

# An .xlsx file from Google Drive is more complicated: ---------------
# You have to first download the file into a local file, then
# import the local file into R. 
googlepath <- "https://docs.google.com/spreadsheets/d/1sF5WnJs0uxeLKApn7_JzGJMozu6wgSPJ/edit"
tempxlsfile <- tempfile(fileext = ".xlsx")
drive_download(as_id(googlepath), path = tempxlsfile, overwrite = TRUE)
MyDatafromGoogleDrive  <- read_excel(tempxlsfile)
unlink(tempxlsfile)

# Or directly from github: ---------------------
MyDatafromGithub <- read.csv("https://raw.githubusercontent.com/shannonmcwaters/Directed-exploration/refs/heads/main/Maze%20Data%20Raw")

# Import a whole list of files that are local ----------------
# Note that this will only work if all the files have the same columns (and otherwise
# it is anyway doubtful that this would make sense)
# You may have to use setwd(..) first to make sure 
files <- (Sys.glob("./example_data/similardatasheets/*.csv"))
# Initiate a blank data frame
EnormousDataLocal <- data.frame()
# Read content of all files into a list
listOfDataframes <- lapply(files, 
                             function(x) {
                               read.table(x, 
                                          header = T,
                                          sep = ",",
                                          skip = 6
                               )
                             }
)
# Add all the rows from all the files together
EnormousDataLocal <- do.call("bind_rows", listOfDataframes)

# Import a whole list of xls files from Google Drive -------------------
# Headache! Saving as csv is better...
googledrivefolderfiles <- drive_ls(as_id("https://drive.google.com/drive/folders/1aZWvhJjgTH9QTrD9hV1LiPiOKFRxmH7x"))
files2 <- googledrivefolderfiles$id
names2 <- googledrivefolderfiles$name
files_local <- vector(mode = "character", length = length(files2))
dir.create(file.path("./", "temp"), showWarnings = FALSE)
for (i in 1:length(files2)) {
  # Put name in files_local
  files_local[i] <- paste("./temp/", names2[i])
  # Download file
  drive_download(files2[i], path = files_local[i], overwrite = TRUE)
}
EnormousData <- data.frame()
# Read content of all files into a list
listOfDataframes <- lapply(files_local, 
                           function(x) {
                             read_excel(x)
                           }
)
# Add all the rows from all the files together
EnormousData <- do.call("bind_rows", listOfDataframes)


