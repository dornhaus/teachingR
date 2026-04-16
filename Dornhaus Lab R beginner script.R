
## ALWAYS START BY MAKING A HEADER THAT CONTAINS YOUR NAME AND THE YEAR AT 
## MINIUMUM

# Anna Dornhaus & Lab
# R script for data analysis and figures for 
# just learning to do it in R
# 2026

# Paper reference: NONE

### INTRO ----------------------

# Note how you can collapse parts of the code by clicking on the small triangles
# next to the line numbers. 
# Do this now for all of them except the Intro if you like. 
# This code starts with libraries and graphics setup, which are sections you 
# should have in every script, in addition to the header above. 
# You may initially have the libraries and graphics setup sections empty, and 
# populate them as your code gets more complicated. Your header should never be
# empty!! Also, make sure immediately that your code is synced to github, and 
# don't have several undistinguished versions lying around. 


### CODE OUTLINE --------------------
# You pretty much always want this outline:

# Libraries used
# Color and parameter settings
# Importing your data
# Data wrangling
# Defining questions/sections of your code
# Simulating data
# Analysis
# Graphing/illustrating data

### Libraries ------------------

# Overall rule: only add the ones you know you need!

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


### Color and parameter settings -----------------------------

# Think about the types of things you might want to illustrate, e.g. 
# treatments or factor levels that you might want to represent. 

# I really like the viridis package for color sets - 
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html 
# The examples below use this.

# But I also just heard about the Wes Anderson package - 
# https://github.com/karthik/wesanderson 
# Sounds awesome. Whichever one you use, pre-made color palettes are usually
# better than you picking your own colors, especially when you have more than 
# two. 

# For example:
no_categories <- 15
## Colorpalette
twogroupcolors <- c("#5a9a8f", "#7b6ea8")
threegroupcolors <- viridis(3)
severalcategories_colors <- magma(no_categories)

## Similar to colors, it often makes sense to define some other things
## universally for all your plots, e.g. margins, where the sample sizes
## are plotted, y-axis range. This depends on your figures though.
# For example:
y_max <- 6
y_min <- 0
N_y_offset <- 0.95 # This puts sample size numbers 5% below max, for example



### IMPORT ----------------------------------

# There is a separate script called
# Dornhaus Lab R lots of import methods.R
# that lists several different methods. Read it if the basic method here doesn't work
# for you!

# Ideally I want everyone to import directly from Google Sheets, or 
# have their data in their github folder. In the second case, if your script 
# is also in your github folder and automatically synched to github, 
# then your data should automatically always be available locally. 

# A common problem when reading any files is that you must make sure
# that R uses the folder you want as 'working directory'. You can
# pick the working directory from the 'Session' menu above; or 
# you can write the path here in the code, e.g.:
setwd("../teachingR")
# The ".." means 'go up one directory level' and then
# it will go into the folder to the right of the "/". 
# This will work if your teachingR folder is in the github folder along with your
# own script's folder. 
MyDataLocalFolder <- read.table("./example_data/bb col data.csv"
                                , header=T
                                , row.names=1
                                , sep = ","
                                , dec = "."
)

# Or directly from a Google Sheet:
# Note that you need to modify the path to match this one from the last /
# onwards!
MyDatafromGoogleSheets <- read.csv("https://docs.google.com/spreadsheets/d/1K2D2rH770iDfZzlwakHK89bwvoPqWDLaHbVNsanJFX8/gviz/tq?tqx=out:csv")

### DATA WRANGLING -------------------------------

# Typically there is some modifying of the original data sheet necessary, or 
# even extensive calculations. Do those here. 

### Defining questions/sections of your code -----------------

# For example, you may organize your script around 'Fig 1' and 'Fig 2', 
# or some different questions you want to answer, and separate these sections
# with 
##################################################################s

### ANALYSIS ------------------------

# Analysis methods can vary depending on your data, question, and philosophy.
# I'd like us to generally adopt this workflow:
# - Define a generative model and simulate data
# - Make sure simulated and cleaned real data have identical format 
#   (e.g. column headers)
# - Define one or more statistical hypotheses, i.e. models that reflect your 
#   hypotheses. One should match how you simulated your data qualitatively. 
# - Fit the model(s) first to the simulated data, and compare the fitted
#   parameters to what values you used to generate the simulated data. If 
#   the model seems to have estimated the parameters somewhat correctly, re-run
#   the exact same fitting process with the real data. 
# - Illustrate not only the resulting estimated effect size but also the
#   uncertainty in it. 

# IF YOU ARE AN UNDERGRADUATE STUDENT OR NEW TO R, you may want to just do a 
# shorter process of running the test we recommend to you, illustrating the data
# themselves, and quoting at minimum p-value, sample size (N), and name of the
# test. 

# Always feel free to ask for help!!

### Simulating data --------------------------------

### Define model = statistical hypotheses -------------------

### Fit model to both real and simulated data ------------------

### GRAPHING THINGS ------------------------------
# There is no limit to the complexity and design of graphs, and you can 
# do whole courses on scientific illustration and data visualization. 
# HOWEVER. I typically find simpler better, as well as including a lot of 
# information. These two things may seem contradictory, but the point is 
# to include pattern only where it is informative. 

# Some things that I do differently than default:
# Make points and labels MUCH larger than the default. The information is 
# in the points, not the white space. 
# Make points partially transparent to allow for overlap. 

# Check out detailed code for graphs in 
# Dornhaus Lab R graphing.R


