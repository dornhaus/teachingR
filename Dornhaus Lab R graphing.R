# Anna Dornhaus & Lab
# R script for data analysis and figures for 
# just learning to do it in R
# 2026

### Libraries ------------------
# Overall rule: only add the ones you know you need!
# General data handling
library(tidyverse) # for bind.rows
# File access
library(readxl)
# Colors
library(scales)  # for number_format & color scales
library(viridis)

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




### Import ------------------------
# Just some examples:
setwd("../teachingR")
MyData <- read.table("./example_data/bb col data.csv", header=T, row.names=1, sep = ",")
MyData2 <- read.csv("https://docs.google.com/spreadsheets/d/1K2D2rH770iDfZzlwakHK89bwvoPqWDLaHbVNsanJFX8/gviz/tq?tqx=out:csv")
colnames(MyData2) <- c("Tree", "Leaf", "Ants", "Aphids")
files <- (Sys.glob("./example_data/similardatasheets/*.csv"))
EnormousDataLocal <- data.frame()
listOfDataframes <- lapply(files, 
                           function(x) {
                             read.table(x, 
                                        header = T,
                                        sep = ",",
                                        skip = 6
                             )
                           }
)
EnormousDataLocal <- do.call("bind_rows", listOfDataframes)


# GRAPHING DATA ------------------------------
# In order for us to graph anything, you of course have to have data. 
# I'm hoping you will pick code for a graph that fits your own data, and 
# you'll adjust it to fit with your own data table. 
# For the most part, I'm assuming your data table is called
MyData
# And then you have columns that become the x and y axes:
# Remember that the x-axis is typically a treatment or manipulated variable, 
# or a factor that is thought to (perhaps) explain something.
# The y axis is the outcome or response variable, the measurement you made as a 
# result of your method. 
MyData$X <- MyData$treatment
MyData$Y <- MyData$avgsize

# But if we want code to be reusable, it is often useful to assign the specific
# data we want to graph to a standard variable names, e.g. 
graph_data <- MyData
# That way, your graph code always uses 'graph_data', and you can copy and paste
# that code to work in different places, and you have to only change this one line.

# By far the most frequent plots you'll need are boxplots (categories, like
# treatments, on the x-axis) or scatterplots (a continuous factor on the x-axis).

### BOXPLOTS ### ---------------
# Generally you use a boxplot when plotting a continuous y
# against a categorical x axis (e.g. an outcome against 2 treatments).

# Bare boxplot
boxplot(Y ~ X
        , data = graph_data
        , xlab = "X Concept [unit measured]"
        , ylab = "Y Concept [unit measured]"
        , col = threegroupcolors
        , range = 0 # to get whiskers to extend to range (no outlier points)
)

# Fancy boxplot - but this should be your default
# What makes it fancy:
# Adjust margins
# Add sample sizes
# Add data points

# Margins:
par(oma = c(2,2,2,2), mar = c(4,4,1,1), mgp=c(3, 1, 0), las=1) 
# in order: bottom, left, top, right
par(mfrow=c(1,1)) # one graph panel

# How to make a great boxplot -

# Saving the plot into a variable allows us to access plot parameters afterwards.
Nice_Plot <- boxplot(Y ~ X
                     , data = graph_data
                     , xlab = "X Concept [unit measured]"
                     , ylab = "Y Concept [unit measured]"
                     , range = 0
# Always make axis descriptions as clear and comprehensive as possible
                     , names = c("Large bees", "Middling bees", "Small bees")
                     , col = alpha(threegroupcolors, 0.5) # use same colors as elsewhere, 
                     # but slightly transparent so we can see data points
                     , ylim = c(y_min, y_max) # always think about the scale - starting from zero is typically better
)

# Putting sample sizes above bars
nbGroup <- nlevels(as.factor(Nice_Plot$names)) # this is just a way to extract
# category names from the plot - you could get this directly from data
text(x=c(1:nbGroup) 
  , y=N_y_offset*y_max
  , cex = 1
  , col = threegroupcolors
  , paste("N=", Nice_Plot$n, sep="")  # again, the sample size 'n' is directly extracted from the plot
)

mtext("additional margin label", side=2, line=2, las=0)
mtext("additional margin label on outside", side=1, line=4, las=0, xpd = TRUE)

# Represent the raw data as well, especially for mid- to low sample sizes.
stripchart(Y ~ X
           , data = graph_data
           , add = TRUE # this plots this graph on top of the existing one
           , pch = 19
           , col = threegroupcolors
           , method = "jitter"
           , jitter = 0.2
           , vertical = TRUE
)



### SCATTERPLOTS ### ---------------
# A scatterplot is typically used whenever we have a continuous variable on the 
# x-axis. 
# E.g. 
graph_data$X <- graph_data$stdevsize

# You may or may not need all the features included here. But at minimum, you want
# fairly large points, in almost all cases make them slightly transparent, and
# really clear axis labels. 

# These are just the standard margins I use - you could have something 
# else - but I want to make sure they are reset here in case you were experimenting
# with it before. 
par(oma = c(0,0,0,0), mar = c(4,4,1,1), mgp=c(3, 1, 0), las=1) 
# bottom, left, top, right
par(mfrow=c(1,1))
plot(Y ~ X
     , data = graph_data
     , pch = 19 # set point shape
     , col = threegroupcolors[graph_data$treatmentcode]
     #, col = threegroupcolors[sapply(graph_data$treatment, function(x) switch(x, "S"=1, "M"=2, "L"=3))]
     # So you can use the first line if you have a numerical column you want to use
     # to determine the color, or you use the second version if you have a set of label names. 
     , cex = 1.5 # point size - 1 is default, but I like them bigger
     , xlab = "X-Axis Label [units]"
     , ylab = "Y-Axis Label [units]"
)

# In some cases, it makes sense to label points individually. 
# First we define the location of the labels:
label_yoffset <- 0
label_xoffset <- -max(graph_data$stdevsize) * 0.002 # you have to play around with 
# this to see what looks good. I do it relative to the x-axis for comparability
# between plots. 
text(graph_data$X + label_xoffset # x coordinates of labels
     , graph_data$Y + label_yoffset # y coordinates of labels
     , labels = graph_data$queensproduced # text in labels
     , cex = 0.5 # size of text
     , pos = 4 # make the text left aligned (to the right of given coordinates)
)

# Graphing a larger dataset ---------------------
# Let's try something like this with a larger dataset:
graph_data <- EnormousDataLocal
graph_data$X <- graph_data$average.exploited.resource.distance
graph_data$Y <- graph_data$collected.resource.units

# I'm going to redefine colors here to make sure it fits with this dataset -
# in your own script, you would presumably do this in the graphics settings
# at the top.
num_colors <- 15
# I also want the points to be semi-transparent. This is called 'alpha', which is 
# a number between 0 (transparent) and 1 (totally opaque). There are different 
# functions to do this, but it's also built-in in the viridis package color scales.
colorgradient <- magma(num_colors, alpha = 0.6)
# For that, I 'cut', i.e. categorize, a continuous variable into the number of 
# colors I want to use. 
graph_data$colors <- cut(graph_data$total.agent.timesteps.spent.searching, breaks = num_colors)

# Ok now the actual graph code:
par(oma = c(0,0,0,0), mar = c(4,4,1,1), mgp=c(3, 1, 0), las=1) 
# bottom, left, top, right
par(mfrow=c(1,1))
plot(Y ~ X
     , data = graph_data
     , pch = 19 # set point shape
     , col = colorgradient[graph_data$colors]
     , cex = sapply(as.character(graph_data$number.of.clusters), function(x) switch(x, "5"=0.5, "10"=1, "50"=1.5, "100"=2))
     , xlab = "X-Axis Label [units]"
     , ylab = "Y-Axis Label [units]"
)
# Here I used 'number.of.clusters' to define the size of the points as well. 

# We can even upgrade this to a multi-panel plot, if we want a boxplot on the sides
# to show the distribution of values. This makes some sense here also since the points
# overlap so much it is hard to tell how many are where.

# For this we can use another par() setting (e.g. mfrow=c(2,2)), or for more 
# control use 'layout()'.
layout(matrix(c(1,2,0,3), 2, 2, byrow = T), widths=c(1,5), 
       heights=c(5,1)) 
# This gives us a four-panel plot; the plots will be inserted into the panels in 
# order by row.
# How: matrix() gives the table, it has 4 entries, so 4-panel plot. 
# byrow = T means we are labeling the 4 panels by row, i.e. first the first row, 
# then the second, etc.
# Inside matrix(), the four numbers are the order in which plots below will be 
# inserted into the four panels: first the first one (top left), then the second 
# (top right) then the bottom right panel is empty, then the bottom left is number 
# 3. So we expect a total of three plots below. 
# heights() specifies the heights of the two rows. The first row is much taller than
# the second row. widths() specifies the width of the columns: the second column is
# much wider. So plot number 2 will be both wide and high, plot number one will be tall 
# but narrow, and plot number 3 is wide and short. The cell in bottom left doesn't
# have a plot but is both short and narrow. 

# See above for the color strategy here. 
num_colors <- 15
colorgradient <- magma(num_colors, alpha = 0.6)
graph_data$colors <- cut(graph_data$total.agent.timesteps.spent.searching, breaks = num_colors)

# Various other format adjustments
par(oma = c(0,0,0,0), mgp=c(3, 1, 0), las=1)

# Panel 1: Distribution of y-values
par(mar = c(4,0,1,0)) # bottom, left, top, right
boxplot(graph_data$Y
        , xaxt = 'n'
        , yaxt = 'n'
        , frame = FALSE
        , range = 0
        , col = twogroupcolors[1]
)

# Panel 2: The main graph, a scatterplot
par(mar = c(4,4,1,2)) # bottom, left, top, right
plot(Y ~ X
     , data = graph_data
     , pch = 19 # set point shape
     , col = colorgradient[graph_data$colors]
     , cex = sapply(as.character(graph_data$number.of.clusters), function(x) switch(x, "5"=0.5, "10"=1, "50"=1.5, "100"=2))
     , xlab = "X-Axis Label [units]"
     , ylab = "Y-Axis Label [units]"
)

legend("topright"
       , title = "Time searching"
       , c(paste("t=", min(graph_data$total.agent.timesteps.spent.searching))
           , paste("t=", median(graph_data$total.agent.timesteps.spent.searching))
           , paste("t=", max(graph_data$total.agent.timesteps.spent.searching)))
       , col = c(colorgradient[1], colorgradient[round(num_colors/2)], colorgradient[num_colors])
       , pch = 19 # you'll normally match the shape of the scatterplot points
)

# Panel 3: empty
# We put a 0 in the layout matrix there, so R should know we don't want this to be used
# One could put a legend or text here if needed.

# Panel 4: Boxplot of x-axis values
par(mar = c(0,4,0,2)) # bottom, left, top, right
boxplot(graph_data$X
        , xaxt = 'n' # This deletes this axis
        #, yaxt = 'n'
        , frame = FALSE
        , range = 0
        , col = twogroupcolors[2]
        , horizontal=TRUE
)

# Note that if you are doing a graph of this type, I would
# - make sure the boxplot colors are not the same as the scatterplot points but somehow
# make sense in the context. Possibly better to leave the boxplots white.
# - make sure the color gradient doesn't have a very light yellow like this one that
# is nearly invisible when transparent (see third point in legend); or make sure the point
# in the legend does not have transparency. 
# - one could also squish the boxplots on the sides more by changing the heights and widths
# argument in layout, to get rid of more of the white space.

# ILLUSTRATING STATISTICAL RESULTS ------------------------



