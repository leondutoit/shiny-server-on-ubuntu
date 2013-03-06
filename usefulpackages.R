
# A r-program to install useful packages not included in the standard library
# Execute as sudo

options("repos"="http://cran.uib.no/") # set the cran mirror

# Package management and development
install.packages("devtools", dep=TRUE)

# Plotting
install.packages("ggplot2", dep=TRUE) # Grammar of graphics implementation

# Working with data
install.packages("plyr", dep=TRUE) # data wrangling
install.packages("reshape2", dep=TRUE) # data wrangling
install.packages("lubridate", dep=TRUE) # date handling
install.packages("stringr", dep=TRUE) # string handling

# File/data input/output
install.packages("RCurl", dep=TRUE) # get files through http
install.packages("RJSONIO", dep=TRUE) # work with JSON data structures

# Web development
install.packages("shiny", dep=TRUE) # application framework (local usage)
