
# This file tests for the presence of the required packages and 
# installs them if needed.
#
# This Shiny Application was built on R 3.1.1 (Platform: x86_64-apple-darwin13.1.0)
# Required packages and the version used in development ar below:
#
#   Package           Version
#    shiny             0.10.1
#    shinyIncubator    0.2.1
#    ggplot2           1.0.0
#    shinythings       1.0
#    devtools          1.5
#    fpc               2.1-8
#    lsa               0.73
#    extrafont         0.16
#    igraph            0.7.1
#    reshape2          1.4
#    wordcloud         2.5
#    stringr           0.6.2
#    jsonlite          0.9.11
#    RWeka             0.4-23
#    tm                0.6
#    dplyr             0.2
#    plyr              1.8.1
#    SnowballC         0.5.1
#    mclust            4.4



###### Install Packages ######
# # Installed Packages
# iPkgs = rownames(installed.packages())
# 
# # Required Packages
# cranPkgs = c("devtools", "ggplot2", "fpc", "lsa", "extrafont", "igraph", "reshape2", "wordcloud", 
#              "stringr", "jsonlite", "RWeka", "tm", "plyr", "dplyr", "SnowballC", "mclust")
# gitPkgs = c("shinyIncubator", "shinythings")
# 
# # Iterate packges to test if it is installed
# for( i in 1:length(cranPkgs) ){
#   if( !(cranPkgs[i] %in% iPkgs) ){
#     cat("Installing required package: ", cranPkgs[i])
#     install.packages(cranPkgs[i], repos="http://cran.rstudio.com")
#   }
# }
# 
# for( i in 1:length(gitPkgs) ){
#   if( !(gitPkgs[i] %in% iPkgs) ){
#     if( gitPkgs[i] == "shinyIncubator" ) {
#       cat("Installing required package: ", gitPkgs[i])
#       devtools::install_github( "rstudio/shiny-incubator",
#                                 ref="5a78877229deb60fc00ee21d6f2170c90099443b")
#     }
#     if( gitPkgs[i] == "shinythings" ) {
#       cat("Installing required package: ", gitPkgs[i])
#       devtools::install_github( "dcurrier/shinythings", 
#                                 ref="83fd649709601b5b6154e75d52b79f4fdfe66fae")
#     }
#   }
# }




###### Source Functions ######
# Now that we have the required packages installed, we need to source the functions.
source("AnalysisFunctions.R")
source("ExploratoryFunctions.R")
source("Read_Agg.R")


