# load the required packages and install them if they are not.
packages_loaded <- c(
  "here",
  "dplyr",
  "ggplot2",
  "tidyverse",
  "lubridate",
  "DT",
  "plotly",
  "ggridges",
  "leaflet",
  "sf",
  "treemap",
  "tidyr",
  "scales",
  "ggwordcloud",
  "tm",
  "wordcloud2",
  "ggrepel",
  "kableExtra",
  "knitr",
  "visdat",
  "summarytools",
  "readr",
  "jsonlite",
  "sf",
  "RColorBrewer",
  "viridis")

# Function that install the packages if not already installed on your computer
for (pkg in packages_loaded) {
  if (!pkg %in% installed.packages()) {
    install.packages(pkg)}}

# load the packages
for (pkg in packages_loaded) {
  library(pkg, character.only = TRUE)}

# cleaning of the environment
rm(pkg,
   packages_loaded)

#colors
default_color <- "#DA291C"

discrete_color <- brewer.pal(11, "RdYlGn")
discrete_color_less <- brewer.pal(4, "RdYlGn")

maps_palette <- "Reds"
