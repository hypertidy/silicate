library(rgdal)
library(rgeos)
library(igraph)

# Read states layer from a GeoJSON file
#url = "https://raw.githubusercontent.com/alignedleft/d3-book/master/chapter_12/us-states.json"
url <- "https://raw.githubusercontent.com/python-visualization/folium/master/examples/data/us-states.json"
#download.file(url, basename(url), mode = "wb")
state = readOGR(
  dsn = url, 
  layer = "us-states", 
  stringsAsFactors = FALSE, 
  verbose = FALSE
)

# Transform to US-Atlas projection
state = spTransform(
  state, 
  "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
)

# Assign state names to 'row.names'
row.names(state) = state$name
