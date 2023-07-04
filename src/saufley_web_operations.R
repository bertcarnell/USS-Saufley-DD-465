## Saufley Operations
require(XML)
require(xml2)
require(assertthat)
require(hunspell)
require(stringr)
require(plotKML)
require(sf)
require(lubridate)
require(webshot)
require(leaflet)
require(sp)
require(dplyr)
require(htmlwidgets)

### Validate the XML
source("src/validate_saufley_xml.R")
validate_saufley_xml("docs")

### Spell Check
source("src/check_saufley_spelling.R")
source("src/CustomDictionary.R")
word_list <- create_word_list()
ship_names <- read.csv("src/ShipNames.csv")
check_saufley_spelling("docs", word_list, ship_names)

### Create Maps
source("src/War Report Utilities.R")
source("src/create_saufley_maps.R")
create_saufley_maps("docs", file.path("src", "kml"), file.path("src", "leaflet"))

