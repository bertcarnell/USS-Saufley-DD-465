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

source("src/validate_saufley_xml.R")
source("src/check_saufley_spelling.R")
source("src/CustomDictionary.R")
source("src/War Report Utilities.R")
source("src/create_saufley_maps.R")

################################################################################
## War Diaries

### Validate the XML
validate_saufley_xml("docs")

### Spell Check
word_list <- create_word_list()
ship_names <- read.csv("src/ShipNames.csv")
check_saufley_spelling("docs", word_list, ship_names)

##### check for non-capitalized words after periods
X <- readLines(file.path("docs", "USS_Saufley_WarReports.xml"))
X[grep("[.][ ][ ][a-z]", X)]
##### Check for non-capitalized words after times (except for yds and yards)
X[grep("[.][ ]+[0-9][0-9][0-9][0-9][ ][a-xz]", X)]

# check that the objectPages are sequential
X <- xml2::read_xml(file.path("docs", "USS_Saufley_WarReports.xml"))
temp <- as.integer(xml2::xml_text(xml2::xml_find_all(X, ".//source/images/image/objectPage")))
temp2 <- xml2::xml_text(xml2::xml_find_all(X, ".//source/images/image/file"))
for (i in 2:length(temp))
{
  if (temp[i] != temp[i-1] + 1 & temp[i] != 1)
  {
    print(temp2[(i-1):(i+1)])
    stop(paste("error", i))
  }
}

### Create Maps
create_saufley_maps("docs", file.path("src", "kml"), file.path("src", "leaflet"))

################################################################################
## After Actions

word_list <- create_word_list()
ship_names <- read.csv("src/ShipNames.csv")

# read files
html_filepath <- file.path("docs", "after_action_reports.html")
X <- xml2::read_html(html_filepath)
# extract descriptions
x_p <- unlist(lapply(xml2::xml_find_all(X, "//p"), xml2::xml_text))
x_td <- unlist(lapply(xml2::xml_find_all(X, "//td"), xml2::xml_text))
x_th <- unlist(lapply(xml2::xml_find_all(X, "//th"), xml2::xml_text))
x_u <- unlist(lapply(xml2::xml_find_all(X, "//u"), xml2::xml_text))
x_li <- unlist(lapply(xml2::xml_find_all(X, "//li"), xml2::xml_text))

X_descriptions <- c(x_p, x_td, x_th, x_u, x_li)

# Drop ship names in descriptions
for (j in seq_along(ship_names$Ship))
{
  ship_to_find <- paste0(ship_names$Ship[j], "$")
  X_descriptions <- gsub(ship_to_find, "", X_descriptions)
  ship_to_find <- paste0(ship_names$Ship[j], "[[:punct:][:space:]]")
  X_descriptions <- gsub(ship_to_find, "", X_descriptions)
  if (grepl("^USS", ship_to_find))
  {
    ship_to_find <- gsub("^USS", "U.S.S.", ship_to_find)
    X_descriptions <- gsub(ship_to_find, "", X_descriptions)
  } else if (grepl("^SS", ship_to_find))
  {
    ship_to_find <- gsub("^SS", "S.S.", ship_to_find)
    X_descriptions <- gsub(ship_to_find, "", X_descriptions)
  }
}  

# Drop ordinal numbers
X_descriptions <- gsub("[0-9]+[tT][hH]", "", X_descriptions)
X_descriptions <- gsub("[0-9]+[rR][dD]", "", X_descriptions)
X_descriptions <- gsub("[0-9]+[nN][dD]", "", X_descriptions)

# Drop 's and n't
X_descriptions <- gsub("['][sS]", "", X_descriptions)
X_descriptions <- gsub("[nN]['][tT]", "", X_descriptions)

# Drop xml tags
X_descriptions <- gsub("[<][/]*[a-z]+[>]", "", X_descriptions)

# Drop Radio Sigals
# "ZUG", "DLS", "DLYI", "DJLF", "TBU",
# "IJA", "FYK", "VK", "GHE",  "WDI", "GKA", "BLO", "MF", "MBNH", "WQ",
# "ICLN",
X_descriptions <- gsub("[(][A-Z]+[)]", "", X_descriptions)
X_descriptions <- gsub("BLO[-][0-9]+", "", X_descriptions)
X_descriptions <- gsub(" BT[.]", "", X_descriptions)


# Check Spelling with custom dictionary
temp_spelling <- hunspell::hunspell(X_descriptions, format = "text", ignore = word_list)
ind <- which(sapply(temp_spelling, length) > 0)
if (length(ind) > 0)
{
  print(temp_spelling[ind])
  cat("\n")
  stop("Spelling Issues Found")
} else
{
  print("No Spelling Issues Found")
}

