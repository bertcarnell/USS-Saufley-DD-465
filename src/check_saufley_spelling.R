stopifnot(require(xml2))
stopifnot(require(hunspell))

check_saufley_spelling <- function(path_to_xml, word_list, ship_names)
{
  # source("src/CustomDictionary.R")
  # word_list <- create_word_list()
  # ship_names <- read.csv("src/ShipNames.csv")
  # path_to_xml <- "docs"
  
  # read files
  xml_filepath <- file.path(path_to_xml, "USS_Saufley_WarReports.xml")
  X <- xml2::read_xml(xml_filepath)
  # extract descriptions
  X_descriptions <- unlist(lapply(xml2::xml_find_all(X, "//description"), xml2::xml_text))
  
  # Drop ship names in descriptions
  for (i in seq_along(X_descriptions))
  {
    for (j in seq_along(ship_names$Ship))
    {
      ship_to_find <- paste0(ship_names$Ship[j], "[[:punct:][:space:]]")
      X_descriptions[i] <- gsub(ship_to_find, "", X_descriptions[i])
      if (grepl("^USS", ship_to_find))
      {
        ship_to_find <- gsub("^USS", "U.S.S.", ship_to_find)
        X_descriptions[i] <- gsub(ship_to_find, "", X_descriptions[i])
      } else if (grepl("^SS", ship_to_find))
      {
        ship_to_find <- gsub("^SS", "S.S.", ship_to_find)
        X_descriptions[i] <- gsub(ship_to_find, "", X_descriptions[i])
      }
    }
  }
  
  # Drop ordinal numbers
  for (i in seq_along(X_descriptions))
  {
    X_descriptions[i] <- gsub("[0-9]+[t][h]", "", X_descriptions[i])
    X_descriptions[i] <- gsub("[0-9]+[r][d]", "", X_descriptions[i])
  }
  
  # Drop 's
  X_descriptions <- gsub("[']s", "", X_descriptions)
  
  # Drop xml tags
  X_descriptions <- gsub("[<][/]*[a-z]+[>]", "", X_descriptions)
  
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
}
