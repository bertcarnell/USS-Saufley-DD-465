stopifnot(require(xml2))
stopifnot(require(hunspell))

check_saufley_spelling <- function(path_to_xml, word_list, ship_names)
{
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

  # Check Spelling with custom dictionary
  temp_spelling <- hunspell::hunspell(X_descriptions, format = "xml", ignore = word_list)
  ind <- which(sapply(temp_spelling, length) > 0)
  if (length(ind) > 0)
  {
    print(temp_spelling[ind])
    cat("\n")
    print(X_descriptions[ind])
    cat("\n")
    stop("Spelling Issues Found")
  } else
  {
    print("No Spelling Issues Found")
  }
}
