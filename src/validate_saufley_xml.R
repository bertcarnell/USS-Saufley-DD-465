stopifnot(require(XML))
stopifnot(require(assertthat))

validate_saufley_xml <- function(path_to_xml)
{
  validate_xml(file.path(path_to_xml, "USS_Saufley_WarReports.xml"),
               file.path(path_to_xml, "USS_Saufley_WarReports.xsd"))
  validate_xml(file.path(path_to_xml, "saufley_crew_photos.xml"),
               file.path(path_to_xml, "saufley_crew_photos.xsd"))
  validate_xml(file.path(path_to_xml, "saufley_reunion_photos.xml"),
               file.path(path_to_xml, "saufley_reunion_photos.xsd"))
}

validate_xml <- function(xml_filepath, xsd_filepath)
{
  assertthat::assert_that(file.exists(xml_filepath),
                          msg = paste("missing ", xml_filepath))
  assertthat::assert_that(file.exists(xsd_filepath),
                          msg = paste("Missing", xsd_filepath))
  temp <- XML::xmlSchemaValidate(XML::xmlSchemaParse(xsd_filepath),
                                 XML::xmlParse(xml_filepath))
  if (length(temp$errors) > 0)
  {
    sapply(temp[[2]], function(x) {print(x$msg); print(x$line)})
    stop(paste("Errors found in ", xml_filepath))
  } else
  {
    cat(paste("\nNo errors in ", xml_filepath, "\n"))
  }
}
