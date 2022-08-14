## Utility Functions for Parsing War Reports

Source <- R6::R6Class("Source",
  public = list(
    initialize = function(source_node){
      assertthat::assert_that(xml2::xml_name(source_node) == "source")
      private$name <- xml2::xml_text(xml2::xml_find_first(source_node, ".//name"))
      private$type <- xml2::xml_text(xml2::xml_find_first(source_node, ".//type"))
      private$date <- xml2::xml_text(xml2::xml_find_first(source_node, ".//date"))
      private$url <- xml2::xml_text(xml2::xml_find_first(source_node, ".//url"))
      private$directory <- xml2::xml_text(xml2::xml_find_first(source_node, ".//directory"))
      temp_images <- xml2::xml_find_all(source_node, ".//images/image")
      private$images <- lapply(temp_images, function(z) return(Image$new(z)))
    },
    get_metadata = function(){
      return(list(name = private$name, 
                  type = private$type, 
                  date = private$date, 
                  url = private$url, 
                  directory = private$directory))
    },
    get_images = function(){
      return(private$images)
    }
  ),
  private = list(
    name = character(),
    type = character(),
    date = character(),
    url = character(),
    directory = character(),
    images = list()
  )
)

Image <- R6::R6Class("Image",
  public = list(
   initialize = function(image_node){
     assertthat::assert_that(xml2::xml_name(image_node) == "image")
     private$filename <- xml2::xml_text(xml2::xml_find_first(image_node, ".//file"))
     temp_items <- xml2::xml_find_all(image_node, ".//items/item")
     private$items <- lapply(temp_items, function(z) return(Item$new(z)))
   },
   get_filename = function(){
     return(private$filename)
   },
   get_items = function(){
     return(private$items)
   }
  ),
  private = list(
   filename = character(),
   items = list()
  )
)

Item <- R6::R6Class("Item",
  public = list(
    initialize = function(item_node){
      assertthat::assert_that(xml2::xml_name(item_node) == "item")
      private$date <- xml2::xml_text(xml2::xml_find_first(item_node, ".//date"))
      private$description <- xml2::xml_text(xml2::xml_find_first(item_node, ".//description"))
      temp_posits <- xml2::xml_find_all(item_node, ".//posit")
      private$posits <- lapply(temp_posits, function(z) Posit$new(z))
    },
    get_date = function(){
      return(private$date)
    },
    get_description = function(){
      return(private$description)
    },
    get_posits = function(){
      return(do.call("rbind", lapply(private$posits, function(z) z$get())))
    }
  ),
  private = list(
    date = character(),
    description = character(),
    posits = list()
  )
)

Posit <- R6::R6Class("Posit",
  public = list(
   initialize = function(posit_node){
     assertthat::assert_that(xml2::xml_name(posit_node) == "posit")
     private$time <- xml2::xml_attr(posit_node, "time")
     private$lat <- xml2::xml_attr(posit_node, "lat")
     private$lon <- xml2::xml_attr(posit_node, "lon")
   },
   get = function(){
     return(c(time = private$time,
              lat = self$translate_lat(private$lat),
              lon = self$translate_lat(private$lon)))
   },
   translate_lat = function(slat){
     if (slat == "") 
       return(NA)
     lsign <- ifelse(grepl("N", slat) | grepl("E", slat), 1, -1)
     lnum <- slat %>%
       stringr::str_extract("[0-9]*[-][0-5][0-9][-][0-5][0-9]") %>%
       stringr::str_split("-") %>%
       magrittr::extract2(1) %>%
       as.numeric()
     ret <- (lnum[1] + lnum[2]/60 + lnum[3]/3600) * lsign
     if (is.na(ret))
     {
       warning(paste0("Potentially malformed Lat/Long in ", slat))
     }
     return(ret)
   }
  ),
  private = list(
   time = character(),
   lat = character(),
   lon = character()
  )
)

## INTERNAL VERSION OF plotKML:::.kml_layer_sfc_LINESTRING

my_kml_layer_sfc_LINESTRING <- function(
  obj,
  subfolder.name = paste(class(obj)),
  extrude = FALSE,
  z.scale = 1,
  metadata = NULL,
  html.table = NULL,
  TimeSpan.begin = "",
  TimeSpan.end = "",
  ...
) {
  kml.out <- get("kml.out", envir = plotKML.fileIO)
  
  if (is.na(sf::st_crs(obj)) || is.null(sf::st_crs(obj))) {
    stop("CRS of obj is missing")
  }
  if (!sf::st_is_longlat(obj) || st_crs(obj) != st_crs(4326)) {
    obj <- sf::st_transform(obj, crs = 4326)
    message("Reprojecting to ", get("ref_CRS", envir = plotKML.opts))
  }
  
  aes <- kml_aes(obj, ...)
  
  lines_names <- aes[["labels"]]
  colours <- aes[["colour"]]
  widths <- aes[["width"]]
  altitude <- aes[["altitude"]]
  altitudeMode <- aes[["altitudeMode"]]
  balloon <- aes[["balloon"]]
  
  if (
    balloon & 
    (inherits(obj, "sf") ||  (isS4(obj) && "data" %in% slotNames(obj)))
  ) {
    html.table <- .df2htmltable(obj)
  }
  
  message("Writing to KML...")
  pl1 = newXMLNode("Folder", parent = kml.out[["Document"]])
  pl2 <- newXMLNode("name", subfolder.name, parent = pl1)
  
  if (!is.null(metadata)) {
    md.txt <- kml_metadata(metadata, asText = TRUE)
    txtm <- sprintf('<description><![CDATA[%s]]></description>', md.txt)
    parseXMLAndAdd(txtm, parent=pl1)
  }
  
  lv <- length(sf::st_geometry(obj)) 
  coords <- NULL
  for (i.line in seq_len(lv)) {
    xyz <- matrix(sf::st_coordinates(obj[i.line])[, 1:2], ncol = 2)
    xyz <- cbind(xyz, rep(altitude[i.line], nrow(xyz)))
    coords[[i.line]] <- paste(xyz[, 1], ',', xyz[, 2], ',', xyz[, 3], collapse='\n ', sep = "")
  }
  
  txts <- sprintf(
    '<Style id="line%s"><LineStyle><color>%s</color><width>%.0f</width></LineStyle><BalloonStyle><text>$[description]</text></BalloonStyle></Style>', 
    seq_len(lv), 
    colours, 
    widths
  )
  parseXMLAndAdd(txts, parent=pl1)
  
  if (length(html.table) > 0) {
    if (nzchar(TimeSpan.begin[1]) & nzchar(TimeSpan.end[1])) {
      if (identical(TimeSpan.begin, TimeSpan.end)) {
        when = TimeSpan.begin
        if (length(when) == 1L){
          when = rep(when, lv) 
        }
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><TimeStamp><when>%s</when></TimeStamp><description><![CDATA[%s]]></description><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
          lines_names, 
          seq_len(lv), 
          when, 
          html.table, 
          rep(as.numeric(extrude), lv), 
          rep(altitudeMode, lv), 
          paste(unlist(coords))
        )
      } else {
        if (length(TimeSpan.begin) == 1L) {
          TimeSpan.begin = rep(TimeSpan.begin, lv) 
        }
        if (length(TimeSpan.end) == 1L) {
          TimeSpan.end = rep(TimeSpan.end, lv) 
        }
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><TimeSpan><begin>%s</begin><end>%s</end></TimeSpan><description><![CDATA[%s]]></description><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
          lines_names, 
          seq_len(lv), 
          TimeSpan.begin, 
          TimeSpan.end, 
          html.table, 
          rep(as.numeric(extrude), lv), 
          rep(altitudeMode, lv), 
          paste(unlist(coords))
        )    
      }
    } else {      
      txt <- sprintf(
        '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><description><![CDATA[%s]]></description><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
        lines_names, 
        seq_len(lv), 
        html.table, 
        rep(as.numeric(extrude), lv), 
        rep(altitudeMode, lv), 
        paste(unlist(coords))
      )
    }
  } else {
    if (nzchar(TimeSpan.begin[1]) & nzchar(TimeSpan.end[1])) {
      if (identical(TimeSpan.begin, TimeSpan.end)) {
        when = TimeSpan.begin
        if (length(when) == 1L) {
          when = rep(when, lv) 
        }
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><TimeStamp><when>%s</when></TimeStamp><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
          lines_names, 
          seq_len(lv), 
          when, 
          rep(as.numeric(extrude), lv), 
          rep(altitudeMode, lv), 
          paste(unlist(coords))
        )
      } else {
        if (length(TimeSpan.begin) == 1L){
          TimeSpan.begin = rep(TimeSpan.begin, lv)
        }
        if (length(TimeSpan.end) == 1L){
          TimeSpan.end = rep(TimeSpan.end, lv)
        }   
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><TimeSpan><begin>%s</begin><end>%s</end></TimeSpan><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
          lines_names, 
          seq_len(lv), 
          TimeSpan.begin, 
          TimeSpan.end, 
          rep(as.numeric(extrude), lv), 
          rep(altitudeMode, lv), 
          paste(unlist(coords))
        )
      }     
    } else {
      txt <- sprintf(
        '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
        lines_names, 
        seq_len(lv), 
        rep(as.numeric(extrude), lv), 
        rep(altitudeMode, lv), 
        paste(unlist(coords))
      ) 
    }
  }
  
  parseXMLAndAdd(txt, parent=pl1)
  
  assign('kml.out', kml.out, envir=plotKML.fileIO)
}

new_kml_layer_sfc_LINESTRING <- function(
    obj,
    kml.out,
    subfolder.name = paste(class(obj)),
    extrude = FALSE,
    z.scale = 1,
    metadata = NULL,
    html.table = NULL,
    TimeSpan.begin = "",
    TimeSpan.end = "",
    my_colors,
    my_labels,
    ...) 
{
  # obj <- sf::st_cast(sf_lines, "LINESTRING")
  # kml.out <- kmlout  
  # subfolder.name = paste(ym, "Routes")
  # TimeSpan.begin = format(sf_times, "%Y-%m-%dT%H:%M:%SZ")
  # TimeSpan.end = format(sf_times + 60, "%Y-%m-%dT%H:%M:%SZ")
  # labels = sf_names
  # my_colors = plotKML::col2kml(year_color[k])

  if (is.na(sf::st_crs(obj)) || is.null(sf::st_crs(obj))) {
    stop("CRS of obj is missing")
  }
  if (!sf::st_is_longlat(obj) || st_crs(obj) != st_crs(4326)) {
    obj <- sf::st_transform(obj, crs = 4326)
    message("Reprojecting to ", get("ref_CRS", envir = plotKML.opts))
  }
  
  aes <- kml_aes(obj, ...)
  
  #lines_names <- aes[["labels"]]
  lines_names <- my_labels
  #colours <- aes[["colour"]]
  colours <- my_colors
  widths <- aes[["width"]]
  altitude <- aes[["altitude"]]
  altitudeMode <- aes[["altitudeMode"]]
  balloon <- aes[["balloon"]]
  
  if (
    balloon & 
    (inherits(obj, "sf") ||  (isS4(obj) && "data" %in% slotNames(obj)))
  ) {
    html.table <- .df2htmltable(obj)
  }
  
  pl1 <- newXMLNode("Folder", parent = kml.out[["Document"]])
  pl2 <- newXMLNode("name", subfolder.name, parent = pl1)
  
  if (!is.null(metadata)) {
    md.txt <- kml_metadata(metadata, asText = TRUE)
    txtm <- sprintf('<description><![CDATA[%s]]></description>', md.txt)
    parseXMLAndAdd(txtm, parent=pl1)
  }
  
  lv <- length(sf::st_geometry(obj)) 
  coords <- NULL
  for (i.line in seq_len(lv)) {
    xyz <- matrix(sf::st_coordinates(obj[i.line])[, 1:2], ncol = 2)
    xyz <- cbind(xyz, rep(altitude[i.line], nrow(xyz)))
    coords[[i.line]] <- paste(xyz[, 1], ',', xyz[, 2], ',', xyz[, 3], collapse='\n ', sep = "")
  }
  
  txts <- sprintf(
    '<Style id="line%s"><LineStyle><color>%s</color><width>%.0f</width></LineStyle><BalloonStyle><text>$[description]</text></BalloonStyle></Style>', 
    seq_len(lv), 
    colours, 
    widths
  )
  parseXMLAndAdd(txts, parent=pl1)
  
  if (length(html.table) > 0) {
    if (nzchar(TimeSpan.begin[1]) & nzchar(TimeSpan.end[1])) {
      if (identical(TimeSpan.begin, TimeSpan.end)) {
        when = TimeSpan.begin
        if (length(when) == 1L){
          when = rep(when, lv) 
        }
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><TimeStamp><when>%s</when></TimeStamp><description><![CDATA[%s]]></description><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
          lines_names, 
          seq_len(lv), 
          when, 
          html.table, 
          rep(as.numeric(extrude), lv), 
          rep(altitudeMode, lv), 
          paste(unlist(coords))
        )
      } else {
        if (length(TimeSpan.begin) == 1L) {
          TimeSpan.begin = rep(TimeSpan.begin, lv) 
        }
        if (length(TimeSpan.end) == 1L) {
          TimeSpan.end = rep(TimeSpan.end, lv) 
        }
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><TimeSpan><begin>%s</begin><end>%s</end></TimeSpan><description><![CDATA[%s]]></description><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
          lines_names, 
          seq_len(lv), 
          TimeSpan.begin, 
          TimeSpan.end, 
          html.table, 
          rep(as.numeric(extrude), lv), 
          rep(altitudeMode, lv), 
          paste(unlist(coords))
        )    
      }
    } else {      
      txt <- sprintf(
        '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><description><![CDATA[%s]]></description><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
        lines_names, 
        seq_len(lv), 
        html.table, 
        rep(as.numeric(extrude), lv), 
        rep(altitudeMode, lv), 
        paste(unlist(coords))
      )
    }
  } else {
    if (nzchar(TimeSpan.begin[1]) & nzchar(TimeSpan.end[1])) {
      if (identical(TimeSpan.begin, TimeSpan.end)) {
        when = TimeSpan.begin
        if (length(when) == 1L) {
          when = rep(when, lv) 
        }
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><TimeStamp><when>%s</when></TimeStamp><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
          lines_names, 
          seq_len(lv), 
          when, 
          rep(as.numeric(extrude), lv), 
          rep(altitudeMode, lv), 
          paste(unlist(coords))
        )
      } else {
        if (length(TimeSpan.begin) == 1L){
          TimeSpan.begin = rep(TimeSpan.begin, lv)
        }
        if (length(TimeSpan.end) == 1L){
          TimeSpan.end = rep(TimeSpan.end, lv)
        }   
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><TimeSpan><begin>%s</begin><end>%s</end></TimeSpan><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
          lines_names, 
          seq_len(lv), 
          TimeSpan.begin, 
          TimeSpan.end, 
          rep(as.numeric(extrude), lv), 
          rep(altitudeMode, lv), 
          paste(unlist(coords))
        )
      }     
    } else {
      txt <- sprintf(
        '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
        lines_names, 
        seq_len(lv), 
        rep(as.numeric(extrude), lv), 
        rep(altitudeMode, lv), 
        paste(unlist(coords))
      ) 
    }
  }
  
  parseXMLAndAdd(txt, parent=pl1)
  
  return(kml.out)
}

new_kml_layer_sfc_POINT <- function(
    obj,
    kml.out,
    subfolder.name = paste(class(obj)),
    extrude = TRUE,
    z.scale = 1,
    LabelScale = get("LabelScale", envir = plotKML.opts),
    metadata = NULL,
    html.table = NULL,
    TimeSpan.begin = "",
    TimeSpan.end = "",
    points_names,
    my_colors,
    ...
  ) {
  # Checking the projection
  # AG: The sp code here used check_projection() and reproject(). AFAICT,
  # check_projection() is used to 
  # 1) check that the input CRS is not NA; 
  # 2) check if CRS of obj is projected or not, and 
  # 3) check that CRS of obj is valid. 
  # reproject() is just a wrapper around spTransform. 
  # I think the following code perform exactly the same operations since the
  # checks on the input CRS are performed by sf::st_crs(). Moreover, AFAIK,
  # proj4strings are getting deprecated
  # (https://www.r-spatial.org/r/2020/03/17/wkt.html), so I wouldn't use the old
  # check_projection code. Moreover, using recent versions of SF/PROJ/GDAL it
  # shows a warning message.
  if (is.na(sf::st_crs(obj)) || is.null(sf::st_crs(obj))) {
    stop("CRS of obj is missing")
  }
  if (!sf::st_is_longlat(obj)) {
    obj <- sf::st_transform(obj, crs = get("ref_CRS", envir = plotKML.opts))
    # AG: Maybe here we should consider redefine ref_CRS according to PROJ/GDAL
    # versions (i.e. proj4string or EPSG)
    message("Reprojecting to ", get("ref_CRS", envir = plotKML.opts))
  }
  
  # Parsing the call for aesthetics
  aes <- kml_aes(obj, ...)
  
  # Read the relevant aesthetics
  if(missing(points_names)){
    points_names <- aes[["labels"]]
  }
  #colours <- aes[["colour"]]
  colours <- my_colors
  shapes <- aes[["shape"]]
  sizes <- aes[["size"]]
  altitude <- aes[["altitude"]]
  altitudeMode <- aes[["altitudeMode"]]
  balloon <- aes[["balloon"]]
  
  # Parse ATTRIBUTE TABLE (for each placemark):
  if (is.null(html.table)) {
    if (
      (balloon | class(balloon) %in% c('character','numeric')) & 
      # The following code generalizes obj@data to sf/sfc objects
      (inherits(obj, "sf") ||  (isS4(obj) && "data" %in% slotNames(obj)))
    ) {
      # .data_sp_sf_sfc generalizes obj@data to sf/sfc objects
      html.table <- .df2htmltable(.data_sp_sf_sfc(obj)) 
    }
  }
  
  # Folder and name of the points folder
  pl1 <- newXMLNode("Folder", parent = kml.out[["Document"]])
  pl2 <- newXMLNode("name", subfolder.name, parent = pl1)
  
  # Insert metadata:
  if (!is.null(metadata)) {
    md.txt <- kml_metadata(metadata, asText = TRUE)
    txt <- sprintf('<description><![CDATA[%s]]></description>', md.txt)
    parseXMLAndAdd(txt, parent = pl1)
  }
  message("Writing to KML...")
  
  # Writing points styles
  # =====================
  txts <- sprintf(
    '<Style id="pnt%s"><LabelStyle><scale>%.1f</scale></LabelStyle><IconStyle><color>%s</color><scale>%s</scale><Icon><href>%s</href></Icon></IconStyle><BalloonStyle><text>$[description]</text></BalloonStyle></Style>', 
    # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
    seq_len(plotKML:::.length_sp_sf(obj)), 
    rep(LabelScale, plotKML:::.length_sp_sf(obj)), 
    colours, 
    sizes, 
    shapes
  )
  parseXMLAndAdd(txts, parent = pl1)
  
  # Writing points coordinates
  # ==========================
  
  # with attributes:
  if (length(html.table) > 0) {
    if (nzchar(TimeSpan.begin[1]) & nzchar(TimeSpan.end[1])) {
      if (identical(TimeSpan.begin, TimeSpan.end)) {
        when = TimeSpan.begin
        if (length(when) == 1L) {
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          when = rep(when, plotKML:::.length_sp_sf(obj)) 
        }
        txtc <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#pnt%s</styleUrl><TimeStamp><when>%s</when></TimeStamp><description><![CDATA[%s]]></description><Point><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%.5f,%.5f,%.0f</coordinates></Point></Placemark>', 
          points_names, 
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          seq_len(plotKML:::.length_sp_sf(obj)), 
          when, 
          html.table, 
          rep(as.numeric(extrude), plotKML:::.length_sp_sf(obj)), 
          rep(altitudeMode, plotKML:::.length_sp_sf(obj)), 
          sf::st_coordinates(obj)[, 1], 
          sf::st_coordinates(obj)[, 2], 
          altitude
        )
      } else {
        if (length(TimeSpan.begin) == 1) {
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          TimeSpan.begin = rep(TimeSpan.begin, plotKML:::.length_sp_sf(obj))
        }
        if (length(TimeSpan.end) == 1) {
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          TimeSpan.end = rep(TimeSpan.end, plotKML:::.length_sp_sf(obj)) 
        }
        txtc <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#pnt%s</styleUrl><TimeSpan><begin>%s</begin><end>%s</end></TimeSpan><description><![CDATA[%s]]></description><Point><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%.5f,%.5f,%.0f</coordinates></Point></Placemark>', 
          points_names, 
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          seq_len(plotKML:::.length_sp_sf(obj)), 
          TimeSpan.begin, 
          TimeSpan.end, 
          html.table, 
          rep(as.numeric(extrude), plotKML:::.length_sp_sf(obj)), 
          rep(altitudeMode, plotKML:::.length_sp_sf(obj)), 
          sf::st_coordinates(obj)[, 1], 
          sf::st_coordinates(obj)[, 2], 
          altitude
        )      
      }
    } else {
      txtc <- sprintf(
        '<Placemark><name>%s</name><styleUrl>#pnt%s</styleUrl><description><![CDATA[%s]]></description><Point><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%.5f,%.5f,%.0f</coordinates></Point></Placemark>', 
        points_names, 
        # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
        seq_len(plotKML:::.length_sp_sf(obj)), 
        html.table, 
        rep(as.numeric(extrude), plotKML:::.length_sp_sf(obj)), 
        rep(altitudeMode, plotKML:::.length_sp_sf(obj)), 
        sf::st_coordinates(obj)[, 1], 
        sf::st_coordinates(obj)[, 2], 
        altitude
      )  
    }
  } else {
    # without attributes:
    if (nzchar(TimeSpan.begin[1]) & nzchar(TimeSpan.end[1])) {
      if (identical(TimeSpan.begin, TimeSpan.end)) {
        when = TimeSpan.begin
        if (length(when) == 1L) {
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          when = rep(when, plotKML:::.length_sp_sf(obj))
        }
        txtc <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#pnt%s</styleUrl><TimeStamp><when>%s</when></TimeStamp><Point><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%.5f,%.5f,%.0f</coordinates></Point></Placemark>', 
          points_names, 
          seq_len(plotKML:::.length_sp_sf(obj)),  
          when, 
          rep(as.numeric(extrude), plotKML:::.length_sp_sf(obj)), 
          rep(altitudeMode, plotKML:::.length_sp_sf(obj)), 
          sf::st_coordinates(obj)[, 1], 
          sf::st_coordinates(obj)[, 2], 
          altitude
        )
      } else {
        if (length(TimeSpan.begin) == 1L) {
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          TimeSpan.begin = rep(TimeSpan.begin, plotKML:::.length_sp_sf(obj))
        }
        if (length(TimeSpan.end) == 1L) {
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          TimeSpan.end = rep(TimeSpan.end, plotKML:::.length_sp_sf(obj))
        }      
        txtc <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#pnt%s</styleUrl><TimeSpan><begin>%s</begin><end>%s</end></TimeSpan><Point><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%.5f,%.5f,%.0f</coordinates></Point></Placemark>', 
          points_names, 
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          seq_len(plotKML:::.length_sp_sf(obj)), 
          TimeSpan.begin, 
          TimeSpan.end, 
          rep(as.numeric(extrude), plotKML:::.length_sp_sf(obj)), 
          rep(altitudeMode, plotKML:::.length_sp_sf(obj)), 
          sf::st_coordinates(obj)[, 1], 
          sf::st_coordinates(obj)[, 2], 
          altitude
        )    
      }     
    } else {
      txtc <- sprintf(
        '<Placemark><name>%s</name><styleUrl>#pnt%s</styleUrl><Point><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%.5f,%.5f,%.0f</coordinates></Point></Placemark>', 
        points_names, 
        # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
        seq_len(plotKML:::.length_sp_sf(obj)), 
        rep(as.numeric(extrude), plotKML:::.length_sp_sf(obj)), 
        rep(altitudeMode, plotKML:::.length_sp_sf(obj)), 
        sf::st_coordinates(obj)[, 1], 
        sf::st_coordinates(obj)[, 2], 
        altitude
      )      
    }
  }
  
  parseXMLAndAdd(txtc, parent = pl1)
  
  return(kml.out)
}

new_kml_open <- function(file.name, folder.name = file.name, kml_open = TRUE, 
                         kml_visibility = TRUE, overwrite = TRUE, use.Google_gx = FALSE, 
                         kml_xsd = get("kml_xsd", envir = plotKML.opts), xmlns = get("kml_url", 
                                                                                      envir = plotKML.opts), xmlns_gx = get("kml_gx", envir = plotKML.opts)) 
{
  if (file.exists(file.name) & overwrite == FALSE) {
    stop(paste("File", file.name, "already exists. Set the overwrite option to TRUE or choose a different name."))
  }
  if (use.Google_gx) {
    kml.out <- newXMLNode("kml", attrs = c(version = "1.0"), 
                          namespaceDefinitions = c(xsd = kml_xsd, xmlns = xmlns, 
                                                   `xmlns:gx` = xmlns_gx))
  }
  else {
    kml.out <- newXMLNode("kml", attrs = c(version = "1.0"), 
                          namespaceDefinitions = c(xsd = kml_xsd, xmlns = xmlns))
  }
  h2 <- newXMLNode("Document", parent = kml.out)
  h3 <- newXMLNode("name", folder.name, parent = h2)
  if (kml_visibility == FALSE) {
    h4 <- newXMLNode("visibility", as.numeric(kml_visibility), 
                     parent = h2)
  }
  h5 <- newXMLNode("open", as.numeric(kml_open), parent = h2)
  return(kml.out)
}

new_kml_close <- function(file.name, kml.out, overwrite = FALSE) 
{
  #kml.out <- get("kml.out", envir = plotKML.fileIO)
  XML::saveXML(kml.out, file.name)
}
