stopifnot(require(assertthat))
stopifnot(require(xml2))
stopifnot(require(stringr))
stopifnot(require(plotKML))
stopifnot(require(sf))
stopifnot(require(lubridate))
stopifnot(require(webshot))
stopifnot(require(leaflet))
stopifnot(require(XML))
stopifnot(require(sp))
stopifnot(require(dplyr))
stopifnot(require(htmlwidgets))

create_saufley_maps <- function(path_to_xml, kml_outpath, leaflet_outpath)
{
  cat("\nCreate Lat and Long")
  latlon <- create_latlon_list(path_to_xml)
  cat("\nCreate Google Earth plots")
  create_saufley_google_earth(latlon, kml_outpath)
  cat("\nCreate leaflet plots")
  create_leaflet_plots(latlon, leaflet_outpath)
  cat("\nDeploy Leaflet Plots\n")
  deploy_leaflet_plots(path_to_xml, leaflet_outpath)
}

# testing
if (FALSE)
{
  source("src/War Report Utilities.R")
  source("src/create_saufley_maps.R")
  latlon <- create_latlon_list("docs")
  create_saufley_google_earth(latlon, file.path("src", "kml"))
  create_leaflet_plots(latlon, file.path("src", "leaflet"), b_save = FALSE)
}

create_latlon_list <- function(path_to_xml)
{
  xml_filepath <- file.path(path_to_xml, "USS_Saufley_WarReports.xml")
  assertthat::assert_that(file.exists(xml_filepath),
                          msg = paste("Missing", xml_filepath))
  
  X <- xml2::read_xml(xml_filepath)
  source_list <- lapply(xml2::xml_find_all(X, ".//source"), function(z) Source$new(z))
  
  source_metadata <- do.call("rbind", lapply(source_list, function(z) z$get_metadata()))

  latlon <- lapply(source_list, function(z) {
    do.call("rbind", lapply(z$get_images(), function(w) {
      do.call("rbind", lapply(w$get_items(), function(d) {
        tempdf <- d$get_posits()
        tempdf <- data.frame(time = tempdf[,1],
                             lat = as.numeric(tempdf[,2]),
                             lon = as.numeric(tempdf[,3]))
        if (nrow(tempdf) > 0){
          tempdf$date <- as.Date(d$get_date())
          tempdf$Description <- paste0("\n<h3>", tempdf$date, ": ", tempdf$time, 
                                       "</h3>\n", d$get_description())
          tempdf$Name <- paste(stringr::str_pad(lubridate::day(tempdf$date), 
                                                width = 2, pad = "0"),
                               tempdf$time)
        }
        return(tempdf)
      }))
    }))
  })
  return(latlon)
}

create_saufley_google_earth <- function(latlon, outpath)
{
  # latlon <- create_latlon_list("docs")
  # outpath <- file.path("src", "kml")
  
  include_series <- which(sapply(latlon, function(z) ifelse(is.null(z$date), FALSE, TRUE)))
  
  # need to put the include_series in date order
  first_dates <- do.call("c", lapply(include_series, function(z) latlon[[z]]$date[1]))
  
  available_years <- 1942:1945
  year_color <- c("orange", "red", "yellow", "purple")
  
  if (!dir.exists(outpath))
    dir.create(outpath)
  
  # loop over years
  for (k in seq_along(available_years))
  {
    cat("\n", available_years[k])
    # k <- 2
    ind <- which(lubridate::year(first_dates) == available_years[k])
    
    if (length(ind) == 0)
      next
    
    year_file <- file.path(outpath, paste0("test", available_years[k], ".kml"))
    series_in_year <- include_series[ind]
    dates_in_year <- first_dates[ind]
    
    ord <- order(dates_in_year)
    
    #plotKML::kml_open(year_file, folder.name = available_years[k])
    kmlout <- new_kml_open(year_file, folder.name = available_years[k])
    
    for (i in include_series[ind][ord])
    {
      cat("\n", as.character(latlon[[i]]$date[1]), "\n")
      # i <- 21
      plot_data <- latlon[[i]]
      plot_coords <- plot_data %>% dplyr::select(lon, lat) # must go lon then lat (x then y)
      
      if (any(is.na(as.matrix(plot_coords))))
      {
        cat("\nSkipping Output of ", as.character(latlon[[i]]$date[1]), "\n")
        next
      }
      
      sf_points <- sf::st_sfc(sf::st_multipoint(as.matrix(plot_coords), dim = "XY"))
      sf_points <- sf::st_set_crs(sf_points, 4326)
      
      point_time <- strptime(paste(plot_data$date, plot_data$time), 
                             format = "%Y-%m-%d %H%M", tz = "GMT")
      
      ym <- paste0(lubridate::year(plot_data$date[1]), "-",
                   stringr::str_pad(lubridate::month(plot_data$date[1]), 
                                    width = 2, pad = "0"))
      
      kmlout <- new_kml_layer_sfc_POINT(
        sf::st_cast(sf_points, "POINT"),
        kml.out = kmlout,
        subfolder.name = paste(ym, "Locations"), 
        points_names = plot_data$Name, 
        html.table = plot_data$Description,
        TimeSpan.begin = format(point_time, "%Y-%m-%dT%H:%M:%SZ"),
        TimeSpan.end = format(point_time + 60, "%Y-%m-%dT%H:%M:%SZ"),
        my_colors = plotKML::col2kml(year_color[k]),
        shape = "http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png")
      
      # first lines
      sf_lines <- sf::st_linestring(as.matrix(plot_coords[1:2,]), dim = "XY")
      sf_names <- plot_data$Name[1]
      sf_times <- point_time[1]
      # add lines for connections between points which are nearby in time (< 2 days),
      #   but not in the same place
      for (j in 2:(nrow(plot_data) - 1))
      {
        # j <- 2
        if (!all(plot_coords[j,] == plot_coords[j+1,]) &
            (as.numeric(point_time[j+1]) - as.numeric(point_time[j])) < 60*60*24*2)
        {
          sf_lines <- c(sf_lines, sf::st_linestring(as.matrix(plot_coords[j:(j+1),]), dim = "XY"))
          sf_names <- c(sf_names, plot_data$Name[j])
          sf_times <- c(sf_times, point_time[j])
        }
      }
      sf_lines <- sf::st_sfc(sf_lines)
      
      sf_lines <- sf::st_set_crs(sf_lines, 4326)
      
      kmlout <- new_kml_layer_sfc_LINESTRING(
        sf::st_cast(sf_lines, "LINESTRING"),
        kml.out = kmlout,                      
        subfolder.name = paste(ym, "Routes"),
        TimeSpan.begin = format(sf_times, "%Y-%m-%dT%H:%M:%SZ"),
        TimeSpan.end = format(sf_times + 60, "%Y-%m-%dT%H:%M:%SZ"),
        my_colors = plotKML::col2kml(year_color[k]),
        my_labels = sf_names)
    }

    new_kml_close(year_file, kml.out = kmlout)
  }
  
  utils::zip(file.path(outpath, "Track_of_USS_Saufley.kmz"), 
             files = file.path(outpath, c("Track_of_USS_Saufley.kml", 
                                          paste0(list.files(outpath, pattern = "[2-5][.]kml")))))
}

create_leaflet_year_plot <- function(plot_data_full, output_file_base, 
                                     str_date_start, str_date_end,
                                     b_save = TRUE)
{
  # plot_data_full <- do.call("rbind", latlon)
  # plot_data_full$datetime <- with(plot_data_full, paste0(date, ": ", time))
  # str_date_start <- "1943-01-01"
  # str_date_end <- "1943-12-31"
  
  #### this code plots all points and lines
  # myear <- leaflet::leaflet(data = plot_data_full %>%
  #                             dplyr::filter(date >= as.Date(str_date_start), 
  #                                           date <= as.Date(str_date_end))) %>% 
  #   leaflet::addProviderTiles(leaflet::providers$Esri.NatGeoWorldMap) %>%
  #   leaflet::addCircleMarkers(lng = ~lon, lat = ~lat,
  #                             popup = ~Description, label = ~datetime,
  #                             color = "darkblue", radius = 5) %>%
  #   leaflet::addPolylines(lng = ~lon, lat = ~lat,
  #                         color = "gold", weight = 3, opacity = 0.5, fill = FALSE)
  
  #### this code subsets to non-overlapping points and only plots lines for well connected points
  point_subset <- plot_data_full %>%
    dplyr::filter(date >= as.Date(str_date_start), 
                  date <= as.Date(str_date_end))
  ind <- NULL
  for (i in 2:nrow(point_subset))
  {
    if (is.na(point_subset$lat[i]) ||
        is.na(point_subset$lon[i]) ||
        (!is.na(point_subset$lat[i-1]) &&
         !is.na(point_subset$lon[i-1]) &&
         point_subset$lat[i] == point_subset$lat[i-1] &&
         point_subset$lon[i] == point_subset$lon[i-1]))
    {
      ind <- c(ind, i)
    }
  }
  if (length(ind) > 0)
    point_subset <- point_subset[-ind,]
  
  myear <- leaflet::leaflet(data = plot_data_full %>%
                              dplyr::filter(date >= as.Date(str_date_start), 
                                            date <= as.Date(str_date_end))) %>% 
    leaflet::addProviderTiles(leaflet::providers$Esri.NatGeoWorldMap) %>%
    leaflet::addCircleMarkers(data = point_subset, lng = ~lon, lat = ~lat,
                              popup = ~Description, label = ~datetime,
                              color = "darkblue", radius = 5)
  
  line_subset <- plot_data_full %>%
    dplyr::filter(date >= as.Date(str_date_start), 
                  date <= as.Date(str_date_end)) %>%
    dplyr::arrange(date)

  posit <- 2
  while (posit < nrow(line_subset))
  {
    ind <- NULL
    for (i in posit:nrow(line_subset))
    {
      if (!is.na(line_subset$lat[i]) &&
          !is.na(line_subset$lat[i-1]) &&
          !is.na(line_subset$lon[i]) &&
          !is.na(line_subset$lon[i-1]) &&
          line_subset$lat[i] != line_subset$lat[i-1] &&
          line_subset$lon[i] != line_subset$lon[i-1] && 
          line_subset$date[i] - line_subset$date[i-1] <= 2)
        ind <- c(ind, i)
      else
        break
    }
    if (length(ind) >= 2)
    {
      myear <- myear %>%
        leaflet::addPolylines(data = line_subset[ind,], lng = ~lon, lat = ~lat,
                              color = "gold", weight = 3, 
                              opacity = 0.5, 
                              fill = FALSE,
                              stroke = c(rep(TRUE, length(ind) - 1), FALSE))
    }
    posit <- i+1
  }
  
  if (b_save)
  {
    htmlwidgets::saveWidget(myear, paste0(output_file_base, ".html"), selfcontained = FALSE)
  } else 
  {
    print(myear)
  }
}

create_leaflet_plots <- function(latlon, outpath, b_save = TRUE)
{
  assertthat::assert_that(webshot::is_phantomjs_installed(),
                          msg = "Phantom_js must be installed, run 'webshot::install_phantomjs()'")
  if (!dir.exists(outpath))
  {
    dir.create(outpath)
  }

  # add 360 to the negative longitudes so the plot wraps across the international
  #   dateline
  latlon <- lapply(latlon, function(x) {
    ind <- which(x$lon < 0)
    if (length(ind > 0))
    {
      x$lon[ind] <- x$lon[ind] + 360
    }
    return(x)
  })
  
  plot_data_full <- do.call("rbind", latlon)
  plot_data_full$datetime <- with(plot_data_full, paste0(date, ": ", time))

  ### Overall Heatmap
  heatm <- leaflet(data = plot_data_full %>%
                          dplyr::filter(!is.na(lat) & !is.na(lon)),
                   options = leafletOptions(zoomControl = FALSE)) %>%
    #leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>%
    leaflet::addProviderTiles(leaflet::providers$Stamen) %>%
    leaflet::addCircleMarkers(lng = ~lon, lat = ~lat, radius = 2, fill = TRUE, stroke = FALSE) %>%
    leaflet::fitBounds(lng1 = 90, lng2 = 220, lat1 = -38, lat2 = 38)

  ### Yearly Plots
  create_leaflet_year_plot(plot_data_full, file.path(outpath, "map1942"), 
                           "1942-01-01", "1942-12-31", b_save)
  create_leaflet_year_plot(plot_data_full, file.path(outpath, "map1943"), 
                           "1943-01-01", "1943-12-31", b_save)
  create_leaflet_year_plot(plot_data_full, file.path(outpath, "map1944"), 
                           "1944-01-01", "1944-12-31", b_save)
  create_leaflet_year_plot(plot_data_full, file.path(outpath, "map1945"), 
                           "1945-01-01", "1945-12-31", b_save)
  
  if (b_save)
  {
    heatm %>%
      htmlwidgets::saveWidget(file.path(outpath, "location_heatmap.html"), 
                              selfcontained = TRUE)
    
    webshot::webshot(url = file.path(outpath, "location_heatmap.html"), 
                     file = file.path(outpath, "location_heatmap.png"), 
                     zoom = 10, vwidth = 1000, vheight = 150)
  } else
  {
    print(heatm)
  }
}


deploy_leaflet_plots <- function(path_to_html, outpath)
{
  # transfer javascript dependencies
  file.copy(from = list.dirs(file.path(outpath, "map1942_files"))[-1], to = file.path(path_to_html, "javascripts"), recursive = TRUE)
  
  # create new html from template
  # read the target html
  target_html_filename <- file.path(path_to_html, "saufley_patrol_track_template.html")
  output_html_filename <- file.path(path_to_html, "saufley_patrol_track.html")
  target_html <- readLines(target_html_filename)
  # read the year htmls
  html_1942 <- readLines(file.path(outpath, "map1942.html"))
  html_1943 <- readLines(file.path(outpath, "map1943.html"))
  html_1944 <- readLines(file.path(outpath, "map1944.html"))
  html_1945 <- readLines(file.path(outpath, "map1945.html"))
  
  # find the tags to be replaced in the template
  cut_ind <- c(grep("<!-- Replace with 1942 -->", target_html),
               grep("<!-- Replace with 1943 -->", target_html),
               grep("<!-- Replace with 1944 -->", target_html),
               grep("<!-- Replace with 1945 -->", target_html))
  befores <- cut_ind - 1
  afters <- cut_ind + 1
  
  # find the section to replace it with
  starts <- c(grep("htmlwidget_container", html_1942),
              grep("htmlwidget_container", html_1943),
              grep("htmlwidget_container", html_1944),
              grep("htmlwidget_container", html_1945))
  ends <- c(grep("application/json", html_1942),
            grep("application/json", html_1943),
            grep("application/json", html_1944),
            grep("application/json", html_1945))
  
  # remake the new file
  output_html <- c(
    "<!-- This File is Auto Generated from saufley_patrol_track_template.html.  Do not edit directly -->",
    target_html[1:befores[1]],
    html_1942[starts[1]:ends[1]],
    target_html[afters[1]:befores[2]],
    html_1943[starts[2]:ends[2]],
    target_html[afters[2]:befores[3]],
    html_1944[starts[3]:ends[3]],
    target_html[afters[3]:befores[4]],
    html_1945[starts[4]:ends[4]],
    target_html[afters[4]:length(target_html)]
  )

  # save the file
  writeLines(output_html, output_html_filename)
  
  # Remove generated files
  file.remove(file.path(outpath, c("map1942.html", "map1943.html", "map1944.html", "map1945.html")))
  unlink(file.path(outpath, "map1942_files"), recursive = TRUE)
  unlink(file.path(outpath, "map1943_files"), recursive = TRUE)
  unlink(file.path(outpath, "map1944_files"), recursive = TRUE)
  unlink(file.path(outpath, "map1945_files"), recursive = TRUE)
  
  file.copy(file.path(outpath, "location_heatmap.png"), file.path(path_to_html, "images"), overwrite = TRUE)
  file.remove(file.path(outpath, c("location_heatmap.png", "location_heatmap.html")))
  unlink(file.path(outpath, "location_heatmap_files"), recursive = TRUE)
}
