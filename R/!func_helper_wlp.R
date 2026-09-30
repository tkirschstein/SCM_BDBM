
# helper function tour generation ###############
tour2mat.func <- function(tour) cbind(tour, c(tail(tour, -1), tour[1]))  

# reorder tour to start with depot ###############
reorder.tour.func <-  function(x, dep.id = 1){
  tmp.depot.id <- which(x == dep.id)
  if(tmp.depot.id != dep.id){
    x <- c(x[tmp.depot.id:length(x)], x[1:(tmp.depot.id-1)]  )
  }
  return(x)
}


# get isochrones ###########################
get.isochrones.func <- function(coords = c(13.331273,38.10849), api.key = "58d904a497c67e00015b45fcabf19c2776ed48769cc4c98594f7ad11", ors.url.api = "https://api.openrouteservice.org/v2/isochrones/driving-car", range = 600, range.type = "time"){
  # range in seconds or meters (also vector)
  # range.type "time" or "distance" (also vector)
  
  tmp <- POST(url = ors.url.api, body = list("locations" = list(coords), range = as.list(range), range_type = range.type, smoothing = 1, attributes = list("area","reachfactor","total_pop") ), accept("application/json, application/geo+json, application/gpx+xml, img/png; charset=utf-8"), content_type("application/json; charset=utf-8"),  add_headers(Authorization = api.key), encode = "json")    
  
  tmp.res <- fromJSON(content(tmp,"text"))  
  tmp.coords <- tmp.res$features$geometry$coordinates
  tmp.coords <- tmp.coords[[1]][,,1:2]
  
  tmp.features <- tmp.res$features$properties
  
  return(list(coords = tmp.coords, feat = tmp.features)) #  raw.res = tmp.res
}

# get elevation  ########################
elevation.get.func <- function(coords= c(13.349762,38.11295), api.key = "5b3ce3597851110001cf624879c098c57d1f440fadb63ba89d59e491", format.out = c("geojson", "point"), ors.url.api = "https://api.openrouteservice.org/elevation/point"){
  
  format.out <- match.arg(format.out)
  
  tmp.coord.substr <- paste(coords, collapse = ",")
  tmp <- GET(paste(ors.url.api, "?api_key=", api.key, "&geometry=", tmp.coord.substr, "&format_out=", format.out, sep="") )
  if(format.out == "point" ){
    tmp <- as.character(unlist(strsplit(content(tmp,"text"), "geometry", fixed=T))[2])
    tmp <- unlist(strsplit(tmp,"[", fixed=T))[2]
    tmp <- unlist(strsplit(tmp,"]", fixed=T))[1]
    tmp <- as.numeric(unlist(strsplit(tmp,",", fixed=T))[3])
  }
  
  return(tmp)
}

# get routable adress  ########################
address.routable.get.func <- function(lat, lon, api.key = "58d904a497c67e00015b45fcabf19c2776ed48769cc4c98594f7ad11", ors.url.api = "https://api.openrouteservice.org/geocode/reverse?"){
  
  # search for coordinates of closest address
  url <- paste0(
    ors.url.api,
    "api_key=", api.key,
    "&point.lat=", lat,
    "&point.lon=", lon,
    "&size=1",
    "&layers=street",
    #    "&resolve_location=true",
    "&boundary.circle.radius=2000"
  )
  
  tmp <- GET(url)
  
  # decode result
  tmp.res <- fromJSON(content(tmp,"text"))  
  
  # retrieve coordinates
  tmp.coords <- tmp.res$features$geometry$coordinates[[1]]
  
  return(tmp.coords)
}

# direction ###########################
# Waycategory/waytype definition
# https://github.com/GIScience/openrouteservice-docs#waycategory

shortest.path.get.func <- function(pts.mat = rbind(c(13.331273,38.10849),c(13.33,38.12)), api.key = "58d904a497c67e00015b45fcabf19c2776ed48769cc4c98594f7ad11", ors.url.api = "https://api.openrouteservice.org/v2/directions/driving-car", add.elevation = FALSE, pref = c("recommended", "shortest"), retry_limit = 3, wait_between = 2){
  
  pref <- match.arg(pref)
  
  pts.mat <- as.matrix(pts.mat)
  
  tmp.pts <- lapply(seq_len(nrow(pts.mat)), function(x) pts.mat[x,] )
  
  body <- list(
    coordinates = tmp.pts,
    radiuses = c(-1, -1),  # maximum search radius for routable addresses
    elevation = ifelse(add.elevation, "true", "false"),
    instructions = "false", # no instructions
    maneuvers = "false",  # no manouveres
    preference = pref , 
    extra_info = list("waytype","waycategory"),
    attributes = list("avgspeed")
  )
  
  # Convert the body to JSON
  body_json <- toJSON(body, auto_unbox = TRUE)
  attempt <- 1
  while (attempt <= retry_limit) {
    # Make the POST request
    tmp <- POST(
      url = ors.url.api,
      add_headers(
        "Content-Type" = "application/json",
        "Authorization" = api.key
      ),
      body = body_json
    )
    
    if (status_code(tmp) == 200) {
      tmp.res <- fromJSON(content(tmp,"text"))  
      tmp.path <- decodePolyline(tmp.res$routes$geometry)
      tmp.dist <- tmp.res$routes$summary$distance/1000 # in km
      tmp.dur <- tmp.res$routes$summary$duration/60/60       # in h
      tmp.speed <- tmp.dist/tmp.dur
      
      return(list(path = tmp.path, dist = tmp.dist, dur = tmp.dur, speed = tmp.speed)) 
      
    } 
    else {
      # Handle the error case
      # 429 - Rate limit exceeded
      if (status_code(tmp) == 429) {
        retry_after <- headers(tmp)[["Retry-After"]]
        wait_time <- ifelse(!is.null(retry_after), as.numeric(retry_after), wait_between)
        Sys.sleep(wait_time)
      } else {
        # Wait a bit before next try for other errors
        Sys.sleep(wait_between)
      }
      attempt <- attempt + 1
    }
  }
  return(NULL) # Handle the error case
}


# matrix ###########################
shortest.path.matrix.func <- function(pts.mat = rbind(c(13.331273,38.10849),c(13.33,38.12),c(13.31,38.08)), api.key = "58d904a497c67e00015b45fcabf19c2776ed48769cc4c98594f7ad11", ors.url.api = "https://api.openrouteservice.org/v2/matrix/driving-car"){
  
  pts.mat <- as.matrix(pts.mat)
  tmp.pts <- lapply(seq_len(nrow(pts.mat)), function(x) pts.mat[x,] )
  tmp.nb.pts <- length(tmp.pts)
  
  if(tmp.nb.pts^2 > 3500){
    
    tmp.nb.col <- floor(3500/tmp.nb.pts)
    tmp.sub.list.ind <- split(seq_len(tmp.nb.pts), ceiling(seq_len(tmp.nb.pts)/tmp.nb.col))
  }
  else{
    tmp.sub.list.ind <- list(1:tmp.nb.pts)
  }
  
  
  tmp.res <- lapply(tmp.sub.list.ind, function(x){
    
    tmp <- POST(url = ors.url.api, body = list("locations" = tmp.pts, "destinations" = as.list(x-1), metrics = list("distance","duration") ),accept("application/json, application/geo+json, application/gpx+xml, img/png; charset=utf-8"), content_type("application/json; charset=utf-8"),  add_headers(Authorization = api.key), encode = "json")    
    
    tmp.res <- fromJSON(content(tmp,"text"))  
    tmp.dist <- tmp.res$distances/1000 # in km
    tmp.dur <- tmp.res$durations/60/60       # in h
    
    list(dist = tmp.dist , dur = tmp.dur)
  })
  
  tmp.dist <- do.call("cbind", lapply(tmp.res , FUN = '[[', "dist"))
  tmp.dur <- do.call("cbind", lapply(tmp.res , FUN = '[[', "dur"))
  tmp.speed <- tmp.dist/tmp.dur
  
  return(list(dist = tmp.dist, dur = tmp.dur, speed = tmp.speed)) #  raw.res = tmp.res
}


ors_drive_distance_matrix <- function(origins, destinations, api_key= "58d904a497c67e00015b45fcabf19c2776ed48769cc4c98594f7ad11") {
  
  # Validate inputs
  if (is.null(api_key)) stop("API key is required.")
  if (ncol(origins) != 2 || ncol(destinations) != 2) stop("Coordinates must be 2-column (lon, lat).")
  
  # Build coordinate list
  coordinates <- rbind(origins, destinations)
  coord_list <- unname(split(coordinates, seq(nrow(coordinates))))
  
  # Indexing: origins and destinations are 0-based indexes
  sources <- seq(0, nrow(origins) - 1)
  destinations_idx <- seq(nrow(origins), nrow(origins) + nrow(destinations) - 1)
  
  # Construct body for POST request
  body <- list(
    locations = coord_list,
    sources = sources,
    destinations = destinations_idx,
    metrics = list("distance"),
    units = "km"
  )
  
  # Make POST request to OpenRouteService matrix endpoint
  response <- httr::POST(
    url = "https://api.openrouteservice.org/v2/matrix/driving-car",
    add_headers("Authorization" = api_key, "Content-Type" = "application/json"),
    body = toJSON(body, auto_unbox = TRUE)
  )
  
  # Error handling
  if (http_error(response)) {
    stop("API request failed: ", content(response, "text", encoding = "UTF-8"))
  }
  
  # Parse and return distance matrix
  result <- content(response, as = "parsed", simplifyVector = TRUE)
  return(result$distances)
}



