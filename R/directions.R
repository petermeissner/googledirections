#' a aplace to store google directions api requests for the duration of one R session
#' @export
gdar_cache <-  new.env(parent=emptyenv())

#' requesting information on google directions api
#' @export
google_directions_api_request <- function(
    origin,
    destination,
    mode           = c("transit","driving","walking","bicycling"),
    arrival_time   = next_workday_at(),
    departure_time = NULL,
    output         = c("json","xml"),
    units          = c("metric", "imperial"),
    language       = "de",
    params         = list(),
    server_key     = get_server_key(),
    force          = FALSE
  ){
    url <- paste0("https://maps.googleapis.com/maps/api/directions/", output[1])

    params$key         = server_key
    params$origin      = origin
    params$destination = destination
    params$units       = units[1]
    params$language    = language

    params$mode        = mode[1]
    if( !is.null(arrival_time) & !is.null(departure_time) & params$mode=="transit"){
      stop("do not use arrival_time and departure_time at the same time")
    }
    if( !is.null(arrival_time) & params$mode=="transit"  ){
      params$arrival_time <- arrival_time
    }
    if( !is.null(departure_time) & params$mode=="transit" ){
      params$departure_time <- departure_time
    }


    # caching and request
    name <- paste0("cache_",digest::digest(params))
    if(
      is.null(gdar_cache[[name]]) |
      force == TRUE
    ){
      request <- httr::GET(url, query=params)
      if( httr::status_code(request)==200 ) {
        gdar_cache[[name]] <- request
      }
    }else{
      request <- gdar_cache[[name]]
    }
    request
}

#' function providing google api key
#' @param path path that leads to file from which to read api key
#' @export
get_server_key <- function(path=path.expand("~/.google_directions.api_key")){
  readLines(path)[1]
}

#' function for getting duration out of api request
#' @param request result of an httr::GET request - usually returned by google_directions_api_request()
#' @export
duration <- function(request){
  as.data.frame(
    lapply(
      httr::content(dings)$routes[[1]]$legs,
      '[[', "duration"
    ),
    stringsAsFactors = FALSE
  )
}




















