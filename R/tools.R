#' next working day
#' @param h which hour to use as default for time stamp to return
#' @export
next_workday_at <- function(h=7) {
  as.numeric(as.POSIXct(Sys.Date()+min((1:7)[(as.POSIXlt((Sys.Date()+1:7))$wday %in% 1:5)])))+h*60*60
}
