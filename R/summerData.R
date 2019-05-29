#' Identify data that is recorded during the summer period
#' 
#' Recorder metrics can be biased if there are annual breaks in data collection. In these cases it is better to ensure that only data in the recording period (typically summer), is included. This function is an objective way to identify the recording period.
#' 
#' @param input_data the data.frame of recording information
#' @param probs A vector of two proportions giving the positions of the start and end of summer. Default value of 0.025 and 0.975 mean that the central 95 percent of day in any year is classed as the recording period.
#' @param date_col the name of the column that contains the date. This must be formatted as a date
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' # load example data
#' head(cit_sci_data)
#' 
#' # Subset this data to summer periods only
#' SD <- summerData(input_data = cit_sci_data,
#'                  probs = c(0.025, 0.975),
#'                  date_col = 'date')
#' 
#' head(SD)
#' 
#' # Data not in the summer period is removed
#' nrow(cit_sci_data)
#' nrow(SD)
#'
#' # The cutoffs used to define summer are also returned
#' attr(SD, which = 'cutoffs')
#' }
#' 
#' 
#' @return Only data identified as from the the primary recording period (e.g. summer) is returned. Three additional columns are returned.
#' \itemize{
#'  \item{\code{Jday} - }{The day of the year as a numeric value, the first day of the year being 1, the second 2 and so on}
#'  \item{\code{year} - }{The year of the record in the format YYYY}
#'  \item{\code{summer} - }{Logical, does this record fall in the summer period (i.e. the annual period of heightened recording)}
#' }
#' 
#' The returned object has an attribute \code{cutoffs} which details the days (\code{Jday}) used as the first and last days of summer in each year. 


summerData <- function(input_data,
                       probs = c(0.025, 0.975),
                       date_col = 'date_start'){
  
  # check date column
  if(!inherits(input_data[, date_col], 'Date')){
    stop('Your date column is not a date')
  }
  if(any(is.na(input_data[, date_col]))){
    stop('Your date column contains NA values')
  }
  
  # check probs
  if(length(probs) != 2) stop('probs must be of length 2')
  stopifnot(is.numeric(probs))
  
  # create J-day column
  input_data$Jday <- as.POSIXlt(input_data[,date_col])$yday
  
  # create year column
  input_data$year <- as.POSIXlt(input_data[,date_col])$year+1900
  
  # create summer column
  input_data$summer <- FALSE
  
  qsf <- qsl <- NULL  
  
  # now for each year loop through and create an 
  # index column
  for(i in sort(unique(input_data$year))){
    
    year_quantiles <- quantile(input_data$Jday[input_data$year == i], probs = probs)
    qsf <- c(qsf, year_quantiles[1])
    qsl <- c(qsl, year_quantiles[2])
    input_data$summer[input_data$Jday >= year_quantiles[1]
                & input_data$Jday <= year_quantiles[2] 
                & input_data$year == i] <- TRUE
  }
  
  summer_data <- input_data[input_data$summer, ]
  attr(summer_data, 'cutoffs') <- data.frame(year = sort(unique(input_data$year)),
                                             quantile_first = qsf,
                                             quantile_last = qsl)  
  return(summer_data)
}
