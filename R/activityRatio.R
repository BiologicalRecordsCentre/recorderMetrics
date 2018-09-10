#' Calculate activity ratio metrics
#' 
#' This function takes in data for a recorder and calculates the activity ratio, total duration and number of active days
#' 
#' @param recorder_name the name of the recorder for whom you want to calculate the metrics
#' @param data the data.frame of recording information
#' @param recorder_col the name of the column that contains the recorder names
#' @param date_col the name of the column that contains the date. This must be formatted as a date
#' @param summer_days Activity ratio is only calculated for the summmer period is this parameter is provided. This is a three column data.frame: year, Jday_start (numeric Julian day of the first day of summer), Jday_end (numeric Julian day of the last day of summer). These are returned as an attribute from \code{summerData}.
#'  
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' # load example data
#' head(cit_sci_data)
#' 
#' # Get the summer period
#' SD <- summerData(cit_sci_data, date_col = 'date')
#' 
#' ar <- activityRatio(recorder_name = 3007,
#'                     data = cit_sci_data,
#'                     recorder_col = 'recorder',
#'                     date_col = 'date',
#'                     summer_days = attr(SD, 'cutoffs'))
#'                     
#' # Run the metric for all recorders
#' ar_all <- lapply(unique(cit_sci_data$recorder),
#'                  FUN = activityRatio,
#'                  data = cit_sci_data,
#'                  recorder_col = 'recorder',
#'                  date_col = 'date',
#'                  summer_days = attr(SD, 'cutoffs'))
#'
#' # summarise as one table
#' ar_all_sum <- do.call(rbind, ar_all)
#'
#' hist(ar_all_sum$active_days, breaks = 80)
#' }
#' 
#' @return A data.frame with four columns
#' \itemize{
#'  \item{\code{recorder} - }{The name of the recorder, as given in the recorder_name argument}
#'  \item{\code{activity_ratio} - }{The proportion of days on which the volunteer was active in relation to the total days he/she remained linked to the project (i.e. \code{active_days} / \code{duration}. Duration is either the number of days elapsed from their first to last record, or if \code{summer_days} is provided it is the the number of days elapsed from their first to last record that fall into the summer periods)}
#'  \item{\code{total_duration} - }{The total number of days the volunteer was involved in the project, calculated as the number of days from the first record they submitted to the last (inclusive)}
#'  \item{\code{active_days} - }{The total number of unique dates on which observations were made}
#' }

activityRatio <-
function(recorder_name,
         data,
         recorder_col = 'recorders',
         date_col = 'date_start',
         summer_days = NULL){
  
  # check date column
  if(!inherits(data[, date_col], 'Date')){
    stop('Your date column is not a date')
  }
  
  # check name
  if(!recorder_name %in% data[,recorder_col]) stop(paste(recorder_name, 'does not appear in the recorder column of your data'))
  
  # Get the recorders data
  data <- data[data[,recorder_col] == recorder_name, ]
  
  # Some people might have no data from the summer period
  if(nrow(data) < 1){
    
    return(data.frame(recorder = recorder_name,
                      activity_ratio = NA,
                      total_duration = NA,
                      active_days = NA))
  } else {
    
  # Get unique dates
  dates <- unique(data[,date_col])
  
  # Get the first and last date
  first_last <- range(dates)
  
  # Total duration of this recorder
  duration <- as.numeric(first_last[2] - first_last[1]) + 1 
  summer_duration <- NA
  
  if(is.null(summer_days)){
    
    # calculate ratio
    activity_ratio <- length(dates)/duration
    
  } else {
    
    # This returns a vector is dates that represent a summer for a given year
    SDconv <- function(x){
      d0 <- as.Date(paste(as.character(x[1]), '01', '01', sep = '-'))-1
      s <- d0 + x[2]
      e <- d0 + x[3]
      days <- seq(s, e, by = 'days')
      return(days)
    }
    
    # Apply the function across all years
    summer_dates <- apply(X = summer_days,
                          MARGIN = 1,
                          FUN = SDconv)
    
    # Join the result together
    summer_dates <- do.call(c, summer_dates)
    
    # Compare to a vector of all dates a record we around and sum the intersect
    # this is the total number of summer days passed from beginning to end of 
    # the recoder's engagement
    summer_duration <- sum(summer_dates %in% seq(first_last[1], first_last[2], by = 'days'))
    
    # calculate ratio
    activity_ratio <- length(dates)/summer_duration
    
  }
    
  # return
  return(data.frame(recorder = recorder_name,
                    activity_ratio = activity_ratio,
                    total_duration = duration,
                    summer_duration = summer_duration,
                    active_days = length(dates)))
  }
}
