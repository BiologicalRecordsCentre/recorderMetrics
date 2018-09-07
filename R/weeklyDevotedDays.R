#' Metrics identifying the breadth and proportion of taxa recorded 
#' 
#' These metrics describe the 'expericence' the recorder has had recording species within the group.
#'
#' @param recorder_name the name of the recorder for whom you want to calculate the metrics
#' @param data the data.frame of recording information
#' @param recorder_col the name of the column that contains the recorder names
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
#' 
#' WDD <- weeklyDevotedDays(recorder_name = 3007,
#'                          data = cit_sci_data,
#'                          recorder_col = 'recorder',
#'                          date_col = 'date')
#' 
#' head(WDD)
#' 
#' # Run for more than one recorder, this can be slow 
#' WDD_all <- lapply(unique(cit_sci_data$recorder),
#'                   FUN = weeklyDevotedDays, 
#'                   data = cit_sci_data,
#'                   recorder_col = 'recorder',
#'                   date_col = 'date')
#' 
#' # summarise as one table
#' WDD_all_sum <- do.call(rbind, WDD_all)
#' 
#' hist(WDD_all_sum$median_weekly_devoted_days)
#' 
#' } 
#' 
#' @return 
#' \itemize{
#'  \item{\code{recorder} - }{The name of the recorder, as given in the recorder_name argument}
#'  \item{\code{median_weekly_devoted_days} - }{The median number of days this person recorders in a weeks where they make at least one observation}
#'  \item{\code{n_weeks} - }{The total number of weeks in which this recorder has made at least one observation}
#'  \item{\code{n_days} - }{The total number of days on which this recorder has made observations}
#' }
#' 


weeklyDevotedDays <- function(recorder_name,
                              data,
                              recorder_col = 'recorders',
                              date_col = 'date_start'){
  
  # check date column
  if(!inherits(data[, date_col], 'Date')){
    stop('Your date column is not a date')
  }
  
  # Get the recorders data
  data <- data[data[,recorder_col] == recorder_name, ]
  
  # Get unique dates as dates
  dates <- unique(data[,date_col])
  
  # Get all week_year combinations
  week_year <- paste(strftime(as.POSIXlt(dates), format = '%W'),
                     format(dates, '%Y'), sep = '_')
  
  # here are the counts
  week_counts <- table(week_year)
  
  # As these are counts taking the median is probably best
  weekly_devoted_days <- median(week_counts)
  
  return(data.frame(recorder = recorder_name,
                    median_weekly_devoted_days = weekly_devoted_days,
                    n_weeks = length(week_counts),
                    n_recs = sum(week_counts), row.names = NULL))
}
