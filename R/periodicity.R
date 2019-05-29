#' Calculate periodicity metrics
#' 
#' This function takes in data for a recorder and calculates the periodicity metrics. 
#' 
#' @param recorder_name the name of the recorder for whom you want to calculate the metrics
#' @param data the data.frame of recording information
#' @param date_col the name of the column that contains the date. This must be formatted as a date
#' @param recorder_col the name of the column that contains the recorder names
#' @param day_limit the threshold number of days a recorder must be active before these metrics are estimated. If the number of active days for the recorder is less than this number then the function will return NA values.
#'    
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' # load example data
#' head(cit_sci_data)
#' 
#' # run for one recorder
#' P <- periodicity(recorder_name = 3007,
#'                 data = cit_sci_data,
#'                 date_col = 'date',
#'                 recorder_col = 'recorder',
#'                 day_limit = 5)
#'
#' # Run the metric for all recorders
#' P_all <- lapply(unique(cit_sci_data$recorder),
#'                FUN = periodicity,
#'                data = cit_sci_data,
#'                date_col = 'date',
#'                recorder_col = 'recorder',
#'                day_limit = 5)
#'
#' # summarise as one table
#' P_all_sum <- do.call(rbind, P_all)
#'
#' hist(P_all_sum$max_streak)
#' }
#' 
#' @details In this function a streak is defined as a series of consecutive days on which a person made observations. A streak of 1 is a single days recording in isolation. 2 is 2 days back-to-back, 3 is 3 days in a row and so on.
#' 
#' @return A data.frame with seven columns
#' \itemize{
#'  \item{\code{recorder} - }{The name of the recorder, as given in the recorder_name argument}
#'  \item{\code{periodicity} - }{The median number of days elapsed between each pair of sequential active days. This describes the regularity with which people record.}
#'  \item{\code{periodicity_variation} - }{The standard deviation of the times elapsed between each pair of sequential active days}
#'  \item{\code{median_streak} - }{The median length of streaks, including streak lengths of 1}
#'  \item{\code{sd_streak} - }{The standard deviation of streaks lengths, including streak lengths of 1}
#'  \item{\code{max_streak} - }{the length of this recorders longest streak}
#'  \item{\code{n_days} - }{The number of dates on which this recorder made observations}
#' }

periodicity <- function(recorder_name,
                        data,
                        date_col = 'date_start',
                        recorder_col = 'recorders',
                        day_limit = 5){
  
  # check date column
  if(!inherits(data[, date_col], 'Date')){
    stop('Your date column is not a date')
  }
  
  # Get the recorders data
  data <- data[data[,recorder_col] == recorder_name, ]
  
  # Get unique dates as dates
  dates <- sort(unique(data[,date_col]))
  
  # we cannot calculate these metrics if people have very few
  # dates on which they record
  if(length(unique(dates)) < day_limit){
    
    # return
    return(data.frame(recorder = recorder_name,
                      periodicity = NA,
                      periodicity_variation = NA,
                      median_streak = NA,
                      sd_streak = NA,
                      max_streak = NA,
                      n_days = length(unique(dates))))
      
  } else {
    
    # Calculate the elapsed days between each date in sequence
    # this needs to be done within years
    elapses <- NULL
    
    for(year in unique(format(dates, '%Y'))){
      
      temp_dates <- dates[format(dates, '%Y') == year]
       
      # There must be at least 2 dates in a year
      if(length(temp_dates) > 1){
        temp_elapses <- sapply(1:(length(temp_dates)-1),
           FUN = function(x){
             return(as.numeric(temp_dates[x + 1] - temp_dates[x]))
           })  
      
        elapses <- c(elapses, temp_elapses)
      
      }
      
    }
  
    # periodicity calculation
    periodicity <- median(elapses)
    
    # variation in periodicity
    periodicity_variation <- sd(elapses)
    
    # average streak length
    # Streaks are IDed by 1's
    non_streak <- length(elapses[elapses > 1])
    streaks <- rle(elapses)
    streaks_1 <- (streaks$lengths[streaks$value == 1]) + 1
    
    # Combine streaks and non-streaks
    streak_lengths <- c(rep(1, non_streak), streaks_1)
    
    # calculate ome metrics
    median_streak <- median(streak_lengths)
    sd_streak <- sd(streak_lengths)
    max_streak <- max(streak_lengths)
    
    # return
    return(data.frame(recorder = recorder_name,
                      periodicity = periodicity,
                      periodicity_variation = periodicity_variation,
                      median_streak = median_streak,
                      sd_streak = sd_streak,
                      max_streak = max_streak,
                      n_days = length(unique(dates))))
    
  }
  
  
}
