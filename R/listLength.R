#' Calculate list length metrics
#' 
#' This function takes in data for a recorder and calculates the list length metrics. These metrics are based around the idea of a 'list', defined as the species recorded at a single location (often a 1km square) on a single day by an individual recorder. 
#' 
#' @param recorder_name the name of the recorder for whom you want to calculate the metrics
#' @param data the data.frame of recording information
#' @param threshold how many lists do there need to be before we calculate the metrics? If this is not met NA is reported for all metrics except \code{n_lists}
#' @param plot should a plot of a histogram of list lengths be created
#' @param sp_col the name of the column that contains the species names
#' @param date_col the name of the column that contains the date. This must be formatted as a date
#' @param recorder_col the name of the column that contains the recorder names
#' @param location_col the name of the column that contains the location. This is a character, such as a grid reference and should be representative of the scale at which recording is done over a single day, typically 1km-square is used.
#'    
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' # load example data
#' head(cit_sci_data)
#' 
#' # Location might be a site name column in your data or a unique combination of lat and long
#' # Our data is missing a location column so we will use lat and long
#' # It might be more sensible to convert lat long to a grid reference and 
#' # use a 1 km square grid reference to represent a site 
#' cit_sci_data$location <- paste(round(cit_sci_data$lat, 4), round(cit_sci_data$long, 4))
#' 
#' # run for one recorder
#' LL <- listLength(data = cit_sci_data,
#'                  recorder_name = 3007,
#'                  threshold = 10,
#'                  plot = FALSE,
#'                  sp_col = 'species',
#'                  date_col = 'date',
#'                  recorder_col = 'recorder',
#'                  location_col = 'location')
#' 
#' # Run the metric for all recorders
#' LL_all <- lapply(unique(cit_sci_data$recorder),
#'                  FUN = listLength,
#'                  data = cit_sci_data,
#'                  threshold = 10,
#'                  plot = FALSE,
#'                  sp_col = 'species',
#'                  date_col = 'date',
#'                  recorder_col = 'recorder',
#'                  location_col = 'location')
#' 
#' # summarise as one table
#' LL_all_sum <- do.call(rbind, LL_all)
#' 
#' hist(LL_all_sum$n_lists, breaks = 80)
#' }
#' 
#' @return A data.frame with seven columns
#' \itemize{
#'  \item{\code{recorder} - }{The name of the recorder, as given in the recorder_name argument}
#'  \item{\code{mean_LL} - }{The mean number of species recorded across all lists}
#'  \item{\code{median_LL} - }{The median number of species recorded across all lists}
#'  \item{\code{variance} - }{The variance in the number of species recorded across all lists}
#'  \item{\code{p1} - }{The proportion of visits that had a single species recorded}
#'  \item{\code{p4} - }{The proportion of visits that had four or more species recorded}
#'  \item{\code{n_lists} - }{The number of lists this recorder recorded}
#' }

listLength <-
function(recorder_name,
         data,
         threshold = 10,
         plot = FALSE,
         sp_col = 'preferred_taxon',
         date_col = 'date_start',
         recorder_col = 'recorders',
         location_col = 'kmsq'){
 
  # get the data for this recorder
  rec_data <- data[data[ ,recorder_col] == recorder_name, ]
  
  # create a unique visit column
  rec_data$visit <- paste(rec_data[ , date_col], rec_data[ , location_col],
                      sep = '_')
  
  # calculate list-lengths
  rec_LL <- tapply(rec_data[,sp_col], rec_data$visit,
                   FUN = function(x) length(unique(x)))

  # Only calculate if the number of lists meets the threshold
  if(length(rec_LL) >= threshold){
    if(plot) hist(rec_LL,
                  breaks = max(rec_LL),
                  main = 'Histogram of list-length',
                  xlab = 'List-length')   
    
    mean_LL <- mean(rec_LL)
    median_LL <- median(rec_LL)
    variance <- var(rec_LL)
    
    # proportion of 'visits' that are of length 1
    p1 <- sum(rec_LL == 1)/length(rec_LL)
    
    # proportion of 'visits' that are of 4 or more
    p4 <- sum(rec_LL >= 4)/length(rec_LL)
    
    
    df <- data.frame(recorder = recorder_name,
                     mean_LL = mean_LL,
                     median_LL = median_LL,
                     variance = variance,
                     p1 = p1,
                     p4 = p4,
                     n_lists = length(rec_LL))
    return(df)
  } else {
    
    df <- data.frame(recorder = recorder_name,
                     mean_LL = NA,
                     median_LL = NA,
                     variance = NA,
                     p1 = NA,
                     p4 = NA,
                     n_lists = length(rec_LL))
    return(df)
  }
}
