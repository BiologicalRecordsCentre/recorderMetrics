#' Calculate the rarity of species recorded by a volunteer
#' 
#' This function takes in data for a recorder and calculates the recorder's rarity metrics.
#' 
#' @param recorder_name the name of the recorder for whom you want to calculate the metrics
#' @param data the data.frame of recording information
#' @param sp_col the name of the column that contains the species names
#' @param recorder_col the name of the column that contains the recorder names
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' # load example data
#' head(cit_sci_data)
#' 
#' # Run for a single recorder
#' SR <- speciesRarity(recorder_name = 3007,
#'                    data = cit_sci_data, 
#'                    sp_col = 'species',
#'                    recorder_col = 'recorder')
#'                    
#' # Run the metric for all recorders
#' SR_all <- lapply(unique(cit_sci_data$recorder),
#'                  FUN = speciesRarity,
#'                  data = cit_sci_data,
#'                  sp_col = 'species',
#'                  recorder_col = 'recorder')
#'
#' # summarise as one table
#' SR_all_sum <- do.call(rbind, SR_all)
#'
#' hist(SR_all_sum$median_diff_rarity, breaks = 20) 
#' 
#' ## Accounting for spatial restriction in movement
#' # If recorders where restricted to the countries that
#' # make up GB (Scotland England and Wales). We should
#' # analyse the data by country
#' library(sp)
#' plot(GB)
#' 
#' # Convert our citizen science data to a SpatialPointsDataframe
#' SP <- SpatialPointsDataFrame(data = cit_sci_data,
#'                              coords = cit_sci_data[,c('long','lat')])
#' # Define lat long coordinate system
#' CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#' proj4string(SP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#' 
#' # Empty object for all results
#' SR_all_countries <- NULL
#' 
#' # Loop through counties
#' for(i in unique(GB$NAME)){
#'   
#'   # Subset by country
#'   SP_C <- SP[GB[GB$NAME == i, ], ]
#'   
#'   # Calculate the metric within country
#'   SR_one_country <- lapply(unique(SP_C$recorder),
#'                            FUN = speciesRarity,
#'                            data = SP_C@data,
#'                            sp_col = 'species',
#'                            recorder_col = 'recorder')
#'   
#'   # combine data
#'   SR_one_country <- do.call(rbind, SR_one_country)
#'   SR_one_country$country <- i
#'   SR_all_countries <- rbind(SR_all_countries,
#'                             SR_one_country)
#' }
#' 
#' # Note that recorders that have recorded in more than
#' # one country are replicated in our results (n = 75)
#' sum(table(SR_all_countries$recorder) > 1)
#' 
#' # Alternativly we can subset data by a buffer around the
#' # recorders records, rather than by country.
#' # Here I use a buffer of 150km
#' library(raster)
#' library(rgeos)
#' 
#' # Empty object for all results
#' SR_all_150km_buffer <- NULL
#' 
#' for(i in unique(SP$recorder)){
#'   
#'   SP_R <- SP[SP$recorder == i, ]
#'   SP_R_buffer <- buffer(SP_R, 150000)
#'   SP_P <- SP[SP_R_buffer, ]
#'   
#'   SR_one_buffer <- speciesRarity(recorder_name = i,
#'                                  data = SP_P@data,
#'                                  sp_col = 'species',
#'                                  recorder_col = 'recorder')
#'   
#'   SR_all_30km_buffer <- rbind(SR_all_30km_buffer,
#'                               SR_one_buffer)
#' }
#' 
#' # Compare results with original analysis
#' combo <- merge(y = SR_all_30km_buffer,
#'                x = SR_all_sum,
#'                by = 'recorder')
#' 
#' plot(combo$median_diff_rarity.x[combo$n.x > 10],
#'      combo$median_diff_rarity.y[combo$n.x > 10],
#'      xlab = 'Original',
#'      ylab = 'By buffer',
#'      main = 'Median Rarity Difference')
#' abline(0,1)
#' 
#' }
#' 
#' @details This function examines the rarity of the species that a
#' recorder observes and compares this to the rarity of species recorded
#' across all recorders. First all species are ranked by rarity across all
#' observations from all recorders. These ranks are then scaled to 100 so
#' that the metrics are independent of the number of species in the group.
#' Each observation, in the entire dataset is then assigned a value for
#' the ranked rarity of the species observed and a median is calculated.
#' This median is therefore the median ranked rarity of species observed,
#' where 1 is the most common species and 100 the rarest (ranks are scaled
#' to 100). The median rarity for the individual recorder is calculated 
#' in the same way, but using the species' rank values from the overall
#' dataset.
#' 
#' @return A data.frame with seven columns
#' \itemize{
#'  \item{\code{recorder} - }{The name of the recorder, as given in the recorder_name argument}
#'  \item{\code{median_rarity} - }{The median rarity of all observations made by the recorder}
#'  \item{\code{median_diff_rarity} - }{The median rarity of an recorders observations subtracted from the median rarity across all observers. Negative numbers mean a recorder is recording more common species than the population of recorders as a whole. Positive numbers mean the recorder is recording more rare species than the population of recorders as a whole.}
#'  \item{\code{stdev} - }{The standard deviation of the rarity ranks of a recorders observations}
#'  \item{\code{n} - }{The total number of observations made by this recorder}
#' }

speciesRarity <-
function(recorder_name,
         data, 
         sp_col = 'preferred_taxon',
         recorder_col = 'recorders'){
  
  data <- data[,c(sp_col, recorder_col)]
  rank_species <- rank(abs(table(data[,sp_col])-max(table(data[,sp_col]))))
  
  # scale ranks to 100
  rank_species <- (rank_species/max(rank_species)) * 100
  
  sp_counts <- table(data[,sp_col])
  
  rank_reps <- rep(rank_species, sp_counts)
  grand_median <- median(rank_reps)
  grand_sd <- sd(rank_reps)
  
  recorder_data <- data[data[,recorder_col] == recorder_name,]
  recorder_data$rank <- rank_species[as.character(recorder_data[ ,sp_col])]
  
  return(data.frame(recorder = as.character(recorder_name),
                    median_rarity = median(recorder_data$rank),
                    median_diff_rarity = median(recorder_data$rank) - grand_median,
                    stdev = sd(recorder_data$rank),
                    n = nrow(recorder_data)))
}
