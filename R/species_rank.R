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
#' SR <- species_rank(recorder_name = 3007,
#'                    data = cit_sci_data, 
#'                    sp_col = 'species',
#'                    recorder_col = 'recorder')
#'                    
#' # Run the metric for all recorders
#' SR_all <- lapply(unique(cit_sci_data$recorder),
#'                  FUN = species_rank,
#'                  data = cit_sci_data,
#'                  sp_col = 'species',
#'                  recorder_col = 'recorder')
#'
#' # summarise as one table
#' SR_all_sum <- do.call(rbind, SR_all)
#'
#' hist(SR_all_sum$median_diff_rarity, breaks = 20) 
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

species_rank <-
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
  recorder_data$rank <- rank_species[recorder_data[ ,sp_col]]
  
  return(data.frame(recorder = as.character(recorder_name),
                    median_rarity = median(recorder_data$rank),
                    median_diff_rarity = median(recorder_data$rank) - grand_median,
                    stdev = sd(recorder_data$rank),
                    n = nrow(recorder_data)))
}
