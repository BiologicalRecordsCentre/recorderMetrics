#' Calculate recorder metrics and the 4-axes of recorder behaviour
#' 
#' This function replicates the analyses presented in August et al XXXX. This allows you to use your
#' own data to extract the values for the 4-axes (recording intensity, spatial extent, recording potential,
#' and rarity recording). This function applies the same centring and scaling values as used in 
#' August et al and performs the same pre-analysis log transformations. Note that the axis values,
#' while comparable to those used in August et al, may not be optimal for your data, and you should also
#' extract the raw metrics and apply your own PCA to see if the same axes are important for explaining
#' the variation observed in your data. 
#' 
#' @param data The data.frame of recording information. See `head(cit_sci_data)`, for an example format. This should be the data for all observations made to your citizen science project.
#' @param recorders Optional. A vector of recorders (as in `recorder_col`), for which you want to calculate values.
#' @param verbose Should progress be reported?
#' @param recorder_col The name of the column that contains the recorder names
#' @param date_col The name of the column that contains the date. This must be formatted as a date
#' @param y_col The name of the column that contains the y coordinate (e.g. latitude) of the observation. This should be a numeric.
#' @param x_col The name of the column that contains the x coordinate (e.g. longitude) of the observation. This should be a numeric.
#' @param square_km_col To calculate list lengths the location of recorders is defined by the 1km-square in which they are recorded. To make results comparable to August et al provide the 1km-square of each record here (i.e. a grid reference).
#' @param active_days_limit If there are less than this number of active days NA values will be returned for the metrics. August et al use 10, and changing this value will result in metrics that are not comparable to August et al
#' @param crs The proj4 string that describes the projection your data are using. For GPS lat long this is "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs". You can find more at http://spatialreference.org/
#' @param new_crs The proj4 string that the describes the coordinate system your data should be reprojected to. THIS IS IMPORTANT. Your data must be on a projection that has units in meters so that results are comparable to other studies. An appropriate system in the UK is the UK national grid "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs". If your original crs (given in the argument crs), already has units in meters then set new_crs = NULL. WARNING: if you set this to NULL but your coordinate system is not in units of meters you will likely have errors.
#' @param sp_col The name of the column that contains the species names
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' # load example data
#' head(cit_sci_data)
#' 
#' # Run for 10 recorders
#' metrics_axes <- predictAxes(data = cit_sci_data,
#'                             recorders = unique(cit_sci_data$recorder)[1:10])
#'             
#' # The returned object is a list of the metrics...
#' metrics_axes$recorder_metrics
#' 
#' # ...and the axes values
#' metrics_axes$axes
#'               
#' # Run the metric all recorders. NOTE: this takes a long time
#' metrics_axes <- predictAxes(data = cit_sci_data)
#' 
#' }

predictAxes <- function(data,
                        recorders = NULL,
                        verbose = TRUE,
                        recorder_col = 'recorder',
                        date_col = 'date',
                        y_col = 'lat',
                        x_col = 'long',
                        square_km_col = 'km_sq',
                        active_days_limit = 10,
                        crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                        new_crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs",
                        sp_col = 'species'){
  
  column_names <- c(recorder_col, y_col, x_col, date_col, sp_col, square_km_col)
  
  if(!all(column_names %in% colnames(data))){
    stop(paste('The following are not column names in your data:',
               paste(column_names[!column_names %in% colnames(data)], collapse = ', ')))
  }
  
  # Change the column names
  data.names <- colnames(data)
  data.names[data.names == recorder_col] <- 'recorder'
  data.names[data.names == y_col] <- 'lat'
  data.names[data.names == x_col] <- 'long'
  data.names[data.names == date_col] <- 'date'
  data.names[data.names == sp_col] <- 'species'
  data.names[data.names == square_km_col] <- 'location'
  
  colnames(data) <- data.names
  
  summer_data <- summerData(data = data,
                            probs = c(0.05, 0.95),
                            date_col = 'date')

  if(is.null(recorders)) recorders <- unique(data[,'recorder']) 
  
  if(!all(recorders %in% data[,'recorder'])){
    stop(paste(paste(recorders[!recorders %in% data[,'recorder']], collapse = ", "),
               'are not know recorders in your data'))
  }
  
  not_enough_data <- 0
  
  if(verbose) cat('Calculating recorder metrics...\n')
  
  if(length(recorders) > 1) progress <- txtProgressBar(style = 3)
  r = 0
  
  for(i in recorders){
    
    r = r + 1
    if(length(recorders) > 1) setTxtProgressBar(progress, value = r/length(recorders))
    
    rec.activity <- activityRatio(data = summer_data,
                                  recorder_name = i,
                                  recorder_col = 'recorder',
                                  date_col = 'date',
                                  summer_days = attr(summer_data, 'cutoffs'))

    
    if(rec.activity$active_days >= active_days_limit){
      
      rec.weeklyDevotedDays <- weeklyDevotedDays(recorder_name = i,
                                                 data = data,
                                                 recorder_col = 'recorder',
                                                 date_col = 'date')
      
      rec.periodicity <- periodicity(recorder_name = i,
                                     data = summer_data,
                                     date_col = 'date',
                                     recorder_col = 'recorder',
                                     day_limit = active_days_limit)
      
      rec.spatial <- spatialBehaviour(data = data,
                                      recorder_name = i,
                                      y_col = 'lat',
                                      x_col = 'long',
                                      crs = crs,
                                      new_crs = new_crs,
                                      recorder_col = 'recorder',
                                      threshold = active_days_limit)
      
      rec.taxaBreadth <- taxaBreadth(recorder_name = i,
                                     data = data,
                                     sp_col = 'species',
                                     recorder_col = 'recorder')
      
      rec.speciesRarity <- speciesRarity(recorder_name = i,
                                         data = data,
                                         sp_col = 'species',
                                         recorder_col = 'recorder')
      
      
      rec.listLength <- listLength(data = data,
                                   recorder_name = i,
                                   threshold = active_days_limit,
                                   plot = FALSE,
                                   sp_col = 'species',
                                   date_col = 'date',
                                   recorder_col = 'recorder',
                                   location_col = 'location')
      
      # Combine the metrics needed
      key_variables <- data.frame(recorder = i,
                                  activity_ratio = rec.activity$activity_ratio,
                                  active_days = rec.activity$active_days,
                                  median_weekly_devoted_days = rec.weeklyDevotedDays$median_weekly_devoted_days,
                                  periodicity = rec.periodicity$periodicity,
                                  periodicity_variation = rec.periodicity$periodicity_variation,
                                  upper_area = rec.spatial$upper_area,
                                  upper_n_poly = rec.spatial$upper_n_poly,
                                  ratio = rec.spatial$ratio,
                                  taxa_prop = rec.taxaBreadth$taxa_prop,
                                  median_diff_rarity = rec.speciesRarity$median_diff_rarity,
                                  p1 = rec.listLength$p1)
      
      
    } else {
      
      not_enough_data <- not_enough_data + 1

      key_variables <- data.frame(recorder = i,
                                  activity_ratio = rec.activity$activity_ratio,
                                  active_days = rec.activity$active_days,
                                  median_weekly_devoted_days = NA,
                                  periodicity = NA,
                                  periodicity_variation = NA,
                                  upper_area = NA,
                                  upper_n_poly = NA,
                                  ratio = NA,
                                  taxa_prop = NA,
                                  median_diff_rarity = NA,
                                  p1 = NA)
      
    }
    
    if(!exists('master_key_variables')){
      master_key_variables <- key_variables
    } else {
      master_key_variables <- rbind(master_key_variables, key_variables)  
    }
    
  }
  
  close(progress)
  
  if(not_enough_data > 0){
    warning(paste("Metrics cannot be calculated for recorders with fewer than the threshold",
                  "number of active days. The threshold is set to", active_days_limit,
                  "and", not_enough_data, "recorders fail this"))
  }
  
  if(verbose) cat('Calculating axis values...')
  
  all_vars <- master_key_variables
  master_key_variables <- na.omit(master_key_variables)
  
  to_log <- c('upper_n_poly','upper_area','periodicity_variation',
              'periodicity','activity_ratio','active_days')
  
  for(j in to_log){
    
    master_key_variables[,j] <- log(master_key_variables[,j] + 0.001)
    
  }
  
  scaled <- scale(master_key_variables[-1],
                  center = scaling$scale_centre,
                  scale = scaling$scale_scale)

  # Temporal PCA ----
  temporal_metrics <- c('activity_ratio',
                        'median_weekly_devoted_days',
                        'periodicity', 'periodicity_variation')
  prediction.temp <- predict(pca_results$temp.pca,
                             as.data.frame(scaled[ , temporal_metrics, drop = FALSE]))
  
  # Spatial PCS ----
  spatial_metrics <- c('upper_area', 'upper_n_poly', 'ratio')
  prediction.spat <- predict(pca_results$spat.pca,
                             as.data.frame(scaled[ , spatial_metrics, drop = FALSE]))

  # Data content PCA ----
  taxanomic_metrics <- c('taxa_prop', 'median_diff_rarity', 'p1')
  prediction.taxa <- predict(pca_results$taxa.pca,
                             as.data.frame(scaled[ , taxanomic_metrics, drop = FALSE]))
  
  if(verbose) cat(' Done\n')

  return(list(axes = data.frame(recorder = master_key_variables$recorder,
                                recording_intensity = prediction.temp[,1],
                                spatial_extent = prediction.spat[,1],
                                recording_potential = -prediction.taxa[,1],
                                rarity_recording = -prediction.taxa[,2]),
              recorder_metrics = all_vars))
  
}