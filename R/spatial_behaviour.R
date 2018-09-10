#' Calculate spatial metrics
#' 
#' This function takes in data for a recorder and calculates the spatial metrics. Note that the spatial projection of the data allows for estimates of distance in meters. Pay particular attention to the specification or the parameters \code{crs} and \code{new_crs}
#' 
#' @param recorder_name the name of the recorder for whom you want to calculate the metrics
#' @param data the data.frame of recording information
#' @param y_col the name of the column that contains the y coordinate (e.g. latitude) of the observation. This should be a numeric.
#' @param x_col the name of the column that contains the x coordinate (e.g. longitude)  of the observation. This should be a numeric.
#' @param crs the proj4 string that describes the projection your data are using. For GPS lat long this is "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs". You can find more at \url{http://spatialreference.org/}
#' @param new_crs the proj4 string that the describes the coordinate system your data should be reprojected to. THIS IS IMPORTANT. Your data must be on a projection that has units in meters so that results are comparable to other studies. An appropriate system in the UK is the UK national grid "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs". If your original crs (given in the argument \code{crs}), already has units in meters then set \code{new_crs = NULL}. WARNING: if you set this to NULL but your coordinate system is not in units of meters you will likely have errors.
#' @param recorder_col the name of the column that contains the recorder names
#' @param upper_percentile The percentile used to create a polygon that encapsulates a proportion of the recorders observations using the kernel method (\code{?adehabitatHR::kernelUD}). This is used to estimate the area covered as will as the ratio of core to total area covered, see \code{Value}.
#' @param lower_percentile see \code{upper_percentile}
#' @param h a numeric smoothing parameter for drawing the kernels. See \code{?adehabitatHR::kernelUD} for details.
#' @param res a numeric giving the resolution for kernel estimation.
#' @param threshold If there are less than this number of observations NA values will be returned for the metrics. Default is 5
#'    
#' @export
#' @import adehabitatHR
#' @import raster
#' @importFrom sp CRS
#' 
#' @examples
#' \dontrun{
#' 
#' # load example data
#' head(cit_sci_data)
#' 
#' # Run for one recorder using the UK grid
#' 
#' ## get the proj4 strings from http://spatialreference.org
#' # current form is lat long
#' WGS_84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#' # I want to change to UK national grid as that is in meters
#' UKNG <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
#'
#' SB <- spatial_behaviour(recorder_name = 3007,
#'                        data = cit_sci_data, 
#'                        crs = WGS_84,
#'                        new_crs = NULL,
#'                        y_col = 'lat',
#'                        x_col = 'long',
#'                        recorder_col = 'recorder')
#'
#' SB
#' plot(SB$poly_upper)
#'
#' # Run for more than one recorder, this can be slow 
#' SB_all <- lapply(unique(cit_sci_data$recorder)[1:10],
#'                 FUN = spatial_behaviour, 
#'                 data = cit_sci_data, 
#'                 crs = WGS_84,
#'                 new_crs = UKNG,
#'                 y_col = 'lat',
#'                 x_col = 'long',
#'                 recorder_col = 'recorder')
#'
#' # summarise as one table
#' SB_all_sum <- do.call(rbind, SB_all)
#'
#' SB_all_sum
#' }
#' 
#' @return A data.frame with seven columns
#' \itemize{
#'  \item{\code{recorder} - }{The name of the recorder, as given in the recorder_name argument}
#'  \item{\code{spPoint} - }{The observation points as a SpatialPoints object}
#'  \item{\code{poly_upper} - }{A SpatialPolygonsDataFrame object giving the are containing the upper_percentile percentage of records using the kernel method}
#'  \item{\code{poly_lower} - }{A SpatialPolygonsDataFrame object giving the are containing the lower_percentile percentage of records using the kernel method}
#'  \item{\code{upper_n_poly} - }{The number of polygons that make up \code{poly_upper}}
#'  \item{\code{lower_n_poly} - }{The number of polygons that make up \code{poly_lower}}
#'  \item{\code{upper_area} - }{The area of the polygons that make up \code{poly_upper} in km squared}
#'  \item{\code{lower_area} - }{The area of the polygons that make up \code{poly_upper} in km squared}
#'  \item{\code{ratio} - }{The ratio of \code{lower_area} to \code{upper_area} calculated as \code{lower_area/upper_area}}
#'  \item{\code{n} - }{The total number of observations made by this recorder}
#' }

spatial_behaviour <- function(recorder_name,
                              data,
                              y_col,
                              x_col,
                              crs,
                              new_crs,
                              recorder_col = 'recorders',
                              upper_percentile = 95,
                              lower_percentile = 60,
                              h = 5000,
                              res = 1000,
                              threshold = 5){
  
  if(is.factor(recorder_name)){
    recorder_name <- as.character(recorder_name)
  } 
  
  n_row <- nrow(data[data[,recorder_col] == recorder_name, ])
  
  if(n_row >= threshold){
  
    # Convert to SpatialPoints
    spPoints_LL <- sp::SpatialPoints(data[data[,recorder_col] == recorder_name,
                                     c(x_col, y_col)])
    # give current projection
    proj4string(spPoints_LL) <- CRS(crs)
  
    # Convert to projection that uses meters
    if(!is.null(new_crs)){
      spPoint <- spTransform(spPoints_LL, new_crs)  
    } else {
      spPoint <- spPoints_LL
    }
    
    # set up grid
    # This allows us to ensure there is space for the 
    # isoclines to be drawn and that the pixel res is the
    # same - here 1km
    grid_exp <- h * 10

    minx <- floor(bbox(spPoint)[1,'min'] - grid_exp)
    miny <- floor(bbox(spPoint)[2,'min'] - grid_exp)
    maxx <- ceiling(bbox(spPoint)[1,'max'] + grid_exp)
    maxy <- ceiling(bbox(spPoint)[2,'max'] + grid_exp)

    grid_ras <- raster(ext = extent(minx, maxx, miny, maxy),
                       res = res,
                       crs = projection(spPoint))

    grid_SP <- as(grid_ras, "SpatialPixels")
    
    # Try kernel density
    KD <- kernelUD(xy = spPoint, h = h, grid = grid_SP)
    # image(KD)
    KA <- kernel.area(KD,
                      percent = c(lower_percentile, upper_percentile),
                      unin = "m",
                      unout = "km2")
    area_upper <- KA[2]
    area_lower <- KA[1]
    poly_lower <- getverticeshr(KD, percent = lower_percentile)
    poly_upper <- getverticeshr(KD, percent = upper_percentile)
    rm(list = 'KD')
    
    holed <- function(x) x@hole
    
    npolys_upper <- sum(!sapply(X = poly_upper@polygons[[1]]@Polygons, FUN = holed))
    npolys_lower <- sum(!sapply(X = poly_lower@polygons[[1]]@Polygons, FUN = holed))

    return(list(recorder = recorder_name,
                spPoint = spPoint,
                poly_upper = poly_upper,
                poly_lower = poly_lower,
                upper_n_poly = npolys_upper,
                lower_n_poly = npolys_lower,
                upper_area = area_upper,
                lower_area = area_lower,
                ratio = area_lower/area_upper,
                n = n_row))
  } else {
    return(list(recorder = recorder_name,
                spPoint = NA,
                poly_upper = NA,
                poly_lower = NA,
                upper_n_poly = NA,
                lower_n_poly = NA,
                upper_area = NA,
                lower_area = NA,
                ratio = NA,
                n = n_row))
  }
}
