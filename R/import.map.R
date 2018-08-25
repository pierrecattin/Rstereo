#' import.map
#'
#' @param path Character indicating path to depth map (greyscale picture)
#'
#' @return Matrix where elements are in [0,1]. White pixel become 1, and black 0.
#' @export
#'
import.map <- function(path){
  #extension <- tolower(substr(path, nchar(path)-2, nchar(path))) # get file extension in lowercase
  cimg <- load.image(path) # import picture
  map <- array(0, dim=c(dim(cimg)[1:2], dim(cimg)[4]))
  map <- cimg[,,1,] # remove time dimension

  # check if dimension 3 (color) is not constant
  layers.1.2.equal <- (sum(map[,,1] != map[,,2]) == 0)
  layers.1.3.equal <- (sum(map[,,1] != map[,,3]) == 0)
  if (!(layers.1.2.equal & layers.1.3.equal)){
    stop("Depth map picture cannot have colors.")
  }
  map <- map[,,1] # remove color dimension (constant)
  map <- map/max(map) # normalize to [0,1]
  map <- t(map)

  return(map)
}

#backup using bmp
# import.map <- function(path){
#   if(typeof(path) != "character"){
#     error("path should be a character.")
#   }
#   extension <- tolower(substr(path, nchar(path)-2, nchar(path))) # get file extension in lowercase
#   if (extension == "bmp"){
#     bitmap <- read.bmp(path) # import picture
#
#     if(attr(bitmap, "header")$depth != 24){
#       warning("depth.map should be a 24 bits bitmap")
#     }
#
#     # check if dimension 3 (color) is not constant
#     layers.1.2.equal <- (sum(bitmap[,,1] != bitmap[,,2]) == 0)
#     layers.1.3.equal <- (sum(bitmap[,,1] != bitmap[,,3]) == 0)
#     if (!(layers.1.2.equal & layers.1.3.equal)){
#       stop("Depth map picture cannot have colors.")
#     }
#     depth.map.mat <- bitmap[,,1] # remove color dimension (constant)
#     depth.map.mat <- depth.map.mat/max(depth.map.mat) # normalize to [0,1]
#     depth.map.mat <- t(depth.map.mat) # rotate image
#   } else {
#     stop("Only .bmp pictures are currently supported.")
#   }
#   return(depth.map.mat)
# }
