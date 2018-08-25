#' import.map
#'
#' @param path Character indicating path to depth map (BW picture)
#'
#' @return matrix where elements are in [0,1]. White pixel become 1, and black 0.
#' @export
#'
import.map <- function(path){
  if(typeof(path) != "character"){
    error("path should be a character.")
  }
  extension <- tolower(substr(path, nchar(path)-2, nchar(path))) # get file extension in lowercase
  if (extension == "bmp"){
    bitmap <- read.bmp(path) # import picture

    if(attr(bitmap, "header")$depth != 24){
      warning("depth.map should be a 24 bits bitmap")
    }

    # check if dimension 3 (color) is not constant
    layers.1.2.equal <- (sum(bitmap[,,1] != bitmap[,,2]) == 0)
    layers.1.3.equal <- (sum(bitmap[,,1] != bitmap[,,3]) == 0)
    if (!(layers.1.2.equal & layers.1.3.equal)){
      stop("Depth map picture cannot have colors.")
    }

    depth.map.mat <- bitmap[,,1] # remove color dimension (constant)
    depth.map.mat <- depth.map.mat/max(depth.map.mat) # normalize to [0,1]
  } else {
    stop("Only .bmp pictures are currently supported.")
  }
  return(depth.map.mat)
}
