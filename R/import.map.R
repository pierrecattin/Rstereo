#' import.map
#'
#' @param path Character indicating path to depth map (greyscale picture)
#'
#' @return Matrix where elements are in [0,1]. White pixel become 1, and black 0.
#'
#' @export
#'
#' @importFrom imager load.image
#'
import.map <- function(path){
  cimg <- load.image(path) # import picture
  map <- array(0, dim=c(dim(cimg)[1:2], dim(cimg)[4]))
  map <- cimg[,,1,] # remove time dimension

  # check if dimension 3 (color) is not constant
  layers.1.2.equal <- (sum(map[,,1] != map[,,2]) == 0)
  layers.1.3.equal <- (sum(map[,,1] != map[,,3]) == 0)
  # if (!(layers.1.2.equal & layers.1.3.equal)){
  #   stop("Depth map picture cannot have colors.")
  # }
  map <- map[,,1] # remove color dimension (constant)
  map <- map/max(map) # normalize to [0,1]
  map <- t(map)

  return(map)
}
