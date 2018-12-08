#' Creates autostereogram based on a depth map and a pattern. If no pattern is provided, a random-dot autostereogram is created. The object returned is a cimg from the image package. Supported picture formats are PNG, JPEG and BMP.
#'
#' @param depth.map Depth map representing the 3D shape. Possible values are:  a) Numeric matrix where lements with large value appear closer. b) Character containing path to greyscale .bmp .jpg or .png picture. The whiter a pixel is, the closer it appears.
#' @param pattern Optional. Path to a picture (.bmp .jpg or .png) representing the pattern used to create the autostereogram. If not provided, a random dot autostereogram is generated.
#' @param repetitions Optional. Number of times the pattern should be repeated horizontally. Default is round(width(depth.map)/100)
#'
#' @return cimg image
#' @export
#' @importFrom imager load.image
#' @importFrom imager resize
#' @importFrom imager cimg
#' @importFrom imager height
#' @importFrom imager width
#' @importFrom stats runif
#'
#' @examples
#' # Plot bivariate gaussian distribution
#' x <- seq(-2.5, 2.5, 0.02)
#' y <- seq(-2.5, 2.5, 0.05)
#' depth.map <- matrix(data=NA, nrow=length(x), ncol=length(y))
#' for(i in 1:length(x)) {
#'   for(j in 1:length(y)) {
#'     depth.map[i,j] <-  (1/(2*pi)) * exp( -((x[i])^2 + (y[j])^2)/2 )
#'   }
#' }
#' gaussian <- autostereogram(depth.map)
#' plot(gaussian, axes=FALSE)

autostereogram <- function (depth.map, pattern, repetitions) {
  # Define constants  ####
  profile.depth <- 1/3 # Ratio depth of 3d object / depth of space
  #img.back.dist <- 1 # depth of space
  eye.back.dist <- 2 # y-distance between eyes and back plane

  # Create Depth Map ####
  if (typeof(depth.map) == "character"){
    map <- import.map(depth.map) # import picture as matrix
  } else if(is.matrix(depth.map)){
    if(typeof(depth.map) != "double"){
      stop("If depth.map is a matrix, its type should be 'double'")
    }
    map.raw <- depth.map
    # normalize to [0,1]
    map.raw <- map.raw - min(map.raw)
    map <- map.raw / max(map.raw)
    } else {
    stop("depth.map should be a character or a matrix")
  }
  map.width <- ncol(map)
  map.height <- nrow(map)
  intereye.dist <- map.width/4 # distance between two eyes, in pixels

  if(missing(repetitions)){
    repetitions <- max(round(map.width/100),2)
  }

  #E <- round(map.width/repetitions*4)

  # Define function to compute the x value of a point projected on the image plane for left and right eye
  project.on.image <- function(x, z){ # x: x-position on depth map; z: depth value
    x.image.left <- x - abs(x-(map.width-intereye.dist)/2) * (1-z)/(eye.back.dist-z) # x value on image plane for left eye
    x.image.right <- x - abs(x-(map.width+intereye.dist)/2) * (1-z)/(eye.back.dist-z) # x value on image plane for right eye
    return(c(left=x.image.left, right=x.image.right))
  }

  # project.r <- function(x, z) x - (x - (E + map.width)/2)*(img.back.dist - z*profile.depth*img.back.dist)/(2*img.back.dist - z*profile.depth*img.back.dist)
  #project.l <- function(x, z) x - (x - (map.width-E)/2) * (img.back.dist-z*profile.depth*img.back.dist)/(2*img.back.dist - z*profile.depth*img.back.dist)
  # s <- function(z) E*(1-z*profile.depth)/(2-z*profile.depth) # ???

  # Expand depth.map
  # d <- round(s(0)) # width to add on image's sides
  # C <- d + 2*map.width
  # img.back.dist <- 2*d + 2*map.width
  # map.redim <- matrix(nrow = map.height, ncol = 2*map.width+2*d) # Resized new matrix

  # map.redim[, 1:(d-1)] <- 0
  #
  # for(i in d:(C-1) ){
  #   j <- round(((i-d+1)+0.1)/2)
  #   map.redim[,i] <- map[,j]
  # }
  #
  # map.redim[, C:(img.back.dist-1)] <- 0
  #
  # map <- map.redim

  # update dimensions
  # map.width <- ncol(map)
  # map.height <- nrow(map)


  # Import/generate pattern ####
  pattern.width <- round(map.width/repetitions) # find width of pattern so that there is the right number of repetition
  if (!missing(pattern)){
    pattern <- load.image(pattern)
    pattern <- resize(pattern,
                      pattern.width, # set width
                      round(height(pattern)*pattern.width/width(pattern))) # scale height to preserve pattern proportions
  } else {  # generate random pattern
    pattern <- array(data=NA, dim=c(nrow=map.height, ncol=pattern.width, 1, 4))
    pattern[,,1,] <- round(runif(pattern.width*map.height)) # random black and white pixels
    pattern[,,1,4] <- 1
  }

  dim.pixel <- length(pattern[1,1,1,])
  pattern.height <- length(pattern[1,,1,1])

  # array that will store final image
  image <- array(data=NA, dim=c(map.height, map.width, dim.pixel))

  for(i in 1:map.height){ # Scan each line
    x.far <- 1:map.width # X values in remote plane
    map.line <- map[i,] # Extract map values of current line
    x.left.all <- round(project.l(x.far, map.line)) # Compute X values corresponding to map for left eye
    x.right.all <-round(project.r(x.far, map.line)) # Compute X values corresponding to map for right eye

    while(sum(is.na(image[i,,1])) > 0){ # Continues as long as there are NA pixel values
      x.first <- which(is.na(image[i,,1]))[1] # Find first undefined pixel
      x.image <- x.first

      if(sum(x.left.all==x.image, na.rm = T) > 0 | sum(x.right.all==x.image, na.rm = T) > 0){ # Check that both eyes are in image domain
        pattern.y <- (i - 1) %% pattern.height + 1
        pattern.x <- (x.first - 1) %% pattern.width + 1
        col <- pattern[pattern.x, pattern.y,1, ] # Find RGB values of pattern's pixel corresponding to current position

        while(!is.na(x.image)){  # Check that both eyes are in image domain

          image[i, x.image, ] <- col
          x.right <- x.image + round(s(map.line[which(x.left.all == x.image)[1]]))
          if(!is.na(x.right)) image[i, x.right, ] <- col # Check if pixel is still in domain

          x.image <- x.right # Switch to next pixel
        }
      }else{ # At least one eye is outside of domain
        image[i, x.image, ] <- rep(0, dim.pixel)
      }
    }
  }
  # rotate image
  image <- aperm(image, perm=c(2,1,3))

  # convert to cimg
  cimage <- array(NA, dim=c(dim(image)[1:2], 1, dim(image)[3]))
  cimage[,,1,] <- image
  cimage <- cimg(cimage)
  return(cimage)
}
