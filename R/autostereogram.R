#' autostereogram
#'
#' @param depth.map Depth map representing the 3D shape. Possible values are:  a) Numeric matrix where lements with large value appear closer. b) Character containing path to greyscale .bmp .jpg or .png picture. The whiter a pixel is, the closer it appears.
#' @param pattern Optional. Path to a picture (.bmp .jpg or .png) representing the pattern used to create the autostereogram. If not provided, a random dot autostereogram is generated.
#' @param repetitions Optional. Number of times the pattern should be repeated horizontally. Default is round(width(depth.map)/100)
#'
#' @return cimg image
#' @export
#'
autostereogram <- function (depth.map, pattern, repetitions) {
  library(imager)
  # Define constants  ####
  u <- 1/3 # Ratio depth of 3d object / depth of space
  D <- 30 # depth of space

  # Create Depth Map ####
  if (typeof(depth.map) == "character"){
    map <- import.map(depth.map) # import picture as matrix
  } else if(is.matrix(depth.map)){
    if(typeof(depth.map) != "double"){
      stop("If depth.map is a matrix, its type should be 'double'")
    }
    map <- depth.map
    # normalize to [0,1]
    map <- map - min(map)
    map <- map / max(map)
    } else {
    stop("depth.map should be a character or a matrix")
  }
  map.width <- ncol(map)
  map.height <- nrow(map)

  if(missing(repetitions)){
    repetitions <- round(map.width/100)
  }

  E <- round(map.width/repetitions*4)

  # Define functions to compute the x value of a point projected on the image plane for right and left eye
  project.r <- function(x, z) x - (x - (E + map.width)/2)*(D - z*u*D)/(2*D - z*u*D)
  project.l <- function(x, z) x - (x - (map.width-E)/2) * (D-z*u*D)/(2*D - z*u*D)
  s <- function(z) E*(1-z*u)/(2-z*u)

  # Expand depth.map
  d <- round(s(0)) # width to add on image's sides
  C <- d + 2*map.width
  D <- 2*d + 2*map.width
  map.redim <- matrix(nrow = map.height, ncol = 2*map.width+2*d) # Nouvelle matrice redimensionnée

  map.redim[, 1:(d-1)] <- 0

  for(i in d:(C-1) ){
    j <- round(((i-d+1)+0.1)/2)
    map.redim[,i] <- map[,j]
  }

  map.redim[, C:(D-1)] <- 0

  map <- map.redim

  # update dimensions
  map.width <- ncol(map)
  map.height <- nrow(map)


  # Import/generate pattern ####
  if (!missing(pattern)){
    pattern <- load.image(pattern)
    pattern <- resize(pattern, round(E/2), round(height(pattern)*E/(2*width(pattern))))
  } else {  # generate random pattern
    pattern <- array(data=NA, dim=c(nrow=map.height, ncol=E/2, 1, 4))
    pattern[,,1,] <- round(runif(E/2*map.height)) # random black and white pixels
    pattern[,,1,4] <- 1
  }

  dim.pixel <- length(pattern[1,1,1,])
  pattern.map.width <- length(pattern[,1,1,1])
  pattern.map.height <- length(pattern[1, ,1,1])

  # array that will store final image
  image <- array(data=NA, dim=c(map.height, map.width, dim.pixel))

  for(i in 1:map.height){ # Scan each line
    x.far <- 1:map.width # Valeurs de X sur le plan lointain
    map.line <- map[i,]# Extrait les valeurs de map sur cette ligne
    x.left.all <- round(project.l(x.far, map.line)) # Calcule les valeurs de X correspontants aux map pour l'oeil gauche
    x.right.all <-round(project.r(x.far, map.line)) # Calcule les valeurs de X correspontants aux map pour l'oeil droit

    while(sum(is.na(image[i,,1])) > 0){ # Tourne tant que des pixels ne sont pas définis
      x.first <- which(is.na(image[i,,1]))[1] # Trouve le premier pixel non-défini
      x.image <- x.first

      if(sum(x.left.all==x.image, na.rm = T) > 0 | sum(x.right.all==x.image, na.rm = T) > 0){ # Dans le domaine avec les 2 yeux
        pattern.y <- (i - 1) %% pattern.map.height + 1
        pattern.x <- (x.first - 1) %% pattern.map.width + 1
        col <- pattern[pattern.x, pattern.y,1, ] # Trouve les valeurs RGB du pixel du pattern correspondant a la position actuelle

        while(!is.na(x.image)){ # Dans le domaine avec les 2 yeux

          image[i, x.image, ] <- col
          x.right <- x.image + round(s(map.line[which(x.left.all == x.image)[1]]))
          if(!is.na(x.right)) image[i, x.right, ] <- col # Teste si le pixel est encore dans le domaine

          x.image <- x.right # On décale au pixel suivant
        }
      }else{ # Au moins un des yeux est dehors du domaine
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
