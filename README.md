# Rstereo

### Description
This is an R package to create [autostereograms](https://en.wikipedia.org/wiki/Autostereogram).

### Installation
```
library(devtools)
install_github("pierrecattin/Rstereo")
library(Rstereo)
```

### Example 1: Random dot autostereogram of gaussian distribution

```
# generate surface to plot
x <- seq(-2.5, 2.5, 0.02)
y <- seq(-2.5, 2.5, 0.05)
depth.map <- matrix(data=NA, nrow=length(x), ncol=length(y))
for(i in 1:length(x)) {
  for(j in 1:length(y)) {
    depth.map[i,j] <-  (1/(2*pi)) * exp( -((x[i])^2 + (y[j])^2)/2 )
  }
}

# create and display autostereogram
gaussian <- autostereogram(depth.map)
plot(gaussian, axes=F)
```
![gaussian](img/gaussian.jpg)

### Example 2: autostereogram of Shark using pattern
Note: to run this example, test_data/depth_map.bmp and and test_data/sample_pattern.bmp need to be downloaded and placed in working directory.
```
shark <- autostereogram("depth_map.bmp", "pattern.bmp")
plot(shark, axes=F)
```
![shark](img/shark.jpg)

### Dependency
package imager

### Reference
Thimbleby, H. W., Inglis, S., & Witten, I. H. (1994). Displaying 3D images: Algorithms for single-image random-dot stereograms. Computer, 27(10), 38-48.