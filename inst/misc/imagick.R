# some example data & desired colour mapping of [0-1] ranged data matrix
library(RColorBrewer)
cols=colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlBu"))(1080)
colfun=colorRamp(RColorBrewer::brewer.pal(11, "RdYlBu"))
mat1=matrix(seq(1:1080)/1080,nrow=1920,ncol=1080,byrow=TRUE)
mat2=matrix(seq(1:1080)/1080,ncol=1920,nrow=1080,byrow=FALSE)
system.time(rastmat <- as.raster(apply(mat2, 2, function (col) rgb(colfun(col),max=255)))) # 2.55s

# grid graphics - winner, though slower than image() if timing of 2.55s for creation of raster is included
library(grid)
system.time(grid.raster(rastmat,interpolate=FALSE)) # 0.20s, or 0.2+2.55=2.75s

# base R image()
op <- par(mar=c(0, 0, 0, 0))
system.time(image(mat1,axes=FALSE,useRaster=TRUE,col=cols)) # 1.09s
par(op)

raster2RasterLayer <- function(x) {
  raster::setExtent(raster::setValues(
    raster::brick(ncol = nrow(x),
                  nrow = ncol(x),
                  nl = 3),
            t(col2rgb(x))), raster::extent(0, ncol(x), 0, nrow(x)))
}
##plotRGB(raster2RasterLayer(rastmat))
## write to magick
library(raster)
rgdal::writeGDAL(as(raster2RasterLayer(rastmat), "SpatialGridDataFrame"),
                 "out.png", drivername = "PNG")
library(magick)
mgk <- image_read("out.png")

to_px <- function(x) {
  paste(x, collapse = "x")
}
system.time(print(mgk))
#format width height colorspace matte filesize
#1    PNG  1080   1920       sRGB FALSE    13840
#user  system elapsed
#0.522   0.007   0.530

# magick, again slower than image() if timing of 2.55s for creation of raster is included
library(magick)
par(mar=c(0, 0, 0, 0))
system.time(plot(rastmat)) # 0.22s, or 0.22+2.55=2.77s
par(op)

# imager - doesn't plot in base R graphics device, so this one won't work together with Shiny
# If you wouldn't have to press ESC to return control to R this
# might have some potential though...
library(imager)
display(as.cimg(rastmat))

# ggplot2 - just for the record...
mat2=expand.grid(y=1:1080,x=1:1920)
mat2$z=seq(1,1080)/1080
library(ggplot2)
system.time({q <- qplot(data=mat2,x=x,y=y,fill=z,geom="raster") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colours = cols) +
  theme_void() + theme(legend.position="none"); print(q)}) # 23s
