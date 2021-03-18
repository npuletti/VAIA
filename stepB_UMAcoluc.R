# Arezzo 22/10/2020 Nicola Puletti
# STEP b: Averaging of pre-event images per study area

rm(list=ls())
library(raster)
library(sf)
library(stars)

radarDir <- "D:/progetti/2020/07_GAIAVAIA/raster/radar"
(radar.filenames <- list.files(radarDir))

(pn0 <- substr(radar.filenames, 36,45))
(pn1 <- substr(radar.filenames, 29,45))
(pn2 <- substr(radar.filenames, 68,69))

wHH <- which(pn2=="HH")
wHV <- which(pn2=="HV")
wMASK <- which(pn2=="_s")

# Area COLUC/UMA
# ::: Create filname list :::::::::::::::::::::::::::::::::::::::::

a.list <- c('680-180528', '920-180530', '920-180725', '920-180905')

(a.HH.filenames <- paste(radarDir, radar.filenames[as.numeric(names(which(table(c(which(pn0 %in% a.list),wHH))==2)))], sep='/'))
(a.HV.filenames <- paste(radarDir, radar.filenames[as.numeric(names(which(table(c(which(pn0 %in% a.list),wHV))==2)))], sep='/'))
(a.MASK.filenames <- paste(radarDir, radar.filenames[as.numeric(names(which(table(c(which(pn0 %in% a.list),wMASK))==2)))], sep='/'))



# ::: Import and masking Radar images :::::::::::::::::::::::::::::::::::::::::

Mask1.o <- Mask1 <- raster(a.MASK.filenames[1])
Mask2.o <- Mask2 <- raster(a.MASK.filenames[2])
Mask3.o <- Mask3 <- raster(a.MASK.filenames[3])
Mask4.o <- Mask4 <- raster(a.MASK.filenames[4])


v1 <- rep(NA, length(values(Mask1.o)))
v1[values(Mask1.o)==0] <- 1
values(Mask1) <- v1

v2 <- rep(NA, length(values(Mask2.o)))
v2[values(Mask2.o)==0] <- 1
values(Mask2) <- v2

v3 <- rep(NA, length(values(Mask3.o)))
v3[values(Mask3.o)==0] <- 1
values(Mask3) <- v3

v4 <- rep(NA, length(values(Mask4.o)))
v4[values(Mask4.o)==0] <- 1
values(Mask4) <- v4

plot(Mask1)


HH1 <- raster(a.HH.filenames[1], band=1)*Mask1
HH2 <- raster(a.HH.filenames[2], band=1)*Mask2
HH3 <- raster(a.HH.filenames[3], band=1)*Mask3
HH4 <- raster(a.HH.filenames[4], band=1)*Mask4

HV1 <- raster(a.HV.filenames[1], band=1)*Mask1
HV2 <- raster(a.HV.filenames[2], band=1)*Mask2
HV3 <- raster(a.HV.filenames[3], band=1)*Mask3
HV4 <- raster(a.HV.filenames[4], band=1)*Mask4

plot(HH1)

# :: Re-projects in new EPSG and Re-sample at 8 m ::::::::::::::::::::::::::::::::::::::::::

newproj <- "+init=epsg:32632"

HH1.32632.10m <- projectRaster(HH1, crs=newproj, res=8)
HH2.32632.10m <- projectRaster(HH2, crs=newproj, res=8)
HH3.32632.10m <- projectRaster(HH3, crs=newproj, res=8)
HH4.32632.10m <- projectRaster(HH4, crs=newproj, res=8)
# plot(HH1.32632.10m)

HV1.32632.10m <- projectRaster(HV1, crs=newproj, res=8)
HV2.32632.10m <- projectRaster(HV2, crs=newproj, res=8)
HV3.32632.10m <- projectRaster(HV3, crs=newproj, res=8)
HV4.32632.10m <- projectRaster(HV4, crs=newproj, res=8)


# :::: Create spatial extent as reference, with 10 m of resolution

ex1 <- extent(HH1.32632.10m)
ex2 <- extent(HH2.32632.10m)
ex3 <- extent(HH3.32632.10m)
ex4 <- extent(HH4.32632.10m)

exUMA <- merge(ex1, ex2, ex3, ex4)
rUMA <- raster(ext=exUMA, resolution=10, crs=newproj)


# ::::::: Resampling at the spatial extent (res = 10 m)

rsHH.1 <- raster::resample(HH1.32632.10m, rUMA)
rsHH.2 <- raster::resample(HH2.32632.10m, rUMA)
rsHH.3 <- raster::resample(HH3.32632.10m, rUMA)
rsHH.4 <- raster::resample(HH4.32632.10m, rUMA)

rsHV.1 <- raster::resample(HV1.32632.10m, rUMA)
rsHV.2 <- raster::resample(HV2.32632.10m, rUMA)
rsHV.3 <- raster::resample(HV3.32632.10m, rUMA)
rsHV.4 <- raster::resample(HV4.32632.10m, rUMA)

# ::::::::: Mosaicing
plot(ex1)
plot(ex2, add=T, col='red')
plot(ex3, add=T, col='blue')
plot(ex4, add=T, col='orange')


mosaic.UMA.HH <- mosaic(rsHH.1, rsHH.2, fun=mean)
mosaic.UMA.HH <- mosaic(mosaic.UMA.HH, rsHH.3, fun=mean)
mosaic.UMA.HH <- mosaic(mosaic.UMA.HH, rsHH.4, fun=max)

mosaic.UMA.HV <- mosaic(rsHV.1, rsHV.2, fun=mean)
mosaic.UMA.HV <- mosaic(mosaic.UMA.HV, rsHV.3, fun=mean)
mosaic.UMA.HV <- mosaic(mosaic.UMA.HV, rsHV.4, fun=max)

plot(mosaic.UMA.HV)



# :::: Save mosaicated images

writeRaster(mosaic.UMA.HH,
            "D:/progetti/2020/07_GAIAVAIA/raster/Mosaics/m_UMA_HH_10m_epsg32632.tif")

writeRaster(mosaic.UMA.HV,
            "D:/progetti/2020/07_GAIAVAIA/raster/Mosaics/m_UMA_HV_10m_epsg32632.tif")
