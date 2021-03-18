# Arezzo 22/10/2020 Nicola Puletti
# STEP b: Averaging of pre-event images per study area

rm(list=ls())
library(raster)
library(sf)
library(stars)

radarDir <- "D:/progetti/2020/07_GAIAVAIA/raster/radar/Alos2_1"
(radar.filenames <- list.files(radarDir))


# Area Tesista/Trentino
# ::: Create filname list :::::::::::::::::::::::::::::::::::::::::

a.list <- c('680-180505', '920-180530', '920-180725', '920-180905', '690-180402')

(a.HH.filenames <- paste(radarDir, radar.filenames[as.numeric(names(which(table(c(which(pn0 %in% a.list),wHH))==2)))], sep='/'))
(a.HV.filenames <- paste(radarDir, radar.filenames[as.numeric(names(which(table(c(which(pn0 %in% a.list),wHV))==2)))], sep='/'))
(a.MASK.filenames <- paste(radarDir, radar.filenames[as.numeric(names(which(table(c(which(pn0 %in% a.list),wMASK))==2)))], sep='/'))



# ::: Import and masking Radar images :::::::::::::::::::::::::::::::::::::::::

Mask1.o <- Mask1 <- raster(a.MASK.filenames[1])
Mask2.o <- Mask2 <- raster(a.MASK.filenames[2])
Mask3.o <- Mask3 <- raster(a.MASK.filenames[3])
Mask4.o <- Mask4 <- raster(a.MASK.filenames[4])
Mask5.o <- Mask5 <- raster(a.MASK.filenames[5])


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

v5 <- rep(NA, length(values(Mask5.o)))
v5[values(Mask5.o)==0] <- 1
values(Mask5) <- v5



HH1 <- raster(a.HH.filenames[1], band=1)*Mask1
HH2 <- raster(a.HH.filenames[2], band=1)*Mask2
HH3 <- raster(a.HH.filenames[3], band=1)*Mask3
HH4 <- raster(a.HH.filenames[4], band=1)*Mask4
HH5 <- raster(a.HH.filenames[5], band=1)*Mask5

HV1 <- raster(a.HV.filenames[1], band=1)*Mask1
HV2 <- raster(a.HV.filenames[2], band=1)*Mask2
HV3 <- raster(a.HV.filenames[3], band=1)*Mask3
HV4 <- raster(a.HV.filenames[4], band=1)*Mask4
HV5 <- raster(a.HV.filenames[5], band=1)*Mask5


# :: Re-projects in new EPSG and Re-sample at 8 m ::::::::::::::::::::::::::::::::::::::::::

newproj <- "+init=epsg:32632"

HH1.32632.10m <- projectRaster(HH1, crs=newproj, res=8)
HH2.32632.10m <- projectRaster(HH2, crs=newproj, res=8)
HH3.32632.10m <- projectRaster(HH3, crs=newproj, res=8)
HH4.32632.10m <- projectRaster(HH4, crs=newproj, res=8)
HH5.32632.10m <- projectRaster(HH5, crs=newproj, res=8)
# plot(HH1.32632.10m)

HV1.32632.10m <- projectRaster(HV1, crs=newproj, res=8)
HV2.32632.10m <- projectRaster(HV2, crs=newproj, res=8)
HV3.32632.10m <- projectRaster(HV3, crs=newproj, res=8)
HV4.32632.10m <- projectRaster(HV4, crs=newproj, res=8)
HV5.32632.10m <- projectRaster(HV5, crs=newproj, res=8)

# :::: Create spatial extent as reference, with 10 m of resolution

ex1 <- extent(HH1.32632.10m)
ex2 <- extent(HH2.32632.10m)
ex3 <- extent(HH3.32632.10m)
ex4 <- extent(HH4.32632.10m)
ex5 <- extent(HH5.32632.10m)

exTT <- merge(ex1, ex2, ex3, ex4, ex5)
rTT <- raster(ext=exTT, resolution=10, crs=newproj)

# ::::::: Resampling at the spatial extent (res = 10 m)

rsHH.1 <- raster::resample(HH1.32632.10m, rTT)
rsHH.2 <- raster::resample(HH2.32632.10m, rTT)
rsHH.3 <- raster::resample(HH3.32632.10m, rTT)
rsHH.4 <- raster::resample(HH4.32632.10m, rTT)
rsHH.5 <- raster::resample(HH5.32632.10m, rTT)

rsHV.1 <- raster::resample(HV1.32632.10m, rTT)
rsHV.2 <- raster::resample(HV2.32632.10m, rTT)
rsHV.3 <- raster::resample(HV3.32632.10m, rTT)
rsHV.4 <- raster::resample(HV4.32632.10m, rTT)
rsHV.5 <- raster::resample(HV5.32632.10m, rTT)

# ::::::::: Mosaicing
plot(ex1)
plot(ex2, add=T, col='red')
plot(ex3, add=T, col='blue')
plot(ex4, add=T, col='orange')
plot(ex5, add=T, col='green')


mosaic.TT.HH <- mosaic(rsHH.1, rsHH.2, fun=mean)
mosaic.TT.HH <- mosaic(mosaic.TT.HH, rsHH.3, fun=mean)
mosaic.TT.HH <- mosaic(mosaic.TT.HH, rsHH.4, fun=max)
mosaic.TT.HH <- mosaic(mosaic.TT.HH, rsHH.5, fun=max)

mosaic.TT.HV <- mosaic(rsHV.1, rsHV.2, fun=mean)
mosaic.TT.HV <- mosaic(mosaic.TT.HV, rsHV.3, fun=mean)
mosaic.TT.HV <- mosaic(mosaic.TT.HV, rsHV.4, fun=max)
mosaic.TT.HV <- mosaic(mosaic.TT.HV, rsHV.5, fun=max)

plot(mosaic.TT.HH)


# :::: Save mosaicated images

writeRaster(mosaic.TT.HH,
            "D:/progetti/2020/07_GAIAVAIA/raster/Mosaics/m_Trentino_HH_10m_epsg32632.tif")

writeRaster(mosaic.TT.HV,
            "D:/progetti/2020/07_GAIAVAIA/raster/Mosaics/m_Trentino_HV_10m_epsg32632.tif")
