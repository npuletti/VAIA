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

# Area Asiago
# ::: Create filname list :::::::::::::::::::::::::::::::::::::::::

a.list <- c('690-180402', '690-180505', '910-180530', '910-180725')

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

exASIAGO <- merge(ex1, ex2, ex3, ex4)
rASIAGO <- raster(ext=exASIAGO, resolution=10, crs=newproj)


# ::::::: Resampling at the spatial extent (res = 10 m)

rsHH.1 <- raster::resample(HH1.32632.10m, rASIAGO)
rsHH.2 <- raster::resample(HH2.32632.10m, rASIAGO)
rsHH.3 <- raster::resample(HH3.32632.10m, rASIAGO)
rsHH.4 <- raster::resample(HH4.32632.10m, rASIAGO)

rsHV.1 <- raster::resample(HV1.32632.10m, rASIAGO)
rsHV.2 <- raster::resample(HV2.32632.10m, rASIAGO)
rsHV.3 <- raster::resample(HV3.32632.10m, rASIAGO)
rsHV.4 <- raster::resample(HV4.32632.10m, rASIAGO)

# ::::::::: Mosaicing
mosaic.asiago.HH <- mosaic(rsHH.1, rsHH.step2, fun=mean)
mosaic.asiago.HH <- mosaic(mosaic.asiago.HH, rsHH.3, fun=max)
mosaic.asiago.HH <- mosaic(mosaic.asiago.HH, rsHH.4, fun=max)

mosaic.asiago.HV <- mosaic(rsHV.step1, rsHV.2, fun=mean)
mosaic.asiago.HV <- mosaic(mosaic.asiago.HV, rsHV.3, fun=max)
mosaic.asiago.HV <- mosaic(mosaic.asiago.HV, rsHV.4, fun=max)

plot(mosaic.asiago.HH)
plot(mosaic.asiago.HV)



# :::: Save mosaicated images

writeRaster(mosaic.asiago.HH,
            "D:/progetti/2020/07_GAIAVAIA/raster/Mosaics/m_Asiago_HH_10m_epsg32632.tif")

writeRaster(mosaic.asiago.HV,
            "D:/progetti/2020/07_GAIAVAIA/raster/Mosaics/m_Asiago_HV_10m_epsg32632.tif")
