# Arezzo 07/01/2021 Nicola Puletti
# Estrazione su poligoni Trentino controllati da Clara

rm(list=ls())
library(raster)
library(sf)
library(tidyverse)
library(ggplot2)
library(stars)
library(ggpmisc)

# Main ----

# Import raster file ----

radarDir <- "D:/progetti/2020/07_GAIAVAIA/raster/radar/Alos2_1"
(radar.filenames <- list.files(radarDir))

pre.folder <- "0000317940_004001_ALOS2217030920-180530"
post.folder <- "0000339298_001001_ALOS2243940920-181128"


gamma.HH.pre.fname <- file.path(paste(radarDir, pre.folder, sep='/'),"Gamma0_HH_dB.img")
gamma.HV.pre.fname <- file.path(paste(radarDir, pre.folder, sep='/'),"Gamma0_HV_dB.img")

gamma.HH.post.fname <- file.path(paste(radarDir, post.folder, sep='/'),"Gamma0_HH_dB.img")
gamma.HV.post.fname <- file.path(paste(radarDir, post.folder, sep='/'),"Gamma0_HV_dB.img")


gamma.HH.pre <- raster(gamma.HH.pre.fname)
gamma.HV.pre <- raster(gamma.HV.pre.fname)


gamma.HH.post <- raster(gamma.HH.post.fname)
gamma.HV.post <- raster(gamma.HV.post.fname)


# Import shp file ----
schianti <- st_read(dsn = "shapes", layer = "schianti_tn_edit_ct_wgs84")

# crs(schianti); crs(postHH); crs(postHV); crs(preHH); crs(preHV)

schianti.gamma.HH.pre <- raster::crop(gamma.HH.pre, schianti)
schianti.gamma.HV.pre <- raster::crop(gamma.HV.pre, schianti)

schianti.gamma.HH.post <- raster::crop(gamma.HH.post, schianti)
schianti.gamma.HV.post <- raster::crop(gamma.HV.post, schianti)


gamma.HH.pre.list <- raster::extract(schianti.gamma.HH.pre, schianti)
gamma.HV.pre.list <- raster::extract(schianti.gamma.HV.pre, schianti)

gamma.HH.post.list <- raster::extract(schianti.gamma.HH.post, schianti)
gamma.HV.post.list <- raster::extract(schianti.gamma.HV.post, schianti)

schianti[14,"geometry"] %>% as.character()

plot(schianti.gamma.HH.pre, )
plot(schianti, add=T, col = 'steelblue')


length(gamma.HH.pre.list) == length(gamma.HV.pre.list)
length(gamma.HV.pre.list) == length(gamma.HH.post.list)
length(gamma.HH.post.list) == length(gamma.HV.post.list)

t.df.HH <- tibble(
  polarization = "HH",
  
  pre.N = sapply(gamma.HH.pre.list, length),
  pre.min = sapply(gamma.HH.pre.list, min),
  pre.max = sapply(gamma.HH.pre.list, max),
  pre.mean = sapply(gamma.HH.pre.list, mean),
  pre.sd = sapply(gamma.HH.pre.list, sd),
  
  post.N = sapply(gamma.HH.post.list, length),
  post.min = sapply(gamma.HH.post.list, min),
  post.max = sapply(gamma.HH.post.list, max),
  post.mean = sapply(gamma.HH.post.list, mean),
  post.sd = sapply(gamma.HH.post.list, sd)
  )
  
t.df.HV <- tibble(
  polarization = "HV",
  
  pre.N = sapply(gamma.HV.pre.list, length),
  pre.min = sapply(gamma.HV.pre.list, min),
  pre.max = sapply(gamma.HV.pre.list, max),
  pre.mean = sapply(gamma.HV.pre.list, mean),
  pre.sd = sapply(gamma.HV.pre.list, sd),
  
  post.N = sapply(gamma.HV.post.list, length),
  post.min = sapply(gamma.HV.post.list, min),
  post.max = sapply(gamma.HV.post.list, max),
  post.mean = sapply(gamma.HV.post.list, mean),
  post.sd = sapply(gamma.HV.post.list, sd)
)

t.df <- t.df.HH %>% bind_rows(t.df.HV)
t.df %>% data.frame
table(t.df$polarization)

t.df %>% ggplot(aes(
  pre.mean, post.mean
)) +
  geom_abline(intercept =0, slope = 1, size = .1, colour = "orange") +
  geom_point() +
  facet_wrap(~polarization) +
  xlim(-25,10) + ylim(-25,10) + coord_fixed() +
  geom_smooth(method="lm", formula=y~x,color="red",alpha=.2) +
stat_poly_eq(formula = y ~ x,
             aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
             parse=TRUE,label.x.npc = "left")

t.df %>% dplyr::filter(polarization=='HH') %>% 
  ggplot(aes(pre.N)) + geom_histogram(bins = 50) + xlim(0,1000)

10000/56.25



t.df%>% dplyr::filter(polarization=='HV') %>% mutate(
  meanHV = post.mean-pre.mean
) %>% ggplot(aes(meanHV)) + geom_histogram(bins = 50)

t.df%>% dplyr::filter(polarization=='HV') %>% dplyr::select(
  polarization, pre.mean, post.mean
) %>% pivot_longer(c(pre.mean, post.mean)) %>% ggplot(aes(value)) + 
  geom_histogram(bins = 50) + facet_wrap(~name)

t.df%>% dplyr::filter(polarization=='HV') %>% dplyr::select(
  polarization, pre.mean, post.mean
) %>% mutate(fid=1:20) %>%
  pivot_longer(c(pre.mean, post.mean)) %>% ggplot(aes(x=fid, y=value,
                                                      fill = factor(name)))+
  geom_bar(stat = "identity",  width = 0.4,
               position=position_dodge(width = 0.5)) + 
