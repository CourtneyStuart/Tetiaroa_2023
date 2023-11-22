#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com OR courtney.stuart@mansfield.ox.ac.uk)

#### PLEASE NOTE ####
# Due to the fine spatial resolution (30cm by 30cm) and large spatial extent
# (25860 cols x 25740 rows) of the base topographic-bathymetric LiDAR raster,
# this script is both time-intensive and computationally demanding. The full 
# script took several days to run to completion when performed by Courtney
# Stuart on a desktop computer (processor: Intel(R) Xeon(R) Silver 4114 CPU 
# @ 2.20 GHz 2.19 GHzX; installed RAM: 128 GB) connected to a 2TB Seagate 
# external hard drive. Parallel processing may speed-up the calculations, but
# will ultimately depend on the number of processors/cores, amount of RAM, 
# etc. available on your machine/cluster. 

#### DIRECTORIES ####
# working directory
setwd("E:/Data/Tetiaroa/Courtney_Stuart/Tetiaroa_2023/")

# data directories
bathy_wd = "E:/Data/Tetiaroa/Courtney_Stuart/Tetiaroa_2023/Source_Data/Topobathy/" 
ras_wd = "E:/Data/Tetiaroa/Courtney_Stuart/Tetiaroa_2023/Rasters/"

#### LIBRARIES ####
library(easypackages)
libraries("raster", "terra", "MultiscaleDTM", "sp", "sf", 
          "conflicted", "spatialEco", "dplyr")
conflict_prefer("terrain", "terra")

#### PROCESSING ####
# save PROJ.4 string for Teti'aroa projection
# Projected Coordinate System	WGS 1984 UTM Zone 6S (EPSG WKID	32706)
my_crs = CRS("+init=epsg:32706")

# read in topobathy raster
topobathy = raster(paste0(bathy_wd, "tetiaroa_dtm_30cm_2017_1.tif"))
compareCRS(topobathy, my_crs) # check projection
res(topobathy) # check resolution, which should be 30cm by 30cm
topobathyspat = rast(topobathy) # convert to SpatRaster for processing/storing
# export topobathy raster to desired folder and remove from environment to save space
writeRaster(topobathy,
            paste0(ras_wd, "TopoBathy.tif"),
            overwrite = T)
rm(topobathy)

# derive topographic-bathymetric measures - all calculated using a quadratic 
# local fit and the Queen's case (8 neighboring cells). 

# slope
slope = Qfit(r = topobathyspat, 
             w = c(3,3), 
             unit = "degrees",
             metrics = "qslope")

writeRaster(slope, 
            paste0(ras_wd, "QSlope.tif"),
            overwrite = T)

rm(slope)
gc()

# aspect
aspect = Qfit(r = topobathyspat, 
             w = c(3,3), 
             metrics = "qaspect")

writeRaster(aspect, 
            paste0(ras_wd, "QAspect.tif"),
            overwrite = T)

rm(aspect)
gc()

# eastness
eastness = Qfit(r = topobathyspat, 
                w = c(3,3), 
                metrics = "qeastness")

writeRaster(eastness, 
            paste0(ras_wd, "QEastness.tif"),
            overwrite = T)

rm(eastness)
gc()

# northness
northness = Qfit(r = topobathyspat, 
                w = c(3,3), 
                metrics = "qnorthness")

writeRaster(northness, 
            paste0(ras_wd, "QNorthness.tif"),
            overwrite = T)

rm(northness)
gc()

# profile curvature
profc = Qfit(r = topobathyspat, 
                 w = c(3,3), 
                 metrics = "profc")

writeRaster(profc, 
            paste0(ras_wd, "QProfCurve.tif"),
            overwrite = T)

rm(profc)
gc()

# planform curvature
planc = Qfit(r = topobathyspat, 
             w = c(3,3), 
             metrics = "planc")

writeRaster(planc, 
            paste0(ras_wd, "QPlanCurve.tif"),
            overwrite = T)

rm(planc)
gc()

# mean curvature
meanc = Qfit(r = topobathyspat, 
             w = c(3,3), 
             metrics = "meanc")

writeRaster(meanc, 
            paste0(ras_wd, "QMeanCurve.tif"),
            overwrite = T)

rm(meanc)
gc()

# surface area to planar area ratio rugosity (slope-corrected)
sapa = SAPA(r = topobathyspat, 
            w = c(3,3),
            slope_correction = T)

writeRaster(sapa, 
            paste0(ras_wd, "SAPARugosity.tif"),
            overwrite = T)

rm(sapa)
gc()


#### OPTIONAL ####
# read back in all rasters and check CRS
#slope = raster(paste0(ras_wd, "QSlope.tif")) 
#aspect = raster(paste0(ras_wd, "QAspect.tif"))
#eastness = raster(paste0(ras_wd, "QEastness.tif"))
#northness = raster(paste0(ras_wd, "QNorthness.tif"))
#profc = slope = raster(paste0(ras_wd, "QProfCurve.tif"))
#planc = raster(paste0(ras_wd, "QPlanCurve.tif"))
#meanc = raster(paste0(ras_wd, "QMeanCurve.tif"))
#sapa = raster(paste0(ras_wd, "SAPARugosity.tif"))
#compareCRS(slope, my_crs)
#compareCRS(aspect, my_crs)
#compareCRS(eastness, my_crs)
#compareCRS(northness, my_crs)
#compareCRS(profc, my_crs) 
#compareCRS(planc, my_crs) 
#compareCRS(meanc, my_crs)
#compareCRS(sapa, my_crs) 