#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com OR courtney.stuart@mansfield.ox.ac.uk)

#### DIRECTORIES ####
# working directory
setwd("E:/Data/Tetiaroa/Courtney_Stuart/Tetiaroa_2023/")

# data directories
# habitat data
hab_wd = "E:/Data/Tetiaroa/Courtney_Stuart/Tetiaroa_2023/Source_Data/Habitat/"
# raster data (geomorphological predictors created in 00_Topobathy_Derivatives.R)
ras_wd = "E:/Data/Tetiaroa/Courtney_Stuart/Tetiaroa_2023/Rasters/"
# baseline algae nutrient data from Oregon State, Oxford, Lancaster, Tetiaroa Society collaboration
alg_wd = "E:/Data/Tetiaroa/Courtney_Stuart/Tetiaroa_2023/Source_Data/Algae_Baseline_Data/"
# baseline seabird biomass and nutrient input data from the Tetiaroa Sociey
bird_wd = "E:/Data/Tetiaroa/Courtney_Stuart/Tetiaroa_2023/Source_Data/Seabird_Baseline_Data/"
# github project where all processed data will be saved to 
git_wd = "E:/Data/Tetiaroa/Courtney_Stuart/Tetiaroa_2023/GitHub/Tetiaroa_2023/Data/"
# temporary files
temp_wd = "E:/Data/Tetiaroa/Courtney_Stuart/Tetiaroa_2023/Temp/"

#### LIBRARIES ####
library(easypackages)
libraries("raster", "fasterize", "sp", "sf", "dplyr", 
          "conflicted", "spatialEco", "PNWColors")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

#### PROCESSING ####
# save PROJ.4 string for Teti'aroa projection
# Projected Coordinate System	WGS 1984 UTM Zone 6S (EPSG WKID	32706)
my_crs = CRS("+init=epsg:32706")

# first read in the habitat map shapefile from the Living Oceans Foundation.
# according to the metadata, these habitat data were derived from WorldView-02 
# (WV2) satellite imagery collected by DigitalGlobe, Inc. the images 
# have a per pixel spatial resolution of 2m by 2m, so that is the spatial resolution
# we'll use when converting from vector to raster
habitat = st_read(paste0(hab_wd, "FPSOTE_habitats_final.shp"))
compareCRS(habitat, my_crs)

# each benthic habitat class is associated with an integer habitat code. we will use 
# this integer code to define habitats when converting from vector to raster.
# create and save a data.frame of habitat class-code pairs
hab_cod_df = habitat %>%
  st_drop_geometry() %>%
  select(Habitat, Hab_Cod) %>%
  unique()
#write.csv(hab_cod_df,
#         paste0(hab_wd, "Habitat_Class_Raster_Codes.csv"), 
#        row.names = F)

# use the topobathy raster as a guide to define our spatial extent, but use the 
# recommended 2m by 2m spatial resolution
topobathy = raster(paste0(ras_wd, "TopoBathy.tif"))
guide = raster(ext = extent(topobathy), 
               res = c(2,2), 
               crs = my_crs)
rm(topobathy)

# rasterize the habitat data and export the new 2m by 2m habitat raster
habitat_ras = writeRaster((
  fasterize(sf = habitat,
            raster = guide,
            field = "Hab_Cod",
            fun = "max")), 
  file = file.path(ras_wd, "Habitat_2m.tif"), 
  format = "GTiff", 
  overwrite = T)
compareCRS(habitat_ras, my_crs)

# read in the topobathy derivative rasters created in 00_Topobathy_Derivatives.R
topobathy = raster(paste0(ras_wd, "TopoBathy.tif"))
slope = raster(paste0(ras_wd, "QSlope.tif"))
aspect = raster(paste0(ras_wd, "QAspect.tif"))
eastness = raster(paste0(ras_wd, "QEastness.tif"))
northness = raster(paste0(ras_wd, "QNorthness.tif"))
planc = raster(paste0(ras_wd, "QPlanCurve.tif"))
profc = raster(paste0(ras_wd, "QProfCurve.tif"))
meanc = raster(paste0(ras_wd, "QMeanCurve.tif"))
sapa = raster(paste0(ras_wd, "SAPARugosity.tif"))
landdist = raster(paste0(ras_wd, "LandDist.tif"))

# resample the habitat raster to have 30cm by 30cm resolution and the same number of
# rows and columns as the other predictors. this will not add any new data or 
# information relative to the 2m by 2m raster, but will enable us to add the habitat
# raster to a raster stack with the other predictors. we can then use the stack to
# easily extract the value of all predictors at our algae sampling points using one 
# line of code. save the resampled raster to our folder of temporary files.
habitat = resample(x = habitat_ras,
                   y = topobathy,
                   method = "ngb")
#writeRaster(habitat, paste0(temp_wd, "Habitat_Resampled_30cm.tif"))
#habitat = raster(paste0(temp_wd, "Habitat_Resampled_30cm.tif"))

# create a raster stack with all desired predictors
env = stack(x = c(habitat, topobathy, slope, aspect, eastness, northness, planc,
                  profc, meanc, sapa, landdist))

# read in the baseline algae data collected for the ATA project in 2021
algaeATA = read.csv(paste0(alg_wd, "Tetiaroa_Turbinaria_ATA_November_2021_compiledMarch2023.csv"))

# get rid of any rows with missing (NA) N15 values and remove the extra columns that R
# added on to the end of our dataframe (the last column in the csv is 'Triplicate_C')
algaeATA = algaeATA %>%
  filter(!is.na(N15)) %>%
  select(1:23)

# read in the baseline algae data collected on transects in 2021 
# get rid of any rows with missing (NA) N15 values and remove any extra columns
algaeTRAN = read.csv(paste0(alg_wd, "Tetiaroa_Turbinaria_Transects_November_2021_compiledMarch2023.csv"))
algaeTRAN = algaeTRAN %>%
  filter(!is.na(N15))

# combine the ATA and transect datasets into a single dataframe using shared columns
algae = rbind(
  select(algaeATA, Date_collected, Month, Year, Motu, Purpose,
         Collected_by, GPS_name, Longitude, Latitude, vial, N15,
         C13, N_percent, C_percent, Notes), 
  select(algaeTRAN, Date_collected, Month, Year, Motu, Purpose,
         Collected_by, GPS_name, Longitude, Latitude, vial, N15, 
         C13, N_percent, C_percent, Notes))

# read in the baseline seabird survey data from 2021. This stores motu-level estimates
# of the total density and biomass of breeding seabirds.Nutrient input scales with body
# mass, so we can use these data as a proxy for nutrient delivery to each motu.
bird = read.csv(paste0(bird_wd, "seabird_biomass_by_motu_draft.csv"))

# we want to combine dataframes based on motu names, make sure they match up
unique(bird$Motu)
unique(algae$Motu)

# correct the algae records for Hiraanae (correct spelling) and Tauini (the local name for 
# Tiaraunu's Hoa)
algae$Motu = ifelse(algae$Motu == "Hiranae", "Hiraanae", algae$Motu)
algae$Motu = ifelse(algae$Motu == "Tiaraunu Hoa", "Tauini", algae$Motu)

# add the seabird density and biomass data to our algae dataframe
algae = left_join(algae,
                  select(bird, -X),
                  by = "Motu")
summary(algae)

# convert to spatial data using lat and long columns and re-project from GCS WGS 1984
# (decimal degrees) to our desired WGS 1984 UTM Zone 6S (EPSG WKID 32706)
gcs = CRS("+init=epsg:4326")
# convert to spatial data
algae = st_as_sf(algae,
                 coords = c("Longitude", "Latitude"), 
                 crs = gcs)
algae = st_transform(algae, my_crs) # reproject
compareCRS(algae, topobathy) # check that the projection is now correct

# extract values of predictors at all algae sampling points
algae_data = cbind(algae, raster::extract(env, algae))

# some quick renaming
algae_data = algae_data %>%
  rename(BirdBiomass = breeding_biomass_kgha_motu,
         BirdDensity = breeding_density_ha_motu,
         Depth = TopoBathy,
         Slope = qslope,
         Aspect = qaspect,
         Eastness = qeastness,
         Northness = qnorthness,
         PlanCurve = planc,
         ProfCurve = profc,
         MeanCurve = meanc,
         SAPARugosity = sapa,
         LandDistance = tetiaroa_dtm_30cm_2017_1) # distance to land raster

# add a column with the habitat class names, using the integer codes saved above
#hab_cod_df = read.csv(paste0(hab_wd, "Habitat_Class_Raster_Codes.csv"))
algae_data = left_join(algae_data, hab_cod_df, c("Habitat_Resampled_30cm" = "Hab_Cod")) %>%
  relocate(Habitat, .after = Habitat_Resampled_30cm) # place it after the habitat integer column

# remove rows where depth values are positive (above 0m)...although algae were clearly collected here and these
# sites may be right along the beach line, I don't want to confuse the model by providing both elevations and depths.
# This will only remove 4 records, which is negligible in this case.
algae_data = algae_data %>%
  filter(Depth <= 0)

# do any of the algae collection sites say they're in terrestrial vegetation? 
unique(algae_data$Habitat)

# GPS record 65 (Rimatuu) shows up as terrestrial, but our original habitat and remote sensing
# daat show that it's actually submerged rock. This issue is probably an articfact of the 
# rasterization process and can be confidently overwritten as rock. 
algae_data$Habitat = ifelse(algae_data$Habitat == "Terrestrial vegetation", "Rock", 
                            algae_data$Habitat)
unique(algae_data$Habitat) # done

# There are also a few typos in the month of collection column that we can fix
algae_data$Month = ifelse(grepl("Dec", algae_data$Date_collected), "Dec", algae_data$Month)

# save the longitude and latitude information to new columns, place these before the 
# GPS_name column
prepped_algae = algae_data %>%
  mutate(LongUTM6S = sf::st_coordinates(.)[,1],
         LatUTM6S = sf::st_coordinates(.)[,2]) %>%
  relocate(LatUTM6S, .before = GPS_name) %>%
  relocate(LongUTM6S, .before = LatUTM6S) %>%
  st_drop_geometry()

# save the cleaned and prepped algae-environment data as a csv and push to github
write.csv(prepped_algae, 
          paste0(alg_wd, "Cleaned_Tetiaroa_Turbinaria_2021_Data.csv"),
          row.names = FALSE)
#write.csv(algae_data_csv,
#         paste0(git_wd, "Cleaned_Tetiaroa_Turbinaria_2021_Data.csv"),
#        row.names = FALSE)
