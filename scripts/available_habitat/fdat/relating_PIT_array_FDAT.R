library(httr2)
library(lubridate)
library(sf)
library(ggmap)

## Defining functions for querying USGS StreamStats to define watershed

getWatershed <- function(lat, lon, rcode, crs = 4326) {
  # Prepare the URL and parameters for the GET request
  url <- "https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?"
  
  request(url) %>% 
    # Add query parameters
    req_url_query(rcode = rcode,
                  xlocation = lon,
                  ylocation = lat,
                  crs = crs) %>% 
    req_perform() %>% 
    resp_body_json() 
  
}

convert_geo_to_df <- function(dat) {
  rows <- length(dat[[1]]); rows
  cnames <- c("lon","lat")
  
  newdat <- as.data.frame(matrix(NA,nrow = rows, ncol = length(cnames)))
  colnames(newdat) <- cnames
  
  for (i in 1:rows) {
    newdat[i,] <- unlist(dat[[1]][[i]])
  }
  newdat
  
}

converttodf <- function(dat) {
  rows <- length(dat); rows
  cnames <- names(dat[[1]])
  
  newdat <- as.data.frame(matrix(NA,nrow = rows, ncol = length(cnames)))
  colnames(newdat) <- cnames
  
  for (i in 1:length(dat)) {
    newdat[i,] <- unlist(dat[[i]])
  }
  newdat
}

## Habitat metric, in this case I am using FDAT ()

dir("C:/Users/rvosbigian/Documents/GIS_data/FDAT_DanIsaak/FDAT_densities")

fdat <- st_read(dsn = "C:/Users/rvosbigian/Documents/GIS_data/FDAT_DanIsaak/FDAT_densities",
                layer = "FDAT_lines_densities")

fdatcrs <- st_transform(fdat,crs = 4326)

setwd("C:/Users/rvosbigian/OneDrive - University of Idaho/Chapter3_Habitat/HabitatIPReplacement")


library(readxl)

SteelheadAbundance <- read_excel("LGR_AllSummaries_Steelhead_KinzerGithub.xlsx",sheet = "Site Esc")


# this is from the PTAGIS website (see https://www.ptagis.org/Sites/Map)
RefMMR <- read_excel("PTAGISMRRSitesRef.xlsx",sheet = 1) %>%
  dplyr::select(site = `MRR Site Info Code`,
                Latitude = `MRR Site Latitude Value`,
                Longitude = `MRR Site Longitude Value`) %>%
  mutate(Longitude = as.double(Longitude),
         Latitude = as.double(Latitude)) %>%
  distinct(site,.keep_all = TRUE) # arbitrarily dropping duplicates

# this is from the PTAGIS website (see https://www.ptagis.org/Sites/Map)
RefInt <- read_excel("PTAGISInterrogationSitesRef.xlsx",sheet = 1) %>%
  dplyr::select(site = `Interrogation Site Code`,
                Latitude = `Location Latitude`,
                Longitude = `Location Longitude`) %>%
  mutate(Longitude = as.double(Longitude),
         Latitude = as.double(Latitude)) %>%
  distinct(site,.keep_all = TRUE) # arbitrarily dropping duplicates

CombRefs <- rbind(RefMMR,RefInt)

# I am adding site here manually. The map double checks they are all assigned correctly

AbundSite <- left_join(SteelheadAbundance,CombRefs,by = "site") %>%
  filter(!is.na(Longitude)) %>% # drop missing ones for now
  mutate(state = case_when(Latitude > 46 & Longitude < -116.9 ~ "WA",
                           Latitude < 46 & Longitude < -116.5 ~ "OR",
                           TRUE ~ "ID"))

# check that all the states are correctly assigned
library(sf)
library(maps)
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE,resolution = 0))


points_sf <- st_as_sf( AbundSite, coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

# check that all the states are correctly assigned

ggplot(usa) +
  geom_sf() +
  # coord_sf(xlim = c(-118., -113.8), 
  #          ylim = c(44, 47.1))+
  xlim(-118.8, -113) +
  ylim(44, 47.1)+
  geom_sf(mapping= aes(col = state),data = points_sf,alpha = 0.5) +
  # scale_color_gradient(low = "red",high = "blue") +
  geom_hline(aes(yintercept = 46)) +
  geom_vline(aes(xintercept = -116.9),color = "green") +
  geom_vline(aes(xintercept = -116.5),color = "salmon")


# note that they are all assigned properly. We need the state for streamstats

SF_objects <- list()

abundance_est_dat <- data.frame(Site = as.character(),
                                Param = as.character(),
                                Abundance = as.double())


# see USGS StreamStats for the definition of these
basin_data <- data.frame(Site = as.character(),
                         "DRNAREA" = as.double(),
                         "ELEV" = as.double(),
                         "ELEVMAX" = as.double(),
                         "MINBELEV" = as.double(),
                         "BSLDEM10M" = as.double(),
                         "BSLDEM30M" = as.double(),
                         "CSL1085LFP" = as.double(),
                         "RELIEF" = as.double(),
                         "PRECIP" = as.double(),
                         "PRECPRIS10" = as.double())


unique_sites <- AbundSite %>%
  distinct(site,Latitude, Longitude,.keep_all = TRUE)

# defining a map so that we can visualize each catchment.
sn_map <- ggplot(usa) +
  geom_sf() +
  # coord_sf(xlim = c(-118., -113.8), 
  #          ylim = c(44, 47.1))+
  xlim(-118.8, -113) +
  ylim(44, 47.1)


for (i in 1:nrow(unique_sites)) {
  
  
  
  message(paste0("querying site: ",unique_sites$site[i]))
  tmp_ws <- FALSE
  polygondat <- FALSE
  notyet <- TRUE
  failures <- 0
  while(notyet){
    
    
    tmp_ws <- tryCatch(expr = {getWatershed(lat = unique_sites$Latitude[i], 
                                            lon = unique_sites$Longitude[i], 
                                            rcode = unique_sites$state[i], crs = 4326)},
                       error = function(err) {
                         
                         message("Query failed...retrying")
                         
                         return(FALSE)
                       })
    
    
    # polygondat <- 
    
    polygondat <- tryCatch(expr = {tmp_ws$featurecollection[[2]][[2]]$features[[1]]$geometry},
                           error = function(err) {
                             
                             message("Watershed not returned properly...retrying")
                             
                             return(FALSE)
                           })
    
    
    if (is.logical(tmp_ws) == FALSE & is.logical(polygondat) == FALSE) {
      notyet <- FALSE
      
    } else {
      failures <- failures + 1
    }
    
    if (failures > 3) {
      message("Query failed...failed 3 times so skipping")
      notyet <- FALSE
    }
    
  }
  
  if (failures  > 3) {
    message("put in some NAs in the data please")
    print(unique_sites$site[i])
    print(paste0("lat: ",unique_sites$Latitude[i]))
    print(paste0("lon: ",unique_sites$Longitude[i]))
    next()
  }
  
  
  # convert polygon to a data frame
  polygondf <- convert_geo_to_df(polygondat$coordinates)
  
  # convert coordinates to a polygon
  polygon <- polygondf %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  # plot(polygon)
  polygon <- st_make_valid(polygon)
  
  SF_objects[[i]] <- polygon
  
  tmpmap <- sn_map +
    # xlim(-118.8, -113) +
    # ylim(44, 47.1)+
    geom_sf(data = polygon,inherit.aes = FALSE,fill = NA,color = "red",linewidth = 1) +
    geom_sf(data = points_sf,inherit.aes = FALSE,size = 0.5)
  
  
  # optional. This will plot the catchment in a figure so you can view it later
  ggsave(paste0("catchments/Map_",unique_sites$site[i],".jpeg"),plot = tmpmap,
         width = 6,height = 4,units="in"
  )
  
  # Clip the steelhead density object (fdatcrs) using the catchment
  polygon_fdat_int <- st_intersection(fdatcrs,polygon)
  # remeasure the length of each reach (units are in meters by default)
  polygon_fdat_int$ReachLenNew <- st_length(polygon_fdat_int)
  
  # Estimate abundance of juvenile steelhead within the catchment (take a look at polygon_fdat_int. it has
  # mean abundance estimates for 2000 to 2018 [S1_00_18] as well as abundance estimates for each year.
  polygon_fdat_int_abund <- polygon_fdat_int %>%
    mutate_at(.funs = function(x) x*polygon_fdat_int$ReachLenNew/100,.vars = vars(matches("S._|S.._")))
  
  # plot(polygon_fdat_int_abund)
  # this makes it into a nicer data frame 
  tmp_abundance_est_dat <- as.data.frame(polygon_fdat_int_abund) %>%
    dplyr::select(matches("S._|S.._")) %>%
    summarize_all(sum) %>%
    pivot_longer(matches("S._|S.._"),names_to = "Param",values_to = "Abundance") %>%
    mutate(Site = unique_sites$site[i],
           Abundance = as.double(Abundance)) %>%
    dplyr::select(Site, Param, Abundance)
  
  abundance_est_dat <- rbind(abundance_est_dat,tmp_abundance_est_dat)
  
  
  basinpars <- data.frame(code = c('BSLDEM10M','BSLDEM30M','CSL1085LFP','DRNAREA',
                                   'ELEV','ELEVMAX','MINBELEV','PRECIP','PRECPRIS10','RELIEF'))
  
  tmp_basin <- converttodf(tmp_ws$parameters)
  tmp_basin <- tmp_basin %>%
    dplyr::select(code,value) %>%
    filter(code %in% c("DRNAREA","ELEV","ELEVMAX","MINBELEV","BSLDEM10M","BSLDEM30M","CSL1085LFP","RELIEF","PRECIP","PRECPRIS10")) %>%
    right_join(basinpars,by = "code") %>%
    pivot_wider(names_from = "code",values_from = "value")
  # 
  basin_data <- rbind(basin_data,
                      c("Site" = unique_sites$site[i],tmp_basin))
  
}

# save(abundance_est_dat,SF_objects,
#      fdatcrs,SteelheadAbundance,basin_data,
#      CombRefs,AbundSite,unique_sites,file = "data_fdat_res.RData")




# load("data_fdat_res.RData")


# filter out sites where the delineation failed (maybe DRY2C)
subsetabundance_est_dat <- abundance_est_dat %>%
  filter(!(Site %in% c("ACM","BTM","CAC","CLC","DRY2C","JOHNSC","JOSEPC",
                       "JUL","KHS","KRS","LAP","LLS","LSEEF","LTR","MTR",
                       "PAHH","PCA","TUCH","UGR","YPP")))

# This matches up the juvenile steelhead abundance estimates to spawn year.
# The spawn year is the previous year, because steelhead are first counted as
# age-1 (not age-0). This data frame name is a little misleading, and probably
# should be age1_plus_parr_abundance
age1_parr_abundance <- subsetabundance_est_dat %>%
  mutate(Scenario = as.character(gsub("S.*_","",Param))) %>%
  filter(grepl("....",Scenario)) %>%
  mutate(Year = as.integer(Scenario),
         spawn_yr = Year - 1,
         ID = paste0(Site,"_",spawn_yr),
         parrabund = Abundance) %>% # match up to spawn year
  dplyr::select(-Year,-spawn_yr,-Site,-Scenario,-Abundance)

# This matches juvenile abundance to spawner abundance by spawn year
age1_joined <- AbundSite %>%
  mutate(ID = paste0(site,"_",spawn_yr)) %>%
  left_join(age1_parr_abundance,by = "ID")

# Plot parr abundance versus adult spawner abundance
age1_joined %>%
  filter(!is.na(parrabund),
         parrabund != 0,
         estimate != 0) %>%
  mutate(spawn_yr = as.factor(spawn_yr)) %>%
  ggplot(aes(log(parrabund),log(estimate), color = spawn_yr)) +
  geom_point(alpha = 0.4) + geom_smooth(method = "lm",se = FALSE) +
  facet_wrap(~spawn_yr)


age1_joined %>%
  filter(!is.na(parrabund),
         parrabund != 0,
         estimate != 0) %>%
  mutate(spawn_yr = as.factor(spawn_yr)) %>%
  group_by(spawn_yr) %>%
  summarize()

age1_subset <- age1_joined %>%
  filter(!is.na(parrabund),
         parrabund != 0,
         estimate != 0) %>%
  mutate(spawn_yr = as.factor(spawn_yr))


m1 <- lm(log(estimate) ~ log(parrabund) , data = age1_subset)
summary(m1)
plot(m1)
library(effects)
plot(allEffects(m1))



m2 <- glm(estimate ~ log(parrabund),family = poisson,data = age1_subset)
summary(m2)
plot(m2)
plot(allEffects(m2))


## Now use the 2000 to 2018 mean

mean_parr_abundance <- subsetabundance_est_dat %>%
  filter(grepl("S1_",Param)) %>%
  dplyr::select(site = Site,meanparrabund = Abundance) %>%
  distinct(site, meanparrabund)


meanparabund_adultspawners <- AbundSite %>%
  mutate(ID = paste0(site,"_",spawn_yr)) %>%
  left_join(mean_parr_abundance,by = "site")

meanparabund_adultspawners_subset <- meanparabund_adultspawners %>%
  filter(!is.na(meanparrabund),
         meanparrabund != 0,
         estimate != 0) 
  

m3 <- lm(log(estimate) ~ log(meanparrabund) , data = meanparabund_adultspawners_subset)
summary(m3)
plot(m3)
library(effects)
plot(allEffects(m3))


sum((exp(rstudent(m3)))^2) /  nrow(meanparabund_adultspawners_subset)
