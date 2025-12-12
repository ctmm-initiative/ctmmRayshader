library(colorspace)
library(rayimage)
library(readxl) 
library(magick)
library(rayshader)
library(sp)
library(raster)
library(scales)
library(sf)
library(rstac)
library(dplyr)
library(ctmm)
library(tiff)
library(ggplot2)
library(lubridate)

# Creates a bounding box for a movement bank formatted dataframe
make_bbox=function(sf_object=NULL, DATA=NULL, padding=0){
  if(missing(sf_object)==FALSE){
    sf_latlon <- st_transform(sf_object, crs = 4326)
    bbox=c()

    bb=st_bbox(sf_latlon)
    bbox[[1]]=bb[[1]]-padding #west
    bbox[[2]]=bb[[2]]-padding #south
    bbox[[3]]=bb[[3]]+padding #east
    bbox[[4]]=bb[[4]]+padding #north
    return(bbox)
  }
  else if(missing(DATA)==FALSE){
    if (class(DATA)=="list" | class(DATA)=="telemetry"){
      extent_ctmm=ctmm::extent(DATA)
      bbox=c()
      bbox[[1]]=extent_ctmm[1,1]-padding#west
      bbox[[2]]= extent_ctmm[1,2]-padding#south
      bbox[[3]]= extent_ctmm[2,1]+padding#east
      bbox[[4]]= extent_ctmm[2,2]+padding#north
      return(bbox)
    }
    
    bbox_temp=extent(DATA)
    bbox=c()
    bbox[[1]]=min(bbox_temp$longitude)-padding #west
    bbox[[2]]=min(bbox_temp$latitude)-padding#south
    bbox[[3]]= max(bbox_temp$longitude)+padding#east
    bbox[[4]]= max(bbox_temp$latitude)+padding#north
    return(bbox)
  }
  
}


##################################################
# find landsat tiles that cover the bounding box
#################################################

merge_tiles=function(df_bbox){
  ##print("in")
  # Step 1: Define bounding box coordinates
  xmin = df_bbox[[1]]
  xmax = df_bbox[[3]]
  ymin = df_bbox[[2]]
  ymax = df_bbox[[4]]
  
  # Step 2: Create a matrix with the 4 corners, closing the loop
  bounding_box_coords = matrix(c(xmin, ymin,  # bottom-left
                                  xmax, ymin,  # bottom-right
                                  xmax, ymax,  # top-right
                                  xmin, ymax,  # top-left
                                  xmin, ymin), # closing the loop back to bottom-left
                                ncol = 2, byrow = TRUE)
  ##print("bbox made")
  # Step 3: Create a polygon geometry using the coordinates
  bounding_box_polygon = st_polygon(list(bounding_box_coords))
  ##print("bbox polygon made")
  
  # Step 4: Read shapefile and convert to sf object
  tiles = sf::read_sf("WRS2_descending_0/WRS2_descending.shp")
  ##print("tiles file loaded")
  
  # Assuming that the shapefile already has geometry columns, no need for coords parameter
  tiles_sf = st_as_sf(tiles, crs = st_crs(map))
  ##print("make tiles_sf")
  
  # Step 5: Loop over geometries and check if they cover the bounding box polygon
  tiles_sf$cover_area=0
  paths_rows = list()
  
  for (i in 1:length(tiles_sf$geometry)) {
    
    temp_intersection=st_intersection(bounding_box_polygon, tiles_sf$geometry[[i]])
    if(length(temp_intersection)==1){
      ###print("the tile is on the bbox")
      cover_area=st_area(temp_intersection)
      tiles_sf$cover_area[[i]]=cover_area
      
      if(cover_area==st_area(bounding_box_polygon)){
        ##print("the tile completly covers the bbox")
        
        # Append a list containing path and row to the paths_rows list
        merged_tile=tiles_sf$geometry[[i]]
        path = tiles_sf$PATH[[i]]
        row = tiles_sf$ROW[[i]]
        paths_rows = append(paths_rows, list(list(path, row)))
        return(list(merged_tile, paths_rows,bounding_box_polygon))
        break
      }
    }
  }
  
  if(length(paths_rows)>0){
    ##print("done")
  } else{
    ###print("not done")
    tiles_sf=tiles_sf[order(tiles_sf$cover_area, decreasing = TRUE),]
    i=2
    merged_tile=tiles_sf$geometry[[1]]
    path = tiles_sf$PATH[[1]]
    row = tiles_sf$ROW[[1]]
    paths_rows = append(paths_rows, list(list(path, row)))
    while(cover_area!=st_area(bounding_box_polygon)){
      path = tiles_sf$PATH[[i]]
      row = tiles_sf$ROW[[i]]
      paths_rows = append(paths_rows, list(list(path, row)))
      merged_tile=st_union(merged_tile, tiles_sf$geometry[[i]])
      merged_intersection=st_intersection(bounding_box_polygon, merged_tile)
      cover_area=st_area(merged_intersection)
      ###print(paste0(i," tiles added"))
      i=i+1
    }
  }
  return(list(merged_tile, paths_rows,bounding_box_polygon))
}

########################################
# get_landsat
# use the stac api to get the get data from Landsat collection 2 level 2  
#stored on the Microsoft planetary computer
########################################

get_landsat=function(df_bbox, time_interval){
  s_obj <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1/")
  print("test1.1")
  landsat_obj <- s_obj %>%
    stac_search(collections = "landsat-c2-l2",
                bbox = df_bbox
                #,datetime = time_interval
    )%>%get_request()%>%items_sign(sign_fn = sign_planetary_computer())
  return(landsat_obj)
}

#####################################
#get_nasadem
## use the stac api to get the elevation data from nasadem
#stored on the Microsoft planetary computer
####################################

get_nasadem=function(df_bbox){
  s_obj <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1/")
  nasadem_obj <- s_obj %>%
    stac_search(collections = "nasadem",
                bbox = df_bbox) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())
}

#################################
# make time interval
#makes a time inverval that the movement data was colected over
###############################

# I need to fix this
make_time_interval=function(df){
  # if (class(df)=="list" | class(df)=="telemetry"){
  #   ##print("hi")
  #   min_time=trimws(min(DATA$timestamp))
  #   max_time=trimws(max(DATA$timestamp))
  # 
  # 
  #   substr(min_time,11,11)="T"
  #   min_time=substr(min_time,1,19)
  #   min_time=paste0(min_time,"Z")
  # 
  #   substr(max_time,11,11)="T"
  #   max_time=substr(max_time,1,19)
  #   max_time=paste0(max_time,"Z")
  # 
  #   time_interval=paste(min_time, max_time, sep="/")
  #   ##print(time_interval)
  return("2020-01-01T10:15:10Z/2024-12-29T10:03:47Z")
  ###print(time_interval)
  #return(time_interval)
}

##########################
#get_seasons
#finds indexes of landsat rasters from a given season
###########################
get_seasons=function(landsat_obj, season="all"){
  if(season=="all"){
    season_indexes=c(1:length(landsat_obj$features))
    return(season_indexes)
  }
  seasons=data.frame(spring=c(3,4,5),
                     fall=c(9,10,11),
                     summer=c(6,7,8),
                     winter=c(12,1,2))
  
  
  season_indexes=list()
  
  i=1
  for(obj in landsat_obj$features){
    date=obj$properties$created
    month=as.integer(substr(date, 6, 7))
    if(month %in% seasons[[season]]){
      season_indexes=append(season_indexes, i)
    }
    i=i+1
  }
  return(season_indexes)
}

#############################
# find_min_cloud
# finds the index of the landsat object that has the least cloud coverage 
#with in the time interval
############################

find_min_cloud=function(landsat_obj,indexes_of_intrest){
  min_cloud_cover=100
  min_cloud_index=0
  for (i in indexes_of_intrest){
    ##print(paste0(i, " In min cloud function and checking this index"))
    if (landsat_obj$features[[i]]$properties$`eo:cloud_cover`<=min_cloud_cover 
        & landsat_obj$features[[i]]$properties$`eo:cloud_cover`>0
    ){
      min_cloud_cover=landsat_obj$features[[i]]$properties$`eo:cloud_cover`
      min_cloud_index=i
    }
  }
  return(min_cloud_index)
}


##############################
# make_landsat_rgb 
#take the min cloud index and the landsate_obj and creates an rgb raster
##############################

make_landsat_rgb=function(landsat_obj, min_cloud_index){
  b_url <- paste0("/vsicurl/", landsat_obj$features[[min_cloud_index]]$assets$blue$href)
  b_data <- terra::rast(b_url)
  landsat_b = raster::raster(b_data)
  
  r_url <- paste0("/vsicurl/", landsat_obj$features[[min_cloud_index]]$assets$red$href)
  r_data <- terra::rast(r_url)
  landsat_r = raster::raster(r_data)
  
  
  g_url <- paste0("/vsicurl/", landsat_obj$features[[min_cloud_index]]$assets$green$href)
  g_data <- terra::rast(g_url)
  landsat_g = raster::raster(g_data)
  #print("rasters are loaded")
  
  #landsat_rgb=sqrt(raster::stack(landsat_r, landsat_g, landsat_b))
  #landsat_rgb=raster::stack(landsat_r, landsat_g, landsat_b)
  landsat_rgb <- raster::stack(landsat_r, landsat_g, landsat_b)
  
  # Force loading into memory
  landsat_rgb <- sqrt(landsat_rgb)
  
  return(landsat_rgb)
}

#############################
# crop_rasters
#crops both the landsat raster and the nasadem rasters to the size
#of the bounding box created from the data frame
#############################

crop_rasters=function(e, landsat_rbg, elevation_utm){
  landsat_rgb_cropped = raster::crop(landsat_rbg , e)
  elevation_cropped = raster::crop(elevation_utm, e)
  
  names(landsat_rgb_cropped) = c("r","g","b")
  
  landsat_r_cropped = rayshader::raster_to_matrix(landsat_rgb_cropped$r)
  landsat_g_cropped = rayshader::raster_to_matrix(landsat_rgb_cropped$g)
  landsat_b_cropped = rayshader::raster_to_matrix(landsat_rgb_cropped$b)
  #print("rgb is cropped")
  
  elevation_matrix = rayshader::raster_to_matrix(elevation_cropped)
  
  landsat_rgb_array = array(0,dim=c(nrow(landsat_r_cropped),ncol(landsat_r_cropped),3))
  
  landsat_rgb_array[,,1] = landsat_r_cropped/255 #Red layer
  landsat_rgb_array[,,2] = landsat_g_cropped/255 #Blue layer
  landsat_rgb_array[,,3] = landsat_b_cropped/255 #Green layer
  
  
  landsat_rgb_array = aperm(landsat_rgb_array, c(2,1,3))
  
  return(list(landsat_rgb_array, elevation_cropped, elevation_matrix))
} 

##########################
# merge_nasadem_rasters
#merges the rasters for the nasadem when there are more than one tile
##########################

merge_nasadem_rasters=function(nasadem_obj){
  if (length(nasadem_obj$features)==1){
    elevation_data=raster::raster(nasadem_obj$features[[1]]$assets$elevation$href)
  }else{
    elevations=c()
    for (i in 1:length(nasadem_obj$features)){
      elevations[[i]]=raster::raster(nasadem_obj$features[[i]]$assets$elevation$href)
    }
    
    elevation_data = do.call(raster::merge, elevations)
  }
  return(elevation_data)
}


#########################
# get_extent_utm
# #converts the  extent from the original CRS to the UTM CRS
#########################

get_extent_utm=function(df_bbox,elevation_utm){
  south_west = c(y=df_bbox[[1]], x=df_bbox[[2]])
  north_east   = c(y=df_bbox[[3]], x=df_bbox[[4]])
  
  extent_latlong = sp::SpatialPoints(rbind(south_west,north_east),proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  
  extent_utm = sp::spTransform(extent_latlong, raster::crs(elevation_utm))

  e = raster::extent(extent_utm)
  return(e)
}

###############################
# convert_location_data_to_utm
# converts the data frame lat long to the same CRS that is being
#plotted with the rayshader plot 3d method
################################
convert_location_data_to_utm=function(DATA, elevation_cropped){
  
  #locations <- DATA%>%select(longitude,latitude)
  locations=data.frame(longitude=DATA$longitude,
                       latitude=DATA$latitude)
  coords <- SpatialPoints(locations, proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  # Get CRS from elevation_cropped
  crs_utm <- crs(elevation_cropped)
  coords_utm <- SpatialPoints(spTransform(coords, crs_utm))
  
  utm_y=c() 
  utm_x=c()
  
  for(i in 1:length(coords_utm)){
    utm_y[[i]]=coordinates(coords_utm[i])[2]
    utm_x[[i]]=coordinates(coords_utm[i])[1]
  }
  return(list(utm_y, utm_x))
}

########################
# FIT_CTMM
# fits ctmm models for a list of individuals using the ctmm.guess function
########################

FIT_CTMM=function(DATA, individuals){
  FIT_list=list()
  if(length(individuals)==1){
    for(individual in individuals){
      GUESS <- ctmm.guess(DATA,interactive=FALSE)
      FIT <- ctmm.select(DATA,GUESS,trace=3)
      FIT_list=append(FIT_list, list(FIT))
      if(!file.exists(paste0(" fit_models/",individual,".rda"))){
        #print("Saving model")
        save(FIT, file=paste0("fit_models/",individual,".rda"))
      }
      
    }
    return(FIT_list)
  }
  for(individual in individuals){
    GUESS <- ctmm.guess(DATA[[individual]],interactive=FALSE)
    FIT <- ctmm.select(DATA[[individual]],GUESS,trace=3)
    FIT_list=append(FIT_list, list(FIT))
    
    if(!file.exists(paste0("fit_models/",individual,".rda"))){
      #print("Saving model")
      save(FIT, file=paste0("fit_models/",individual,".rda"))
    }
  }
  
  return(FIT_list)
}


###########################
#animate_sun_circle
# makes mp4 file
# shadows movements are animated
###########################

animate_sun_circle=function(akde_list, DATA,landsat_rgb_cropped,elevation_cropped,
                            combined_CI, AKDE_color_list,individuals, AKDE=TRUE,
                            show_data=FALSE,shadow_intesity, stretch,spin_animation=FALSE, 
                            num_frames=num_frames, Upscale_factor=Upscale_factor){
  if(show_data==TRUE & !is.null(DATA)){
    utm_locations=list()
    locations=data.frame()
    if(length(individuals)==1){
      temp=data.frame(longitude=DATA$longitude,
                      latitude=DATA$latitude,
                      identity=DATA@info$identity
      )
      locations=rbind(locations,temp)
    }else{
      for(df in DATA){
        temp=data.frame(longitude=df$longitude,
                        latitude=df$latitude,
                        identity=df@info$identity
        )
        locations=rbind(locations,temp)
      }
    }
    
    locations=na.omit(locations)
    coords <- SpatialPoints(subset(locations, select=-c(identity)), proj4string = CRS("+proj=longlat +datum=WGS84"))
    
    # Get CRS from elevation_cropped
    crs_utm <- crs(elevation_cropped)
    coords_utm <- SpatialPoints(spTransform(coords, crs_utm))
    
    utm_y=c() 
    utm_x=c()
    
    for(i in 1:length(coords_utm)){
      utm_y[[i]]=coordinates(coords_utm[i])[2]
      utm_x[[i]]=coordinates(coords_utm[i])[1]
    }
    locations_utm=data.frame(y=utm_y,
                             x=utm_x,
                             identity=locations$identity)
    #print("location data converted to UTM")
    
    colors=c()
    for(palette in AKDE_color_list){
      colors=append(colors,head(sequential_hcl(n=20, palette),1))
    }
    #print("colors made")
    
    if(show_data==TRUE){
      lookup <- data.frame(identity = individuals, color = colors)
      locations <- locations %>%
        left_join(lookup, by = "identity")
      #print("test")
      
      color_vector=as.vector(subset(locations, select=c(color)))
      
    }
  }
  elevation_matrix = rayshader::raster_to_matrix(elevation_cropped)
  
  angles= seq(0,360,length.out = num_frames)[-1]
  j=1
  for(i in 1:num_frames) {
    
    hillshade_rgb_array = make_hillshade(landsat_rgb_cropped, elevation_cropped, akde_list, combined_CI, AKDE_color_list, AKDE, sunangle=angles[i], shadow_intesity, stretch)
    
    
    rayshader::plot_3d(hillshade_rgb_array, elevation_matrix, windowsize = c(900,900), zscale = 30/Upscale_factor, shadowdepth = -50,
                       zoom=.65, phi=40,theta=45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
    
    if(spin_animation==TRUE){
      render_camera(theta=angles[num_frames-i],phi=20)
    }
    
    
    if(show_data==TRUE){
      render_points(extent=attr(elevation_cropped,"extent"),
                    lat=unlist(utm_y), long=unlist(utm_x),
                    heightmap = rayshader::raster_to_matrix(elevation_cropped),
                    # altitude = df_individual$height.above.ellipsoid,
                    size=8,
                    zscale=30/Upscale_factor,
                    color=color_vector$color)
    }
    
    #scalebar_length=as.integer(nrow(elevation_cropped)*30/1000)
    
    #render_scalebar(limits=c(0,scalebar_length/2,scalebar_length), label_unit = "km", position = "W", y=50)
    #render_compass()
    
    render_snapshot(filename = sprintf("animation/landsat_hillshade%i.png", j))  
    #print(j)
    j=j+1
    
  }
  
  
  
  
  
  av::av_encode_video(sprintf("animation/landsat_hillshade%d.png",seq(1,num_frames,by=1)), framerate = 30,
                      output = "landsat_hillshade_7.mp4")
  
  
  system("ffmpeg -framerate 30 -i animation/landsat_hillshade%d.png -pix_fmt yuv420p landsat_hillshade_7.mp4")
}

########################
#make_hillshade
# adds shadows to landsat rgb raster using the rayshader ray_shade function
########################

make_hillshade=function(landsat_rgb_cropped, elevation_cropped, akde_list=NULL, combined_CI=NULL, AKDE_color_list,AKDE=FALSE, sunangle=50,shadow_intesity, stretch){
  
  
  heightmap=rayshader::raster_to_matrix(elevation_cropped)
  
  
  #set to correct CRS
  
  #individual_CDF_utm$layer[individual_CDF_utm$layer < .0005] <- NA
  #CDF_array <- as.array(individual_CDF_utm)
  
  
  hillshade=ray_shade(heightmap, sunangle = sunangle, sunaltitude = 80)
  Rayshade_raster=raster::raster(hillshade)
  #Rayshade_raster=flip(flip(Rayshade_raster, direction = 'y'), direction = 'x')
  # Rayshade_raster=t(flip(Rayshade_raster, direction = 'y'))
  Rayshade_raster=flip(Rayshade_raster, direction = 'y')
  # Rayshade_raster=flip(Rayshade_raster, direction = 'x')
  extent(Rayshade_raster)=extent(landsat_rgb_cropped)
  crs(Rayshade_raster)=crs(landsat_rgb_cropped)
  Black_raster=Rayshade_raster
  Black_raster[]=0
  alpha=shadow_intesity*abs(Rayshade_raster-1) #make shadow raster
  
  #plotting overlay
  #######################################################################
  tiff("landsat_with_overlay.tiff",
       width = ncol(landsat_rgb_cropped),   # Width of the image in pixels
       height = nrow(landsat_rgb_cropped), # Height of the image in pixels
       compression = "none")
  
  # plotRGB(landsat_rgb_cropped, r = 1, g = 2, b = 3, scale = maxValue(landsat_rgb_cropped), stretch="lin")
  landsat_rgb_stretched <- raster::stretch(landsat_rgb_cropped, minq = stretch[1], maxq = stretch[2])
  plotRGB(landsat_rgb_stretched, r = 1, g = 2, b = 3) 
  
  if(AKDE==TRUE){
    combined_CI_utm=sf::st_transform(combined_CI,crs(landsat_rgb_cropped))
    areas=c()
    for(i in 1:length(akde_list)){
      areas=append(areas,st_area(akde_list[[i]]$CI[3]))
      #print(st_area(akde_list[[i]]$CI[3]))
    }
    sorted_index=rank(-areas, ties.method="first")
    
    #convert AKDE to raster
    CDF_utm_list=c()
    for(i in sorted_index){
      #print(i)
      individual_CDF_utm=raster::projectRaster(akde_list[[i]]$CDF,landsat_rgb_cropped,method='bilinear')
      
      CI_mask=sf::st_transform(akde_list[[i]]$CI[3],crs(landsat_rgb_cropped))
      
      masked_CDF=mask(individual_CDF_utm, as(CI_mask, "Spatial"))
      #individual_CDF_utm$layer[individual_CDF_utm$layer < .0005] <- NA
      
      CDF_utm_list[[i]]=masked_CDF
    }
    
    # make_landsat_overlay("landsat_with_overlay.tiff", Black_raster, alpha, combined_CI_utm, CDF_utm_list, landsat_rgb_cropped, AKDE_color_list, sorted_index)
    
    
    for(i in sorted_index){
      #print(i)
      hue=head(sequential_hcl(n=20, AKDE_color_list[[i]]),-3)
      
      image(1-CDF_utm_list[[i]]$layer,col=alpha(hue,.6),legend=FALSE
            , add=TRUE
      )
    }
    
    
    plot(combined_CI_utm, legend=FALSE
         , add=TRUE
    )
    
    extent(Black_raster)=extent(landsat_rgb_cropped)
    crs(Black_raster)=crs(landsat_rgb_cropped)
    
    
    
    #par( mar=c(3,3,3,7), new=TRUE)
    
  }
  
  raster::plot(Black_raster, alpha=alpha, col="#1B1B1B", legend=FALSE
               , add=TRUE
  )
  
  dev.off()
  #######################################################################
  
  tiff_image <- readTIFF("landsat_with_overlay.tiff")
  
  hillshade_rgb_array = array(0,dim=c(nrow(landsat_rgb_cropped),ncol(landsat_rgb_cropped),3))
  hillshade_rgb_array[,,1] = tiff_image[,,1] #Red layer
  hillshade_rgb_array[,,2] = tiff_image[,,2] #Blue layer
  hillshade_rgb_array[,,3] = tiff_image[,,3]#Green layer
  return(hillshade_rgb_array)
}


#######################################################################################
# part_1=function(individuals,
#                 df=df,
#                 FIT_list=NULL,
#                 sim_path=FALSE,
#                 AKDE=FALSE,
#                 #AKDE_color_list = NULL,
#                 #show_data = FALSE,
#                 padding =.05,
#                 #animate_shadows=FALSE,
#                 #spin_animation=FALSE,
#                 #sim_animation=FALSE,
#                 season="summer",
#                 shadow_intesity=.5,
#                 stretch=c(.001,999),
#                 Upscale_factor=1
#                 ){
#   
plot_rayshader=function(individuals,df=df, FIT_list=NULL,
                        sim_path=FALSE,AKDE=FALSE,AKDE_color_list = NULL,
                        show_data = FALSE,padding =.05,animate_shadows=FALSE,spin_animation=FALSE,sim_animation=FALSE,
                        season="summer", shadow_intesity=.5, stretch=c(.001,999), animation_duration=30,Upscale_factor=1){
  

  
  #make sure the data is in the form as a telemetry object for an individual
  if (class(df)!="list" & class(df)!="telemetry"){
    #print("DATA 1")
    print(length(df$individual.local.identifier))
    print(individuals)
    print(class(individuals))
    DATA=df[df$individual.local.identifier %in%  individuals,]

    # colnames(DATA)[colnames(DATA) == "location.long"] <- "longitude"
    # colnames(DATA)[colnames(DATA) == "location.lat"] <- "latitude"
    DATA=ctmm::as.telemetry(DATA)
    #print("1.2")
  }else if(class(df)=="list"){
    print("DATA 2")
    DATA=df[individuals]
  }else{
    #print("DATA 3")
    DATA=df
  }
  
  print(class(DATA))
  time_interval=make_time_interval(DATA)


  #make fit list for individual.(it is a list since I took it from my code for multiple individuals)
  
  if(AKDE==FALSE & sim_path==FALSE){
    df_bbox=make_bbox(DATA=DATA, padding=padding)
  }else{
    if(is.null(FIT_list)){
      
      print("fitting models")
      FIT_list=FIT_CTMM(DATA, individuals)
      #save model after fitting 
    }else{
      print("models are already fit")
    }
    #print("fit list is made")
    
    akde_list=c()
    i=1
    
    for(FIT in FIT_list){
      if(length(FIT_list)==1){
        UD1_pHREML <- ctmm::akde(DATA, FIT)
        print("AKDE made from FIT model")
      } else{
        UD1_pHREML <- ctmm::akde(DATA[[FIT@info$identity]], FIT)
        print("AKDE made from FIT model")
      }
      
      
      # Load the raster file
      raster <- 1-ctmm::raster(UD1_pHREML)
      
      # Convert raster to a dataframe for plotting
      #raster_df <- as.data.frame(raster, xy = TRUE, na.rm = TRUE)
      
      # Load your sf object
      sf_akde=ctmm::as.sf(UD1_pHREML, error=TRUE)
      sf_object <- sf_akde$geometry
      
      akde_list[[i]]=list(CDF=raster,CI=sf_object)
      i=i+1
    }
    
    
    CI_list=list()
    for(i in 1:length(akde_list)){
      CI_list[[i]]=akde_list[[i]]$CI
    }
    
    combined_CI=st_combine(do.call("c", CI_list))
    
    #plot(combined_CI)
    
    #df_bbox=make_bbox(sf_object = sf_object, padding=padding)
    df_bbox=make_bbox(sf_object = combined_CI, padding=padding)
    #print("sf_object bbox made")
    #print(df_bbox)
    

  }
  
  temp=merge_tiles(df_bbox)
  #print("tiles merged")
  merged_tile=temp[[1]]
  paths_rows=temp[[2]]
  bounding_box_polygon=temp[[3]]
  
  # get data from the nasa digital elevation model that covers bounding box

  nasadem_obj=get_nasadem(df_bbox)
  elevation_data=merge_nasadem_rasters(nasadem_obj)

  
  # get landsat api end point
 
  landsat_obj=get_landsat(df_bbox, time_interval)
  return(landsat_obj)
  #print("landsat object made")
  
  #This nested for loop will find the lansat rasters that have the min cloud cover over the study area
  min_cloud_indices=list()
  for(i in 1:length(paths_rows)){
    path=paths_rows[[i]][[1]]
    row=paths_rows[[i]][[2]]
    indexes_of_intrest=list()
    for(j in 1:length(landsat_obj$features)){
      if((as.integer(landsat_obj$features[[j]]$properties$`landsat:wrs_path`)==path) & (as.integer(landsat_obj$features[[j]]$properties$`landsat:wrs_row`)==row)){
        indexes_of_intrest=append(indexes_of_intrest,j)
      }
    }
    season_indexes=get_seasons(landsat_obj, season=season)
    indexes_of_intrest <- intersect(indexes_of_intrest, season_indexes)
    min_cloud_index=find_min_cloud(landsat_obj,indexes_of_intrest)
    min_cloud_indices=append(min_cloud_indices,min_cloud_index)
  }
  
  #print("min cloud index found")
  
  
  
  # load the rgb rasters
  landsat_tiles=list()
  j=1
  for(i in min_cloud_indices){
    temp_rgb_raster=make_landsat_rgb(landsat_obj, i)
    #print(crs(temp_rgb_raster, asText=TRUE))
    landsat_tiles = append(landsat_tiles, temp_rgb_raster)
    if (j == 1) {
      # Use the first tile as the reference (template)
      landsat_tiles[[j]] <- temp_rgb_raster
    } else {
      # Project subsequent tiles to match the first tile
      temp_rgb_raster <- projectRaster(temp_rgb_raster, to = landsat_tiles[[1]], method = "bilinear")
      landsat_tiles[[j]] <- temp_rgb_raster
    }
    #print("rasters projected")
    #print(crs(landsat_tiles[[j]],asText=TRUE))
    j=j+1
  }
  
  
  # Merge the rgb rasters into one large raster
  if(length(landsat_tiles)>1){
    names(landsat_tiles)[1:2] <- c('x', 'y')
    landsat_tiles$fun <- mean
    landsat_tiles$na.rm <- TRUE
    
    merged_rgb <- do.call(mosaic, landsat_tiles)
  }else{
    merged_rgb=landsat_tiles[[1]]
  }
  
  
  #convert the crs of the elevation data to the landsat raster data crs
  elevation_utm=raster::projectRaster(elevation_data, merged_rgb , crs=crs(merged_rgb , asText=TRUE), method='bilinear')
  #print("rasters projected")
  
  #convert the extent of bounding box of the study from lat,long to UTM
  e=get_extent_utm(df_bbox, elevation_utm)
  #print("convert bbox to extent")
  
  # cropt the elevation raster and landsat raster to the size of the study area
  cropped_list=crop_rasters(e, merged_rgb, elevation_utm)
  #print("raster cropped")
  landsat_rgb_array=cropped_list[[1]]
  elevation_cropped=cropped_list[[2]]
  elevation_matrix=cropped_list[[3]]
  
  
  
  # increase the contrast of the RGB landsat raster
  landsat_rgb_contrast = scales::rescale(landsat_rgb_array,to=c(0,1))
  #landsat_rgb_contrast=landsat_rgb_array
  #print("landsat rescaled")
  
  #make raster brick for the RGB 
  landsat_rgb_cropped=raster::brick(landsat_rgb_contrast)
  extent(landsat_rgb_cropped)=e #set extent 
  crs(landsat_rgb_cropped)=crs(merged_rgb, asText=TRUE) #set CRS
  
  landsat_rgb_cropped = disaggregate(landsat_rgb_cropped, fact=Upscale_factor)
  elevation_cropped = disaggregate(elevation_cropped, fact=Upscale_factor)
  elevation_matrix = rayshader::raster_to_matrix(elevation_cropped)
  
  
  
  if(AKDE==FALSE & sim_path==FALSE){
    return(list(landsat=landsat_rgb_cropped,
                elvation=elevation_cropped))
  }else{
    return(list(landsat=landsat_rgb_cropped,
                elvation=elevation_cropped,
                combined_CI=combined_CI,
                akde_list=akde_list,
                FIT_list=FIT_list))
  }
  
  
  
#}

#########################################################################

########################
#plot_rayshader
# main function that user will interact with
#####################


  
  
# plot_rayshader=function(individuals,df=df, FIT_list=NULL,
#                         sim_path=FALSE,AKDE=FALSE,AKDE_color_list = NULL,
#                         show_data = FALSE,padding =.05,animate_shadows=FALSE,spin_animation=FALSE,sim_animation=FALSE,
#                         season="summer", shadow_intesity=.5, stretch=c(.001,999),part_1_stuff, animation_duration=30,Upscale_factor=1){
  

  # print("1")  
  # #make sure the data is in the form as a telemetry object for an individual
  # if (class(df)!="list" & class(df)!="telemetry"){
  #   #print("DATA 1")
  #   DATA=as.telemetry(df[df$individual.local.identifier %in%  individuals,])
  # }else if(class(df)=="list"){
  #   #print("DATA 2")
  #   DATA=df[individuals]
  # }else{
  #   #print("DATA 3")
  #   DATA=df
  # }
  # 
  # print("2")  
  # if(AKDE==FALSE & sim_path==FALSE){
  #   landsat_rgb_cropped=part_1_stuff$landsat
  #   elevation_cropped=part_1_stuff$elvation
  # }else{
  #   landsat_rgb_cropped=part_1_stuff$landsat
  #   elevation_cropped=part_1_stuff$elvation
  #   combined_CI=part_1_stuff$combined_CI
  #   akde_list=part_1_stuff$akde_list
  #   FIT_list=part_1_stuff$FIT_list
  # }
  
 
  print("3")  
  elevation_matrix=rayshader::raster_to_matrix(elevation_cropped)
  
  
  print(class(landsat_rgb_cropped))
  print(length(landsat_rgb_cropped))
  
  
  colors=c()
  for(palette in AKDE_color_list){
    colors=append(colors,head(sequential_hcl(n=20, palette),1))
  }
  
 
  print("4")  
  num_frames=animation_duration*30
  #make time interval form start to end of study
  #time_interval=make_time_interval(DATA)



if(animate_shadows==TRUE){
    animate_sun_circle(akde_list,DATA, landsat_rgb_cropped,
                       elevation_cropped, combined_CI, AKDE_color_list,individuals,
                       AKDE, show_data, shadow_intesity=shadow_intesity, stretch=stretch, spin_animation=FALSE, num_frames=num_frames, Upscale_factor)
}

if(AKDE==FALSE & sim_path==FALSE){
  hillshade_rgb_array = make_hillshade(landsat_rgb_cropped, elevation_cropped, akde_list=NULL, combined_CI=NULL, AKDE_color_list, AKDE, shadow_intesity=shadow_intesity, stretch=stretch)
  
}else{
  hillshade_rgb_array = make_hillshade(landsat_rgb_cropped, elevation_cropped, akde_list, combined_CI, AKDE_color_list, AKDE, shadow_intesity=shadow_intesity, stretch=stretch)
}

print("5")  
  if(sim_path==TRUE){
    #print("about to simulate path")
    lon=c()
    lat=c()
    
    SIM_locations_df=data.frame()
    print(class(FIT_list))
    print(length(FIT_list))
    for( FIT in FIT_list){
      individual=FIT@info$identity
      if(length(FIT_list)==1){
        df=DATA
      }else{
        df=DATA[[individual]]
        
      }
      
      #DATA=as.telemetry(df[df$individual.local.identifier==individual,])
      
      # SUB <- DATA$'6_tkbhai'[1:length(DATA$'6_tkbhai'$t),] 
      # SEQ_tkbhai <- seq(from=SUB$t[1],to=SUB$t[length(DATA$'6_tkbhai'$t)],by=5 %#% 'min')
      # SIM_tkbhai <- simulate(SUB,FIT_tkbhai,t=SEQ_tkbhai, complete=TRUE)
      
      SUB <- df[1:length(df$t),]
      
      SEQ <- seq(from=SUB$t[1],to=SUB$t[length(df$t)],by=10 %#% 'sec')
      #SIM <- simulate(SUB,FIT,t=SEQ, complete=TRUE) 
      SIM <- predict(SUB,FIT,t=SEQ, complete=TRUE) 
      lat=c(lat,SIM$latitude)
      lon=c(lon,SIM$longitude)
      temp=data.frame(longitude=lon,
                      latitude=lat,
                      identity=df@info$identity
      )
      SIM_locations_df=rbind(SIM_locations_df, temp)

      coords <- SpatialPoints(subset(SIM_locations_df, select=-c(identity)), proj4string = CRS("+proj=longlat +datum=WGS84"))
      # Get CRS from elevation_cropped
      crs_utm <- crs(elevation_cropped)
      coords_utm <- SpatialPoints(spTransform(coords, crs_utm))
      
      utm_y=c() 
      utm_x=c()
      
      for(i in 1:length(coords_utm)){
        utm_y[i]=coordinates(coords_utm[i])[2]
        utm_x[i]=coordinates(coords_utm[i])[1]
      }
      SIM_locations_utm=data.frame(y=utm_y,
                                   x=utm_x,
                                   identity=SIM_locations_df$identity)
    }
    
    
    print("sim locations made")
  }


  
  
  
  if(show_data==TRUE){
    utm_locations=list()
    locations=data.frame()
    # if(length(individuals)==1){
    #   temp=data.frame(longitude=DATA$longitude,
    #                   latitude=DATA$latitude,
    #                   identity=DATA@info$identity
    #   )
    #   locations=rbind(locations,temp)
    # }else{
    #   for(df in DATA){
    #     temp=data.frame(longitude=df$longitude,
    #                     latitude=df$latitude,
    #                     identity=df@info$identity
    #     )
    #     locations=rbind(locations,temp)
    #   }
    # }

    if(class(DATA)=="telemetry"
       #& class(DATA)!="list"
       ){
      print("one individual")
      temp=data.frame(longitude=DATA$longitude,
                      latitude=DATA$latitude,
                      identity=DATA@info$identity
      )
      locations=rbind(locations,temp)
    }
    else{
      print("multiple individual")
      for(df in DATA){
        print(df@info$identity)
        temp=data.frame(longitude=df$longitude,
                        latitude=df$latitude,
                        identity=df@info$identity
        )
        locations=rbind(locations,temp)  
    }
    
    }

    locations=na.omit(locations)
    coords <- SpatialPoints(subset(locations, select=-c(identity)), proj4string = CRS("+proj=longlat +datum=WGS84"))
    
    # Get CRS from elevation_cropped
    crs_utm <- crs(elevation_cropped)
    coords_utm <- SpatialPoints(spTransform(coords, crs_utm))
    
    utm_y=c() 
    utm_x=c()
    
    for(i in 1:length(coords_utm)){
      utm_y[[i]]=coordinates(coords_utm[i])[2]
      utm_x[[i]]=coordinates(coords_utm[i])[1]
    }
    locations_utm=data.frame(y=utm_y,
                             x=utm_x,
                             identity=locations$identity)
    #print("location data converted to UTM")
  }


  if(show_data==TRUE || sim_path==TRUE){
    lookup <- data.frame(identity = individuals, color = colors)
    if(sim_path==TRUE){
      locations=SIM_locations_df
    }
    locations <- locations %>%
      left_join(lookup, by = "identity")
    print(locations)
    color_vector=as.vector(subset(locations, select=c(color)))
  }
  

  #############################################################################################################
  if(spin_animation==TRUE & animate_shadows==FALSE){
    
    rayshader::plot_3d(hillshade_rgb_array, elevation_matrix, windowsize = c(900,900), zscale = 30/Upscale_factor, shadowdepth = -50,
                       zoom=.65, phi=50,theta=90,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
    
    if(show_data==TRUE){
      
      render_points(extent=attr(elevation_cropped,"extent"),
                    lat=unlist(utm_y), long=unlist(utm_x),
                    heightmap = rayshader::raster_to_matrix(elevation_cropped),
                    # altitude = df_individual$height.above.ellipsoid,
                    size=8,
                    zscale=30/Upscale_factor,
                    color=color_vector$color)
    }
    
    if(sim_path==TRUE){
      i=1
      for(individual in individuals){
        SIM_individual=SIM_locations_utm[SIM_locations_utm$identity==individual,]
        
        utm_y=SIM_individual$y
        utm_x=SIM_individual$x
        render_path(extent=attr(elevation_cropped,"extent"),
                    #lat=unlist(utm_y), long=unlist(utm_x),
                    lat=c(utm_y), long=utm_x,
                    heightmap = rayshader::raster_to_matrix(elevation_cropped),
                    #altitude = df_individual$height.above.ellipsoid,
                    resample_evenly = TRUE,
                    resample_n = 1000,
                    reorder=TRUE,
                    zscale=30/Upscale_factor,
                    color=colors[i],
                    antialias=TRUE
        )
        i=i+1
      }
    }
    
    angles= seq(0,360,length.out = num_frames)[-1]
    j=1
    for(i in 1:num_frames) {
      
      render_camera(theta=angles[num_frames-i],phi=40)
      render_snapshot(filename = sprintf("animation/landsat_hillshade%i.png", j))  
      j=j+1
      
    }
    close3d()
    av::av_encode_video(sprintf("animation/landsat_hillshade%d.png",seq(1,num_frames,by=1)), framerate = 30,
                        output = "www/Rayshader_spin_animation.mp4")
    system("ffmpeg -framerate 30 -i animation/landsat_hillshade%d.png -pix_fmt yuv420p Rayshader_spin_animation.mp4")
    
  }

  
  #-------------------------------
  #plotting time
  #print("about to plot rayshader")
  rayshader::plot_3d(hillshade_rgb_array, elevation_matrix, windowsize = c(900,900), zscale = 30/Upscale_factor, shadowdepth = -50,
                     zoom=.65, phi=50,theta=90,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
  
  if(show_data==TRUE){
    #   
    
    
    render_points(extent=attr(elevation_cropped,"extent"),
                  lat=unlist(utm_y), long=unlist(utm_x),
                  heightmap = rayshader::raster_to_matrix(elevation_cropped),
                  # altitude = df_individual$height.above.ellipsoid,
                  size=8,
                  zscale=30/Upscale_factor,
                  color=color_vector$color)
  }
  
  if(sim_path==TRUE){
    i=1
    for(individual in individuals){
      SIM_individual=SIM_locations_utm[SIM_locations_utm$identity==individual,]
      
      utm_y=SIM_individual$y
      utm_x=SIM_individual$x
      render_path(extent=attr(elevation_cropped,"extent"),
                  #lat=unlist(utm_y), long=unlist(utm_x),
                  lat=c(utm_y), long=utm_x,
                  heightmap = rayshader::raster_to_matrix(elevation_cropped),
                  #altitude = df_individual$height.above.ellipsoid,
                  resample_evenly = TRUE,
                  resample_n = 1000,
                  reorder=TRUE,
                  zscale=30/Upscale_factor,
                  color=colors[i],
                  antialias=TRUE
      )
      i=i+1
    }
  }
  
  
  scalebar_length=as.integer((nrow(elevation_cropped)*(30/Upscale_factor))/1000)
  render_scalebar(limits=c(0,scalebar_length/2,scalebar_length), label_unit = "km", position = "W", y=50)
  render_compass()
  
  
  
}














