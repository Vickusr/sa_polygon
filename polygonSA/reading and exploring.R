library(sf)
library(tidyverse)
library(leaflet)
library(lwgeom)

subplaces <- st_read('full export-20191212T042317Z-001/full export/Sub Places.shp', crs = 4326)

subplaces <- subplaces %>% 
  select(FeatureId,OBJECTID,FeatureCla,Label,Length,Area,starts_with('SP_'),starts_with('MP_'),starts_with('DC_'))

cleaned_poly <- subplaces %>% filter(FeatureId == "A")
total_poly <- nrow(subplaces)

for (i in 1:total_poly){
  x <- tryCatch( {
         leaflet(subplaces[i,]) %>% addTiles() %>% addPolygons()
         
         }, 
        
         error = function(cond){
         cond

    
      },
      warning=function(cond){
        cond
      }
    )
 # print(x)
  
  if(is.null(x$message)){
    cleaned_poly <- cleaned_poly %>% rbind(subplaces[i,])
    
    
  }else if(x$message == "polygons not (all) closed"){
   temp <- st_make_valid(subplaces[i,])
   cleaned_poly <- cleaned_poly %>% rbind(temp)
  }else{
    print('stopped')
    print(i)
    break
  }
  
}

saveRDS(cleaned_poly,'cleaned_polygons.Rds')

testing <- subplaces[1:100,] %>% arrange(DC_NAME)


t <- leaflet(cleaned_poly[1:3000,]) %>% 
  addTiles() %>% 
  addPolygons()
t



rework_test <- st_polygon(list(as.matrix(testing[63,]$geometry[[1]])))

t <- leaflet(testing[63,]) %>% 
  addTiles() %>% 
  addPolygons()
t
