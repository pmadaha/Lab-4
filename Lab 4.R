library(tidyverse)
library(sf)
install.packages("spData")
install.packages ("terra")
install.packages ("tmap")
install.packages("grid")
library(spData)
library(terra)
library(tmap)
library(grid)


sf::write_sf (oh_counties, "oh_counties.shp")
sf::write_sf (oh_parks, "oh_parks.shp")
sf::write_sf (oh_places, "oh_places.shp")


oh_counties<- read_sf ("./oh_counties.shp")
oh_parks <- read_sf ("./oh_parks.shp")
oh_places <- read_sf ("./oh_places.shp")
oh_rivers <- read_sf ("./tl_2023_39153_linearwater.shp")
oh_rivers2<- read_sf ("./tl_2022_39133_linearwater.shp")
oh_census<- read_csv ("./oh_counties_DP2020.csv")
oh_elev = rast("./neoh_dem.tif")



#OHIO SCALE 

oh_census<- subset(oh_census, name != "Ohio")
Oh_county_census<-left_join(oh_counties, oh_census, by = c("GEOIDFQ" = "geoid")) 

Ohio<- tm_shape(Oh_county_census) + tm_polygons (fill = "blackoraa", lty=1, lwd=1, fill.scale = tm_scale_intervals (values = "matplotlib.oranges", style = "jenks"), fill.legend = tm_legend(title = "Population Distribution", orientation = "landscape", position = tm_pos_out("center", "bottom")))+ 
  tm_scalebar(breaks = c(0, 50, 100, 150,200), text.size = 0.2, position = c ("center", "bottom")) +
  tm_layout(inner.margins = c(0.15,0.15,0.15,0.15), title = "Black or African American\nPopulation Distribution in OHIO", title.fontfamily = "mono", title.size = 0.6, component.autoscale = FALSE)

Ohio 

Ohio_inset<- tm_shape(Oh_county_census) + tm_polygons (fill = "blackoraa", lty=1, lwd=1, fill.scale = tm_scale_intervals (values = "matplotlib.oranges", style = "jenks")) + tm_legend (position = tm_pos_in("right", "bottom")) +
  tm_shape (port_summ) + tm_polygons (alpha = 0.1) + tm_borders(lty=1, lwd =3) +
  tm_scalebar(text.size = 0.2, position = c ("left", "top")) + 
  tm_layout(component.autoscale = FALSE, frame = TRUE, legend.text.size = 0.4, legend.title.size = 0.3, legend.width = 1.5, legend.height = 3)

Ohio_inset 

#LOCAL SCALE: PLACES, PARKS, AND STREAMS

port_summ<- oh_counties %>% filter(NAME=="Portage"|NAME=="Summit")
placemap<- tm_shape(oh_places) + tm_polygons()
placemap

oh_places<- st_transform(oh_places, crs= st_crs(oh_counties))
places_port_summ<-st_intersection(oh_places, port_summ)
oh_parks<- st_transform(oh_parks, crs= st_crs(places_port_summ))
oh_rivers<- st_transform(oh_rivers, crs= st_crs(oh_counties))
oh_rivers2<- st_transform(oh_rivers2, crs = st_crs(oh_counties))
river_port_summ<- st_intersection (oh_rivers, port_summ)
river2_port_summ<- st_intersection(oh_rivers2,port_summ)
finalriver<- st_union(river_port_summ,river2_port_summ)
parks_port_summ<- st_intersection(oh_parks, port_summ)


munc_bound<- tm_shape(places_port_summ) + tm_polygons(alpha = 0.5) + tm_borders(lwd = 1, col = "black") # tm_text("NAME", size = 0.5) (I would've added this code but I think some of the polygons overlap I tried making it valid but I still ran into the same error)
munc_bound


park_map<- tm_shape(parks_port_summ) + tm_polygons(fill = "FEATTYPE", palette= "matplotlib.greens")
park_map

stream_map<- tm_shape (finalriver) + tm_lines (lwd=1, col = "darkblue")
  
stream_map

#LOCAL SCALE ELEVATION

oh_elev_proj<- terra::project(oh_elev, crs(port_summ))

plot(oh_elev_proj)

spatvect<- vect(port_summ)
crs(oh_elev_proj) == crs(spatvect)

port_summ_elev<- terra::crop (oh_elev_proj, spatvect)
plot(port_summ_elev)


portsumm_elevmap<- tm_shape(port_summ_elev) + tm_raster() +  tm_compass(position = c("right", "bottom"), size = 1)+ tm_shape(port_summ) + tm_polygons(alpha = 0.5) + tm_lines (lwd = 2, col = "black")
portsumm_elevmap

portsumm_map<- portsumm_elevmap + munc_bound + stream_map + park_map + tm_check_fix() + tm_legend (orientation = "landscape", position = tm_pos_out("center", "bottom"))
portsumm_map

  
#INSET MAP
  
norm_dim = function(obj){
  bbox = st_bbox(obj)
  width = bbox[["xmax"]] - bbox[["xmin"]]
  height = bbox[["ymax"]] - bbox[["ymin"]]
  w = width / max(width, height)
  h = height / max(width, height)
  return(unit(c(w, h), "snpc"))
}
main_dim = norm_dim(oh_region)
ins_dim = norm_dim(port_summ)

main_vp = viewport(width = main_dim[2], height = main_dim[1])

ins_vp = viewport(width = ins_dim[2] * 0.7, height = ins_dim[1] * 0.7,
                  x = unit(1, "npc") - unit(0.6, "cm"), y = unit(0.9, "cm"),
                  just = c("right", "bottom"))

grid.newpage()
print(portsumm_map, vp = main_vp)
pushViewport(main_vp)
print(Ohio_inset, vp = ins_vp)
grid.text("Portage x Summit Park & Stream Distribution", x = 0.5, y = 0.99)


#STATIC MAP 

#Looking at the relationship with park availability and population density  

Pop_dens <- Oh_county_census %>% st_transform(crs = 5030 ) %>% mutate(land_area = as.numeric(st_area(.)/1e6), pop_density =  poptotal / land_area) #projected based on the best projection to find area, mutate area field and find the pop density using area and total population 
NEpop_dens<- Pop_dens %>% dplyr::filter ( NAME %in% c("Ashland","Ashtabula", "Carroll", "Columbiana", "Cuyahoga", "Erie", "Geauga", "Holmes", "Huron", "Lorain", "Lake", "Medina", "Mahoning", "Portage", "Richland", "Stark", "Summit", "Tuscarawas","Trumbull", "Wayne")) #filter based on NE counties according to google. SO that the counties are within the dem. The counties on the left (Eerie, Huron etc) were included as extra.
plot(NEpop_dens)

oh_elev_projected<- terra::project(oh_elev, crs(NEpop_dens))
spat_pop_vect<- vect(NEpop_dens)
pop_dens_elev<- terra::crop (oh_elev_projected, spat_pop_vect) #crop the raster to the vector and discover elevation values

plot(pop_dens_elev)

NEparks<- st_intersection(oh_parks, NEpop_dens)
st_crs(NEparks) == st_crs(NEpop_dens) #change projection to match projected elevation. 

#Trial Map: wanted to find the relationship between black or AA population based on pop density however, mapping it was difficult due to the difficulty in seeong the elevation in the background. 

Static_Map = tm_shape(pop_dens_elev, bbox = NEpop_dens) + tm_raster(palette = "Greys", alpha = 0.2, legend.show = TRUE) +
  tm_shape (NEpop_dens) + tm_polygons (fill = "blackoraa", lty=1, lwd=1,fill_alpha = 0.4, fill.scale = tm_scale_intervals (values = "matplotlib.oranges", style = "jenks")) +
  tm_shape(NEparks) + tm_polygons(fill = "FEATTYPE", palette= "matplotlib.greens") +
  tm_shape(NEpop_dens) + tm_bubbles( size = "pop_density", fill = "tomato", lty = 1, lwd = 1, fill.scale = tm_scale_intervals(style = "equal"))

#Instead focused on just the pop density and parks, included the elevation 

Static_Map_final = tm_shape(pop_dens_elev, bbox = NEpop_dens) + tm_raster(palette = "Greys", alpha = 0.6, legend.show = TRUE, title = "NEOH Elevation") +
  tm_shape (NEpop_dens) + tm_polygons (fill_alpha = 0.4, lty = 1, lwd = 1) +
  tm_shape(NEparks) + tm_polygons(fill = "FEATTYPE", palette= "matplotlib.greens", title = "Type of Park") +
  tm_shape(NEpop_dens) + tm_bubbles( size = "pop_density", col = "tomato", title.size = "Population Desnity KMsq", lty = 1, lwd = 1, fill.scale = tm_scale_intervals(style = "equal")) + 
  tm_scalebar(breaks = c(0, 50, 100, 150), text.size = 0.75, position = c ("center", "bottom")) +
  tm_compass(position = c("right", "bottom"), size = 1) + 
  tm_layout(inner.margins = c(0.15,0.15,0.15,0.15), title = "Population and it's influence of Park Distribution\nin Northeast OHIO", title.fontfamily = "mono", title.size = 0.6, component.autoscale = FALSE)



Static_Map_final

#Q1: I made map one just to understand the distribution of black or African Americans within the state of OH, and was curious to see if they have access to social spaces after doing the rest of the local scale maps. 
#Q2: I felt like my group were more aware on how to code everything, I typically have to do it step by step and see what each line of code does and how it affects the final view. Also I don't know why I got into margins a lot while doing this lab, and I've seen how that impacts the placement of certain objects within the frame, and that was different for the Frankenmap and how it affects scaling hence my choice on changing how the inset should look for the final frankenmap.
#Q3: I decided to recycle the data I already have to make the second map. At first I wanted to view the distribution of African Americans within the State and if they are prone to be residing in dense areas, but due to the lack of comfortably with the code I wasn't able to communicate this well. Instead I decided to focus on how parks are distributed within NE Ohio and if there is a relationship to their locations with densely populated areas. The elevation was an added bonus as now you get to see how people and parks are distributed across different elevations. I think I was able to achieve this goal one can make a relationship between the three variables. 
#Q4: That everything, I mean everything has to be coded with intent, the scaling, spacing, and design of the map needs to be detailed. The only thing I truly struggled with was the dimensions the trial and error with numbers consumed a lot of time, and trying to understand how scaling other components of the map can impact the design of it and how everything is organized. 

