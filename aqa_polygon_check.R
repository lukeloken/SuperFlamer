#paths to intersected flame data and original flame data
dir <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/ProcessedObjects"
flame_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/Merged_Illinois_May_2022_Jul_2023/Shapefiles"
flame_file <- "Merged_Illinois_May_2022_Jul_2023_Shapefile_AllData.rds"

#original flame data
points_all <- readRDS(file.path(flame_path, flame_file))
points_all <- st_as_sf(points_all)

#intersected with aquatic areas polygons
points_aqa <- readRDS(file.path(dir, "1_flame_intersected", "all_tripsflame_intersected_aqa.rds"))

#merged aqa polygon with Luke's edits
aqa <- readRDS(file.path(dir, "new_aqa_loken_merged.rds"))

#subset of intersected data
p <- points_aqa %>%
  select(date_time, geometry, AQUA_CODE, AQUA_DESC, CH4_Dry, Pool, Name)

#data in backwaters
p2 <- p%>% filter(AQUA_CODE != "MNC", AQUA_CODE != "CB")

#data in non-aquatic areas
p3 <- p %>% filter(AQUA_CODE=="N")

#points outside of aquatic areas polygons
p_diff <- setdiff(points_all$geometry, p$geometry)

#frequency table of flame data in each aquatic area
aqa_count <- p %>% count(AQUA_DESC)

#frequency table of flame data in each named backwater (and count of unnamed)
named_areas <- p2 %>% count(Name)

#plot points that fall outside of new polygons
outside_aqa <- ggplot()+
  geom_sf(data=aqa, aes(geometry=geometry), color="black")+
  geom_sf(data=p_diff, aes(geometry=geometry), color="orangered3", size=2)+
  labs(title="points outside of aquatic areas polygons")+
  theme_classic()
print(outside_aqa)

outside_zoom <- ggplot()+
  geom_sf(data=aqa, aes(geometry=geometry), color="black")+
  geom_sf(data=p_diff, aes(geometry=geometry), color="orangered3", size=2)+
  labs(title="points outside of aquatic areas polygons")+
  xlim(c(905000, 960000))+
  ylim(c(4600000, 4670000))+
  theme_classic()
print(outside_zoom)

#plot points still classified as non-aquatic
nonaquatic <- ggplot()+
  geom_sf(data=aqa, aes(geometry=geometry), color="black")+
  geom_sf(data=p3, aes(geometry = geometry), color="orangered3")+
  labs(title="Points classified as non-aquatic")+
  theme_classic()
print(nonaquatic)
