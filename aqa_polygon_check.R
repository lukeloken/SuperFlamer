dir <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/ProcessedObjects/1_flame_intersected"

points_aqa <- readRDS(file.path(dir, "all_tripsflame_intersected_aqa.rds"))

p <- points_aqa %>%
  select(AQUA_CODE, AQUA_DESC, CH4_Dry, Pool, geometry, Name)

p2 <- p%>% filter(AQUA_CODE != "MNC", AQUA_CODE != "CB")

p3 <- p %>% filter(AQUA_CODE=="N")

aqa_count <- p %>% count(AQUA_DESC)

named_areas <- p2 %>% count(Name)

point_plot2 <- ggplot()+
  geom_sf(data=points, aes(geometry=geometry), color="red")+
  geom_sf(data=p, aes(geometry=geometry), color="black")+
  theme_classic()
print(point_plot2)

nonaquatic <- ggplot()+
  geom_sf(data=p, aes(geometry = geometry), color='black')+
  geom_sf(data=p3, aes(geometry = geometry), color="darkgreen")+
  theme_classic()
print(nonaquatic)
#looks like they're clustered at locks?