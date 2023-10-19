#bin data by aquatic area and plot

#I don't think I want snapped points here.
#I think the cropped flame data is good
points <- readRDS(file.path(processed_path, paste(flame_file, "_all_snapped", ".rds", sep="")))

points$AQUA_CODE <- factor(points$AQUA_CODE, levels=c("MNC", "CB", "SC", "TRC", "CFL", "LM", "N"))
points$AQUA_DESC <- factor(points$AQUA_DESC, levels=c("Main Navigation Channel", 
                                                      "Channel Border", 
                                                      "Side Channel",
                                                      "Tributary Channel", 
                                                      "Contiguous Floodplain Lake", 
                                                      "Lake Michigan",
                                                      "Non-aquatic"))

no3 <- ggplot()+
  geom_boxplot(data = points, aes(AQUA_DESC, NO3_mgL, fill=AQUA_DESC))+
  scale_fill_brewer(palette="Dark2")+
  labs(x="Aquatic area feature", y="NO3 mg/L")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  theme(legend.position="none")+
  theme(text=element_text(size=14))
print(no3)

FP_bg <- ggplot()+
  geom_boxplot(data = points, aes(AQUA_DESC, FP_BlueGreen, fill=AQUA_DESC))+
  scale_fill_brewer(palette="Dark2")+
  labs(x="Aquatic area feature", y="FP_BlueGreen")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  theme(legend.position="none")+
  theme(text=element_text(size=14))
print(FP_bg)

chla <- ggplot()+
  geom_boxplot(data = points, aes(AQUA_DESC, chlor_ugL, fill=AQUA_DESC))+
  scale_fill_brewer(palette="Dark2")+
  labs(x="Aquatic area feature", y="Chlor a ug/L")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  theme(legend.position="none")+
  theme(text=element_text(size=14))
print(chla)

CH4_Dry <- ggplot()+
  geom_boxplot(data = points, aes(AQUA_DESC, CH4_Dry, fill=AQUA_DESC))+
  scale_fill_brewer(palette="BuGn")+
  labs(x="Aquatic area feature", y="CH4_Dry")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  theme(legend.position="none")+
  theme(text=element_text(size=14))
print(CH4_Dry)
