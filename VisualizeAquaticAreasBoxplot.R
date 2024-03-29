#bin data by aquatic area and plot

#I don't think I want snapped points here.
#I think the cropped flame data is good
# output_path <- file.path("C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/Merged_Illinois_Jul_2023", "boxplots")

# points <- readRDS(file.path(processed_path, paste(flame_file, "_all_snapped", ".rds", sep="")))
plot_aqa_boxplots <- function(points, geodata, output_path, date){

  plotvars_i <- names(geodata)

var_i = "NO3_mgL"

  for (var_i in plotvars_i) {
    
    data_i <- points %>% 
      select(any_of(var_i), AQUA_DESC) %>% 
      filter(!is.na(var_i))
  
  # points$AQUA_CODE <- factor(points$AQUA_CODE, levels=c("MNC", "CB", "SC", "TRC", "CFL", "LM", "N"))
    data_i$AQUA_DESC <- factor(points$AQUA_DESC, levels=c("Lake Michigan",
                                                        "Main Navigation Channel", 
                                                        "Channel Border", 
                                                        "Side Channel",
                                                        "Tributary Channel", 
                                                        "Contiguous Floodplain Lake", 
                                                        "Contiguous Impounded",
                                                        "Non-aquatic",
                                                        "Other"))
    
    data_i <- data_i %>%
      arrange(AQUA_DESC)
  
    colors_map = c("#2A5783", "black", "#005f73", "#0a9396", "#c1d5d2", "#b0d1b1", "#519d54", "#24693d", '#BB745a')
    names(colors_map) <- levels(data_i$AQUA_DESC)
    
    
    fig <- ggplot()+
      geom_boxplot(data = data_i, aes(AQUA_DESC, .data[[var_i]], fill=AQUA_DESC))+
      scale_fill_manual("Aquatic areas", values = colors_map)+
      labs(x="Aquatic area feature", y=var_i, title=date)+
      scale_x_discrete(labels=c("LM", "MNC", "CB", "SC", "TRC", "CFL", "CIMP", "N", "Other"))+
      theme_classic()+
      # theme(axis.text.x = element_blank())+
      # theme(axis.text.x = element_text(angle=45, hjust=1))+
      theme(legend.position = "right",
            legend.justification = c(0,0), 
            legend.background = element_rect(fill = NA, colour=NA),
            legend.text=element_text(size=8),
            legend.title=element_text(size=10),
            legend.title.align = 0.5,
            legend.key.height = unit(.4, "cm"),
            legend.key.width = unit(1.2, "cm"), 
            panel.border=element_rect(fill=NA, colour="black"), 
            legend.direction="vertical")+
      guides(fill = guide_legend(ncol = 1))+
      theme(text=element_text(size=11))
    print(fig)
  
    ggsave(file.path(output_path, "boxplots", date, paste(var_i, ".png", sep="")),
           fig, width = 6, height = 2.5, units = "in")
  }
}
# FP_bg <- ggplot()+
#   geom_boxplot(data = points, aes(AQUA_DESC, FP_BlueGreen, fill=AQUA_DESC))+
#   scale_fill_brewer(palette="Dark2")+
#   labs(x="Aquatic area feature", y="FP_BlueGreen")+
#   theme_classic()+
#   theme(axis.text.x = element_text(angle=45, hjust=1))+
#   theme(legend.position="none")+
#   theme(text=element_text(size=14))
# print(FP_bg)
# 
# chla <- ggplot()+
#   geom_boxplot(data = points, aes(AQUA_DESC, chlor_ugL, fill=AQUA_DESC))+
#   scale_fill_brewer(palette="Dark2")+
#   labs(x="Aquatic area feature", y="Chlor a ug/L")+
#   theme_classic()+
#   theme(axis.text.x = element_text(angle=45, hjust=1))+
#   theme(legend.position="none")+
#   theme(text=element_text(size=14))
# print(chla)
# 
# CH4_Dry <- ggplot()+
#   geom_boxplot(data = points, aes(AQUA_DESC, CH4_Dry, fill=AQUA_DESC))+
#   scale_fill_brewer(palette="BuGn")+
#   labs(x="Aquatic area feature", y="CH4_Dry")+
#   theme_classic()+
#   theme(axis.text.x = element_text(angle=45, hjust=1))+
#   theme(legend.position="none")+
#   theme(text=element_text(size=14))
# print(CH4_Dry)
