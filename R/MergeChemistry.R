

MergeChemistry <- function(home_path, 
                           chem_path, 
                           samples_merged, 
                           merge_name, 
                           map, 
                           legend = "bottomleft", 
                           plot_title = "", 
                           title_location = "outside", 
                           color_scale = "samplechem"){
  
  if(!title_location %in% c("outside", "inside")){
    title_location = "outside"
    warning('plot_location set to "outside". Only acceptes "inside" or "outside"')
  }
  
  if(!color_scale %in% c("allchem", "samplechem")){
    color_scale = "samplechem"
    warning('color_scale set to "samplechem". Only acceptes "allchem" or "samplechem"')
  }
  
  library(rgdal)
  library(sp)
  library(sf)
  library(RODBC)
  library(RgoogleMaps)
  library(ggmap)
  library(riverdist)
  library(viridis)
  library(data.table)
  library(janitor)
  library(tidyr)
  library(dplyr)
  library(lubridate)
  library(openxlsx)
  library(ggplot2)
  library(egg)
  
  crsLONGLAT <- 4326
  dir.create(file.path(home_path, merge_name, "Maps_chemistry"))
  
  chem_files <- list.files(chem_path, full.names = TRUE)
  
  Hg_csvs <- chem_files[grepl("_LCL.csv", chem_files)]
  UCD_csvs <- chem_files[grepl("DOC_for", chem_files)]
  UCD_xlsx <- chem_files[grepl("DOC_Anions", chem_files)]
  
  Cam_csvs <- chem_files[grepl("WQ_FLAMe", chem_files)]
  
  
  
  # Get data
  #USGS mercury lab
  Hg_list <- lapply(Hg_csvs, fread, sep=",", skip=0, header=T,
                    stringsAsFactors=F, na.strings=c('NA', 'NaN', '', 'null', '"NAN"'))
  
  Hg_df <- bind_rows(Hg_list) %>%
    clean_names() %>%
    mutate(lab = "USGS-Hg", 
           result_id = as.character(result_id), 
           across(c(sample_date, received, analyzed), mdy)) %>%
    filter(medium %in% c("WS", "SB")) %>%
    mutate(result_value = ifelse(result_value > ddl, result_value, 0))
  
  head(Hg_df)
  
  
  if(length(UCD_xlsx) == 0){
    #UC-Davis
    UCD_list <- lapply(UCD_csvs, fread, skip=0, header=T, fill = TRUE,
                       stringsAsFactors=F, 
                       na.strings=c('NA', 'NaN', '', 'null', '"NAN"'))
    
    UCD_df <- bind_rows(UCD_list) %>%
      clean_names() %>%
      dplyr::select(result_id = ucd_lab_id, 
                    sample_date = sample_date_time, 
                    site_id, 
                    received = date_received,
                    comments, 
                    qa_flags = qa_flag, 
                    doc_mg_c_l:hix) %>%
      mutate(lab = "UCD", 
             result_id = as.character(result_id), 
             sample_date = mdy(sample_date), 
             received = mdy(received)) %>%
      pivot_longer(doc_mg_c_l:hix, 
                   names_to = "constituent",
                   values_to = "result_value") %>%
      mutate(unit = case_when(constituent == "doc_mg_c_l" ~ "MG/L", 
                              constituent == "suva_254_l_mg_m" ~ "L/MG/M", 
                              constituent == "uv_abs_254_nm_au_cm" ~ "nm/au/cm", 
                              TRUE ~ "")) %>%
      mutate(constituent = recode(constituent, 
                                  "doc_mg_c_l" = "doc", 
                                  "suva_254_l_mg_m" = "suva254", 
                                  "uv_abs_254_nm_au_cm" = "uv_abs254"))
    
    
  } else if(length(UCD_xlsx) > 0){
    
    
    UCD_list_DOC <- lapply(UCD_xlsx, 
                           openxlsx::read.xlsx, 
                           sheet = "DOC")
    
    
    UCD_list_Anion <- lapply(UCD_xlsx, 
                             openxlsx::read.xlsx, 
                             sheet = "Anions")
    
    UCD_df_DOC <- bind_rows(UCD_list_DOC) %>%
      clean_names() %>%
      mutate(sample_date = as.Date(sample_date, origin = "1899-12-30"), 
             date_received = as.Date(date_received, origin = "1899-12-30")) %>%
      dplyr::select(result_id_doc = ucd_lab_id, 
                    sample_date, 
                    site_id = site_id_or_other_id, 
                    received = date_received,
                    comments_doc = comments,
                    # qa_flags = qa_flag, 
                    doc_mg_c_l:hix) %>%
      select(-contains("qa")) %>%
      mutate(result_id_doc = as.character(result_id_doc),
             lab = "UCD", 
             site_id = gsub(" ", "", site_id)) %>%
      mutate(site_id = recode(site_id, 
                              "DRI" = "DR1", 
                              # "TSL1a" = "TSL1", 
                              # "TSL1b" = "TSL1"
      )) %>%
      mutate(sample_date = ymd(sample_date))
    
    
    UCD_df_Anion <- bind_rows(UCD_list_Anion) %>%
      clean_names() %>%
      mutate(sample_date = as.Date(sample_date, origin = "1899-12-30"), 
             date_received = as.Date(date_received, origin = "1899-12-30")) %>%
      dplyr::select(result_id_anion = ucd_lab_id, 
                    sample_date, 
                    site_id = site_id_or_other_id, 
                    received = date_received,
                    comments_anion = comments,
                    # qa_flags = qa_flag, 
                    chloride_ppm:sulfate_ppm) %>%
      select(-contains("qa")) %>%
      mutate(result_id_anion = as.character(result_id_anion),
             lab = "UCD", 
             site_id = gsub(" ", "", site_id)) %>%
      mutate(site_id = recode(site_id, 
                              "DRI" = "DR1", 
                              # "TSL1a" = "TSL1", 
                              # "TSL1b" = "TSL1"
      )) %>%
      mutate(sample_date = ymd(sample_date))
    
    UCD_df <- full_join(UCD_df_DOC, UCD_df_Anion) %>%
      select(lab, sample_date, site_id, received, 
             contains("result_id"), contains("comments"), 
             everything()) %>%
      arrange(sample_date, site_id) %>% 
      pivot_longer(doc_mg_c_l:sulfate_ppm, 
                   names_to = "constituent",
                   values_to = "result_value") %>%
      mutate(unit = case_when(constituent == "doc_mg_c_l" ~ "MG/L", 
                              constituent == "suva_254_l_mg_m" ~ "L/MG/M", 
                              constituent == "uv_abs_254_nm_au_cm" ~ "nm/au/cm", 
                              constituent == "chloride_ppm" ~ "ppm", 
                              constituent == "nitrate_as_n_ppm" ~ "ppm", 
                              constituent == "phosphate_ppm" ~ "ppm", 
                              constituent == "sulfate_ppm" ~ "ppm", 
                              TRUE ~ "")) %>%
      mutate(constituent = recode(constituent, 
                                  "doc_mg_c_l" = "doc", 
                                  "suva_254_l_mg_m" = "suva254", 
                                  "uv_abs_254_nm_au_cm" = "uv_abs254", 
                                  "chloride_ppm" = "chloride", 
                                  "nitrate_as_n_ppm" = "nitrate", 
                                  "phosphate_ppm" = "phosphate", 
                                  "sulfate_ppm" = "sulfate")) %>%
      mutate(result_value = signif(as.numeric(result_value), 4))
  }
  
  head(UCD_df)
  
  #Cambodia
  Cam_list <- lapply(Cam_csvs, fread, skip=0, header=T, fill = TRUE,
                     stringsAsFactors=F, 
                     na.strings=c('NA', 'NaN', '', 'null', '"NAN"'))
  
  Cam_df <- bind_rows(Cam_list) %>%
    clean_names() %>%
    dplyr::select(site_id, 
                  sample_date = date_flame, 
                  contains("g_l")) %>%
    mutate(lab = "Cambodia", 
           sample_date = mdy(sample_date)) %>%
    pivot_longer(contains("g_l"), 
                 names_to = "constituent",
                 values_to = "result_value") %>%
    mutate(unit = case_when(grepl("ug_l", constituent) ~ "ug/L",
                            grepl("mg_l", constituent) ~ "mg/L",
                            TRUE ~ "")) %>%
    mutate(constituent = recode(constituent, 
                                "tp_ug_l" = "TP", 
                                "tdn_ug_l" = "TDN", 
                                "doc_mg_l" = "DOC2", 
                                "tn_ug_l" = "TN", 
                                "toc_mg_l" = "TOC", 
                                "n_nh4_ug_l" = "NH4"))
  
  #Join it all together
  chem_join <- full_join(UCD_df, Hg_df) %>%
    full_join(Cam_df) %>%
    mutate(sample_date = as.Date(sample_date))
  
  flame_all <- samples_merged %>%
    mutate(sample_date = as.Date(DateTime)) %>%
    rename(site_id = "Sample.Number") %>%
    mutate(site_id = recode(site_id, "BR2" = "B2")) %>%
    dplyr::select(site_id, sample_date, latitude, longitude, everything())
  
  site_table <- data.frame(site_id = unique(flame_all$site_id), site_type = NA) %>%
    mutate(site_type = case_when(site_id %in% c("M10", "M9", "SK1", "SS1", 
                                                "M7", "M6", "M5") ~ "MekongUpper", 
                            site_id %in% c("MRU", "MRD", "MRL", 
                                           "BR1", "B2", "B1", 
                                           "M4", "M3", "M2", "M1",
                                           "TSR1", "TL21") ~ "MekongLower", 
                            site_id %in% c("TSR3", "TSR4", "TLR2", "PS1", "TL20", "m5") ~ "TonleSapOutlet", 
                            site_id %in% c("TSL1", "TSM1", "TL12", "TSU1") ~ "TonleSapMiddle", 
                            TRUE ~ "TonleSapEdge" )) %>%
    arrange(site_type, site_id) %>%
    distinct()
  
  flame_all <- flame_all %>% 
    left_join(site_table)
  
  flame_prep <- flame_all %>%
    dplyr::select(site_id, site_type, 
                  sample_date, latitude, longitude, 
                  intersect(names(samples_merged), 
                            c("ODO_mgL", "nn03_mg", "NO3_mgL", 
                              "CH4uM", "CO2uM", 
                              "fDOM_QSU", "turb_FNU")))
  
  head(flame_prep)
  head(flame_all)
  
  chem_geo <- left_join(chem_join, flame_prep) %>%
    mutate(zzzdetect = ifelse(grepl("<", detection_flag), 
                              "Below reporting level", 
                              "Above reporting level"), 
           zzzdetect = factor(zzzdetect, c("Above reporting level", 
                                           "Below reporting level"))) 
  
  
  #chems not joined
  filter(chem_geo, is.na(latitude)) %>%
    dplyr::select(site_id, sample_date, lab) %>%
    distinct()
  
  #flame not joined
  filter(chem_geo, is.na(lab)) %>%
    dplyr::select(site_id, sample_date) %>%
    distinct()
  
  chem_geo <- chem_geo %>%
    # filter(!is.na(latitude), !is.na(longitude)) %>%
    select(sample_date, sample_time, 
           site_id, site_type, 
           site_name, latitude, longitude, 
           everything())
  # st_as_sf(coords = c("longitude", "latitude"),
  #          crs = crsLONGLAT)
  
  plot_vars <- filter(chem_geo, !is.na(constituent)) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    pull(constituent) %>%
    unique()
  
  if (legend == "bottomleft"){
    loc <- c(.02, .04)
  } else if (legend == "topleft"){
    loc <- c(.02, .85)
  } else if (legend == "bottomright"){
    loc <- c(.85, .04)
  } else if (legend == "topright"){
    loc <- c(.85, .85)
  }
  
  var_i <- 1
  for (var_i in seq_along(plot_vars)){
    var_name <- plot_vars[var_i]
    
    chem_geo_i <- filter(chem_geo, constituent == var_name) %>%
      filter(!is.na(result_value)) 
    
    if(color_scale == "samplechem") {
      chem_geo_i <- chem_geo_i %>%
        filter(!is.na(latitude) & !is.na(longitude))
    }
    
    unit_name <- unique(chem_geo_i$unit)[1]
    if(nchar(unit_name) == 0){
      label = var_name 
    } else {
      label <- paste0(var_name, " (", unit_name, ")")
    }
    
    plot_i <- ggmap(map) +
      geom_point(aes(x = longitude, 
                     y = latitude, 
                     fill = result_value, 
                     shape = zzzdetect, 
                     size = zzzdetect), 
                 data = chem_geo_i, 
                 alpha = .8) +
      scale_shape_manual(values = c(21, 25), drop = FALSE) + 
      scale_size_manual(values = c(5, 3), guide = "none") + 
      labs(fill = label, shape = "") +
      scale_fill_distiller(palette = "YlOrRd", direction = 1) +
      theme(legend.position = loc, 
            legend.justification = c(0,0), 
            legend.background = element_rect(fill = NA, colour=NA),
            legend.text=element_text(size=10, color = "white"),
            legend.title=element_text(size=12, color = "white"), 
            # legend.key.height = unit(0.5, "cm"),
            # legend.key.width = unit(1, "cm"), 
            legend.key=element_blank()) +
      guides(fill = guide_colorbar(title.position = 'top', 
                                   title.hjust = 0.5, 
                                   barwidth = 1, 
                                   barheight = 5, 
                                   ticks.colour = "white", 
                                   frame.colour = "white", 
                                   order = 1)) +
      guides(shape = guide_legend(override.aes = list(fill = "white", alpha = 1,
                                                      color = "black", size = c(5))))
    
    if(var_name == "UMHG"){
      plot_i <- plot_i + 
        scale_fill_distiller(palette = "YlOrRd", direction = 1, trans = "sqrt", 
                             limits = c(0, NA), oob = scales::squish)
    }
    
    if(plot_title != ""){
      plot_i <- plot_i + 
        ggtitle(plot_title) +
        theme(plot.title = element_text(hjust = 0.5))
      
      if(title_location == "inside"){
        plot_i <- plot_i + 
          theme(plot.title = element_text(hjust = 0.5, 
                                          margin = margin(t = 0,b = -20),
                                          color = "white"))
      }
      
    }
    
    
    print(plot_i)
    
    ggsave(file.path(home_path, merge_name, "Maps_chemistry", 
                     paste0(merge_name, "_", var_name, ".png")), 
           plot_i, 
           height = 6, width = 6, units = "in", dpi = 600)
    
    cat("\n", "Finished", var_name)
    
  }  
  
  

  chem_geo <- chem_geo %>%
    filter(!is.na(latitude), !is.na(longitude))
  
  saveRDS(chem_geo, file.path(home_path, merge_name,  "Shapefiles",
                              paste0("ChemFlame", merge_name, ".rds")))
  
  
  #Scatterplots
  variable_table <- chem_geo %>%
    select(lab, constituent, unit) %>%
    distinct()
  
  
  chem_geo_wide <- chem_geo %>%
    select(sample_date, site_id, 
           latitude, longitude, constituent, result_value) %>%
    distinct() %>% 
    group_by(sample_date, site_id, latitude, longitude, constituent) %>% 
    summarize(result_value = mean(result_value, na.rm = TRUE)) %>% 
    pivot_wider(names_from = constituent, values_from = result_value) %>%
    filter(!grepl("BLK", site_id))
  
  chem_geo_wide_all <- chem_geo_wide %>%
    full_join(flame_all) %>% 
    select(sample_date, site_id, site_type, 
           latitude, longitude, everything()) %>%
    arrange(sample_date)
  
  saveRDS(chem_geo_wide_all, file.path(home_path, merge_name, 
                                       paste0(merge_name, "_Samples_Flame.rds")))
  
  write.csv(chem_geo_wide_all, file.path(home_path, merge_name, 
                                         paste0(merge_name, "_Samples_Flame.csv")), 
            row.names = FALSE)
  
  chem_geo_wide <- chem_geo_wide %>% 
    left_join(flame_prep)
  
  
  site_map <- ggmap(map) +
    geom_point(aes(x = longitude, 
                   y = latitude, 
                   # shape = site_type, 
                   color = site_type), 
               data = chem_geo_wide, 
               shape = 16,
               alpha = .8) +
    geom_text_repel(aes(x = longitude, 
                        y = latitude, 
                        label = site_id, 
                        color = site_type), 
                    data = chem_geo_wide, 
                    max.overlaps = 40, 
                    size = 2, 
                    show.legend  = FALSE) +
    theme(legend.title = element_blank()) +
    theme(legend.position = c(0, .1), 
          legend.justification = c(0, 0), 
          legend.background = element_rect(fill = NA), 
          legend.text = element_text(color = "yellow"), 
          legend.key = element_blank()) +
    guides(color = guide_legend(override.aes = list(stroke = 2, alpha = 1)))
  
  print(site_map)
  
  ggsave(file.path(home_path, merge_name, "Maps_chemistry", 
                   paste0(merge_name, "sitenames.png")), 
         site_map, 
         height = 6, width = 6, units = "in", dpi = 600)
  
  
  
  flame_var_table <- data.frame(constituent = names(flame_prep), 
                                lab = "FLAMe",
                                unit = "") %>% 
    filter(!constituent %in% c("site_id", "sample_date", "latitude", "longitude"))
  
  variable_table <- variable_table %>% 
    full_join(flame_var_table)
  
  variable_table_HG <- variable_table %>%
    filter(constituent %in% c("doc", "suva254", "sulfate", "DOC2") |
             lab %in% c("FLAMe", "USGS-Hg")) %>%
    distinct()
  
  chem_geo_wide_HG <- chem_geo %>%
    filter(constituent %in% variable_table_HG$constituent) %>%
    select(sample_date, site_id, latitude, longitude, constituent, result_value) %>%
    distinct() %>% 
    group_by(sample_date, site_id, latitude, longitude, constituent) %>% 
    summarize(result_value = mean(result_value, na.rm = TRUE)) %>% 
    pivot_wider(names_from = constituent, values_from = result_value) %>%
    left_join(flame_prep)
  
  var_x = variable_table$constituent[1]
  var_y = variable_table$constituent[4]
  
  for(var_y in variable_table$constituent[]) {
    list1 <- list()
    for(var_x in variable_table$constituent[]){
      if(var_x == var_y) {next}
      # if(which(variable_table_HG$constituent == var_x) >= which(variable_table_HG$constituent == var_y)) {next}
      unit_x = variable_table %>%
        filter(constituent == var_x) %>% 
        pull(unit)
      
      unit_y = variable_table %>%
        filter(constituent == var_y) %>% 
        pull(unit)
      
      chem_geo_i <- chem_geo_wide %>%
        select(sample_date, site_id, site_type, 
               latitude, longitude, all_of(c(var_x, var_y))) %>%
        filter(!is.na(.data[[var_x]]) & !is.na(.data[[var_y]]))
      
      if(nrow(chem_geo_i) == 0) {next}
      
      p1 <- ggplot(chem_geo_i, aes(x = .data[[var_x]], y = .data[[var_y]])) +
        geom_point(size = 1, shape = 16, color = "red") +
        labs(x = paste(var_x, unit_x), 
             y = paste(var_y, unit_y)) +
        theme_bw() +
        geom_text(aes(label = site_id), size = 2) +
        theme(axis.title.y = element_blank())
      
      if(var_y == "UMHG"){
        p1 <-  p1 +
          scale_y_sqrt()
      }
      
      # print(p1)
      
      list1[[length(list1) + 1]] <- p1
      
      # ggsave(file.path(home_path, merge_name, "Scatter_chemistry", 
      #                  paste0(merge_name, "_", var_x, "_", var_y, ".png")), 
      #        p1, 
      #        height = 5, width = 5, units = "in", dpi = 300)
      
      
    }
    library(egg)
    m1 <- egg::ggarrange(plots = list1, nrow = (floor(sqrt(length(list1)))-1), left = paste(var_y, unit_y))
    
    ggsave(file.path(home_path, merge_name, "Scatter_chemistry", 
                     paste0(merge_name, "_", var_y, "_multi.png")), 
           m1, height = 10, width = 15, units = "in", dpi = 300)
    
  }
  
  var_x = variable_table_HG$constituent[1]
  var_y = variable_table_HG$constituent[4]
  
  for(var_y in variable_table_HG$constituent[]) {
    list1 <- list()
    for(var_x in variable_table_HG$constituent[]){
      if(var_x == var_y) {next}
      # if(which(variable_table_HG$constituent == var_x) >= which(variable_table_HG$constituent == var_y)) {next}
      unit_x = variable_table_HG %>%
        filter(constituent == var_x) %>% 
        pull(unit)
      
      unit_y = variable_table_HG %>%
        filter(constituent == var_y) %>% 
        pull(unit)
      
      chem_geo_i <- chem_geo_wide_HG %>%
        select(sample_date, site_id, site_type, 
               latitude, longitude, all_of(c(var_x, var_y))) %>%
        filter(!is.na(.data[[var_x]]) & !is.na(.data[[var_y]]))
      
      if(nrow(chem_geo_i) == 0) {next}
      
      p1 <- ggplot(chem_geo_i, aes(x = .data[[var_x]], y = .data[[var_y]])) +
        # geom_point(size = 1, shape = 16, color = "red") +
        # geom_smooth(aes(color = site_type, group = site_type), 
        #             method = "lm", se = FALSE) + 
        geom_point(size = 2, shape = 16, aes(color = site_type)) +
        labs(x = paste(var_x, unit_x), 
             y = paste(var_y, unit_y)) +
        theme_bw() +
        theme(legend.position = "none")
        # geom_text(aes(label = site_id), size = 2)
      
      if(var_y == "UMHG"){
        p1 <-  p1 +
          scale_y_sqrt()
      }
      
      # print(p1)
      
      list1[[length(list1) + 1]] <- p1
      
      # ggsave(file.path(home_path, merge_name, "Scatter_Hg_chemistry", 
      #                  paste0(merge_name, "_", var_x, "_", var_y, ".png")), 
      #        p1, 
      #        height = 5, width = 5, units = "in", dpi = 300)
      
      
    }
    
    if (length(list1) == 0) {next}
    
    library(egg)
    m1 <- egg::ggarrange(plots = list1, nrow = 3)
    
    ggsave(file.path(home_path, merge_name, "Scatter_Hg_chemistry", 
                     paste0(merge_name, "_", var_y, "_multi.png")), 
           m1, height = 9, width = 12, units = "in", dpi = 300)
    
  }
  
  pairs(chem_geo_wide_HG[,5:15])
  
  return(chem_geo)
  
}

