
# var = "CDOM_C6P" 
# rho = 0.01
# alpha = 0.006
# temp = 25

TurnerTempTurbCorrect <- function(correctdata, 
                                  var = NULL, 
                                  rho = NULL,
                                  alpha = NULL, 
                                  temp = 25){
  
  correctdata_out <- correctdata
  
  if(is.null(var)){stop("No variable indicated")}
  
  string <- c(var, "Temp_C6P", "Turb_C6P")
  new_cols <- paste0(var, c("_wt", "_turb"))
  wt <- turb <- rep(NA, nrow(correctdata_out))
  
  #Check if columns are in data.frame
  if(any(string %in% names(correctdata))){
    cat("\n", "Applying temperature and/or turibidy corrections to", var, "\n", 
        "These are site- and sensor-specific.")
    
    if(is.null(rho)){
      cat("\n", "No temperature coefficient provided.", "\n")
      
    } else if (!is.null(rho)){
      wt <- pull(correctdata, var) / (1 + (rho * (-1) * (correctdata$Temp_C6P - temp)))
    }
    
    if(is.null(alpha)){
      cat("\n", "No turbidity coefficient provided", "\n")
    } else if(!is.null(alpha) & !is.null(rho)){
      
      turb <- wt / exp(alpha * (-1) * correctdata$Turb_C6P)
      
    } 
    
    newdata <- data.frame(wt, turb)
    names(newdata) <- new_cols
    
    correctdata_out <- correctdata_out %>%
      bind_cols(newdata)
    
    library(ggplot2)
    library(egg)
    
    p1 <- ggplot(correctdata_out) + 
      geom_abline() + 
      theme_bw() + 
      geom_point(aes_string(x = var, y = new_cols[1], color = "Temp_C6P"),
                 size = 2) +
      scale_color_distiller(palette = "RdYlBu") +
      theme(legend.position = c(0.05,.95), 
            legend.justification = c(0,1), 
            legend.background = element_rect(fill = NA)) +
      labs(x = paste("Raw", var), 
           y = paste("Temperature-corrected", var), 
           color = expression(paste("Temperature ", degree~C))) +
      ggtitle(paste(expression("Standard temp:", temp, " ", degree~C)))
    
    p2 <- ggplot(correctdata_out) + 
      geom_abline() + 
      theme_bw() + 
      geom_point(aes_string(x = new_cols[1], y = new_cols[2], color = "Turb_C6P"),
                 size = 2) +
      scale_color_distiller(palette = "YlOrBr", direction = (1)) +
      theme(legend.position = c(0.05,.95), 
            legend.justification = c(0,1), 
            legend.background = element_rect(fill = NA)) +
      labs(y = paste("Temperature-Turbidity corrected", var),
           x = paste("Temperature-corrected", var), 
           color = expression(paste("Turbidity (NTU)")))
    
    print(egg::ggarrange(plots = list(p1, p2), ncol = 1))
    
    
  } 
  
  return(correctdata_out)
}