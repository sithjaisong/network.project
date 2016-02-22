#############################################################
#
# R script for data analysis as presented in Figures:
# - Fig : Boxplot of each of injury variables 
#
# files needed: 
# - data from \do.R
# 
#############################################################

boxplot_survey_data <- function(data){
  long.injury.profiles <- melt(data)
  
  #varname <- unique(as.character(as.factor(long.injury.profiles$variable)))
  
  varname1 <- c("DH", "RT",  "GM", "WH", "DP",  "FS", "NB",  "SHB", "SHR", "SR") # foliar injuries
  varname2 <- c("LF",  "WM",  "BLB",  "BLS", "BS",   "LB",  "NBS", "RS") # tiller or hill injuries
  varname3 <- c("BPH", "WPH", "RB", "GLH") # number dsu
  
  
  
  ##### boxplots of tiller injuries ####
  
  tiller.boxplot <- list()
  
  for (i in 1:length(varname1)) {
    
    tiller.boxplot[[i]] <- ggplot(long.injury.profiles %>% filter(variable == varname1[i]), aes(x = country, y = value, fill = season, color = season )) +
      geom_boxplot() + 
      stat_summary(fun.y = "mean", geom = "point", size = 1, shape = 4, position = position_dodge(width = 0.75), fill = "black") +
      theme(legend.position = "none",
            panel.background = element_blank(),
            axis.line = element_line(size = 0.5),
            axis.title.x = element_blank()) +
      scale_fill_manual(values = c("WS" = "steelblue1", "DS" = "indianred1")) + 
      scale_color_manual(values = c("WS" = "steelblue4", "DS" = "indianred4")) +
      ylab("% incidence") +
      ylim(0, 100) +
      ggtitle(paste(varname1[i]))
    
  }
  
  
  
  ##### boxplots of folier injuries ####
  
  folier.boxplot <- list()
  
  for (i in 1:length(varname2)) {
    
    folier.boxplot[[i]] <- ggplot(long.injury.profiles %>% filter(variable == varname2[i]), aes(x = country, y = value, fill = season, color = season )) +
      geom_boxplot() + 
      stat_summary(fun.y = "mean", geom = "point", size = 1, shape = 4, position = position_dodge(width = 0.75), color = "black") +
      theme(legend.position = "none") + theme(panel.background = element_blank(),
                                              axis.line = element_line(size = 0.5),
                                              axis.title.x = element_blank()) +
      ylab("% dsu") +
      scale_fill_manual(values = c("WS" = "steelblue1", "DS" = "indianred1")) + 
      scale_color_manual(values = c("WS" = "steelblue4", "DS" = "indianred4")) +
      ggtitle(paste(varname2[i]))
    
  }
  
  
  #### boxplot of insect number ####
  
  insect.boxplot <- list()
  
  for (i in 1:length(varname3)) {
    
    insect.boxplot[[i]] <- ggplot(long.injury.profiles %>% filter(variable == varname3[i]), aes(x = country, y = value, fill = season, color = season )) +
      geom_boxplot() + 
      stat_summary(fun.y = "mean", geom = "point", size = 1, shape = 4, position = position_dodge(width = 0.75), color = "black") +
      theme(legend.position = "none") + theme(panel.background = element_blank(),
                                              axis.line = element_line(size = 0.5),
                                              axis.title.x = element_blank()) +
      scale_fill_manual(values = c("WS" = "steelblue1", "DS" = "indianred1")) + 
      scale_color_manual(values = c("WS" = "steelblue4", "DS" = "indianred4")) +
      ylab("N dsu") + 
      ggtitle(paste(varname3[i]))
    
  }
  
  # combine all 
  
  all.dataset.boxplot <- marrangeGrob(c(folier.boxplot, tiller.boxplot, insect.boxplot), nrow = 3, ncol = 3)
  
  return(all.dataset.boxplot)
}

#ggsave(file = "./chapter3/results/plots/dataset_boxplot.pdf", all.dataset.boxplot, dpi = 720)
