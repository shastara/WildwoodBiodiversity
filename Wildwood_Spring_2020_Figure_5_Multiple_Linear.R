

# This code clears the Global Environment

rm(list=ls())



# This code makes a table of the dataset longitudinally per site.

library(readxl)

Complete <- read_excel("~/Documents/01 Research/00 Publications/Wildwood Emergence/Wildwood_Spring_2020_R_Datasets_11_25_20.xlsx", 
                          sheet = "Figure 4 and 6")


Complete




# This code makes a LINEAR SCATTER PLOT WITH SECONDARY AXIS: 

# SITE vs. various measures of RICHNESS

library (ggplot2)



# Adds the x-axis and changes the scale

Fig.4 <- ggplot(data = Complete, aes (x = Site2)) +  
  
  scale_x_continuous(breaks = seq(1, 12, 1),
                     limits = c(1, 12))

Fig.4   



# Adds the TRAP FAMILY RICHNESS y-axis and regression line

Fig.4 <- Fig.4 + 
  
  geom_point(aes (y = TTotRich, 
                  shape = 16,
                  size = 1,
                  colour = "Emergence Trap (Solid)")) +
  
  scale_shape_identity() +
  
  scale_size (guide = "none") +
  
  geom_smooth(aes(y = TTotRich), 
              method = "lm", 
              formula = y ~ x,
              se = FALSE,
              fullrange = FALSE,
              size = 1,
              col = "black",
              linetype = "solid")

Fig.4   



# Changes the scale of the y-axis.

Fig.4 <- Fig.4 + 
  
  scale_y_continuous( breaks = seq(0, 40, 5),
                      limits = c(0, 40))

Fig.4 




# Adds the WATER eDNA RICHNESS y-axis and regression line

Fig.4 <- Fig.4 + 
  
  geom_point(aes (y = WTotRich,
                  shape = 2,
                  size = 1,
                  colour = "Water eDNA (Dash)"))  +
  
  geom_smooth(aes(y = WTotRich),  
              method = "lm", 
              formula = y ~ x,
              se = FALSE,
              fullrange = FALSE,
              size = 1,
              col = "black",
              linetype = "longdash")

Fig.4   



# Adds the SOIL eDNA RICHNESS y-axis and regression line

Fig.4 <- Fig.4 + 
  
  geom_point(aes (y = STotRich,
                  shape = 18,
                  size = 1,
                  colour = "Sediment eDNA (Dotted)")) +
  
  geom_smooth(aes(y = STotRich),  
              method = "lm", 
              formula = y ~ x,
              se = FALSE,
              fullrange = FALSE,
              size = 1,
              col = "black",
              linetype = "dotted")

Fig.4 



# Changes the colors of the dots to their respective variable

Fig.4 <- Fig.4 + 
  
  scale_colour_manual(values = c("black", "black", "black", "black", "black"),
                      guide = guide_legend (override.aes = list (shape = c(16, 2, 18),
                                                                 size = 3, 3, 3)))

Fig.4 



# Adds the legend and renames the right y-axis and x-axis

Fig.4 <- Fig.4 +
  labs(y = "Family Richness",
       x = "Site",
       colour = "Collection Method")

Fig.4   


# Makes the figure pretty

Fig.4 <- Fig.4 + 
  
  theme(panel.grid.major = element_blank(),    # Removes the major grid lines
        
        panel.grid.minor = element_blank(),    # Removes the minor grid lines
        
        panel.background = element_blank(),    # Removes the background color
        
        legend.position = "right",          # Moves the legend to the right
        
        legend.justification = c(0, 1),     # Moves the legend to the top
        
        legend.text = element_text (size = 12),  # Changes the legend text size to 14-pt font
        
        legend.title = element_text (size = 14,     # Changes the legend title size to 14-pt font
                                     face = "bold"), 
        
        axis.line = element_line(colour = "black"),     # Makes the axes lines black
        
        axis.text=element_text(size = 12),    # Makes the axes values 12-pt font
        
        axis.title=element_text(size = 14, 
                                face = "bold"),    # Makes the axes titles 14-pt font
        
        
        # The below code moves the axes titles, where the 
        # margin = unit (c(top, right, bottom, left))
        
        axis.title.y.left = element_text(margin = unit (c(0, 3, 0, 2), "mm"),
                                         angle = 90),
        
        axis.title.x = element_text(margin = unit (c(3, 0, 2, 0), "mm")))

Fig.4




# Adds "Upstream" and "Downstream" text to x-axis

library (grid)

text_ds <- textGrob ("Downstream", 
                     gp = gpar (fontsize = 12))

text_us <- textGrob ("Upstream", 
                     gp = gpar (fontsize = 12))

Fig.4t <- Fig.4 + 
  
  coord_cartesian (clip = "off") +
  
  annotation_custom(text_us, xmin = 1, xmax = 1, ymin = -4.5, ymax = -6.5) + 
  
  annotation_custom(text_ds, xmin = 12, xmax = 12, ymin = -4.5, ymax = -6.5)

Fig.4t


ggsave("Figure_5.pdf", 
       path = "~/Documents/01 Research/00 Publications/Wildwood Emergence/Figures", 
       plot = last_plot(),
       device='pdf', 
       dpi=1200)











# This code clears the Global Environment

rm(list=ls())



# This code makes a table of the dataset longitudinally per site.

library(readxl)

Complete <- read_excel("~/Documents/01 Research/00 Publications/Wildwood Emergence/Wildwood_Spring_2020_R_Datasets_11_25_20.xlsx", 
                       sheet = "Figure 4 and 6")
Complete



# Assumptions to Run a Pearson Correlation
# 1) The relationship between x and y is linear
# 2) The samples were collected independently
# 3) Normal distribution



# This code tests for a normal distribution
# p > 0.05 = normal distribution AND no transformation is needed.
# p < 0.05 = not a normal distribution AND transformation is needed OR
# run a non-parametric test.

shapiro.test(Complete$TTotRich)    # p = 0.304 - GOOD - Pearson

shapiro.test(Complete$STotRich)     # p = 0.166 - GOOD - Pearson

shapiro.test(Complete$WTotRich)    # p = 0.022 - NOT GOOD


Complete$LogWTotRich <- log (Complete$WTotRich)

shapiro.test(Complete$LogWTotRich)    # p = 0.003 - NOT GOOD - Spearman




# Site vs. Trap Richness, Soil Richness, and Water Richness


cor.test(Complete$TTotRich, Complete$Site2, method = c("pearson"), exact=FALSE) 
# p = 0.098, r = 0.500


cor.test(Complete$STotRich, Complete$Site2, method = c("pearson"), exact=FALSE) 
# p = 0.724, r = -0.186


cor.test(Complete$LogWTotRich, Complete$Site2, method = c("spearman"), exact=FALSE)
# p = 0.036, r = -0.841







