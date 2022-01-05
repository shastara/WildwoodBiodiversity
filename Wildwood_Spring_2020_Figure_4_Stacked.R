

# This code clears the Global Environment

rm(list=ls())



# This code makes a table of the dataset longitudinally per site.


library(readxl)

Stacked <- read_excel("~/Documents/01 Research/00 Publications/Wildwood Emergence/Wildwood_Spring_2020_R_Datasets_1_5_2022.xlsx", 
                                                       sheet = "Figure 5")

Stacked



Stacked$Week2 <- factor(Stacked$Week, labels = c("1", "2", "3", "4", "5", "6"))



# This code makes a STACKED BAR GRAPH: 

# TIME vs. various measures of RELATIVE ABUNDANCE

library (ggplot2)


# Adds the x-axis and changes the scale

Fig.5a <- ggplot(data = Stacked, aes (fill = Order, y = Abundance, x = Week2)) +
  
  geom_bar (position = "fill", stat = "identity")


Fig.5a  



# Changes the Color of the Bars

Fig.5a <- Fig.5a +
  
  scale_fill_manual (values = c("darkorange", "darkblue", "darkolivegreen4", 
                                "darkred", "yellow3", "maroon3", "darkslategrey",
                                "darkorchid4", "deeppink4", "black"))

Fig.5a



# Changes the name of the x-axis labels

Fig.5a <- Fig.5a +
  
  scale_x_discrete (breaks = c("1", "2", "3", "4", "5", "6"),
                    labels = c("Week 1", "Week 2", "Week 3", "Week 4", 
                    "Sediment Reads", "Water Reads")) +
  
  ylab ("Relative Abundance")


Fig.5a



# Makes the figure pretty

Fig.5a <- Fig.5a + 
  
  theme(panel.grid.major = element_blank(),    # Removes the major grid lines
        
        panel.grid.minor = element_blank(),    # Removes the minor grid lines
        
        panel.background = element_blank(),    # Removes the background color
        
        legend.position = "right",          # Moves the legend to the right
        
        legend.justification = c(0, 1),     # Moves the legend to the top
        
        legend.text = element_text (size = 14),  # Changes the legend text size to 14-pt font
        
        legend.title = element_text (size = 16,     # Changes the legend title size to 14-pt font
                                     face = "bold"),  
        
        axis.line = element_line(colour = "black"),     # Makes the axes lines black
        
        axis.text = element_text(size = 14),    # Makes the axes values 14-pt font
        
        axis.title.x = element_blank(),    # Remove x-axis title
        
        axis.title.y = element_text (size = 16, 
                                face = "bold"),    # Makes the axes titles 16-pt font
        
        # The below code moves the axes titles, where the 
        # margin = unit (c(top, right, bottom, left))
        
        axis.title.y.left = element_text(margin = unit (c(0, 3, 0, 2), "mm"),
                                         angle = 90))
        
Fig.5a



# Adds "A" text to y-axis

library (grid)

text_A <- textGrob ("A", 
                     gp = gpar (fontsize = 24,
                                fontface = "bold"))


Fig.5t <- Fig.5a + 
  
  coord_cartesian (clip = "off") +
  
  annotation_custom(text_A, xmin = -0.05, xmax = -0.05, ymin = 1.03, ymax = 1.03)

Fig.5t


Fig.5a <- Fig.5t




# This code makes a STACKED BAR GRAPH: 

# TIME vs. various measures of RELATIVE RICHNESS

library (ggplot2)


# Adds the x-axis and changes the scale

Fig.5b <- ggplot(data = Stacked, aes (fill = Order, y = Richness, x = Week2)) +
  
  geom_bar (position = "fill", stat = "identity")


Fig.5b  



# Changes the Color of the Bars

Fig.5b <- Fig.5b +
  
  scale_fill_manual (values = c("darkorange", "darkblue", "darkolivegreen4", 
                                "darkred", "yellow3", "maroon3", "darkslategrey",
                                "darkorchid4", "deeppink4", "black"))

Fig.5b



# Changes the name of the x-axis labels

Fig.5b <- Fig.5b +
  
  scale_x_discrete (breaks = c("1", "2", "3", "4", "5", "6"),
                    labels = c("Week 1", "Week 2", "Week 3", "Week 4", 
                               "Sediment eDNA", "Water eDNA")) +
  
  ylab ("Relative Richness")


Fig.5b



# Makes the figure pretty

Fig.5b <- Fig.5b + 
  
  theme(panel.grid.major = element_blank(),    # Removes the major grid lines
        
        panel.grid.minor = element_blank(),    # Removes the minor grid lines
        
        panel.background = element_blank(),    # Removes the background color
        
        legend.position = "none",          # No legend 
        
        axis.line = element_line(colour = "black"),     # Makes the axes lines black
        
        axis.text = element_text(size = 14),    # Makes the axes values 14-pt font
        
        axis.title.x = element_blank(),    # Remove x-axis title
        
        axis.title.y = element_text (size = 16, 
                                     face = "bold"),    # Makes the axes titles 16-pt font
        
        # The below code moves the axes titles, where the 
        # margin = unit (c(top, right, bottom, left))
        
        axis.title.y.left = element_text(margin = unit (c(0, 3, 0, 2), "mm"),
                                         angle = 90))

Fig.5b



# Adds "A" text to y-axis

library (grid)

text_A <- textGrob ("B", 
                    gp = gpar (fontsize = 24,
                               fontface = "bold"))


Fig.5t2 <- Fig.5b + 
  
  coord_cartesian (clip = "off") +
  
  annotation_custom(text_A, xmin = -0.05, xmax = -0.05, ymin = 1.03, ymax = 1.03)

Fig.5t2


Fig.5b <- Fig.5t2





# This code creates an A and B panel in one figure

library (patchwork)

Fig.5 <-Fig.5a + Fig.5b +
  
  plot_layout (ncol = 1)

Fig.5



ggsave("Figure_4 - Composite.pdf", 
       path = "~/Documents/01 Research/00 Publications/Wildwood Emergence/Figures", 
       plot = last_plot(),
       device='pdf', 
       dpi=300)




