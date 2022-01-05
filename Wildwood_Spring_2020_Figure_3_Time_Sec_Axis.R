


# This code clears the Global Environment

rm(list=ls())



# This code imports the data.

library(readxl)

Emergence <- read_excel("~/Documents/01 Research/00 Publications/Wildwood Emergence/Wildwood_Spring_2020_R_Datasets_11_25_20.xlsx", 
                                                       sheet = "Figure 3")

Emergence



# This code makes a MULTIPLE LINEAR SCATTER PLOT: 

# WEEKS vs. ABUNDANCE, RICHNESS, and DIVERSITY

library (ggplot2)


# Adds the x-axis

Fig.3a <- ggplot(data = Emergence, aes (x = Weeks)) +
  
  scale_y_continuous(breaks = seq(0, 5, 0.5),
                     limits = c(0, 5))

Fig.3a   


# Adds the Abundance y-axis and regression line

Fig.3a <- Fig.3a + 
  
  geom_point(aes (y = LogAbundance, 
                  shape = 18,
                  size = 1,
                  colour = "Abundance (Dotted)")) +
  
  scale_shape_identity() +
  
  scale_size (guide = "none") +
  
  geom_smooth(aes(y = LogAbundance), 
              method = "lm", 
              formula = y ~ x,
              se = FALSE,
              fullrange = FALSE,
              size = 1,
              col = "black",
              linetype = "dotted")

Fig.3a   



# Adds the Richness y-axis and regression line

Fig.3a <- Fig.3a + 
  
  geom_point(aes (y = LogRichness, 
                  shape = 2,
                  size = 1,
                  colour = "Richness (Solid)")) +
  
  geom_smooth(aes(y = LogRichness), 
              method = "lm", 
              formula = y ~ x,
              se = FALSE,
              fullrange = FALSE,
              size = 1,
              col = "black",
              linetype = "longdash")

Fig.3a   



# Adds the Diversity y-axis and regression line

Fig.3a <- Fig.3a + 
  
  geom_point(aes (y = LogDiversity, 
                  shape = 16,
                  size = 1,
                  colour = "Diversity (Dash)")) +   
  
  geom_smooth(aes(y = LogDiversity),  
              method = "lm", 
              formula = y ~ x,
              se = FALSE,
              fullrange = FALSE,
              size = 1,
              col = "black",
              linetype = "solid")

Fig.3a 



# Changes the colors of the dots to their respective variable

Fig.3a <- Fig.3a + 
  
  scale_colour_manual(values = c("black", "black", "black"),
                      guide = guide_legend (override.aes = list (shape = c(18, 2, 16),
                                                                 size = 3, 3, 3)))

Fig.3a 



# Adds the legend and renames the right y-axis and x-axis

Fig.3a <- Fig.3a +
  
  labs(y = "Logarithmic Scale",
       x = "Week",
       colour = "Measure")

Fig.3a   



# Makes the figure pretty

Fig.3a <- Fig.3a + 
  
  theme(panel.grid.major = element_blank(),    # Removes the major grid lines
        
        panel.grid.minor = element_blank(),    # Removes the minor grid lines
        
        panel.background = element_blank(),    # Removes the background color
        
        legend.position = c(0.15, 0.85),       # Moves the legend to the top right
        
        legend.text = element_text (size = 12),  # Changes the legend text size to 14-pt font
        
        legend.title = element_text (size = 14,     # Changes the legend title size to 14-pt font
                                     face = "bold"),  
        
        axis.line = element_line(colour = "black"),     # Makes the axes lines black
        
        axis.text = element_text(size = 14),    # Makes the axes values 16-pt font
        
        axis.title.y = element_text(size = 16,
                                    colour = "black",
                                    face = "bold"),    # Makes the axes titles 20-pt font
        
        axis.title =element_text(size = 16, 
                                 face = "bold"),    # Makes the axes titles 20-pt font
        
        
        # The below code moves the axes titles, where the 
        # margin = unit (c(top, right, bottom, left))
        
        axis.title.y.left = element_text(margin = unit (c(0, 3, 0, 2), "mm"),
                                         angle = 90),
        
        axis.title.x = element_text(margin = unit (c(3, 0, 2, 0), "mm")))

Fig.3a





ggsave("Figure_3 - Time.pdf", 
       path = "~/Documents/01 Research/00 Publications/Wildwood Emergence/Figures", 
       plot = last_plot(),
       device='pdf',
       dpi=300)



# Assumptions to Run a Pearson Correlation Analysis
# 1) x and y are correlated
# 2) The samples were collected independently
# 4) Normal distribution



# This code tests for a normal distribution
# p > 0.05 = normal distribution AND no transformation is needed.
# p < 0.05 = not a normal distribution AND transformation is needed OR
# run a non-parametric test.

shapiro.test(Emergence$Abundance)    # p < 0.001 - NOT NORMAL

shapiro.test(Emergence$Richness)     # p < 0.001 - NOT NORMAL

shapiro.test(Emergence$Diversity)    # p = 0.023 - NOT NORMAL

shapiro.test(Emergence$Biomass)      # p < 0.001 - NOT NORMAL



# This code transforms the non-normal variables and re-test normality.

Emergence$LogAbundance <- log (Emergence$Abundance + 1)

shapiro.test(Emergence$LogAbundance)     # p = 0.305 - NORMAL


Emergence$LogRichness <- log (Emergence$Richness + 1)

shapiro.test(Emergence$LogRichness)     # p = 0.181 - NORMAL


Emergence$LogDiversity <- log (Emergence$Diversity + 1)

shapiro.test(Emergence$LogDiversity)    # p = 0.004 - NOT NORMAL


Emergence$LogBiomass <- log (Emergence$Biomass + 1)

shapiro.test(Emergence$LogBiomass)      # p = 0.006 - NOT NORMAL



# The following code performs the Pearson or Spearman rank correlations


# Weeks vs. Abundance, Richness, Diversity, and Biomass

cor.test(Emergence$Weeks, Emergence$LogAbundance, method = c("pearson"), exact=FALSE) 
# p < 0.001, r = 0.699


cor.test(Emergence$Weeks, Emergence$LogRichness, method = c("pearson"), exact=FALSE)
# p < 0.001, r = 0.668


cor.test(Emergence$Weeks, Emergence$Diversity, method = c("spearman"), exact=FALSE) 
# p < 0.001, rho = 0.669


cor.test(Emergence$Weeks, Emergence$Biomass, method = c("spearman"), exact=FALSE) 
# p < 0.001, rho = 0.591



















































# This code makes a MULTIPLE LINEAR SCATTER PLOT: 

# WEEKS vs. ABUNDANCE AND BIOMASS

library (ggplot2)


# Adds the x-axis

Fig.3a <- ggplot(data = Emergence, aes (x = Weeks)) +
  
  scale_y_continuous(breaks = seq(0, 5, 0.5),
                     limits = c(0, 5))

Fig.3a   


# Adds the Abundance y-axis and regression line

Fig.3a <- Fig.3a + 
  
  geom_point(aes (y = LogAbundance, 
                  shape = 16,
                  colour = "Abundance")) +
  
  scale_shape_identity() +
  
  geom_smooth(aes(y = LogAbundance), 
              method = "lm", 
              formula = y ~ x,
              se = FALSE,
              fullrange = FALSE,
              size = 0.5,
              col = "black",
              linetype = "solid")

Fig.3a   



# Adds the Biomass y-axis and regression line

Fig.3a <- Fig.3a + 
  
  geom_point(aes (y = LogBiomass, 
                  shape = 1,
                  colour = "Biomass")) +   
  
  geom_smooth(aes(y = LogBiomass),  
              method = "lm", 
              formula = y ~ x,
              se = FALSE,
              fullrange = FALSE,
              size = 0.5,
              col = "black",
              linetype = "longdash")

Fig.3a 



# Changes the colors of the dots to their respective variable

Fig.3a <- Fig.3a + 
  
  scale_colour_manual(values = c("black", "black"),
                      guide = guide_legend (override.aes = list (shape = c(16, 1))))

Fig.3a 



# Adds the legend and renames the right y-axis and x-axis

Fig.3a <- Fig.3a +
  
  labs(y = "Logarithmic Scale",
       x = "Week",
       colour = "Measure")

Fig.3a   


# Makes the figure pretty

Fig.3a <- Fig.3a + 
  
  theme(panel.grid.major = element_blank(),    # Removes the major grid lines
        
        panel.grid.minor = element_blank(),    # Removes the minor grid lines
        
        panel.background = element_blank(),    # Removes the background color
        
        legend.position = c(0.15, 0.85),       # Moves the legend to the top right
        
        legend.text = element_text (size = 12),  # Changes the legend text size to 14-pt font
        
        legend.title = element_text (size = 14,     # Changes the legend title size to 14-pt font
                                     face = "bold"),  
        
        axis.line = element_line(colour = "black"),     # Makes the axes lines black
        
        axis.text=element_text(size = 14),    # Makes the axes values 16-pt font
        
        axis.title=element_text(size = 16, 
                                face = "bold"),    # Makes the axes titles 20-pt font
        
        
        # The below code moves the axes titles, where the 
        # margin = unit (c(top, right, bottom, left))
        
        axis.title.y.left = element_text(margin = unit (c(0, 3, 0, 2), "mm"),
                                         angle = 90),
        
        axis.title.x = element_text(margin = unit (c(3, 0, 2, 0), "mm")))

Fig.3a




# Adds "A" text to y-axis

library (grid)

text_A <- textGrob ("A", 
                    gp = gpar (fontsize = 24,
                               fontface = "bold"))


Fig.3t <- Fig.3a + 
  
  coord_cartesian (clip = "off") +
  
  annotation_custom(text_A, xmin = 0.6, xmax = 0.6, ymin = 5.1, ymax = 5.1)

Fig.3t


Fig.3a <- Fig.3t





# This code makes a MULTIPLE LINEAR SCATTER PLOT: 

# WEEKS vs RICHNESS AND DIVERSITY

library (ggplot2)


# Adds the x-axis

Fig.3b <- ggplot(data = Emergence, aes (x = Weeks)) +
  
  scale_y_continuous(breaks = seq(0, 3, 0.5),
                     limits = c(0, 3))

Fig.3b   


# Adds the Abundance y-axis and regression line

Fig.3b <- Fig.3b + 
  
  geom_point(aes (y = LogRichness, 
                  shape = 16,
                  colour = "Richness")) +
  
  scale_shape_identity() +
  
  geom_smooth(aes(y = LogRichness), 
              method = "lm", 
              formula = y ~ x,
              se = FALSE,
              fullrange = FALSE,
              size = 0.5,
              col = "black",
              linetype = "solid")

Fig.3b   



# Adds the Biomass y-axis and regression line

Fig.3b <- Fig.3b + 
  
  geom_point(aes (y = LogDiversity, 
                  shape = 1,
                  colour = "Diversity")) +   
  
  geom_smooth(aes(y = LogDiversity),  
              method = "lm", 
              formula = y ~ x,
              se = FALSE,
              fullrange = FALSE,
              size = 0.5,
              col = "black",
              linetype = "longdash")

Fig.3b 



# Changes the colors of the dots to their respective variable

Fig.3b <- Fig.3b + 
  
  scale_colour_manual(values = c("black", "black"),
                      guide = guide_legend (override.aes = list (shape = c(16, 1))))

Fig.3b 



# Adds the legend and renames the right y-axis and x-axis

Fig.3b <- Fig.3b +
  
  labs(y = "Logarithmic Scale",
       x = "Week",
       colour = "Measure")

Fig.3b   


# Makes the figure pretty

Fig.3b <- Fig.3b + 
  
  theme(panel.grid.major = element_blank(),    # Removes the major grid lines
        
        panel.grid.minor = element_blank(),    # Removes the minor grid lines
        
        panel.background = element_blank(),    # Removes the background color
        
        legend.position = c(0.15, 0.85),       # Moves the legend to the top right
        
        legend.text = element_text (size = 12),  # Changes the legend text size to 14-pt font
        
        legend.title = element_text (size = 14,     # Changes the legend title size to 14-pt font
                                     face = "bold"),  
        
        axis.line = element_line(colour = "black"),     # Makes the axes lines black
        
        axis.text = element_text(size = 14),    # Makes the axes values 16-pt font
        
        axis.title.y = element_text(size = 16,
                                    colour = "white",
                                    face = "bold"),    # Makes the axes titles 20-pt font
        
        axis.title =element_text(size = 16, 
                                 face = "bold"),    # Makes the axes titles 20-pt font
        
        
        # The below code moves the axes titles, where the 
        # margin = unit (c(top, right, bottom, left))
        
        axis.title.x = element_text(margin = unit (c(3, 0, 2, 0), "mm")))

Fig.3b




# Adds "B" text to y-axis

library (grid)

text_B <- textGrob ("B", 
                    gp = gpar (fontsize = 24,
                               fontface = "bold"))


Fig.3t2 <- Fig.3b + 
  
  coord_cartesian (clip = "off") +
  
  annotation_custom(text_B, xmin = 0.6, xmax = 0.6, ymin = 3.1, ymax = 3.1)

Fig.3t2


Fig.3b <- Fig.3t2




# This code creates an A and B panel in one figure

library (patchwork)

Fig.3 <-Fig.3a + Fig.3b

Fig.3























# This code makes a LINEAR SCATTER PLOT WITH SECONDARY AXIS: 

# WEEKS vs. ABUNDANCE & RICHNESS

library (ggplot2)


# Adds the x-axis

Fig.3a <- ggplot(data = Emergence, aes (x = Weeks))

Fig.3a   


# Adds the Abundance y-axis and regression line

Fig.3a <- Fig.3a + 
  
  geom_point(aes (y = LogAbundance, colour = "Abundance")) +
  
  geom_smooth(aes(y = LogAbundance), 
              method = "lm", 
              formula = y ~ x,
              se = FALSE,
              fullrange = FALSE,
              size = 0.5,
              col = "black",
              linetype = "solid")

Fig.3a   


# Adds the Richness y-axis and regression line

Fig.3a <- Fig.3a + 
  
  geom_point(aes (y = LogBiomass, colour = "Biomass")) +   
  
  geom_smooth(aes(y = LogBiomass),  
              method = "lm", 
              formula = y ~ x,
              se = FALSE,
              fullrange = FALSE,
              size = 0.5,
              col = "black",
              linetype = "longdash")

Fig.3a   


# Adds the Biomass as a secondary y-axis (because Biomass was the last axis added)

Fig.3a <- Fig.3a + 
  
  scale_y_continuous(sec.axis = sec_axis(~., name = "Emerging Insect Biomass (log)"),
                     breaks = seq(0, 5, 0.5),
                     limits = c(0, 5))

Fig.3a   


# Changes the dots to their respective variable

Fig.3a <- Fig.3a + 
  
  scale_colour_manual(values = c("black", "black"),
                      guide = guide_legend (override.aes = list (shape = c(16, 17))))

Fig.3a  


# Adds the legend and renames the right y-axis and x-axis

Fig.3a <- Fig.3a +
  labs(y = "Emerging Insect Abundance",
       x = "# of Weeks",
       colour = "Parameter")

Fig.3a   


# Makes the figure pretty

Fig.3a <- Fig.3a + 
  
  theme(panel.grid.major = element_blank(),    # Removes the major grid lines
        
        panel.grid.minor = element_blank(),    # Removes the minor grid lines
        
        panel.background = element_blank(),    # Removes the background color
        
        legend.position = c(0.15, 0.85),       # Moves the legend to the top right
        
        axis.line = element_line(colour = "black"),     # Makes the axes lines black
        
        axis.text=element_text(size = 12),    # Makes the axes values 12-pt font
        
        axis.title=element_text(size = 14, 
                                face = "bold"),    # Makes the axes titles 14-pt font
        
        
        # The below code moves the axes titles, where the 
        # margin = unit (c(top, right, bottom, left))
        
        axis.title.y.right = element_text(margin = unit (c(0, 2, 0, 3), "mm"),
                                          angle = 90),
        
        axis.title.y.left = element_text(margin = unit (c(0, 3, 0, 2), "mm"),
                                         angle = 90),
        
        axis.title.x = element_text(margin = unit (c(3, 0, 2, 0), "mm")))

Fig.3a


# Adds regression and p-value text to the plot

Fig.3a <- Fig.3a + 
  
  annotate (geom = "text", 
            x = 3.5, y = 24, 
            label = "R^2 == 0.435",
            parse = TRUE,
            color = "darkred") +
  
  annotate (geom = "text", 
            x = 3.49, y = 22, 
            label = "p < 0.001",
            color = "darkred") +
  
  annotate (geom = "text", 
            x = 3.5, y = 9, 
            label = "R^2 == 0.375",
            parse = TRUE,
            color = "darkgreen") +
  
  annotate (geom = "text", 
            x = 3.49, y = 7, 
            label = "p < 0.001",
            color = "darkgreen") +
  
  annotate (geom = "text", 
            x = 1, y = 70, 
            label = "A",
            parse = TRUE,
            color = "black",
            size = 10,
            fontface = "bold") +
  
  annotate(geom = 'segment', y = Inf, yend = Inf, 
           color = 'black', 
           x = -Inf, xend = Inf, 
           size = 1)

Fig.3a





# This code makes a LINEAR SCATTER PLOT WITH SECONDARY AXIS: 

# WEEKS vs. DIVERSITY & BIOMASS

library (ggplot2)


# Adds the x-axis

Fig.3b <- ggplot(data = Emergence, aes (x = Weeks))

Fig.3b   


# Adds the Diversity y-axis and regression line

Fig.3b <- Fig.3b + 
  
  geom_point(aes (y = Diversity, colour = "Diversity")) +
  
  geom_smooth(aes(y = Diversity), 
              method = "lm", 
              formula = y ~ x,
              se = FALSE,
              fullrange = FALSE,
              col = "darkred")

Fig.3b   


# Adds the Biomass y-axis and regression line

Fig.3b <- Fig.3b + 
  
  geom_point(aes (y = LogBiomass, colour = "Biomass")) +   
  
  geom_smooth(aes(y = LogBiomass),  
              method = "lm", 
              formula = y ~ x,
              se = FALSE,
              fullrange = FALSE,
              col = "darkgreen")

Fig.3b   


# Adds the Biomass as a secondary y-axis (because Biomass was the last axis added)

Fig.3b <- Fig.3b + 
  
  scale_y_continuous(sec.axis = sec_axis(~., name = "Emerging Insect Biomass (log)"),
                     breaks = seq(0, 6, 0.5),
                     limits = c(0, 6))

Fig.3b   


# Changes the colors of the dots to their respective variable

Fig.3b <- Fig.3b + scale_colour_manual(values = c("darkred", "darkgreen"))

Fig.3b  


# Adds the legend and renames the right y-axis and x-axis

Fig.3b <- Fig.3b +
  labs(y = "Emerging Insect Diversity",
       x = "# of Weeks",
       colour = "Parameter")

Fig.3b   


# Makes the figure pretty

Fig.3b <- Fig.3b + 
  
  theme(panel.grid.major = element_blank(),    # Removes the major grid lines
        
        panel.grid.minor = element_blank(),    # Removes the minor grid lines
        
        panel.background = element_blank(),    # Removes the background color
        
        legend.position = c(0.15, 0.85),       # Moves the legend to the top right
        
        axis.line = element_line(colour = "black"),     # Makes the axes lines black
        
        axis.text=element_text(size = 12),    # Makes the axes values 12-pt font
        
        axis.title=element_text(size = 14, 
                                face = "bold"),    # Makes the axes titles 14-pt font
        
        
        # The below code moves the axes titles, where the 
        # margin = unit (c(top, right, bottom, left))
        
        axis.title.y.right = element_text(margin = unit (c(0, 2, 0, 3), "mm"),
                                          angle = 90),
        
        axis.title.y.left = element_text(margin = unit (c(0, 3, 0, 2), "mm"),
                                         angle = 90),
        
        axis.title.x = element_text(margin = unit (c(3, 0, 2, 0), "mm")))

Fig.3b


# Adds regression and p-value text to the plot

Fig.3b <- Fig.3b + 
  
  annotate (geom = "text", 
            x = 3.5, y = 2.7, 
            label = "R^2 == 0.426",
            parse = TRUE,
            color = "darkgreen") +
  
  annotate (geom = "text", 
            x = 3.49, y = 2.5, 
            label = "p < 0.001",
            color = "darkgreen") +
  
  annotate (geom = "text", 
            x = 3.5, y = 1.5, 
            label = "R^2 == 0.338",
            parse = TRUE,
            color = "darkred") +
  
  annotate (geom = "text", 
            x = 3.49, y = 1.3, 
            label = "p < 0.001",
            color = "darkred") +
  
  annotate (geom = "text", 
            x = 1, y = 6, 
            label = "B",
            parse = TRUE,
            color = "black",
            size = 10,
            fontface = "bold") +
  
  annotate(geom = 'segment', y = Inf, yend = Inf, 
           color = 'black', 
           x = -Inf, xend = Inf, 
           size = 1)

Fig.3b




# This code creates an A and B panel in one figure

library (patchwork)

Fig.3 <-Fig.3a + Fig.3b

Fig.3















# This code makes a linear scatter plot: Weeks vs. Abundance

library (ggplot2)

g1 <- ggplot(data = Emergence, aes (x = Weeks, y = LogAbundance)) +
  
  geom_point() +
  
  geom_smooth(method = lm, 
              formula = y ~ x, 
              se = FALSE, 
              fullrange = TRUE) +
  
  xlab ("# of Weeks") +
  ylab ("Emerging Insect Abundance (log)") +
  
  scale_y_continuous(breaks = seq(0, 5, 0.5),
                     limits = c(0, 5)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

g1



# This code runs a linear regression: Weeks vs. Abundance

Regress1 <- lm(LogAbundance ~ Weeks, data = Emergence)

summary (Regress1)




# This code makes a scatter plot: Weeks vs. Richness

library (ggplot2)

g2 <- ggplot(data = Emergence, aes (x = Weeks, y = LogRichness)) +
  
  geom_point() +
  
  geom_smooth(method = lm, 
              formula = y ~ x, 
              se = FALSE, 
              fullrange = TRUE) +
  
  xlab ("# of Weeks") +
  ylab ("Emerging Family Richness (log)") +
  
  scale_y_continuous(breaks = seq(0, 3, 0.5),
                     limits = c(0, 3)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

g2


# this code runs a linear regression

Regression2 <- lm(LogRichness ~ Weeks, data = Emergence)

summary (Regression2)




# This code makes a scatter plot: Weeks vs. Diversity

library (ggplot2)

g3 <- ggplot(data = Emergence, aes (x = Weeks, y = LogDiversity)) +
  
  geom_point() +
  
  geom_smooth(method = lm, 
              formula = y ~ x, 
              se = FALSE, 
              fullrange = TRUE) +
  
  xlab ("# of Weeks") +
  ylab ("Emerging Family Diversity (log)") +
  
  scale_y_continuous(breaks = seq(0, 2.5, 0.5),
                     limits = c(0, 2.5)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

g3


# this code runs a linear regression

Regression3 <- lm(LogDiversity ~ Weeks, data = Emergence)

summary (Regression3)




# This code makes a scatter plot: Weeks vs. Biomass

library (ggplot2)

g4 <- ggplot(data = Emergence, aes (x = Weeks, y = LogBiomass)) +
  
  geom_point() +
  
  geom_smooth(method = lm, 
              formula = y ~ x, 
              se = FALSE, 
              fullrange = TRUE) +
  
  xlab ("# of Weeks") +
  ylab ("Emerging Family Biomass (log g)") +
  
  scale_y_continuous(breaks = seq(0, 5, 0.5),
                     limits = c(0, 5)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14, face="bold"))

g4


# this code runs a linear regression

Regression4 <- lm(LogBiomass ~ Weeks, data = Emergence)

summary (Regression4)




# This code creates an A and B panel in one figure

library (patchwork)

g1 + g2 + g3 + g4
