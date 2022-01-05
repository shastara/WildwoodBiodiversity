

# This code clears the Global Environment

rm(list=ls())


# This code makes a table of the dataset over time.

Emergence <- data.frame (
  Weeks = c(1, 2, 3, 4,
            1, 2, 3, 4,
            1, 2, 3, 4,
            1, 2, 3, 4,
            1, 2, 3, 4,
            1, 2, 3, 4,
            1, 2, 3, 4,
            1, 2, 3, 4,
            1, 2, 3, 4,
            1, 2, 3, 4,
            1, 2, 3, 4,
            1, 2, 3, 4),
   Abundance = c(0, 12, 2, 10,
                0, 29, 13, 20,
                1, 5, 12, 69,
                1, 8, 7, 32,
                1, 12, 6, 15,
                0, 11, 5, 18,
                3, 9, 7, 33,
                4, 25, 0, 20,
                4, 17, 5, 41,
                2, 22, 3, 27,
                5, 10, 18, 14,
                1, 15, 10, 18),
  Richness = c(1, 3, 2, 6,
               2, 1, 1, 3,
               4, 3, 5, 12,
               1, 2, 3, 3,
               1, 4, 4, 5,
               0, 3, 3, 6,
               2, 4, 5, 6,
               2, 4, 0, 5,
               1, 4, 2, 7,
               2, 4, 3, 10,
               4, 2, 10, 10,
               1, 1, 3, 5),
  Diversity = c(0.000, 0.721, 0.693, 1.696,
                0.000, 0.000, 0.000, 0.518,
                0.000, 0.950, 1.234, 1.435,
                0.000, 0.377, 0.956, 0.657,
                0.000, 0.837, 1.330, 1.229,
                0.000, 0.600, 0.950, 1.303,
                0.637, 1.215, 1.475, 1.362,
                0.562, 0.499, 0.000, 1.235,
                0.000, 0.790, 0.673, 1.234,
                0.693, 0.548, 1.099, 1.781,
                1.332, 0.325, 2.120, 2.243,
                0.000, 0.000, 0.639, 0.961),
  Biomass = c(0.00, 0.50, 0.80, 21.52,
               0.00, 0.85, 0.69, 9.83,
               1.15, 1.80, 7.18, 27.53,
               11.16, 0.87, 1.80, 3.87,
               1.65, 5.39, 2.07, 2.04,
               0.00, 0.87, 14.17, 29.71,
               9.47, 9.60, 15.35, 35.74,
               1.13, 22.59,	0.00, 5.67,
               0.08, 6.72, 8.11, 113.40,
               0.93, 0.75, 1.39, 11.57,
               2.75, 0.38, 73.89, 77.42,
               0.19, 0.45, 1.12, 8.77))

Emergence



# This code makes a table of the dataset longitudinally per site.

library(readxl)

Complete <- read_excel("Documents/01 Research/00 Publications/Wildwood Emergence/Wildwood_Spring_2020_Complete_Dataset_11_10_20.xlsx", 
                                                             sheet = "Week 1 Analysis")

Complete



# Assumptions to Run a Linear Regression
    # 1) The relationship between x and y is linear
    # 2) The samples were collected independently
    # 3) Homoscedasticity = variances are equal
    # 4) Normal distribution



# This code tests for a normal distribution
# p > 0.05 = normal distribution AND no transformation is needed.
# p < 0.05 = not a normal distribution AND transformation is needed OR
            # run a non-parametric test.

shapiro.test(Complete$Abundance)    # p = 0.07

shapiro.test(Complete$Richness)     # p = 0.03

shapiro.test(Complete$Diversity)    # p < 0.001

shapiro.test(Complete$Biomass)      # p < 0.001

shapiro.test(Complete$Cadmium)      # p = 0.22

shapiro.test(Complete$Chromium)     # p = 0.01

shapiro.test(Complete$Copper)       # p = 0.03

shapiro.test(Complete$Lead)         # p = 0.004

shapiro.test(Complete$Mercury)      # p < 0.001

shapiro.test(Complete$Manganese)    # p = 0.01



# This code transforms the non-normal variables and re-test normality.

Complete$LogRichness <- log (Complete$Richness +1)
shapiro.test(Complete$LogRichness)     # p = 0.13


Complete$LogDiversity <- log (Complete$Diversity + 1)
shapiro.test(Complete$LogDiversity)    # p < 0.001 (worse)


Complete$SQRTDiversity <- sqrt (Complete$Diversity + 1)
shapiro.test(Complete$SQRTDiversity)    # p < 0.001 (worse still)
# might do a t-test for diversity to compare no diversity to some diversity


Complete$LogBiomass <- log (Complete$Biomass + 1)
shapiro.test(Complete$LogBiomass)      # p = 0.03


Complete$SQRTBiomass <- sqrt (Complete$Biomass + 1)
shapiro.test(Complete$SQRTBiomass)    # p = 0.03
# Consider removing the 11 g outlier


Complete$LogChromium <- log (Complete$Chromium)
shapiro.test(Complete$LogChromium)     # p = 0.14


Complete$LogCopper <- log (Complete$Copper)
shapiro.test(Complete$LogCopper)       # p = 0.08


Complete$LogLead <- log (Complete$Lead)
shapiro.test(Complete$LogLead)         # p = 0.06


Complete$LogMercury <- log (Complete$Mercury)
shapiro.test(Complete$LogMercury)      # p = 0.03 (better)


Complete$LogManganese <- log (Complete$Manganese)
shapiro.test(Complete$LogManganese)    # p = 0.01


# We can use abundance, richness, biomass (as a t-test), chromium, copper, lead
# Adjusted R^2 accounts for the number of variables.

FullModel <- lm(Abundance ~ LogChromium + LogCopper + LogLead + LogMercury + LogManganese,
                data = Complete)

summary (FullModel)



FullModel2 <- lm(Richness ~ LogChromium + LogCopper + LogLead + LogMercury + LogManganese,
                data = Complete)

summary (FullModel2)



FullModel3 <- lm(Abundance ~ LogChromium + LogCopper + LogLead,
                 data = Complete)

summary (FullModel3)



FullModel4 <- lm(Richness ~ LogChromium + LogCopper + LogLead,
                 data = Complete)

summary (FullModel4)





Model1 <- lm(Abundance ~ LogCopper, data = Complete)

summary (Model1)



Model2 <- lm(LogRichness ~ LogCopper, data = Complete)

summary (Model2)





# This code makes a linear scatter plot: Copper vs. Abundance

library (ggplot2)

g1 <- ggplot(data = Complete, aes (x = LogCopper, y = Abundance)) +
  
  geom_point() +
  
  geom_smooth(method = lm, 
              formula = y ~ x, 
              se = FALSE, 
              fullrange = TRUE) +
  
  xlab ("Concentration of Copper (log ppm)") +
  ylab ("Emerging Insect Abundance") +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

g1




# This code makes a linear scatter plot: Copper vs. Abundance

library (ggplot2)

g2 <- ggplot(data = Complete, aes (x = LogCopper, y = LogRichness)) +
  
  geom_point() +
  
  geom_smooth(method = lm, 
              formula = y ~ x, 
              se = FALSE, 
              fullrange = TRUE) +
  
  xlab ("Concentration of Copper (log ppm)") +
  ylab ("Emerging Insect Richness (log)") +
  
  scale_x_continuous(breaks = seq(3.3, 3.9, 0.1),
                     limits = c(3.3, 3.9)) +
  
  scale_y_continuous(breaks = seq(0, 2, 0.5),
                     limits = c(0, 2)) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

g2



# This code creates an A and B panel in one figure

library (patchwork)

g1 + g2



# This code makes a linear scatter plot with secondary axis: 
# Copper vs. Abundance & Richness

library (ggplot2)

ggplot(data = Complete, aes(x = LogCopper)) + 
  
  geom_point(aes (y = Abundance, 
                  colour = "Abundance")) +
  
  xlab("Concentration of Copper (log ppm)") +
  
  ylab("Emerging Insect Abundance") +
  
  geom_point(aes (y = LogRichness, 
                  colour = "LogRichness")) +
  
  scale_y_continuous(sec.axis = sec_axis( ~. , 
                            name = "Emerging Insect Richnes (log)")) +
  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        axis.title.y.right = element_text(angle = 90))



# This code makes a linear scatter plot with secondary axis: 
# Weeks vs. Richness & Biomass

library (ggplot2)

Fig.3 <- ggplot(data = Complete, aes (x = LogCopper))

Fig.3 <- Fig.3 + 
  
  geom_point(aes (y = Abundance, colour = "Abundance"))

Fig.3 <- Fig.3 + 
  
  geom_point(aes (y = LogRichness, colour = "Richness"))

Fig.3 <- Fig.3 + 
  
  scale_y_continuous(sec.axis = sec_axis(~., name = "Emerging Insect Richness (log)"),
                     breaks = seq(0, 5, 1),
                     limits = c(0, 5))

Fig.3 <- Fig.3 + scale_colour_manual(values = c("blue", "red"))

Fig.3 <- Fig.3 +
  labs(y = "Emerging Insect Abundance",
       x = "Concentration of Copper (log ppm)",
       colour = "Parameter")

Fig.3 <- Fig.3 + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

Fig.3



