library(ggplot2)
library(viridis)

#final plot

f7 <- ggplot(Rdata, aes(fill=Method, y=count, x=insect)) + geom_bar(position="stack", stat="identity") + scale_fill_viridis(discrete=TRUE, option="plasma")

f7 <- f7 + xlab("Insect type") + ylab("Counts") + scale_x_discrete(labels = c("Aquatic", "Semi-aquatic", "Terrestrial"))

f7 <- f7 + 
  
  theme(panel.grid.major = element_blank(),    # Removes the major grid lines
        
        panel.grid.minor = element_blank(),    # Removes the minor grid lines
        
        panel.background = element_blank(),    # Removes the background color
        
        legend.position = "right",          # Moves the legend to the right
        
        legend.justification = c(0, 1),     # Moves the legend to the top
        
        legend.text = element_text (size = 14),  # Changes the legend text size to 14-pt font
        
        legend.title = element_text (size = 16,     # Changes the legend title size to 14-pt font
                                     face = "bold"),  
        
        axis.line = element_line(colour = "black"),     # Makes the axes lines black
        
        axis.text = element_text(size = 12),    # Makes the axes values 12-pt font
        
        axis.title.x = element_text (size = 16, 
                                     face = "bold"),
        
        axis.title.y = element_text (size = 16, 
                                     face = "bold"),    # Makes the axes titles 16-pt font
        
        # The below code moves the axes titles, where the 
        # margin = unit (c(top, right, bottom, left))
        
        axis.title.y.left = element_text(margin = unit (c(0, 3, 0, 2), "mm"),
                                         angle = 90))

tiff(filename = "by-type.tiff", width = 7, height = 4, units = 'in', res = 600)
f7
dev.off()  

#test
ggplot(Rdata, aes(fill=insect, y=count, x=method)) + geom_bar(position="stack", stat="identity")

ggplot(Rdata, aes(fill=type, y=count, x=insect)) + geom_bar(position="stack", stat="identity")


#change colors
ggplot(Rdata, aes(fill=insect, y=count, x=type)) + geom_bar(position="stack", stat="identity")

ggplot(Rdata, aes(fill=type, y=count, x=insect)) + geom_bar(position="stack", stat="identity")

p<-ggplot(Rdata, aes(fill=type, y=count, x=insect, color=fill)) + geom_bar(position="stack", stat="identity")

p + (scale_color_manual(values=c("black", "orange", "yellow","green","blue","pink","purple")))
  
p + scale_fill_viridis(discrete=TRUE, option="plasma")
a<-scale_color_viridis(discrete=TRUE, option="plasma")

ggplot(Rdata, aes(fill=type, y=count, x=insect)) + geom_bar(position="stack", stat="identity")+ scale_fill_viridis(discrete=TRUE, option="plasma")

ggplot(Rdata, aes(fill=type, y=count, x=insect)) + geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("black", "orange", "yellow","green","blue","pink","purple"))


