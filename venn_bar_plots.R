library(ggplot2)

######
#venn#
######
library(readxl)

venn_data <- read_excel("Dropbox/WW_ms/WW_data/venn_data.xlsx")
venn_data <- as.data.frame(venn_data)

soil <- venn_data[venn_data['Soil'] == 1, 'Family']
water <- venn_data[venn_data['Water'] == 1, 'Family']
trap <- venn_data[venn_data['Trap'] == 1, 'Family']

library(VennDiagram)

myCol <- c("tan4", "dodgerblue", "maroon")

venn.diagram(x = list(soil, water, trap), filename = "venn.tiff", category.names = c("Soil" , "Water" , "Trap"), fill = myCol, imagetype = "tiff")

######
#eDNA#
######

ww <- read.csv("~/Desktop/WW_data/e_data.csv")
names(ww)

#for just seq data
ww<-ww[c(1:12),]

g.1 <- ggplot(ww, aes(x=Site, y=Family, colour=type)) + geom_point() + xlab("") + ylab("Family richness") + scale_color_manual(values=c("black", "red"))

g.2 <- ggplot(ww, aes(x=Site, y=OTU, colour=type)) + geom_point(size = 2.5) + xlab("") + ylab("OTU richness") + scale_color_manual(values=c("brown", "blue"))

#######
##all##
#######

g.3 <- ggplot(ww, aes(x=Site, y=Family_insect, shape=type)) + geom_point(size = 3) + xlab("") + ylab("Family richness") + scale_shape_manual(values=c(15, 16, 17))

tiff("family_all.tiff", units="in", width=8, height=5, res=300)
g.3
dev.off()

png("family_all.png", units="in", width=8, height=5, res=300)
g.3
dev.off()

######################################################
#get insect OTUs

#get list of all insect OTUs
OTU_insects <- read.delim("~/Desktop/WW_data/OTU_insects.txt", header=FALSE)
OTU_i <- OTU_insects[,1]
OTU_i <- gsub(".*: ","",OTU_i)

OTU_all <- read.delim("~/Desktop/metabarcoding/2020_04_01_022820TPmlcoi_Analysis_Pipeline/analysisfiles/eukaryote/metazoa/022820TPmlcoi-zotus.fa.OTU.txt", header=T)

OTU_final <- OTU_all[is.element(OTU_all$otu.name, OTU_i),]
write.csv(OTU_final, file="insect_OTUs.csv")






