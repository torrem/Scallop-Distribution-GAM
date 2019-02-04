# Map the abundance of juvenile and adult scallops

library(sp)
library(ggplot2)
library(maptools)
library(colorRamps)
library(gridExtra)
library(plyr)
library(RColorBrewer)

setwd("C:/Users/Mike/Documents/Scallop/Data/GAM")


word.tif = function(filename="Word_Figure_%03d.tif", zoom=3, width=20, height=20, pointsize=30, ...) {
  if (!grepl("[.]ti[f]+$", filename, ignore.case=TRUE))
    filename = paste0(filename,".tif")
  tiff(filename=filename, compression="lzw", res=600*zoom,
       width=width, height=height, units='cm', pointsize=pointsize, ...)
}

#font to Times
windowsFonts(Times=windowsFont("TT Times New Roman"))



#-----------------------Format tow/abundance data-----------------------------------------------

scallop = read.csv("scallop data17DIST.csv")


scallop$lat_start <- as.numeric(as.character(scallop$lat_start))
scallop$long_start <- as.numeric(as.character(scallop$long_start))

#make lon-lat the coords 
coordinates(scallop)=~long_start+lat_start
scallop <-data.frame(scallop)


#add in a column that transforms abundance.. log10
scallop$logABU <- log(scallop$abundance)


scallop$logABU <- replace(scallop$logABU, scallop$logABU==-Inf, 0)


#read in shapefiles
COAST = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/GIS/US_Coastlines/US_Coast.shp")
#COAST = readOGR(dsn = "C:/Users/Mike/Documents/Scallop/Data/GIS/gshhg/GSHHS_shp/i", layer = "GSHHS_i_L1")

  






##--------------abundance map!--------------
##SPPLOT version?
#grid.arrange(W, Sp, Su, Fa, ncol=2)
coordinates(scallop)=~long_start+lat_start


col = colorRampPalette(brewer.pal(5, "Purples")[-(1:1)])(20)

#scallop abundance map
word.tif('Scal ABU')
spplot(scallop,"logABU",
       sp.layout = list("sp.polygons", COAST, first = FALSE, fill = "grey", lty = 0),
       col.regions = col,
       scales=list(draw=TRUE), 
       cex=.7*(1:5),
       at = (0:5)/5,
       xlim = c(-70.5, -66.9), ylim = c(43.2, 45.6))
dev.off()

word.tif('Scal ABUout')
spplot(scallop,"logABU",
       sp.layout = list("sp.polygons", COAST, first = FALSE, fill = "grey", lty = 1),
       col.regions = col,
       scales=list(draw=TRUE), 
       cex=0*(1:2),
       at = (0:2)/2,
       xlim = c(-73, -66), ylim = c(40, 48))
dev.off()

crop = c(-72, -66, 39.7, 46)

#scallop abundance map & zones

word.tif('Scal ABU Zone')

spplot(scallop,"logABU",
       sp.layout = list(list("sp.polygons", COAST, first = FALSE, fill = "grey"),
                        list(z1, fill="blue", first=TRUE),
                        list(z2, fill="yellow", first=TRUE),
                        list(z3, fill="purple", first=TRUE),
                        list(NGOM, fill="light blue", first=TRUE)),
       col.regions = colorRampPalette(brewer.pal(7, "PuBu")[-(1:1)])(20),
       scales=list(draw=TRUE), 
       cex=.6*(1:5),
       at = (0:5)/5,
       xlim = c(-71.2, -66.6), ylim = c(41.2, 45.6))


dev.off()

spplot(COAST, fill = 'grey', zcol = "ID", colorkey=F,
       sp.layout = list(list(z1, fill="blue", first=FALSE),
                        list(z2, fill="yellow", first=FALSE),
                        list(z3, fill="purple", first=FALSE),
                        list(NGOM, fill="light blue", first=FALSE)),
       scales=list(draw=T),
       xlim=c(-71.2, -66.6), ylim=c(41.2, 45.6))



#-------------------------------End------------------------------------------------------

#---------------------------------PLOT Abundance Data


# Adult <- ggplot(COAST, aes(x = long, y = lat)) + geom_polygon(aes(group = group), fill="white", colour="black")+
#   theme_bw(base_size = 24)+
#   theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
#   theme(axis.title.y = element_blank(),axis.text.y = element_blank())+ 
#   theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
#         line = element_blank(), panel.background = element_blank())+
#   geom_point(data=scallop, aes(x=long_in, y=lat_in, colour=logadultABU), pch=19, size=3)+
#   coord_cartesian(xlim=c(-71.2, -66.6), ylim=c(41.2, 45.6))+
#   scale_colour_gradientn(colours=matlab.like2(10))+
#   labs(colour = "Log\nAbundance")+
#   labs(title="Scallops > 80mm Shell Height")  
# 
# Juvenile <- ggplot(COAST, aes(x = long, y = lat)) + geom_polygon(aes(group = group), fill="white", colour="black")+
#   theme_bw(base_size = 24)+
#   theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
#   theme(axis.title.y = element_blank(),axis.text.y = element_blank())+ 
#   theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
#         line = element_blank(), panel.background = element_blank())+
#   geom_point(data=scallop, aes(x=long_in, y=lat_in, colour=logjuvABU), pch=19, size=3)+
#   coord_cartesian(xlim=c(-71.2, -66.6), ylim=c(41.2, 45.6))+
#   scale_colour_gradientn(colours=matlab.like2(10))+
#   labs(colour = "Log\nAbundance")+
#   labs(title="Scallops < 80mm Shell Height")  
# 
# 
# 










# #look at sampling dist among months & years
# count(all_tow, vars = "year") #count number of observations in each year
# count(all_tow, vars = "month")
# 
# #Subset by month
# w_adult <- adult_tow[adult_tow$month %in% c(12, 1, 2),]
# sp_adult <- adult_tow[adult_tow$month %in% c(3, 4, 5),]
# su_adult <- adult_tow[adult_tow$month %in% c(6, 7),]
# f_adult <- adult_tow[adult_tow$month %in% c(10, 11),]
# 
# w_juvenile <- juvenile_tow[juvenile_tow$month %in% c(12, 1, 2),]
# sp_juvenile <- juvenile_tow[juvenile_tow$month %in% c(3, 4, 5),]
# su_juvenile <- juvenile_tow[juvenile_tow$month %in% c(6, 7),]
# f_juvenile <- juvenile_tow[juvenile_tow$month %in% c(10, 11),]

