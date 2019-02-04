# Produce SI plots for variables in HSI Model
# HSI Stat Areas 467,511,512,513,514,515

library(classInt)
library(mgcv)
library(fmsb)
library(maptools)
library(rgdal)
library(gstat)
library(automap)
library(visreg)
library(colorRamps)
library(plyr)
library(geosphere)
library(raster)
library(readr)
library(rgeos) 
library(spatstat)


options(scipen = 999)


word.tif = function(filename="Word_Figure_%03d.tif", zoom=3, width=20, height=20, pointsize=14, ...) {
  if (!grepl("[.]ti[f]+$", filename, ignore.case=TRUE))
    filename = paste0(filename,".tif")
  tiff(filename=filename, compression="lzw", res=600*zoom,
       width=width, height=height, units='cm', pointsize=pointsize, ...)
}

#font to Times
windowsFonts(Times=windowsFont("TT Times New Roman"))

setwd("C:/Users/Mike/Documents/Scallop/Data/GAM")
 



####--------------------read in things----------------
#COAST = readOGR(dsn = "C:/Users/Mike/Documents/Scallop/Data/GIS/gshhg/GSHHS_shp/i", layer = "GSHHS_i_L1")
COAST = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/GIS/US_Coastlines/US_Coast.shp")


LEK = readOGR(dsn = "C:/Users/Mike/Documents/Scallop/Data/GAM/Fisherman Info", layer = "Combined LEK")


#----DMR Data
scallop = read.csv("scallop data17DIST.csv")

#scallop = subset(scallop, !(cruise %in% c('CRNGM09','CRNGM12', 'CRNGM16')))# Exclude NGOM data
#scallop = subset(scallop, cruise %in% c('CRNGM16'))# Grab only NGOM data
#scallop = subset(scallop, cruise %in% c('CRNGM09','CRNGM12', 'CRNGM16'))# Grab only NGOM data

scallop = subset(scallop, wq < 0.04)


#COAST = readOGR(dsn = "C:/Users/Mike/Documents/Scallop/Data/GIS/gshhg/GSHHS_shp/i", layer = "GSHHS_i_L1")

 # coordinates(scallop)=~long_start+lat_start
 # wq = apply(gDistance(scallop, COAST,byid=TRUE),2,min)
 # scallop = cbind(data.frame(scallop),wq)

  






#----FED Data
#scallop = read_csv("C:/Users/Mike/Documents/Scallop/Data/Raw Data/IOOS Data.csv")

# word.tif('fed map1')
# plot(scallop$lat ~ scallop$lon, col = 'blue')
# plot(COAST, col = 'grey',  add = TRUE)
# dev.off()

#----change abundance to density--------
# as = vector()
# for (i in 1:nrow(scallop)){
#   
#   n = distm(c(scallop$lat_start[i], scallop$long_start[i]), 
#             c(scallop$lat_stop[i], scallop$long_stop[i]), 
#             fun=distHaversine)
#   as[i] = n*2.13
# }
# 
# scallop$areaswept = as
# 
# scallop = scallop[!is.na(scallop$areaswept),]
# 
# for (i in 1:nrow(scallop)){
#   a = scallop[i,18]
#   
#   m = median( as[which(as<1000 & as>300) ]  )
#   if( a > 1000){
#     a=m
#   }
#   if( a < 300){
#     a=m
#   }
#   
#   scallop[i,18]=a
# }
# 
# scallop$abundance = scallop$abundance/scallop$areaswept
# 
# scallop = scallop[,c(1:17)]



#----Load FVCOM Data----
#Build FVCOM data in separate file "LIS-FVCOM". 
#Use LIS-FVCOM-Fall for Fall Adult and Juvenile
#Use LIS-FVCOM-SPring for Spring Adult and Juvenile

#   0       -     431
#jan 1978       Dec 2013

FVCOM = read.csv("FVCOM data17full.csv")
substations = read.csv("FVCOM.StationsDIST.csv")
clip = c(511,512,513, 467) # Select statistical areas to subset FVCOM data
substations = subset(substations, area %in% clip)

substations = subset(substations, distance_offshore<0.265)
#plot(substations$latitude ~ substationss$longitude)

FVCOM = FVCOM[is.element(FVCOM$stationID, substations$id),]
plot(FVCOM$latitude ~ FVCOM$longitude)

FVCOM = FVCOM[!is.na(FVCOM$depth) & !is.na(FVCOM$sediment), ]


#space=rep(0,(nrow(FVCOM)))

#FVCOM = cbind(space,FVCOM)

#yr <- 1978:2013

#pull out temp, salinity, u, and v data from correct year
ts1978=FVCOM[,c(2:7, 8:19, 440:451, 872:883, 1304:1315)]
ts1979=FVCOM[,c(2:7, 20:31, 452:463, 884:895, 1316:1327)]
ts1980=FVCOM[,c(2:7, 32:43, 464:475, 896:907, 1328:1339)]
ts1981=FVCOM[,c(2:7, 44:55, 476:487, 908:919, 1340:1351)]
ts1982=FVCOM[,c(2:7, 56:67, 488:499, 920:931, 1352:1363)]
ts1983=FVCOM[,c(2:7, 68:79, 500:511, 932:943, 1364:1375)]
ts1984=FVCOM[,c(2:7, 80:91, 512:523, 944:955, 1376:1387)]
ts1985=FVCOM[,c(2:7, 92:103, 524:535, 956:967, 1388:1399)]
ts1986=FVCOM[,c(2:7, 104:115, 536:547, 968:979, 1400:1411)]
ts1987=FVCOM[,c(2:7, 116:127, 548:559, 980:991, 1412:1423)]
ts1988=FVCOM[,c(2:7, 128:139, 560:571, 992:1003, 1424:1435)]
ts1989=FVCOM[,c(2:7, 140:151, 572:583, 1004:1015, 1436:1447)]
ts1990=FVCOM[,c(2:7, 152:163, 584:595, 1016:1027, 1448:1459)]
ts1991=FVCOM[,c(2:7, 164:175, 596:607, 1028:1039, 1460:1471)]
ts1992=FVCOM[,c(2:7, 176:187, 608:619, 1040:1051, 1472:1483)]
ts1993=FVCOM[,c(2:7, 188:199, 620:631, 1052:1063, 1484:1495)]
ts1994=FVCOM[,c(2:7, 200:211, 632:643, 1064:1075, 1496:1507)]
ts1995=FVCOM[,c(2:7, 212:223, 644:655, 1076:1087, 1508:1519)]
ts1996=FVCOM[,c(2:7, 224:235, 656:667, 1088:1099, 1520:1531)]
ts1997=FVCOM[,c(2:7, 236:247, 668:679, 1100:1111, 1532:1543)]
ts1998=FVCOM[,c(2:7, 248:259, 680:691, 1112:1123, 1544:1555)]
ts1999=FVCOM[,c(2:7, 260:271, 692:703, 1124:1135, 1556:1567)]
ts2000=FVCOM[,c(2:7, 272:283, 704:715, 1136:1147, 1568:1579)]
ts2001=FVCOM[,c(2:7, 284:295, 716:727, 1148:1159, 1580:1591)]
ts2002=FVCOM[,c(2:7, 296:307, 728:739, 1160:1171, 1592:1603)]
ts2003=FVCOM[,c(2:7, 308:319, 740:751, 1172:1183, 1604:1615)]
ts2004=FVCOM[,c(2:7, 320:331, 752:763, 1184:1195, 1616:1627)]
ts2005=FVCOM[,c(2:7, 332:343, 764:775, 1196:1207, 1628:1639)]
ts2006=FVCOM[,c(2:7, 344:355, 776:787, 1208:1219, 1640:1651)]
ts2007=FVCOM[,c(2:7, 356:367, 788:799, 1220:1231, 1652:1663)]
ts2008=FVCOM[,c(2:7, 368:379, 800:811, 1232:1243, 1664:1675)]
ts2009=FVCOM[,c(2:7, 380:391, 812:823, 1244:1255, 1676:1687)]
ts2010=FVCOM[,c(2:7, 392:403, 824:835, 1256:1267, 1688:1699)]
ts2011=FVCOM[,c(2:7, 404:415, 836:847, 1268:1279, 1700:1711)]
ts2012=FVCOM[,c(2:7, 416:427, 848:859, 1280:1291, 1712:1723)]
ts2013=FVCOM[,c(2:7, 428:439, 860:871, 1292:1303, 1724:1735)]

#add year column
ts1978$year = 1978
ts1979$year = 1979
ts1980$year = 1980
ts1981$year = 1981
ts1982$year = 1982
ts1983$year = 1983
ts1984$year = 1984
ts1985$year = 1985
ts1986$year = 1986
ts1987$year = 1987
ts1988$year = 1988
ts1989$year = 1989
ts1990$year = 1990
ts1991$year = 1991
ts1992$year = 1992
ts1993$year = 1993
ts1994$year = 1994
ts1995$year = 1995
ts1996$year = 1996
ts1997$year = 1997
ts1998$year = 1998
ts1999$year = 1999
ts2000$year = 2000
ts2001$year = 2001
ts2002$year = 2002
ts2003$year = 2003
ts2004$year = 2004
ts2005$year = 2005
ts2006$year = 2006
ts2007$year = 2007
ts2008$year = 2008
ts2009$year = 2009
ts2010$year = 2010
ts2011$year = 2011
ts2012$year = 2012
ts2013$year = 2013


#Calculate max yearly temperature, min salinity, & mean current speed

x = list(ts1978=ts1978, ts1979=ts1979, ts1980=ts1980, ts1981=ts1981, ts1982=ts1982,
           ts1983=ts1983, ts1984=ts1984, ts1985=ts1985, ts1986=ts1986, ts1987=ts1987,
           ts1988=ts1988, ts1989=ts1989, ts1990=ts1990, ts1991=ts1991, ts1992=ts1992,
           ts1993=ts1993, ts1994=ts1994, ts1995=ts1995, ts1996=ts1996, ts1997=ts1997,
           ts1998=ts1998, ts1999=ts1999, ts2000=ts2000, ts2001=ts2001, ts2002=ts2002,
           ts2003=ts2003, ts2004=ts2004, ts2005=ts2005, ts2006=ts2006, ts2007=ts2007,
           ts2008=ts2008, ts2009=ts2009, ts2010=ts2010, ts2011=ts2011, ts2012=ts2012,ts2013=ts2013)

x = x[29:36]

#year=1978

for (i in (1:length(x))){
  
  y=x[[i]]
  
  t=y[,c(7:18)]
  s=y[,c(19:30)]
  u=as.matrix(y[,c(31:42)])
  v=as.matrix(y[,c(43:54)])
  
  cs=(sqrt((u^2) + (v^2)))
  
  mt = apply(t, 1, max)
  ms = apply(s, 1, min)
  mcs = apply(cs, 1, mean)
  
  y=cbind((y[,c(55,1:6)]),mt,ms,mcs)
  row.names(y)<-NULL
  
  names(y)[8:10]=c("temperature_b","salinity_b","Current_b")
  
  x[[i]]=y
}

t = x
s = x
c = x

for (i in 1:length(x)){
  y=x[[i]]
  
  t[[i]]=y[,c(4,5,8)]
  s[[i]]=y[,c(4,5,9)]
  c[[i]]=y[,c(4,5,10)]
}

t = join_all(t, by = c("latitude", "longitude"))
s = join_all(s, by = c("latitude", "longitude"))
c = join_all(c, by = c("latitude", "longitude"))

dt = t
ds = s
dc = c


t$tmed = apply(t[-c(1,2)], 1, median, na.rm = TRUE)
s$smed = apply(s[-c(1,2)], 1, median, na.rm = TRUE)
c$cmed = apply(c[-c(1,2)], 1, median, na.rm = TRUE)

g = x[[1]]
dg =g
tsc = cbind(g[,c(2:7)],t$tmed, s$smed, c$cmed)

names(tsc)[c(3:5,7:9)]=
  c("long_start","lat_start","surveydepth","max_temperature","min_salinity","maxFlowMag2d")

names(substations)[1:2] = c('lat_start', 'long_start')

tsc = merge(tsc,substations, by=c('lat_start', 'long_start'))

tsc = tsc[,c(1:9,16)]

names(tsc)[10] = 'wq'












#----Tw GAM---------


#--------------Full 1---------------_
# d = gam(abundance~s(surveydepth, k=5) +s(max_temperature, k=5) + s(lat_start,k=5) + s(long_start,k=5)+s(min_salinity,k=5)+
#           s(maxFlowMag2d,k=5) +s(lat_start,long_start,k=30)+s(max_temperature,min_salinity,k=30)+
#           s(surveydepth,lat_start,k=30), data=scallop,
#         family = "tw(theta = NULL, link = 'log', a=1.01, b=1.99)" )
# summary(d)
# 
# plot(d,pages=1)
# AIC(d)
# BIC(d)
# #--------------Reduced 1---------------_
# 
# d = gam(abundance~s(max_temperature, k=5) + s(lat_start,k=5) + s(long_start,k=5)+s(min_salinity,k=5)+
#           s(maxFlowMag2d,k=5) +s(lat_start,long_start,k=30)+s(max_temperature,min_salinity,k=30)+
#           s(surveydepth,lat_start,k=30), data=scallop,
#         family = "tw(theta = NULL, link = 'log', a=1.01, b=1.99)" )
# summary(d)
# 
# plot(d,pages=1)
# AIC(d)
# BIC(d)
# 
# #--------------Reduced 2-USE THIS ONE FOR k=5--------------_
# d = gam(abundance~ s(lat_start,k=5) + s(long_start,k=5)+s(min_salinity,k=5)+
#           s(maxFlowMag2d,k=5) +s(lat_start,long_start,k=30)+s(max_temperature,min_salinity,k=30)+
#           s(surveydepth,lat_start,k=30), data=scallop,
#         family = "tw(theta = NULL, link = 'log', a=1.01, b=1.99)" )
# summary(d)
# 
# plot(d,pages=1)
# AIC(d)
# BIC(d)
# #--------------Reduced 3---------------_
# 
# d = gam(abundance~ s(lat_start,k=5) + s(long_start,k=5)+
#           s(maxFlowMag2d,k=5) +s(lat_start,long_start,k=30)+s(max_temperature,min_salinity,k=30)+
#           s(surveydepth,lat_start,k=30), data=scallop,
#         family = "tw(theta = NULL, link = 'log', a=1.01, b=1.99)" )
# summary(d)
# 
# plot(d,pages=1)
# AIC(d)
# BIC(d)
# 
# #--------------Full additional penalty term---------------_
# d = gam(abundance~s(surveydepth, bs="ts") +s(max_temperature, bs="ts") + s(lat_start,long_start,bs="ts") + s(wq,bs="ts") 
#         +s(maxFlowMag2d,bs="ts"),
#         data=scallop,
#         family = "tw(theta = NULL, link = 'log', a=1.01, b=1.99)", select=T )
# summary(d)
# 
# plot(d,pages=1)
# 
# AIC(d)

#--------------Full additional penalty term plus interactions---------------_
d = gam(abundance~s(surveydepth, bs="ts") +s(max_temperature, bs="ts") + s(lat_start,bs="ts") + s(long_start,bs="ts")+s(min_salinity,bs="ts")+
          s(maxFlowMag2d,bs="ts") +s(lat_start,long_start,bs="ts")+s(max_temperature,min_salinity,bs="ts")+
          s(surveydepth,lat_start,bs="ts"), data=scallop,
        family = "tw(theta = NULL, link = 'log', a=1.01, b=1.99)", select=T )
summary(d)

#word.tif('GAM Plots Shrink')
plot(d,pages=1)
#dev.off()

#word.tif('latldepthint')
#vis.gam(d,view=c("surveydepth","lat_start"),type = 'response', n.grid=100,
#        plot.type="contour", color="cm",too.far=0.1)
#dev.off()


#AIC(d)
#BIC(d)




# #--------------Reduced---------------_
# 
# d = gam(abundance~s(surveydepth, k=4) +s(max_temperature, k=4)  + s(lat_start,long_start,k=30)+
#           +s(maxFlowMag2d,k=4),
#         data=scallop,
#         family = "tw(theta = NULL, link = 'log', a=1.01, b=1.99)")
# summary(d)
# 
# plot(d,pages=1)
# 
# AIC(d)
# 
# d = gam(abundance~s(surveydepth, k=5) +s(max_temperature, k=7) + s(lat_start) +s(long_start)  
#         ,
#         data=scallop,
#         family = "tw(theta = NULL, link = 'log', a=1.01, b=1.99)")
# summary(d)
# 
# plot(d,pages=1)
# 
# AIC(d)
# 
# d$aic



#---------------TW GAM plotting----


abu = predict.gam(d, newdata=tsc, type='response')
predabu = cbind(tsc[c('long_start', 'lat_start')],abu)


coordinates(predabu) = ~long_start + lat_start
variogram = autofitVariogram(abu ~ 1, predabu)
plot(variogram)


g = gstat(formula = abu ~ 1, model = variogram$var_model, data = predabu, maxdist = 0.1)  

xrange = range(predabu$long_start)
yrange = range(predabu$lat_start)

grid= expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = .004), 
                  latitude = seq(from = yrange[1], to = yrange[2], by = .004))
gridded(grid) = ~longitude + latitude

p = predict(g, newdata = grid)

pp = as.data.frame(p)
 

col <- colorRampPalette(c("blue", "cyan", "yellow", "red"), bias = 3)
#col = matlab.like(10)

#word.tif('Med_GAM')
spplot(p,
       col.regions = col,sp.layout = list("sp.polygons", COAST, first = FALSE, fill = "grey"),
       at = ((min(abu)*50):(max(abu)*50))/50, 
       #at = (0:160)/160,
       scales=list(draw=TRUE),
       zcol = "var1.pred", #turn this off when you want both variance and prediction map in one output
       xlim = c(-70.8, -66.8), ylim = c(43.3, 45.3))
#dev.off()

# e = extent(p)
# tr = 200
# nr = tr*1.01
# nc = as.numeric(tr*(sqrt((e[1] - e[2])^2)/sqrt((e[3] - e[4])^2)))
# 
# r <- raster(e, ncol=nc, nrow=nr)
# 
# rasabu <- rasterize(pp[, c(1,2)], r, pp[,3], fun=mean)
# 
# writeRaster(rasabu, "scallop pred", format = "ascii")

#---------------all years plotting----


# 
# #all stat areas combined
# 
# allyr = cbind(g[,c(2:7)],t,s,c)
# 
# yr = 1
# years=matrix(NA, ncol=8, nrow=nrow(ts2004))
# for ( i in 1:length(x)){
#   
#   h = allyr[,c(1:6, i+8, i+19, i+30)]
#   
#   
#   names(h)[names(h)=='depth'] <- 'surveydepth'
#   names(h)[names(h)=='temperature_b'] <- 'max_temperature'
#   names(h)[names(h)=='latitude'] <- 'lat_start'
#   names(h)[names(h)=='longitude'] <- 'long_start'
#   names(h)[names(h)=='salinity_b'] <- 'min_salinity'
#   names(h)[names(h)=='Current_b'] <- 'maxFlowMag2d'
#   abu = predict.gam(d, newdata=h, type='response')
#   years[,i] = abu
#   
# }
# 
# years = data.frame(years)
# years = cbind(h[,c(3,4)],years)


#---------------Delta Plot----
# years = data.frame(dg[,4:6])
# 
# for (i in 3:length(dt)){
#   k=i+1
#   t = dt[[i]]
#   s = ds[[i]]
#   c = dc[[i]]
#   
#   tsc = cbind(years[,1:3],t,s,c)
#   names(tsc)[1:6]=
#     c("long_start","lat_start","surveydepth","max_temperature","min_salinity","maxFlowMag2d")
#   
#   abu = predict.gam(d, newdata=tsc, type='response')
#  years[[k]] = abu
#   
# }
# years = years[,c(1:2,4:11)]
# 
# #betaf is a function to calc temporal change
# 
# betaf = function(vec){
#   
#   beta = lm(vec ~ seq(1:c(ncol(years)-2)))$coef[2]
#   p = summary(lm(vec ~ seq(1:c(ncol(years)-2))))$ coefficienyears [2,4]
#   return(beta) # slope
#   #   return(p) # p-value
#   
# }
# 
# res = data.frame(r = apply(years[, 3:ncol(years)], 1, betaf))
# res$r[res$r > 15] = 15;res$r[res$r < -15] = 15
# 
# 
# 
# map = cbind(years[,1:2], res)
# colnames(map)[3] = "diff"
# 
# coordinates(map) = ~longitude + latitude
# variogram = autofitVariogram(diff ~ 1, map)
# plot(variogram)
# 
# 
# g = gstat(formula = diff ~ 1, model = variogram$var_model, data = map, maxdist = 0.1)  
# 
# xrange = range(map$longitude)
# yrange = range(map$latitude)
# 
# grid= expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = .004), 
#                   latitude = seq(from = yrange[1], to = yrange[2], by = .004))
# gridded(grid) = ~longitude + latitude
# 
# p = predict(g, newdata = grid)
# 
# 
# 
# 
# col <- colorRampPalette(c("blue", "white", "red"), bias=1)
# 
# 
# word.tif('deltaGAM')
# spplot(p,
#        col.regions = col,sp.layout = list("sp.polygons", COAST, first = FALSE, fill = "grey"),
#        #at = ((min(map$diff)*50):(max(map$diff)*50))/50, 
#        at = -15:15,
#        scales=list(draw=TRUE),
#        zcol = "var1.pred", #turn this off when you want both variance and prediction map in one output
#        xlim = c(-70.8, -66.8), ylim = c(43.3, 45.3))
# dev.off()

















#------------- model validation using LEK---- 


# LEK <- LEK[LEK@data$Z_surv!=0|LEK@data$Z_score_su!=0,]
# LEK$ZZZ = LEK@data$Z_surv+LEK@data$Z_score_su  
# #LEK$ZZZ = LEK@data$Z_combo+LEK@data$Z_score_co 

LEK <- LEK[LEK@data$Intvw_scor!=0,]

projection(LEK, asText=TRUE)
projection(predabu) <- projection(LEK, asText=TRUE)
projection(predabu, asText=TRUE)

join = over(LEK, predabu[,"abu"], FN= mean)

f = cbind(data.frame(LEK),join)

f = f[!is.na(f$abu),]

f$Zbin = f$Intvw_scor

f$Zbin = ifelse(f$Zbin > 0, "H", "L")

f$predbin = f$abu

f$predbin = ifelse(f$predbin > 90, "H", "L")

classIntervals(f$abu, n=2, style="fisher")
classIntervals(f$abu, n=2, style="jenks")

f$sumER = ifelse(f$predbin == f$Zbin, 0, 1)

1-(sum(f$sumER)/nrow(f))


#plot(f$abu~f$Intvw_scor)
#abline(h=54,col='red', lty = 3, lwd=3)
#abline(v=0,col='red', lty = 3, lwd=3)


##write an anova to get statistical differences between high and low 

#aggregate(f$abu~f$Zbin, FUN=mean)

#summary(aov(abu ~ Zbin, f))



#H = subset(f, Zbin=="H")
#L = subset(f, Zbin=="L")

#dd = data.frame(Den=c('H','L'), M =c(mean(H$abu), mean(L$abu)), SE = c(sd(H$abu)/sqrt(length(H$abu)), sd(L$abu)/sqrt(length(L$abu)) )     )

#word.tif('red2bp')
# ggplot(dd, aes(x=Den, y=M, fill=Den, stat="identity"))+
#   theme_bw()+
#   theme(text=element_text(family="Times", size=18))+
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
#   geom_bar(position = "dodge", stat="identity", color="black", show_guide=FALSE)+
#   scale_y_continuous(limits=c(0, 110),expand = c(0,0))+
#   geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=0.2, position=position_dodge(width=0.9))+
#   scale_fill_manual(values=c("white", "gray32", "white","gray32"))
# #dev.off()  











#word.tif('boxplot reduced method')
#boxplot(abu ~ Zbin, data=f, outline=TRUE)
#dev.off()
  

#   
# f$var1.pred = scale(f$var1.pred, center = TRUE, scale = TRUE)
# 
# SSE = sum((f$var1.pred - f$ZZZ)^2)
# SSE

#plot(f$var1.pred ~ f$ZZZ)





modelPred = as.character(f$predbin);LEKScore = f$Zbin

MK = data.frame(cbind(modelPred,LEKScore))

a = length(which(MK$modelPred == "L" & MK$LEKScore == "L"))
b = length(which(MK$modelPred == "H" & MK$LEKScore == "L"))
c = length(which(MK$modelPred == "L" & MK$LEKScore == "H"))
d = length(which(MK$modelPred == "H" & MK$LEKScore == "H"))

Sensitivity = d/(c+d)
Specificity = a/(a+b)
Accuracy = a+d/(a+b+c+d)

#-----------------------------Cross Validation---------------------------

it = 100
difs = matrix(NA, nrow = it, ncol = 4)
colnames(difs) = c('INT', 'slope', 'rsquared', 'SSE')
sslist = list()
pb <- txtProgressBar(min = 0, max = it, style = 3)
for (i in 1:it){
  
  split=sample(1:(nrow(scallop)), size = nrow(scallop)*0.2)
  test = scallop[split,]
  train = scallop[-split,]
  
  d1 = gam(abundance~s(max_temperature, k=5) + s(lat_start,k=5) + s(long_start,k=5)+s(min_salinity,k=5)+
            s(maxFlowMag2d,k=5) +s(lat_start,long_start,k=30)+s(max_temperature,min_salinity,k=30)+
            s(surveydepth,lat_start,k=30), data=scallop,
          family = "tw(theta = NULL, link = 'log', a=1.01, b=1.99)" )
  summary(d1)
  
  
  

  
  
    test$abu1 = predict.gam(d1, newdata=test, type='response')
  
  
    ss = lm(abundance ~ abu1, data=test)
    SSE = 1
    
    sslist[[i]] = ss
    names(sslist)[i] = paste0("ss_",i)
    difs[i,] = c(coef(ss)[1], coef(ss)[2], summary(lm(ss))$r.squared, SSE )

  
 
  
  sslist[[i]] = ss
  names(sslist)[i] = paste0("ss_",i)
  difs[i,] = c(coef(ss)[1], coef(ss)[2], summary(lm(ss))$r.squared, SSE )
  setTxtProgressBar(pb, i)
}
close(pb)

apply(difs, 2, FUN = median) 

#word.tif('red1CV')
plot(NULL, xlim = c(0,1000), ylim = c(0,1000), main = "s(La) + s(Lo) + s(Ds) + s(De) + s(Bt) + s(Bs) + s(Fv) ",
     xlab = expression(bold("Predicted")),
     ylab = expression(bold("Observed")), xaxt="n", yaxt="n")

for (i in 1:length(sslist)){
  x = sslist[[i]]
  abline(x, col='gray', lwd=2)
  
}

abline(0,1,col='red', lty = 3, lwd=5)
#dev.off()

write.csv(difs, "Red2")




















#---------------Model evaluation----

# it = 10
# difs = matrix(NA, nrow = it, ncol = 4)
# colnames(difs) = c('INT', 'slope', 'rsquared', 'SSE')
# sslist = list()
# for (i in 1:it){
# 
#   split=sample(1:(nrow(scallop)), size = nrow(scallop)*0.2)
#   test = scallop[split,]
#   train = scallop[-split,]
# 
# 
#   d1 = gam(abundance~s(max_temperature, k=7) + s(lat_start,k=7) + s(long_start,k=7)+s(min_salinity,k=7)+
#             s(maxFlowMag2d,k=7) +s(lat_start,long_start,k=30)+s(max_temperature,min_salinity,k=30)+
#             s(surveydepth,lat_start,k=30), data=scallop,
#           family = "tw(theta = NULL, link = 'log', a=1.01, b=1.99)" )
#   summary(d1)
#   
# 
#   
# 
#   
# 
#  
#   
#   
#   
#   abu1 = predict.gam(d1, newdata=tsc, type='response')
#   predabu1 = cbind(tsc[c('long_start', 'lat_start')],abu1)
#   
#   
#   coordinates(predabu1) = ~long_start + lat_start
#   variogram = autofitVariogram(abu1 ~ 1, predabu1)
#   plot(variogram)
#   
#   
#   g1 = gstat(formula = abu1 ~ 1, model = variogram$var_model, data = predabu1, maxdist = 0.1)  
#   
#   xrange = range(predabu1$long_start)
#   yrange = range(predabu1$lat_start)
#   
#   grid= expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = .004), 
#                     latitude = seq(from = yrange[1], to = yrange[2], by = .004))
#   gridded(grid) = ~longitude + latitude
#   
#   p = predict(g1, newdata = grid)
#   
#   
# #match train to ABU
#   coordinates(test)=~long_start+lat_start
#   x <- as.ppp(test)
#   p <- as.ppp(p)
#   
#   xp <- nncross(x,p)$which  
#   
#   x <- data.frame(x)
#   p <- data.frame(p)
#   
#   p <-cbind((1:nrow(p)),p)
#   names(p)[1] <- "IDpc"
#   
#   test <- cbind(x,xp)
#   names(test)[21] <-"IDpc"
#   
#   test <- merge(test, p, by = "IDpc")
#   
#   #plot(test$abundance ~ test$var1.pred)
# 
#   SSE = sum((log(test$abundance) - log(test$var1.pred))^2)
#   
# 
#  ss = lm(abundance ~ var1.pred, data=test)
#   
#  sslist[[i]] = ss
#  names(sslist)[i] = paste0("ss_",i)
#   difs[i,] = c(coef(ss)[1], coef(ss)[2], summary(lm(ss))$r.squared, SSE )
# }
# 
# apply(difs, 2, FUN = median) 
# 
# #word.tif('red1CV')
# 
# plot(NULL, xlim = c(0,1000), ylim = c(0,1000), main = "s(La) + s(Lo) + s(Ds) + s(De) + s(Bt) + s(Bs) + s(Fv) ",
#      xlab = expression(bold("Predicted")),
#      ylab = expression(bold("Observed")), xaxt="n", yaxt="n")
# 
# for (i in 1:length(sslist)){
#   x = sslist[[i]]
#   abline(x, col='gray', lwd=2)
#   
# }
# 
# abline(0,1,col='red', lty = 3, lwd=5)

#dev.off()
  #pull out line, int, slope, 
 #c(coef(q)[1], coef(q)[2], summary(lm(q))$r.squared, AIC(q))

  #--------------END----------------------------

  # specify = c('max_temperature','min_salinity','maxFlowMag2d', 'depth' 
  #             ** add lat long)
  # 
  # 
  # s = paste('s','(', specify[***],'k=',')', sep = '' )
  
  
  # for (k in 1:length(specify)){



