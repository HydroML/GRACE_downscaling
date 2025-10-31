
# process JPL GRACE data. downloaded from https://podaac.jpl.nasa.gov/dataset/TELLUS_GRAC-GRFO_MASCON_CRI_GRID_RL06.3_V4 

rm(list = ls())
library(ncdf4)
library(ncdf4.helpers)
library(lubridate)
library(infotheo)
library(sf)
library(sp)
library(jtools)



currentWeatherdata<-nc_open(paste0("C:/Users/joeja/Desktop/research_postdoc/GRACE_DOWNSCALE/DATA/GRCTellus.JPL.200204_202508.GLO.RL06.3M.MSCNv04CRI.nc"))
londata<-ncvar_get(currentWeatherdata,varid = "lon_bounds")
latdata<-ncvar_get(currentWeatherdata,varid = "lat_bounds")
timedata<-ncvar_get(currentWeatherdata,varid = "time_bounds")

timedata<-ncvar_get(currentWeatherdata,varid = "time_bounds")

lwe<-ncvar_get(currentWeatherdata,varid = "lwe_thickness")
uncertainty<-ncvar_get(currentWeatherdata,varid = "uncertainty")
scale_fac<-ncvar_get(currentWeatherdata,varid = "scale_factor")
land_mask<-ncvar_get(currentWeatherdata,varid = "land_mask")
nc_close(currentWeatherdata)

library(lubridate)
numdays<-timedata[2,]-timedata[1,]
monthvec<-rep(NA,ncol(timedata))
yearvec<-rep(NA,ncol(timedata))
for(i in 1:ncol(timedata)){
  cur_seq<-seq(from=as.Date("2002-01-01"),by="day",length.out=colMeans(timedata)[i])
  monthvec[i]<-month(cur_seq[length(cur_seq)])
  yearvec[i]<-year(cur_seq[length(cur_seq)])
}


londata[londata>180]<- londata[londata>180] - 360


whichlon<-which((londata[2,]>= -130 & londata[2,]<= -105) | (londata[1,]>= -130 & londata[1,]<= -105))
whichlat<-which((latdata[2,]>= 25 & latdata[2,]<= 50) | (latdata[1,]>= 25 & latdata[1,]<= 50))
lonvec<-londata[1,whichlon]
latvec<-latdata[1,whichlat]
posinfo<-expand.grid(latvec,lonvec)

image(1:length(lonvec),1:length(latvec), (land_mask[whichlon,whichlat]), col = terrain.colors(60), axes = FALSE)


colnames(posinfo)<-c("min_lat","min_lon")
posinfo$max_lat<-posinfo$min_lat+0.5
posinfo$max_lon<-posinfo$min_lon+0.5
posinfo$lwe<-NA
posinfo$uncertainty<-NA
posinfo$scale_fac<-NA
posinfo$month<-NA
posinfo$year<-NA
posinfo$numdays<-NA
GRACE_ALL<-NULL

for(t in 1:ncol(timedata)){
  
  for(j in 1:nrow(posinfo)){
    curlonpos<-which(londata[1,]==posinfo$min_lon[j])
    curlatpos<-which(latdata[1,]==posinfo$min_lat[j])
    
    posinfo$lwe[j]<-lwe[curlonpos,curlatpos,t]
    posinfo$uncertainty[j]<-uncertainty[curlonpos,curlatpos,t]
    posinfo$scale_fac[j]<-scale_fac[curlonpos,curlatpos]
    posinfo$month[j]<-monthvec[t]
    posinfo$year[j]<-yearvec[t]
    posinfo$numdays[j]<-numdays[t]
  }
  GRACE_ALL<-rbind(GRACE_ALL,posinfo)
  write.csv(posinfo,paste0("C:/Users/joeja/Desktop/research_postdoc/GRACE_DOWNSCALE/DATA/JPL_MONTHLY/GRACE_JPL_month_",t,".csv"),row.names = F)
  print(t)
}

write.csv(GRACE_ALL,paste0("C:/Users/joeja/Desktop/research_postdoc/GRACE_DOWNSCALE/DATA/GRACE_JPL_all.csv"),row.names = F)







# process CSR GRACE data. downloaded from https://search.earthdata.nasa.gov/

rm(list = ls())
library(ncdf4)
library(ncdf4.helpers)
library(lubridate)
library(infotheo)
library(sf)
library(sp)
library(jtools)
library(lubridate)


filez1<-list.files(path="C:/Users/joeja/Desktop/research_postdoc/GRACE_DOWNSCALE/DATA/TELLUS_GRAC_L3_CSR_RL06_LND_v04_RL06v04-20251031_180235",pattern = "*.nc",full.names = T)
filez2<-list.files(path="C:/Users/joeja/Desktop/research_postdoc/GRACE_DOWNSCALE/DATA/TELLUS_GRFO_L3_CSR_RL06.3_LND_v04_RL06.3v04-20251031_194603",pattern = "*.nc",full.names = T)
filez<-c(filez1,filez2)


GRACE_ALL<-NULL
for(i in 1:length(filez)){
  currentWeatherdata<-nc_open(filez[i])
  
  londata<-ncvar_get(currentWeatherdata,varid = "lon_bounds")
  latdata<-ncvar_get(currentWeatherdata,varid = "lat_bounds")
  timedata<-ncvar_get(currentWeatherdata,varid = "time_bounds")
  lwe<-ncvar_get(currentWeatherdata,varid = "lwe_thickness")
  uncertainty<-ncvar_get(currentWeatherdata,varid = "uncertainty")
  nc_close(currentWeatherdata)
  
  numdays<-timedata[2]-timedata[1]
  cur_seq<-seq(from=as.Date("2002-01-01"),by="day",length.out=mean(timedata))
  monthvec<-month(cur_seq[length(cur_seq)])
  yearvec<-year(cur_seq[length(cur_seq)])
  
  londata[londata>180]<- londata[londata>180] - 360
  
  
  whichlon<-which((londata[2,]>= -130 & londata[2,]<= -105) | (londata[1,]>= -130 & londata[1,]<= -105))
  whichlat<-which((latdata[2,]>= 25 & latdata[2,]<= 50) | (latdata[1,]>= 25 & latdata[1,]<= 50))
  lonvec<-londata[1,whichlon]
  latvec<-latdata[1,whichlat]
  posinfo<-expand.grid(latvec,lonvec)
  
  colnames(posinfo)<-c("min_lat","min_lon")
  posinfo$max_lat<-posinfo$min_lat+1
  posinfo$max_lon<-posinfo$min_lon+1
  posinfo$lwe<-NA
  posinfo$uncertainty<-NA
  posinfo$scale_fac<-NA
  posinfo$month<-NA
  posinfo$year<-NA
  posinfo$numdays<-NA
  
  for(j in 1:nrow(posinfo)){
    curlonpos<-which(londata[1,]==posinfo$min_lon[j])
    curlatpos<-which(latdata[1,]==posinfo$min_lat[j])
    
    posinfo$lwe[j]<-lwe[curlonpos,curlatpos]
    posinfo$uncertainty[j]<-uncertainty[curlonpos,curlatpos]
    posinfo$month[j]<-monthvec
    posinfo$year[j]<-yearvec
    posinfo$numdays[j]<-numdays
  }
    
    
  GRACE_ALL<-rbind(GRACE_ALL,posinfo)
  write.csv(posinfo,paste0("C:/Users/joeja/Desktop/research_postdoc/GRACE_DOWNSCALE/DATA/CSR_MONTHLY/GRACE_CSR_month_",i,".csv"),row.names = F)
  print(i)

}

write.csv(GRACE_ALL,paste0("C:/Users/joeja/Desktop/research_postdoc/GRACE_DOWNSCALE/DATA/GRACE_CSR_all.csv"),row.names = F)





# process GFZ GRACE data. downloaded from https://search.earthdata.nasa.gov/

rm(list = ls())
library(ncdf4)
library(ncdf4.helpers)
library(lubridate)
library(infotheo)
library(sf)
library(sp)
library(jtools)
library(lubridate)


filez1<-list.files(path="C:/Users/joeja/Desktop/research_postdoc/GRACE_DOWNSCALE/DATA/TELLUS_GRAC_L3_GFZ_RL06_LND_v04_RL06v04-20251031_165814",pattern = "*.nc",full.names = T)
filez2<-list.files(path="C:/Users/joeja/Desktop/research_postdoc/GRACE_DOWNSCALE/DATA/TELLUS_GRFO_L3_GFZ_RL06.3_LND_v04_RL06.3v04-20251031_173627",pattern = "*.nc",full.names = T)
filez<-c(filez1,filez2)


GRACE_ALL<-NULL
for(i in 1:length(filez)){
  currentWeatherdata<-nc_open(filez[i])
  
  londata<-ncvar_get(currentWeatherdata,varid = "lon_bounds")
  latdata<-ncvar_get(currentWeatherdata,varid = "lat_bounds")
  timedata<-ncvar_get(currentWeatherdata,varid = "time_bounds")
  lwe<-ncvar_get(currentWeatherdata,varid = "lwe_thickness")
  uncertainty<-ncvar_get(currentWeatherdata,varid = "uncertainty")
  nc_close(currentWeatherdata)
  
  numdays<-timedata[2]-timedata[1]
  cur_seq<-seq(from=as.Date("2002-01-01"),by="day",length.out=mean(timedata))
  monthvec<-month(cur_seq[length(cur_seq)])
  yearvec<-year(cur_seq[length(cur_seq)])
  
  londata[londata>180]<- londata[londata>180] - 360
  
  
  whichlon<-which((londata[2,]>= -130 & londata[2,]<= -105) | (londata[1,]>= -130 & londata[1,]<= -105))
  whichlat<-which((latdata[2,]>= 25 & latdata[2,]<= 50) | (latdata[1,]>= 25 & latdata[1,]<= 50))
  lonvec<-londata[1,whichlon]
  latvec<-latdata[1,whichlat]
  posinfo<-expand.grid(latvec,lonvec)
  
  colnames(posinfo)<-c("min_lat","min_lon")
  posinfo$max_lat<-posinfo$min_lat+1
  posinfo$max_lon<-posinfo$min_lon+1
  posinfo$lwe<-NA
  posinfo$uncertainty<-NA
  posinfo$month<-NA
  posinfo$year<-NA
  posinfo$numdays<-NA
  
  for(j in 1:nrow(posinfo)){
    curlonpos<-which(londata[1,]==posinfo$min_lon[j])
    curlatpos<-which(latdata[1,]==posinfo$min_lat[j])
    
    posinfo$lwe[j]<-lwe[curlonpos,curlatpos]
    posinfo$uncertainty[j]<-uncertainty[curlonpos,curlatpos]
    posinfo$month[j]<-monthvec
    posinfo$year[j]<-yearvec
    posinfo$numdays[j]<-numdays
  }
  
  
  GRACE_ALL<-rbind(GRACE_ALL,posinfo)
  write.csv(posinfo,paste0("C:/Users/joeja/Desktop/research_postdoc/GRACE_DOWNSCALE/DATA/GFZ_MONTHLY/GRACE_GFZ_month_",i,".csv"),row.names = F)
  print(i)
  
}

write.csv(GRACE_ALL,paste0("C:/Users/joeja/Desktop/research_postdoc/GRACE_DOWNSCALE/DATA/GRACE_GFZ_all.csv"),row.names = F)







