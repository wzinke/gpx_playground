setwd('/home/zinke/Downloads/')
filename="Christmasmorningwalk.gpx"

Pckglst  = c("ggplot2", "maptools", "XML", "rgdal", "spatstat", "png",
             "dplyr", "purrr", "sp", "ggmap", "raster", "pracma")


for(cPckg in Pckglst){
    if(!require(cPckg , character.only=TRUE)){
        install.packages(cPckg)
        library(cPckg , character.only = TRUE)
    }
}


gpx_dt = filename %>%
    xmlTreeParse(useInternalNodes = TRUE) %>%
    xmlRoot %>%
    xmlToList %>%
    (function(x) x$trk) %>%
    (function(x) unlist(x[names(x) == "trkseg"], recursive = FALSE)) %>%
    map_df(function(x) as.data.frame(t(unlist(x)), stringsAsFactors=FALSE))


names(gpx_dt)

names(gpx_dt) = c("Time", "Latitude", "Longitude") # Just_Draw_It
#names(gpx_dt) = c("Elevation", "Time", "Latitude", "Longitude") # Komoot
#names(gpx_dt) = c("Time", "Elevation", "Latitude", "Longitude")


gpx_dt$Longitude = as.numeric(gpx_dt$Longitude)
gpx_dt$Latitude  = as.numeric(gpx_dt$Latitude)
#gpx_dt$Elevation = as.numeric(gpx_dt$Elevation)
#gpx_dt$Time      = as.POSIXct(gpx_dt$Time, tz="GMT", format="%Y-%m-%dT%H:%M:%OS")

Npts = length(gpx_dt$Longitude)


# interpolate times  <time>2020-09-27T14:34:42Z</time>

Tstart = as.POSIXct('2021-12-25 07:35:00', tz="GMT", format="%Y-%m-%d %H:%M:%OS")
Tend   = as.POSIXct('2021-12-25 08:18:28', tz="GMT", format="%Y-%m-%d %H:%M:%OS")
Tdur = difftime(Tend, Tstart,units='h')

nDuration = seq(from=0, to=as.numeric(Tdur), by=5/3600)

gpx_dt$Duration = linspace(0, as.numeric(Tdur), n = Npts)

#gpx_dt$Elevation = linspace(gpx_dt$Elevation[1], max(gpx_dt$Elevation), n = Npts)

Lon_ip = approxfun(gpx_dt$Duration, gpx_dt$Longitude, rule=2)
Lat_ip = approxfun(gpx_dt$Duration, gpx_dt$Latitude,  rule=2)
#Ele_ip = approxfun(gpx_dt$Duration, gpx_dt$Elevation, rule=2)


nLon = Lon_ip(nDuration)
nLat = Lat_ip(nDuration)
#nEle = Ele_ip(nDuration)

nTim = Tstart + as.difftime(nDuration, format='%H', units = "hours")

trk_df = data.frame(Latitude=nLat, Longitude=nLon, Time=nTim)

writeGPX = function(gpx_trk, file="file.gpx"){
    o = c('<gpx version="1.1" creator="R">','<trk>','<trkseg>')
    o = c(o, paste('<trkpt lat="', gpx_trk$Latitude,'" lon="', gpx_trk$Longitude,'"><time>',
                   paste(gsub(' ','T', as.character(gpx_trk$Time)), 'Z', sep=''),
                   '</time></trkpt>', sep=''))

    o = c(o, '</trkseg>', '</trk>', '</gpx>')
    cat(o, file=file, sep='\n')
}

writeGPX(trk_df, file="gpx_Tip.gpx")


