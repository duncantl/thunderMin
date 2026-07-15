if(FALSE) {

    i = read.csv("soundingInfo.csv")
    dt = mapply(getSoundingData, i$station, i$date, SIMPLIFY = FALSE)
    names(dt) = i$name
    saveRDS(dt, "SoundingData1.rds")
}

getSoundingData =
function(station, date, asCSV = FALSE)
{

    if(is.character(date))
        date = as.Date(date)

    if(inherits(date, "POSIXt"))
        date = as.Date(date)
    
    if(inherits(date, "Date"))
        date = format(date, "%Y-%m-%d")
      
   u = sprintf("https://weather.uwyo.edu/wsgi/sounding?datetime=%s%%2000:00:00&id=%s&type=TEXT:CSV&src=BUFR",
               date, station)

   txt = readLines(u)
   if(asCSV)
       return(txt)
   
   dt = read.csv(textConnection(txt))
   dt$time = as.POSIXct(strptime(dt$time, "%Y-%m-%d %H:%M:%S"))
   
   dt
}    
