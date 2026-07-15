if(FALSE) {

    i = read.csv("soundingInfo.csv")
    dt = mapply(getSoundingData, i$station, i$date, SIMPLIFY = FALSE)
    names(dt) = i$name
    saveRDS(dt, "SoundingData1.rds")
}

getSoundingData =
function(station, date)
{    
   u =sprintf("https://weather.uwyo.edu/wsgi/sounding?datetime=%s%%2000:00:00&id=%s&type=TEXT:CSV&src=BUFR",
              date, station)

    txt = readLines(u)
    read.csv(textConnection(txt))
}    
