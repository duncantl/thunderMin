foo =
function (pressure, altitude, temp, dpt, wd, ws, title = "", 
    parcel = "MU", max_speed = 25, buoyancy_polygon = TRUE, SRH_polygon = "03km_RM", 
    DCAPE = FALSE, meanlayer_bottom_top = c(0, 500), storm_motion = c(999, 
                                                                      999), ...)
{    
    output = sounding_export(pressure, altitude, temp, dpt, wd, 
                             ws, meanlayer_bottom_top = meanlayer_bottom_top, storm_motion = storm_motion)
    output2 = sounding_export(pressure, altitude, temp,
                              ifelse(dpt == -273, NA, dpt),
                              wd, ws, meanlayer_bottom_top = meanlayer_bottom_top, 
                              storm_motion = storm_motion)


    skewt_plot(close_par = FALSE)
    skewt_lines(output2$dpt, output2$pressure, col = t_col("forestgreen", 
        10), lwd = 2, ptop = 100)
    skewt_lines(output$temp, output$pressure, col = t_col("red", 
        10), lwd = 2, ptop = 100)
}


t_col = function(color, percent, name = NULL) {
    rgb.val = col2rgb(color)
    t.col = rgb(rgb.val[1], rgb.val[2], rgb.val[3], maxColorValue = 255, 
                alpha = (100 - percent) * 255/100, names = name)
    invisible(t.col)
}
