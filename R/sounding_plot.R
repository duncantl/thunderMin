if(FALSE) {
    draw(carr)
    
    with(carr,
         sounding_plot(
             pressure = pressure_hPa,
             altitude = geopotential.height_m, # altitude,
             temp = temperature_C,
             dpt = dew.point.temperature_C,
             wd = wind.direction_degree,
             ws = wind.speed_m.s,
             title = "Carr Fire - July 28, 2018",
             showCAPEBoundary = FALSE,
             xlab = "", ylab = ""
         )
         )
}


draw =
function(d, title = "Carr Fire - July 28, 2018",
         showCAPEBoundary = FALSE,
         showCAPEText = showCAPEBoundary,
         CAPE.boundary.col = "orange",
         CAPE.cols = c("#FF000035", "#FFA50025"),
         temp.col = "red",
         dew.col = "black",         
         xlab = "", ylab = "",
         degc = seq(-40, 60, by = 10))
{
    sounding_plot(
        pressure = d$pressure_hPa,
        altitude = d$geopotential.height_m, # altitude,
        temp = d$temperature_C,
        dpt = d$dew.point.temperature_C,
        wd = d$wind.direction_degree,
        ws = d$wind.speed_m.s,
        title = title,
        showCAPEBoundary = showCAPEBoundary,
        showCAPEText = showCAPEText,
        CAPE.boundary.col = CAPE.boundary.col,
        CAPE.cols = CAPE.cols,
        temp.col = temp.col,
        dew.col = dew.col,
        xlab = xlab, ylab = ylab,
        degc = degc
    )
}

sounding_plot =
function (pressure, altitude, temp, dpt, wd, ws, title = "", 
          parcel = "MU",
          max_speed = 25,                             # unused
          buoyancy_polygon = TRUE,
          SRH_polygon = "03km_RM", DCAPE = FALSE,     # both unused
          meanlayer_bottom_top = c(0, 500),           
          storm_motion = c(999, 999),                 
          ...,
          showCAPEBoundary = TRUE,
          showCAPEText = showCAPEBoundary,
          CAPE.boundary.col = "orange",
          CAPE.cols = c("#FF000035", "#FFA50025"),          
          temp.col = "red",
          dew.col = "forestgreen",
          xlab = expression(paste("Temperature [°C]")),
          ylab = "Pressure [hPa]",
          degc = seq(-40, 60, by = 10)
          )
{
    oldpar = par(no.readonly = TRUE)
    on.exit(par(oldpar))

    if (all(storm_motion == 999, na.rm = TRUE) == 2) 
        storm_motion = c(999, 999, 999)
    else 
        storm_motion = c(storm_motion[1], storm_motion[2], 999)

    
    output = sounding_export(pressure, altitude, temp, dpt, wd, 
                             ws, meanlayer_bottom_top = meanlayer_bottom_top, storm_motion = storm_motion)
    output2 = sounding_export(pressure, altitude, temp,
                              ifelse(dpt == -273, NA, dpt),
                              wd, ws, meanlayer_bottom_top = meanlayer_bottom_top, 
                              storm_motion = storm_motion)

    skewt_plot(isoterms_col = NA, mixing_ratio_col = "", dry_adiabats_col = "", moist_adiabats_col = "",
               isotherm0 = FALSE , close_par = FALSE, xlab = xlab, ylab = ylab, degc = degc)

    skewt_lines(output2$dpt, output2$pressure, col = t_col(dew.col, 10), lwd = 2, ptop = 100)
    skewt_lines(output$temp, output$pressure, col = t_col(temp.col, 10), lwd = 2, ptop = 100)


    parametry = sounding_compute(pressure, altitude, temp, dpt, 
                                 wd, ws, accuracy = 3, meanlayer_bottom_top = meanlayer_bottom_top, 
                                 storm_motion = storm_motion[1:2])



    # CAPE
    # Need any intermediate variables
    # Boundary
    if(parcel != "none" && parcel != "") {
        if(showCAPEBoundary)
            skewt_lines(output$MU, output$pressure, col = CAPE.boundary.col, lty = 1, lwd = 1, ptop = 100)
        
        # Polygon
        LP = max(which(!is.na(names(parametry))))
        showCAPE(parcel, parametry, output, LP, buoyancy_polygon, meanlayer_bottom_top, showCAPEText, CAPE.cols)
    }

    showBarbs(output)
    
    title(title, outer = TRUE, line = -1.5)
}


t_col = function(color, percent, name = NULL) {
    rgb.val = col2rgb(color)
    t.col = rgb(rgb.val[1], rgb.val[2], rgb.val[3], maxColorValue = 255, 
                alpha = (100 - percent) * 255/100, names = name)
    invisible(t.col)
}


showBarbs =
function(output)
{
    par(fig = c(0.93, 1., 0.03, 0.95), new = TRUE, mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
    over100 = output[["pressure"]] > 100
    sounding_barbs(pressure = output[["pressure"]][over100], 
                   ws = output[["ws"]][over100], wd = output[["wd"]][over100], 
                   altitude = output[["altitude"]][over100], convert = FALSE, 
                   barb_cex = 0.8)
}



showCAPE =
function(parcel, parametry, output, LP, buoyancy_polygon, meanlayer_bottom_top, showCAPEText = TRUE, CAPE.cols = c("#FF000035", "#FFA50025"), ...)
{
    switch(parcel,
           ML = showCAPE.ML(parcel, parametry, output, LP, buoyancy_polygon, meanlayer_bottom_top, showCAPEText, CAPE.cols, ...),
           MU = showCAPE.MU(parcel, parametry, output, LP, buoyancy_polygon, showCAPEText, CAPE.cols, ...),
           SB = showCAPE.SB(parcel, parametry, output, LP, buoyancy_polygon, showCAPEText, CAPE.cols, ...),                      
           stop(paste("unknown parcel value", parcel)))
}


showCAPE.ML =
function(parcel, parametry, output, LP, buoyancy_polygon, meanlayer_bottom_top, showCAPEText = TRUE, CAPE.cols = c("#FF000035", "#FFA50025"), ...)    
{
    vsb_lcl = parametry[which(names(parametry[1:LP]) == 
                              "ML_LCL_HGT")] + output$altitude[1]
    vsb_lfc = parametry[which(names(parametry[1:LP]) == 
                              "ML_LFC_HGT")] + output$altitude[1]
    vsb_muhgt = meanlayer_bottom_top[1] + output$altitude[1]
    vsb_el = parametry[which(names(parametry[1:LP]) == 
                             "ML_EL_HGT")] + output$altitude[1]
    vsb_eff = (parametry[which(names(parametry[1:LP]) == 
                               "ML_EL_HGT")]/2) + output$altitude[1]
    ind_lcl = which.min(abs(output$altitude - vsb_lcl))
    ind_lfc = which.min(abs(output$altitude - vsb_lfc))
    ind_muhgt = which.min(abs(output$altitude - vsb_muhgt))
    ind_el = which.min(abs(output$altitude - vsb_el))
    ind_eff = which.min(abs(output$altitude - vsb_eff))
    y_eff = skewty(output$pressure[ind_eff])
    x_eff = skewtx(output$ML[ind_eff], skewty(output$pressure[ind_eff]))
    y_el = skewty(output$pressure[ind_el])
    x_el = skewtx(output$ML[ind_el], skewty(output$pressure[ind_el]))
    y_lfc = skewty(output$pressure[ind_lfc])
    x_lfc = skewtx(output$ML[ind_lfc], skewty(output$pressure[ind_lfc]))
    y_lcl = skewty(output$pressure[ind_lcl])
    x_lcl = skewtx(ifelse(output$ML[ind_lcl] > output$tempV[ind_lcl], 
                          output$ML[ind_lcl], output$tempV[ind_lcl]), skewty(output$pressure[ind_lcl]))
    y_muhgt = skewty(output$pressure[ind_muhgt])
    x_muhgt = skewtx(output$ML[ind_muhgt], skewty(output$pressure[ind_muhgt]))
#    skewt_lines(output$ML, output$pressure, col = "orange", lty = 1, lwd = 1, ptop = 100)
    v = skewty(c(output$pressure[ind_muhgt:ind_el]))
    diff = ifelse((skewtx(output$tempV[ind_muhgt:ind_el], 
                          v) - skewtx(output$ML[ind_muhgt:ind_el], v)) > 
                  0, 1, 0)
    v = subset(v, v < 44)
    diff = subset(diff, v < 44)
    inte = rle(diff)
    end_pol = cumsum(inte$lengths)
    if (length(end_pol) == 1) {
        start_pol = 1
    }
    else {
        start_pol = c(1, cumsum(inte$lengths) + 1)[-length(end_pol) - 
                                                   1]
    }
    if (buoyancy_polygon == TRUE & ind_lfc != ind_el) {
        for (i in 1:length(end_pol)) {
            if (inte$values[i] == 1) {
                if (parametry[which(names(parametry[1:LP]) == 
                                    "ML_CIN")] < 0) {
                    polygon(c(skewtx(output$tempV[ind_muhgt:ind_el], 
                                     v)[start_pol[i]:end_pol[i]], rev(skewtx(output$ML[ind_muhgt:ind_el], 
                                                                             v)[start_pol[i]:end_pol[i]])), c(v[start_pol[i]:end_pol[i]], 
                                                                                                              rev(v[start_pol[i]:end_pol[i]])), col = CAPE.cols[1], 
                            border = NA)
                }
            }
            if (inte$values[i] == 0) {
                if (parametry[which(names(parametry[1:LP]) == 
                                    "ML_CAPE")] > 0) {
                    polygon(c(skewtx(output$tempV[ind_muhgt:ind_el], 
                                     v)[start_pol[i]:end_pol[i]], rev(skewtx(output$ML[ind_muhgt:ind_el], 
                                                                             v)[start_pol[i]:end_pol[i]])), c(v[start_pol[i]:end_pol[i]], 
                                                                                                              rev(v[start_pol[i]:end_pol[i]])), col = CAPE.cols[2], 
                            border = NA)
                }
            }
        }
    }

    # Text

  if(showCAPEText) {
    if (meanlayer_bottom_top[1] == 0) {
        if (parametry[which(names(parametry[1:LP]) == 
                            "ML_CAPE")] > 0) {
            if (output$pressure[which(output$altitude - 
                                      output$altitude[1] == (parametry[which(names(parametry[1:LP]) == 
                                                                             "ML_EL_HGT")]))] > 100 & which(names(parametry[1:LP]) == 
                                                                                                            "ML_EL_HGT") != 0) {
                text(x_el, y_el, paste0("---- ML EL"), pos = 4, 
                     cex = 0.62, col = "black")
            }
            text(x_lcl, y_lcl, paste0("---- ML LCL"), pos = 4, 
                 cex = 0.62, col = "black")
        }
    }
    else {
        if (parametry[which(names(parametry[1:LP]) == 
                            "ML_CAPE")] > 0) {
            if (output$pressure[which(output$altitude - 
                                      output$altitude[1] == (parametry[which(names(parametry[1:LP]) == 
                                                                             "ML_EL_HGT")]))] > 100 & which(names(parametry[1:LP]) == 
                                                                                                            "ML_EL_HGT") != 0) {
                text(x_el, y_el, paste0("---- ML EL"), pos = 4, 
                     cex = 0.62, col = "black")
            }
            text(x_lcl, y_lcl, paste0("---- ML LCL"), pos = 4, 
                 cex = 0.62, col = "black")
        }
    }
  }
    
}


showCAPE.MU =
function(parcel, parametry, output, LP, buoyancy_polygon, showCAPEText = TRUE, CAPE.cols = c("#FF000035", "#FFA50025"), ...)    
{
    vsb_lcl = parametry[which(names(parametry[1:LP]) == 
                              "MU_LCL_HGT")] + output$altitude[1]
    vsb_lfc = parametry[which(names(parametry[1:LP]) == 
                              "MU_LFC_HGT")] + output$altitude[1]
    vsb_muhgt = parametry[which(names(parametry[1:LP]) == 
                                "HGT_max_thetae_03km")] + output$altitude[1]
    vsb_el = parametry[which(names(parametry[1:LP]) == 
                             "MU_EL_HGT")] + output$altitude[1]
    vsb_eff = ((parametry[which(names(parametry[1:LP]) == 
                                "MU_EL_HGT")] - parametry[which(names(parametry[1:LP]) == 
                                                                "HGT_max_thetae_03km")])/2) + output$altitude[1]
    ind_lcl = which.min(abs(output$altitude - vsb_lcl))
    ind_lfc = which.min(abs(output$altitude - vsb_lfc))
    ind_muhgt = which.min(abs(output$altitude - vsb_muhgt))
    ind_el = which.min(abs(output$altitude - vsb_el))
    ind_eff = which.min(abs(output$altitude - vsb_eff))
    y_eff = skewty(output$pressure[ind_eff])
    x_eff = skewtx(output$MU[ind_eff], skewty(output$pressure[ind_eff]))
    y_el = skewty(output$pressure[ind_el])
    x_el = skewtx(output$MU[ind_el], skewty(output$pressure[ind_el]))
    y_lfc = skewty(output$pressure[ind_lfc])
    x_lfc = skewtx(output$MU[ind_lfc], skewty(output$pressure[ind_lfc]))
    y_lcl = skewty(output$pressure[ind_lcl])
    x_lcl = skewtx(ifelse(output$MU[ind_lcl] > output$tempV[ind_lcl], 
                          output$MU[ind_lcl], output$tempV[ind_lcl]), skewty(output$pressure[ind_lcl]))
    y_muhgt = skewty(output$pressure[ind_muhgt])
    x_muhgt = skewtx(output$MU[ind_muhgt], skewty(output$pressure[ind_muhgt]))
    # skewt_lines(output$MU, output$pressure, col = "orange", lty = 1, lwd = 1, ptop = 100)
    v = skewty(c(output$pressure[ind_muhgt:ind_el]))
    diff = ifelse((skewtx(output$tempV[ind_muhgt:ind_el], 
                          v) - skewtx(output$MU[ind_muhgt:ind_el], v)) > 
                  0, 1, 0)
    v = subset(v, v < 44)
    diff = subset(diff, v < 44)
    inte = rle(diff)
    end_pol = cumsum(inte$lengths)
    if (length(end_pol) == 1) {
        start_pol = 1
    }
    else {
        start_pol = c(1, cumsum(inte$lengths) + 1)[-length(end_pol) - 
                                                   1]
    }
    if (buoyancy_polygon == TRUE & ind_lfc != ind_el) {
        for (i in 1:length(end_pol)) {
            if (inte$values[i] == 1) {
                if (parametry[which(names(parametry[1:LP]) == "MU_CIN")] < 0) {
                    polygon(c(skewtx(output$tempV[ind_muhgt:ind_el], v)[start_pol[i]:end_pol[i]],
                              rev(skewtx(output$MU[ind_muhgt:ind_el], v)[start_pol[i]:end_pol[i]])),
                            c(v[start_pol[i]:end_pol[i]],
                              rev(v[start_pol[i]:end_pol[i]])),
                            col = CAPE.cols[1], 
                            border = NA)
                }
            }
            if (inte$values[i] == 0) {
                if (parametry[which(names(parametry[1:LP]) == 
                                    "MU_CAPE")] > 0) {
                    polygon(c(skewtx(output$tempV[ind_muhgt:ind_el], 
                                     v)[start_pol[i]:end_pol[i]],
                              rev(skewtx(output$MU[ind_muhgt:ind_el], v)[start_pol[i]:end_pol[i]])),
                            c(v[start_pol[i]:end_pol[i]], rev(v[start_pol[i]:end_pol[i]])),
                            col = CAPE.cols[2],
                            border = NA)
                }
            }
        }
    }


    # Text

    if (showCAPEText && parametry[which(names(parametry[1:LP]) == "MU_CAPE")] >  0) {
        if (output$pressure[which(output$altitude - output$altitude[1] == 
                                  (parametry[which(names(parametry[1:LP]) == 
                                                   "MU_EL_HGT")]))] > 100 & which(names(parametry[1:LP]) == 
                                                                                  "MU_EL_HGT") != 0) {
            text(x_el, y_el, paste0("---- MU EL"), pos = 4, 
                 cex = 0.62, col = "black")
        }
        text(x_lcl, y_lcl, paste0("---- MU LCL"), pos = 4, 
             cex = 0.62, col = "black")
    }    
}

    
    
showCAPE.SB =
function(parcel, parametry, output, LP, buoyancy_polygon, showCAPEText = TRUE, CAPE.cols = c("#FF000035", "#FFA50025"), ...)    
{
    vsb_lcl = parametry[which(names(parametry[1:LP]) == 
                              "SB_LCL_HGT")] + output$altitude[1]
    vsb_lfc = parametry[which(names(parametry[1:LP]) == 
                              "SB_LFC_HGT")] + output$altitude[1]
    vsb_el = parametry[which(names(parametry[1:LP]) == 
                             "SB_EL_HGT")] + output$altitude[1]
    vsb_eff = (parametry[which(names(parametry[1:LP]) == 
                               "SB_EL_HGT")]/2) + output$altitude[1]
    ind_lcl = which.min(abs(output$altitude - vsb_lcl))
    ind_lfc = which.min(abs(output$altitude - vsb_lfc))
    ind_el = which.min(abs(output$altitude - vsb_el))
    ind_eff = which.min(abs(output$altitude - vsb_eff))
    y_eff = skewty(output$pressure[ind_eff])
    x_eff = skewtx(output$SB[ind_eff], skewty(output$pressure[ind_eff]))
    y_el = skewty(output$pressure[ind_el])
    x_el = skewtx(output$SB[ind_el], skewty(output$pressure[ind_el]))
    y_lfc = skewty(output$pressure[ind_lfc])
    x_lfc = skewtx(output$SB[ind_lfc], skewty(output$pressure[ind_lfc]))
    y_lcl = skewty(output$pressure[ind_lcl])
    x_lcl = skewtx(ifelse(output$SB[ind_lcl] > output$tempV[ind_lcl], 
                          output$SB[ind_lcl], output$tempV[ind_lcl]), skewty(output$pressure[ind_lcl]))
    # skewt_lines(output$SB, output$pressure, col = "orange", lty = 1, lwd = 1, ptop = 100)
    v = skewty(c(output$pressure[1:ind_el]))
    diff = ifelse((skewtx(output$tempV[1:ind_el], v) - 
                   skewtx(output$SB[1:ind_el], v)) > 0, 1, 0)
    v = subset(v, v < 44)
    diff = subset(diff, v < 44)
    inte = rle(diff)
    end_pol = cumsum(inte$lengths)
    if (length(end_pol) == 1) {
        start_pol = 1
    }
    else {
        start_pol = c(1, cumsum(inte$lengths) + 1)[-length(end_pol) - 
                                                   1]
    }
    if (buoyancy_polygon == TRUE & ind_lfc != ind_el) {
        for (i in 1:length(end_pol)) {
            if (inte$values[i] == 1) {
                if (parametry[which(names(parametry[1:LP]) == 
                                    "SB_CIN")] < 0) {
                    polygon(c(skewtx(output$tempV[1:ind_el], v)[start_pol[i]:end_pol[i]],
                              rev(skewtx(output$SB[1:ind_el], v)[start_pol[i]:end_pol[i]])),
                            c(v[start_pol[i]:end_pol[i]], rev(v[start_pol[i]:end_pol[i]])),
                            col = CAPE.cols[1], 
                            border = NA)
                }
            }
            if (inte$values[i] == 0) {
                if (parametry[which(names(parametry[1:LP]) == 
                                    "SB_CAPE")] > 0) {
                    polygon(c(skewtx(output$tempV[1:ind_el], v)[start_pol[i]:end_pol[i]],
                              rev(skewtx(output$SB[1:ind_el], v)[start_pol[i]:end_pol[i]])),
                            c(v[start_pol[i]:end_pol[i]], rev(v[start_pol[i]:end_pol[i]])),
                            col = CAPE.cols[2], border = NA)
                }
            }
        }
    }


    # Text
    if (showCAPEText && parametry[which(names(parametry[1:LP]) == "SB_CAPE")] > 0) {
        if (output$pressure[which(output$altitude - output$altitude[1] == 
                                  (parametry[which(names(parametry[1:LP]) == 
                                                   "SB_EL_HGT")]))] > 100 & which(names(parametry[1:LP]) == 
                                                                                  "SB_EL_HGT") != 0) {
            text(x_el, y_el, paste0("---- SB EL"), pos = 4, 
                 cex = 0.62, col = "black")
        }
        text(x_lcl, y_lcl, paste0("---- SB LCL"), pos = 4, 
             cex = 0.62, col = "black")
    }    
}
    
