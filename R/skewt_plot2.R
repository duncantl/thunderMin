skewt_plot = 
function (ptop = 100, isoterms_col = "#d8be9b", temp_stripes = FALSE, 
    mixing_ratio_col = "#8470FF90", dry_adiabats_col = "#d6878750", 
    moist_adiabats_col = "#00FF0095", deg45 = FALSE, isotherm0 = TRUE, 
    close_par = TRUE, ...
  , segments = function(...) NULL,
    degc = seq(-50, 50, by = 10),
    xlab = expression(paste("Temperature [°C]")),
    ylab = "Pressure [hPa]") 
{
    oldpar = par(no.readonly = TRUE)
    if (close_par) {
        on.exit(par(oldpar))
    }
    if (deg45) {
        par(pty = "s")
    }
    ymax = skewty(1050)
    ymin = skewty(ptop)
    xmin = skewtx(-50, skewty(1050))
    # orig:   xmax = skewtx(48.3, skewty(995))
    xmax = max(skewtx(48.3, skewty(995)), max(degc))
    xc = c(xmin, xmin, xmax, xmax, xmin)
    yc = c(ymin, ymax, ymax, ymin, ymin)
    plot(xc, yc, type = "l", axes = FALSE, xlab = "", ylab = "", 
        lwd = 1)
    ypos = skewty(1050)
    axis(1, at = skewtx(degc, ypos), labels = degc, 
        pos = ymax, cex.axis = 0.65, padj = -0.15, tck = -0.01)
    mtext(side = 1, line = 1, xlab, 
        cex = 0.65)
    pres = c(1050, 1000, 850, 700, 500, 300, 200, 100)
    NPRES = length(pres)
    xpl = rep(xmin, times = NPRES)
    xpr = c(xmax, xmax, xmax, xmax, skewtx(20, skewty(500)))
    ypos = skewty(pres[2:NPRES])
    axis(2, las = 1, at = ypos, labels = pres[2:NPRES], pos = xmin, 
        cex.axis = 0.65, lwd = 0)
    mtext(side = 2, line = 1.3, ylab, padj = 2, cex = 0.65)
    kinkx = skewtx(10.5, skewty(400))
    temp = seq(from = -150, to = 60, by = 10)
    NTEMP = length(temp[temp < 60])
    lendt = rep(1050, NTEMP)
    lendt[1:11] = c(49, 63, 87, 118, 163, 222, 303, 414, 565, 
        770, 1050)
    inds = seq(1, length(temp))[(temp > -50 & temp < 60)]
    exponent = (127.182 - (kinkx - 0.54 * temp[inds])/0.90692)/44.061
    rendt = rep(ptop, NTEMP)
    rendt[inds] = 10^exponent
    rendt[which(rendt < ptop)] = ptop
    lendt[which(lendt < ptop)] = ptop
    yl = skewty(rendt)
    xl = skewtx(temp[temp < 60], yl)
    yr = skewty(lendt)
    xr = skewtx(temp[temp < 60], yr)
    segments(xl, yl, xr, yr, col = isoterms_col, lwd = 0.8)
    if (temp_stripes) {
        strt = ifelse(ptop == 150, 1, 2)
        for (i in seq(strt, length(xl), by = 2)) {
            polygon(x = c(xl[i], xr[i], xr[i + 1], xl[i + 1]), 
                y = c(yl[i], yr[i], yr[i + 1], yl[i + 1]), border = NA, 
                col = "#f0e8f475")
        }
    }
    if (isotherm0) {
        inds = which(temp == 0)
        segments(xl[inds], yl[inds], xr[inds], yr[inds], col = "blue3", 
            lwd = 1, lty = 3)
        inds = which(temp == -20)
        segments(xl[inds], yl[inds], xr[inds], yr[inds], col = "blue3", 
            lwd = 1, lty = 3)
    }
    temp1050 = c(256.65, 265.25, 274.45, 284.4, 295.1) - 273.15
    temp600 = c(250.15, 258.25, 266.9, 276.25, 286.25) - 273.15
    yl = skewty((rep(600, times = length(temp600))))
    xr = skewtx(temp1050, skewty(rep(1050, times = length(temp1050))))
    yr = skewty((rep(1050, times = length(temp1050))))
    xl = skewtx(temp600, skewty(rep(600, times = length(temp600))))
    if (!is.na(mixing_ratio_col) && mixing_ratio_col != "") {
        segments(xl, yl, xr, yr, col = mixing_ratio_col, lwd = 0.8, 
            lty = 1)
        text(xl, yl + 0.75, labels = c(1, 2, 4, 8, 16), col = "#8470FF90", 
            adj = 0.5, cex = 0.6)
    }
    theta = seq(from = -50, to = 250, by = 10)
    NTHETA = length(theta)
    lendth = rep(ptop, times = NTHETA)
    lendth[1:8] = c(950, 690, 500, 360, 250, 170, 110, 60)
    if ((!is.na(dry_adiabats_col) && dry_adiabats_col != "")) {
        lendth[lendth < ptop] = ptop
        rendth = rep(1050, times = NTHETA)
        for (itheta in 1:NTHETA) {
            p = seq(from = lendth[itheta], to = rendth[itheta], 
                length = 200)
            sy = skewty(p)
            dry = tda(theta[itheta], p)
            sx = skewtx(dry, sy)
            sx = sx[sx <= xmax]
            if (length(sx)) {
                sy = sy[1:length(sx)]
                lines(sx, sy, lty = 1, col = dry_adiabats_col, 
                  lwd = 0.7)
            }
        }
    }
    if (!is.na(moist_adiabats_col) && moist_adiabats_col != "") {
        p = seq(from = 1050, to = ptop, by = -2)
        npts = length(p)
        sy = skewty(p)
        sx = double(length = npts)
        pseudo = c(34, 28, 21, 14, 5, -5, -18, -32, -45)
        NPSEUDO = length(pseudo)
        holdx = matrix(0, nrow = npts, ncol = NPSEUDO)
        holdy = matrix(0, nrow = npts, ncol = NPSEUDO)
        for (ipseudo in 1:NPSEUDO) {
            for (ilen in 1:npts) {
                moist = satlft(pseudo[ipseudo], p[ilen])
                sx[ilen] = skewtx(moist, sy[ilen])
            }
            inds = (sx < xmin)
            sx[inds] = NA
            sy[inds] = NA
            holdx[, ipseudo] = sx
            holdy[, ipseudo] = sy
        }
        for (ipseudo in 1:NPSEUDO) {
            sx = holdx[, ipseudo]
            sy = holdy[, ipseudo]
            lines(sx, sy, lty = 3, col = moist_adiabats_col)
        }
    }
    y = skewty(pres)
    segments(-27.85, y, 26, y, col = "black", lwd = 0.25, lty = 1)
}
