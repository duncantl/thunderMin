

#

Google identified 2 packages

+ thunder
+ RadioSonde


### thunder

+ dependencies - Rcpp, httr, etc.
+ could potentially get data, but goes to old defunct server
+ sounding_plot() includes what we want, but lots more in the single display.
  + Need to extract what we want.

### RadioSonde 

+ was co-developed by Doug Nychka - good
+ does less which is good
+ doesn't include CAPE.


Working with thunder.

Reasons

+ start with this.
+ includes CAPE
+ hopefully easier to remove than to add
+ barbs for wind closer to what is on the original display.


##  thunder package


## sounding_plot()

Calls 
+ skewt_plot() to get the generic temperature v pressure plot with no date.
  + Can we control the xlim, ylim

+ skewt_lines(output2$dpt, output2$pressure, col = t_col("forestgreen", 10), lwd = 2, ptop = 100)

+ skewt_lines(output$temp, output$pressure, col = t_col("red", 
        10), lwd = 2, ptop = 100)


## skewt_plot

+ Contains a lot of hard-coded constants.  And repeats.

+ Change !is.na(col) || col != ""
   + Need && instead of ||


+ To not show the contour curves, pass the colors as "" or NA

```
skewt_plot(isoterms_col = NA, mixing_ratio_col = "", dry_adiabats_col = "", moist_adiabats_col = "", isotherm0 = FALSE)
```

  + Leaves 1 2 4 8 16 text


+ x-axis label overlapping with ticks.
  + Fixed - change line = 0 to 1

+  Set the xlim
```
skewt_plot(isoterms_col = NA, mixing_ratio_col = "", dry_adiabats_col = "", moist_adiabats_col = "", isotherm0 = FALSE , degc = seq(-60, 40, by = 10))
```
   + But showing wider
