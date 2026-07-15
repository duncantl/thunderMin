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
