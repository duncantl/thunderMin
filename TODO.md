+ get temperature axis max and labels correct.
   + some hard coded constant that may need to be adjusted for different degc min and max.

+ wind barbs to match official diagrams.

+ √ Temperature min and max and tick marks.
  + allow caller to specify

+ √ user-specifiable colors for 2 curves and CAPE.
   + √ 2 curves - `temp.col`, `dew.col`
   + √ CAPE boundary - `CAPE.boundary.col`
   + √ CAPE polygon color - `CAPE.cols`
      + 2 colors

+ allow isotherms, etc. to be displayed.

## Optional

+ match the labels for the pressure axis with the wyoming figures
   + this version omits 400, 600, 800, 900, but has 850

+ put station information in title
   + Id
   + city, state, country
   + latitude and longitude
