# ClimShift - a toolbox for climate shift analysis
**Climate Shift analysis with R**

Currently ClimShift is in early development stage working on E-OBS NetCDF dataset.

**Preliminary steps**
    1.	Data preparation: four NetCDF files has to be present in data folder with a chosen prefix template followed by _tn, _tx, _tg and _rr abbreviations containing the daily minimum, maximum and mean temperatures as well as the daily precipitation amount. All four data files should have the same spatial extent.

    2. R library preparation: the folowing external package have to be installed:
              - shiny
              - shinyFiles
              - leaflet
              - ncdf4
              - rgdal
              - raster
              - shinyWidgets
     3.	Download and launch the toolbox from GitHub inside R Studio

```R
library(shiny)

runGitHub('ClimShift','zsmagyari', destdir="your_temporary_folder", ref="main")
```
where ***your_temporary_folder*** is the full folder reference where you want to store the application.

Sample data is available in sampledata folder.

You can find a short demon video in the ***tutorial folder***.
