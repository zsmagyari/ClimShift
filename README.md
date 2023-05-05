# ClimShift
**Climate Shift analysis with R**

The purpose of this application is to perform climate shift analysis on E-OBS dataset using similarity index.

External package requirements:
- shiny
- shinyFiles
- leaflet
- ncdf4
- rgdal
- raster
- shinyWidgets

To download and run in R environment you should type the following commands
```R
library(shiny)

runGitHub('ClimShift','zsmagyari', destdir="your_temporary_folder", ref="main")
```
where ***your_temporary_folder*** is the full folder reference where you want to store teh application.
Sample data is available in smapledata folder.
More about the application you can find in the tutorial folder.
