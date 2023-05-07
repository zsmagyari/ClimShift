# ClimShift - a toolbox for climate shift analysis
**Climate Shift analysis with R**

Currently ClimShift is in early development stage working on E-OBS NetCDF dataset.

**Preliminary steps**

    
  1. Data preparation: four NetCDF files has to be present in data folder with a chosen prefix template followed by

  2. R library preparation: the folowing external package have to be installed:
          
 - shiny            
 - shinyFiles            
 - leaflet          
 - ncdf4         
 - rgdal        
 - raster        
 - shinyWidgets

  3. Download and launch the toolbox from GitHub inside R Studio

```R
library(shiny)

runGitHub('ClimShift','zsmagyari', destdir="your_temporary_folder", ref="main")
```
where ***your_temporary_folder*** is the full folder reference where you want to store the application.

Sample data is available in sampledata folder. There's a short demon video in the ***tutorial folder***.


**Analysis steps**

  1. Choose the data folder using *Choose folder* button.
  2. Enter the file name prefix template in the Filename template input box.

        - Remarks
            - if all four files can be identified by the toolbox the other controls will be enabled
            - starting and ending years will be set automatically to the lowest and highest year value from the NetCDF file. the spatial extent of the NetCDF datafiles are shown on the maps 
            - base location coordinates are set to the center of the analysis area.
 3. Set the base location and the desired time interval for base and analysis period

     - Remarks 
