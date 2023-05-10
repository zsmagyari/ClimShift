# ClimShift - a toolbox for climate shift analysis
**Climate Shift analysis with R**  by *Zsolt **MAGYARI-SÁSKA**,  Adina-Eliza **CROITORU**, Csaba **HORVÁTH**, Ștefan **DOMBAY** from Babeș-Bolyai University, Faculty of Geography, Romania*  

Currently ClimShift is in the early development stage working on E-OBS NetCDF dataset. 
 
**Preliminary steps**

    
  **1.** Data preparation: four NetCDF files has to be present in data folder with a chosen prefix template followed by *tn*, *tx*, *tg* and *rr* abbreviations containing the daily minimum, maximum and mean temperatures as well as the daily precipitation amount. All four data files should have the same spatial extent.
  
  ![alt text](https://github.com/zsmagyari/ClimShift/blob/main/datafiles.png?raw=true)
  
  **2.** R library preparation: the following external package must be installed:  
  
  - shiny            
  - shinyFiles     
  - shinyjs       
  - leaflet          
  - ncdf4         
  - rgdal        
  - raster        
  - shinyWidgets
  - gsubfn

  **3.** Download and launch the toolbox from GitHub inside R Studio

```R
library(shiny)

runGitHub('ClimShift','zsmagyari', destdir="your_temporary_folder", ref="main")
```
where ***your_temporary_folder*** is the full folder reference where you want to store the application.

The toolbox will be launched inside your browser. Sample data is available in ***sampledata*** subfolder. There's a short demo video in the ***tutorial*** folder.


**Analysis steps**

  **1.** Choose the data folder using *Choose folder* button. 
  
  **2.** Enter the file name prefix template in the *Filename template* input box.  
  
   - ##### Remarks   
        - ###### if all four files can be identified by the toolbox the other controls will be enabled
        - ###### starting and ending years will be set automatically to the lowest and highest year value from the NetCDF file. the spatial extent of the NetCDF datafiles are shown on the maps
        - ###### base location coordinates are set to the centre of the analysis area.
 **3.** Set the base location and the desired time interval for base and analysis period  
 
   - ##### Remarks  
        - ###### setting values outside time interval will disable the Analyze button
        - ###### starting and ending month are considered as continuous time interval for each year. ex. Setting 3 and 5 means that data aggregation period will be March – May for each year. To work with full years set 1 and 12 for starting and ending months respectively
        - ###### the base location can be set interactively with mouse click or entering the values in input fields. Values outside the analysis area will disable the Analyze button. 

**4.** Launch the analysis with *Analyze* button. The progress bar will indicate the processing stage.

**5.** Inspect the result show on the map both for base and analysis period as well as the areas with highest similarity.

**6.** Save the results. Three files will be created in the data folder, two raster files with the base and analysis period similarity index and text file with the area values.

   - ##### Remarks   
     
     - ###### the names of saved files contain the file template name, the base location coordinates the base period year and moth and the analysis period year and month  
     - ###### raster layers are saved in *grd/gri* format, area values in txt file

