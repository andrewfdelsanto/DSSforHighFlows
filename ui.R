
# Load R packages
library(shiny)
library(shinythemes)
library(dataRetrieval)
library(fasstr)
library(lattice)
library(latticeExtra)
library(streamstats)
library(leaflet)
library(Kendall)
library(ggplot2)
library(plyr)
library(caret)
library(randomForest)
library(FloodFreqPlot)
library(tidyverse)
library(stats)
library(dplyr)

###################################################################################################################################
# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage("CAFE - High Flow Calculator",
                           
                           ###Tabs      
                           ###################################################################################################################################
                           
                           #Tab 0 - Introduction
                           tabPanel("Introduction", icon = icon("info-circle"),
                                    
                                    # Input form
                                    sidebarPanel(
                                      h2("Welcome to CAFE!"),
                                      h5("This program, the Calculation Assistant for Flow Extremes (CAFE), is designed to help resource managers throughout the Northeast United States analyze and calculate both extreme low and high streamflow metrics."),
                                      h5("This is the high flow portion of the program. The low flow version of the program can be found at: https://andrewdelsanto.shinyapps.io/LowFlowDSS/ "),
                                      h5("Additionally, you can download this program's documentation below, which explains how to use the program and all functions/packages utilized:"),
                                      downloadButton("downloadBtn2", "Download Documentation")
                                    ),
                                    
                                    # Output display
                                    mainPanel(
                                      h2("CAFE - Calculation Assistant for Flow Extremes"),
                                      imageOutput("prettypicture")
                                    )
                                    
                           ), #tabPanel1
                           
                           navbarMenu("Gaged Flow Calculator", icon = icon("water"),
                                      ###################################################################################################################################                  
                                      
                                      #Tab 0 - Explanation of this Menu & Module
                                      tabPanel("Explanation of this Module",
                                               
                                               # Input form
                                               sidebarPanel(
                                                 h3("Welcome to the Gaged High Flow Calculator!"),
                                                 h5("This portion of the program allows the user to calculate the 25, 50, and 100 year flood for any NWIS streamgage with daily streamflow data."),
                                                 h5("It follows standard Bulletin 17C procedures for calculating the high flow metrics, using Method of Moments (MOM) to estimate parameters combined with Log-Pearson Type 3 extrapolation."),
                                                 h5("Additionally, it allows the user to: 1) subset the streamflow data given a starting and end date, and 2) scale the flows if the location of interest is slightly up or downstream from the gage."),
                                               ),
                                               
                                               # Output display
                                               mainPanel(
                                                 h2("Bulletin 17C - Guidelines for Determining Flood Flow Frequency"),
                                                 imageOutput("bulletin")
                                               )
                                               
                                      ), #tabPanel0   
                                      ####################################################################################################################################
                                      
                                      
                                      #Tab 1 - For downloading NWIS Streamflow data
                                      tabPanel("Gaged Tool Step 1: Download Streamflow Data",
                                               
                                               # Input form
                                               sidebarPanel(
                                                 h2("Retrieve Streamflow"),
                                                 h4("Enter an 8 digit NWIS gage number with daily flow data:"),
                                                 textInput(inputId = "gage", "Enter NWIS Gage Number:", value = "01013500"),
                                                 actionButton(inputId = "dataretrieval", label = "Retrieve Streamflow Data")
                                               ),
                                               
                                               # Output display
                                               mainPanel(
                                                 h4("If succesfully retrieved, the first and last dates of daily streamflow data available will populate below."),
                                                 textOutput(outputId = "result"),
                                                 textOutput(outputId = "firstdate"),
                                                 textOutput(outputId = "lastdate"),
                                                 h4("Once the data is successfully retrieved, you can download an Excel file with the date and daily streamflow (cfs) below."),
                                                 downloadButton("streamflowdownload", "Download Streamflow")
                                               )
                                               
                                               
                                      ), #tabPanel1
                                      
                                      
                                      ####################################################################################################################################
                                      
                                      #Tab 2 - Plotting initial raw streamflow
                                      tabPanel("Gaged Tool Step 2: Display Streamflow",
                                               
                                               sidebarPanel(
                                                 h2("Raw Streamflow Data Analysis"),
                                                 h4("If streamflow was successfully retrieved in Step 1, quantile statistics (in cfs) are given below and displayed on the plot to the right:"),
                                                 textOutput(outputId = "minflow"),
                                                 textOutput(outputId = "twentyfifth"),
                                                 textOutput(outputId = "medianflow"),
                                                 textOutput(outputId = "seventyfifth"),
                                                 textOutput(outputId = "maxflow"),
                                                 downloadButton("downloadfullstreamflowPlot", "Download Current Plot")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 h2("Interactive Streamflow Plot - Raw Streamflow Data"),
                                                 h4("The current y limits of the plot are given by dashed red lines."),
                                                 plotOutput("RawTimeSeriesPlot"),
                                                 sliderInput("XLimits", "Change X Limits:", min = as.Date("01-01-1900","%m-%d-%y"), max = as.Date("12-31-2022","%m-%d-%y"), value=c(as.Date("01-01-1900","%m-%d-%y"),as.Date("12-31-2022","%m-%d-%y")),timeFormat="%m-%d-%y", width = '100%'),
                                                 sliderInput("YLimits", "Change Y Limits:", min = 0, max = 10000, value = c(0, 10000), step = 100, width = '100%')
                                               ) # mainPanel
                                               
                                      ), #tabpanel2
                                      
                                      
                                      ####################################################################################################################################
                                      
                                      #Tab 3 - Plotting initial raw streamflow
                                      tabPanel("Gaged Tool Step 3: Display Streamflow (Log Scale)",
                                               
                                               sidebarPanel(
                                                 h2("Streamflow Analysis (Log Y Axis)"),
                                                 h5("Here, we plot streamflows again but with a log y scale."),
                                                 h5("This is best for gages with large maximum flows that distort the y axis on regular streamflow plots."),
                                                 textOutput(outputId = "minflowlog"),
                                                 textOutput(outputId = "twentyfifthlog"),
                                                 textOutput(outputId = "medianflowlog"),
                                                 textOutput(outputId = "seventyfifthlog"),
                                                 textOutput(outputId = "maxflowlog"),
                                                 downloadButton("downloadfullstreamflowPlotlog", "Download Current Plot")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 h2("Interactive Streamflow Plot - Log Y Scale"),
                                                 plotOutput("RawTimeSeriesPlotlog"),
                                                 sliderInput("XLimitslog", "Change X Limits:", min = as.Date("01-01-1900","%m-%d-%y"), max = as.Date("12-31-2022","%m-%d-%y"), value=c(as.Date("01-01-1900","%m-%d-%y"),as.Date("12-31-2022","%m-%d-%y")),timeFormat="%m-%d-%y", width = '100%'),
                                               ) # mainPanel
                                               
                                      ), #tabpanel2
                                      
                                      
                                      ####################################################################################################################################
                                      
                                      #Tab 4 - Plotting lowest flows
                                      tabPanel("Gaged Tool Step 4: Mann-Kendall Test - Annual Peak Flows",
                                               
                                               sidebarPanel(
                                                 h2("Annual Peak Flows"),
                                                 h4("Here, we plot the annual water year peak flows and check for any trends using the Mann-Kendall Trend Test."),
                                                 h5("See below for the results of the trend test:"),
                                                 textOutput("mktau"),
                                                 textOutput("mkpvalue"),
                                                 downloadButton("downloadmannkendallplot", "Download Plot")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 h2("Annual Peak Flows Plot with Lowess Smoothing Line"),
                                                 plotOutput("AnnualPeakFlowPlotwithLowess"),
                                               ) # mainPanel
                                               
                                      ), #tabpanel3
                                      
                                      ####################################################################################################################################
                                      
                                      #Tab 5 - Plotting watershed and getting physical characteristics
                                      tabPanel("Gaged Tool Step 5: Annual Peak Flows & Flood Metrics",
                                               # Input form
                                               sidebarPanel(
                                                 h2("Annual Peak Flows Plot & Analysis"),
                                                 h4("Here, we plot the annual peak flows with a traditional Log-Pearson Type 3 analysis to calculate the 25, 50, and 100 year floods"),
                                                 textOutput(outputId = "maxannualflow"),
                                                 h6("----------------------------------------------------------"),
                                                 textOutput(outputId = "lp3extrapolated25yf"),
                                                 textOutput(outputId = "lp3extrapolated50yf"),
                                                 textOutput(outputId = "lp3extrapolated100yf"),
                                                 h6("----------------------------------------------------------"),
                                                 downloadButton("logpearson3analysisplot", "Download Current Plot")
                                               ),
                                               
                                               mainPanel(
                                                 h2("Plot of Annual Peak Flows with Log-Pearson Type 3 Distribution"),
                                                 plotOutput("logpearson3plot"),
                                               )# mainPanel
                                               
                                      ), #tabpanel4
                                      
                                      ####################################################################################################################################
                                      
                                      #Tab 5 - Subsetting the Data
                                      tabPanel("Gaged Tool Step 6: Subset Data",
                                               
                                               sidebarPanel(
                                                 h2("Subset Data Tool"),
                                                 h4("Here, we allow you to subset the full streamflow record."),
                                                 h4("Choose a beginning date and an end date to trim the data. You can use the plot in Step 2 to help you select dates."),
                                                 h4("Note- All future calculations will use the subset record from this tab."),
                                                 h6("------------------------------------"),
                                                 h6("------------------------------------"),
                                                 dateInput("lowerbound", "Lower Bound Date:", value = "1992-01-01", format = "mm/dd/yyyy"),
                                                 dateInput("upperbound", "Upper Bound Date:", value = "2022-12-31", format = "mm/dd/yyyy"),
                                                 actionButton(inputId = "savesubset", label = "Save Subset Data?"),
                                                 textOutput(outputId = "savedsubset"),
                                               ),
                                               
                                               mainPanel(
                                                 h2("Choose New Subset"),
                                                 plotOutput("subsetplot"),
                                                 h4("Once you save a subset to the left, you can download the new Excel file below."),
                                                 downloadButton("subsetstreamflowdownload", "Download Subset Streamflow")
                                               )
                                               
                                      ), #tabpanel5
                                      
                                      
                                      ####################################################################################################################################
                                      
                                      #Tab 7 - Plotting lowest flows for subset data
                                      tabPanel("Gaged Tool Step 7: Plot Annual Flows - Subset Data",
                                               
                                               sidebarPanel(
                                                 h2("High Flow Re-Analysis"),
                                                 h4("New Annual Flows for Subset Data (cfs):"),
                                                 textOutput(outputId = "maxannualflowsubset"),
                                                 
                                                 h6("----------------------------------------------------------"),
                                                 
                                                 textOutput(outputId = "minflowrepeat"),
                                                 textOutput(outputId = "lp3extrapolated25yfrepeat"),
                                                 textOutput(outputId = "lp3extrapolated50yfrepeat"),
                                                 textOutput(outputId = "lp3extrapolated100yfrepeat"),
                                                 
                                                 h6("----------------------------------------------------------"),
                                                 
                                                 textOutput(outputId = "lp3extrapolated25yfsubset"),
                                                 textOutput(outputId = "lp3extrapolated50yfsubset"),
                                                 textOutput(outputId = "lp3extrapolated100yfsubset"),
                                                 
                                                 downloadButton("downloadsubsethighflowsplot", "Download Current Plot")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 h2("Interactive Streamflow Plot - Highest 10th Percentile of Flows for Chosen Subset Record"),
                                                 plotOutput("logpearson3plotsubset"),
                                               ) # mainPanel
                                               
                                      ), #tabpanel6
                                      
                                      
                                      ####################################################################################################################################
                                      #Tab 8 - Flow Scaling
                                      tabPanel("Gaged Tool Step 8: Flow Scaling",
                                               
                                               sidebarPanel(
                                                 h2("Area/Area Flow Scaling Tool"),
                                                 h4("Here, you can use a simple area/area ratio to scale up or down the values from before."),
                                                 textOutput(outputId = "gagedrainagearearesult"),
                                                 numericInput(inputId = "ungagedarea", "Enter the up/downstream watershed area (mi^2):", value = NULL),
                                                 actionButton(inputId = "flowscaling", label = "Direct Area/Area Flow Scaling"),
                                                 textOutput(outputId = "scalefactor"),
                                                 textOutput(outputId = "scalenote"),
                                                 h6("----------------------------------------------------------"),
                                                 textOutput(outputId = "scaledlp3extrapolated25yfsubset"),
                                                 textOutput(outputId = "scaledlp3extrapolated50yfsubset"),
                                                 textOutput(outputId = "scaledlp3extrapolated100yfsubset"),
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 h2("Annual Peak Flows for Subset & Scaled Data"),
                                                 plotOutput("Annualpeakflowssubsetandscaled"),
                                                 h4("Once you generate the plot above, you can download it using the button below."),
                                                 downloadButton("flowscaledplotdownload", "Download Flow Scaled Plot")
                                               ) # mainPanel
                                               
                                      ), #tabpanel8
                                      
                           ), #First Menu Navbar
                           
                           ###Ungaged Tabs Start
                           
                           navbarMenu("Ungaged Flow Estimator", icon = icon("bridge-water"),
                                      
                                      ###################################################################################################################################
                                      
                                      #Tab 0 - Explanation of this Menu & Module
                                      tabPanel("Explanation of this Module",
                                               
                                               # Input form
                                               sidebarPanel(
                                                 h3("Welcome to the Ungaged High Flow Estimator!"),
                                                 h5("This portion of the program allows the user to estimate the 100-year-flood in ungaged watersheds, similar to the USGS' StreamStats program."),
                                                 h5("It follows the exact same procedure as StreamStats and even uses the StreamStats' API to delineate the watershed and calculate the physical characteristics of the watershed (area, elevation, etc.)."),
                                                 h5("However, the final regression equations used to calculate the 100-year-flood use regionally-developed regression equations, rather than state-by-state equations."),
                                                 h5("The equations are currently being reviewed for publication in the American Meteorological Society's Journal of Artificial Intelligence for Earth Systems."),
                                                 h5("More information about the exact steps and regression equations can be found in the next tab.")
                                               ),
                                               
                                               # Output display
                                               mainPanel(
                                                 h3("What is the 100-Year-Flood?"),
                                                 imageOutput("runoffpredictioninungagedbasins")
                                               )
                                               
                                      ), #tabPanel0   
                                      ####################################################################################################################################
                                      
                                      #Tab 1 - For StreamStats API
                                      tabPanel("Ungaged Tool Step 1: Background Information",
                                               
                                               sidebarPanel(
                                                 h2("Link to StreamStats"),
                                                 h3("As mentioned in the previous tab, if you prefer to use StreamStats' website to retrieve the watershed information, the link is included below."),
                                                 h4("Go to StreamStats' website:"),
                                                 h4("https://streamstats.usgs.gov/ss/")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 h4("This program mimics the regression procedure for calculating extreme flows used by the USGS' StreamStats."),
                                                 h4("We have trained 3 regression equations to calculate the 100-year-flood in the northeast United States."),
                                                 h4("The first equation uses a simple Multiple Linear Regression equation (not recommended)."),
                                                 h4("The second equation uses a multiple linear regression equation trained in log space (the same as StreamStats' methodology)."),
                                                 h4("The third equation uses a machine learning-based regression estimate (using Random Forest decision trees) shown to perform similar to StreamStats' state-by-state estimates."),
                                                 h4("These equations require knowing your ungaged watershed's area*, the average watershed elevation* and slope*, and the percents of the watershed considered wetland* and forest*."),
                                                 h4("If you have calculated or estimated these already, you can proceed to the final tab to estimate the 100-Year-Flood. If you have not, you have two options:"),
                                                 h4("1) You can go to StreamStats' website right now, delineate your basin, estimate the 5 characteristics above, and input them yourself on the final tab."),
                                                 h4("2) If you want to attempt to import them automatically from StreamStats, all you will need is a valid latitude and longitude of a blue pixel from StreamStats' map."),
                                                 h4("Regardless, both of these will require going to StreamStats' website, so the link on the left will take you to StreamStats."),
                                               ) # mainPanel
                                               
                                      ), #tabPanel1
                                      
                                      ###################################################################################################################################
                                      
                                      #Tab 2 - For StreamStats API
                                      tabPanel("Ungaged Tool Step 2: Contact StreamStats",
                                               
                                               sidebarPanel(
                                                 h2("Contacting StreamStats"),
                                                 h5("If you have found a valid latitude and longitude from StreamStats, please enter it below:"),
                                                 numericInput(inputId = "lat", "Latitude:", value = 42.38495),
                                                 numericInput(inputId = "lon", "Longitude:", value = -72.54446),
                                                 
                                                 h5("Attempt to contact StreamStats? Note- this may take a few minutes, so a pop-up will be displayed in the bottom right corner while waiting."),
                                                 actionButton(inputId = "calculate", label = "Calculate"),
                                                 
                                                 textOutput(outputId = "ssresult"),
                                                 textOutput(outputId = "arearesult"),
                                                 textOutput(outputId = "continue")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 leafletOutput("map")
                                               ) # mainPanel
                                               
                                      ), #tabPanel2
                                      
                                      ####################################################################################################################################
                                      
                                      #Tab 3 - Plotting watershed and getting physical characteristics
                                      tabPanel("Ungaged Tool Step 3: Display Watershed & Characteristics", 
                                               
                                               sidebarPanel(
                                                 h2("Display Results"),
                                                 h4("If delineation was successful in the last tab, the watershed itself, all available physical characteristics, and StreamStats' estimated 100-year-flood (if available) are shown here."),
                                                 textOutput(outputId = "watershedarea"),
                                                 textOutput(outputId = "watershedelevation"),
                                                 textOutput(outputId = "watershedslope"),
                                                 textOutput(outputId = "watershedforest"),
                                                 textOutput(outputId = "watershedwetland"),
                                                 textOutput(outputId = "streamstats100yf")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 leafletOutput("watershed")
                                               ) # mainPanel
                                               
                                      ), #tabpanel3
                                      
                                      ###################################################################################################################################
                                      tabPanel("Ungaged Tool Step 4: Regression-Based 7Q10 Estimation", 
                                               
                                               sidebarPanel(
                                                 h2("Estimate 100-Year-Floods"),
                                                 h4("Here, we use pre-trained regression equations to estimate the 100 Year Flood."),
                                                 h4("For more information on the development of these regression equations, please see the documentation."),
                                                 h3("Please enter the physical characteristics below:"),
                                                 numericInput("ungaged100yfarea", "Watershed Area (square mi):", min = 1, max = 1420, value = 100),
                                                 numericInput("ungaged100yfelevation", "Average Watershed Elevation (ft):", min = 10, max = 3337, value = 1000),
                                                 numericInput("ungaged100yfslope", "Average Watershed Slope (Whole number %):", min = 0, max = 32, value = 7),
                                                 numericInput("ungaged100yfforest", "Forest % (Whole Number) of Watershed:", min = 0, max = 100, value = 1),
                                                 numericInput("ungaged100yfwetland", "Wetland % (Whole Number) of Watershed:", min = 0, max = 100, value = 1),
                                                 actionButton(inputId = "regressions", label = "Calculate 100-Year-Flood")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 h3("Physical Characteristics (if StreamStats was succesfully contacted in Step 2):"),
                                                 textOutput(outputId = "watershedarearepeat"),
                                                 textOutput(outputId = "watershedelevationrepeat"),
                                                 textOutput(outputId = "watershedsloperepeat"),
                                                 textOutput(outputId = "watershedforestrepeat"),
                                                 textOutput(outputId = "watershedwetlandrepeat"),
                                                 textOutput(outputId = "streamstats100yfrepeat"),
                                                 
                                                 h3("100-Year-Flood Estimates:"),
                                                 verbatimTextOutput("MLRfull_output"),
                                                 verbatimTextOutput("LTLRfull_output"),
                                                 verbatimTextOutput("RFfull_output"),
                                               ) # mainPanel          
                                               
                                      ), #tabpanel4
                                      
                           ), #Second Navbar Menu
                           
                           #######################################################################################
                           navbarMenu("Apply Climate Change", icon = icon("house-flood-water"),
                                      #Tab 0 - Explanation of this Menu & Module
                                      tabPanel("Explanation of this Module",
                                               
                                               # Input form
                                               sidebarPanel(
                                                 h3("Welcome to the Climate-Altered High Flow Estimator!"),
                                                 h5("This portion of the program allows the user to estimate projected changes in the 100-year-flood given future projected changes in climate."),
                                                 h5("This specific tool uses the well-known Classeus-Clapeyron relationship that assumes for every 1 degree Celsius increase in temperature, an additional 7% more water will be stored in air, amplifying extreme rain events when they do occur."),
                                                 h5("It follows the procedure given in the Transportation Research Board's (TRB's) National Cooperative Highway Research Program's (NCHRP) 15-61: Applying Climate Change Information to Hydrologic and Hydraulic Design of Transportation Infrastructure (Kilgore et al., 2019)."),
                                                 h5("The general procedure uses the regional logarithmic-regression equation with the climate input variables adjusted for future climate projections."),
                                                 h5("More information can be found at: https://apps.trb.org/cmsfeed/TRBNetProjectDisplay.asp?ProjectID=4046")
                                               ),
                                               
                                               # Output display
                                               mainPanel(
                                                 h3("Applying Climate Change Information to Hydrologic and Hydraulic Design of Transportation Infrastructure"),
                                                 imageOutput("nchrppicture")
                                               )         
                                               
                                      ),
                                      
                                      tabPanel("Step 1: Apply Climate Change",
                                               
                                               sidebarPanel(
                                                 h4("Enter your 100-Year-Flood estimate:"),
                                                 numericInput("estimatedvalue", "Estimated 100-Year-Flood (cfs):", min = 1, max = 100000, value = 100),
                                                 h4("Your new projected 100-Year-Flood estimate using the Logarithmic-Regression Equation (cfs):"),
                                                 textOutput(outputId = "LTLRprojectedvalue100yf")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 h2("Projected Changes:"),
                                                 sliderInput("tempslider", "Projected Temperature Change (F):", min = 0, max = 10, value = 5, step = 1, width = '100%'),
                                                 
                                                 h3("Suggested Values for Short-term (2020-2060): "),
                                                 h4("RCP4.5: Temperature = +3F"),
                                                 h4("RCP8.5: Temperature = +4F"),
                                                 h3("Suggested Values for Long-term (2060-2100): "),
                                                 h4("RCP4.5: Temperature = +5F"),
                                                 h4("RCP8.5: Temperature = +10F"),
                                               ) # mainPanel
                                               
                                      ), #tabpanel4
                                      
                           ),#Close Navbar Menu
                           
                           tabPanel("Documentation", icon = icon("download"),
                                    
                                    sidebarPanel(
                                      h2("Download Documentation File"),
                                      h4("Here, you can download the current version's documentation."),
                                      downloadButton("downloadBtn", "Download Document")
                                    ), # sidebarPanel
                                    
                                    mainPanel(
                                      h4("Thank you for using our app!"),
                                      h4("Funding and project information can be found at: https://www.sciencebase.gov/catalog/item/5f2ac7ab82cef313eda0f21b"),
                                      imageOutput("fundingorgs")
                                    ) # mainPanel
                                    
                           ) #tabpanel for Documentation
                           
                           
                ) #Close ui, navbarPage
) # fluidPage