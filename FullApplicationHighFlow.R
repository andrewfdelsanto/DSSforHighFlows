
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

###################################################################################################################################

###################################################################################################################################  
#Begin actual server code

# Define server function  
server <- function(input, output) {
  
  ############################################################################################################################################################Functions
  source("AllFunctionsNeeded.R")
  ####################################################################################################################################################Functions Over
  
  ####Begin actual server stuff
  
  #Load Random Forest data
  load("highflowRdata.RData")

  #First Tab - Documentation and pretty picture
  
  ####Documentation added to first tab too
  output$downloadBtn2 <- downloadHandler(
    filename = "documentation.docx",
    content = function(file) {
      file.copy("documentation.docx", file)
    }
  )
  
  ####Generate and output pretty picture
  output$prettypicture <- renderImage({
    # Get the path to the image file
    img_path <- "Picturefordss.png"
    
    # Return the image object
    list(src = img_path,
         width = "55%",
         height = "auto",
         alt = "Image")
  },
  deleteFile = FALSE) # Keep the image file after the app session ends
  
  ###################################################################
  #Introduction Tab to Gaged Flow Calculator
  
  ####Generate and output pretty picture
  output$bulletin <- renderImage({
    # Get the path to the image file
    img_path <- "Bulletin17C.png"
    
    # Return the image object
    list(src = img_path,
         width = "55%",
         height = "auto",
         alt = "Image")
  },
  deleteFile = FALSE) # Keep the image file after the app session ends
  
  # Download streamflow data when button is clicked
  observeEvent(input$dataretrieval, {
    
    gage <- input$gage
    site_info <- readNWISsite(gage)
    drainage_area <- as.numeric(site_info$drain_area_va)
    
    # Retrieve streamflow data
    parameterCd <- "00060"
    rawDailyData <- readNWISdv(gage,parameterCd,"1900-01-01")
    peakData <- readNWISpeak(gage)
    
    # Output result
    output$result <- renderText("Download was successful!")
    
    firstdatestreamflow <- head(rawDailyData$Date, n=1)
    lastdatestreamflow <- tail(rawDailyData$Date, n=1)
    
    output$firstdate <- renderText({
      paste("The first date of streamflow data available is:", firstdatestreamflow)
    })
    
    output$lastdate <- renderText({
      paste("The last date of streamflow data available is:", lastdatestreamflow)
    })
    
    ###############################Tab2
    
    names(rawDailyData)[names(rawDailyData) == "X_00060_00003"] <- "Value"
    
    DailyFlows <- rawDailyData[c("Date","Value")]
    
    DailyFlows$Value <- as.numeric(DailyFlows$Value)
    
    output$streamflowdownload <- downloadHandler(
      filename = function() {
        "streamflow.csv"
      },
      
      content = function(file) {
        # Write your data generation logic here
        # Replace the following example with your own data
        
        write.csv(DailyFlows, file, row.names = FALSE)
      }
    )
    
    minflow <- min(DailyFlows$Value, na.rm = TRUE)
    maxflow <- max(DailyFlows$Value, na.rm = TRUE)
    quantiles <- quantile(DailyFlows$Value, seq(.25, .75, by = .25), na.rm = TRUE)
    
    # Update the slider input with the maximum y value rounded up to the 1000s
    observe({
      updateSliderInput(
        inputId = "YLimits",
        max = round_any(maxflow, 1000, f = ceiling),
        step = 10,
        value = c(0,round_any(maxflow, 1000, f = ceiling))
      )
    })
    
    observe({
      updateSliderInput(
        inputId = "XLimits",
        min = firstdatestreamflow,
        max = lastdatestreamflow,
        value = c(firstdatestreamflow,lastdatestreamflow)
      )
    })
    
    output$minflow <- renderText({
      paste("The Minimum Flow (dashed blue) is:  ", minflow)
    })
    
    output$twentyfifth <- renderText({
      paste("The 25th Percentile Flow (dashed blue) is:  ", quantiles[1])
    })
    
    output$medianflow <- renderText({
      paste("The Median Flow (dashed blue, slightly thicker) is:  ", quantiles[2])
    })
    
    output$seventyfifth <- renderText({
      paste("The 75th Percentile Flow (dashed blue) is:  ", quantiles[3])
    })
    
    output$maxflow <- renderText({
      paste("The Maximum Flow (dashed blue) is:  ", maxflow)
    })
    
    # Generate the plot
    RawTimeSeriesPlot <- reactive({
      # Generate the plot based on your data and logic
      # Replace the following example with your own plot
      
      ggplot(DailyFlows, aes(Date, Value)) +
        geom_line() +
        labs(x = "Date", y = "Streamflow (cfs)") +
        geom_hline(yintercept = quantiles[1], col = "blue", linetype = "dashed") +
        geom_hline(yintercept = quantiles[2], col = "blue", linetype = "dashed", size = 1) +
        geom_hline(yintercept = quantiles[3], col = "blue", linetype = "dashed") +
        geom_hline(yintercept = minflow, col = "blue", linetype = "dashed") +
        geom_hline(yintercept = maxflow, col = "blue", linetype = "dashed") +
        
        geom_hline(yintercept = input$YLimits[1], col = "red", linetype = "dashed", size = 0.25) +
        geom_hline(yintercept = input$YLimits[2], col = "red", linetype = "dashed", size = 0.25) +
        
        ylim(input$YLimits[1], input$YLimits[2]) +
        xlim(as.Date(input$XLimits[1],"%m-%d-%y"),as.Date(input$XLimits[2],"%m-%d-%y"))
    })
    
    # Render the plot
    output$RawTimeSeriesPlot <- renderPlot({
      plot <- RawTimeSeriesPlot()
      print(plot)
    })
    
    # Download the plot as a JPEG image
    output$downloadfullstreamflowPlot <- downloadHandler(
      filename = function() {
        "fullstreamflowplot.jpg"
      },
      content = function(file) {
        # Save the plot as a JPEG image
        ggsave(file, plot = RawTimeSeriesPlot(), device = "jpeg", width = 12, height = 6)
      }
    )
    
    ###############################Tab3
    
    # Update the slider input with the new xs
    
    observe({
      updateSliderInput(
        inputId = "XLimitslog",
        min = firstdatestreamflow,
        max = lastdatestreamflow,
        value = c(firstdatestreamflow,lastdatestreamflow)
      )
    })
    
    output$minflowlog <- renderText({
      paste("The Minimum Flow (dashed blue) is:  ", minflow)
    })
    
    output$twentyfifthlog <- renderText({
      paste("The 25th Percentile Flow (dashed blue) is:  ", quantiles[1])
    })
    
    output$medianflowlog <- renderText({
      paste("The Median Flow (dashed blue, slightly thicker) is:  ", quantiles[2])
    })
    
    output$seventyfifthlog <- renderText({
      paste("The 75th Percentile Flow (dashed blue) is:  ", quantiles[3])
    })
    
    output$maxflowlog <- renderText({
      paste("The Maximum Flow (dashed blue) is:  ", maxflow)
    })
    
    # Generate the plot
    RawTimeSeriesPlotlog <- reactive({
      # Generate the plot based on your data and logic
      # Replace the following example with your own plot
      
      sp <- {ggplot(DailyFlows, aes(Date, Value)) +
          geom_line() +
          labs(x = "Date", y = "Streamflow (cfs)") +
          geom_hline(yintercept = quantiles[1], col = "blue", linetype = "dashed") +
          geom_hline(yintercept = quantiles[2], col = "blue", linetype = "dashed", size = 1) +
          geom_hline(yintercept = quantiles[3], col = "blue", linetype = "dashed") +
          geom_hline(yintercept = minflow, col = "blue", linetype = "dashed") +
          geom_hline(yintercept = maxflow, col = "blue", linetype = "dashed") +
          
          xlim(as.Date(input$XLimitslog[1],"%m-%d-%y"),as.Date(input$XLimitslog[2],"%m-%d-%y"))}
      ylim(0, round_any(maxflow, 1000, f = ceiling))
      
      sp + scale_y_log10()
    })
    
    # Render the plot
    output$RawTimeSeriesPlotlog <- renderPlot({
      plot <- RawTimeSeriesPlotlog()
      print(plot)
    })
    
    # Download the plot as a JPEG image
    output$downloadfullstreamflowPlotlog <- downloadHandler(
      filename = function() {
        "fullstreamflowplotlogy.jpg"
      },
      content = function(file) {
        # Save the plot as a JPEG image
        ggsave(file, plot = RawTimeSeriesPlotlog(), device = "jpeg", width = 12, height = 6)
      }
    )
    
    ##################################################################################################Tab4 - Annual Flows Plot with Mann Kendall
    
    annual_years <- format(peakData$peak_dt,"%Y")
    annual_years <- as.numeric(annual_years)
    
    peaks <- peakData$peak_va
    
    # Function to fix duplicate years
    fix_duplicate_years <- function(years_list) {
      # Find indices of duplicate years
      dup_indices <- duplicated(years_list)
      
      # Loop through each index except the last one
      for (i in seq_along(dup_indices)[-length(dup_indices)]) {
        if (dup_indices[i]) {
          # If duplicate, increment the year by 1
          years_list[i] <- years_list[i] + 1
        }
      }
      
      # Check if the last year is a duplicate
      if (years_list[length(years_list)] == years_list[length(years_list) - 1]) {
        # If duplicate, increment the last year by 1
        years_list[length(years_list)] <- years_list[length(years_list)] + 1
      }
      
      return(years_list)
    }
    
    # Apply the function to the sample list
    fixed_annualyears <- fix_duplicate_years(annual_years)
    
    annualpeakflowsfixed <- data.frame(fixed_annualyears, peaks)
    
    MKTest <- MannKendall(peaks)
    tau <- round(MKTest$tau, digits = 2)
    pvalue <- round(MKTest$sl, digits = 2)
    
    output$mktau <- renderText({
      paste("The Test Statistic Tau is:", tau)
    })
    
    output$mkpvalue <- renderText({
      paste("P-Value for the 2 Sided Mann-Kendall Test:", pvalue)
    })
    
    # Generate the plot
    AnnualPeakFlowPlot <- reactive({
      # Generate the plot based on your data and logic
      # Replace the following example with your own plot
      
      ggplot(annualpeakflowsfixed, aes(x = fixed_annualyears, y = peaks)) +
        geom_point() +
        geom_smooth(method = "loess", color = "blue", se=F, na.rm = TRUE) +
        labs(x = "Water Year", y = "Annual Peak Flow (cfs)")
    })
    
    # Render the plot
    output$AnnualPeakFlowPlotwithLowess <- renderPlot({
      plot <- AnnualPeakFlowPlot()
      print(plot)
    })
    
    # Download the plot as a JPEG image
    output$downloadmannkendallplot <- downloadHandler(
      filename = function() {
        "MannKendallPlot.jpg"
      },
      content = function(file) {
        # Save the plot as a JPEG image
        ggsave(file, plot = AnnualPeakFlowPlot(), device = "jpeg", width = 12, height = 6)
      }
    )
    
    ##################################################################################################Tab5 - Annual Flows Plot with Log-Pearson Type 3
    
    savedlp3plot <- ProbPlotCalculateValues(data_obs = peaks, 
                        dist = "LPea3", 
                        T_rp = c(25, 50, 100), 
                        main_title = "Historic Flood Frequency Plot", 
                        x_lab = "Exceedance Probability", 
                        y_lab = "Flow (cfs)",
                        Pcol = "black",
                        Lwd = 2.5)
    
    names(savedlp3plot) <- c("GOF", "FloodEstimates")
    
    flood25 <- round(as.numeric(savedlp3plot$FloodEstimates[1]), digits = 0)
    flood50 <- round(as.numeric(savedlp3plot$FloodEstimates[2]), digits = 0)
    flood100 <- round(as.numeric(savedlp3plot$FloodEstimates[3]), digits = 0)
    
    floodreturnperiods <- c(25,50,100)
    resultingfloods <- c(flood25,flood50,flood100)
    
    maxannual <- max(peaks)
  
    output$maxannualflow <- renderText({
      paste("The Maximum Annual Flow (in cfs) is:  ", maxannual)
    })
    
    output$lp3extrapolated25yf <- renderText({
      paste("The 25-Year-Flood (in cfs) is: ", flood25)
    })
    
    output$lp3extrapolated50yf <- renderText({
      paste("The 50-Year-Flood (in cfs) is: ", flood50)
    })
    
    output$lp3extrapolated100yf <- renderText({
      paste("The 100-Year-Flood (in cfs) is: ", flood100)
    })
    
    output$thisisjustanotherspace <- renderText({
      paste("---------------------------------")
    })
    
    # Generate the plot
    AnnualPeakLP3Plot <- reactive({
      # Generate the plot based on your data and logic
      # Replace the following example with your own plot
    ProbPlotOutputPlot(data_obs = peaks, dist = "LPea3", T_rp = c(25, 50, 100))
    })
    
    # Render the plot
    output$logpearson3plot <- renderPlot({
      annuallp3plot <- AnnualPeakLP3Plot()
      print(annuallp3plot)
    })
    
    # Download the plot as a JPEG image
    output$logpearson3analysisplot <- downloadHandler(
      filename = function() {
        "logpearsonplot.jpg"
      },
      content = function(file) {
        # Save the plot as a JPEG image
        ggsave(file, plot = savedlp3plotreactive(), device = "jpeg", width = 12, height = 6)
      }
    )
    
    
    ################################################################################Tab6
    
    # Plot
    
    # Generate the plot
    interactivesubsetPlot <- reactive({
        
        ggplot(data = DailyFlows, aes(x = Date, y = Value)) +
        geom_line() +
        labs(x = "Date", y = "Streamflow (cfs)") +
        geom_vline(xintercept = input$lowerbound, color = "blue", linetype = "dashed", size = 2) +
        geom_vline(xintercept = input$upperbound, color = "blue", linetype = "dashed", size = 2)
      
    })
    
    # Render the plot
    output$subsetplot <- renderPlot({
      plot <- interactivesubsetPlot()
      print(plot)
    })
    
    observeEvent(input$savesubset, {
      
      output$savedsubset <- renderText({
        paste("Subset Saved!")
      })
      
      savedlowerbound <- input$lowerbound
      savedupperbound <- input$upperbound
      
      savedlowerboundyear <- format(savedlowerbound,"%Y")
      savedupperboundyear <- format(savedupperbound,"%Y")
      
      SubsetDailyFlows <- DailyFlows[DailyFlows$Date > savedlowerbound & DailyFlows$Date < savedupperbound, ]
      
      SubsetAnnualFlows <- annualpeakflowsfixed[fixed_annualyears > savedlowerboundyear & fixed_annualyears < savedupperboundyear, ]
      subsetpeaks <- SubsetAnnualFlows$peaks
      
      output$subsetstreamflowdownload <- downloadHandler(
        filename = function() {
          "subsetstreamflow.csv"
        },
        
        content = function(file) {
          # Write your data generation logic here
          # Replace the following example with your own data
          
          write.csv(SubsetDailyFlows, file, row.names = FALSE)
        }
      )
      
      ##########################################################################Tab7
      
      savedlp3plotsubset <- ProbPlotWithoutPopup(data_obs = subsetpeaks, 
                               dist = "LPea3", 
                               T_rp = c(25, 50, 100), 
                               main_title = "Historic Flood Frequency Plot", 
                               x_lab = "Exceedance Probability", 
                               y_lab = "Flow (cfs)",
                               Pcol = "black",
                               Lwd = 2.5)
      
      names(savedlp3plotsubset) <- c("GOF", "FloodEstimates")
      
      flood25subset <- round(as.numeric(savedlp3plotsubset$FloodEstimates[1]), digits = 0)
      flood50subset <- round(as.numeric(savedlp3plotsubset$FloodEstimates[2]), digits = 0)
      flood100subset <- round(as.numeric(savedlp3plotsubset$FloodEstimates[3]), digits = 0)
      
      maxannualsubset <- max(subsetpeaks)
      
      output$maxannualflowsubset <- renderText({
        paste("The Subset-Record Maximum Annual Flow (cfs) is:  ", maxannualsubset)
      })
      
      output$lp3extrapolated25yfrepeat <- renderText({
        paste("The 25-Year-Flood from before is:  ", flood25)
      })
      
      output$lp3extrapolated50yfrepeat <- renderText({
        paste("The 50-Year-Flood from before is:  ", flood50)
      })
      
      output$lp3extrapolated100yfrepeat <- renderText({
        paste("The 100-Year-Flood from before is:  ", flood100)
      })
      
      output$lp3extrapolated25yfsubset <- renderText({
        paste("The Subset Record 25-Year-Flood (cfs) is:  ", flood25subset)
      })
      
      output$lp3extrapolated50yfsubset <- renderText({
        paste("The Subset Record 50-Year-Flood (cfs) is:  ", flood50subset)
      })
      
      output$lp3extrapolated100yfsubset <- renderText({
        paste("The Subset Record 100-Year-Flood (cfs) is:  ", flood100subset)
      })
      
      
      # Generate the plot
      # Render the plot
      
      AnnualPeakLP3PlotSubset <- reactive({
        # Generate the plot based on your data and logic
        # Replace the following example with your own plot
        ProbPlotOutputPlot(data_obs = subsetpeaks, dist = "LPea3", T_rp = c(25, 50, 100))
      })
      
      output$logpearson3plotsubset <- renderPlot({
        plot <- AnnualPeakLP3PlotSubset()
        print(plot)
        
      })
      
      # Download the plot as a JPEG image
      output$downloadsubsethighflowsplot <- downloadHandler(
        filename = function() {
          "logpearsonsubsetplot.jpg"
        },
        content = function(file) {
          # Save the plot as a JPEG image
          ggsave(file, plot = savedlp3plotsubsetreactive(), device = "jpeg", width = 12, height = 6)
        }
      )
      
      ##############################Tab8
      
      output$gagedrainagearearesult <- renderPrint({
        cat("The drainage area for this streamgage", gage, "is", drainage_area, "square miles.")
      })
      
      observeEvent(input$flowscaling, {
        
        arearatio <- input$ungagedarea/drainage_area
        arearatio <- round(arearatio, digits = 2)
        
        SubsetandScaledDailyFlows <- SubsetDailyFlows
        SubsetandScaledDailyFlows$Value <- SubsetDailyFlows$Value*arearatio
        
        output$scalefactor <- renderText({
          paste("Area/Area Scale Factor is:", arearatio)
        })
        
        output$scalenote <- renderText({
          paste("Note- Ideal scale factors lie in the following range: 0.5 < X < 1.5")
        })
        
        annualflowssubsetandscaled <- subsetpeaks*arearatio
        
        savedlp3plotsubsetandscaled <- ProbPlotWithoutPopup(data_obs = annualflowssubsetandscaled, 
                                       dist = "LPea3", 
                                       T_rp = c(25, 50, 100), 
                                       main_title = "Historic Flood Frequency Plot", 
                                       x_lab = "Exceedance Probability", 
                                       y_lab = "Flow (cfs)",
                                       Pcol = "black",
                                       Lwd = 2.5)
        
        names(savedlp3plotsubsetandscaled) <- c("GOF", "FloodEstimates")
        
        flood25subsetandscaled <- round(as.numeric(savedlp3plotsubsetandscaled$FloodEstimates[1]), digits = 0)
        flood50subsetandscaled <- round(as.numeric(savedlp3plotsubsetandscaled$FloodEstimates[2]), digits = 0)
        flood100subsetandscaled <- round(as.numeric(savedlp3plotsubsetandscaled$FloodEstimates[3]), digits = 0)
        
        output$scaledlp3extrapolated25yfsubset <- renderText({
          paste("The Subset & Scaled 25-Year-Flood (cfs) is: ", flood25subsetandscaled)
        })
        
        output$scaledlp3extrapolated50yfsubset <- renderText({
          paste("The Subset & Scaled 50-Year-Flood (cfs) is: ", flood50subsetandscaled)
        })
        
        output$scaledlp3extrapolated100yfsubset <- renderText({
          paste("The Subset & Scaled 100-Year-Flood (cfs) is: ", flood100subsetandscaled)
        })
        
        # Generate the plot
        
        # Render the plot
        AnnualPeakLP3PlotSubsetandScaled <- reactive({
          # Generate the plot based on your data and logic
          # Replace the following example with your own plot
          ProbPlotOutputPlot(data_obs = SubsetAnnualFlows$peaks*arearatio, dist = "LPea3", T_rp = c(25, 50, 100))
        })
        
        output$Annualpeakflowssubsetandscaled <- renderPlot({
          plot <- AnnualPeakLP3PlotSubsetandScaled()
          print(plot)
        })
        
        # Download the plot as a JPEG image
        output$flowscaledplotdownload <- downloadHandler(
          filename = function() {
            "flowscaledannualpeakplot.jpg"
          },
          content = function(file) {
            # Save the plot as a JPEG image
            ggsave(file, plot = savedlp3plotsubsetandscaledreactive(), device = "jpeg", width = 12, height = 6)
          }
        )
        
        
      })#Observe Flow Scaling Event
      
      
    })#Observe Subset Event
    
    
  })#Download NWIS Data Event
  
  #####################################################################################################################
  ###Start Ungaged Portion
  
  ####Generate and output runoff prediction
  output$runoffpredictioninungagedbasins <- renderImage({
    # Get the path to the image file
    img_path <- "UngagedPrediction.png"
    
    # Return the image object
    list(src = img_path,
         width = "80%",
         height = "auto",
         alt = "Image")
  },
  deleteFile = FALSE) # Keep the image file after the app session ends
  
  ######################################################################################################################
  #Code for Tab1 of Ungaged
  
  #There was originally code here but it ended up not being needed lol
  
  ###################################################################################################################################
  #Code for Tab2
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -71.5, lat = 42.5, zoom = 5) %>%
      addMarkers(lng = input$lon, lat = input$lat) # add marker at user input location
  })
  
  ss100yf <- NULL
  ssarea <- NULL
  sselevation <- NULL
  ssslope <- NULL
  ssforest <- NULL
  sswetland <- NULL
  
  observeEvent(input$calculate, {
    
    setTimeout(100000)
    
    withProgress(message = "Contacting StreamStats...", value = 0, {
      ws <- delineateWatershed(xlocation = input$lon, ylocation = input$lat, crs = 4326, includeparameters = "true")
      output$message <- renderText("Website contacted successfully!")
    })
    
    #Extract landparameters from watershed object and display
    parameters <- ws$parameters
    
    state <- substr(ws$workspaceID,start=1,stop=2)
    #MA    
    
    if (state == "MA")
    {
      areaindex <- match("DRNAREA", parameters$code)
      elevationindex <- match("ELEV", parameters$code)
      slope10index <- match("BSLDEM10M", parameters$code)
      slope250index <- match("BSLDEM250", parameters$code)
      forestindex <- match("FOREST", parameters$code)
      wetlandindex <- match("WETLAND", parameters$code)
      
      drstindex <- match("DRFTPERSTR", parameters$code)
      maregindex <- match("MAREGION", parameters$code)
      
      ssarea <- parameters$value[areaindex]
      sselevation <- parameters$value[elevationindex]
      ss10slope <- parameters$value[slope10index]
      ss250slope <- parameters$value[slope250index]
      ssslope <- ss10slope
      ssforest <- parameters$value[forestindex]
      sswetland <- parameters$value[wetlandindex]
      
      #ssdrst not returning correctly
      ssdrst <- parameters$value[drstindex]
      ssregion <- parameters$value[maregindex]
      
      #Something wrong with this equation
      ss100yf <- 10^((2.256 + 0.767*log10(ssarea)) + (0.790*0.001*(sselevation/3.28084)) - 1.137*0.01*(sswetland))
      ss100yf <- round(ss100yf, digits = 2)
    }
    
    #RI
    if(state == "RI")
    {
      areaindex <- match("DRNAREA", parameters$code)
      elevationindex <- match("ELEV", parameters$code)
      slopeindex <- match("CSL10_85", parameters$code)
      forestindex <- match("FOREST", parameters$code)
      wetlandindex <- match("WETLAND", parameters$code)
      
      streamdensityindex <- match("STRDEN", parameters$code)
      storagenhdindex <- match("STORNHD", parameters$code)
      
      ssarea <- parameters$value[areaindex]
      sselevation <- parameters$value[elevationindex]
      ssslopeftpermi <- parameters$value[slopeindex]
      ssslope <- (ssslopeftpermi/5280*100)
      ssslope <- round(ssslope, digits = 2)
      ssforest <- parameters$value[forestindex]
      sswetland <- parameters$value[wetlandindex]
      
      #ssdrst not returning correctly
      streamdensity <- parameters$value[streamdensityindex]
      basinstoragepercent <- parameters$value[storagenhdindex]
      
      #Something wrong with this equation
      ss100yf <- 10^(2.623 + (0.852*log10(ssarea)) + (0.792*log10(streamdensity)) - (0.941*log10(basinstoragepercent)))
      ss100yf <- round(ss100yf, digits = 2)
    }
    #CT
    if(state == "CT")
    {
      areaindex <- match("DRNAREA", parameters$code)
      elevationindex <- match("ELEV", parameters$code)
      slopeindex <- match("CSL10_85", parameters$code)
      forestindex <- match("LC11DEV", parameters$code)
      wetlandindex <- match("WETLAND", parameters$code)
      
      max24hourprecipindex <- match("I24H100Y", parameters$code)
      soilpercentindex <- match("SSURGOCCDD", parameters$code)
      
      ssarea <- parameters$value[areaindex]
      sselevation <- parameters$value[elevationindex]
      ssslopeftpermi <- parameters$value[slopeindex]
      ssslope <- (ssslopeftpermi/5280*100)
      ssslope <- round(ssslope, digits = 2)
      ssforest <- parameters$value[forestindex]
      ssforest <- 100-ssforest
      sswetland <- parameters$value[wetlandindex]
      
      ssmax24hourprecip <- parameters$value[max24hourprecipindex]
      sssoilpercent <- parameters$value[soilpercentindex]
      
      ss100yf <- 10^(-0.104) * (ssarea)^(0.789) * 10^(0.238*ssmax24hourprecip) * 10^(0.413*(sssoilpercent+1))
      ss100yf <- round(ss100yf, digits = 2)
    }
    
    #VT
    if(state == "VT")
    {
      areaindex <- match("DRNAREA", parameters$code)
      wetlandindex <- match("LC06STOR", parameters$code)
      
      precipitationindex <- match("PRECPRIS10", parameters$code)
      
      ssarea <- parameters$value[areaindex]
      sswetland <- parameters$value[wetlandindex]
      ssprecip <- parameters$value[precipitationindex]
      
      ss100yf <- (0.251)*(ssarea^0.854)*(sswetland^-0.297)*(ssprecip^1.809)
      ss100yf <- round(ss100yf, digits = 2)
      
    }
    
    #NH
    if(state == "NH")
    {
      areaindex <- match("DRNAREA", parameters$code)
      elevationindex <- match("ELEVMAX", parameters$code)
      slopeindex <- match("BSLDEM30M", parameters$code)
      forestindex <- match("LC11DEV", parameters$code)
      wetlandindex <- match("WETLAND", parameters$code)
      
      precindex <- match("APRAVPRE", parameters$code)
      streamslopeindex <- match("CSL10_85", parameters$code)
      
      ssarea <- parameters$value[areaindex]
      ssslope <- parameters$value[slopeindex]
      
      #maxelev <- parameters$value[elevationindex]
      #estchangeinelev <- ssarea^(1/2)*(ssslope/100)*5280
      #sselevation <- maxelev - (0.5)*estchangeinelev
      
      ssforest <- 100-parameters$value[forestindex]
      sswetland <- parameters$value[wetlandindex]
      
      #ssdrst not returning correctly
      ssprec <- parameters$value[precindex]
      ssstreamslope <- parameters$value[streamslopeindex]
      
      
      #StreamStats
      ss100yf <- 7.13*(ssarea^0.867)*(ssprec^1.98)*(10^(-0.0254*sswetland))*(ssstreamslope^0.198)
      ss100yf <- round(ss100yf, digits = 2)
      
      #Dingman
      #ding7q10 <- (-2.22) + (1.25*log10(ssarea)) + (4*10^-4)*sselevation + 1.49*D
    }
    
    #ME
    if(state == "ME")
    {
      areaindex <- match("DRNAREA", parameters$code)
      elevationindex <- match("ELEV", parameters$code)
      slopeindex <- match("BSLDEM10M", parameters$code)
      forestindex <- match("LC11DEV", parameters$code)
      wetlandindex <- match("STORNWI", parameters$code)
      
      precipindex <- match("I24H100Y", parameters$code)
      
      ssarea <- parameters$value[areaindex]
      sselev <- parameters$value[elevationindex]
      ssslope <- parameters$value[slopeindex]
      ssforest <- 100-parameters$value[forestindex]
      sswetland <- parameters$value[wetlandindex]
      
      #ssdrst not returning correctly
      ssprecip <- parameters$value[precipindex]
      
      #Something wrong with this equation
      ss100yf <- 105.196*(ssarea^0.793)*(10^(-0.0255*sswetland))*(10^(0.0782*ssprecip))
      ss100yf <- round(ss100yf, digits = 2)
    }
    
    #NY
    if(state == "NY")
    {
      areaindex <- match("DRNAREA", parameters$code)
      slopeindex <- match("BSLOPCM", parameters$code)
      forestindex <- match("FOREST", parameters$code)
      wetlandindex <- match("STORAGE", parameters$code)
      
      ssprecipindex <- match("PRECIP", parameters$code)
      sslagindex <- match("LAGFACTOR", parameters$code)
      
      ssarea <- parameters$value[areaindex]
      
      
      ssslopeftpermi <- parameters$value[slopeindex]
      ssslope <- (ssslopeftpermi/5280*100)
      ssslope <- round(ssslope, digits = 2)
      
      ssforest <- parameters$value[forestindex]
      sswetland <- parameters$value[wetlandindex]
      ssprecip <- parameters$value[ssprecipindex]
      sslag <- parameters$value[sslagindex]
      
      #Only region 1 working right now
      ss100yf <- 10300*(ssarea^0.962)*((sswetland+1)^-0.202)*(ssprecip^1.106)*((sslag+1)^-0.520)*((ssforest+80)^-1.638)
      ss100yf <- round(ss100yf, digits = 2)
    }
    
    
    if (is.null(ssarea) == TRUE)
    {
      
      output$ssresult <- renderText({
        paste("We are sorry- contacting StreamStats was unsuccessful.")
      })
      
      output$arearesult <- renderText({
        paste("It is likely because your lat/long did not land on a valid StreamStats pixel. Please return to Step 1 and verify that your lat and long are valid.")
      })
      
      output$continue <- renderText({
        paste("If you are sure your point is valid, please refresh the page and try again. For some reason, StreamStats API only works ~75% of the time on the first try.")
      })
      
    }
    
    else
    {
      
      output$ssresult <- renderText({
        paste("Contacting StreamStats was a success!")
      })
      
      output$arearesult <- renderText({
        paste("Area of your watershed (sq mi) is:", ssarea)
      })
      
      output$continue <- renderText({
        paste("If this is correct, please proceed to Step 3.")
      })
      
    }
    
    ###################################################################################################################################
    #Code for Tab 3
    
    output$watershedarea <- renderText({
      paste("Area (sq mi):", ssarea)
    })
    
    output$watershedelevation <- renderText({
      paste("Elevation (ft):", sselevation)
    })
    
    output$watershedslope <- renderText({
      paste("Slope (%):", ssslope)
    })
    
    output$watershedforest <- renderText({
      paste("Forest %:", ssforest)
    })
    
    output$watershedwetland <- renderText({
      paste("Wetland %:", sswetland)
    })
    
    output$streamstats100yf <- renderText({
      paste("StreamStats' 100-year-flood (cfs):", ss100yf)
    })
    
    output$watershed <- renderLeaflet({
      leafletWatershed(ws)
    }) 
    
    ###################################################################################################################################    
    
    output$watershedarearepeat <- renderText({
      paste("Area (sq mi):", ssarea)
    })
    
    output$watershedelevationrepeat <- renderText({
      paste("Elevation (ft):", sselevation)
    })
    
    output$watershedsloperepeat <- renderText({
      paste("Slope (%):", ssslope)
    })
    
    output$watershedforestrepeat <- renderText({
      paste("Forest %:", ssforest)
    })
    
    output$watershedwetlandrepeat <- renderText({
      paste("Wetland %:", sswetland)
    })
    
    output$streamstats100yfrepeat <- renderText({
      paste("StreamStats' 100 Year Flood (in cfs) is:", ss100yf)
    })
    
  })##observe event  
  
  ungagedavgprecip <- 120
  ungagedavgtemp <- 37.60
  
  observeEvent(input$regressions, {
    
    output$MLRfull_output <- renderText({
      intercept <- -1.019e+05
      areapiece <- input$ungaged100yfarea * 5.721e+01
      wetlandpiece <- input$ungaged100yfwetland * -2.901e+02
      forestpiece <- input$ungaged100yfforest * -1.703e+02
      elevationpiece <- input$ungaged100yfelevation * 1.070e+01
      slopepiece <- input$ungaged100yfslope * 4.833e+02
      
      precippiece <- ungagedavgprecip * 8.164e+01
      temppiece <- ungagedavgtemp * 2.594e+03
      
      result <- intercept + areapiece + wetlandpiece + forestpiece + elevationpiece + slopepiece + precippiece + temppiece
      result <- round(result, digits = 2)
      paste("The full record, multiple linear regression estimated 100-Year-Flood (cfs) is:", result)
    })
    
    output$LTLRfull_output <- renderText({
      intercept <- -26.33990
      areapiece <- log10(input$ungaged100yfarea) * 0.92986
      wetlandpiece <- log10(input$ungaged100yfwetland) * -0.11610
      forestpiece <- log10(input$ungaged100yfforest) * 0.20785
      elevationpiece <- log10(input$ungaged100yfelevation) * 0.44177
      slopepiece <- log10(input$ungaged100yfslope) * -0.01257
      
      precippiece <- log10(ungagedavgprecip) * 1.40462
      temppiece <- log10(ungagedavgtemp) * 15.18936
      
      logresult <- intercept + areapiece + wetlandpiece + forestpiece + elevationpiece + slopepiece + precippiece + temppiece
      result <- 10^logresult
      result <- round(result, digits = 2)
      paste("The full record, logarithmic regression (StreamStats methodology) estimated 100-Year-Flood (cfs) is:", result)
    })
    
    output$RFfull_output <- renderText({
      
      df <- data.frame(matrix(ncol = 7, nrow = 1))
      colnames(df)<-c("Area","forestper","wetlandper","avgelev","slopeper","maxprec","maxtemp")
      df$Area <- input$ungaged100yfarea
      df$forestper <- input$ungaged100yfforest
      df$wetlandper <- input$ungaged100yfwetland
      df$avgelev <- input$ungaged100yfelevation
      df$slopeper <- input$ungaged100yfslope
      df$maxprec <- ungagedavgprecip
      df$maxtemp <- ungagedavgtemp
      
      prediction <- predict(rfmodel,df)
      result <- round(prediction, digits = 2)
      paste("The full record, random forest machine learning estimated 100-Year-Flood (cfs) is:", result)
    })
    ##
    
  })
  
  #########################################################################
  ###Start Applying Climate Change Portion
  
  ####Generate and output runoff prediction
  output$nchrppicture <- renderImage({
    # Get the path to the image file
    img_path <- "nchrp.png"
    
    # Return the image object
    list(src = img_path,
         width = "55%",
         height = "auto",
         alt = "Image")
  },
  deleteFile = FALSE) # Keep the image file after the app session ends
  
  ################################################################
  
  output$LTLRprojectedvalue100yf <- renderText({
    
    currentestimated100yf <- input$estimatedvalue
    inlogspace <- log10(currentestimated100yf)
    
    tempfahrenheit <- input$tempslider
    correctunitstemp <- tempfahrenheit*(5/9)
    
    classeusclapyeronadjustmentfactor <- correctunitstemp*0.07
    
    averageprecip <- ungagedavgprecip
    newprecip <- ungagedavgprecip*(1+classeusclapyeronadjustmentfactor)
    
    precippiecechange <- log10(newprecip) - log10(averageprecip)
    
    precipchange <- precippiecechange * 1.40462
    
    logresult <- inlogspace + precipchange
    result <- 10^logresult
    
    result <- round(result, digits = 2)
    paste("", result)
  })
  
  ########################################################################Documentation Tab

    output$downloadBtn <- downloadHandler(
    filename = "documentation.docx",
    content = function(file) {
      file.copy("documentation.docx", file)
          }
    )
    
    ####Generate and output runoff prediction
    output$fundingorgs <- renderImage({
      # Get the path to the image file
      img_path <- "FundingandPartnerOrganizations.png"
      
      # Return the image object
      list(src = img_path,
           width = "55%",
           height = "auto",
           alt = "Image")
    },
    deleteFile = FALSE) # Keep the image file after the app session ends
  
}

###################################################################################################################################
#Creates Shiny object and runs app
shinyApp(ui = ui, server = server)

