#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# 

# This app was built by Bernardo Cardeal, Licarion Pinto and Aderval Severino Luna from the LEAMS group, located in the Rio de Janeiro's State University (UERJ). 
# You can find more information about the group in https://www.leamsuerj.com/home, or by sending an email to the displayed addresses on the running app.
# 
#
#
#
#------------------------------------------------------------------------------VERSION DH-1.2-----------------------------------------------------------------------------------------------------#

#CHANGE LOG 

# Version 1.0

# - Launch

# Version 1.1

# - New and improved correlation map
# - Boxplot and histogram boxes now have matching sizes and their plots align for better printing capability
# - Settings and Normality tests boxes now have the same sizes
# - Introduction of an internal DEMO (Ceramic dataset - He, Z., Zhang, M., & Zhang, H. (2016). Data-driven research on chemical features of Jingdezhen and Longquan celadon by energy dispersive X-ray fluorescence. Ceramics International, 42(4), 5123-5129.)
#                                                       https://archive.ics.uci.edu/ml/datasets/Chemical+Composition+of+Ceramic+Samples
#                                      
#                                     Uv/Vis Coffee - authors
#                                     Iris dataset
#                                     )
#
# - Automatic importation parameters for dataset demos 
# - Corrected minor english errors
# - Corrected sample class visualization when removing samples from the original dataset


# Version 1.2

# - Corrected Moving average and simple derivative processing
# - Added new ways to visualize the correlation in the data
# - Added de interactive spectral plot feature
# - Added new spectral pretreatment layout
# - Added per class univariate normality tests
# - Corrected some PTW alignement bugs
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

library(IDPmisc)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DataExplorer)
library(SmartEDA)
library(VIM)
library(DT)
library(readxl)
library(prospectr)
library(tibble)
library(foreign)
library(psych)
library(signal)
library(psychTools)
library(ggplot2)
library(plotly)
library(reshape2)
library(mdatools)
library(baseline)
library(nortest)
library(dplyr)
library(BiocGenerics)
library(pcaMethods)
library(missForest)
library(caret)
library(scales)
library(ptw)
library(MVN)
library(mvnormtest)
library(corrplot)
library(GGally)

options(shiny.maxRequestSize=1000*1024^2)
library(BiocManager)
options(repos = BiocManager::repositories())


ui <- dashboardPage(title = "UERJ - Data Handling App",
                    
                    dashboardHeader(title = "UERJ - Data Handling App", titleWidth = "400"),
                    
                    dashboardSidebar(shiny::hr(), sidebarMenu(
                      menuItem(tabName = "importtab", 
                               text = "Import Data", 
                               icon = icon("import", lib = "glyphicon")
                      ),
                      menuItem(tabName = "descriptivetab", 
                               text = "Descriptive Analysis",
                               icon = icon("chevron-right", lib = "glyphicon")
                      ),
                      menuItem(tabName = "imputationtab",
                               text = "Data Imputation",
                               icon = icon("chevron-right", lib = "glyphicon")
                      ),
                      menuItem(tabName = "preprocessingtab", 
                               text = "Pre-Treat Spectral Data", 
                               icon = icon("chevron-right", lib = "glyphicon"),
                               menuSubItem("Spectral Transformation", 
                                           tabName = "spectralpretreattab",
                                           icon = icon("menu-right", lib = "glyphicon")),
                               menuSubItem("Spectral Visualization", 
                                           tabName = "spectralvisutab",
                                           icon = icon("menu-right", lib = "glyphicon"))
                      ),
                      menuItem("Variables Preprocessing", 
                               tabName = "variablespreprotab",
                               icon = icon("chevron-right", lib = "glyphicon")
                      ),
                      menuItem(tabName = "savetab", 
                               text = "Save session",
                               icon = icon("floppy-save", lib = "glyphicon")
                      ),
                      menuItem(tabName = "exporttab", 
                               text = "Export Data", 
                               icon = icon("export", lib = "glyphicon")
                      ),
                      menuItem(tabName = "linkstab", 
                               text = "About the Interface", 
                               icon = icon("info-sign", lib = "glyphicon"),
                               menuSubItem(text = "LEAMS Group",
                                           href = 'https://www.leamsuerj.com/home', 
                                           icon = icon("link", lib = "glyphicon")
                               ),
                               menuSubItem(text = "UERJ Institute of Chemistry", 
                                           href = 'http://www.iq.uerj.br/', 
                                           icon = icon("link", lib = "glyphicon")
                               ),
                               menuSubItem(text = "Our GitHub",
                                           href = 'https://github.com/Leams-UERJ-apps/Chemometrics-Web-Apps', 
                                           icon = icon("link", lib = "glyphicon")
                               ),
                               menuSubItem(tabName = "creditstab", 
                                           text = "Credits", 
                                           icon = icon("user", lib = "glyphicon")
                               ),
                               menuSubItem(tabName = "referencestab",
                                           text = "References", 
                                           icon = icon("list", lib = "glyphicon")
                               )
                      )
                    )
                    ),
                    
                    dashboardBody(
                      
                      tabItems(
                        
                        tabItem(tabName = "importtab", 
                                h2("Import Data"), 
                                shiny::hr(),
                                fluidRow(
                                  box(title=h3("Enter Data Settings"), 
                                      width = 4, 
                                      status = "primary",
                                      fileInput("file", "Data", buttonLabel = "Upload..."),
                                      shiny::hr(),
                                      checkboxInput("isspectra",strong("Check if the data is of spectral type.")),
                                      shiny::hr(),
                                      checkboxInput("classcol", strong(" Check if the first/second column presents sample classes."), F),
                                      conditionalPanel(condition = "input.classcol == true", numericInput("classpos", "Select the class column position: (for .RData files and DEMOS, please ignore)", 2)),
                                      shiny::hr(),
                                      selectizeInput("datatype", "Select data type:", 
                                                     c(".txt"="txt", ".csv"="csv", "Excel"="xlsx",  "Interface Standard"="itfstd", "Previous session"="sas", "DEMOS"="demos","Select file type"=""), 
                                                     selected = ""),
                                      shiny::hr(),
                                      conditionalPanel(condition = "input.datatype == 'demos'",
                                                       selectInput("demodata",
                                                                   "Select a Demo data type:",
                                                                   choices = c("Iris dataset"="irisDEMO","Ceramic dataset"="ceramicDEMO", "Uv/Vis Coffee dataset"="coffeeDEMO"))),
                                      conditionalPanel(condition = "input.datatype == 'txt'||input.datatype == 'csv'||input.datatype == 'csv'||input.datatype == 'xls'|| input.datatype == 'xlsx'",
                                                       checkboxInput("labels","Check if the variables have labels.",  F),
                                                       checkboxInput("namerows", "Check if the first column presents sample names.", F),
                                      ),
                                      
                                      conditionalPanel(condition = "input.datatype == 'txt'||input.datatype == 'csv'||input.datatype == 'csv'",
                                                       selectInput("delim", "Delimiter:",
                                                                   c("Comma"=",", "Semicolon"=";", "Tab" = "\t"), 
                                                                   selected = ","),
                                                       selectInput("dec", "Decimal mark:", 
                                                                   c("Dot"=".", "Comma"=","), 
                                                                   selected = ".")
                                      ),
                                      
                                      conditionalPanel(condition = "input.datatype == 'xls'|| input.datatype == 'xlsx'",
                                                       selectInput("excelsheet", "Select the sheet to be imported:", 
                                                                   choices = "")
                                      ),
                                      
                                      actionButton("preview" , "Preview and Use", class = "btn-block btn-success")
                                      
                                  ),
                                  box(title = h3("Data Overview"), 
                                      width = 8, 
                                      status = "primary", 
                                      
                                      tabsetPanel(
                                        tabPanel("Data Description",
                                                 tabsetPanel(id="dataPreview",
                                                             type = "hidden",
                                                             tabPanelBody("panelnormaldata", DTOutput("summary")),
                                                             tabPanelBody("panelspectrum", plotOutput("spectrumpreview")%>%withSpinner()))
                                        ),
                                        tabPanel("NaN’s values positions", 
                                                 verbatimTextOutput("datanullvalues")),
                                        tabPanel("Duplicated Values", 
                                                 verbatimTextOutput("checkdup")),
                                        tabPanel("Remove Data",
                                                 shiny::hr(),
                                                 selectizeInput("removedatarow", "Select sample(s) to be removed from the dataset:", 
                                                                choices=NULL, 
                                                                multiple=T),
                                                 actionButton("removedatarowBT", "Remove Sample(s)", 
                                                              class = "btn-block btn-danger", 
                                                              width = "25%"),
                                                 shiny::hr(),
                                                 selectizeInput("removedatacol", "Select variable(s) to be removed from the dataset:", 
                                                                choices=NULL,
                                                                multiple=T),
                                                 actionButton("removedatacolBT", "Remove Variable(s)", class = "btn-block btn-danger", width = "25%")
                                        )
                                        
                                        
                                      )
                                  ),
                                ),
                                
                                fluidRow(box(title=h3("Data preview"), width = 12, status = "primary",
                                             shiny::hr(),
                                             textOutput("spectramsn"),
                                             DTOutput("preview1", width = "100%")))
                        ),
                        
                        tabItem(tabName = "descriptivetab",
                                h2("Descriptive Analysis"), 
                                shiny::hr(),
                                tabsetPanel(
                                  
                                  tabPanel("Overview",
                                           fluidRow(box(title = "", 
                                                        width = 12 ,
                                                        DTOutput("datasummary", width = "100%")%>% withSpinner()))
                                  ),
                                  tabPanel("Analysis per variable",
                                           fluidRow(box(title=h3("Settings"),
                                                        height = 280,
                                                        width = 4,
                                                        selectizeInput("DAvariableselect","Select Variable:", 
                                                                       choices = NULL),
                                                        shiny::hr(),
                                                        checkboxInput("DAclassselection", "Check to show samples plot separated by class.")),
                                                    box(title = h3("Normality Tests"),
                                                        #height = 280,
                                                        width = 8,
                                                        conditionalPanel(condition = "input.DAclassselection == true", selectInput("whichclass", "Select the class to calculate tests:", choices = c())),
                                                        verbatimTextOutput("DAnormtests"))
                                           ),
                                           fluidRow(box(title=h3("Boxplot"),
                                                        height = 620,
                                                        plotlyOutput("DAvariableboxplot")%>% withSpinner()),
                                                    box(title=h3("Histogram"),
                                                        plotlyOutput("DAhistogramplot")%>% withSpinner(),
                                                        sliderInput("DAhistogramnumbins","Select the number of bins:", 
                                                                    min = 1, 
                                                                    max = 50, 
                                                                    value = 5)
                                                    )
                                           ),
                                           fluidRow(box(title = h3("Normal Q-Q Plot"), 
                                                        plotOutput("DAqqplot")),
                                                    box(title = h3("Cumulative Distribution"),
                                                        plotOutput("DAcumulativedistributionplot")%>% withSpinner())
                                           )
                                  ),
                                  tabPanel("Multivariate Analysis (beta)",
                                           fluidRow(
                                             box(width=12, 
                                                 title = h3("Normality Tests"),
                                                 shiny::hr(),
                                                 actionButton("multivaranalysisBT", "Run multivariate analysis"),
                                                 shiny::hr(),
                                                 verbatimTextOutput("multiDAtests")
                                             ),
                                             
                                             box(width=12, 
                                                 title = h3("Multivariate Shapiro-Wilk"),
                                                 p("Be careful when using the Shapiro-Wilk multivariate normality test with a singular matrix, such as in cases where there are more variables than samples and there are correlation between some variables. The app may crash when a singular matrix is used. "),
                                                 shiny::hr(),
                                                 actionButton("multivarSWanalysisBT", "Run multivariate SW analysis"),
                                                 shiny::hr(),
                                                 verbatimTextOutput("multiSWDAtests")
                                             )
                                           )
                                  ),
                                  tabPanel("Missing Data Analysis",
                                           fluidRow(box(title = h3("Remove Variables with missing data"), 
                                                        width = 12,
                                                        fluidRow(
                                                          column(
                                                            numericInput("pctNAvaluesRM", "Maximum value of missing data per column (%):", 
                                                                         step = 1,
                                                                         max = 100,
                                                                         min = 0,
                                                                         value = 0), 
                                                            width = 4), 
                                                          column(div(style = "height:20px"),
                                                                 checkboxInput("confirmRMNA", "Are you sure you want to remove those variables?"),
                                                                 width = 4),
                                                          column(div(style = "height:20px"),
                                                                 conditionalPanel(condition = "input.confirmRMNA == true", 
                                                                                  actionButton("rmvNAVARS", "Remove Variables", class = "btn-block btn-danger")), 
                                                                 width = 4)))),
                                           fluidRow(box(title=h3("Per Variable Analysis"), 
                                                        DTOutput("NAdataDAsummary")%>% withSpinner(), 
                                                        width = 12)),
                                           fluidRow(box(title = h3("Data Matrix Map"),
                                                        selectizeInput("removevarmatplotDA", "Select Variables to be removed from plot  (multiple selection allowed):" ,
                                                                       choices = NULL, 
                                                                       multiple = T),
                                                        plotOutput("datamatrixmapDA")%>% withSpinner(), 
                                                        width = 12))
                                  ),
                                  tabPanel("Correlation HeatMap",
                                           fluidRow(box(
                                             selectInput("corrplottype", "Select Correlation Plot Type:", choices = c("Heatmap"="heatmap", "Pairs (be aware of long loading time)"="pairs", "Heatmap with dendogram"="clustheatmap")),
                                             conditionalPanel(condition = "input.corrplottype == 'pairs'", checkboxInput("classheatmap", "Separate by classes?")),
                                             plotOutput("DAcorrHM")%>% withSpinner(),
                                             width = 12)))
                                  
                                )),
                        
                        tabItem(tabName = "imputationtab", 
                                h2("Data Imputation"), 
                                shiny::hr(),
                                
                                tabsetPanel(
                                  
                                  tabPanel("Imputation algorithms",
                                           fluidRow(
                                             box(width = 6, 
                                                 title = h3("Simple substitutions"), 
                                                 status = "success", 
                                                 collapsible = T, 
                                                 collapsed = T , 
                                                 shiny::hr(),
                                                 p("NaN’s - Not a Number - are the missing values."),
                                                 shiny::hr(),
                                                 actionButton("sub0simpBT","Substitute NaN's for 0"), 
                                                 shiny::hr(),
                                                 actionButton("meanIMPBT","Substitute NaN's for variable mean"),
                                                 shiny::hr(),
                                                 actionButton("medianIMPBT","Substitute NaN's for variable median")
                                             ),
                                             
                                             box(title = h3("Random-Forest"), 
                                                 status = "success",
                                                 collapsible = T, 
                                                 collapsed = T , 
                                                 width = 6 ,
                                                 shiny::hr(),
                                                 numericInput("ntreesMissIMP","Choose number the of trees in each forest:",
                                                              value = 100, 
                                                              step = 1),
                                                 numericInput("niterMissIMP","Choose the number of iterations:",
                                                              min = 1,
                                                              value = 10,
                                                              step = 1),
                                                 actionButton("MissIMPBT", "Run Random-Forest")
                                             )
                                           ),
                                           
                                           fluidRow(
                                             box(title = h3("K-Nearest Neighbours"), 
                                                 status = "success", 
                                                 collapsible = T, 
                                                 collapsed = T ,
                                                 width = 6 ,
                                                 shiny::hr(),
                                                 numericInput("knumberKNNIMP","Select the number of K neighbours:",
                                                              value = 5, 
                                                              min = 1, 
                                                              step = 1),
                                                 actionButton("knnIMPBT", "Run kNN imputation")),
                                             
                                             box(title = h3("Single Value Decomposition (SVD)"), 
                                                 status = "success", 
                                                 collapsible = T, 
                                                 collapsed = T , 
                                                 width = 6 ,
                                                 shiny::hr(),
                                                 numericInput("numpcsSVDIMP","Number of components:",
                                                              value = 2, 
                                                              min = 1),
                                                 numericInput("numitSVDIMP", "Number of iterations:", 
                                                              value = 100, 
                                                              min = 1),
                                                 checkboxInput("scaleSVDIMP", "Check to perform Scaling before running"),
                                                 actionButton("SVDIMPBT", "Run SVD Imputation"))
                                           ),
                                           
                                           fluidRow(
                                             box(width = 12, 
                                                 title = h3("Principal Components Analysis Imputation"),
                                                 status = "success", 
                                                 collapsible = T, 
                                                 collapsed = T , 
                                                 shiny::hr(),
                                                 numericInput("numpcsPCAIMP","Number of components:",
                                                              value = 10, 
                                                              min = 1),
                                                 selectInput("methodPCAIMP", "Select the PCA method to be applied:", 
                                                             choices = c("Nipals"="nipals", "Bayesian PCA"="bpca", "Probabilistic PCA"="ppca")),
                                                 checkboxInput("scalePCAIMP", "Check to perform Scaling before running "),
                                                 actionButton("pcaIMPBT", "Run PCA Imputation")
                                             )
                                             
                                           ),
                                           fluidRow(
                                             box(width = 12,
                                                 title = h3("Comparison Plots"),
                                                 shiny::hr(),
                                                 selectizeInput("IMPvariableselect","Select Variable to  compare:", 
                                                                choices = NULL),
                                                 fluidRow(column(plotlyOutput("IMPhistPlot")%>% withSpinner(), 
                                                                 width = 6),
                                                          column(plotlyOutput("IMPboxPlot")%>% withSpinner(),
                                                                 width = 6))
                                             )
                                           )
                                  ),
                                  
                                  tabPanel("Confirm Imputation",
                                           fluidRow(box(h3("Confirm changes"),
                                                        shiny::hr(), 
                                                        width = 12,
                                                        fluidRow(
                                                          column("Once confirmed, the original data will be substituted by the imputed data. If it is necessary to use the original non-imputed data again, the data must be re-uploaded and the process restarted", width = 12)),
                                                        shiny::hr(),
                                                        actionButton("confirmimp", "Confirm changes to data"), 
                                                        status = "success", 
                                                        solidHeader = T))
                                  )
                                )
                                
                        ),
                        
                        tabItem("spectralpretreattab", 
                                h2("Spectral Pre-Treatment"),
                                shiny::hr(),
                                tabsetPanel(
                                  
                                  tabPanel("Pre-treatments",
                                           fluidRow(box(h3("Reset all applied methods"),
                                                        shiny::hr(), 
                                                        width = 12, 
                                                        "To better customize and visualize the plotted spectrum use the 'Spectral Visualization' tab. The image updates automatically with the pre-treatment process.",
                                                        shiny::hr(),
                                                        actionButton("resetpretreat", "Reset all applied methods used prior the confirmation of changes"), 
                                                        status = "danger", 
                                                        solidHeader = T)
                                           ),
                                           
                                           fluidRow(
                                             
                                             box(title= h3("Smoothing and Derivative"), 
                                                 width = 6, 
                                                 status = "success", 
                                                 collapsible = T, 
                                                 collapsed = T , 
                                                 shiny::hr(),
                                                 
                                                 tabBox(
                                                   width = 12,
                                                   tabPanel(
                                                     h5(strong("Savitzky-Golay")),
                                                     h3(""),
                                                     radioButtons("linecolSG", "Apply Savitzky-Golay at:", 
                                                                  inline = T ,
                                                                  choices = c("Samples"="lineSG", "Variables"="colSG")),
                                                     numericInput("difforderSG", "Select the differentiation order:",
                                                                  min = 0, 
                                                                  step = 1, 
                                                                  value = 0),
                                                     numericInput("polyorderSG","Select the polynomial order (greater or equal to the differentiation order):", 
                                                                  min = 0, 
                                                                  step = 1, 
                                                                  value = 1),
                                                     numericInput("windowsizeSG", "Select the window size (must be odd):",
                                                                  min = 3,
                                                                  step = 2,
                                                                  value = 23,
                                                                  max = 301),
                                                     actionButton("usesavgol", "Apply Savitzky-Golay")),
                                                   
                                                   tabPanel(h5(strong("Moving Average")), 
                                                            h3(""),
                                                            numericInput("windowsizeMA", "Select the window size:",
                                                                         min = 3,
                                                                         step = 1,
                                                                         value = 23,
                                                                         max = 301),
                                                            actionButton("useMA", "Apply Moving Average")),
                                                   
                                                   tabPanel(h5(strong("Derivative")),
                                                            h3(""),
                                                            numericInput("difforderDV", "Select the differentiation order:",
                                                                         min = 1,
                                                                         step = 1,
                                                                         value = 1),
                                                            actionButton("useDV", "Apply Derivative"))
                                                 )
                                             ),
                                             
                                             box(title=h3("Scatter Correction"),
                                                 width = 6,
                                                 status = 'success',
                                                 collapsible = T,
                                                 collapsed = T,
                                                 shiny::hr(),
                                                 
                                                 tabBox(
                                                   
                                                   width = 12,
                                                   
                                                   tabPanel(h5(strong('Standard Normal Variate (SNV)')), 
                                                      actionButton("usesnv", "Apply SNV")),
                                             
                                                 
                                                   tabPanel(h5(strong('Multiplicative Scatter Correction (MSC)')),
                                                       checkboxInput("mscusemean", "Use mean", T), 
                                                       conditionalPanel(condition = "input.mscusemean == false",
                                                                        selectizeInput("mscreference", "Select spectrum for MSC reference", 
                                                                                       choices=c())),
                                                       actionButton("usemsc", "Apply MSC"))
                                                   )
                                                 )
                                             ),
                                           
                                           fluidRow(
                                             
                                             box(title= h3("Baseline Correction Methods"),
                                                 width = 6,
                                                 status = "success",
                                                 collapsible = T,
                                                 collapsed = T,
                                                 shiny::hr(),
                                                 
                                                 tabBox(
                                                   width = 12,
                                                   
                                                   tabPanel(h5(strong('Simple Corrections')),
                                                            fluidRow(box(h4('Offset Correction'),
                                                                shiny::hr() ,
                                                                solidHeader = T,
                                                                status = "primary",
                                                                width = 12 ,
                                                                selectInput("typeoffset", "Select factor to subtract from each sample:",
                                                                            choices = c("Minimum"="min", "Maximum"="max")),
                                                                actionButton("useoffset", "Apply Offset Correction"))
                                                             ), 
                                                            
                                                            fluidRow(box(h4('Polynomial Correction'),
                                                                shiny::hr() ,
                                                                solidHeader = T,
                                                                status = "primary",
                                                                width = 12,
                                                                numericInput("degreepolyPC", "Degree of Polynomial:", 
                                                                             min = 1, 
                                                                             value = 2, 
                                                                             step = 1),
                                                                numericInput("ittolPC", "Tolerance between iterations", 
                                                                             value = 0.001,
                                                                             step = 0.001),
                                                                numericInput("maxitPC", "Select the maximum number of iterations:",
                                                                             min = 1, 
                                                                             value = 100),
                                                                actionButton("usePC", "Apply Polynomial Correction"))),
                                                            
                                                            ),
                                                   
                                                   tabPanel(h5(strong('Filter Methods')),
                                                            fluidRow(box(h4('Low-Pass Fast Fourier Transform Filtering'), 
                                                                shiny::hr() ,
                                                                solidHeader = T,
                                                                status = "primary", 
                                                                width = 12 ,
                                                                numericInput("steepLPF", "Steepness of filter curve:", 
                                                                             value = 2),
                                                                numericInput("halfpointLPF", "Half-way point of filter curve",
                                                                             value = 5),
                                                                actionButton("useLPF", "Apply Low-Pass Correction")
                                                            )),
                                                            ),
                                                   tabPanel(h5(strong('Least-Square Based')),
                                                            fluidRow(box(h4("Asymmetric Least Square"),
                                                                shiny::hr(), 
                                                                width = 12,
                                                                status = "primary",
                                                                solidHeader = T,
                                                                numericInput("plambdaALS", "Select power of the penalty parameter (if plambda = 5, lambda = 10^5):", 
                                                                             min = 2, 
                                                                             step = 1,
                                                                             value = 5,
                                                                             max = 15),
                                                                numericInput("assimetryALS", "Select the Asymmetry Ratio:", 
                                                                             min = 0.001, 
                                                                             step = 0.001, 
                                                                             value = 0.01, 
                                                                             max = 0.100),
                                                                numericInput("maxiterALS", "Select the maximum number of iterations:",
                                                                             min = 1, 
                                                                             step = 1, 
                                                                             value = 10),
                                                                actionButton("useALS", "Apply ALS Correction")
                                                            )),
                                                           
                                                            fluidRow(box(h4('Robust Baseline Estimation'), 
                                                                shiny::hr() ,
                                                                solidHeader = T, 
                                                                status = "primary",
                                                                width = 12 ,
                                                                actionButton("useRBE", "Apply Robust Baseline Correction")
                                                            )),
                                                            
                                                            fluidRow(box(h4('Iterative Restricted Least Squares'), 
                                                                shiny::hr() ,
                                                                solidHeader = T,
                                                                status = "primary",
                                                                width = 12 ,
                                                                numericInput("lambda1IRLS", "2nd derivative constraint for primary smoothing:", 
                                                                             min = 1,
                                                                             value = 5,
                                                                             step = 1),
                                                                numericInput("lambda2IRLS", "2nd derivative constraint for secondary smoothing:",
                                                                             min = 1, 
                                                                             value = 9, 
                                                                             step = 1),
                                                                numericInput("maxitIRLS", "Select the maximum number of iterations:", 
                                                                             min = 1, 
                                                                             value = 200),
                                                                actionButton("useIRLS", "Apply restricted least squares correction")))
                                                            )
                                                 ),
                                                
                                                 
                                             ),
                                             
                                             box(title = h3("Normalization"),
                                                 width = 6,
                                                 status = "success",
                                                 o = T, 
                                                 collapsed = T,
                                                 shiny::hr(),
                                                 collapsible = T,
                                                 box(h4('Maximum intensity normalization'), 
                                                     shiny::hr() ,
                                                     solidHeader = T,
                                                     status = "primary",
                                                     width = 12 ,
                                                     actionButton("maxnormBT", "Normalize by maximum value")
                                                 ),
                                                 
                                                 box(h4('Internal standard normalization'), 
                                                     shiny::hr() ,
                                                     solidHeader = T, 
                                                     status = "primary",
                                                     width = 12 ,
                                                     "The file must only contain one column, titled 'Pattern', containing a scalar for each sample. Only '.xls' and '.xlsx' files are accepted.",
                                                     shiny::hr(),
                                                     fileInput("internalpattern", "Input file containing a scalar for each sample."),
                                                     shiny::hr(),
                                                     "Please make sure to wait until the 'Upload complete' shows below the file name",
                                                     shiny::hr(),
                                                     actionButton("internalptBT", "Normalize by maximum value")
                                                 ),
                                             ),
                                             
                                             
                                           ),
                                           
                                           
                                           
                                           fluidRow(
                                             box(h3("Current Spectra"),
                                                 plotlyOutput("currentspectraprepro")%>% withSpinner(),
                                                 width = 12))
                                  ),
                                  
                                  tabPanel("Cut Spectra",
                                           fluidRow(box(h3("Select Min and Max X values"),
                                                        shiny::hr(), 
                                                        width = 12, 
                                                        status = "primary", 
                                                        solidHeader = T,
                                                        radioButtons("selectxvalues", "Select one:", 
                                                                     choices = c("Include selected"="includecolspretreat","Exclude selected"="excludecolspretreat", "Substitute values for outside margin means"="subsNApretreat")),
                                                        conditionalPanel("input.selectxvalues == 'subsNApretreat'", 
                                                                         numericInput("npointscutpretreat", "Number of points before and after interval to calculate mean:", 
                                                                                      value = 10, 
                                                                                      min = 1, 
                                                                                      step = 1)),
                                                        selectizeInput("minxcut", "Minimum X value:", 
                                                                       choices = NULL),
                                                        selectizeInput("maxxcut", "Maximum X value:", 
                                                                       choices = NULL),
                                                        actionButton("cutintervalBT", "Cut interval"))
                                           ),
                                           
                                           fluidRow(box(h3('View Spectra (brush mouse to view selected interval)'),
                                                        shiny::hr(),
                                                        solidHeader = T,
                                                        status = "primary",
                                                        verbatimTextOutput("currentspectrapreproview"),
                                                        plotOutput("currentspectrapreprocut",
                                                                   brush = "currentspectrabrush")%>% withSpinner(),
                                                        width = 12)
                                           )
                                  ),
                                  
                                  tabPanel("Peak Alignment",
                                           fluidRow(box(h3("Parametric Time Warping"),
                                                        shiny::hr(), 
                                                        width = 12,
                                                        status = "primary",
                                                        solidHeader = T,
                                                        checkboxInput("alignusemean", "Use mean",
                                                                      F),
                                                        conditionalPanel(
                                                          condition = "input.alignusemean == false",
                                                          selectizeInput("referencealign", "Select spectrum for align reference:", 
                                                                         choices=c()
                                                          )
                                                        ), 
                                                        shiny::hr(),
                                                        selectInput("selecttypealign", "Select one:", 
                                                                    choices = c("Align whole spectra directly"="alignpeakswhole", "Separate windows manually for alignment"="alignselectedwindows", "Select one window for alignment"="alignonewindow")
                                                        ),
                                                        shiny::hr(),
                                                        conditionalPanel("input.selecttypealign == 'alignselectedwindows'",
                                                                         strong("Select the values based on the X scale represented below. Use integer numbers. Keep in mind that the first (variable 1) and last (last number on the scale below) variables are automatically considered, thus explicit them will cause an error! The values can be separate by commas, semi-colon or space!"),
                                                                         shiny::hr(),
                                                                         strong("Use commas to separate the windows delimiters."),
                                                                         shiny::hr(),
                                                                         textInput("selectwindowsaling", "Type the windows delimiters separated by commas:")),
                                                        
                                                        conditionalPanel("input.selecttypealign == 'alignonewindow'",
                                                                         strong("Select the values based on the X scale represented below, in integer numbers. The 1st variable is '1' and the last is the number of columns of the dataset;"),
                                                                         shiny::hr(),
                                                                         strong("Use commas to separate the window delimiters."),
                                                                         shiny::hr(),
                                                                         textInput("selectonewindowaling", "Type the window delimiter separated by a comma:")),
                                                        
                                                        actionButton("alignBT", "Run alingment"),
                                                        shiny::hr(),
                                                        verbatimTextOutput("showwindowsalign")
                                           )
                                           ),
                                           
                                           fluidRow(
                                             box(h3('View Spectra (brush mouse to view selected interval)'),
                                                 shiny::hr(), 
                                                 solidHeader = T,
                                                 status = "primary",
                                                 verbatimTextOutput("currentspectraalignview"), 
                                                 plotOutput("currentspectraaligncut", 
                                                            brush = "currentspectrabrushalign")
                                                 %>% withSpinner(), 
                                                 width = 12))),
                                  tabPanel("Interactive Comparison Plot",
                                           fluidRow(box(h3("Interactive Plots"),
                                                        shiny::hr(),
                                                        p("Keep in mind that these plots may take several minutes to load, depending on the dataset and computer configuration. It's advisable to not use the interactive plot using the web version of the app. While the plots are loading, please wait before using other functionalities of the app."),
                                                        width = 12,
                                                        shiny::hr(),
                                                        actionButton("interactiveplots", "Run interactive Plots"))
                                                    ),
                                           fluidRow(
                                             box(h3("Current data"),
                                                 status = "primary", 
                                                 solidHeader = T, 
                                                 plotlyOutput("originaldataplotINT")%>% withSpinner(),
                                                 width = 12)
                                           ),
                                           fluidRow(
                                             box(h3("Changed data"),
                                                 status = "success", 
                                                 solidHeader = T, 
                                                 plotlyOutput("changeddataplotINT")%>% withSpinner(),
                                                 width = 12))
                                           ),
                                  
                                  tabPanel("Review and confirm changes",
                                           fluidRow(box(h3("Confirm changes"),
                                                        shiny::hr(),
                                                        width = 12,
                                                        fluidRow(column("After the confirmation the original data will be substituted by the corrected, so be sure before confirm the changes. To reevaluate the original data, it is necessary to re-upload the data and restart the process", 
                                                                        width = 12)
                                                        ),
                                                        shiny::hr(),
                                                        actionButton("confirmpretreat", "Confirm changes to data"), 
                                                        status = "success", 
                                                        solidHeader = T)
                                           ),
                                           fluidRow(
                                             box(h3("Current data"),
                                                 status = "primary", 
                                                 solidHeader = T, 
                                                 plotOutput("originaldataplot")%>% withSpinner(),
                                                 width = 12)
                                           ),
                                           fluidRow(
                                             box(h3("Changed data"),
                                                 status = "success", 
                                                 solidHeader = T, 
                                                 plotOutput("changeddataplot")%>% withSpinner(),
                                                 width = 12))
                                  )
                                  
                                  
                                )),
                        
                        tabItem("spectralvisutab", 
                                h2("Spectral Image Visualization and Download"),
                                shiny::hr(),
                                
                                tabPanel("Adjust and copy final image",
                                         fluidRow(
                                           
                                           box(h3("Graphical Settings"), 
                                               status = "primary", 
                                               solidHeader = T, 
                                               width = 6,
                                               numericInput("linewdpretreatplot", "Line width (if means are shown, select '0' to show only the means):",
                                                            value = 1, 
                                                            min = 0, 
                                                            step = 0.1),
                                               numericInput("heightpretreatplot", "Height (px):", 
                                                            min = 1, 
                                                            max = 10000, 
                                                            value = 800),
                                               numericInput("widthpretreatplot", "Width (px):",
                                                            min = 1, 
                                                            max = 10000, 
                                                            value = 1200),
                                               numericInput("respretreatplot", "Res (PPI):", 
                                                            min = 1, 
                                                            max = 900, 
                                                            value = 100),
                                               radioButtons("plotpretreatclasses" ,"Type of Coloring:",
                                                            inline = T,
                                                            choices = c("Show individually"="indpretreatplot", "Show by class"="classpretreatplot", "Show by class with highlighted means"="classmeanspretreatplot")
                                               )
                                           ),
                                           box(h3("Text Settings"), 
                                               status = "primary",
                                               solidHeader = T, 
                                               width = 6,
                                               fluidRow(column(
                                                 textInput("plotpretreattitle", "Plot Title:", placeholder = "My Plot Title"), 
                                                 width = 4),
                                                 column(numericInput("plotpretreattitlecex", "Title font Cex:", 
                                                                     value = 1.5,
                                                                     step = 0.1),
                                                        width = 4),
                                                 column(numericInput("plotpretreattitlefont", "Title font:",
                                                                     value = 2),
                                                        width = 4)),
                                               fluidRow(column(textInput("plotpretreatsubtitle", "Plot Subtitle:", placeholder = "My Plot Subtitle"),
                                                               width = 4),
                                                        column(numericInput("plotpretreatsubtitlecex", "Subtitle font Cex:", 
                                                                            value = 1.5,
                                                                            step = 0.1),
                                                               width = 4),
                                                        column(numericInput("plotpretreatsubtitlefont", "Subtitle font type:", 
                                                                            value = 2), 
                                                               width = 4)),
                                               fluidRow(column(textInput("plotpretreatxlabel", "Plot X axis label:", placeholder = "X Label"),
                                                               width = 4),
                                                        column(numericInput("plotpretreataxiscex", "Axis font Cex:", 
                                                                            value = 1, 
                                                                            step = 0.1), 
                                                               width = 4)
                                               ),
                                               fluidRow(column(textInput("plotpretreatylabel", "Plot Y axis Label:", 
                                                                         placeholder = "Y Label"),
                                                               width = 4),
                                                        column(numericInput("plotpretreatlabelcex", "Label font Cex:", 
                                                                            value = 1, step = 0.1), 
                                                               width = 4),
                                                        column(numericInput("plotpretreatlabelfont", "Label font type:", 
                                                                            value = 1), 
                                                               width = 4))
                                           )  
                                         ),
                                         
                                         fluidRow(
                                           box(h3("View Plot"),
                                               shiny::hr(),
                                               actionButton("pretreatcopyBT", "Update Image Settings"),
                                               shiny::hr(),
                                               downloadButton("downloadplotBT", "Download in .tiff"), 
                                               width = 12), plotOutput("copyfinalpretreatplot"),
                                           width = 12)
                                )
                        ),
                        
                        tabItem("variablespreprotab", 
                                h2("Variables pre-processing"),
                                shiny::hr(),
                                tabsetPanel(  
                                  
                                  tabPanel("Pre-processing methods",
                                           fluidRow(
                                             box(h3("Reset all applied methods"),
                                                 shiny::hr(),
                                                 width = 12,
                                                 actionButton("resetvalprepro", "Reset all applied methods used prior the confirmation of changes"),
                                                 status = "danger", 
                                                 solidHeader = T)),
                                           
                                           fluidRow(
                                             box(title = h3("Variables Operations"),
                                                 width = 6,
                                                 collapsible = T,
                                                 collapsed = T, 
                                                 status = "success",
                                                 shiny::hr(),
                                                 box(h4('Mean Centering'),
                                                     shiny::hr(),
                                                     solidHeader = T,
                                                     status = "primary",
                                                     width = 12 ,
                                                     actionButton("useMeantrans", "Apply Mean Centering")
                                                 ),
                                                 box(h4('Scale'), 
                                                     shiny::hr() ,
                                                     solidHeader = T, 
                                                     status = "primary",
                                                     width = 12 ,
                                                     actionButton("useScaletrans", "Apply Scaling")
                                                 ),
                                                 box(h4('Autoscale'), 
                                                     shiny::hr() ,
                                                     solidHeader = T, 
                                                     status = "primary",
                                                     width = 12 ,
                                                     actionButton("useautoScaletrans", "Apply autoscaling")
                                                 ),
                                                 box(h4('Normalize by Frobenius Norm'), 
                                                     shiny::hr() ,
                                                     solidHeader = T,
                                                     status = "primary", 
                                                     width = 12,
                                                     p("Doesn't work on variables with missing values"),
                                                     shiny::hr(),
                                                     actionButton("useFROBtrans", "Apply frobenius normalization")
                                                 ),
                                                 box(h4('Logarithmic transformation'),
                                                     shiny::hr() ,
                                                     solidHeader = T, 
                                                     status = "primary",
                                                     width = 12 ,
                                                     actionButton("useLOGtrans", "Apply logarithm")
                                                 ),
                                             ),
                                             box(title = h3("Normality inducing transformations"), 
                                                 width = 6, 
                                                 collapsible = T,
                                                 collapsed = T,
                                                 status = "success",
                                                 shiny::hr(),
                                                 box(h4('Yeo-Johnson power transformation'),
                                                     shiny::hr() ,
                                                     solidHeader = T, 
                                                     status = "primary",
                                                     width = 12 ,
                                                     actionButton("useYEOJtrans", "Apply Yeo-Johnson")
                                                 ),
                                                 box(h4('Box-Cox transformation'),
                                                     shiny::hr() ,
                                                     solidHeader = T, 
                                                     status = "primary",
                                                     width = 12 ,
                                                     actionButton("useBOXCOXtrans", "Apply Box-Cox")
                                                 )
                                             )
                                           ),
                                           
                                           fluidRow(
                                             box(title = h3("Plots"),
                                                 width = 12,
                                                 selectizeInput("PPvariableselect", "Select variable to compare:",
                                                                choices = NULL),
                                                 fluidRow(
                                                   column(6, 
                                                          plotOutput("orgPPboxplot")%>% withSpinner()
                                                   ),
                                                   column(6, 
                                                          plotOutput("newPPboxplot")%>% withSpinner()
                                                   )
                                                 ),
                                                 fluidRow(column(6, 
                                                                 plotOutput("orgPPhistogram")%>% withSpinner()
                                                 ),
                                                 column(6,
                                                        plotOutput("newPPhistogram")%>% withSpinner()
                                                 )
                                                 ),
                                                 fluidRow(column(6,
                                                                 plotOutput("orgPPqqplot")%>% withSpinner()
                                                 ),
                                                 column(6, 
                                                        plotOutput("newPPqqplot")%>% withSpinner()
                                                 )
                                                 )
                                             )
                                           )
                                  ),
                                  tabPanel("Confirm Changes",
                                           fluidRow(
                                             box(h3("Confirm changes"),
                                                 shiny::hr(),
                                                 width = 12,
                                                 fluidRow(
                                                   column("Be sure before confirm the changes. After the confirmation, the original data is only be available through re-uploading and restarting the process", 
                                                          width = 12)
                                                 ),
                                                 shiny::hr(),
                                                 actionButton("confirmtrans", "Confirm changes to data"), 
                                                 status = "success", 
                                                 solidHeader = T)
                                           )
                                  )
                                )
                        ),
                        
                        
                        
                        tabItem("savetab", 
                                h2("Save session"),
                                shiny::hr(),
                                
                                fluidRow(
                                  box(h3("About the saving process"), 
                                      width = 12,
                                      shiny::hr(),
                                      fluidRow(column("The method applied for saving the session is to save all variables in a .RData file. In that file some information about the data uploaded and processed will be found, stored in R language. To retrieve your session, you must load the downloaded file into the 'Import Data' tab, using the 'Previous Session' option. If it is desired, it is possible to open the file directly into R, for more personal usage.",
                                                      width = 12)
                                      ),
                                      shiny::hr(),
                                      fluidRow(column("Here all the unconfirmed changes are stored, and the same unfinished analysis can be restarted at some other time.",
                                                      width = 12)
                                      ),
                                      shiny::hr(),
                                      fluidRow(
                                        column(
                                          textInput("savefilename", "Input a name for the .RData file:",
                                                    value = "Session 01/01/2022"),
                                          width = 12)
                                      ),
                                      shiny::hr(),
                                      fluidRow(
                                        column(
                                          downloadButton("saveglobalvariables", "Save session"),
                                          width = 12
                                        )
                                      )
                                  )
                                ),
                        ),
                        tabItem("exporttab",
                                h2("Export data"),
                                shiny::hr(),
                                
                                fluidRow(
                                  box(h4(""),
                                      width = 12,
                                      status = "danger",
                                      fluidRow(
                                        column(h4("The exported data is the one with confirmed changes. Any changes that are unconfirmed will be ignored in the exported file."),
                                               width = 12)
                                      ),
                                      shiny::hr(),
                                      fluidRow(
                                        column(textInput("exportfilename", "Input the file name:",
                                                         value = "Modified data"),
                                               width = 12)
                                      ),
                                      shiny::hr(),
                                      selectizeInput("datatypeExp", "Select data type",
                                                     c(".txt"=".txt", ".csv"=".csv", "Interface Standard" = "Rdata", "Select file type"=""), selected = ""),
                                      conditionalPanel(
                                        condition =  "input.datatypeExp !== 'Rdata'",
                                        selectInput("delimExp", "Delimiter",
                                                    c("Comma"=",", "Semicolon"=";", "Tab" = "\t"), 
                                                    selected = ","),
                                        selectInput("decExp", "Decimal mark",
                                                    c("Dot"=".", "Comma"=","),
                                                    selected = ".")
                                      ),
                                      shiny::hr(),
                                      fluidRow(
                                        column(
                                          conditionalPanel(
                                            condition = "input.datatypeExp !== 'Rdata'",
                                            downloadButton("exportdata", "Export table")
                                          ), 
                                          width = 12)
                                      ),
                                      fluidRow(
                                        column(
                                          conditionalPanel(
                                            condition = "input.datatypeExp == 'Rdata'", 
                                            downloadButton("exportRdata", "Export Rdata")
                                          ),
                                          width = 12)
                                      )
                                  )
                                ),
                        ),
                        
                        tabItem("creditstab", 
                                h2("Credits"),
                                fluidRow(
                                  box(width = 12, 
                                      h3("Credits"),
                                      shiny::hr(),
                                      p(h4(strong("Bernardo Cardeal Goulart Darzé Santos"))),
                                      p(a(href="http://lattes.cnpq.br/0590620499595344", "http://lattes.cnpq.br/0590620499595344",target="_blank")),
                                      p("bernardocardeal@outlook.com"),
                                      
                                      shiny::hr(),
                                      
                                      p(h4(strong("José Licarion Pinto Segundo Neto, Dsc."))),
                                      p(a(href="http://lattes.cnpq.br/5267552018296169", "http://lattes.cnpq.br/5267552018296169",target="_blank")),
                                      p("licarion@gmail.com"),
                                      
                                      shiny::hr(),
                                      
                                      p(h4(strong("Aderval Severino Luna, PhD"))),
                                      p(a(href="http://lattes.cnpq.br/0294676847895948", "http://lattes.cnpq.br/0294676847895948",target="_blank")),
                                      p("adsluna@gmail.com")
                                  )
                                ),
                                fluidRow(
                                  box(
                                    width = 12,
                                    h3("Acknowledgements"),
                                    fluidRow(
                                      column(
                                        width=12 ,
                                        "The authors are thankful to Conselho Nacional de Desenvolvimento Científico e Tecnológico (CNPq), Coordenação de Aperfeiçoamento de Pessoal de Nível Superior (CAPES) Finance Code 001, Fundação de Amparo à Pesquisa no Rio de Janeiro (FAPERJ) (grant number E-26/201.928/2020), and Universidade do Estado do Rio de Janeiro for their financial suppport. ASL has research scholarship from UERJ (Programa Pró-Ciência), FAPERJ (grant number E-26/202.552/2019), and CNPq (grant number 302465/2018-9)."                                        )) 
                                  )
                                )
                        ),
                        
                        tabItem("referencestab", 
                                h2("Bibliographic references"),
                                fluidRow(
                                  box(
                                    width = 12,
                                    h3("Packages"), 
                                    shiny::hr(),
                                    p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/."),
                                    p("RStudio Team (2022). RStudio: Integrated Development Environment for R. RStudio, PBC, Boston, MA URL http://www.rstudio.com/."),
                                    p("Winston Chang, Joe Cheng, JJ Allaire, Carson Sievert, Barret Schloerke, Yihui Xie, Jeff Allen, Jonathan McPherson, Alan Dipert and Barbara Borges (2021). shiny: Web Application Framework for R. R package version 1.7.1. https://shiny.rstudio.com/"),
                                    p("Winston Chang and Barbara Borges Ribeiro (2021). shinydashboard: Create Dashboards with 'Shiny'. R package version 0.7.2. http://rstudio.github.io/shinydashboard/"),
                                    p("Andras Sali and Dean Attali (2020). shinycssloaders: Add Loading Animations to a 'shiny' Output While It's Recalculating. R package version 1.0.0. https://github.com/daattali/shinycssloaders"),
                                    p("Boxuan Cui (2020). DataExplorer: Automate Data Exploration and Treatment. R package version 0.8.2. http://boxuancui.github.io/DataExplorer/"),
                                    p("Dayanand Ubrangala, Kiran R, Ravi Prasad Kondapalli and Sayan Putatunda (2021). SmartEDA: Summarize and Explore the Data. R package version 0.3.8. https://daya6489.github.io/SmartEDA/"),
                                    p("Alexander Kowarik, Matthias Templ (2016). Imputation with the R Package VIM. Journal of Statistical Software, 74(7), 1-16. doi:10.18637/jss.v074.i07"),
                                    p("Yihui Xie, Joe Cheng and Xianying Tan (2021). DT: A Wrapper of the JavaScript Library 'DataTables'. R package version 0.20. https://github.com/rstudio/DT"),
                                    p("Hadley Wickham and Jennifer Bryan (2019). readxl: Read Excel Files. https://readxl.tidyverse.org,  https://github.com/tidyverse/readxl."),
                                    p("Antoine Stevens and Leornardo Ramirez-Lopez (2021). An introduction to the prospectr package. R package Vignette R package version 0.2.2."),
                                    p("Kirill MÃ¼ller and Hadley Wickham (2021). tibble: Simple Data Frames. https://tibble.tidyverse.org/, https://github.com/tidyverse/tibble."),
                                    p("R Core Team (2020). foreign: Read Data Stored by 'Minitab', 'S', 'SAS', 'SPSS', 'Stata', 'Systat', 'Weka', 'dBase', .... R package version 0.8-81. https://CRAN.R-project.org/package=foreign"),
                                    p("Revelle, W. (2021) psych: Procedures for Personality and Psychological Research, Northwestern University, Evanston, Illinois, USA, https://CRAN.R-project.org/package=psych Version = 2.1.9."),
                                    p("Revelle, W. (2021) psychTools:Tools to Accompany the 'psych' Package for Psychological Research Northwestern University, Evanston, Illinois, USA, https://CRAN.R-project.org/package=psychToolsVersion = 2.1.6."),
                                    p("H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016."),
                                    p("C. Sievert. Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC Florida, 2020."),
                                    p("Hadley Wickham (2007). Reshaping Data with the reshape Package. Journal of Statistical Software, 21(12), 1-20. URL http://www.jstatsoft.org/v21/i12/."),
                                    p("Kucheryavskiy S (2020). 'mdatools - R package for chemometrics.' _Chemometrics and Intelligent Laboratory Systems_, *198*. <URL: https://doi.org/10.1016/j.chemolab.2020.103937>."),
                                    p("Kristian Hovde Liland, Trygve AlmÃ¸y, BjÃ¸rn-Helge Mevik (2010). Optimal Choice of Baseline Correction for Multivariate Calibration of Spectra. Applied Spectroscopy 64, pp. 1007-1016."),
                                    p("Juergen Gross and Uwe Ligges (2015). nortest: Tests for Normality. R package version 1.0-4."),
                                    p("Hadley Wickham, Romain FranÃ§ois, Lionel Henry and Kirill MÃ¼ller (2021). dplyr: A Grammar of Data Manipulation. https://dplyr.tidyverse.org, https://github.com/tidyverse/dplyr."),
                                    p("Orchestrating high-throughput genomic analysis with Bioconductor. W. Huber, V.J. Carey, R. Gentleman, ..., M. Morgan Nature Methods, 2015:12, 115. URL = http://www.nature.com/nmeth/journal/v12/n2/full/nmeth.3252.html"),
                                    p("Stacklies, W., Redestig, H., Scholz, M., Walther, D. and Selbig, J.  pcaMethods -- a Bioconductor package providing PCA methods for incomplete data. Bioinformatics, 2007, 23, 1164-1167"),
                                    p("Daniel J. Stekhoven (2013). missForest: Nonparametric Missing Value Imputation using Random Forest. R package version 1.4. Stekhoven D. J., & Buehlmann, P. (2012). MissForest - non-parametric missing value imputation for mixed-type data. Bioinformatics, 28(1), 112-118."),
                                    p("Max Kuhn (2021). caret: Classification and Regression Training. R package version 6.0-90. https://github.com/topepo/caret/"),
                                    p("Hadley Wickham and Dana Seidel (2020). scales: Scale Functions for Visualization. https://scales.r-lib.org, https://github.com/r-lib/scales."),
                                    p("Bloemberg, T. G. et al. (2010) 'Improved Parametric Time Warping for Proteomics', Chemometrics and Intelligent Laboratory Systems, 104 (1), 65-74"),
                                    p("Korkmaz S, Goksuluk D, Zararsiz G. MVN: An R Package for Assessing Multivariate, Normality. The R Journal. 2014 6(2):151-162."),
                                    p("Slawomir Jarek (2012). mvnormtest: Normality test for multivariate variables. R, package version 0.1-9.")
                                  )
                                )
                        )
                      )
                    )
)
###################################################################################################################################################################################################################

server <- function(input, output) {
  
  #------Global Variables
  
  data<-reactiveVal()
  variables<-reactiveVal()
  id<-reactiveVal()
  pretreatdata<-reactiveVal()
  pretreatvariables<-reactiveVal()
  sampleclasscolour<-reactiveVal()
  sampleclass<-reactiveVal()
  imputedata<-reactiveVal()
  transformdata<-reactiveVal()
  
  
  #------DEMOS
  
  
  
  #----Iris Dataset
  observeEvent(c(input$demodata, input$datatype), {
    req(input$datatype=='demos')
    
    if (input$demodata=='irisDEMO'){
      updateCheckboxInput(inputId = "isspectra", value = F)
      updateCheckboxInput(inputId = "classcol", value = T)
    }
  })
  
  observeEvent(input$preview, {
    
    req(input$datatype=='demos')
    req(input$demodata=='irisDEMO')
    
    data(data.frame(matrix(c(5.1,4.9,4.7,4.6,5.0,5.4,4.6,5.0,4.4,4.9,5.4,4.8,4.8,4.3,5.8,5.7,5.4,5.1,5.7,5.1,5.4,5.1,4.6,5.1,4.8,5.0,5.0,5.2,5.2,4.7,4.8,5.4,5.2,5.5,4.9,5.0,5.5,4.9,4.4,5.1,5.0,4.5,4.4,5.0,5.1,4.8,5.1,4.6,5.3,5.0,7.0,6.4,6.9,5.5,6.5,5.7,6.3,4.9,6.6,5.2,5.0,5.9,6.0,6.1,5.6,6.7,5.6,5.8,6.2,5.6,5.9,6.1,6.3,6.1,6.4,6.6,6.8,6.7,6.0,5.7,5.5,5.5,5.8,6.0,5.4,6.0,6.7,6.3,5.6,5.5,5.5,6.1,5.8,5.0,5.6,5.7,5.7,6.2,5.1,5.7,6.3,5.8,7.1,6.3,6.5,7.6,4.9,7.3,6.7,7.2,6.5,6.4,6.8,5.7,5.8,6.4,6.5,7.7,7.7,6.0,6.9,5.6,7.7,6.3,6.7,7.2,6.2,6.1,6.4,7.2,7.4,7.9,6.4,6.3,6.1,7.7,6.3,6.4,6.0,6.9,6.7,6.9,5.8,6.8,6.7,6.7,6.3,6.5,6.2,5.9,3.5,3.0,3.2,3.1,3.6,3.9,3.4,3.4,2.9,3.1,3.7,3.4,3.0,3.0,4.0,4.4,3.9,3.5,3.8,3.8,3.4,3.7,3.6,3.3,3.4,3.0,3.4,3.5,3.4,3.2,3.1,3.4,4.1,4.2,3.1,3.2,3.5,3.1,3.0,3.4,3.5,2.3,3.2,3.5,3.8,3.0,3.8,3.2,3.7,3.3,3.2,3.2,3.1,2.3,2.8,2.8,3.3,2.4,2.9,2.7,2.0,3.0,2.2,2.9,2.9,3.1,3.0,2.7,2.2,2.5,3.2,2.8,2.5,2.8,2.9,3.0,2.8,3.0,2.9,2.6,2.4,2.4,2.7,2.7,3.0,3.4,3.1,2.3,3.0,2.5,2.6,3.0,2.6,2.3,2.7,3.0,2.9,2.9,2.5,2.8,3.3,2.7,3.0,2.9,3.0,3.0,2.5,2.9,2.5,3.6,3.2,2.7,3.0,2.5,2.8,3.2,3.0,3.8,2.6,2.2,3.2,2.8,2.8,2.7,3.3,3.2,2.8,3.0,2.8,3.0,2.8,3.8,2.8,2.8,2.6,3.0,3.4,3.1,3.0,3.1,3.1,3.1,2.7,3.2,3.3,3.0,2.5,3.0,3.4,3.0,1.4,1.4,1.3,1.5,1.4,1.7,1.4,1.5,1.4,1.5,1.5,1.6,1.4,1.1,1.2,1.5,1.3,1.4,1.7,1.5,1.7,1.5,1.0,1.7,1.9,1.6,1.6,1.5,1.4,1.6,1.6,1.5,1.5,1.4,1.5,1.2,1.3,1.5,1.3,1.5,1.3,1.3,1.3,1.6,1.9,1.4,1.6,1.4,1.5,1.4,4.7,4.5,4.9,4.0,4.6,4.5,4.7,3.3,4.6,3.9,3.5,4.2,4.0,4.7,3.6,4.4,4.5,4.1,4.5,3.9,4.8,4.0,4.9,4.7,4.3,4.4,4.8,5.0,4.5,3.5,3.8,3.7,3.9,5.1,4.5,4.5,4.7,4.4,4.1,4.0,4.4,4.6,4.0,3.3,4.2,4.2,4.2,4.3,3.0,4.1,6.0,5.1,5.9,5.6,5.8,6.6,4.5,6.3,5.8,6.1,5.1,5.3,5.5,5.0,5.1,5.3,5.5,6.7,6.9,5.0,5.7,4.9,6.7,4.9,5.7,6.0,4.8,4.9,5.6,5.8,6.1,6.4,5.6,5.1,5.6,6.1,5.6,5.5,4.8,5.4,5.6,5.1,5.1,5.9,5.7,5.2,5.0,5.2,5.4,5.1,0.2,0.2,0.2,0.2,0.2,0.4,0.3,0.2,0.2,0.1,0.2,0.2,0.1,0.1,0.2,0.4,0.4,0.3,0.3,0.3,0.2,0.4,0.2,0.5,0.2,0.2,0.4,0.2,0.2,0.2,0.2,0.4,0.1,0.2,0.1,0.2,0.2,0.1,0.2,0.2,0.3,0.3,0.2,0.6,0.4,0.3,0.2,0.2,0.2,0.2,1.4,1.5,1.5,1.3,1.5,1.3,1.6,1.0,1.3,1.4,1.0,1.5,1.0,1.4,1.3,1.4,1.5,1.0,1.5,1.1,1.8,1.3,1.5,1.2,1.3,1.4,1.4,1.7,1.5,1.0,1.1,1.0,1.2,1.6,1.5,1.6,1.5,1.3,1.3,1.3,1.2,1.4,1.2,1.0,1.3,1.2,1.3,1.3,1.1,1.3,2.5,1.9,2.1,1.8,2.2,2.1,1.7,1.8,1.8,2.5,2.0,1.9,2.1,2.0,2.4,2.3,1.8,2.2,2.3,1.5,2.3,2.0,2.0,1.8,2.1,1.8,1.8,1.8,2.1,1.6,1.9,2.0,2.2,1.5,1.4,2.3,2.4,1.8,1.8,2.1,2.4,2.3,1.9,2.3,2.5,2.3,1.9,2.0,2.3,1.8
    ),
    nrow = 150, ncol = 4, byrow = F)
    ))
    matrix<-data()
    
    sampleclassX<-c("setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica",'virginica',"virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica"
    )
    sampleclass((sampleclassX))
    
    
    # sampleclasscolourX<-c("100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99"
    # )
    # sampleclasscolour(t(sampleclasscolourX))
    
    
    variablesX<-c("sepal_length",	"sepal_width",	"petal_length",	"petal_width"
    )
    variables(t(variablesX))
    colnames(matrix)<-t(variables())
    
    
    idX<-c('Sample 1','Sample 2','Sample 3','Sample 4','Sample 5','Sample 6','Sample 7','Sample 8','Sample 9','Sample 10','Sample 11','Sample 12','Sample 13','Sample 14','Sample 15','Sample 16','Sample 17','Sample 18','Sample 19','Sample 20','Sample 21','Sample 22','Sample 23','Sample 24','Sample 25','Sample 26','Sample 27','Sample 28','Sample 29','Sample 30','Sample 31','Sample 32','Sample 33','Sample 34','Sample 35','Sample 36','Sample 37','Sample 38','Sample 39','Sample 40','Sample 41','Sample 42','Sample 43','Sample 44','Sample 45','Sample 46','Sample 47','Sample 48','Sample 49','Sample 50','Sample 51','Sample 52','Sample 53','Sample 54','Sample 55','Sample 56','Sample 57','Sample 58','Sample 59','Sample 60','Sample 61','Sample 62','Sample 63','Sample 64','Sample 65','Sample 66','Sample 67','Sample 68','Sample 69','Sample 70','Sample 71','Sample 72','Sample 73','Sample 74','Sample 75','Sample 76','Sample 77','Sample 78','Sample 79','Sample 80','Sample 81','Sample 82','Sample 83','Sample 84','Sample 85','Sample 86','Sample 87','Sample 88','Sample 89','Sample 90','Sample 91','Sample 92','Sample 93','Sample 94','Sample 95','Sample 96','Sample 97','Sample 98','Sample 99','Sample 100','Sample 101','Sample 102','Sample 103','Sample 104','Sample 105','Sample 106','Sample 107','Sample 108','Sample 109','Sample 110','Sample 111','Sample 112','Sample 113','Sample 114','Sample 115','Sample 116','Sample 117','Sample 118','Sample 119','Sample 120','Sample 121','Sample 122','Sample 123','Sample 124','Sample 125','Sample 126','Sample 127','Sample 128','Sample 129','Sample 130','Sample 131','Sample 132','Sample 133','Sample 134','Sample 135','Sample 136','Sample 137','Sample 138','Sample 139','Sample 140','Sample 141','Sample 142','Sample 143','Sample 144','Sample 145','Sample 146','Sample 147','Sample 148','Sample 149','Sample 150'
    )
    id(t(idX))
    rownames(matrix)<-t(id())
    
    data(matrix)
    
    
    pretreatdata(data())
    pretreatvariables(variables())
    imputedata(data())
    transformdata(data())
    
  })
  
  
  #----Ceramic
  observeEvent(c(input$demodata, input$datatype), {
    req(input$datatype=='demos')
    
    if (input$demodata=='ceramicDEMO'){
      updateCheckboxInput(inputId = "isspectra", value = F)
      updateCheckboxInput(inputId = "classcol", value = T)
    }
  })
  
  observeEvent(input$preview, {
    
    req(input$datatype=='demos')
    req(input$demodata=='ceramicDEMO')
    
    data(data.frame(matrix(c(0.62,0.57,0.49,0.89,0.03,0.62,0.45,0.59,0.42,0.56,0.35,0.43,0.76,0.03,0.71,0.25,0.43,0.28,0.25,0.16,0.03,0.24,0.29,0.45,0.29,0.41,0.34,0.71,0.3,0.5,0.31,0.2,0.26,0.86,0.17,0.32,0.39,0.18,0.42,0.29,0.55,0.64,0.14,0.31,0.97,1.46,1.05,0.14,0.37,1.09,1.16,1.01,1.88,0.73,0.68,1.29,1.27,0.28,0.34,0.5,0.51,0.14,0.38,0.25,0.2,0.19,0.69,0.65,0.03,0.61,0.03,0.31,0.25,0.11,0.24,0.15,1,0.66,0.32,0.71,0.4,0.03,0.37,0.34,0.72,0.23,0.14,0.14,0.38,0.47,0.19,0.3,0.36,0.18,0.33,0.45,0.53,0.49,0.23,0.7,0.44,0.26,0.31,0.24,0.47,0.22,0.49,0.34,0.36,0.55,0.33,0.46,0.23,0.25,0.47,0.25,0.37,0.32,0.41,0.49,0.32,0.19,0.53,0.22,0.35,0.18,0.18,0.21,0.27,0.19,0.27,0.28,0.07,0.47,0.23,0.41,1.03,0.5,0.58,0.1,0.58,0.25,0.27,0.32,0.54,0.52,0.97,0.66,0.4,0.5,1.04,0.47,0.53,0.57,0.35,0.78,1.01,0.35,0.54,0.53,0.5,0.32,0.39,0.33,0.52,0.53,0.58,0.57,0.47,1.32,0.47,0.55,0.34,0.24,0.46,0.63,19.61,21.19,18.6,18.01,18.41,18.82,17.65,21.42,23.12,19.86,19.53,19.35,19.45,18.34,24.47,23.07,20.67,20.89,20.79,23.77,25.13,22.81,23.49,25.8,22.26,19.48,22.48,19.49,25,25.15,22.77,24.07,26.48,21.54,21.4,22.34,23.2,23.25,22.09,24.35,21.58,21.31,24.01,23.23,11.42,12.96,13.64,12.42,13.15,13.47,13.83,11.84,12.95,13,12.74,12.83,13.01,14.76,13.76,11.3,13.54,14.15,12.37,14.09,12.83,13.61,13.86,13.81,13.05,12.73,13.97,14.63,12.93,11.33,12.64,13.14,13.7,12.95,12.89,11.61,14.38,13.55,13.56,12.37,12.2,12.99,12.62,14.25,71.99,70.09,74.7,74.19,73.99,73.79,74.99,71.46,67.41,72,72.87,71.21,72.52,73.26,65.2,67.37,70.07,70.77,69.92,66.31,64.58,66.31,67.94,64.42,68.93,71.97,67.8,70.85,65.09,65.37,67.75,66.18,63.88,68.95,71.02,68.86,66.4,67.86,69.03,65.43,69.91,69.34,66.7,67.08,74.41,68.79,69.9,67.24,68.98,68.51,71.37,71.13,67.58,71.01,71.93,68.81,73.11,68.65,65.53,69.9,71.35,68.91,67.7,68.46,72.24,70.06,71.38,70.37,72.28,71.6,67.3,68.22,71.59,75.95,74.08,73.76,70.52,72.16,70.6,71.04,66.59,67.66,72.77,70.7,72.19,71.81,69.16,71.55,4.84,4.98,3.47,4.01,4.33,4.28,3.53,3.47,3.81,4.51,4.62,4.77,3.94,5.11,6.16,5.8,5.27,4.79,5.37,6.05,6.56,5.59,4.46,4.57,4.19,5.03,5.45,5.43,6.17,5.34,4.5,4.66,5.62,5.59,2.73,5.28,6.25,5.37,5.17,6.07,4.61,4.9,5.47,5.63,5.7,4.85,4.46,4.29,5.58,5.97,5.14,3.84,2.98,5.78,6.16,5.8,5.46,3.63,3.57,3.88,4.14,5.1,3.89,4.71,5.03,4.7,4.94,5.91,5.56,5.79,3.56,3.6,5.5,5.87,5.11,4.87,6.74,6.57,4.27,5.31,4.16,5.41,6.54,5.33,6.19,5.25,4.34,4.87,0.31,0.49,0.43,0.27,0.65,0.3,0.7,0.35,0.74,0.25,0.28,1.26,0.58,0.14,0.17,0.18,0.13,0.15,0.18,0.16,0.17,0.2,0.17,0.15,0.12,0.28,0.49,0.27,0.18,0.12,0.18,0.2,0.13,0.16,0.14,0.13,0.21,0.14,0.17,0.13,0.13,0.22,0.23,0.16,5.34,8.88,8.43,12.86,7.91,7.23,5.99,9.4,10.28,6.43,6.1,7.92,4.12,10.46,13.69,11.72,8.21,9.21,12.17,9.81,6.92,9.14,6.71,6.14,5.77,6.96,12.53,11.06,6.99,4.37,5.76,6.03,5.34,4.29,8.7,8.13,12.16,8.91,4.12,8.06,6.06,7.15,11.03,6.43,0.07,0.09,0.06,0.09,0.05,0.04,0.07,0.05,0.16,0.23,0.07,0.04,0.07,0.12,0.09,0.14,0.2,0.13,0.15,0.16,0.07,0.18,0.16,0.19,0.29,0.04,0.14,0.14,0.07,0.1,0.24,0.21,0.15,0.08,0.28,0.12,0.1,0.11,0.07,0.1,0.1,0.14,0.09,0.13,0.05,0.11,0.07,0.06,0.08,0.19,0.08,0.1,0.12,0.1,0.09,0.07,0.13,0.07,0.06,0.06,0.07,0.08,0.07,0.07,0.07,0.07,0.16,0.08,0.07,0.07,0.1,0.06,0.06,0.09,0.08,0.05,0.05,0.14,0.05,0.06,0.07,0.11,0.08,0.06,0.04,0.05,0.05,0.08,1.18,1.12,1.07,1.23,1.19,0.96,1.28,1.2,2.81,1.1,1.05,1.23,1.24,1.74,1.89,1.94,1.77,1.77,1.86,2.05,2.11,3.11,2.15,2.97,2.68,1.53,1.83,1.88,1.83,2.1,2.85,2.98,2.17,1.63,2.73,1.73,2.09,1.92,1.86,2.41,1.86,2.27,2.08,2.18,1.04,1.49,1.22,1.58,1.9,2.05,0.85,1.58,2.61,1.71,1.04,1.97,1.36,0.64,1.07,0.98,0.78,0.91,1.38,1.14,1.18,0.66,0.91,1.27,1.24,0.88,0.97,0.58,1.18,0.96,0.71,0.68,1.11,1.69,1.61,1.58,0.77,2,1.09,1.61,1.27,1.29,1.2,1.05,630,380,420,460,380,350,650,500,340,330,320,420,420,480,430,280,300,230,470,280,400,350,310,240,250,490,310,370,360,400,350,410,370,620,180,300,470,390,510,420,330,420,420,360,550,950,590,960,800,870,1050,520,590,1090,810,680,660,2000,2190,1270,910,1410,1830,1460,950,640,770,1500,1750,1740,1340,1950,1290,1600,620,700,1510,2250,880,2450,870,2970,2560,1250,1700,750,920,800,10,20,20,20,40,20,20,10,40,20,70,0,50,0,40,0,10,60,30,40,20,20,0,60,50,80,10,30,30,30,10,20,40,30,30,0,40,20,50,20,40,40,30,20,20,30,20,80,60,20,60,40,80,40,20,30,10,30,20,30,20,10,40,10,10,20,30,30,20,40,40,40,20,50,40,20,70,50,20,40,0,60,20,10,60,40,40,40,70,80,50,70,90,80,90,70,120,70,40,90,100,100,110,70,80,90,120,80,90,110,80,110,120,110,120,110,80,90,100,120,70,120,80,100,110,120,120,100,110,120,70,90,60,40,90,70,120,50,90,50,90,70,20,40,40,120,70,130,120,50,140,100,50,70,120,110,150,140,70,230,90,200,100,110,140,90,70,150,80,180,80,90,110,100,90,90
    ),
    nrow = 88, ncol = 11, byrow = F)
    ))
    matrix<-data()
    
    sampleclassX<-c("Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze"
    )
    sampleclass((sampleclassX))
    
    
    sampleclasscolourX<-c("100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99"
    )
    sampleclasscolour((sampleclasscolourX))
    
    
    variablesX<-c("Na2O","MgO","Al2O3","SiO2","K2O","CaO","TiO2","Fe2O3","MnO","CuO","ZnO")
    variables(t(variablesX))
    colnames(matrix)<-t(variables())
    
    
    idX<-c("FLQ-1-b","FLQ-2-b","FLQ-3-b","FLQ-4-b","FLQ-5-b","FLQ-6-b","FLQ-7-b","FLQ-8-b","FLQ-9-b","FLQ-10-b","FLQ-11-b","FLQ-12-b","FLQ-13-b","DY-BS-1-b","DY-BS-2-b","DY-BS-3-b","DY-BS-4-b","DY-BS-5-b","DY-BS-6-b","DY-BS-7-b","DY-NS-1-b","DY-NS-2-b","DY-NS-3-b","DY-NS-4-b","DY-NS-5-b","DY-NS-6-b","DY-NS-7-b","DY-NS-8-b","DY-Y-1-b","DY-Y-2-b","DY-Y-3-b","DY-Y-4-b","DY-Y-5-b","DY-Y-6-b","DY-Y-7-b","DY-Y-8-b","DY-Y-9-b","DY-M-1-b","DY-M-2-b","DY-M-3-b","DY-QC-1-b","DY-QC-2-b","DY-QC-3-b","DY-QC-4-b","FLQ-1-g","FLQ-2-g","FLQ-3-g","FLQ-4-g","FLQ-5-g","FLQ-6-g","FLQ-7-g","FLQ-8-g","FLQ-9-g","FLQ-10-g","FLQ-11-g","FLQ-12-g","FLQ-13-g","DY-BS-1-g","DY-BS-2-g","DY-BS-3-g","DY-BS-4-g","DY-BS-5-g","DY-BS-6-g","DY-BS-7-g","DY-NS-1-g","DY-NS-2-g","DY-NS-3-g","DY-NS-4-g","DY-NS-5-g","DY-NS-6-g","DY-NS-7-g","DY-NS-8-g","DY-Y-1-g","DY-Y-2-g","DY-Y-3-g","DY-Y-4-g","DY-Y-5-g","DY-Y-6-g","DY-Y-7-g","DY-Y-8-g","DY-Y-9-g","DY-M-1-g","DY-M-2-g","DY-M-3-g","DY-QC-1-g","DY-QC-2-g","DY-QC-3-g","DY-QC-4-g"
    )
    id(t(idX))
    rownames(matrix)<-t(id())
    
    data(matrix)
    
    
    pretreatdata(data())
    pretreatvariables(variables())
    imputedata(data())
    transformdata(data())
    
  })
  
  
  #--------Coffee
  
  observeEvent(input$demodata, {
    if (input$demodata=='coffeeDEMO'){
      updateCheckboxInput(inputId = "isspectra", value = T)
      updateCheckboxInput(inputId = "classcol", value = T)
    }
  })
  
  
  observeEvent(input$preview, {
    
    req(input$datatype=='demos')
    req(input$demodata=='coffeeDEMO')
    
    updateCheckboxInput(inputId = "isspectra", value = T)
    
    data(data.frame(matrix(c(1.65716004371643,2.1431999206543,2.30397009849548,1.30031001567841,1.54627001285553,2.21359992027283,1.86419999599457,1.44939005374908,1.80877995491028,1.62314999103546,1.99026000499725,1.72994005680084,2.17861008644104,1.29288005828857,1.5009800195694,2.84717011451721,2.48274993896484,2.39531993865967,2.10507011413574,2.16958999633789,2.12119007110596,2.43532991409302,2.03776001930237,2.26879000663757,2.36180996894836,2.65240001678467,2.26901006698608,1.78468000888824,2.11086010932922,2.57463002204895,1.46361005306244,1.89471995830536,2.03759002685547,1.15733003616333,1.37097001075745,1.98488998413086,1.66676998138428,1.29296004772186,1.61327004432678,1.44709002971649,1.775750041008,1.5401599407196,1.93257999420166,1.13378000259399,1.31477999687195,2.52414989471436,2.17828989028931,2.10759997367859,1.85494995117188,1.91709995269775,1.88270998001099,2.15132999420166,1.80304002761841,2.00809001922607,2.08213996887207,2.34843993186951,1.98685002326965,1.57296001911163,1.86054003238678,2.27828001976013,1.32458996772766,1.71501994132996,1.83981001377106,1.04908001422882,1.23711001873016,1.82289004325867,1.53310000896454,1.18203997612,1.47652995586395,1.32465004920959,1.62916994094849,1.40906000137329,1.76349997520447,1.01266002655029,1.17417001724243,2.23839998245239,1.95711994171143,1.88788998126984,1.65586996078491,1.71566998958588,1.68849003314972,1.9291900396347,1.61418998241425,1.7972799539566,1.86214005947113,2.09321999549866,1.77850997447968,1.41570997238159,1.66841995716095,2.02390003204346,1.21580004692078,1.5752500295639,1.69530999660492,0.965582013130188,1.13863003253937,1.69950997829437,1.42952001094818,1.09872996807098,1.37323999404907,1.23343002796173,1.51604998111725,1.30884003639221,1.64067995548248,0.927905023097992,1.07612001895905,1.998379945755,1.75654995441437,1.69173002243042,1.48171997070312,1.54001998901367,1.52151000499725,1.73403000831604,1.447350025177,1.61704003810883,1.66977000236511,1.87202000617981,1.59491002559662,1.2765599489212,1.50107002258301,1.80987000465393,1.14163994789124,1.47943997383118,1.5963100194931,0.907582998275757,1.07071995735168,1.61211001873016,1.35678994655609,1.04186999797821,1.30432999134064,1.17261004447937,1.43623995780945,1.23973000049591,1.55648994445801,0.874164998531342,1.01438999176025,1.81947004795074,1.60625004768372,1.54560005664825,1.35020995140076,1.40610003471375,1.39451003074646,1.58653998374939,1.3218799829483,1.48355996608734,1.52614998817444,1.70643997192383,1.45729994773865,1.17181003093719,1.37821996212006,1.65235996246338,1.0833899974823,1.40670001506805,1.51708996295929,0.858729004859924,1.01627004146576,1.53541004657745,1.28936994075775,0.992810010910034,1.24486994743347,1.11916995048523,1.36526000499725,1.18124997615814,1.48499000072479,0.836740970611572,0.972424983978271,1.70677995681763,1.51250004768372,1.45295000076294,1.26816999912262,1.32088994979858,1.31132996082306,1.48942995071411,1.24050998687744,1.39593994617462,1.43113994598389,1.59573996067047,1.3717600107193,1.10508000850677,1.29978001117706,1.55074000358582,1.02625000476837,1.33580005168915,1.44351994991302,0.809723973274231,0.963981986045837,1.44595003128052,1.21456003189087,0.938601016998291,1.17999005317688,1.05825996398926,1.28505003452301,1.11643004417419,1.40782999992371,0.804751992225647,0.936644017696381,1.65601003170013,1.45799005031586,1.40734994411469,1.22990000247955,1.27542996406555,1.26473999023438,1.43418002128601,1.19719004631042,1.3474600315094,1.38082003593445,1.53840005397797,1.32702004909515,1.06481003761292,1.257159948349,1.50078999996185,0.967621982097626,1.2661600112915,1.36922001838684,0.759730994701385,0.909105002880096,1.34706997871399,1.12992000579834,0.878778994083405,1.1073499917984,0.990642011165619,1.19593000411987,1.04455995559692,1.32366001605988,0.771602988243103,0.899030983448029,1.64846003055573,1.44379997253418,1.39869999885559,1.223140001297,1.26223003864288,1.2446700334549,1.40701997280121,1.1810200214386,1.3277200460434,1.36500000953674,1.51933002471924,1.31691002845764,1.04867005348206,1.2433500289917,1.49171996116638,0.920408010482788,1.20911002159119,1.3077700138092,0.722994029521942,0.868803024291992,1.27907001972198,1.07149004936218,0.836405992507935,1.05748999118805,0.942740023136139,1.13345003128052,0.992842972278595,1.26364004611969,0.748003005981445,0.87296599149704,1.67865002155304,1.45448994636536,1.41385996341705,1.2393000125885,1.27338004112244,1.25069999694824,1.41066002845764,1.19245994091034,1.33571004867554,1.38087999820709,1.53488004207611,1.33212995529175,1.05140995979309,1.25012004375458,1.51296997070312,0.896179020404816,1.175989985466,1.27225005626678,0.706366002559662,0.848994970321655,1.25742995738983,1.05466997623444,0.821762979030609,1.04024994373322,0.92704701423645,1.1156200170517,0.976171970367432,1.24539005756378,0.729393005371094,0.852733016014099,1.73171997070312,1.49911999702454,1.4595400094986,1.27488994598389,1.30533003807068,1.28358995914459,1.44644999504089,1.2248899936676,1.36614000797272,1.43115997314453,1.58540999889374,1.37223994731903,1.081169962883,1.2844500541687,1.55467998981476,0.906661987304688,1.1826000213623,1.28328001499176,0.714451014995575,0.856656014919281,1.2878999710083,1.07837998867035,0.836863994598389,1.05788004398346,0.945971012115479,1.14247000217438,0.993339002132416,1.26936995983124,0.73053902387619,0.853434026241302,1.82625997066498,1.56263995170593,1.52838003635406,1.33054995536804,1.36454999446869,1.34420001506805,1.51591002941132,1.28418004512787,1.42920994758606,1.5171400308609,1.67527997493744,1.43597996234894,1.13187003135681,1.33955001831055,1.61836004257202,0.922483026981354,1.19419002532959,1.29495000839233,0.731274008750916,0.871276021003723,1.33684003353119,1.12093997001648,0.863358020782471,1.08498001098633,0.976611971855164,1.18912994861603,1.02550005912781,1.30288994312286,0.726266980171204,0.848285019397736,1.89271998405457,1.61928999423981,1.58006000518799,1.36940002441406,1.40789997577667,1.39303994178772,1.57583999633789,1.33229994773865,1.47614002227783,1.58855998516083,1.74583995342255,1.48241996765137,1.17540001869202,1.38142001628876,1.6674200296402,0.961769998073578,1.23637998104095,1.34139001369476,0.762413024902344,0.901866972446442,1.40448999404907,1.17954003810883,0.901952981948853,1.12928998470306,1.02304995059967,1.25278997421265,1.07471001148224,1.35652995109558,0.739058971405029,0.861082017421722,1.95033001899719,1.66888999938965,1.62767994403839,1.40503001213074,1.44963002204895,1.44247996807098,1.63921999931335,1.37850999832153,1.52296996116638,1.65520000457764,1.81726002693176,1.52358996868134,1.21695005893707,1.42299997806549,1.7104400396347,0.992411017417908,1.26945996284485,1.37582004070282,0.792488992214203,0.929543972015381,1.46326005458832,1.23002994060516,0.935310006141663,1.16411995887756,1.059730052948,1.30729997158051,1.11564004421234,1.3965300321579,0.749305009841919,0.871272027492523,1.94634997844696,1.66788995265961,1.62218999862671,1.40062999725342,1.45318996906281,1.45204997062683,1.65182995796204,1.3846800327301,1.53292000293732,1.66945004463196,1.83542001247406,1.5181599855423,1.21940004825592,1.4180999994278,1.70280003547668,1.02805995941162,1.31014001369476,1.42251002788544,0.823800981044769,0.958513021469116,1.53504002094269,1.29162001609802,0.976395010948181,1.21340000629425,1.10837996006012,1.37668001651764,1.16751003265381,1.45648002624512,0.761059999465942,0.884036004543304,1.89303004741669,1.63082003593445,1.58078002929688,1.36171996593475,1.42530000209808,1.43078005313873,1.63021004199982,1.35885000228882,1.50441002845764,1.63863003253937,1.79692995548248,1.47696995735168,1.19966995716095,1.38457000255585,1.65654003620148,1.05335998535156,1.33689999580383,1.45609998703003,0.843342006206512,0.974250018596649,1.59336996078491,1.34300005435944,1.00850999355316,1.2533700466156,1.14877998828888,1.43216001987457,1.2079199552536,1.507159948349,0.762884974479675,0.884730994701385,1.78201997280121,1.54410004615784,1.48969995975494,1.28232002258301,1.35476005077362,1.36892998218536,1.5648900270462,1.29056000709534,1.43629002571106,1.5529899597168,1.70451998710632,1.39160001277924,1.14660000801086,1.31093001365662,1.56121003627777,1.06160998344421,1.34519004821777,1.46750998497009,0.847329020500183,0.973796010017395,1.62506997585297,1.36750996112823,1.02241003513336,1.27026998996735,1.16867995262146,1.46124994754791,1.22651994228363,1.53036999702454,0.751792013645172,0.871425986289978,1.62993001937866,1.42209994792938,1.36573004722595,1.17446994781494,1.25473999977112,1.2791600227356,1.45955002307892,1.1942800283432,1.33433997631073,1.433109998703,1.57061994075775,1.2754100561142,1.06781005859375,1.21484994888306,1.42926001548767,1.04778003692627,1.32788002490997,1.45003998279572,0.835447013378143,0.956369996070862,1.62425994873047,1.3701000213623,1.01894998550415,1.26772999763489,1.16656005382538,1.46336996555328,1.22238004207611,1.52996003627777,0.72986102104187,0.845438003540039,1.47283005714417,1.29611003398895,1.23729002475739,1.06403994560242,1.15085005760193,1.18156003952026,1.35054004192352,1.09257996082306,1.23245000839233,1.30934000015259,1.43355000019073,1.15479004383087,0.9845330119133,1.11299002170563,1.29395997524261,1.028480052948,1.30238997936249,1.42510998249054,0.818801999092102,0.934651970863342,1.60771000385284,1.35596001148224,1.00576996803284,1.25134003162384,1.1538200378418,1.44681000709534,1.20916998386383,1.50855004787445,0.706775009632111,0.81870698928833,1.36652004718781,1.21356999874115,1.15321004390717,0.985283017158508,1.0749100446701,1.11300003528595,1.2700799703598,1.02037000656128,1.15749001502991,1.22625994682312,1.33758997917175,1.07740998268127,0.930568993091583,1.04560995101929,1.20069003105164,1.00639998912811,1.26877999305725,1.38970994949341,0.799758970737457,0.913048028945923,1.58106005191803,1.33657002449036,0.988300979137421,1.23117995262146,1.13568997383118,1.42481005191803,1.18441998958588,1.48573005199432,0.684307992458344,0.792475998401642,1.29647994041443,1.15620994567871,1.09635996818542,0.935584008693695,1.02708005905151,1.06581997871399,1.21781003475189,0.973990976810455,1.11208999156952,1.17200005054474,1.27320003509521,1.0237900018692,0.893148005008698,0.998081028461456,1.13734996318817,0.989546000957489,1.24597001075745,1.36406004428864,0.786696970462799,0.897364974021912,1.56900000572205,1.32513999938965,0.976373970508575,1.21774005889893,1.12312996387482,1.41057002544403,1.17367005348206,1.46886003017426,0.66745799779892,0.77233099937439,1.25775003433228,1.12559998035431,1.06591999530792,0.904071986675262,0.996258974075317,1.03826999664307,1.18708002567291,0.946636974811554,1.08083999156952,1.14305996894836,1.23786997795105,0.994741976261139,0.874275028705597,0.969821989536285,1.09879994392395,0.983780026435852,1.23178994655609,1.35406005382538,0.781560003757477,0.8887619972229,1.5629700422287,1.32148003578186,0.971433997154236,1.21177995204926,1.11794996261597,1.40548002719879,1.16814994812012,1.46017003059387,0.656136989593506,0.75840300321579,1.23515999317169,1.11018002033234,1.05025005340576,0.884571015834808,0.979174971580505,1.02389001846313,1.17097997665405,0.932132005691528,1.06436002254486,1.13136994838715,1.2232700586319,0.980937004089355,0.866775989532471,0.957861006259918,1.07814002037048,0.986588001251221,1.22949004173279,1.3546199798584,0.783984005451202,0.891333997249603,1.57765996456146,1.33704996109009,0.978995978832245,1.22442996501923,1.12933003902435,1.42207002639771,1.17726004123688,1.47339999675751,0.651404023170471,0.753304004669189,1.2284699678421,1.1068400144577,1.04539000988007,0.873831987380981,0.973101019859314,1.0200400352478,1.17102003097534,0.927156984806061,1.06136000156403,1.13162994384766,1.22109997272491,0.97638601064682,0.86744898557663,0.951004028320312,1.06679999828339,0.993848025798798,1.2333699464798,1.3600800037384,0.787265002727509,0.892409980297089,1.59921002388,1.35357999801636,0.987563014030457,1.23519003391266,1.14086997509003,1.43871998786926,1.1886899471283,1.48767995834351,0.647190988063812,0.747848987579346,1.22783005237579,1.10467004776001,1.04443001747131,0.868080019950867,0.971839010715485,1.02132999897003,1.17182004451752,0.92662501335144,1.06246995925903,1.1380900144577,1.22493994235992,0.973680973052979,0.870532989501953,0.950308978557587,1.06017994880676,0.993447005748749,1.22511994838715,1.35555994510651,0.783187985420227,0.886218011379242,1.59582996368408,1.35124003887177,0.986064016819,1.23017001152039,1.13882994651794,1.43781995773315,1.1842600107193,1.48370003700256,0.63695502281189,0.736451029777527,1.21658003330231,1.09862005710602,1.03778004646301,0.858264029026031,0.964478015899658,1.01688003540039,1.16908001899719,0.921037018299103,1.05773997306824,1.13521003723145,1.21886003017426,0.966701984405518,0.869916975498199,0.94336199760437,1.04783999919891,0.977084994316101,1.20292997360229,1.33262002468109,0.765567004680634,0.864602029323578,1.56639003753662,1.32771003246307,0.965943992137909,1.20753002166748,1.11825001239777,1.4130300283432,1.16217005252838,1.45553004741669,0.618619978427887,0.714754998683929,1.19132995605469,1.07647001743317,1.01657998561859,0.837224006652832,0.944181978702545,0.997461974620819,1.14617002010345,0.902040004730225,1.0362800359726,1.1153199672699,1.19695997238159,0.94725102186203,0.85461300611496,0.923826992511749,1.02418994903564,0.938889026641846,1.15670001506805,1.27837002277374,0.731831014156342,0.825498998165131,1.49005997180939,1.26733005046844,0.921783983707428,1.15225994586945,1.0660799741745,1.34791004657745,1.10852003097534,1.386549949646,0.588612020015717,0.678637981414795,1.14441001415253,1.03424000740051,0.977081000804901,0.805779993534088,0.909088015556335,0.961371004581451,1.10555994510651,0.86939400434494,0.998335003852844,1.0721800327301,1.15029001235962,0.909657001495361,0.822901010513306,0.888451993465424,0.983947992324829,0.881097018718719,1.08587002754211,1.19919002056122,0.683188021183014,0.770159006118774,1.38920998573303,1.1774799823761,0.858512997627258,1.07267999649048,0.990610003471375,1.251620054245,1.03363001346588,1.28933000564575,0.548416972160339,0.632337987422943,1.07553994655609,0.974044024944305,0.917912006378174,0.758700013160706,0.856604993343353,0.904030025005341,1.03928005695343,0.818180978298187,0.938682973384857,1.00567996501923,1.08016002178192,0.85638701915741,0.774107992649078,0.835596978664398,0.927552998065948,0.803108990192413,0.994634985923767,1.09662997722626,0.621281981468201,0.700568974018097,1.25297999382019,1.06304001808167,0.776127994060516,0.971715986728668,0.894044995307922,1.12917995452881,0.932928025722504,1.16550004482269,0.50008898973465,0.576313972473145,0.989242017269135,0.895909011363983,0.844461977481842,0.700487971305847,0.788896024227142,0.831573009490967,0.954430997371674,0.753026008605957,0.864286005496979,0.919721007347107,0.990360021591187,0.787729024887085,0.709918022155762,0.769640028476715,0.857609987258911,0.714931011199951,0.891220986843109,0.977379977703094,0.550913989543915,0.623049974441528,1.0983099937439,0.931586980819702,0.68266499042511,0.854318976402283,0.784089028835297,0.988972008228302,0.820807993412018,1.02254998683929,0.446871995925903,0.514819979667664,0.890693008899689,0.807363986968994,0.760468006134033,0.636007010936737,0.712137997150421,0.747925996780396,0.857594013214111,0.678179025650024,0.778301000595093,0.820931971073151,0.887512981891632,0.711133003234863,0.63639897108078,0.693364977836609,0.778252005577087,0.619356989860535,0.779862999916077,0.850170016288757,0.477708995342255,0.541545987129211,0.936079025268555,0.792825996875763,0.584034979343414,0.73123300075531,0.667931973934174,0.840296983718872,0.702261984348297,0.87439101934433,0.391934990882874,0.451626986265182,0.785089015960693,0.713398993015289,0.671248018741608,0.566681981086731,0.630325973033905,0.658201992511749,0.752739012241364,0.598783016204834,0.686269998550415,0.715655982494354,0.778191983699799,0.629046022891998,0.557363986968994,0.613120019435883,0.694739997386932,0.525417983531952,0.670053005218506,0.725153028964996,0.406347006559372,0.463001012802124,0.777315974235535,0.658307015895844,0.488572001457214,0.611398994922638,0.554974019527435,0.697206020355225,0.587710022926331,0.729615986347198,0.339067012071609,0.391007989645004,0.681433975696564,0.620316982269287,0.583136975765228,0.498093008995056,0.549359023571014,0.568989992141724,0.648669004440308,0.519979000091553,0.594998002052307,0.610705018043518,0.669911026954651,0.548309028148651,0.479449987411499,0.533208012580872,0.611257016658783,0.438284993171692,0.568327009677887,0.610046982765198,0.341122001409531,0.391364991664886,0.633695006370544,0.535999000072479,0.401510000228882,0.502646028995514,0.452562004327774,0.566601991653442,0.482879996299744,0.597947001457214,0.290899008512497,0.335278987884521,0.585319995880127,0.534103989601135,0.501604974269867,0.43470698595047,0.473648995161057,0.486696004867554,0.552488029003143,0.447338998317719,0.51061999797821,0.514301002025604,0.569530010223389,0.473814994096756,0.40743699669838,0.459975004196167,0.533879995346069
    ),
    
    nrow = 30, ncol = 33, byrow = F)
    ))
    matrix<-data()
    
    sampleclassX<-c("1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","2","2","2","2","2","2","2","2","2","2","2","2","2","2","2"
    )
    sampleclass((sampleclassX))
    
    
    sampleclasscolourX<-c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99
    )
    sampleclasscolour((sampleclasscolourX))
    
    
    
    variablesX<-c(200,204,208,212,216,220,224,228,232,236,240,244,248,252,256,260,264,268,272,276,280,284,288,292,296,300,304,308,312,316,320,324,328
    )
    variables(data.frame((variablesX)))
    colnames(matrix)<-t(variables())
    
    
    idX<-c("Caffeine_1","Caffeine_2","Caffeine_3","Caffeine_4","Caffeine_5","Caffeine_6","Caffeine_7","Caffeine_8","Caffeine_9","Caffeine_10","Caffeine_11","Caffeine_12","Caffeine_13","Caffeine_14","Caffeine_15","Decaffeinated_1","Decaffeinated_2","Decaffeinated_3","Decaffeinated_4","Decaffeinated_5","Decaffeinated_6","Decaffeinated_7","Decaffeinated_8","Decaffeinated_9","Decaffeinated_10","Decaffeinated_11","Decaffeinated_12","Decaffeinated_13","Decaffeinated_14","Decaffeinated_15"
    )
    id(data.frame((idX)))
    rownames(matrix)<-t(id())
    
    data(matrix)
    
    
    pretreatdata(data())
    pretreatvariables(variables())
    imputedata(data())
    transformdata(data())
    
  })
  
  
  #------------------------------------------------------Start importation----------------------------------------------------------------------  
  
  {observeEvent(c(input$datatype, input$file),{
    
    req(input$datatype != "itfstd")
    req(input$datatype != "demos")
    
    
    req(input$file)
    if (input$datatype=='xlsx' || input$datatype == 'xls')
    {updateSelectInput(inputId = "excelsheet", choices = c(""))
      validate(need(grepl(".xls", input$file$datapath)==T, message = "Wrong type of file"))
      updateSelectInput(inputId = "excelsheet", choices = excel_sheets(input$file$datapath))}
  }
  
  )}
  
  {observeEvent(input$datatype,{
    req(input$file)
    if (input$datatype=='txt')
      updateSelectInput(inputId = "delim", selected = '\t')
  }
  )}
  
  
  newdata<-eventReactive(input$preview,{
    
    req(input$datatype != "itfstd")
    req(input$datatype != "demos")
    
    req(input$file)
    data<-reactiveVal()
    pretreatdata<-reactiveVal()
    
    
    validate(need(input$datatype!="sas", message = "nope"))
    validate(need(input$datatype!="itfstd", message = "nope"))
    
    
    #-----------------------------------------------------------------Import csv, txt--------------------------------------
    
    
    if (input$datatype=="txt" || input$datatype == "csv")
    {decimal <- input$dec
    delim <- input$delim
    
    b <- read.table(input$file$datapath, fill = T, sep = delim, dec = decimal, header = input$labels, check.names = F)
    
    if (input$namerows == T)
    {c <- duplicated(b[,1])
    validate(need(TRUE%in%c != T, message = "Duplicated sample names are not permitted. Check the sample name option"),
             (need(ncol(b)>1, message = "There is probably a delimiter error, only 1 column has been detected")))
    
    a <- t(b[,1])
    b <- b[2:ncol(b)]
    validate(need(nrow(b)==ncol(a), message = "Check the delimiter selected"))
    rownames(b)<-a
    
    b
    }
    
    if (input$namerows == F)
    {validate(need(ncol(b)>1, message = "There is probably a delimiter error, only 1 column has been detected"))
      rownames(b)<-paste0("Sample ",1:nrow(b)) 
      b}
    
    b  }
    
    
    #------------------------------------------------------------------------Excel---------------------------------------------------------------------    
    if (input$datatype=="xlsx")
    {
      validate(need(grepl(".xls", input$file$datapath)==T, message = "Wrong type of file"))
      
      b <- read_excel(input$file$datapath, sheet = input$excelsheet, col_names = input$labels)
      
      if (input$namerows == T)
      {b<-column_to_rownames(b,var = colnames(b)[1])
      
      b
      }
      
      else
      {validate(need(ncol(b)>1, message = "There is probably a delimiter error, only 1 column has been detected"))
        colnames<-colnames(b)
        b<-data.frame(paste0("Sample ",1:nrow(b)),b)
        colnames(b)<-c("Name",colnames)
        b<-column_to_rownames(b,var = colnames(b)[1])
        #rownames(b)<-paste0("Sample ",1:nrow(b)) 
        b}
      
      
      b  }
    #-----------------------------------------------------------------------Final----------------------------------------------------------------------
    
    if(input$labels==F)
    {colnames(b)<-1:ncol(b)
    b}
    
    b
  })
  
  #-------------------------------------------------------------------------Class--------------------------------------------------------------------
  
  observeEvent(input$preview, {
    req(input$datatype != "itfstd")
    req(input$datatype != "demos")
    req(input$file)
    
    
    if (input$classcol==T)
    {
      
      if(input$namerows == T){
        classpos<-input$classpos-1
      }
      
      if(input$namerows == F){
        classpos<-input$classpos
      }  
      
    matrix<-newdata()
    unique<-unique(matrix[[classpos]])
    length<-length(unique)
    numbers<-c(100:length)
    
    for (i in 1:length)
      matrix[[classpos]][matrix[[classpos]]==unique[[i]]]<-numbers[[i]]
    matrix[[classpos]]
    
    sampleclasscolour(matrix[[classpos]])}
    
    else
      sampleclasscolour(0)
    {}
    
    
  })
  
  observeEvent(input$preview,{
    req(input$datatype != "itfstd")
    req(input$datatype != "demos")
    req(input$file)
    
    if(input$namerows == T){
      classpos<-input$classpos-1
    }
    
    if(input$namerows == F){
      classpos<-input$classpos
    }
    
    
    
    if (input$classcol==T)
    {matrix<-newdata()[,classpos]
    
    if (is.data.frame(matrix)==F)
      sampleclass(matrix)
    
    if(is.data.frame(matrix)==T)
      sampleclass(matrix[[classpos]]) 
    
    }
    
    else
      sampleclass(0)
    {}
  })
  
  
  #----------------------------------------------------------------------------------End of data importation--------------------------------------------  
  observeEvent(input$preview,{
    
    if(input$namerows == T){
      classpos<-input$classpos-1
    }
    
    if(input$namerows == F){
      classpos<-input$classpos
    }
    
    req(input$file)
    req(input$datatype != "itfstd")
    req(input$datatype != "demos")
    
    if (input$classcol==T)
    {data(newdata()[,-c(classpos)])}
    
    
    else
    {data(newdata())}
    
  })
  
  observeEvent(input$preview,{req(input$file)
    req(input$datatype != "itfstd")
    req(input$datatype != "demos")
    
    if(input$isspectra==T)
    {matrix<-data.frame(as.double(sub(',','.',colnames(data()))))
    }
    
    else
    {matrix<-data.frame(colnames(data()))}
    
    variables(matrix)
  })
  
  observeEvent(input$preview,{req(input$file)
    req(input$datatype != "itfstd")
    req(input$datatype != "demos")
    
    id<-data.frame(rownames(data()))
    rownames(id)<-rownames(data())
    id
    
    id(id)
    
  })
  
  observeEvent(input$preview, {
    req(input$file)
    req(input$datatype != "itfstd")
    req(input$datatype != "demos")
    
    if (input$isspectra==T)
    {validate(need(ncol(data())>1, message = "There is probably a delimiter error, only 1 column has been detected"))
      
      matrix<-data()
      colnames(matrix)<-t(variables())
      data(matrix)}
    
    else
    {}
  })
  
  #----------------------------------------------------------------------------Remove data option----------------------------------------------------------
  observeEvent(data(), {
    req(data())
    
    updateSelectizeInput(inputId = "removedatacol", choices = list(as.character(t(variables()))), selected = NULL, server = T)
    updateSelectizeInput(inputId = "removedatarow", choices = list(as.character(t(id()))), selected = NULL, server = T)
    
  })
  
  
  observeEvent(input$removedatacolBT, {
    req(data())
    newdata<-data()[,-which(dimnames(data())[[2]] %in% input$removedatacol)]
    
    data(newdata)
    variables(colnames(data()))
    
    imputedata(data())
    transformdata(data())
    pretreatdata(data())
    
  })
  
  observeEvent(input$removedatarowBT, {
    req(data())
    
    newdata<-data()[-which(dimnames(data())[[1]] %in% input$removedatarow),]
    matrix<-sampleclass()[-which(rownames(data()) %in% input$removedatarow)]
    matrix2<-sampleclasscolour()[-which(rownames(data()) %in% input$removedatarow)]
    
    data(newdata)
    id(rownames(data()))
    
    sampleclass(matrix)
    sampleclasscolour(matrix2)
    
    imputedata(data())
    transformdata(data())
    pretreatdata(data())
    
  })
  
  #-----------------------------------------------------------------------------------------Summary and plot----------------------------------------------------------  
  
  observeEvent(input$preview, {
    req(data())
    
    output$datanullvalues<-renderPrint(which(is.na(data()), arr.ind = T))
    
    if(length(unique(duplicated(data())))!=1)
    {
      
      if(all(sapply (data(), is.numeric)))  
      {cor<-cor(t(data()))
      p<-which(cor>0.9999, arr.ind = T, useNames = F)
      dup<-p[which(p[1:nrow(p),1]!=p[1:nrow(p),2], useNames = T),]
      
      dupmatrix2<-matrix(c(rownames(data()[dup[,1],]),
                           rownames(data()[dup[,2],])),
                         nrow(dup),
                         2,
      )
      
      dupmatrix3<-dupmatrix2[order(dupmatrix2[,1]),]
      output$checkdup<-renderPrint(paste0("Samples ",list(rownames(unique(data()[which(duplicated(data())),]))), " are duplicate of others. It is advised to remove them"))  
      }
      
      showModal(modalDialog(title = "Warning" ,paste0("Samples ",list(rownames(unique(data()[which(duplicated(data())),]))), " have/are duplicates. Check which of then relate by viewing the 'Duplicated Values' tab at 'Data Overview'")
                            ,easyClose = T, footer = "Click anywhere to dismiss")) 
      
    }
    
  })
  
  observeEvent(input$preview,{
    req(data())
    
    if (input$isspectra == F)
    {updateTabsetPanel(inputId ="dataPreview", selected = "panelnormaldata")
      output$preview1 <- renderDT(data(),rownames = T,options = list(pageLength = 10), width = "200px")
      output$summary <- renderDT(data.frame(ExpData(data())),options = list(pageLength = 100))
    }
    if (input$isspectra == T)
    {updateTabsetPanel(inputId ="dataPreview", selected = "panelspectrum")
      updateSelectInput(inputId ="methodpca", selected = "nipals")
      if (input$classcol==T)
      {output$spectrumpreview <- renderPlot({matplot(y=t(data()), x=variables(), type = "l", ylab = "", xlab = "", lty = 1, col = sampleclasscolour())
        legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)})}
      
      if (input$classcol==F)
      {output$spectrumpreview <- renderPlot(matplot(y=t(data()),x=variables(), type = "l", ylab = "", xlab = "", lty = 1, col = ))}
      
      output$spectramsn <- renderText("Showing first 5 variables.")
      output$preview1 <- renderDT(data()[,1:10],rownames = T, options = list(pageLength = 10), width = 200)}
    
    pretreatdata(data())
    imputedata(data())
    transformdata(data())
  })
  
  #------------------------------------------------------------------------------------------Boxplot----------------------------------------------------------
  observeEvent(input$datatype,{output$newdataboxplot<-renderPlot(ggplot())})
  
  observeEvent(input$viewnewdataboxplotBT, {
    if(input$scalenewdataboxplot == T)
    { matrix<-scale(data())
    class<-sampleclass()
    matrix2<-data.frame(class,matrix)
    melted<-melt(matrix2, id='class')
    output$newdataboxplot<-renderPlot(ggplot(melted, aes(x=variable, y=value, col = class))+geom_boxplot())
    }
    
    else
    {output$newdataboxplot<-renderPlot(boxplot(data()))}
    
  })
  
  #---------------------------------------------------------------------------------------Descriptive Analysis---------------------------------------------------------------------
  
  #-------------------Overview
  output$datasummary<-renderDT({
    req(data())
    summary<-psych::describe(data(), skew = T, type = 2, IQR = T)
    print(summary, digits = 4)}, rownames = T, options = list(pageLength = 100), width = "200px")
  
  
  observeEvent(c(variables(), input$confirmpretreat),{req(data())
    updateSelectizeInput(inputId = "DAvariableselect", choices = list(as.character(t(variables()))), server = T)
    updateSelectInput(inputId = "whichclass", choices = unique(sampleclass()))
  })
  
  
  #-----------------Analysis per variable
  observeEvent(c(input$DAvariableselect, input$DAclassselection, data(), input$whichclass) , {req(data())
    
    req(input$DAvariableselect)
    validate(need(is.numeric(data()[[input$DAvariableselect]]) == T, message = "Non numeric variable selected"))
    
     if ((isTRUE(nrow(data())<5000)==T) && ( isTRUE(length(data()[[input$DAvariableselect]])>7)==T)){
      
     if (input$DAclassselection == T){
       Class<-which(sampleclass() == input$whichclass)
       
       t1 <- ks.test(x=data()[[input$DAvariableselect]][Class], "pnorm") # KS
       t2 <- lillie.test(data()[[input$DAvariableselect]][Class]) # Lilliefors
       t3 <- cvm.test(data()[[input$DAvariableselect]][Class]) # Cramer-von Mises
       t4 <- shapiro.test(data()[[input$DAvariableselect]][Class]) # Shapiro-Wilk
       t5 <- sf.test(data()[[input$DAvariableselect]][Class]) # Shapiro-Francia
       t6 <- ad.test(data()[[input$DAvariableselect]][Class]) # Anderson-Darling
     }
      
     if(input$DAclassselection == F){
       
       t1 <- ks.test(x=data()[[input$DAvariableselect]], "pnorm") # KS
       t2 <- lillie.test(data()[[input$DAvariableselect]]) # Lilliefors
       t3 <- cvm.test(data()[[input$DAvariableselect]]) # Cramer-von Mises
       t4 <- shapiro.test(data()[[input$DAvariableselect]]) # Shapiro-Wilk
       t5 <- sf.test(data()[[input$DAvariableselect]]) # Shapiro-Francia
       t6 <- ad.test(data()[[input$DAvariableselect]]) # Anderson-Darling
     }
    
    
    # Tabela de resultados
    testes <- c(t1$method, t2$method, t3$method, t4$method, t5$method,
                t6$method)
    estt <- as.numeric(c(t1$statistic, t2$statistic, t3$statistic,
                         t4$statistic, t5$statistic, t6$statistic))
    valorp <- c(t1$p.value, t2$p.value, t3$p.value, t4$p.value, t5$p.value,
                t6$p.value)
    resultados <- cbind(estt, valorp)
    rownames(resultados) <- testes
    colnames(resultados) <- c("Statistic", "p")
    
    output$DAnormtests<-renderPrint(print(resultados, digits = 3))
    }
    
    
    if (isTRUE(nrow(data())<5000) == F)
    {output$DAnormtests<-renderPrint("There must be less than 5000 samples for normality tests to be calculated")}
    
    if (isTRUE(length(data()[[input$DAvariableselect]])>7) == F)
    {output$DAnormtests<-renderPrint("There must be more than 7 samples for normality tests to be calculated")}
    
    
    if (input$DAclassselection == F)
      
    {output$DAvariableboxplot<-renderPlotly({plot<-ggplot(data(), aes(y=data()[[input$DAvariableselect]], x = as.character(input$DAvariableselect)))+
      geom_boxplot()+
      xlab(input$DAvariableselect)+
      ylab("") +
      stat_summary(fun = mean, geom = "point")
    ggplotly(plot, tooltip = "")})
    
    
    output$DAhistogramplot<-renderPlotly({plot<-ggplot(data(), aes(x=data()[[input$DAvariableselect]], y=..density..))+
      geom_histogram(bins = input$DAhistogramnumbins)+
      geom_density(alpha=1, col = "red")+
      xlab(input$DAvariableselect)
    ggplotly(plot, tooltip = "")}
    )
    
    output$DAqqplot<-renderPlot(ggplot(data(),aes(sample=data()[[input$DAvariableselect]]))+
                                  stat_qq()+
                                  stat_qq_line(aes(data()[[input$DAvariableselect]]),col="red")+
                                  xlab(input$DAvariableselect))
    
    output$DAcumulativedistributionplot<-renderPlot({ggplot(data(),aes(data()[[input$DAvariableselect]]))+
        stat_ecdf()+
        stat_function(fun = pnorm, args = list(mean = mean(data()[[input$DAvariableselect]], na.rm = TRUE),sd = sd(data()[[input$DAvariableselect]], na.rm = TRUE)), col = 'red', size = 0.6, lty=6)+
        xlab(input$DAvariableselect)
    })
    }
    
    if (input$DAclassselection == T)
      
    {output$DAvariableboxplot<-renderPlotly({Class<-as.character(sampleclass())
    plot<-ggplot(data(), aes(y=data()[[input$DAvariableselect]], x = Class, col = Class))+
      geom_boxplot()+
      xlab(input$DAvariableselect)+
      ylab("")+
      stat_summary(fun = mean, geom = "point")
    ggplotly(plot, tooltip = "")}
    )
    
    output$DAhistogramplot<-renderPlotly({Class<-as.character(sampleclass())
    plot<-ggplot(data(), aes(x=data()[[input$DAvariableselect]], y=..density.., fill = Class, col = Class))+
      geom_histogram(bins = input$DAhistogramnumbins, alpha = 0.4, position = "identity")+
      geom_density(alpha=0.4)+
      xlab(input$DAvariableselect)
    ggplotly(plot, tooltip = "")}
    )
    output$DAqqplot<-renderPlot(ggplot(data(),aes(sample=data()[[input$DAvariableselect]],col = as.character(sampleclass())))+
                                  stat_qq()+
                                  stat_qq_line(aes(data()[[input$DAvariableselect]], col = sampleclass()))+
                                  xlab(input$DAvariableselect)+
                                  ylab("")+
                                  labs(colour="Class"))
    
    output$DAcumulativedistributionplot<-renderPlot(ggplot(data(),aes(data()[[input$DAvariableselect]], col = as.character(sampleclass())))+
                                                      stat_ecdf()+
                                                      stat_function(aes(col="Normal Distribution"),fun = pnorm, args = list(mean = mean(data()[[input$DAvariableselect]], na.rm = TRUE),sd = sd(data()[[input$DAvariableselect]], na.rm = TRUE)), colour = 'red', size = 0.6, lty=6)+
                                                      xlab(input$DAvariableselect)+
                                                      ylab("")+
                                                      labs(colour="Class"))
    }
    
  })
  
  #------------------------------------Multivariate analysis
  
  observeEvent(input$multivaranalysisBT, {
    
    req(data())
    showModal(modalDialog(title = "Please wait:", "The calculations are running and may take a few seconds. This window will be automatically closed after the calculation is finished.", easyClose = F, footer = ""))
    
    
    
    if ((isTRUE(nrow(data())<5000)==T) && ( isTRUE(length(data()[[input$DAvariableselect]])>7)==T))
      
    {t1 <- MVN::mvn(data = (data()), mvnTest = "hz") # Henze-Zirkler
    t2 <- MVN::mvn(data = (data()), mvnTest = "mardia") # Mardia
    t3 <- MVN::mvn(data = (data()), mvnTest = "royston") # Royston
    t4 <- MVN::mvn(data = (data()), mvnTest = "dh") # Doornik-Hansens
    
    
    # Tabela de resultados
    testes <- c(t1$multivariateNormality$Test, t2$multivariateNormality$Test, t3$multivariateNormality$Test, t4$multivariateNormality$Test)
    estt <- as.numeric(c(t1$multivariateNormality$HZ, t2$multivariateNormality$Statistic, t3$multivariateNormality$H, t4$multivariateNormality$df))
    valorp <- c(t1$multivariateNormality$`p value`, t2$multivariateNormality$`p value`, t3$multivariateNormality$`p value`, t4$multivariateNormality$`p value`)
    resultados <- cbind(estt, valorp)
    rownames(resultados) <- testes
    colnames(resultados) <- c("Statistic", "p")
    
    output$multiDAtests<-renderPrint(print(resultados, digits = 3))
    }
    removeModal()
  })
  
  observeEvent(input$multivarSWanalysisBT, {
    req(data())
    showModal(modalDialog(title = "Please wait:", "The calculations are running and may take a few seconds. This window will be automatically closed after the calculation is finished.", easyClose = F, footer = ""))
    if ((isTRUE(nrow(data())<5000)==T) && ( isTRUE(length(data()[[input$DAvariableselect]])>7)==T))
    {t5 <- mvnormtest::mshapiro.test(data.matrix((data()%>%t()%>%diff()))) #Shapiro-Wilk
    testes <- c(t5$method)
    estt <- c(t5$statistic)
    valorp <- c(t5$p.value)
    resultados <- cbind(estt, valorp)
    rownames(resultados) <- testes
    colnames(resultados) <- c("Statistic", "p")
    
    output$multiSWDAtests<-renderPrint(print(resultados, digits = 3))
    }
    removeModal() 
  })
  
  
  
  #------------------------------------Missing Data analysis
  observeEvent(data(), {output$NAdataDAsummary<-renderDT({table<-data.frame(ExpData(data(), type = 2), row.names = NULL)
  colnames(table)<-c("Index12", "Variable Name", "Variable Type", "Number of Samples", "Missing Count", "% of Missing Data (0-1)", "Number of Unique Values")
  table[,-1]})
  })
  
  observeEvent(data(),{req(data())
    updateSelectizeInput(inputId = "removevarmatplotDA", choices = list(as.character(t(variables()))), selected =NULL, server = T)})
  
  observeEvent(c(data(), input$removevarmatplotDA), {
    
    if(length(input$removevarmatplotDA)!=0)
    {output$datamatrixmapDA<-renderPlot(matrixplot(data()[,-which(names(data()) %in% input$removevarmatplotDA)]))}
    else
    {output$datamatrixmapDA<-renderPlot(matrixplot(data()))}
  })
  
  #---------------- #Missing data variable removal
  
  observeEvent(input$rmvNAVARS, {req(data())
    
    updateCheckboxInput(inputId = "confirmRMNA", value = F)
    matrix0<-data()
    matrix<-(colSums(is.na(matrix0))/nrow(matrix0) <= (input$pctNAvaluesRM)/100)
    matrix2<-base::which(matrix==T)
    matrix3<-matrix0[,c(matrix2)]
    data(matrix3)
    variables(colnames(data()))
    
  })
  
  #----------------------------------CorrheatMap
  observeEvent(c(data(), input$corrplottype, input$classheatmap),{validate(need(isTRUE(ncol(data())>100)==F, message = "There are too many variables"))
    
    if(input$corrplottype == "pairs"){
      
      if(input$classheatmap == T){
        output$DAcorrHM<-renderPlot(ggpairs(data(), aes(color = sampleclass(), alpha = 0.5)), height = 1000)
      }
      
      if(input$classheatmap == F){
        output$DAcorrHM<-renderPlot(ggpairs(data(), height = 1000))
      }
    }
    
    
    if(input$corrplottype == "heatmap"){
      output$DAcorrHM<-renderPlot({ 
        data0<-data()
        matrix<-which(sapply(data0, is.numeric))
        matrix2<-data0[,matrix]
        
        col<-colorRampPalette(c("blue","white","red"))(200)
        cor<-cor(matrix2)
        corrplot(cor, method = "color", col = col, type = "upper", addCoef.col = "black", tl.col = "black", tl.srt = 60, diag = F)
        
      }, height = 1000)
    }
    
    if(input$corrplottype == "clustheatmap"){
      output$DAcorrHM<-renderPlot(heatmap(data.matrix(data())), height = 1000)
    }
    
    
    
  })
  #--------------------------------------------------------------------------------------------IMPUTATION--------------------------------------------------------------------------------   
  
  observeEvent(c(variables(), input$confirmpretreat),{req(data())
    updateSelectizeInput(inputId = "IMPvariableselect", choices =list(as.character(t(variables()))), server = T)
  })
  
  #-----------------------------------Subtraction by media 
  observeEvent(input$meanIMPBT,{
    data2<-data()
    matrix<-which(sapply(data2, is.numeric))
    matrix2<-data2[,matrix]
    for(i in 1:ncol(matrix2)){
      matrix2[is.na(matrix2[,i]), i] <- mean(matrix2[,i], na.rm = TRUE)
    }
    
    pcaimp<-data.frame(matrix2)
    
    for (i in 1:ncol(pcaimp)) {
      
      data2[matrix[i]]<-data.frame(pcaimp[i])
      
    }
    
    imputedata(data2)
  })
  
  #--------------------------------Subtraction by 0
  
  observeEvent(input$sub0simpBT,{
    
    data0<-data()
    matrix<-which(sapply(data0, is.numeric))
    matrix2<-data0[,matrix]
    matrix2[is.na(matrix2)]<-0
    pcaimp<-data.frame(matrix2)
    for (i in 1:ncol(pcaimp)) {
      
      data0[matrix[i]]<-pcaimp[i]
      
    }
    
    imputedata(data0)
  })
  
  
  #---------------------------------Subtract by median
  observeEvent(input$medianIMPBT,{
    
    data2<-data()
    matrix<-which(sapply(data2, is.numeric))
    matrix2<-data2[,matrix]
    for(i in 1:ncol(matrix2)){
      matrix2[is.na(matrix2[,i]), i] <- median(matrix2[,i], na.rm = TRUE)
    }
    
    pcaimp<-data.frame(matrix2)
    
    for (i in 1:ncol(pcaimp)) {
      
      data2[matrix[i]]<-data.frame(pcaimp[i])
      
    }
    
    imputedata(data2)
    
  })
  
  #-------------------------------Miss forest
  
  observeEvent(input$MissIMPBT,{
    showModal(modalDialog(title = "Please wait:", "The calculations are running and may take a few seconds. This window will be automatically closed when they are finished.", easyClose = F, footer = ""))
    data<-data()
    matrix<-which(sapply(data, is.numeric))
    matrix2<-data.frame(data[,matrix])
    miss<-missForest(matrix2, ntree = input$ntreesMissIMP, maxiter = input$niterMissIMP)
    pcaimp<-data.frame((miss$ximp))
    
    for (i in 1:ncol(pcaimp)) {
      
      data[matrix[i]]<-pcaimp[i]
      
    }
    
    imputedata(data)
    removeModal()
  })
  
  
  #----------------------------KNN
  observeEvent(input$knnIMPBT,{
    
    data<-data()
    matrix<-which(sapply(data, is.numeric))
    matrix2<-data.frame(data[,matrix])
    miss<-VIM::kNN(matrix2, k=input$knumberKNNIMP)
    pcaimp<-data.frame((miss[,1:ncol(matrix2)]))
    
    for (i in 1:ncol(pcaimp)) {
      
      data[matrix[i]]<-pcaimp[i]
      
    }
    
    imputedata(data)
  })
  
  
  #--------------------------PCA methods
  
  observeEvent(input$pcaIMPBT,{
    showModal(modalDialog(title = "Please wait:", "The calculations are running and may take a few seconds. This window will be automatically closed when they are finished.", easyClose = F, footer = ""))
    
    data<-data()
    matrix<-which(sapply(data, is.numeric))
    matrix2<-data.frame(data[,matrix])
    
    if (input$scalePCAIMP == T)
    {matrix3<-scale(matrix2)}
    
    if (input$scalePCAIMP == F)
    {matrix3<-matrix2}
    
    miss<-pcaMethods::pca(matrix3, nPcs = input$numpcsPCAIMP, method = input$methodPCAIMP)
    pcaimp<-data.frame(completeObs(miss))
    
    for (i in 1:ncol(pcaimp)) {
      
      data[matrix[i]]<-pcaimp[i]
      
    }
    
    imputedata(data)
    removeModal()
  })
  
  
  #----------------------------------------------SVD
  
  observeEvent(input$SVDIMPBT,{
    
    showModal(modalDialog(title = "Please wait:", "The calculations are running and may take a few seconds. This window will be automatically closed when they are finished.", easyClose = F, footer = ""))
    data<-data()
    matrix<-which(sapply(data, is.numeric))
    matrix2<-data.frame(data[,matrix])
    
    if (input$scaleSVDIMP == T)
    {matrix3<-scale(matrix2)}
    
    if (input$scaleSVDIMP == F)
    {matrix3<-matrix2}
    
    miss<-pcaMethods::pca(matrix3, nPcs = input$numpcsSVDIMP,  maxSteps = input$numitSVDIMP, method = "svdImpute")
    pcaimp<-data.frame(completeObs(miss))
    
    for (i in 1:ncol(pcaimp)) {
      
      data[matrix[i]]<-pcaimp[i]
      
    }
    
    imputedata(data)
    
    removeModal()
  })
  
  
  
  #-----------------------------------------------Plots
  
  output$IMPhistPlot<-renderPlotly({plot<-ggplot()+
    geom_density(aes(data()[[input$IMPvariableselect]], col="Original"))+
    geom_density(aes(imputedata()[[input$IMPvariableselect]], col="Imputed"))+
    xlab(input$IMPvariableselect)+
    scale_colour_manual(values = c("Original"="Black", "Imputed"="Red"))
  ggplotly(plot)})
  
  output$IMPboxPlot<-renderPlotly({plot<-ggplot()+
    geom_boxplot(aes(y=data()[[input$IMPvariableselect]], col="Original", x= "Original"))+
    geom_boxplot(aes(y=imputedata()[[input$IMPvariableselect]], col="Imputed", x = "Imputed"))+
    scale_colour_manual(values = c("Original"="Black", "Imputed"="Red"))+
    xlab(input$IMPvariableselect)+
    ylab("")
  ggplotly(plot)
  })
  
  
  
  #-----------------------------------------------------Confirm IMP
  
  observeEvent(input$confirmimp,{
    data(imputedata())
    transformdata(imputedata())
    pretreatdata(imputedata())
  })
  
  
  #--------------------------------------------------------------------------Pre-Treatments----------------------------------------------------------------------------    
  observeEvent(pretreatdata(),{req(input$file)
    if(input$isspectra==T)
      pretreatvariables(data.frame(as.double(sub(',','.',colnames(pretreatdata())))))
    
    else
      pretreatvariables(data.frame(colnames(pretreatdata())))
  })
  
  observeEvent(input$interactiveplots, ignoreInit = T, {
    
    req(data())
    
    showModal(modalDialog(title = "Please wait:", "The calculations are running and may take a several minutes. This window will be automatically closed when they are finished.", easyClose = F, footer = ""))
    
    tpretreatdata<-(t(pretreatdata()))
    tdata<-(t(data()))
    
    id<-id()
    variables<-variables()
    pretreatvariables<-pretreatvariables()
    
    
    output$originaldataplotINT<-renderPlotly({
      t <- list(
        family = "times",
        size = 12,
        color = toRGB("grey50"))
      
      Plot <-  plot_ly(type="scatter",
                       #data=tdata,
                       x=as.double(data.matrix(data.frame(t(variables), check.names = F))),
                       y=tdata[,1],
                       mode="lines",
                       #options=c(),
                       showlegend=T,
                       #text=data2,
                       text="",
                       name = data.frame(t(id))[,1],
                       line = list(width = 0.8),
                       #color = colnames(loadings)[[1]],
                       hoverinfo="skip")%>%
        layout(title="<b>Original Spectra</b>",
               xaxis=list(title=paste0(""),zerolinecolor="lightgrey"),
               yaxis=list(title=paste0(""),zerolinecolor="lightgrey"))
      
      for (i in 2:ncol(tpretreatdata)) {
        
        Plot<-add_trace(Plot,
                        y=tdata[,i],
                        mode="lines",
                        showlegend=T,
                        #text=data2,
                        text="",
                        name = data.frame(t(id))[,i],
                        #color = colnames(loadings)[[i]],
                        hoverinfo="skip")
        #add_text(textposition="topright", textfont=t)%>%
        
        
      }
      
      
      Plot
    
    
    })
    
    output$changeddataplotINT<-renderPlotly({
      t <- list(
        family = "times",
        size = 12,
        color = toRGB("grey50"))
      
     Plot <-  plot_ly(type="scatter",
                       #data=tpretreatdata,
                       x=as.double(data.matrix(data.frame(t(pretreatvariables), check.names = F))),
                       y=tpretreatdata[,1],
                       mode="lines",
                       #options=c(),
                       showlegend=T,
                       #text=data2,
                       text="",
                       name = data.frame(t(id))[,1],
                       line = list(width = 0.8),
                       #color = colnames(loadings)[[1]],
                       hoverinfo="skip")%>%
        layout(title="<b>Changed Spectra</b>",
               xaxis=list(title=paste0(""),zerolinecolor="lightgrey"),
               yaxis=list(title=paste0(""),zerolinecolor="lightgrey"))
      
      for (i in 2:ncol(tpretreatdata)) {
        
        Plot<-add_trace(Plot,
                        y=tpretreatdata[,i],
                        mode="lines",
                        showlegend=T,
                        #text=data2,
                        text="",
                        name = data.frame(t(id))[,i],
                        #color = colnames(loadings)[[i]],
                        hoverinfo="skip")
        #add_text(textposition="topright", textfont=t)%>%
        
        
      }
      
      
      Plot
      
      
    })
    
    removeModal()
    
  })
  
  
  
  
  
  
  #output$currentspectraprepro<-renderPlot(matplot(y=t(pretreatdata()), x=pretreatvariables(), type = "l", ylab = "", xlab = "", lty = 1, col = ))
  output$currentspectraprepro<-renderPlotly({
    t <- list(
      family = "times",
      size = 12,
      color = toRGB("grey50"))
    
    Plot <-  plot_ly(type="scatter",
                     #data=tpretreatdata,
                     x=as.double(as.matrix(data.frame(t(pretreatvariables()), check.names = F))),
                     y=t(pretreatdata())[,1],
                     mode="lines",
                     #options=c(),
                     showlegend=T,
                     #text=data2,
                     text="",
                     name = data.frame(t(id()))[,1],
                     line = list(width = 0.8),
                     #color = colnames(loadings)[[1]],
                     hoverinfo="skip")%>%
      layout(title="<b>Changed Spectra</b>",
             xaxis=list(title=paste0(""),zerolinecolor="lightgrey"),
             yaxis=list(title=paste0(""),zerolinecolor="lightgrey"))
    
    for (i in 2:ncol(t(pretreatdata()))) {
      
      Plot<-add_trace(Plot,
                      y=t(pretreatdata())[,i],
                      mode="lines",
                      showlegend=T,
                      #text=data2,
                      text="",
                      name = data.frame(t(id()))[,i],
                      #color = colnames(loadings)[[i]],
                      hoverinfo="skip")
      #add_text(textposition="topright", textfont=t)%>%
      
      
    }
    
    
    Plot
    
    
  })
  output$currentspectrapreprocut<-renderPlot(matplot(y=t(pretreatdata()), x=pretreatvariables(), type = "l", ylab = "", xlab = "", lty = 1, col = )) #com escala
  
  output$currentspectrapreproview<-renderText({xy_range_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("xmin=", round(e$xmin, 2), " xmax=", round(e$xmax, 2))
  }
  paste0("brush: ", xy_range_str(input$currentspectrabrush)
  )
  })
  
  
  
  output$currentspectraaligncut<-renderPlot({matplot(y=t(pretreatdata()), type = "l", ylab = "", xlab = "", lty = 1, col = , xaxt = "n")
    axis(side = 1, at=c(seq(from=0, to=ncol(pretreatdata()), by=100)))}) #sem escala
  output$currentspectraalignview<-renderText({xy_range_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("xmin=", round(e$xmin, 2), " xmax=", round(e$xmax, 2))
  }
  paste0("brush: ", xy_range_str(input$currentspectrabrushalign)
  )
  })
  
  
  
  
  
  observeEvent(pretreatdata(), {req(data())
    updateSelectizeInput(inputId = "minxcut", choices = as.double(sub(',','.',colnames(pretreatdata()))), selected = min(as.double(sub(',','.',colnames(pretreatdata())))))
    updateSelectizeInput(inputId = "maxxcut", choices = as.double(sub(',','.',colnames(pretreatdata()))), selected = max(as.double(sub(',','.',colnames(pretreatdata())))))
    updateSelectizeInput(inputId = "mscreference", choices = rownames(pretreatdata()))
    updateSelectizeInput(inputId = "ptwreference", choices = rownames(pretreatdata()))
    updateSelectizeInput(inputId = "referencealign", choices = rownames(pretreatdata()))
  })
  
  #-----------------------------RESET
  observeEvent(input$resetpretreat,{
    pretreatdata(data())
    pretreatvariables(variables())
    updateActionButton(inputId  = "usesavgol")
    updateActionButton(inputId  = "usesnv")
  })
  
  #----------------------------- Savitzky-Golay
  observeEvent(input$preview,{
    updateNumericInput(inputId = "windowsizeSG", max = ncol(data())-1)
  })
  observeEvent(input$usesavgol, {
    req(data())
    validate(need(input$polyorderSG>=input$difforderSG, message = "Not selected"))
    validate(need(input$windowsizeSG%%2!=0, message = "No message"))
    validate(need(input$windowsizeSG>input$polyorderSG, message = "Not selected"))
    #radioButtons("linecolSG", "Apply Savitzky-Golay at:", inline = T ,choices = c("Samples"="lineSG", "Variables"="colSG")),
    
    
    pretreatdata<-pretreatdata()
    
    if(input$linecolSG=='lineSG')
    {matrix<-savitzkyGolay((pretreatdata), p = input$polyorderSG, w = input$windowsizeSG, m = input$difforderSG)
    pretreatdata((matrix))
    pretreatvariables(data.frame(as.double(sub(',','.',colnames(pretreatdata())))))
    }
    
    if(input$linecolSG=='colSG')
    {matrix<-savitzkyGolay(t(pretreatdata), p = input$polyorderSG, w = input$windowsizeSG, m = input$difforderSG)
    pretreatdata(t(matrix))
    pretreatvariables(data.frame(as.double(sub(',','.',colnames(pretreatdata())))))
    }
    
  })
  #---------------------------Moving Average
  observeEvent(input$preview,{req(data())
    updateNumericInput(inputId = "windowsizeMA", max = ncol(data())-1)
  })
  observeEvent(input$useMA,{
    matrix<-movav(pretreatdata(), input$windowsizeMA)
    pretreatdata(data.frame(matrix, check.names = F))
    pretreatvariables(data.frame(as.double(sub(',','.',colnames(pretreatdata())))))
  })
  #---------------------------SNV
  observeEvent(input$usesnv,{req(data())
    matrix<-standardNormalVariate(pretreatdata())
    pretreatdata(matrix)
  })
  #---------------------------MSC
  observeEvent(input$usemsc,{req(data())
    
    if(input$mscusemean==F)
    {matrix<-msc(pretreatdata(), reference_spc = as.vector(t(pretreatdata()[input$mscreference,])))
    pretreatdata(matrix)}
    
    else
    {matrix<-msc(pretreatdata())
    pretreatdata(matrix)} 
    
    pretreatdata()
  })
  
  #-------------------------Simple derivative
  observeEvent(input$useDV,{req(data())
    matrix<-t(diff(t(pretreatdata()), differences = input$difforderDV))
    pretreatdata(data.frame(matrix, check.names = F))
    pretreatvariables(data.frame(as.double(sub(',','.',colnames(pretreatdata())))))
  })
  
  
  #------------------------ALS baseline
  
  observeEvent(input$useALS,{req(data())
    showModal(modalDialog(title = "Please wait:", "The calculations are running and may take a few minutes. This window will be automatically closed when they are finished.", easyClose = F, footer = ""))
    
    matrix<-prep.alsbasecorr(pretreatdata(), plambda = input$plambdaALS, p = input$assimetryALS, max.niter = input$maxiterALS)
    pretreatdata(matrix)
    
    removeModal()
  })
  
  
  #----------------------Offset baseline
  
  observeEvent(input$useoffset,{req(data())
    matrix<-apply(pretreatdata(), 1, input$typeoffset)
    matrix2<-data.frame(sapply(pretreatdata(), "-", matrix))
    rownames(matrix2)<-t(id())
    colnames(matrix2)<-t(pretreatvariables())
    
    pretreatdata(matrix2)
  })
  
  
  #---------------------Polynomial Correction
  
  observeEvent(input$usePC,{req(data())
    showModal(modalDialog(title = "Please wait:", "The calculations are running and may take a few seconds. This window will be automatically closed when they are finished.", easyClose = F, footer = ""))
    
    matrix<-data.matrix(pretreatdata())
    matrix2<-baseline::baseline(matrix, method = "modpolyfit", degree=input$degreepolyPC, tol = input$ittolPC, rep = input$maxitPC)
    matrix3<-matrix2@corrected
    colnames(matrix3)<-t(pretreatvariables())
    
    pretreatdata(as.data.frame(matrix3))
    removeModal()
  })
  
  #-------------------Low-Pass
  
  observeEvent(input$useLPF,{req(data())
    
    showModal(modalDialog(title = "Please wait:", "The calculations are running and may take a few seconds. This window will be automatically closed when they are finished.", easyClose = F, footer = ""))
    
    matrix<-data.matrix(pretreatdata())
    matrix2<-baseline(matrix, method = "lowpass", steep=input$steepLPF, half = input$halfpointLPF)
    matrix3<-matrix2@corrected
    colnames(matrix3)<-t(pretreatvariables())
    
    pretreatdata(as.data.frame(matrix3))
    removeModal()
  })
  
  
  #-----------------Robust baseline correction
  
  observeEvent(input$useRBE,{req(data())
    showModal(modalDialog(title = "Please wait:", "The calculations are running and may take a few seconds. This window will be automatically closed when they are finished.", easyClose = F, footer = ""))
    matrix<-data.matrix(pretreatdata())
    matrix2<-baseline(matrix, method = "rfbaseline")
    matrix3<-matrix2@corrected
    colnames(matrix3)<-t(pretreatvariables())
    
    pretreatdata(as.data.frame(matrix3))
    removeModal()
  })
  
  #-----------------Iterative least square
  
  observeEvent(input$useIRLS,{req(data())
    showModal(modalDialog(title = "Please wait:", "The calculations are running and may take a few seconds. This window will be automatically closed when they are finished.", easyClose = F, footer = ""))
    matrix<-data.matrix(pretreatdata())
    matrix2<-baseline(matrix, method = "irls", lambda1 = input$lambda1IRLS, lambda2 = input$lambda2IRLS, maxit = input$maxitIRLS)
    matrix3<-matrix2@corrected
    colnames(matrix3)<-t(pretreatvariables())
    
    pretreatdata(as.data.frame(matrix3))
    removeModal()
  })
  
  
  #----------------Normalize by max intensity
  observeEvent(input$maxnormBT,{req(data())
    matrix<-data.matrix(pretreatdata())
    maxvalues<-rowMax(matrix)
    matrix2<-(matrix/(maxvalues))
    colnames(matrix2)<-t(pretreatvariables())
    
    pretreatdata(as.data.frame(matrix2))
    
  })
  
  #---------------Internal Standard
  observeEvent(input$internalptBT, {
    req(data())
    req(input$internalpattern)
    
    matrix<-data.matrix(pretreatdata())
    pattern<-as.vector(t(read_excel(input$internalpattern$datapath, col_names = T)))
    
    matrix2<-matrix/pattern
    
    colnames(matrix2)<-t(pretreatvariables())
    
    pretreatdata(as.data.frame(matrix2))
  })
  
  #-----------------------------Cut interval
  observeEvent(input$cutintervalBT, {
    req(input$maxxcut)
    req(input$minxcut)
    
    if (input$selectxvalues == "excludecolspretreat")
    {matrix<-pretreatdata()[,-c(which(colnames(pretreatdata()) == input$minxcut):which(colnames(pretreatdata()) == input$maxxcut))]
    pretreatdata(matrix)}
    
    if (input$selectxvalues == "includecolspretreat")
    {matrix<-pretreatdata()[,c(which(colnames(pretreatdata()) == input$minxcut):which(colnames(pretreatdata()) == input$maxxcut))]
    pretreatdata(matrix)}
    
    if(input$selectxvalues == "subsNApretreat")
    {matrix<-pretreatdata()
    matrix[,c(which(colnames(pretreatdata()) == input$minxcut):which(colnames(pretreatdata()) == input$maxxcut))]<-rowMeans(pretreatdata()[,c((which(colnames(pretreatdata()) == input$minxcut)-input$npointscutpretreat):which(colnames(pretreatdata()) == input$minxcut),
                                                                                                                                              which(colnames(pretreatdata()) == input$maxxcut):(which(colnames(pretreatdata()) == input$maxxcut)+input$npointscutpretreat))])
    pretreatdata(matrix)}
    
    pretreatdata()
  })
  
  
  #---------------------------Align Peaks
  
  refalign<-reactiveVal()
  
  observeEvent(c(input$referencealign,input$alignusemean), {req(data)
    
    if (input$alignusemean==F)
    {ref<-pretreatdata()[input$referencealign,]}
    
    else
    {ref<-(lapply(pretreatdata(), mean))}
    
    refalign(data.frame(ref))
    
  })
  
  
  #-------------------------------All spectra
  
  observeEvent(input$alignBT, {
    req(data())
    req(input$selecttypealign=="alignpeakswhole")
    showModal(modalDialog(title = "Please wait:", "The calculations are running and may take several minutes. This window will be automatically closed when they are finished.", easyClose = F, footer = ""))
    
    pretreatdata1<-pretreatdata()
    pretreatdata2<-pretreatdata()
    
    ptw<-ptw(refalign(), pretreatdata1, warp.type = "individual", verbose=T)
    
    pretreatdata2<-ptw$warped.sample
    
    pretreatdata(pretreatdata2)
    pretreatvariables(colnames(pretreatdata2))
    
    removeModal()
    
    
  })
  
  #-------------------------------User selected windows
  
  observeEvent(input$alignBT, {
    req(data())
    req(input$selecttypealign=="alignselectedwindows")
    
    showModal(modalDialog(title = "Please wait:", "The calculations are running and may take several minutes. This window will be automatically closed when they are finished.", easyClose = F, footer = ""))
    
    pretreatdata1<-pretreatdata()  
    pretreatdata2<-pretreatdata()
    
    
    input<-as.numeric(unlist(regmatches(input$selectwindowsaling, gregexpr("[[:digit:]]+", input$selectwindowsaling))))
    intervalsvalues<-cbind(1,t(input),ncol(pretreatdata1))
    
    
    for (i in 1:(length(intervalsvalues)-1)) {
      
      ptw<-ptw(refalign()[, intervalsvalues[[i]]:intervalsvalues[[i+1]]], pretreatdata1[,intervalsvalues[[i]]:intervalsvalues[[i+1]]], warp.type = "individual", verbose=T)
      
      pretreatdata2[,intervalsvalues[[i]]:intervalsvalues[[i+1]]]<-ptw$warped.sample
      pretreatdata2[is.na(pretreatdata2)]<-0
      
    }
    
    pretreatdata(pretreatdata2)
    #pretreatvariables(colnames(pretreatdata1))
    
    
    output$showwindowsalign<-renderPrint(intervalsvalues)
    removeModal()
    
  })
  
  
  #--------------------------------One window
  
  observeEvent(input$alignBT, {
    req(data())
    req(input$selecttypealign=="alignonewindow")
    showModal(modalDialog(title = "Please wait:", "The calculations are running and may take several minutes. This window will be automatically closed when they are finished.", easyClose = F, footer = ""))
    
    pretreatdata1<-pretreatdata()
    pretreatdata2<-pretreatdata()
    
    input<-as.numeric(unlist(regmatches(input$selectonewindowaling, gregexpr("[[:digit:]]+", input$selectonewindowaling))))
    
    intervalsvalues<-cbind(t(input))
    
    ptw<-ptw(refalign()[, intervalsvalues[[1]]:intervalsvalues[[1+1]]], pretreatdata1[,intervalsvalues[[1]]:intervalsvalues[[1+1]]], warp.type = "individual", verbose=T)
    
    pretreatdata2[,(intervalsvalues[[1]]):intervalsvalues[[1+1]]]<-ptw$warped.sample
    
    pretreatdata(pretreatdata2)
    #pretreatvariables(colnames(pretreatdata1))
    
    
    output$showwindowsalign<-renderPrint(intervalsvalues)
    removeModal()
    
    
  })
  
  #-----------------------------------------------------------------------Confirm tab
  
  output$originaldataplot<-renderPlot(matplot(y=t(data()),x=variables(), type = "l", ylab = "", xlab = "", lty = 1, col = ))
  output$changeddataplot<-renderPlot(matplot(y=t(pretreatdata()),x=pretreatvariables(), type = "l", ylab = "", xlab = "", lty = 1, col =))
  
  observeEvent(input$confirmpretreat, {
    
    matrix<-data.frame(pretreatdata())
    matrix2<-pretreatvariables()
    colnames(matrix)<-t(matrix2)
    
    data(matrix)
    variables(matrix2)
    
    imputedata(matrix)
    transformdata(matrix)
    
  })
  
  #----------------------------------------------------------------------Plot Tab  
  
  observeEvent(c(input$pretreatcopyBT, input$preview),{
    main = input$plotpretreattitle
    sub=input$plotpretreatsubtitle
    ylab =input$plotpretreatylabel
    xlab =input$plotpretreatxlabel
    cex.main = input$plotpretreattitlecex
    font.main=input$plotpretreattitlefont
    cex.sub = input$plotpretreatsubtitlecex
    font.sub=input$plotpretreatsubtitlefont
    cex.lab = input$plotpretreatlabelcex
    font.lab = input$plotpretreatlabelfont
    lwd = input$linewdpretreatplot
    cex.axis=input$plotpretreataxiscex
    
    
    
    
    if (input$plotpretreatclasses == "indpretreatplot")
    {output$copyfinalpretreatplot<-renderPlot({matplot(y=t(pretreatdata()), x=pretreatvariables(),lty = 1, col = , type = "l", main = "", xlab = "", ylab = "", lwd = lwd, cex.axis = cex.axis)
      title(main = main, sub = sub, xlab = xlab, ylab = ylab, cex.main = cex.main, font.main = font.main, cex.sub = cex.sub, font.sub = font.sub, cex.lab = cex.lab, font.lab = font.lab)
    }, 
    height = input$heightpretreatplot, width = input$widthpretreatplot, res = input$respretreatplot)
    
    output$downloadplotBT<-downloadHandler(
      filename = function(){"downloadedplot.tiff"},
      content = function(file){
        tiff(filename = file, height = input$heightpretreatplot, width = input$widthpretreatplot, res = input$respretreatplot)
        {matplot(y=t(pretreatdata()), x=pretreatvariables(),lty = 1, col = , type = "l", main = "", xlab = "", ylab = "", lwd = lwd, cex.axis = cex.axis)
          title(main = main, sub = sub, xlab = xlab, ylab = ylab, cex.main = cex.main, font.main = font.main, cex.sub = cex.sub, font.sub = font.sub, cex.lab = cex.lab, font.lab = font.lab)
        }
        dev.off()
      }
    )}
    
    if (input$plotpretreatclasses == "classpretreatplot")
    {output$copyfinalpretreatplot<-renderPlot({
      
      matplot(y=t(pretreatdata()), x=pretreatvariables(),lty = 1, col = sampleclasscolour(), type = "l", main = "", xlab = "", ylab = "", lwd = lwd, cex.axis = cex.axis)
      title(main = main, sub = sub, xlab = xlab, ylab = ylab, cex.main = cex.main, font.main = font.main, cex.sub = cex.sub, font.sub = font.sub, cex.lab = cex.lab, font.lab = font.lab)
      legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)},
      height = input$heightpretreatplot, width = input$widthpretreatplot, res = input$respretreatplot)
    
    output$downloadplotBT<-downloadHandler(
      filename = function(){"downloadedplot.tiff"},
      content = function(file){
        tiff(filename = file, height = input$heightpretreatplot, width = input$widthpretreatplot, res = input$respretreatplot)
        {matplot(y=t(pretreatdata()), x=pretreatvariables(),lty = 1, col = sampleclasscolour(), type = "l", main = "", xlab = "", ylab = "", lwd = lwd, cex.axis = cex.axis)
          title(main = main, sub = sub, xlab = xlab, ylab = ylab, cex.main = cex.main, font.main = font.main, cex.sub = cex.sub, font.sub = font.sub, cex.lab = cex.lab, font.lab = font.lab)
          legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)
        }
        dev.off()
      }
    )}
    
    
    if (input$plotpretreatclasses == "classmeanspretreatplot")
    {output$copyfinalpretreatplot<-renderPlot({
      
      matplot(t(pretreatdata()), x=pretreatvariables(), col = alpha("white", 0.1), type = "l", lty = 1, main = "", xlab = "", ylab = "", lwd = 2*lwd/3, cex.axis = cex.axis)
      
      for (i in 1:length(unique(sampleclass()))) {
        
        matplot(y=t(pretreatdata()[which(grepl(unique(sampleclass())[[i]], sampleclass(), fixed = T)),]), x=pretreatvariables(),type="l", col=alpha(unique(sampleclasscolour())[[i]],0.25), lty=1, alpha = 1, add = T, main = "", xlab = "", ylab = "", lwd = lwd/2, cex.axis = cex.axis)
        
      }
      
      for (i in 1:length(unique(sampleclass()))) {
        
        matplot(colMeans(pretreatdata()[which(grepl(unique(sampleclass())[[i]], sampleclass(), fixed = T)),]),  x=pretreatvariables(), col = alpha(unique(sampleclasscolour())[[i]],1) ,type="l", lty = 1, add=T, main = "", xlab = "", ylab = "", lwd = lwd+2, cex.axis = cex.axis)
        
      }
      
      title(main = main, sub = sub, xlab = xlab, ylab = ylab, cex.main = cex.main, font.main = font.main, cex.sub = cex.sub, font.sub = font.sub, cex.lab = cex.lab, font.lab = font.lab)
      legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)
      
    },  
    
    height = input$heightpretreatplot, width = input$widthpretreatplot, res = input$respretreatplot)
    
    output$downloadplotBT<-downloadHandler(
      filename = function(){"downloadedplot.tiff"},
      content = function(file){
        tiff(filename = file, height = input$heightpretreatplot, width = input$widthpretreatplot, res = input$respretreatplot)
        {matplot(t(pretreatdata()), x=pretreatvariables(),col = alpha("white", 0.1), type = "l", lty = 1)
          
          for (i in 1:length(unique(sampleclass()))) {
            
            matplot(y=t(pretreatdata()[which(grepl(unique(sampleclass())[[i]], sampleclass(), fixed = T)),]), x=pretreatvariables(),type="l", col=alpha(unique(sampleclasscolour())[[i]],0.15), lty=1, alpha = 1, add = T, main = "", xlab = "", ylab = "", lwd = 2*lwd/3, cex.axis = cex.axis)
            
          }
          
          for (i in 1:length(unique(sampleclass()))) {
            
            matplot(colMeans(pretreatdata()[which(grepl(unique(sampleclass())[[i]], sampleclass(), fixed = T)),]),  x=pretreatvariables(), col = alpha(unique(sampleclasscolour())[[i]],1) ,type="l", lty = 1, add=T, main = "", xlab = "", ylab = "", lwd = lwd+2, cex.axis = cex.axis)
            
          }
          
          title(main = main, sub = sub, xlab = xlab, ylab = ylab, cex.main = cex.main, font.main = font.main, cex.sub = cex.sub, font.sub = font.sub, cex.lab = cex.lab, font.lab = font.lab)
          legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)
          
        }
        dev.off()
      })
    
    }
    
    
    
    
    
    
    
  })
  
  #-------------------------------------------------------------------Variable Transformations
  observeEvent(c(variables(), input$confirmpretreat),{req(data())
    updateSelectizeInput(inputId = "PPvariableselect", choices = list(as.character(t(variables()))), server = T)
  })
  
  output$orgPPboxplot<-renderPlot({if (isTRUE(ncol(data())>100)==F)
    boxplot(data()[,which(sapply(data(), is.numeric))], main="Original")
    else
      matplot(t(data()), type = "l", lty = 1, main = "Original")
  })
  output$newPPboxplot<-renderPlot({if (isTRUE(ncol(data())>100)==F)
    boxplot(transformdata()[,which(sapply(data(), is.numeric))], main="Transformed")
    else
      matplot(t(transformdata()), type = "l", lty = 1, main = "Transformed")
  })
  
  output$orgPPhistogram<-renderPlot({ggplot(data(), aes(y=data()[[input$PPvariableselect]], x = as.character(input$PPvariableselect)))+
      geom_boxplot()+
      xlab(input$PPvariableselect)+
      ylab("")
  })
  output$newPPhistogram<-renderPlot({ggplot(transformdata(), aes(y=transformdata()[[input$PPvariableselect]], x = as.character(input$PPvariableselect)))+
      geom_boxplot()+
      xlab(input$PPvariableselect)+
      ylab("")
  })
  output$orgPPqqplot<-renderPlot({ggplot(transformdata(), aes(sample=data()[[input$PPvariableselect]]))+
      stat_qq()+
      stat_qq_line(aes(data()[[input$PPvariableselect]]),col="red")+
      xlab(input$PPvariableselect)+
      ylab("")
  })
  output$newPPqqplot<-renderPlot({ggplot(transformdata(), aes(sample=transformdata()[[input$PPvariableselect]]))+
      stat_qq()+
      stat_qq_line(aes(data()[[input$PPvariableselect]]),col="red")+
      xlab(input$PPvariableselect)+
      ylab("")
  })
  
  
  #--------------reset
  observeEvent(input$resetvalprepro, {
    transformdata(data())
  })
  
  #--------------Scaling 
  observeEvent(input$useScaletrans, {req(data())
    data<-transformdata()
    matrix<-which(sapply(data, is.numeric))
    matrix2<-data.frame(data[,matrix])
    matrix3<-data.frame(scale(matrix2, center=F))
    
    for (i in 1:ncol(matrix3)) {
      data[matrix[i]]<-matrix3[i]
    }
    colnames(data)<-t(variables())
    transformdata(data)
  })
  
  #--------------AutoScaling 
  observeEvent(input$useautoScaletrans, {req(data())
    data<-transformdata()
    matrix<-which(sapply(data, is.numeric))
    matrix2<-data.frame(data[,matrix])
    matrix3<-data.frame(scale(matrix2, center=T, scale=T))
    
    for (i in 1:ncol(matrix3)) {
      data[matrix[i]]<-matrix3[i]
    }
    colnames(data)<-t(variables())
    transformdata(data)
  })
  
  #--------------------Mean centering
  
  observeEvent(input$useMeantrans, {req(data())
    data<-transformdata()
    matrix<-which(sapply(data, is.numeric))
    matrix2<-data.frame(data[,matrix])
    matrix3<-data.frame(scale(matrix2, scale=F, center=T))
    
    for (i in 1:ncol(matrix3)) {
      data[matrix[i]]<-matrix3[i]
    }
    colnames(data)<-t(variables())
    transformdata(data)
  })
  
  #------------------YeoJohnson
  observeEvent(input$useYEOJtrans,{
    data<-transformdata()
    bc<-preProcess(data, method="YeoJohnson", na.remove = T) #YeoJohnson // BoxCox // expoTrans // range
    bctrans<-predict(bc,data)
    transformdata(bctrans)
  })
  
  #-----------------BoxCox
  observeEvent(input$useBOXCOXtrans,{
    data<-transformdata()
    bc<-preProcess(data, method="BoxCox", na.remove = T) #YeoJohnson // BoxCox // expoTrans // range
    bctrans<-predict(bc,data)
    transformdata(bctrans)
  })
  
  #----------------Frobenius Norm
  
  observeEvent(input$useFROBtrans,{
    data<-transformdata()
    matrix<-which(sapply(data, is.numeric))
    matrix2<-data.frame(data[,matrix])
    
    datamean<-data
    matrix0<-data
    matrixFRO<-0
    matrixFRO<-norm(data.matrix(matrix0), type = "f")
    datamean<-data/matrixFRO
    
    colnames(datamean)<-t(variables())
    transformdata(datamean)
  })
  
  #------------------------LOG
  
  observeEvent(input$useLOGtrans, {req(data())
    data<-transformdata()
    matrix<-which(sapply(data, is.numeric))
    matrix2<-data.frame(data[,matrix])
    matrix3<-data.frame(log(matrix2))
    
    for (i in 1:ncol(matrix3)) {
      data[matrix[i]]<-matrix3[i]
    }
    
    colnames(data)<-t(variables())
    transformdata(data)
    
  })
  #------------------------Confirm
  
  observeEvent(input$confirmtrans,{req(data())
    
    data(transformdata())
    imputedata(transformdata())
    pretreatdata(data())
    
    
  })
  
  #-------------------------------------------------------------------Save and load Session  
  
  output$saveglobalvariables<-downloadHandler(
    filename = function() {
      paste0(input$savefilename, ".RData")
    },
    content = function(file) {
      data<-data()
      variables<-variables()
      id<-id()
      pretreatdata<-pretreatdata()
      pretreatvariables<-pretreatvariables()
      sampleclass<-sampleclass()
      sampleclasscolour<-sampleclasscolour()
      imputedata<-imputedata()
      transformdata<-transformdata()
      
      save(data,
           variables,
           id,
           pretreatdata,
           pretreatvariables,
           sampleclass,
           sampleclasscolour,
           imputedata,
           transformdata,
           file = file)
    }
  )
  
  observeEvent(input$preview, {validate(need(input$datatype == "sas", message = "nope2"))
    
    load(input$file$datapath)
    data(data)
    variables(variables)
    id(id)
    pretreatdata(pretreatdata)
    pretreatvariables(pretreatvariables)
    sampleclasscolour(sampleclasscolour)
    sampleclass(sampleclass)
    imputedata(imputedata)
    transformdata(transformdata)
    
  })
  
  observeEvent(input$preview, {validate(need(input$datatype == "itfstd", message = "nope2"))
    
    load(input$file$datapath)
    
    data(data)
    variables(variables)
    id(id)
    pretreatdata(data)
    pretreatvariables(variables)
    sampleclasscolour(sampleclasscolour)
    sampleclass(sampleclass)
    imputedata(data)
    transformdata(data)
    
  })
  
  #--------------------------------------Export data
  
  output$exportdata<-downloadHandler(
    
    filename = function() {
      paste0(input$exportfilename, input$datatypeExp)
    },
    
    content = function(file) {
      
      if (length(sampleclass())>1)
      {dataframe<-data.frame(id(),sampleclass(), data(), check.names = F)
      rownames(dataframe)<-NULL
      colnames(dataframe)<-c("Sample Name","Sample Class", t(variables()))}
      
      else
      {dataframe<-data.frame(id(),data())
      rownames(dataframe)<-NULL
      colnames(dataframe)<-c("Sample Name", t(variables()))}
      
      write.table(dataframe, sep = input$delimExp, dec = input$decExp ,file=file, row.names = F)
    }
    
  )
  
  output$exportRdata<-downloadHandler(
    filename = function() {
      paste0(input$exportfilename, ".RData")
    },
    content = function(file) {
      data<-data()
      variables<-variables()
      id<-id()
      pretreatdata<-pretreatdata()
      pretreatvariables<-pretreatvariables()
      sampleclass<-sampleclass()
      sampleclasscolour<-sampleclasscolour()
      imputedata<-imputedata()
      transformdata<-transformdata()
      
      save(data,
           variables,
           id,
           pretreatdata,
           pretreatvariables,
           sampleclass,
           sampleclasscolour,
           imputedata,
           transformdata,
           file = file)
    }
  )
  
  
  
  
}       

# Run the application 
shinyApp(ui = ui, server = server)
