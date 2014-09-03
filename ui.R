require(shiny)

shinyUI(pageWithSidebar(
  
  titlePanel(imageOutput("logoPNG",width="180px",height="50px"),
             tags$head(tags$link(rel="icon",type="image/png",href="browserlogo.png",sizes="64x64"),
                       tags$title("Redhyte"))
            ),
  
#   #header panel
#   headerPanel(
#     
#     windowTitle="Redhyte"
#   ),#end header panel
  
  #sidebar panel
  sidebarPanel(
    imageOutput("titlePNG",height="70px"),
    
    tags$hr(),
    
    #data input
    fileInput('datFile', tags$h5(tags$strong('Choose .csv or .txt file to upload local file')),
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    
    tags$hr(),
    
    #checkbox to indicate header == true or false
    checkboxInput('datHeader', 'Header contains attribute names', TRUE),
    
    tags$hr(),
    
    #file type
    radioButtons('datSep', tags$h5(tags$strong('File type')),
                 c("Comma(.csv)"=',',
                   "Tab(.txt/.tsv)"='\t'),
                  'Comma(.csv)'),
    tags$hr(),
    
    #quotation used in data file
    radioButtons('datQuote', tags$h5(tags$strong('Quotes used in data file')),
                 c(None='',
                   'Double Quotes'='"',
                   'Single Quotes'="'"),
                 'Double Quotes'),
    
    tags$hr(),
    
    #transpose data
    checkboxInput('datTranspose','Transpose data?',FALSE)
    
  ), #end sidebar panel
  
  #main panel
  mainPanel(
    
    tabsetPanel(
      #data preview tab panel
      tabPanel("1. Uploaded data",
               tags$h4("Displaying the first 10 rows of your data"),
               tableOutput("dataPreview")
               ),
      
      #eda tab panel
      tabPanel("2. Data visualization",
               tags$h4("Simple exploratory data analysis"),
               sidebarPanel(
                htmlOutput("edaCtrl1"),
                verbatimTextOutput("type1"),
                tableOutput("tukeyfive1"),
                plotOutput("hist1"),
                plotOutput("boxplot1")
               ),
               sidebarPanel(
                 htmlOutput("edaCtrl2"),
                 verbatimTextOutput("type2"),
                 tableOutput("tukeyfive2"),
                 plotOutput("hist2"),
                 plotOutput("boxplot2")
               ),
               sidebarPanel(
                 plotOutput("scatterplot")
               )
               
               
              ),

      tabPanel("3. Hypothesis testing",
               #some definitions and explanation
#                tags$h4("It's testing time."),
#                tags$h5("Definitions:"),
#                tags$h5("-> Target attribute: attribute representing the result or desired outcome, e.g. (lowering of) cholesterol levels"),
#                tags$h5("-> Comparing attribute: attribute representing the act of comparison and/or intervention, e.g. statin vs. placebo"),
#                tags$h5("Note that while the target attribute can be either continuous or discrete, the comparing attribute must be discrete (2-class)."),
#                tags$h4("Let's start by indicating target and comparing attributes."),
               sidebarPanel(
#                  tags$style(type="text/css", '#leftPanel { width:200px; float:left;}'),
#                  id = "leftPanel",
                 htmlOutput("targetCtrl"),
                 verbatimTextOutput("targetType"),
                 htmlOutput("tgtClassCtrl"),
                 
                 htmlOutput("comparingCtrl"),
                 verbatimTextOutput("comparingType"),
                 htmlOutput("cmpClassCtrl")
               ),
               tags$h6("Contingency table"),
               tableOutput("contTable"),
               sidebarPanel(
#                  tags$style(type="text/css", '#rightPanel { width:220px; float:left;}'),
#                  id = "rightPanel",
                 
                 tags$h6("First test"),
                 tableOutput("firstTest")
               
              )
          ),
      tabPanel("Test Data2()",
               tableOutput("testData2")
               )

    )
    
      
      
      
      
    
    
  ) #end main panel
  
  
  ))