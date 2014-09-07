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
    checkboxInput('datTranspose','Transpose data?',FALSE),
    width=3), #end sidebar panel
  
  #main panel
  mainPanel(
    
    tabsetPanel(
      tabPanel("0. Introduction to Redhyte",
               imageOutput("algoPNG",height="700px")
               ),
      #data preview tab panel
      tabPanel("1. Data preview",
               tags$h4("Displaying a preview of your data"),
               sliderInput("previewRows","Number of rows to display",
                           min=1,max=20,value=1,step=1,animate=TRUE),
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
                plotOutput("boxplot1"),
                width=3),
               sidebarPanel(
                 htmlOutput("edaCtrl2"),
                 verbatimTextOutput("type2"),
                 tableOutput("tukeyfive2"),
                 plotOutput("hist2"),
                 plotOutput("boxplot2"),
                 width=3),
               sidebarPanel(
                 plotOutput("scatterplot"),
                 width=4)
               
               
              ),

      tabPanel("3. Initial test",
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
                 htmlOutput("cmpClassCtrl"),
                 width=3
               ),
               tags$h6("Table based on initial context"),
               tableOutput("contTable"),
               sidebarPanel(
#                  tags$style(type="text/css", '#rightPanel { width:220px; float:left;}'),
#                  id = "rightPanel",
                 
                 tags$h6("Initial test"),
                 tableOutput("initialTest")
               
              )
          ),
#       tabPanel("Test Data2()",
#                tableOutput("testData2")
#                )
      tabPanel("4. Test diagnostics"), #not implemented yet, will come back to this
      tabPanel("5. Hypothesis mining",
               tags$h6("Redhyte's hypothesis mining implementation works by first constructing two
                       random forest models, using the context attributes to predict the
                       target and comparing attributes."))

    )
    
      
      
      
      
    
    
  ) #end main panel
  
  
  ))