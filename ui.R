shinyUI(pageWithSidebar(
  
  titlePanel(imageOutput("logoPNG",width="180px",height="50px"),
             tags$head(tags$link(rel="icon",type="image/png",href="browserlogo.png",sizes="64x64"),
                       tags$title("Redhyte"))),
  
  #sidebar panel
  sidebarPanel(
    imageOutput("titlePNG",
                height="70px"),
    
    tags$hr(),
    
    #data input
    fileInput('datFile',
              tags$h5(tags$strong('Choose .csv or .txt file to upload local file')),
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    
    tags$hr(),
    
    #checkbox to indicate header == true or false
    checkboxInput('datHeader','Header contains attribute names', TRUE),
    
    tags$hr(),
    
    #file type
    radioButtons('datSep',
                 tags$h5(tags$strong("Separator")),
                 c("Comma(.csv)"=',',
                   "Tab(.txt/.tsv)"='\t'),
                  'Comma(.csv)'),
    
    tags$hr(),
    
    #quotation used in data file
    radioButtons('datQuote',
                 tags$h5(tags$strong('Quotes used in data file')),
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
      #0. Introduction to Redhyte
      tabPanel("0. Introduction to Redhyte",
               imageOutput("algoPNG",height="700px")),
      
      #=============================================#
      #==============1. Data preview================#
      #=============================================#
      
      tabPanel("1. Data preview",
               tags$h4("Displaying a preview of your data"),
               sliderInput("previewRows","Number of rows to display",
                           min=1,max=20,value=1,step=1,animate=TRUE),
               tableOutput("dataPreview")),
      
      #=============================================#
      #==============2. Data viz====================#
      #=============================================#
      
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
                 width=5)),
      
      #=============================================#
      #==============3. Initial test================#
      #=============================================#
      
      tabPanel("3. Initial test",
               navlistPanel(
                 tabPanel("Initial context",
                          sidebarPanel(
                            htmlOutput("targetCtrl"),
                            verbatimTextOutput("targetType"),
                            htmlOutput("tgtClassCtrl"),
                            htmlOutput("tgtAttrValueCtrl"),
                            width=4),
                          sidebarPanel(
                            htmlOutput("comparingCtrl"),
                            verbatimTextOutput("comparingType"),
                            htmlOutput("cmpClassCtrl"),
                            width=4)),
                 tabPanel("Table & test",
                          tableOutput("contTable"),
                          sidebarPanel(
                            tags$h6("Initial test"),
                            tableOutput("initialTest"))),
                 widths=c(3,9))),
      
      #=============================================#
      #============4. Test diagnostics==============#
      #=============================================#
      
      tabPanel("4. Test diagnostics"), #not implemented yet, will come back to this
      
      #=============================================#
      #==========5. Hypothesis mining===============#
      #=============================================#
      
      tabPanel("5. Hypothesis mining",
               tags$h6("Redhyte's hypothesis mining implementation works by first constructing two
                       random forest models, using the context attributes to predict the
                       target and comparing attributes."),
               tableOutput("testRF"))

    )#end tabset panel
  ) #end main panel
))#end shiny app