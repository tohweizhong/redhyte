require(shinyIncubator)
require(shiny)

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
    checkboxInput("datRownames","Row names present in file",FALSE),
    tags$hr(),
    #file type
    radioButtons('datSep',
                 tags$h5(tags$strong("Separator")),
                 c("Comma(.csv)"=',',
                   "Tab(.txt/.tsv)"='\t'),
                  ','),
    tags$hr(),
    #quotation used in data file
    radioButtons('datQuote',
                 tags$h5(tags$strong('Quotes used in data file')),
                 c(None='',
                   'Double Quotes'='"',
                   'Single Quotes'="'"),
                 '"'),
    tags$hr(),
    #transpose data
    checkboxInput('datTranspose','Transpose data?',FALSE),
    width=3), #end sidebar panel
  
  #main panel
  mainPanel(
    
    tabsetPanel(
      #=============================================#
      #=============0. Getting started==============#
      #=============================================#
      tabPanel("0. Getting started",
               imageOutput("algoPNG",height="700px")),
      
      #=============================================#
      #==============1. Data preview================#
      #=============================================#
      
      tabPanel("1. Data preview",
               tags$h4("Displaying a preview of your data"),
               sliderInput("previewRows","Number of rows to display",
                           min=1,max=20,value=10,step=1,animate=FALSE),
               tableOutput("data.preview")),
      
      #=============================================#
      #==============2. Data viz====================#
      #=============================================#
      
      tabPanel("2. Data visualization",
               tags$h4("Simple exploratory data analysis"),
               navlistPanel(
                 tabPanel("Select attributes",
                          sidebarPanel(
                            htmlOutput("viz.ctrl1"),
                            verbatimTextOutput("viz.type1"),
                            htmlOutput("viz.ctrl2"),
                            verbatimTextOutput("viz.type2")),
                          width=4),
                 tabPanel("Univariate",
                          tableOutput("viz.tukeyfive1"),
                          plotOutput("viz.hist1"),
                          #plotOutput("viz.boxplot1"),
                          tableOutput("viz.tukeyfive2"),
                          plotOutput("viz.hist2")),
                          #plotOutput("viz.boxplot2")),
                 tabPanel("Relationship",
                          plotOutput("viz.scatterplot")),
                 tabPanel("Multivariate",
                          tags$h6("PCA score plot?")),
                 widths=c(3,9))),
      
      #=============================================#
      #==============3. Initial test================#
      #=============================================#
      
      tabPanel("3. Initial test",
               tags$h4("Initial test"),
               tags$h5("Your hypothesis:"),
               verbatimTextOutput("hypothesis.statement.it"),
               navlistPanel(
                 tabPanel("Target attribute",
                          sidebarPanel(
                            htmlOutput("test.tgt.ctrl"),
                            verbatimTextOutput("test.tgt.type"),
                            width=4),
                          sidebarPanel(
                            htmlOutput("test.tgt.class.ctrl1"),
                            width=4),
                          sidebarPanel(
                            htmlOutput("test.tgt.class.ctrl2"),
                            width=4)),
                 tabPanel("Comparing attribute",
                          sidebarPanel(
                            htmlOutput("test.cmp.ctrl"),
                            verbatimTextOutput("test.cmp.type"),
                            width=4),
                          sidebarPanel(
                            htmlOutput("test.cmp.class.ctrl1"),
                            width=4),
                          sidebarPanel(
                            htmlOutput("test.cmp.class.ctrl2"),
                            width=4)),
                 tabPanel("Initial context",
                          sidebarPanel(
                            htmlOutput("test.ctx.ctrl"),
                            width=4),
                          sidebarPanel(
                            htmlOutput("test.ctx.item.ctrl"),
                            width=8)),
                 tabPanel("Table(s) & test",
                          tableOutput("contTable"),
                          tableOutput("initialTest"),
                          tableOutput("contTable2"),
                          tableOutput("initialTest2")),
                 widths=c(3,9))),
      
      #=============================================#
      #============4. Contexted data================#
      #=============================================#
      
      tabPanel("4. Contexted Data",
               tags$h4("Displaying a preview of the contexted data"),
               sliderInput("ctxRows","Number of rows to display",
                           min=1,max=20,value=10,step=1,animate=FALSE),
               tableOutput("ctx.data")),
      
      #=============================================#
      #============5. Test diagnostics==============#
      #=============================================#
      
      tabPanel("5. Test diagnostics",
               tags$h4("Test diagnostics"),
               tags$h6("Objectives:"),
               tags$h6("-> if Tinitial is t-test, check parametric assumptions"),
               tags$h6("-> if Tinitial is collapsed chi-squared test, find top contributor in the Acmp classes"),
               tags$h5("Your hypothesis:"),
               verbatimTextOutput("hypothesis.statement.td"),
               navlistPanel(
                 tabPanel("t-test",
                          tableOutput("KStest"),
                          tableOutput("Ftest"),
                          tableOutput("MWtest")),
                 tabPanel("Chi-squared test",
                          tableOutput("flat.table"),
                          tableOutput("flat.chi.sq"),
                          tableOutput("chi.sq.top")))),
      #half implemented, will come back to this
      
      #=============================================#
      #============6. Context mining================#
      #=============================================#
      
      tabPanel("6. Context mining",
               tags$h4("Context mining"),
               tags$h6("Redhyte's hypothesis mining implementation works by first constructing two
                       random forest models, using all other attributes in the data to predict the target and comparing attributes.
                       For each of these two models, Redhyte extract the top k attributes that contribute
                       to the classification of target and/or comparing attributes, if the model(s) is accurate.
                       Confusion matrices of the models will be displayed, as well as the list of mined context attributes."),
               tags$h5("Initial hypothesis: "),
               verbatimTextOutput("hypothesis.statement.cm"),
               navlistPanel(
                 tabPanel("Attributes to exclude",
                          htmlOutput("attr.to.exclude")),
                 tabPanel("Mined context attributes",
                          progressInit(),
                          verbatimTextOutput("run.time.tgt"),
                          verbatimTextOutput("run.time.cmp"),
                          tableOutput("testRF1"),
                          tableOutput("testRF2"),
                          tableOutput("testRF3")),
                 tabPanel("Visualization",
                          tableOutput("contTable.ctx"),
                          tableOutput("contTable2.ctx"),
                          sidebarPanel(
                            htmlOutput("minedAttrCtrl")),
                          plotOutput("mined.attr.viz")),
                 widths=c(3,9),selected="Attributes to exclude")),
      
      #=============================================#
      #===========7. Hypothesis mining==============#
      #=============================================#
      
      tabPanel("7. Hypothesis mining",
               tags$h4("Hypothesis mining"),
               tableOutput("hypotheses")),
      
      #=============================================#
      #===========8. Hypothesis analysis============#
      #=============================================#
      
      tabPanel("8. Hypothesis analysis",
               tags$h4("Hypothesis analysis"),
               navlistPanel(
                 tabPanel("Select context item",
                          sidebarPanel(
                            htmlOutput("analyse.ctrl"),
                            htmlOutput("analyse.sort.ctrl.one"),
                            htmlOutput("analyse.sort.ctrl.two"),
                            width=4),
                          tableOutput("analyse.hypothesis")),
                 tabPanel("Analysis",
                          tags$h5("Initial hypothesis:"),
                          verbatimTextOutput("analyse.hypothesis.statement.initial"),
                          tableOutput("analyse.contTable"),
                          tableOutput("analyse.initialTest"),
                          tags$h5("Mined hypothesis:"),
                          verbatimTextOutput("analyse.hypothesis.statement"),
                          tableOutput("analyse.cont.tab"),
                          tableOutput("analyse.test")),
               widths=c(3,9))),
      
      #=============================================#
      #=============9. Session log==================#
      #=============================================#
      
      tabPanel("9. Session log",
               tableOutput("session.log"))
      
    )#end tabset panel
  ) #end main panel
))#end shiny app