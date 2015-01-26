source("settings.R")
library(shiny)
library(shinyIncubator)
library(shinythemes)
library(randomForest)

shinyUI(fluidPage(
  #theme = shinytheme("cerulean"),
  #theme = shinytheme("cosmo"),
  #theme = shinytheme("united"),
  theme = "lumen.css",
  
  titlePanel(imageOutput("logoPNG",width="180px",height="60px"),
             tags$head(tags$link(rel="icon",type="image/png",href="browserlogo.png",sizes="64x64"),
                       tags$title("Redhyte"))),
  
  #main panel
  mainPanel(
    tabsetPanel(id="theTabs",
                
                #=============================================#
                #=============0. Settings=====================#
                #=============================================#
                
                tabPanel("0. Settings",
                         sidebarLayout(
                           sidebarPanel(
                             imageOutput("titlePNG",
                                         height="70px"),
                             tags$hr(),
                             #data input
                             fileInput('datFile',
                                       tags$h5(tags$strong('Choose file to analyse')),
                                       accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                             tags$h5("Example dataset to try out Redhyte with:"),
                             tags$a(href="https://dl.dropboxusercontent.com/u/36842028/linkouts/datasets/adult.txt",
                                    "US Census dataset",target="_blank"),
                             tags$hr(),
                             #checkbox to indicate header == true or false
                             checkboxInput('datHeader','Header contains attribute names', TRUE),
                             #checkboxInput("datRownames","Row names present in file",FALSE),
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
                             width=3),
                           mainPanel(
                             tags$h4("Settings used in Redhyte"),
                             tags$h5("Default settings are suitable for most purposes"),
                             tags$hr(),
                             sliderInput("p.significant",label ="p-value for test diagnostics",
                                         min=0,max=0.15,value=0.05,step=0.05),
                             sliderInput("acc.rf.default",label="Classification accuracy for context mining",
                                         min=0,max=1,value=0.7,step=0.05),
                             sliderInput("top.k",label="Number of mined context attributes to shortlist",
                                         min=1,max=10,value=5,step=1),
                             sliderInput("class.ratio",label="Class ratio threshold for class-imbalance learning",
                                         min=2,max=5,value=3,step=1),
                             sliderInput("min.sup.cij",label="Minimum cell support for mined hypotheses",
                                         min=5,max=20,value=10,step=1)))),
                
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
                
                tabPanel("2. Data viz",
                         tags$h4("Simple exploratory data analysis"),
                         navlistPanel(id="viz",
                                      tabPanel("Select attributes",
                                               sidebarPanel(
                                                 htmlOutput("viz.ctrl1"),
                                                 verbatimTextOutput("viz.type1"),
                                                 htmlOutput("viz.ctrl2"),
                                                 verbatimTextOutput("viz.type2")),
                                               width=4),
                                      tabPanel("Distributions",
                                               tableOutput("viz.tukeyfive1"),
                                               plotOutput("viz.hist1"),
                                               #plotOutput("viz.boxplot1"),
                                               tableOutput("viz.tukeyfive2"),
                                               plotOutput("viz.hist2")),
                                      #plotOutput("viz.boxplot2")),
                                      tabPanel("Relationship",
                                               plotOutput("viz.scatterplot")),
                                      #tabPanel("Multivariate",
                                      #tags$h6("PCA score plot?")),
                                      widths=c(2,10))),
                
                #=============================================#
                #==============3. Initial test================#
                #=============================================#
                
                tabPanel("3. Initial test",
                         tags$h4("Set up your initial hypothesis and test"),
                         tags$h5("Your hypothesis:"),
                         verbatimTextOutput("hypothesis.statement.it"),
                         navlistPanel(id="initial",
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
                                      tabPanel("Table(s) & test(s)",
                                               h5(textOutput("text.comp.or.cont")),
                                               tableOutput("contTable"),
                                               h5(textOutput("text.initial.test")),
                                               tableOutput("initialTest"),
                                               h5(textOutput("text.comp.or.cont2")),
                                               tableOutput("contTable2"),
                                               h5(textOutput("text.initial.test2")),
                                               tableOutput("initialTest2")),
                                      widths=c(2,10))),
                
                #=============================================#
                #============4. Contexted data================#
                #=============================================#
                
                tabPanel("4. Contexted data",
                         tags$h4("Displaying a preview of the contexted data"),
                         sliderInput("ctxRows","Number of rows to display",
                                     min=1,max=20,value=10,step=1,animate=FALSE),
                         downloadButton("ctx.download","Download contexted data"),
                         tags$hr(),
                         tableOutput("ctx.data")),
                
                #=============================================#
                #============5. Test diagnostics==============#
                #=============================================#
                
                tabPanel("5. Test diagnostics",
                         tags$h4("Test diagnostics"),
                         tags$h5("Objectives:"),
                         tags$h5("-> if initial test is a t-test, i) check parametric assumptions and ii) find chi-squared top contributor if valid"),
                         tags$h5("-> if initial test is a collapsed chi-squared test, find top contributor amongst the classes of the comparing attribute"),
                         tags$h5("-> for both t-test and collapsed chi-squared test, use Cochran-Mantel-Haenszel Chi-Squared Test to identify potential confounders"),
                         tags$h4("Your hypothesis:"),
                         verbatimTextOutput("hypothesis.statement.td"),
                         navlistPanel(id="diag",
                                      tabPanel("Continuous target attribute",
                                               h4(textOutput("text.SWtest")),
                                               tableOutput("SWtest.cmp1"),
                                               tableOutput("SWtest.cmp2"),
                                               h4(textOutput("text.Ftest")),
                                               tableOutput("Ftest"),
                                               h4(textOutput("text.MWtest")),
                                               tableOutput("MWtest"),
                                               h4(textOutput("text.flat.table.num")),
                                               tableOutput("flat.table.num"),
                                               h4(textOutput("text.flat.chi.sq.num")),
                                               tableOutput("flat.chi.sq.num"),
                                               h4(textOutput("text.chi.sq.top.contributor")),
                                               tableOutput("chi.sq.top.contributor"),
                                               h4(textOutput("text.MHtest.num")),
                                               tableOutput("MHtest.num")),
                                      tabPanel("Categorical target attribute",
                                               h4(textOutput("text.flat.table.cate")),
                                               tableOutput("flat.table.cate"),
                                               h4(textOutput("text.flat.chi.sq.cate")),
                                               tableOutput("flat.chi.sq.cate"),
                                               h4(textOutput("text.chi.sq.top.cate")),
                                               tableOutput("chi.sq.top.cate"),
                                               h4(textOutput("text.MHtest.cate")),
                                               tableOutput("MHtest.cate")),widths=c(2,10))),
                
                #=============================================#
                #============6. Context mining================#
                #=============================================#
                
                tabPanel("6. Context mining",
                         tags$h4("Context mining"),
                         tags$h5("Redhyte's hypothesis mining implementation works by first constructing two
                       random forest models, using all other attributes in the data to predict the target and comparing attributes.
                       For each of these two models, Redhyte extract the top attributes that contribute
                       to the classification of target and/or comparing attributes, if the model(s) is/are accurate."),
                         tags$h5("Confusion matrices of the models, a list of mined context attributes, random forest variable importance plots 
                       and stratified histograms of the mined context attributes are displayed after mining."),
                         tags$h4("Initial hypothesis: "),
                         verbatimTextOutput("hypothesis.statement.cm"),
                         navlistPanel(id="ctx",
                                      tabPanel("Attributes to exclude",
                                               htmlOutput("attr.to.exclude")),
                                      tabPanel("Mined context attributes",
                                               #checkboxInput("start.ctx.mining", label = "Start context mining?", value = FALSE),
                                               progressInit(),
                                               verbatimTextOutput("run.time.tgt"),
                                               verbatimTextOutput("run.time.cmp"),
                                               tags$h5("Confusion matrix of target model"),
                                               tableOutput("cm.tgt"),
                                               tags$h5("Confusion matrix of comparing model"),
                                               tableOutput("cm.cmp"),
                                               tableOutput("minedAttr")),
                                      tabPanel("Variable Importance",
                                               plotOutput("VIplot.tgt"),
                                               plotOutput("VIplot.cmp")),
                                      tabPanel("Visualization",
                                               tableOutput("contTable.ctx"),
                                               tableOutput("contTable2.ctx"),
                                               sidebarPanel(
                                                 htmlOutput("minedAttrCtrl")),
                                               plotOutput("mined.attr.viz")),
                                      widths=c(2,10),selected="Attributes to exclude")),
                
                #=============================================#
                #===========7. Hypothesis mining==============#
                #=============================================#
                
                tabPanel("7. Hypothesis mining",
                         tags$h4("Analysis of mined hypotheses"),
                         tags$h5("From the mined context attributes, Redhyte considers every possible context item 
                        and includes each item in the initial hypothesis to form mined hypotheses.
                        Each mined hypothesis is then evaluated using various metrics."),
                         navlistPanel(id="hypo",
                                      tabPanel("Mined hypotheses",
                                               tableOutput("hypotheses")),
                                      tabPanel("Select context item",
                                               sidebarPanel(
                                                 htmlOutput("analyse.sort.ctrl.one"),
                                                 htmlOutput("analyse.sort.ctrl.two"),
                                                 htmlOutput("analyse.ctrl"),
                                                 width=4),
                                               tableOutput("analyse.summary"),
                                               tableOutput("analyse.hypothesis")),
                                      tabPanel("Hypothesis analysis",
                                               tags$h5("Initial hypothesis:"),
                                               verbatimTextOutput("analyse.hypothesis.statement.initial"),
                                               tableOutput("analyse.contTable"),
                                               tableOutput("analyse.initialTest"),
                                               tags$h5("Mined hypothesis:"),
                                               verbatimTextOutput("analyse.hypothesis.statement"),
                                               tableOutput("analyse.cont.tab"),
                                               tableOutput("analyse.test"),
                                               h5(textOutput("text.analyse.flat.table")),
                                               tableOutput("analyse.flat.table"),
                                               h5(textOutput("text.analyse.flat.chi.sq")),
                                               tableOutput("analyse.flat.chi.sq"),
                                               h5(textOutput("text.analyse.chi.sq.top.cont")),
                                               tableOutput("analyse.chi.sq.top.cont")),
                                      tabPanel("Hypothesis mining metrics",
                                               htmlOutput("analyse.plot.metric.ctrl.one"),
                                               htmlOutput("analyse.plot.metric.ctrl.two"),
                                               plotOutput("analyse.metric.plot")),
                                      widths=c(2,10))),
                
                #=============================================#
                #=============8. Session log==================#
                #=============================================#
                
                tabPanel("8. Log",
                         tags$h4("Analysis session log"),
                         downloadButton("log.download","Download session log"),
                         tags$hr(),
                         tableOutput("session.log")),
                
                #=============================================#
                #=============9. About Redhyte================#
                #=============================================#
                
                tabPanel("9. About Redhyte",
                         uiOutput("about.text"))
    )#end tabset panel
    ,width=12) #end main panel, sidebarLayout
))#end fluidPage, end shinyUI