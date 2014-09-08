require(shiny)
require(randomForest)
source("related_codes/settings.R")

shinyServer(function(input,output){
  
  #fancy work here
  #render images
  output$titlePNG<-renderImage({
    list(src="images/title.png", alt=NULL)
  },deleteFile=FALSE)
  
  output$logoPNG<-renderImage({
    list(src="images/logo.png", alt=NULL)
  },deleteFile=FALSE)
  
  output$algoPNG<-renderImage({
    list(src="images/redhyte algo.png",alt=NULL)
  },deleteFile=FALSE)
  
  #=============================================#
  #=============================================#
  #=============================================#
  
  #grabbing the data
  
  #===============REACTIVE======================#

  #Data() consists of *THREE* things at the moment
  # 1. Data()[[1]] is the data itself
  # 2. Data()[[2]] is the type of variable: continuous or categorical
  # 3. Data()[[3]] is the number of classes for categorical attributes, NA for cont.
  
  #here begins the real work..
  
  Data<-reactive({
    datFile<-input$datFile #still just the file name
    #if (is.null(datFile)) return(NULL)
    
    path<-as.character(datFile$datapath)
    
    df<-read.csv(path,
                 header=input$datHeader,
                 sep=input$datSep,
                 quote=input$datQuote,
                 stringsAsFactors=F)
    
    print(str(df))
    print(str(datFile))
    print(datFile$datapath)
    #checking the variable type of the attributes: continuous or categorical
    #and number of classes for cate. attr.
    typ<-NULL
    numCl<-NULL
    for(i in seq(ncol(df))){
      if(is.numeric(df[,i])){
        typ<-c(typ,"Cont")
        numCl<-c(numCl,NA)
      }
      else{
        typ<-c(typ,"Cate")
        numCl<-c(numCl,length(unique(df[,i])))
      }
    }
    names(typ)<-names(numCl)<-colnames(df)
    return(list(df,typ,numCl))
  })
  
  #=============================================#
  #==============1. Data preview================#
  #=============================================#
  
  #first tab panel: "1. Data preview"
  
  #displaying a preview of the data
  #10 rows, all columns
  output$dataPreview<-renderTable({
    if (is.null(Data()[1])) return(NULL)
    data.frame(Data()[[1]][1:input$previewRows,])
  },digits=3)
  
  #=============================================#
  #==============2. Data viz====================#
  #=============================================#
  
  #second tab panel: "2. Data visualization"
  
  #create two dropdown boxes to choose two attributes,
  #display
  # 1. Type of attribute (continuous or categorical)
  # 2. Tukey's five number summary or counts
  # 3. histogram or barplot
  # 4. boxplots
  # 5. scatterplot for simple eda
  output$edaCtrl1<-renderUI({
    selectizeInput("whichAttr1","Select attribute to visualize",colnames(Data()[[1]]))
  })
  
  output$edaCtrl2<-renderUI({
    selectizeInput("whichAttr2","Select attribute to visualize",colnames(Data()[[1]]))
  })
  
  #display type of attribute: continuous or categorical
  output$type1<-renderText({
    if(Data()[[2]][input$whichAttr1]=="Cont")
      type<-"Type: Continuous"
    else type<-"Type: Categorical"
    type
  })
  
  output$type2<-renderText({
    if(Data()[[2]][input$whichAttr2]=="Cont")
      type<-"Type: Continuous"
    else type<-"Type: Categorical"
    type
  })
  
  #display boxplot stats (Tukey's five) if cont,
  #else display frequencies
  output$tukeyfive1<-renderTable({
    if(Data()[[2]][input$whichAttr1]=="Cont"){
      qt<-as.data.frame(fivenum(Data()[[1]][,input$whichAttr1]))
      rownames(qt)<-c("Min","25%","Median","75%","Max")
      colnames(qt)<-NULL
      qt
    }
    else{
      tb<-data.frame(table(Data()[[1]][,input$whichAttr1]))
      colnames(tb)<-c(input$whichAttr1,"Frequency")
      tb
    }

  })
  
  output$tukeyfive2<-renderTable({
    if(Data()[[2]][input$whichAttr2]=="Cont"){
      qt<-as.data.frame(fivenum(Data()[[1]][,input$whichAttr2]))
      rownames(qt)<-c("Min","25%","Median","75%","Max")
      colnames(qt)<-NULL
      qt
    }
    else{
      tb<-data.frame(table(Data()[[1]][,input$whichAttr2]))
      colnames(tb)<-c(input$whichAttr2,"Frequency")
      tb
    }
  })
  
  #plotting histograms or barcharts
  output$hist1<-renderPlot({
    if(Data()[[2]][input$whichAttr1]=="Cont")
      hist(Data()[[1]][,input$whichAttr1],main="",xlab="")
    else{
      tb<-data.frame(table(Data()[[1]][,input$whichAttr1]))
      colnames(tb)<-c(input$whichAttr1,"Frequency")
      barplot(tb$Frequency)
    }
      
  })
  
  output$hist2<-renderPlot({
    if(Data()[[2]][input$whichAttr2]=="Cont")
      hist(Data()[[1]][,input$whichAttr2],main="",xlab="")
    else{
      tb<-data.frame(table(Data()[[1]][,input$whichAttr2]))
      colnames(tb)<-c(input$whichAttr2,"Frequency")
      barplot(tb$Frequency)
    }
  })
  
  #plotting boxplots
  output$boxplot1<-renderPlot({
    if(Data()[[2]][input$whichAttr1]=="Cont")
      boxplot(Data()[[1]][,input$whichAttr1])
  })
  
  output$boxplot2<-renderPlot({
    if(Data()[[2]][input$whichAttr2]=="Cont")
      boxplot(Data()[[1]][,input$whichAttr2])
  })
  
  #plotting scatterplot
  output$scatterplot<-renderPlot({
    plot(Data()[[1]][,input$whichAttr2]~
           Data()[[1]][,input$whichAttr1],
         xlab=input$whichAttr1,
         ylab=input$whichAttr2)
  })
  
  #=============================================#
  #==============3. Initial test================#
  #=============================================#
    
  #third tab panel: "3. Initial test"
  
  #dropdown boxes to select tgt and cmp attr
  output$targetCtrl<-renderUI({
    selectizeInput("targetAttr",
                   "Indicate target attribute (May be continuous or categorical)",
                   colnames(Data()[[1]]))
  }) #return: input$targetAttr
  
  output$comparingCtrl<-renderUI({
    selectizeInput("comparingAttr",
                   "Indicate comparing attribute (Must be categorical)",
                   colnames(Data()[[1]]))
  }) #return: input$comparingAttr

  output$tgtAttrValueCtrl<-renderUI({
    if(Data()[[2]][input$targetAttr] == "Cate"){
      selectizeInput("tgtAttrValue",
                     "Indicate target attribute value",
                     unique(Data()[[1]][,input$targetAttr]))
    }
    else{
      selectizeInput("tgtAttrValue",
                     "Indicate target attribute value",
                     "NA")
    }
  })
  
  #checkboxes to select classes of Atgt and Acmp to form starting ctx
  output$tgtClassCtrl<-renderUI({
    if(Data()[[2]][input$targetAttr]=="Cate"){
      checkboxGroupInput("whichtgtclasses",
                         "Indicate which target attribute classes to use as part of initial context",
                         choices=c("Use all classes",unique(Data()[[1]][,input$targetAttr])),
                         selected="Use all classes")
    }
  }) #return: input$whichtgtclasses

  output$cmpClassCtrl<-renderUI({
    if(Data()[[2]][input$comparingAttr]=="Cate"){
      checkboxGroupInput("whichcmpclasses",
                         "Indicate which comparing attribute class to use as part of initial context",
                         choices=c("Use all classes",unique(Data()[[1]][,input$comparingAttr])),
                         selected="Use all classes")
    }
  }) #return: input$whichcmpclasses
 
  #display type of attribute: continuous or categorical
  output$targetType<-renderText({
    if(Data()[[2]][input$targetAttr]=="Cont")
      type<-"Type: Continuous"
    else type<-"Type: Categorical"
    type
  })
  
  output$comparingType<-renderText({
    if(Data()[[2]][input$comparingAttr]=="Cont")
      type<-"Type: Continuous"
    else type<-"Type: Categorical"
    type
  })

  #===============REACTIVE======================#

  #Because Data() contains information regarding the attributes,
  #while Data2() considers starting context formed by specific
  #classes of Atgt and Acmp, information in Data() can be re-used,
  #except Data()[[3]].
  #Data()[[3]] contains the number of classes for each categorical attribute,
  #which changes for Atgt and Acmp
  #030914: will return Data()[[[3]]] as it is anyway for now.

  #Data2() consists of *FOUR* things at the moment
  # 1. Data2()[[1]] is the data itself
  # 2. Data2()[[2]] is the type of variable: continuous or categorical
  # 3. Data2()[[3]] is the number of classes for categorical attributes, NA for cont.
  # 4. Data2()[[4]] is the ctxFlag, indicating if a starting context is being used
  
  Data2<-reactive({
    
    dfWithCtx<-Data()[[1]]
    
    #assuming no starting ctx yet
    rowsToUse<-seq(nrow(dfWithCtx))
    ctxFlag<-FALSE
    
    #consider the type of Atgt and Acmp
    #starting context can only be considered for categorical attributes,
    #ie. Atgt must be categorical while Acmp is already categorical
    #both`Atgt and Acmp are categorical
    if(Data()[[2]][input$targetAttr] == "Cate" && Data()[[2]][input$comparingAttr] == "Cate"){
      #use all?
      if(input$whichtgtclasses == "Use all classes" && input$whichcmpclasses == "Use all classes")
        rowsToUse<-seq(nrow(dfWithCtx)) #all rows
      else if(input$whichtgtclasses == "Use all classes" && input$whichcmpclasses != "Use all classes"){
        rowsToUse<-which(dfWithCtx[,input$comparingAttr] %in% input$whichcmpclasses == TRUE) # <------ PROBLEM IS HERE
        #060914: PROBLEM RESOLVED
        ctxFlag<-TRUE
      }
      else if(input$whichtgtclasses != "Use all classes" && input$whichcmpclasses == "Use all classes"){
        rowsToUse<-which(dfWithCtx[,input$targetAttr] %in% input$whichtgtclasses == TRUE) # <------ PROBLEM IS HERE
        ctxFlag<-TRUE
      }
      else{
        rowsToUse<-intersect(which(dfWithCtx[,input$targetAttr] %in% input$whichtgtclasses == TRUE),
                             which(dfWithCtx[,input$comparingAttr] %in% input$whichcmpclasses ==  TRUE)) # <------ PROBLEM IS HERE
        ctxFlag<-TRUE
      }
    }
    
    #Only Atgt is categorical and Acmp is not
    #(does not comply to Redhyte's algorithm)
    else if(Data()[[2]][input$targetAttr] == "Cate" && Data()[[2]][input$comparingAttr] != "Cate"){
      if(input$whichtgtclasses == "Use all classes")
        rowsToUse<-seq(nrow(dfWithCtx)) #all rows
      else if(input$whichtgtclasses != "Use all classes"){
        rowsToUse<-which(dfWithCtx[,input$targetAttr] == input$whichtgtclasses) # <------ PROBLEM IS HERE
        ctxFlag<-TRUE
      }
    }
    
    #only Acmp is categorical and Atgt is not
    else if(Data()[[2]][input$targetAttr] != "Cate" && Data()[[2]][input$comparingAttr] == "Cate"){
      if(input$whichcmpclasses == "Use all classes")
        rowsToUse<-seq(nrow(dfWithCtx)) #all rows
      else if(input$whichcmpclasses != "Use all classes"){
        rowsToUse<-which(dfWithCtx[,input$comparingAttr] %in% input$whichcmpclasses == TRUE) # <------ PROBLEM IS HERE
        ctxFlag<-TRUE
      }
    }
    
    #retrieve the row numbers of the row to be used in subsequent analysis,
    #forming the starting context
    dfWithCtx<-dfWithCtx[rowsToUse,]
#     print(rowsToUse)
#     print(is.null(rowsToUse))
#     print(str(rowsToUse))
#     print(tail(rowsToUse))
#     print(str(dfWithCtx))
    return(list(dfWithCtx,Data()[[2]],Data()[[3]],ctxFlag))
    #Data2()[[3]] is incorrect for now. refer to comments above
  })
  
#   #for testing Data2()
#   output$testData2<-renderTable({
#     if (is.null(Data2()[1])) return(NULL)
#     rowsToDisplay<-20
#     if(rowsToDisplay > 0.5*nrow(Data2()[[1]])) rowsToDisplay<-0.5*nrow(Data2()[[1]])
#     data.frame(Data2()[[1]][1:rowsToDisplay,])
#   },digits=3)

  #generate contingency table if target attribute is cate.
  #else generate a comparison table

  #===============REACTIVE======================#

  #Table() consists of *FOUR* things at the moment
  # 1. Table()[[1]] is the table itself, be it contingency or comparison table
  # 2. Table()[[2]] is the type of table: contingency or comparison
  # 3. Table()[[3]] is the number of classes for the categorical Acmp
  # 4. Table()[[4]] is the data with the starting context itself
  
  #reactive wrapper for table
  Table<-reactive({
    #retrieve the relevant data
    
    if(Data2()[[4]] == FALSE){ #no starting ctx
    
      df<-Data()[[1]][c(input$targetAttr,input$comparingAttr)]
      
      #is the target attribute continuous or categorical?
      if(Data()[[2]][input$targetAttr] == "Cate")
        return(list(table(df),
                    "Contingency",
                    length(unique(df[,input$comparingAttr])),
                    df))
      
      else{ #target attribute is continuous
        cl<-unique(df[,input$comparingAttr]) #all classes of comparing attribute
        means<-NULL
        for(i in seq(length(cl))){
          #for a given class of the comparing attribute,
          #compute the mean value of the target attribute
          whichones<-which(df[,2] == cl[i]) #2nd column is comparing attribute
          means<-c(means,mean(df[whichones,1])) #1st column is target atrribute
        }
        names(means)<-cl
        tmp<-data.frame(means)
        colnames(tmp)<-paste("means of ",input$targetAttr,sep="")
        return(list(tmp,
                    "Comparison",
                    length(cl),
                    df))
      }
    }
    
    else if(Data2()[[4]] == TRUE){ #there is a starting ctx
      
      df<-Data2()[[1]][c(input$targetAttr,input$comparingAttr)]
      #is the target attribute continuous or categorical?
      if(Data2()[[2]][input$targetAttr] == "Cate")
        return(list(table(df),
                    "Contingency",
                    length(unique(df[,input$comparingAttr])),
                    df))
      
      else{ #target attribute is continuous
        cl<-unique(df[,input$comparingAttr]) #all classes of comparing attribute
        means<-NULL
        for(i in seq(length(cl))){
          #for a given class of the comparing attribute,
          #compute the mean value of the target attribute
          whichones<-which(df[,2] == cl[i]) #2nd column is comparing attribute
          means<-c(means,mean(df[whichones,1])) #1st column is target atrribute
        }
        names(means)<-cl
        tmp<-data.frame(means)
        colnames(tmp)<-paste("means of ",input$targetAttr,sep="")
        return(list(tmp,
                    "Comparison",
                    length(cl),
                    df))
      }
    }
  })

  output$contTable<-renderTable({
    Table()[[1]]
  })

  #initial parametric test
  output$initialTest<-renderTable({

    #check the type of table
    if(Table()[[2]] == "Contingency"){
      test<-chisq.test(Table()[[1]]) #chisq.test() works on the table itself
      stats<-test$statistic
      pvalue<-test$p.value
      method<-test$method
      
      returnMe<-as.data.frame(c(as.character(method),
                                 as.character(round(stats,3)),
                                 as.character(round(pvalue,7))
                                 ))
      rownames(returnMe)<-c("Method","Test statistic","p-value")
      colnames(returnMe)<-NULL
      returnMe
    }
    else if(Table()[[2]] == "Comparison"){
      if(Table()[[3]] == 2){ #t-test
        test<-t.test(Table()[[1]]) #Table()[[1]] is a data.frame of means
        stats<-test$statistic
        pvalue<-test$p.value
        method<-test$method
        
        returnMe<-as.data.frame(c(as.character(method),
                                  as.character(round(stats,3)),
                                  as.character(round(pvalue,7))
        ))
        rownames(returnMe)<-c("Method","Test statistic","p-value")
        colnames(returnMe)<-NULL
        returnMe
      }
      else{ #anova
        
        test<-aov(data=Table()[[4]],formula=as.formula(paste(input$targetAttr,"~",input$comparingAttr)))
        stats<-summary(test)[[1]]$"F value"[1]
        pvalue<-summary(test)[[1]]$"Pr(>F)"[1]
        method<-"Analysis of variance (AOV)"
        
        returnMe<-as.data.frame(c(as.character(method),
                                  as.character(round(stats,3)),
                                  as.character(round(pvalue,7))))
        rownames(returnMe)<-c("Method","Test statistic","p-value")
        colnames(returnMe)<-NULL
        returnMe
      }
    }
  })
  #=============================================#
  #============4. Test diagnostics==============#
  #=============================================#

  #test diagnostics for t-test and ANOVA only  
  
  #===============REACTIVE======================#
  Test<-reactive({
    if(Table()[[2]] == "Contingency") return("t-test")
    else if(Table()[[3]] == 2) return("chisq-test")
    else if(Table()[[3]] > 2) return("anova")
  })

  #=============================================#
  #==========5. Hypothesis mining===============#
  #=============================================#

  #first step: build the two RF models
  minedAttributes<-reactive({
    df<-Data2()[[1]]
    
    #need to convert the character attributes to factors first before building models
    whichchar<-which(Data2()[[2]] == "Cate")
    df[,whichchar]<-lapply(df[,whichchar],factor)
    df<-df[1:nrow(df),1:ncol(df)]
    #print(str(df))
    #print(summary(df))
    modtgt<-randomForest(data=df,
                         formula=as.formula(paste(input$targetAttr,"~.",sep="")),
                         importance=T)
    modcmp<-randomForest(data=df,
                         formula=as.formula(paste(input$comparingAttr,"~.",sep="")),
                         importance=T)
    
    return(list(modtgt$confusion,modcmp$confusion))
    
    #compute accuracies
  })
  output$testRF<-renderTable({
    minedAttributes()[[1]]
  })
})