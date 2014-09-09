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
  
  #***************REACTIVE**********************#

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
    #this part is ok, based on the response I got from stackoverflow
    #the NULL that appears after
    
    #print(str(df))
    
    #is apparently the property of print.default()

    #checking the variable type of the attributes: continuous or categorical
    #and number of classes for cate. attr.
    typ<-NULL
    numCl<-NULL
    for(i in seq(ncol(df))){
      if(is.numeric(df[,i])){ # <--- POSSIBLE SOURCE OF BUG, BECAUSE THIS IS A LITTLE HARD-CODING HERE
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
  
  #*********************************************#
  
  #=============================================#
  #==============1. Data preview================#
  #=============================================#
  
  #displaying a preview of the data, 10 rows, all columns
  output$dataPreview<-renderTable({
    if (is.null(Data()[1])) return(NULL)
    data.frame(Data()[[1]][1:input$previewRows,])
  },digits=3)
  
  #=============================================#
  #==============2. Data viz====================#
  #=============================================#
  
  #create two dropdown boxes to choose two attributes, display:
  # 1. Type of attribute (continuous or categorical)
  # 2. Tukey's five number summary or counts
  # 3. histogram or barplot
  # 4. boxplots
  # 5. scatterplot for simple eda
  
  #selecting which attributes to visualise
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
  
  #dropdown boxes to select Atgt and Acmp
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

  #dropdown box to select vtgt
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
  }) #return: input$tgtAttrValue
  
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

  #***************REACTIVE**********************#
  
  #The objectives of Data2() are:
  # -> subsetting the data based on the user's initial context
  # -> if Atgt is continuous, include a binary attribute based on
  #    median(Atgt). This is done because it will speed up the
  #    construction of the RF models later. (Regression RF is
  #    apparently slower than classification RF.)
  
  #Because Data() contains information regarding the attributes,
  #while Data2() considers starting context formed by specific
  #classes of Atgt and Acmp, information in Data() can be re-used,
  #except Data()[[3]].
  #Data()[[3]] contains the number of classes for each categorical attribute,
  #which changes for Atgt and Acmp, depending on initial context
  #030914: will return Data()[[[3]]] as it is anyway for now.

  #Data2() consists of *FOUR* things at the moment
  # 1. Data2()[[1]] is the data itself, including the median cutoff attribute
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
    
    #lastly, add the median cutoff attribute if Atgt is continuous
    #this median value based on the data after considering Cinitial
    if(Data()[[2]][input$targetAttr] == "Cont"){
      m<-mean(dfWithCtx[,input$targetAttr])
      #using mean instead of median,
      #because median cannot handle extremely skewed data
      
      #print(unique(dfWithCtx[,input$targetAttr]))
      
      dfWithCtx$tgt.class<-sapply(dfWithCtx[,input$targetAttr],
                                  FUN=function(x){
                                    if(x>=m) return("High")
                                    else return("Low")})
    }
    
    #add the attribute type for the cutoff attribute if required
    attr.type<-Data()[[2]]
    if(ncol(dfWithCtx) > ncol(Data()[[1]])){ #meaning the cutoff attribute is added
      attr.type<-c(attr.type,"Cate")
    }
    
    return(list(dfWithCtx,attr.type,Data()[[3]],ctxFlag))
    #Data2()[[3]] is incorrect for now. refer to comments above
  })
  
  #*********************************************#
  
  #for testing Data2()
  output$testData2<-renderTable({
    if (is.null(Data2()[1])) return(NULL)
    rowsToDisplay<-10
    if(rowsToDisplay > 0.5*nrow(Data2()[[1]])) rowsToDisplay<-0.5*nrow(Data2()[[1]])
    data.frame(Data2()[[1]][1:rowsToDisplay,])
  },digits=3)

  #generate contingency table if target attribute is cate.
  #else generate a comparison table

  #***************REACTIVE**********************#

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

  #*********************************************#
  
  #render the comparison or contingency table
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
  #yet to implement non-parametric test yet
  
  #=============================================#
  #============4. Test diagnostics==============#
  #=============================================#

  #test diagnostics for t-test and ANOVA only  
  #not implemented yet
  
  #***************REACTIVE**********************#
  
  Test<-reactive({
    if(Table()[[2]] == "Contingency") return("t-test")
    else if(Table()[[3]] == 2) return("chisq-test")
    else if(Table()[[3]] > 2) return("anova")
  }) #not implemented yet
  
  #*********************************************#
  
  #=============================================#
  #==========5. Hypothesis mining===============#
  #=============================================#

  #first step: build the two RF models
  
  #***************REACTIVE**********************#
  
  minedAttributes<-reactive({
    df<-Data2()[[1]]
    
    #need to convert the character attributes to factors first before building models
    #initial data input options use stringsAsFactors=FALSE (see doc.txt)
    which.are.char<-which(Data2()[[2]] == "Cate")
    df[,which.are.char]<-lapply(df[,which.are.char],factor)
    
    str(df)
    
    #formulate the formulae for random forest models
    #start with Atgt
    if(!is.null(df$tgt.class)){
      #df$tgt.class has been defined, meaning Atgt is continuous
      #find all other context attributes
      ctx.col.names<-intersect(
        colnames(df)[which(colnames(df) != input$comparingAttr)],
        colnames(df)[which(colnames(df) != input$targetAttr)])
      
      ctx.col.names<-intersect(ctx.col.names,
        colnames(df)[which(colnames(df) != "tgt.class")])
      
      fm.tgt<-paste(" ",ctx.col.names,sep="",collapse="+")
      fm.tgt<-as.formula(paste("tgt.class","~",fm.tgt,sep=""))
    }
    else{
      ctx.col.names<-intersect(
        colnames(df)[which(colnames(df) != input$comparingAttr)],
        colnames(df)[which(colnames(df) != input$targetAttr)])
      
      fm.tgt<-paste(" ",ctx.col.names,sep="",collapse="+")
      fm.tgt<-as.formula(paste(input$targetAttr,"~",fm.tgt,sep=""))
    }
    
    #now formulate for Acmp
    fm.cmp<-paste(" ",ctx.col.names,sep="",collapse="+")
    fm.cmp<-as.formula(paste(input$comparingAttr,"~",fm.cmp,sep=""))

    print(fm.tgt)
    print(fm.cmp)
    
    #construct models
    mod.tgt<-randomForest(formula=fm.tgt,
                          data=df,
                          importance=TRUE)
    mod.cmp<-randomForest(formula=fm.cmp,
                          data=df,
                          importance=TRUE)
    
    #compute accuracies
    cm.tgt<-mod.tgt$confusion
    cm.cmp<-mod.cmp$confusion
    cm.tgt<-cm.tgt[,-ncol(cm.tgt)]
    cm.cmp<-cm.cmp[,-ncol(cm.cmp)]
    
    #090914: consider only the diagonal entries of the confusion matrices for now
    acc.tgt<-sum(diag(cm.tgt))/sum(cm.tgt)
    acc.cmp<-sum(diag(cm.cmp))/sum(cm.cmp)
    #this is severly affected by class-imbalance and multi-classes
    
    #compare the accuracies of the models with the default accuracy threshold
    #defined in related_codes/settings.R
    #3 cases:
    # -> acc.tgt >= acc.rf.default && acc.cmp >= acc.rf.default
    # -> acc.tgt >= acc.rf.default && acc.cmp <  acc.rf.default
    # -> acc.tgt <  acc.rf.default && acc.cmp >= acc.rf.default
    
    if(acc.tgt >= acc.rf.default && acc.cmp < acc.rf.default){
      mined.attr<-rownames(mod.tgt$importance)[seq(k)]
    }
    else if(acc.tgt < acc.rf.default && acc.cmp >= acc.rf.default){
      mined.attr<-rownames(mod.cmp$importance)[seq(k)]
    }
    else{
      #both models are accurate; extract top k attributes
      #from the intersection of the top attributes in
      #both models based on variable importance (VI)
      
      #combine the list of MeanDecreaseAccuracy
      mda.tgt<-mod.tgt$importance[,"MeanDecreaseAccuracy"]
      mda.cmp<-mod.cmp$importance[,"MeanDecreaseAccuracy"]
      
      names(mda.tgt)<-paste(mda.tgt,".tgt",sep="")
      names(mda.cmp)<-paste(mda.cmp,".cmp",sep="")
      
      mda.both<-c(mda.tgt,mda.cmp)
      mda.both<-sort(mda.both)
      
    }
    
    #now evaluate whether mod.tgt has been accurate
    #three things to consider:
    # @ whether Atgt is continous or categorical
    # @ if Atgt is categorical, whether Atgt is binary or multi-class
    # @ if Atgt is categorical, whether Atgt has class-imbalance
    #the only piece of additional guiding input from the user is vtgt
    #the other problems need to be resolved using class-imbalance learning techniques
    
    return(list(mod.tgt$confusion,mod.cmp$confusion))
  })
  
  #*********************************************#
  
  output$testRF1<-renderTable({
    minedAttributes()[[1]]
  })
  
  output$testRF2<-renderTable({
    minedAttributes()[[2]]
  })
})