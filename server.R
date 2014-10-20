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
    datFile<-input$datFile
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
  output$data.preview<-renderTable({
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
  output$viz.ctrl1<-renderUI({
    selectizeInput("viz.which.attr1","Select attribute to visualize",colnames(Data()[[1]]))
  })
  output$viz.ctrl2<-renderUI({
    selectizeInput("viz.which.attr2","Select attribute to visualize",colnames(Data()[[1]]))
  })
  
  #display type of attribute: continuous or categorical
  output$viz.type1<-renderText({
    if(Data()[[2]][input$viz.which.attr1]=="Cont")
      type<-"Type: Continuous"
    else type<-"Type: Categorical"
    type
  })
  output$viz.type2<-renderText({
    if(Data()[[2]][input$viz.which.attr2]=="Cont")
      type<-"Type: Continuous"
    else type<-"Type: Categorical"
    type
  })
  
  #display boxplot stats (Tukey's five) if cont,
  #else display frequencies
  output$viz.tukeyfive1<-renderTable({
    if(Data()[[2]][input$viz.which.attr1]=="Cont"){
      qt<-as.data.frame(fivenum(Data()[[1]][,input$viz.which.attr1]))
      rownames(qt)<-c("Min","25%","Median","75%","Max")
      colnames(qt)<-NULL
      qt
    }
    else{
      tb<-data.frame(table(Data()[[1]][,input$viz.which.attr1]))
      colnames(tb)<-c(input$viz.which.attr1,"Frequency")
      tb
    }

  })
  output$viz.tukeyfive2<-renderTable({
    if(Data()[[2]][input$viz.which.attr2]=="Cont"){
      qt<-as.data.frame(fivenum(Data()[[1]][,input$viz.which.attr2]))
      rownames(qt)<-c("Min","25%","Median","75%","Max")
      colnames(qt)<-NULL
      qt
    }
    else{
      tb<-data.frame(table(Data()[[1]][,input$viz.which.attr2]))
      colnames(tb)<-c(input$viz.which.attr2,"Frequency")
      tb
    }
  })
  
  #plotting histograms or barcharts
  output$viz.hist1<-renderPlot({
    if(Data()[[2]][input$viz.which.attr1]=="Cont")
      hist(Data()[[1]][,input$viz.which.attr1],main="",xlab="")
    else{
      tb<-data.frame(table(Data()[[1]][,input$viz.which.attr1]))
      colnames(tb)<-c(input$viz.whichAttr1,"Frequency")
      barplot(tb$Frequency)
    }
      
  })
  output$viz.hist2<-renderPlot({
    if(Data()[[2]][input$viz.which.attr2]=="Cont")
      hist(Data()[[1]][,input$viz.which.attr2],main="",xlab="")
    else{
      tb<-data.frame(table(Data()[[1]][,input$viz.which.attr2]))
      colnames(tb)<-c(input$viz.which.attr2,"Frequency")
      barplot(tb$Frequency)
    }
  })
  
  #plotting boxplots
  output$viz.boxplot1<-renderPlot({
    if(Data()[[2]][input$viz.which.attr1]=="Cont")
      boxplot(Data()[[1]][,input$viz.which.attr1])
  })
  output$viz.boxplot2<-renderPlot({
    if(Data()[[2]][input$viz.which.attr2]=="Cont")
      boxplot(Data()[[1]][,input$viz.which.attr2])
  })
  
  #plotting scatterplot
  output$viz.scatterplot<-renderPlot({
    plot(Data()[[1]][,input$viz.which.attr2]~
           Data()[[1]][,input$viz.which.attr1],
         xlab=input$viz.which.attr1,
         ylab=input$viz.which.attr2)
  })
  
  #=============================================#
  #==============3. Initial test================#
  #=============================================#
  
  #dropdown boxes to select Atgt and Acmp
  output$test.tgt.ctrl<-renderUI({
    selectizeInput("targetAttr",
                   "Indicate target attribute (May be continuous or categorical)",
                   colnames(Data()[[1]]))
  }) #return: input$targetAttr
  output$test.cmp.ctrl<-renderUI({
    selectizeInput("comparingAttr",
                   "Indicate comparing attribute (Must be categorical)",
                   colnames(Data()[[1]]))
  }) #return: input$comparingAttr
  
  #display type of attribute: continuous or categorical
  output$test.tgt.type<-renderText({
    if(Data()[[2]][input$targetAttr]=="Cont")
      type<-"Type: Continuous"
    else type<-"Type: Categorical"
    type
  })
  output$test.cmp.type<-renderText({
    if(Data()[[2]][input$comparingAttr]=="Cont")
      type<-"Type: Continuous"
    else type<-"Type: Categorical"
    type
  })
  
  #target and comparing control
  output$test.tgt.class.ctrl1<-renderUI({
    if(Data()[[2]][input$targetAttr] == "Cate"){
      checkboxGroupInput("whichtgtclassesA",
                         "Indicate which target attribute classes to form group A",
                         choices=c(unique(Data()[[1]][,input$targetAttr])))
    }
  }) #return: input$whichtgtclassesA
  output$test.tgt.class.ctrl2<-renderUI({
    if(Data()[[2]][input$targetAttr] == "Cate"){
      checkboxGroupInput("whichtgtclassesB",
                         "Indicate which target attribute classes to form group B",
                         choices=c(unique(Data()[[1]][,input$targetAttr])))
    }
  }) #return: input$whichtgtclassesB
  output$test.cmp.class.ctrl1<-renderUI({
    if(Data()[[2]][input$comparingAttr] == "Cate"){ #this must be true
      checkboxGroupInput("whichcmpclassesX",
                         "Indicate which comparing attribute class to form group X",
                         choices=c(unique(Data()[[1]][,input$comparingAttr])))
    }
  }) #return: input$whichcmpclassesX
  output$test.cmp.class.ctrl2<-renderUI({
    if(Data()[[2]][input$comparingAttr] == "Cate"){ #this must be true
      checkboxGroupInput("whichcmpclassesY",
                         "Indicate which comparing attribute classes to form group Y",
                         choices=c(unique(Data()[[1]][,input$comparingAttr])))
    }
  }) #return: input$whichcmpclassesY

  #context control
  output$test.ctx.ctrl<-renderUI({
    tmp.choices<-colnames(Data()[[1]])[intersect(
      which(colnames(Data()[[1]]) != input$targetAttr),
      which(colnames(Data()[[1]]) != input$comparingAttr))]
    
    .choices<-NULL
    for(c in tmp.choices){
      if(Data()[[2]][c] == "Cate") .choices<-c(.choices,c)
    }
    
    checkboxGroupInput("ctxAttr",
                       "Indicate which categorical attributes to form initial context",
                       choices=.choices)
  }) #return: input$ctxAttr
  output$test.ctx.item.ctrl<-renderUI({
    ctx.attr<-input$ctxAttr
    if(is.null(ctx.attr)) return()
    .choices<-NULL
    
    paste.fun<-function(attr,class){
      return(paste(attr, "=", class, sep=""))
    }
    
    for(i in seq(length(ctx.attr))){
      classes<-unique(Data()[[1]][,ctx.attr[i]])
      for(j in seq(length(classes))){
        .choices<-c(.choices,paste.fun(ctx.attr[i],classes[j]))
      }
    }
    checkboxGroupInput("ctxItems",
                       "Indicate which items to form initial context",
                       choices=.choices)
    
  }) #return input$ctxItems

  #***************REACTIVE**********************#

  # Groupings() is a simple reactive module to
  # -> Keep track of type of Atgt (cont or cate)
  # -> keep track of tgt.class and cmp.class groupings
  # mainly for displaying the correct context in the UI
  
  # Groupings() consists of *THREE* things at the moment:
  # 1. Groupings()[[1]] is Atgt type
  # 2. Groupings()[[2]] is Atgt.names
  # 3. Groupings()[[3]] is Acmp.names
  
  Groupings<-reactive({
    if(Data()[[2]][input$targetAttr] == "Cont")
      return(list(Atgt.type="Cont",
                  Atgt.names=c(paste(input$targetAttr,": High",sep=""),
                               paste(input$targetAttr,": Low",sep="")),
                  Acmp.names=c(paste(input$whichcmpclassesX,collapse="&"),
                               paste(input$whichcmpclassesY,collapse="&"))))
    
    else if(Data()[[2]][input$targetAttr] == "Cate")
      return(list(Atgt.type="Cate",
                  Atgt.names=c(paste(input$whichtgtclassesA,collapse="&"),
                               paste(input$whichtgtclassesB,collapse="&")),
                  Acmp.names=c(paste(input$whichcmpclassesX,collapse="&"),
                               paste(input$whichcmpclassesY,collapse="&"))))
  })
  
  #*********************************************#
  
  #***************REACTIVE**********************#
  
  # The objectives of Data2() are:
  #  -> subsetting the data based on the user's initial context
  #  -> if Atgt is continuous, include a binary attribute based on
  #    mean(Atgt). This is done because it will speed up the
  #    construction of the RF models later. (Regression RF is
  #    apparently slower than classification RF.)
  
  # Because Data() contains information regarding the attributes,
  # while Data2() considers starting context formed by specific
  # classes of Atgt and Acmp, information in Data() can be re-used, except Data()[[3]].
  # Data()[[3]] contains the number of classes for each categorical attribute,
  # which changes for Atgt and Acmp, depending on initial context
  # 030914: will return Data()[[[3]]] as it is anyway for now.

  # Data2() consists of *THREE* things at the moment
  #  1. Data2()[[1]] is the data itself, including the median cutoff attribute if Atgt is cont
  #  2. Data2()[[2]] is the type of variable: continuous or categorical
  #  3. Data2()[[3]] is the number of classes for categorical attributes, NA for cont.
  
  Data2<-reactive({
    
    dfWithCtx<-Data()[[1]]
    
    #retrieve all elements of initial context first
    grpA.classes<-input$whichtgtclassesA # <--- could be NULL if Atgt is cont
    grpB.classes<-input$whichtgtclassesB # <--- could be NULL
    grpX.classes<-input$whichcmpclassesX
    grpY.classes<-input$whichcmpclassesY
    ctx.attr    <-input$ctxAttr
    ctx.items   <-input$ctxItems # in the format of Actx = vctx
    
    rowsToUse.cmp<-seq(nrow(dfWithCtx))
    rowsToUse.tgt<-seq(nrow(dfWithCtx))
    rowsToUse.ctx<-NULL
    
    #start with Acmp
    rowsToUse.cmp<-which(dfWithCtx[,input$comparingAttr] %in% input$whichcmpclassesX == TRUE) #only X
    rowsToUse.cmp<-union(
      rowsToUse.cmp,
      which(dfWithCtx[,input$comparingAttr] %in% input$whichcmpclassesY == TRUE)) #X U Y
    
    #now with Atgt
    if(Data()[[2]][input$targetAttr] == "Cate"){
      rowsToUse.tgt<-which(dfWithCtx[,input$targetAttr] %in% input$whichtgtclassesA == TRUE) #only A
      rowsToUse.tgt<-union(
        rowsToUse.tgt,
        which(dfWithCtx[,input$targetAttr] %in% input$whichtgtclassesB == TRUE)) #A U B
    }
    rowsToUse<-intersect(rowsToUse.tgt,rowsToUse.cmp)    
    #now for Actx
    if(!is.null(ctx.attr)){
      items.df<-data.frame(t(data.frame(sapply(ctx.items,FUN=strsplit,"="),
                                        stringsAsFactors=F)),
                           stringsAsFactors=F) #ugly, will change this later
      rownames(items.df)<-NULL
      colnames(items.df)<-c("Actx","vctx")
  
      for(i in seq(nrow(items.df))){
          attr<-items.df$Actx[i]
          class<-items.df$vctx[i]
          rowsToUse.ctx<-c(rowsToUse.ctx,which(dfWithCtx[,attr] == class))
      }
      rowsToUse.ctx<-unique(rowsToUse.ctx)
      #now, combine the rowsToUse
      rowsToUse<-intersect(rowsToUse.cmp,intersect(rowsToUse.tgt,rowsToUse.ctx))
    }
    
    #retrieve the data
    dfWithCtx<-dfWithCtx[rowsToUse,]

    #add class attributes (A,B,X,Y)
    #start with Atgt
    if(Groupings()[[1]] == "Cont"){
      m<-mean(dfWithCtx[,input$targetAttr])
      #using mean instead of median,
      #because median cannot handle extremely skewed data
      #print(unique(dfWithCtx[,input$targetAttr]))
      dfWithCtx$tgt.class<-sapply(dfWithCtx[,input$targetAttr],
                                  FUN=function(x){
                                  if(x>=m) return("High")
                                  else return("Low")})
    }
    else{ #Atgt is continuous
      dfWithCtx$tgt.class<-sapply(dfWithCtx[,input$targetAttr],
                                  FUN=function(x){
                                    if(x %in% grpA.classes) return("1")
                                    else if(x %in% grpB.classes) return("2")})
    }
    #now with Acmp
    dfWithCtx$cmp.class<-sapply(dfWithCtx[,input$comparingAttr],
                                FUN=function(x){
                                  if(x %in% grpX.classes) return("1")
                                  else if(x %in% grpY.classes) return("2")})
    
    
    # For the contexted data stored in memory as a data.frame,
    # tgt.class and cmp.class do not contain the Atgt.names and Acmp.names
    # as per in Groupings()
    # they only contain "High"/"Low" or "1"/"2"
    
    #add the attribute type for the cutoff attribute
    attr.type<-Data()[[2]]
    attr.type<-c(attr.type,"Cate","Cate")
    
    #**console**#
    print(paste("nrow(dfWithCtx): ",nrow(dfWithCtx)))
    
    return(list(dfWithCtx,attr.type,Data()[[3]]))
    #081014: context bug resolved
  })
  
  #*********************************************#
  
  # display contexted data
  output$ctx.data<-renderTable({
    if (is.null(Data2()[1])) return(NULL)
    rowsToDisplay<-10
    if(rowsToDisplay > 0.5*nrow(Data2()[[1]])) rowsToDisplay<-0.5*nrow(Data2()[[1]])
    data.frame(Data2()[[1]][1:rowsToDisplay,])
  },digits=3)

  #***************REACTIVE**********************#

  # Table() consists of *FOUR* things at the moment
  #  1. Table()[[1]] is the table itself, be it contingency or comparison table
  #  2. Table()[[2]] is the type of table: contingency or comparison
  #  3. Table()[[3]] is the data with the starting context itself,
  #     with 4 columns: Atgt, Acmp, tgt.class, cmp.class
  
  # generate contingency table if target attribute is cate.
  # else generate a comparison table
  
  # reactive wrapper for table
  Table<-reactive({
    # retrieve the relevant data
    
    df<-Data2()[[1]][c(input$targetAttr,input$comparingAttr,"tgt.class","cmp.class")]
    
    #is the target attribute continuous or categorical?
    if(Data2()[[2]][input$targetAttr] == "Cate"){
      tab<-t(table(df[,c("tgt.class","cmp.class")]))
      rownames(tab)<-c(paste(input$whichcmpclassesX,collapse="&"),
                       paste(input$whichcmpclassesY,collapse="&"))
      colnames(tab)<-c(paste(input$whichtgtclassesA,collapse="&"),
                       paste(input$whichtgtclassesB,collapse="&"))
      
      return(list(cont.tab=tab,
                  tab.type="Contingency",
                  tab.df=df))
    }
    
    else{ #target attribute is continuous
      
      # want to return both the data.frame of means and
      # the contingency table based on tgt.class = {"High", "Low"}
      # 131014: for now only the data.frame of means will be displayed in the UI
      
      # first, the data.frame of means
      mean1<-mean(df[which(df$cmp.class == "1"),input$targetAttr])
      mean2<-mean(df[which(df$cmp.class == "2"),input$targetAttr])
      
      means.df<-data.frame(c(mean1,mean2))
      rownames(means.df)<-c(paste(input$whichcmpclassesX,collapse="&"),
                            paste(input$whichcmpclassesY,collapse="&"))
      colnames(means.df)<-paste("means of ",input$targetAttr,sep="")
      
      # next, the contingency table
      tab<-t(table(df[,c("tgt.class","cmp.class")]))
      rownames(tab)<-Groupings()[[3]]
      colnames(tab)<-Groupings()[[2]]
      
      print(tab)
      
      return(list(means.df=means.df,
                  cont.tab=tab,
                  tab.type="Comparison",
                  tab.df=df))
    }
  })

  #*********************************************#
  
  #render the comparison or contingency table
  output$contTable<-renderTable({
    Data3()
    Table()[[1]]
  })

  #initial parametric test
  output$initialTest<-renderTable({
    
    # 081014: only t-test and chi-squared tests are used now.
    # no more ANOVA
    # 2 groups only

    # check the type of table
    if(Groupings()[[1]] == "Cate"){
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
    else if(Groupings()[[1]] == "Cont"){
      #t-test
      test<-t.test(Data2()[[1]][,input$targetAttr]~Data2()[[1]]$cmp.class) #t-test bug resolved
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
  })
  # yet to implement non-parametric test yet
  
  #=============================================#
  #============4. Test diagnostics==============#
  #=============================================#

  # test diagnostics for t-test and ANOVA only  
  # not implemented yet
  
  #***************REACTIVE**********************#
  
  Test<-reactive({
    if(Table()[["tab.type"]] == "Contingency") return("t-test") # <--- WRONG
    else return("chisq-test")
  }) #not implemented yet
  
  #*********************************************#
  
  #***************REACTIVE**********************#
  
  Data3<-reactive({
    df<-Data2()[[1]]
    attr.type<-Data2()[[2]]
    
    #change all continuous attributes to categorical before hypothesis mining
    mean.discre<-function(an.attr){
      m<-mean(df[,an.attr])
      new.col<-sapply(df[,an.attr],
                      FUN=function(x){
                        if(x>=m) return("High")
                        else return("Low")})
      return(new.col)
    }
    
    which.are.cont<-which(attr.type == "Cont")
    for(an.attr in which.are.cont)
      df[,an.attr]<-mean.discre(an.attr)
    
    str(df)
    
    attr.type<-rep("Cate",length(attr.type))
    
    return(list(df,attr.type,Data2()[[3]]))
  })
  
  
  #=============================================#
  #============5. Context mining================#
  #=============================================#
  
  # first step: build the two RF models
  
  #***************REACTIVE**********************#
  
  minedAttributes<-reactive({
    df<-Data3()[[1]]
    
    #need to convert the character attributes to factors first before building models
    #initial data input options use stringsAsFactors=FALSE (see doc.txt)
    which.are.char<-which(Data3()[[2]] == "Cate") # all are categorical
    df[,which.are.char]<-lapply(df[,which.are.char],factor)
    
    # formulate the formulae for random forest models
    
    # start with Atgt
    # find all the predictors to be used in models
    predictors<-colnames(df)[which(colnames(df) != input$comparingAttr)]
    
    predictors<-intersect(predictors,
                          colnames(df)[which(colnames(df) != input$targetAttr)])
    
    predictors<-intersect(predictors,
                          colnames(df)[which(colnames(df) != "tgt.class")])
    
    predictors<-intersect(predictors,
                          colnames(df)[which(colnames(df) != "cmp.class")])
    
    fm.tgt<-paste(" ",predictors,sep="",collapse="+")
    fm.tgt<-as.formula(paste("tgt.class","~",fm.tgt,sep=""))
    
    # now formulate for Acmp
    fm.cmp<-paste(" ",predictors,sep="",collapse="+")
    fm.cmp<-as.formula(paste("cmp.class","~",fm.cmp,sep=""))
    
    # construct models
    mod.tgt<-randomForest(formula=fm.tgt,
                          data=df,
                          importance=TRUE)
    mod.cmp<-randomForest(formula=fm.cmp,
                          data=df,
                          importance=TRUE)

    # now evaluate whether the models has been accurate
    # three things to consider:
    #  @ whether Atgt is continous or categorical
    #  @ if Atgt is categorical, whether Atgt is binary or multi-class
    #  @ if Atgt is categorical, whether Atgt has class-imbalance
    # the only piece of additional guiding input from the user is vtgt
    # the other problems need to be resolved using class-imbalance learning techniques
    
    # compute accuracies
    tmp.cm.tgt<-mod.tgt$confusion
    tmp.cm.cmp<-mod.cmp$confusion
    tmp.cm.tgt<-tmp.cm.tgt[,-ncol(tmp.cm.tgt)]
    tmp.cm.cmp<-tmp.cm.cmp[,-ncol(tmp.cm.cmp)]
    # these confusion matrices are for computing accuracies only,
    # will not be returned by this reactive module
    
    # 090914: consider only the diagonal entries of the confusion matrices for now
    acc.tgt<-sum(diag(tmp.cm.tgt))/sum(tmp.cm.tgt)
    acc.cmp<-sum(diag(tmp.cm.cmp))/sum(tmp.cm.cmp)
    # this is severly affected by class-imbalance and multi-classes
    print(paste("acc.tgt: ",acc.tgt,sep=""))
    print(paste("acc.cmp: ",acc.cmp,sep=""))
    
    # compare the accuracies of the models with the default accuracy threshold
    # defined in related_codes/settings.R
    # 3 cases:
    # -> acc.tgt >= acc.rf.default && acc.cmp >= acc.rf.default
    # -> acc.tgt >= acc.rf.default && acc.cmp <  acc.rf.default
    # -> acc.tgt <  acc.rf.default && acc.cmp >= acc.rf.default
    
    mined.attr<-NULL
    
    if(acc.tgt >= acc.rf.default && acc.cmp < acc.rf.default){
      mined.attr<-rownames(mod.tgt$importance)[seq(k)]
      names(mined.attr)<-paste(mined.attr,".tgt",sep="") # adding a tail ".tgt" or ".cmp"
    }
    else if(acc.tgt < acc.rf.default && acc.cmp >= acc.rf.default){
      mined.attr<-rownames(mod.cmp$importance)[seq(k)]
      names(mined.attr)<-paste(mined.attr,".cmp",sep="")
    }
    else if(acc.tgt >= acc.rf.default && acc.cmp >= acc.rf.default){
      # both models are accurate; extract top k attributes
      # from the intersection of the top attributes in
      # both models based on variable importance (VI)
      
      # combine the list of MeanDecreaseAccuracy
      mda.tgt<-mod.tgt$importance[,"MeanDecreaseAccuracy"]
      mda.cmp<-mod.cmp$importance[,"MeanDecreaseAccuracy"]
      
      names(mda.tgt)<-paste(names(mda.tgt),".tgt",sep="")
      names(mda.cmp)<-paste(names(mda.cmp),".cmp",sep="")
      
      mda.both<-c(mda.tgt,mda.cmp)
      mda.both<-sort(mda.both,decreasing=TRUE)
      
      # take the first k attributes, consider them shortlisted
      mined.attr<-names(mda.both)[seq(k)]
      
      # function to remove the tails ".tgt" and ".cmp"
      remove.tail<-function(s){
        last.dot<-regexpr("\\.[^\\.]*$", s)
        return(substr(s,1,last.dot-1))
      }
      mined.attr<-sapply(mined.attr,FUN=remove.tail)
      
      # while-loop to remove duplicates in mined.attr and add new ones
      idx<-k
      while(length(unique(mined.attr)) != length(mined.attr)){
        dup<-which(duplicated(mined.attr) == TRUE)
        mined.attr<-mined.attr[-dup[1]] # remove one at a time
        original.name<-names(mda.both)[idx+1]
        mined.attr<-c(mined.attr,remove.tail(names(mda.both)[idx+1]))
        names(mined.attr)[(length(mined.attr))]<-original.name
        idx<-idx+1
      }
    }
    
    #**console**#
    print(paste("mined.attr: ",names(mined.attr)))
    
    cm.tgt<-mod.tgt$confusion
    cm.cmp<-mod.cmp$confusion
    
    rownames(cm.tgt)<-colnames(cm.tgt)[1:2]<-Groupings()[[2]]
    rownames(cm.cmp)<-colnames(cm.cmp)[1:2]<-Groupings()[[3]]
    
    if(!is.null(mined.attr)) return(list(cm.tgt,cm.cmp,mined.attr))
    #both mod.tgt and mod.cmp are inaccurate, therefore no mined attributes
    else return(list(cm.tgt,cm.cmp,NULL))
  })
  
  #*********************************************#
  
  output$testRF1<-renderTable({
    minedAttributes()[[1]]
  })
  output$testRF2<-renderTable({
    minedAttributes()[[2]]
  })
  output$testRF3<-renderTable({
    if(is.null(minedAttributes()[[3]])){
      tmp<-data.frame("No significant context attributes were found")
      colnames(tmp)<-""
      return(tmp)
    }
    df<-data.frame(minedAttributes()[[3]])
    colnames(df)<-"Mined context attributes"
    df
  })
  
  #visualization of the mined context attrtibutes
  output$minedAttrCtrl<-renderUI({
    selectizeInput("mined.attr",
                   "Which mined attribute?",
                   minedAttributes()[[3]])
  }) #return: input$mined.attr
  
  #render the plot for one mined attribute indicated by user
  output$mined.attr.viz<-renderPlot({
    if(is.null(minedAttributes()[[3]])) return(NULL)
    tgt.attr<-input$targetAttr
    cmp.attr<-input$comparingAttr
    mined.attr<-input$mined.attr
    
    
    #grab the relevant data
    df.to.plot<-Data2()[[1]][,c(tgt.attr,cmp.attr,mined.attr,"tgt.class","cmp.class")]
    
    #**console**#
    print(paste("ncol(df.to.plot): ",ncol(df.to.plot)))
    par(mfrow=c(2,2))

    #need to consider whether Atgt is continuous or categorical
    #check this using the Data()[[2]]
    for(i in seq(1,2)){
      for(j in seq(1,2)){
        
        #grab the subset of data
        rows.to.plot<-intersect(
          which(df.to.plot[,"tgt.class"] == i),
          which(df.to.plot[,"cmp.class"] == j)
        )
        plot.dat<-df.to.plot[rows.to.plot,mined.attr]
        if(Data()[[2]][mined.attr] == "Cate"){
          barplot(data.frame(table(plot.dat))[,2],
                  main=paste(Groupings()[[2]][i], Groupings()[[3]][j], sep="&"))
        }
        else if(Data()[[2]][mined.attr] == "Cont")
          hist(plot.dat,main=paste(Groupings()[[2]][i], Groupings()[[3]][j], sep="&"))
      }
    }
  })
  
  #***************REACTIVE**********************#

  Hypotheses<-reactive({
    
    tgt.attr<-input$targetAttr
    cmp.attr<-input$comparingAttr
    mined.attr<-minedAttributes()[[3]]
    
    df<-Data3()[[1]][,c(tgt.attr,cmp.attr,"tgt.class","cmp.class",c(mined.attr))]
    
    #function to compute proportions
    compute.prop<-function(tab){
      p1<-tab[1,1]/sum(tab[1,1],tab[1,2])
      p2<-tab[2,1]/sum(tab[2,1],tab[2,2])
      return(c(p1,p2))
    }
    
    # function to compute support
    compute.sup<-function(tab){
      c11<-tab[1,1]
      c12<-tab[1,2]
      c21<-tab[2,1]
      c22<-tab[2,2]
      n1<-c11+c12
      n2<-c21+c22
      return(c(c11=c11,c12=c12,c21=c21,c22=c22,n1=n1,n2=n2))
    }
    
    # compute initial proportions
    cont.tab<-Table()[["cont.tab"]]
    initial.p1<-compute.prop(cont.tab)[1]
    initial.p2<-compute.prop(cont.tab)[2]
    # compute initial n
    initial.n1<-compute.sup(cont.tab)["n1"]
    initial.n2<-compute.sup(cont.tab)["n2"]
    
    # create the master data.frame that has all of the following columns:
    # $ rownames: ctx items
    # $ Actx, vctx
    # $ p1prime, p2prime
    # $ c11, c12, c21, c22
    # $ n1prime, n2prime,
    # $ difflift, contri
    prop.df.names<-NULL
    
    for(a.ctx.attr in mined.attr){
      classes<-unique(df[,a.ctx.attr])
      for(an.item in classes)
        prop.df.names<-c(prop.df.names,paste(a.ctx.attr,an.item,sep="="))
    }
    
    prop.df<-data.frame(cbind(rep(NA,length(prop.df.names)),
                              rep(NA,length(prop.df.names))),
                     row.names=prop.df.names)
    colnames(prop.df)<-c("p1prime","p2prime")
    
    i<-1
    for(an.item in rownames(prop.df)){
      Actx<-unlist(strsplit(an.item,"="))[1]
      vctx<-unlist(strsplit(an.item,"="))[2]
      
      rows.to.prop<-which(df[,Actx] == vctx)
      df.to.prop<-df[rows.to.prop,c("tgt.class","cmp.class")]
      tab.to.prop<-t(table(df.to.prop))
      
      prop.df$Actx[i]<-Actx
      prop.df$vctx[i]<-vctx
      
      if(nrow(tab.to.prop)*ncol(tab.to.prop) == 4){ # 2x2 contingency table
        prop.df[i,c("p1prime","p2prime")]<-compute.prop(tab.to.prop)
        
        tmp.sup<-compute.sup(tab.to.prop)
        
        prop.df$c11[i]<-tmp.sup["c11"]
        prop.df$c12[i]<-tmp.sup["c12"]
        prop.df$c21[i]<-tmp.sup["c21"]
        prop.df$c22[i]<-tmp.sup["c22"]
        
        prop.df$n1prime[i]<-tmp.sup["n1"]
        prop.df$n2prime[i]<-tmp.sup["n2"]
      }
      i<-i+1
    }
    
    # function to compute difflift
    compute.dl<-function(prop.vec){
      return(prop.vec[1]-prop.vec[2])/(initial.p1-initial.p2)
    }
    
    # function to compute contribution
    compute.contri<-function(prop.vec){
      
      p1prime<-prop.vec[1]
      p2prime<-prop.vec[2]
      n1prime<-prop.vec[3]
      n2prime<-prop.vec[4]

      numer<-(n1prime/initial.n1)*(p1prime-initial.p1) - 
        (n2prime/initial.n2)*(p2prime-initial.p2)
      denom<-initial.p1-initial.p2
      
      return(numer/denom)
    }
    
    prop.df$difflift<-apply(prop.df[,c("p1prime","p2prime")],
                            MARGIN=1,
                            FUN=compute.dl)
    prop.df$contri<-apply(prop.df[,c("p1prime","p2prime","n1prime","n2prime")],
                          MARGIN=1,
                          FUN=compute.contri)
    
    prop.df$SP<-sapply(prop.df[,"difflift"],
                      FUN=function(x){
                        if(is.na(x)) return(FALSE)
                        else if(x<0) return(TRUE)
                        else if(x>=0) return(FALSE)
                      })
    prop.df<-prop.df[with(prop.df,order(difflift,contri)),]
    
    # now, append the chi-squared test stats and p-values
    
    for(i in seq(nrow(prop.df))){
      
      if(!is.na(prop.df$difflift[i])){
        Actx<-prop.df$Actx[i]
        vctx<-prop.df$vctx[i]
        
        # extract the subset of data
        rows<-which(df[,Actx] == vctx)
        tmp.df<-df[rows,c("tgt.class","cmp.class")]
        
        # run chisq.test on the contingency table
        tmp.tab<-t(table(tmp.df))
        test<-chisq.test(tmp.tab)
        
        # extract test stats and p-value and append to master data.frame
        prop.df$stats[i]<-test$statistic
        prop.df$pvalue[i]<-test$p.value
      }
    }
    prop.df<-prop.df[with(prop.df,order(difflift,contri,pvalue)),]
    return(prop.df)
  })
  
  output$hypotheses<-renderTable({
    #subset(Metrics(),select=c(n1prime,n2prime,difflift,contri,SP))
    Hypotheses()
  },digits=3)
}) #end shinyServer