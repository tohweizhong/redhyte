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
  
  #The objectives of Data2() are:
  # -> subsetting the data based on the user's initial context
  # -> if Atgt is continuous, include a binary attribute based on
  #    mean(Atgt). This is done because it will speed up the
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
    
    #retrieve all elements of initial context first
    grpA.classes<-input$whichtgtclassesA # <--- could be NULL if Atgt is cont
    grpB.classes<-input$whichtgtclassesB # <--- could be NULL
    grpX.classes<-input$whichcmpclassesX
    grpY.classes<-input$whichcmpclassesY
    ctx.attr    <-input$ctxAttr
    ctx.items   <-input$ctxItems # in the format of Actx = vctx
    
    print(ctx.attr)
    
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
    
    print(length(rowsToUse))
    print(length(rowsToUse.tgt))
    print(length(rowsToUse.cmp))
    print(length(rowsToUse.ctx))
    
    #retrieve the data
    dfWithCtx<-dfWithCtx[rowsToUse,]

    #add class attributes (A,B,X,Y)
    #start with Atgt
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
    
    
    #add the attribute type for the cutoff attribute
    attr.type<-Data()[[2]]
    attr.type<-c(attr.type,"Cate","Cate")
    
    str(dfWithCtx)
    
    return(list(dfWithCtx,attr.type,Data()[[3]]))
    #081014: context bug resolved
  })
  
  #*********************************************#
  
  #for testing Data2()
  output$ctx.data<-renderTable({
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
    
     df<-Data2()[[1]][c(input$targetAttr,input$comparingAttr,"tgt.class","cmp.class")]
      #is the target attribute continuous or categorical?
      if(Data2()[[2]][input$targetAttr] == "Cate"){
        tab<-t(table(df[,c("tgt.class","cmp.class")]))
        rownames(tab)<-c(paste(input$whichcmpclassesX,collapse="&"),
                         paste(input$whichcmpclassesY,collapse="&"))
        colnames(tab)<-c(paste(input$whichtgtclassesA,collapse="&"),
                         paste(input$whichtgtclassesB,collapse="&"))
        
        return(list(tab,
                    "Contingency",
                    length(unique(df[,"cmp.class"])), # <--- must be 2 now
                    df))
      }
      
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
                    length(cl), # <--- must be 2 now
                    df))
      }
  })

  #*********************************************#
  
  #render the comparison or contingency table
  output$contTable<-renderTable({
    Table()[[1]]
  })

  #initial parametric test
  output$initialTest<-renderTable({
    
    #081014: only t-test and chi-squared tests are used now.
    #2 groups only

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
    if(Table()[[2]] == "Contingency") return("t-test") # <--- WRONG
    else if(Table()[[3]] == 2) return("chisq-test")
    else if(Table()[[3]] > 2) return("anova")
  }) #not implemented yet
  
  #*********************************************#
  
  #=============================================#
  #============5. Context mining================#
  #=============================================#
  
  #first step: build the two RF models
  
  #***************REACTIVE**********************#
  
  minedAttributes<-reactive({
    df<-Data2()[[1]]
    
    #need to convert the character attributes to factors first before building models
    #initial data input options use stringsAsFactors=FALSE (see doc.txt)
    which.are.char<-which(Data2()[[2]] == "Cate")
    df[,which.are.char]<-lapply(df[,which.are.char],factor)
    
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
    
    #construct models
    mod.tgt<-randomForest(formula=fm.tgt,
                          data=df,
                          importance=TRUE)
    mod.cmp<-randomForest(formula=fm.cmp,
                          data=df,
                          importance=TRUE)

    #now evaluate whether the models has been accurate
    #three things to consider:
    # @ whether Atgt is continous or categorical
    # @ if Atgt is categorical, whether Atgt is binary or multi-class
    # @ if Atgt is categorical, whether Atgt has class-imbalance
    #the only piece of additional guiding input from the user is vtgt
    #the other problems need to be resolved using class-imbalance learning techniques
    
    #compute accuracies
    cm.tgt<-mod.tgt$confusion
    cm.cmp<-mod.cmp$confusion
    cm.tgt<-cm.tgt[,-ncol(cm.tgt)]
    cm.cmp<-cm.cmp[,-ncol(cm.cmp)]

    #090914: consider only the diagonal entries of the confusion matrices for now
    acc.tgt<-sum(diag(cm.tgt))/sum(cm.tgt)
    acc.cmp<-sum(diag(cm.cmp))/sum(cm.cmp)
    #this is severly affected by class-imbalance and multi-classes
    
    print(acc.tgt)
    print(acc.cmp)
    
    #compare the accuracies of the models with the default accuracy threshold
    #defined in related_codes/settings.R
    #3 cases:
    # -> acc.tgt >= acc.rf.default && acc.cmp >= acc.rf.default
    # -> acc.tgt >= acc.rf.default && acc.cmp <  acc.rf.default
    # -> acc.tgt <  acc.rf.default && acc.cmp >= acc.rf.default
    
    if(acc.tgt >= acc.rf.default && acc.cmp < acc.rf.default){
      mined.attr<-rownames(mod.tgt$importance)[seq(k)]
      names(mined.attr)<-paste(mined.attr,".tgt",sep="")
    }
    else if(acc.tgt < acc.rf.default && acc.cmp >= acc.rf.default){
      mined.attr<-rownames(mod.cmp$importance)[seq(k)]
      names(mined.attr)<-paste(mined.attr,".cmp",sep="")
    }
    else if(acc.tgt >= acc.rf.default && acc.cmp >= acc.rf.default){
      #both models are accurate; extract top k attributes
      #from the intersection of the top attributes in
      #both models based on variable importance (VI)
      
      #combine the list of MeanDecreaseAccuracy
      mda.tgt<-mod.tgt$importance[,"MeanDecreaseAccuracy"]
      mda.cmp<-mod.cmp$importance[,"MeanDecreaseAccuracy"]
      
      names(mda.tgt)<-paste(names(mda.tgt),".tgt",sep="")
      names(mda.cmp)<-paste(names(mda.cmp),".cmp",sep="")
      
      mda.both<-c(mda.tgt,mda.cmp)
      mda.both<-sort(mda.both,decreasing=TRUE)
      
      #take the first k attributes, consider them shortlisted
      mined.attr<-names(mda.both)[seq(5)]
      
      #print("sl:")
      #print(sl)
      
      #function to remove the tails ".tgt" and ".cmp"
      mined.attr<-sapply(mined.attr,FUN=function(s){
        last.dot<-regexpr("\\.[^\\.]*$", s)
        return(substr(s,1,last.dot-1))
      })
      
      #print("sl after remove.tail")
      #print(sl)
      
      #while-loop to remove duplicates in mined.attr and add new ones
      idx<-k
      while(length(unique(mined.attr)) != length(mined.attr)){
        dup<-which(duplicated(mined.attr) == TRUE)
        mined.attr<-mined.attr[-dup[1]] #remove one at a time
        original.name<-names(mda.both)[idx+1]
        mined.attr<-c(mined.attr,remove.tail(names(mda.both)[idx+1]))
        names(mined.attr)[(length(mined.attr))]<-original.name
        idx<-idx+1
        print(idx)
      }
      print(mined.attr)
    }
    return(list(mod.tgt$confusion,mod.cmp$confusion,mined.attr))
  })
  
  #*********************************************#
  
  output$testRF1<-renderTable({
    minedAttributes()[[1]]
  })
  output$testRF2<-renderTable({
    minedAttributes()[[2]]
  })
  output$testRF3<-renderTable({
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
    #this visualization requires at least two, at most three attributes
    tgt.attr<-input$targetAttr
    cmp.attr<-input$comparingAttr
    mined.attr<-input$mined.attr
    
    #consider Cinitial for Acmp
    c.initial.cmp<-input$whichcmpclasses #have not considered "Use all classes" yet
    num.classes.cmp<-length(c.initial.cmp)
    #will need to consider Atgt as well
    
    #grab the relevant data
    df.to.plot<-Data2()[[1]][,c(tgt.attr,cmp.attr,mined.attr)]
    
    #need to consider whether Atgt is continuous or categorical
    #check this using the Data()[[2]]
    if(Data()[[2]][tgt.attr]=="Cont"){
      #Atgt is continuous, simulate a comparison table
      #plotting only involves two attributes:
      # -> mined.attr
      # -> cmp.attr
      
      par(mfrow=c(length(num.classes.cmp,1)))
      
      for(i in seq(length(num.classes.cmp))){
        #grab the subset of data
        plot.dat<-df.to.plot[which(df.to.plot[,cmp.attr] == c.initial.cmp[i]),mined.attr]
        if(Data()[[3]][mined.attr] == "Cate") barplot(plot.dat)
        else if(Data()[[3]][mined.attr == "Cont"]) hist(plot.data)
      }
    }
    else if(Data()[[2]][tgt.attr] == "Cate"){
      #Atgt is continuous, simulate a contingency table
      #plotting now involves all three attributes
      
      #consider Cinitial defined by Atgt
      c.initial.tgt<-input$whichtgtclasses
      num.classes.tgt<-length(c.initial.tgt)
      
      par(mfrow=c(num.classes.cmp,num.classes.tgt))
      print(num.classes.cmp)
      print(num.classes.tgt)
      
      for(i in seq(num.classes.cmp)){
        for(j in seq(num.classes.tgt)){
          
          rowsToPlot<-intersect(which(df.to.plot[,cmp.attr] == c.initial.cmp[i]),
                                which(df.to.plot[,tgt.attr] == c.initial.tgt[j]))
          
          plot.dat<-df.to.plot[rowsToPlot,mined.attr]
          if(Data()[[2]][mined.attr] == "Cate"){
            barplot(data.frame(table(plot.dat))[,2],
                    main=paste(cmp.attr,"=",c.initial.cmp[i],",",
                               tgt.attr,"=",c.initial.tgt[j],sep=""))
          }
          else if(Data()[[2]][mined.attr] == "Cont")
            hist(plot.dat,main=paste(cmp.attr,"=",c.initial.cmp[i],",",
                                     tgt.attr,"=",c.initial.tgt[j],sep=""))
        }
      }
    }
  })
}) #end shinyServer