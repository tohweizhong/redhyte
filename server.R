options(shiny.maxRequestSize=20*1024^2)
shinyServer(function(input,output,session){
  
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
  
  output$title.text<-reactive({
    htmlCode<-"
      <h4><strong><font color=red face=Arial Black>
          An Interactive Platform for 
          Rapid Exploration of Data and Hypothesis Testing
      </font></strong></h4>
    "
  })
  
  #=============================================#
  #=============================================#
  #=============================================#
  
  #grabbing the data
  
  #*********************************************#
  #***************REACTIVE**********************#
  #*********************************************#
  
  #Data() consists of *THREE* things at the moment
  # 1. Data()[[1]] is the data itself
  # 2. Data()[[2]] is the type of variable: numerical or categorical
  # 3. Data()[[3]] is the number of classes for categorical attributes, NA for num.
  
  #here begins the real work..
  
  Data<-reactive({
    datFile<-input$datFile
    path<-as.character(datFile$datapath)
    df<-read.csv(path,
                 header=input$datHeader,
                 #row.names=ifelse(input$datRownames,1,NULL),
                 sep=input$datSep,
                 quote=input$datQuote,
                 stringsAsFactors=F)
    #this part is ok, based on the response I got from stackoverflow
    #the NULL that appears after
    
    #print(str(df))
    
    #is apparently the property of print.default()

    #checking the variable type of the attributes: numerical or categorical
    #and number of classes for cate. attr.
    
    if(input$datTranspose == TRUE) df<-t(df)
    
    typ<-NULL
    numCl<-NULL
    for(i in seq(ncol(df))){
      if(is.numeric(df[,i]) && length(unique(df[,i]))>input$maxClass){
        typ<-c(typ,"Num")
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
  #***************END REACTIVE******************#
  #*********************************************#
  
  #=============================================#
  #==============1. Data preview================#
  #=============================================#
  
  #displaying a preview of the data, 10 rows, all columns
  output$data.preview<-renderTable({
    if(is.null(Data()[1])) return(NULL)
    data.frame(Data()[[1]][1:input$previewRows,])
  },digits=3)
  
  #=============================================#
  #==============2. Data viz====================#
  #=============================================#
  
  #create two dropdown boxes to choose two attributes, display:
  # 1. Type of attribute (numerical or categorical)
  # 2. Tukey's five number summary or counts
  # 3. histogram or barplot
  # 4. boxplots
  # 5. scatterplot for simple eda
  
  # === Select attributes === #
  
  #selecting which attributes to visualise
  output$viz.ctrl1<-renderUI({
    selectizeInput("viz.which.attr1","Select an attribute to visualize",colnames(Data()[[1]]))
  })
  output$viz.ctrl2<-renderUI({
    selectizeInput("viz.which.attr2","Select another attribute to visualize",colnames(Data()[[1]]))
  })
  
  #display type of attribute: numerical r categorical
  output$viz.type1<-renderText({
    if(Data()[[2]][input$viz.which.attr1]=="Num")
      type<-"Type: Numerical"
    else type<-"Type: Categorical"
    type
  })
  output$viz.type2<-renderText({
    if(Data()[[2]][input$viz.which.attr2]=="Num")
      type<-"Type: Numerical"
    else type<-"Type: Categorical"
    type
  })
  
  # === Distributions === #
  
  #display boxplot stats (Tukey's five) if num,
  #else display frequencies
  output$viz.tukeyfive1<-renderTable({
    if(Data()[[2]][input$viz.which.attr1]=="Num"){
      qt<-as.data.frame(fivenum(Data()[[1]][,input$viz.which.attr1]))
      rownames(qt)<-c("Min","25%","Median","75%","Max")
      colnames(qt)<-input$viz.which.attr1
      qt
    }
    else{
      tb<-data.frame(table(Data()[[1]][,input$viz.which.attr1]))
      colnames(tb)<-c(input$viz.which.attr1,"Frequency")
      tb
    }

  })
  output$viz.tukeyfive2<-renderTable({
    if(Data()[[2]][input$viz.which.attr2]=="Num"){
      qt<-as.data.frame(fivenum(Data()[[1]][,input$viz.which.attr2]))
      rownames(qt)<-c("Min","25%","Median","75%","Max")
      colnames(qt)<-input$viz.which.attr2
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
    if(Data()[[2]][input$viz.which.attr1]=="Num")
      hist(Data()[[1]][,input$viz.which.attr1],main=input$viz.which.attr1,xlab="")
    else{
      tb<-data.frame(table(Data()[[1]][,input$viz.which.attr1]))
      colnames(tb)<-c(input$viz.which.attr1,"Frequency")
      barplot(tb$Frequency,
              names.arg=tb[,1],
              las=2,
              cex.names=0.9,
              main=input$viz.which.attr1)
    }
      
  })
  output$viz.hist2<-renderPlot({
    if(Data()[[2]][input$viz.which.attr2]=="Num")
      hist(Data()[[1]][,input$viz.which.attr2],main=input$viz.which.attr2,xlab="")
    else{
      tb<-data.frame(table(Data()[[1]][,input$viz.which.attr2]))
      colnames(tb)<-c(input$viz.which.attr2,"Frequency")
      barplot(tb$Frequency,
              names.arg=tb[,1],
              las=2,
              cex.names=0.9,
              main=input$viz.which.attr2)
    }
  })
  
  # === Relationship === #
  
  # plotting boxplots
  output$viz.boxplot1<-renderPlot({
    if(Data()[[2]][input$viz.which.attr1]=="Num")
      boxplot(Data()[[1]][,input$viz.which.attr1])
  }) # not used
  output$viz.boxplot2<-renderPlot({
    if(Data()[[2]][input$viz.which.attr2]=="Num")
      boxplot(Data()[[1]][,input$viz.which.attr2])
  }) # not used
  
  # plotting scatterplot, boxplot, or spineplot
  output$viz.scatterplot<-renderPlot({
    type1<-Data()[[2]][input$viz.which.attr1]
    type2<-Data()[[2]][input$viz.which.attr2]
    
    if(type1 == "Num" && type2 == "Num")
    plot(Data()[[1]][,input$viz.which.attr2]~
           Data()[[1]][,input$viz.which.attr1],
         xlab=input$viz.which.attr1,
         ylab=input$viz.which.attr2)
    else if(type1 == "Num" && type2 =="Cate")
      boxplot(Data()[[1]][,input$viz.which.attr1]~
                Data()[[1]][,input$viz.which.attr2],
              las=2,
              xlab=input$viz.which.attr2,
              ylab=input$viz.which.attr1)
    else if(type1 == "Cate" && type2 == "Num")
      boxplot(Data()[[1]][,input$viz.which.attr2]~
                Data()[[1]][,input$viz.which.attr1],
              las=2,
              xlab=input$viz.which.attr1,
              ylab=input$viz.which.attr2)
    else{
      par(las=2)
      spineplot(as.factor(Data()[[1]][,input$viz.which.attr1])~
                  as.factor(Data()[[1]][,input$viz.which.attr2]),
                xlab=input$viz.which.attr2,
                ylab=input$viz.which.attr1)
    }
  })
  
  #=============================================#
  #==============3. Initial test================#
  #=============================================#
  
  # === Target attribute === #
  # === Comparing attribute === #
  
  #dropdown boxes to select Atgt and Acmp
  output$test.tgt.ctrl<-renderUI({
    selectizeInput("targetAttr",
                   "Indicate target attribute (May be numerical or categorical)",
                   colnames(Data()[[1]]))
  }) #return: input$targetAttr
  output$test.cmp.ctrl<-renderUI({
    which.are.cate<-which(Data()[[2]] == "Cate")
    
    selectizeInput("comparingAttr",
                   "Indicate comparing attribute (Must be categorical)",
                   colnames(Data()[[1]])[which.are.cate])
  }) #return: input$comparingAttr
  
  #display type of attribute: numerical or categorical
  output$test.tgt.type<-renderText({
    if(Data()[[2]][input$targetAttr]=="Num")
      type<-"Type: Numerical"
    else type<-"Type: Categorical"
    type
  })
  output$test.cmp.type<-renderText({
    if(Data()[[2]][input$comparingAttr]=="Num")
      type<-"Type: Numerical"
    else type<-"Type: Categorical"
    type
  })

  #*********************************************#
  #***************REACTIVE**********************#
  #*********************************************#
  
  # doesnt work
  Ctx.state<-reactive({
    
    # did not do the subsetting of data yet!
    dfWithCtx<-Data()[[1]]
    
    tgt.attr<-input$targetAttr
    cmp.attr<-input$comparingAttr
    ctx.attr<-input$ctxAttr
    
    grpA.classes<-input$whichtgtclassesA
    grpB.classes<-input$whichtgtclassesB
    grpX.classes<-input$whichcmpclassesX
    grpY.classes<-input$whichcmpclassesY
    ctx.attr    <-input$ctxAttr
    ctx.items   <-input$ctxItems # in the format of Actx = vctx
    
    rowsToUse.cmp<-seq(nrow(dfWithCtx))
    rowsToUse.tgt<-seq(nrow(dfWithCtx))
    rowsToUse.ctx<-NULL
    
    if(!is.null(cmp.attr)){
      rowsToUse.cmp<-which(dfWithCtx[,input$comparingAttr] %in% input$whichcmpclassesX == TRUE) #only X
      rowsToUse.cmp<-union(
        rowsToUse.cmp,
        which(dfWithCtx[,input$comparingAttr] %in% input$whichcmpclassesY == TRUE)) #X U Y
    }
    
    # now with Atgt
    if(!is.null(tgt.attr) && Data()[[2]][input$targetAttr] == "Cate"){
      rowsToUse.tgt<-which(dfWithCtx[,input$targetAttr] %in% input$whichtgtclassesA == TRUE) #only A
      rowsToUse.tgt<-union(
        rowsToUse.tgt,
        which(dfWithCtx[,input$targetAttr] %in% input$whichtgtclassesB == TRUE)) #A U B
    }
    rowsToUse<-intersect(rowsToUse.tgt,rowsToUse.cmp)    
    # now for Actx
    if(!is.null(ctx.attr)){
      items.df<-data.frame(t(data.frame(sapply(ctx.items,FUN=strsplit,"="),
                                        stringsAsFactors=F)),
                           stringsAsFactors=F) #ugly, will change this later
      rownames(items.df)<-NULL
      colnames(items.df)<-c("Actx","vctx")
      
      # based on the ctx items, retrieve the rows
      list.of.rows<-vector("list",length(ctx.attr))
      for(i in seq(length(ctx.attr))){
        # from items.df, find all Actx that are the same
        # from these, need to concatenate the rowsToUse
        # when the Actx differs, do intersect
        where.in.items.df<-which(items.df$Actx == ctx.attr[i])
        ctx.classes<-items.df$vctx[where.in.items.df]
        for(a.class in ctx.classes){
          list.of.rows[[i]]<-c(list.of.rows[[i]],which(dfWithCtx[,ctx.attr[i]] == a.class))
        }
      }
      rowsToUse.ctx<-list.of.rows[[1]]
      for(i in seq(length(list.of.rows)-1)){
        if(length(list.of.rows) != 1) rowsToUse.ctx<-intersect(rowsToUse.ctx,list.of.rows[[i+1]])
        # weird behavior of seq(): seq(0) == c(1,0) !
      }
      # now, combine the rowsToUse
      rowsToUse<-intersect(rowsToUse.cmp,intersect(rowsToUse.tgt,rowsToUse.ctx))
    }
    # retrieve the data
    dfWithCtx<-dfWithCtx[rowsToUse,]
    
    tgt.attr.sup<-NULL
    cmp.attr.sup<-NULL
    ctx.attr.sup<-NULL
    
    # based on the attributes, find the support for their classes
    
    if(!is.null(tgt.attr)){
      if(Data()[[2]][tgt.attr] == "Cate"){
        classes<-unique(dfWithCtx[,tgt.attr])
        for(c in classes)
          tgt.attr.sup<-c(tgt.attr.sup,
                          length(which(dfWithCtx[,tgt.attr] == c)))
      }
    }
    
    if(!is.null(cmp.attr)){
      if(Data()[[2]][cmp.attr] == "Cate"){
        classes<-unique(dfWithCtx[cmp.attr])
        for(c in classes)
          cmp.attr.sup<-c(cmp.attr.sup,
                          length(which(dfWithCtx[,cmp.attr] == c)))
      }
    }
    
    if(!is.null(ctx.attr)){
      for(a.ctx.attr in ctx.attr){
        if(Data()[[2]][a.ctx.attr] == "Cate"){ #always true
          classes<-unique(dfWithCtx[,a.ctx.attr])
          for(c in classes)
            ctx.attr.sup<-c(ctx.attr.sup,
                            length(which(dfWithCtx[,a.ctx.attr] == c)))
        }
      }
    }
    
    #print(tgt.attr.sup)
    #print(cmp.attr.sup)
    #print(ctx.attr.sup)
    
    return(list(tgt.attr.sup=tgt.attr.sup,
                cmp.attr.sup=cmp.attr.sup,
                ctx.attr.sup=ctx.attr.sup
                ))
  })

  #*********************************************#
  #***************END REACTIVE******************#
  #*********************************************#
  
  #target and comparing control
  output$test.tgt.class.ctrl1<-renderUI({
    if(Data()[[2]][input$targetAttr] == "Cate"){
      .choices<-unique(Data()[[1]][,input$targetAttr])
      .names<-NULL
      
      i<-1
      for(c in .choices){
  
        #sup<-Ctx.state()[[1]][i]
        sup<-length(which(Data()[[1]][,input$targetAttr] == c))
        .names<-c(.names,paste(c," (",sup,")",sep=""))
        i<-i+1
      }
      names(.choices)<-.names
      
      checkboxGroupInput("whichtgtclassesA",
                         "Indicate which target attribute classes to form group 1 (target value group)",
                         choices=c(sort(.choices)))
    }
  }) #return: input$whichtgtclassesA
  output$test.tgt.class.ctrl2<-renderUI({
    if(Data()[[2]][input$targetAttr] == "Cate"){
      
      .choices<-unique(Data()[[1]][,input$targetAttr])
      .names<-NULL
      for(c in .choices){
        sup<-length(which(Data()[[1]][,input$targetAttr] == c))
        .names<-c(.names,paste(c," (",sup,")",sep=""))
      }
      names(.choices)<-.names
      
      checkboxGroupInput("whichtgtclassesB",
                         "Indicate which target attribute classes to form group 2",
                         choices=c(sort(.choices)))
    }
  }) #return: input$whichtgtclassesB
  output$test.cmp.class.ctrl1<-renderUI({
    if(Data()[[2]][input$comparingAttr] == "Cate"){ #this must be true
      .choices<-unique(Data()[[1]][,input$comparingAttr])
      .names<-NULL
      for(c in .choices){
        sup<-length(which(Data()[[1]][,input$comparingAttr] == c))
        .names<-c(.names,paste(c," (",sup,")",sep=""))
      }
      names(.choices)<-.names
      
      checkboxGroupInput("whichcmpclassesX",
                         "Indicate which comparing attribute class to form group 1",
                         choices=c(sort(.choices)))
    }
  }) #return: input$whichcmpclassesX
  output$test.cmp.class.ctrl2<-renderUI({
    if(Data()[[2]][input$comparingAttr] == "Cate"){ #this must be true
      .choices<-unique(Data()[[1]][,input$comparingAttr])
      .names<-NULL
      for(c in .choices){
        sup<-length(which(Data()[[1]][,input$comparingAttr] == c))
        .names<-c(.names,paste(c," (",sup,")",sep=""))
      }
      names(.choices)<-.names
      
      checkboxGroupInput("whichcmpclassesY",
                         "Indicate which comparing attribute classes to form group 2",
                         choices=c(sort(.choices)))
    }
  }) #return: input$whichcmpclassesY

  # === Initial context === #
  
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
    
  }) #return: input$ctxItems
  
  #*********************************************#
  #***************REACTIVE**********************#
  #*********************************************#

  # Groupings() is a simple reactive module to
  # -> Keep track of type of Atgt (numerical or cate)
  # -> keep track of tgt.class and cmp.class groupings
  # mainly for displaying the correct context in the UI
  
  # Groupings() consists of *THREE* things at the moment:
  # 1. Groupings()[[1]] is Atgt type
  # 2. Groupings()[[2]] is Atgt.names
  # 3. Groupings()[[3]] is Acmp.names
  
  Groupings<-reactive({
    if(Data()[[2]][input$targetAttr] == "Num")
      return(list(Atgt.type="Num",
                  Atgt.names=c(paste(input$targetAttr," above/equal mean",sep=""), # <----
                               paste(input$targetAttr," below mean",sep="")),
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
  #***************END REACTIVE******************#
  #*********************************************#
  
  #*********************************************#
  #***************REACTIVE**********************#
  #*********************************************#
  
  # The objectives of Data2() are:
  #  -> subsetting the data based on the user's initial context
  #  -> if Atgt is numerical, include a binary attribute based on
  #     mean(Atgt). This is done because it will speed up the
  #     construction of the RF models later. (Regression RF is
  #     apparently slower than classification RF.)
  
  # Because Data() contains information regarding the attributes,
  # while Data2() considers starting context formed by specific
  # classes of Atgt and Acmp, information in Data() can be re-used, except Data()[[3]].
  # Data()[[3]] contains the number of classes for each categorical attribute,
  # which changes for Atgt and Acmp, depending on initial context
  # 030914: will return Data()[[[3]]] as it is anyway for now.

  # Data2() consists of *THREE* things at the moment
  #  1. Data2()[[1]] is the data itself, including the mean cutoff attribute if Atgt is num
  #  2. Data2()[[2]] is the type of variable: numerical or categorical
  #  3. Data2()[[3]] is the number of classes for categorical attributes, NA for numerical.
  
  Data2<-reactive({
    
    # three steps:
    # 1. if Atgt is numerical, add the tgt.class attribute based on mean first
    # 2. subset the data to include the required rows, based on tgt, cmp and ctx items
    # 3. finally, add tgt.class for cate Atgt and cmp.class based on tgt and cmp items
    
    dfWithCtx<-Data()[[1]]
    
    #retrieve all elements of initial context first
    grpA.classes<-input$whichtgtclassesA # <--- could be NULL if Atgt is numerical
    grpB.classes<-input$whichtgtclassesB # <--- could be NULL
    grpX.classes<-input$whichcmpclassesX
    grpY.classes<-input$whichcmpclassesY
    ctx.attr    <-input$ctxAttr
    ctx.items   <-input$ctxItems # in the format of Actx = vctx
    
    rowsToUse.cmp<-seq(nrow(dfWithCtx))
    rowsToUse.tgt<-seq(nrow(dfWithCtx))
    rowsToUse.ctx<-NULL
    
    # step one: if Atgt is numerical, add the tgt.class attribute based on mean first
    if(Groupings()[[1]] == "Num"){
      m<-mean(dfWithCtx[,input$targetAttr])
      #using mean instead of median,
      #because median cannot handle extremely skewed data
      #print(unique(dfWithCtx[,input$targetAttr]))
      dfWithCtx$tgt.class<-sapply(dfWithCtx[,input$targetAttr],
                                  FUN=function(x){
                                    if(x>=m) return("above/equal mean")
                                    else return("below mean")})
    }
    
    # step two: subset the data to include the required rows, based on tgt, cmp and ctx items
    # start with Acmp
    rowsToUse.cmp<-which(dfWithCtx[,input$comparingAttr] %in% input$whichcmpclassesX == TRUE) #only X
    rowsToUse.cmp<-union(
      rowsToUse.cmp,
      which(dfWithCtx[,input$comparingAttr] %in% input$whichcmpclassesY == TRUE)) #X U Y
    
    # now with Atgt
    if(Data()[[2]][input$targetAttr] == "Cate"){
      rowsToUse.tgt<-which(dfWithCtx[,input$targetAttr] %in% input$whichtgtclassesA == TRUE) #only A
      rowsToUse.tgt<-union(
        rowsToUse.tgt,
        which(dfWithCtx[,input$targetAttr] %in% input$whichtgtclassesB == TRUE)) #A U B
    }
    rowsToUse<-intersect(rowsToUse.tgt,rowsToUse.cmp)    
    # now for Actx
    if(!is.null(ctx.attr)){
      items.df<-data.frame(t(data.frame(sapply(ctx.items,FUN=strsplit,"="),
                                        stringsAsFactors=F)),
                           stringsAsFactors=F) #ugly, will change this later
      rownames(items.df)<-NULL
      colnames(items.df)<-c("Actx","vctx")
  
      # based on the ctx items, retrieve the rows
      list.of.rows<-vector("list",length(ctx.attr))
      for(i in seq(length(ctx.attr))){
        # from items.df, find all Actx that are the same
        # from these, need to concatenate the rowsToUse
        # when the Actx differs, do intersect
        where.in.items.df<-which(items.df$Actx == ctx.attr[i])
        ctx.classes<-items.df$vctx[where.in.items.df]
        for(a.class in ctx.classes){
          list.of.rows[[i]]<-c(list.of.rows[[i]],which(dfWithCtx[,ctx.attr[i]] == a.class))
        }
      }
      rowsToUse.ctx<-list.of.rows[[1]]
      for(i in seq(length(list.of.rows)-1)){
        if(length(list.of.rows) != 1) rowsToUse.ctx<-intersect(rowsToUse.ctx,list.of.rows[[i+1]])
        # weird behavior of seq(): seq(0) == c(1,0) !
      }
      # now, combine the rowsToUse
      rowsToUse<-intersect(rowsToUse.cmp,intersect(rowsToUse.tgt,rowsToUse.ctx))
    }
    # retrieve the data
    dfWithCtx<-dfWithCtx[rowsToUse,]

    # step three: finally, add tgt.class for cate Atgt and cmp.class based on tgt and cmp items
    if(Groupings()[[1]] == "Cate" && is.null(dfWithCtx$tgt.class)){
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
    # tgt.class and cmp.class do not contain the Atgt.names and Acmp.names as per in Groupings()
    # they only contain "above/equal mean"/"below mean" or "1"/"2"
    
    #add the attribute type for the cutoff attribute
    attr.type<-Data()[[2]]
    attr.type<-c(attr.type,"Cate","Cate")
    
    #**console**#
    #print(paste("nrow(dfWithCtx): ",nrow(dfWithCtx)))
    
    # number of classes
    numCl<-NULL
    for(j in seq(ncol(dfWithCtx))){
      if(attr.type[j] == "Cate") numCl<-c(numCl,length(unique(dfWithCtx[,j])))
      else if(attr.type[j] == "Num") numCl<-c(numCl,NA)
    }
    
    return(list(dfWithCtx,attr.type,numCl))
    #081014: context bug resolved
  })
  
  #*********************************************#
  #***************END REACTIVE******************#
  #*********************************************#
  
  #*********************************************#
  #***************REACTIVE**********************#
  #*********************************************#
  
  # Table() consists of *THREE OR FOUR* things at the moment
  #  1. Table()[["means.df"]] is the comparison table for num Atgt
  #  2. Table()[["cont.tab"]]] is the contingency table, for both num or cate Atgt
  #  3. Table()[["tab.type"]] is the type of (primary) table
  #  4. Table()[["tab.df"]] is the data.frame that was tabulated with 4 columns: Atgt, Acmp, tgt.class, cmp.class
  
  # generate contingency table if target attribute is cate.
  # else generate a comparison table and a contingency table
  
  # reactive wrapper for table
  Table<-reactive({
    # retrieve the relevant data
    
    df<-Data2()[[1]][c(input$targetAttr,input$comparingAttr,"tgt.class","cmp.class")]
    
    #is the target attribute numerical or categorical?
    if(Data2()[[2]][input$targetAttr] == "Cate"){
      tab<-table(df[,c("cmp.class","tgt.class")])
      # cmp.class is rows, tgt.class is columns
      if(nrow(tab)*ncol(tab) == 4){
        rownames(tab)<-c(paste(input$whichcmpclassesX,collapse="&"),
                         paste(input$whichcmpclassesY,collapse="&"))
        colnames(tab)<-c(paste(input$whichtgtclassesA,collapse="&"),
                         paste(input$whichtgtclassesB,collapse="&"))
        
        return(list(cont.tab=tab,
                    tab.type="Contingency",
                    tab.df=df,
                    sufficient="Sufficient"))
      
      }
      else{
        tab<-data.frame("Insufficient support for hypothesis")
        colnames(tab)<-""
        return(list(cont.tab=tab,
                    tab.type="Contingency",
                    tab.df=df,
                    sufficient="Insufficient"))
      }
    }
    
    else{ #target attribute is numerical
      
      # want to return both the data.frame of means and
      # the contingency table based on tgt.class = {"above/equal mean", "below mean"}
      # 131014: for now only the data.frame of means will be displayed in the UI
      
      if(length(which(df$cmp.class == "1")) !=  0
         && length(which(df$cmp.class == "2")) != 0){
      
        # first, the data.frame of means and std deviation
        mean1<-mean(df[which(df$cmp.class == "1"),input$targetAttr])
        mean2<-mean(df[which(df$cmp.class == "2"),input$targetAttr])
        
        sd1<-sd(df[which(df$cmp.class == "1"),input$targetAttr])
        sd2<-sd(df[which(df$cmp.class == "2"),input$targetAttr])
        
        means.df<-data.frame(c(mean1,mean2))
        rownames(means.df)<-c(paste(input$whichcmpclassesX,collapse="&"),
                              paste(input$whichcmpclassesY,collapse="&"))
        
        means.df<-cbind(means.df,c(sd1,sd2))
        
        colnames(means.df)<-c(paste("Means of ",input$targetAttr,sep=""),
                              paste("sd of ",input$targetAttr,sep=""))
        
        # next, the contingency table
        tab<-table(df[,c("cmp.class","tgt.class")])
        rownames(tab)<-Groupings()[[3]] # Acmp, cmp.class
        colnames(tab)<-Groupings()[[2]] # Atgt, tgt.class
        
        return(list(means.df=means.df,
                    cont.tab=tab,
                    tab.type="Comparison",
                    tab.df=df,
                    sufficient="Sufficient"))
      }
      else{
        tab<-data.frame("Insufficent support for hypothesis")
        colnames(tab)<-""
        return(list(means.df=df,
                    cont.tab=tab,
                    tab.type="Comparison",
                    tab.df=df,
                    sufficient="Insufficient"))
      }
    }
  })

  #*********************************************#
  #***************END REACTIVE******************#
  #*********************************************#

  # === Table(s) & test(s) === #
  
  # for the next 4 output objects:
  # -> render text for comparison or contingency table
  # -> render the comparison or contingency table
  # -> render text for initial test
  # -> initial parametric test
  
  output$text.comp.or.cont<-renderText({
    if(Table()[["tab.type"]] == "Contingency" && Table()[["sufficient"]] == "Sufficient")
      return("Contingency table:")
    else if(Table()[["tab.type"]] == "Comparison" && Table()[["sufficient"]] == "Sufficient")
      return("Comparison table:")
  })
  output$contTable<-renderTable({
    tab<-Table()[[1]]
    
    if(Table()[["tab.type"]] == "Contingency" && Table()[["sufficient"]] == "Sufficient"){
      append.col<-c((tab[1,1]+tab[1,2]),
                    (tab[2,1]+tab[2,2]))
      append.row<-c((tab[1,1]+tab[2,1]),
                    (tab[1,2]+tab[2,2]),
                    sum(tab))
      append.col<-round(append.col,2)
      append.row<-round(append.row,2)
      
      # include cell proportions in rendered table
      cell.proportions<-c(tab[1,1]/(tab[1,1]+tab[1,2]),
                          tab[1,2]/(tab[1,1]+tab[1,2]),
                          tab[2,1]/(tab[2,1]+tab[2,2]),
                          tab[2,2]/(tab[2,1]+tab[2,2]))
      cell.proportions<-round(cell.proportions,2)
      
      tab[1,1]<-paste(tab[1,1]," (",cell.proportions[1],")",sep="")
      tab[1,2]<-paste(tab[1,2]," (",cell.proportions[2],")",sep="")
      tab[2,1]<-paste(tab[2,1]," (",cell.proportions[3],")",sep="")
      tab[2,2]<-paste(tab[2,2]," (",cell.proportions[4],")",sep="")
      
      tab<-cbind(tab,append.col)
      tab<-rbind(tab,append.row)
      colnames(tab)[ncol(tab)]<-rownames(tab)[nrow(tab)]<-"Total"
      return(tab)
    }
    else if(Table()[["tab.type"]] == "Comparison" && Table()[["sufficient"]] == "Sufficient"){
      
      cont.tab<-Table()[["cont.tab"]]
      append.col<-c((cont.tab[1,1]+cont.tab[1,2])/sum(cont.tab),
                    (cont.tab[2,1]+cont.tab[2,2])/sum(cont.tab))
      tab<-cbind(tab,
                 c(cont.tab[1,1]+cont.tab[1,2],cont.tab[2,1]+cont.tab[2,2]),
                 append.col)
      colnames(tab)[ncol(tab)-1]<-"Support"
      colnames(tab)[ncol(tab)]<-"Proportions"
      return(tab)
    }
    return(Table()[["cont.tab"]])
  })
  #===#
  output$text.initial.test<-renderText({
    return("Initial test:")
  })
  output$initialTest<-renderTable({
    # 081014: only t-test and chi-squared tests are used now.
    # no more ANOVA
    # 2 groups only

    # check the type of table
    if(Groupings()[[1]] == "Cate" && Table()[["sufficient"]] == "Sufficient"){
      test<-chisq.test(Table()[[1]]) #chisq.test() works on the table itself
      stats<-test$statistic
      pvalue<-test$p.value
      method<-test$method
      
      returnMe<-as.data.frame(c(as.character(method),
                                 as.character(round(stats,3)),
                                 as.character(pvalue)))
      rownames(returnMe)<-c("Method","Test statistic","p-value")
      colnames(returnMe)<-"Initial chi-squared test on contingency table"
      returnMe
    }
    else if(Groupings()[[1]] == "Num" && Table()[["sufficient"]] == "Sufficient"){
      #t-test
      test<-t.test(Data2()[[1]][,input$targetAttr]~Data2()[[1]]$cmp.class) #t-test bug resolved
      stats<-test$statistic
      pvalue<-test$p.value
      method<-test$method
      
      returnMe<-as.data.frame(c(as.character(method),
                                as.character(round(stats,3)),
                                as.character(pvalue)))
      rownames(returnMe)<-c("Method","Test statistic","p-value")
      colnames(returnMe)<-"Initial t-test on means"
      returnMe
    }
  })
  #===#
  # render contingency table for numerical Atgt as well
  output$text.comp.or.cont2<-renderText({
    if(Table()[["tab.type"]] == "Comparison")
      return(paste("Contingency table on discretized ",
                   input$targetAttr,
                   ": ",
                   sep=""))
  })
  output$contTable2<-renderTable({
    if(Table()[["tab.type"]] == "Comparison"){
      tab<-Table()[["cont.tab"]]
      append.col<-c((tab[1,1]+tab[1,2]),
                    (tab[2,1]+tab[2,2]))
      append.row<-c((tab[1,1]+tab[2,1]),
                    (tab[1,2]+tab[2,2]),
                    sum(tab))
      append.col<-round(append.col,2)
      append.row<-round(append.row,2)
      
      # include cell proportions in rendered table
      cell.proportions<-c(tab[1,1]/(tab[1,1]+tab[1,2]),
                          tab[1,2]/(tab[1,1]+tab[1,2]),
                          tab[2,1]/(tab[2,1]+tab[2,2]),
                          tab[2,2]/(tab[2,1]+tab[2,2]))
      cell.proportions<-round(cell.proportions,2)
      
      tab[1,1]<-paste(tab[1,1]," (",cell.proportions[1],")",sep="")
      tab[1,2]<-paste(tab[1,2]," (",cell.proportions[2],")",sep="")
      tab[2,1]<-paste(tab[2,1]," (",cell.proportions[3],")",sep="")
      tab[2,2]<-paste(tab[2,2]," (",cell.proportions[4],")",sep="")
      
      tab<-cbind(tab,append.col)
      tab<-rbind(tab,append.row)
      colnames(tab)[ncol(tab)]<-rownames(tab)[nrow(tab)]<-"Total"
      return(tab)
    }
  })
  #===#
  output$text.initial.test2<-renderText({
    if(Table()[["tab.type"]] == "Comparison")
      return("Chi-squared test on contingency table:")
  })
  output$initialTest2<-renderTable({
    
    if(Groupings()[[1]] == "Num"){
      test<-chisq.test(Table()[["cont.tab"]]) #chisq.test() works on the table itself
      stats<-test$statistic
      pvalue<-test$p.value
      method<-test$method
      
      returnMe<-as.data.frame(c(as.character(method),
                                as.character(round(stats,3)),
                                as.character(pvalue)))
      rownames(returnMe)<-c("Method","Test statistic","p-value")
      colnames(returnMe)<-"Chi-squared test on contingency table"
      returnMe
    }
  })

  # hypothesis statement
  output$hypothesis.statement.it<-renderText({
    if(is.null(input$targetAttr) || is.null(input$comparingAttr)) return("")
    
    tgt.attr<-input$targetAttr
    cmp.attr<-input$comparingAttr
    
    tgt.class1<-input$whichtgtclassesA # <--- could be NULL if Atgt is numerical
    tgt.class2<-input$whichtgtclassesB # <--- could be NULL
    cmp.class1<-input$whichcmpclassesX
    cmp.class2<-input$whichcmpclassesY
    
    ctx.attr    <-input$ctxAttr
    ctx.items   <-input$ctxItems # in the format of Actx = vctx
    
    ctx.items.text<-paste(ctx.items, collapse=" & ")
    tgt.class1.text<-paste(tgt.class1,collapse=" & ")
    tgt.class2.text<-paste(tgt.class2,collapse=" & ")
    cmp.class1.text<-paste(cmp.class1,collapse=" & ")
    cmp.class2.text<-paste(cmp.class2,collapse=" & ")
    
    if(Groupings()[[1]] == "Cate")
      statement<-paste("In the context of {",
                       ctx.items.text,
                       "}, is there a difference in ",
                       toupper(tgt.attr),
                       " between {",
                       tgt.class1.text,
                       "} vs. {",
                       tgt.class2.text,
                       "} when comparing the samples on ",
                       toupper(cmp.attr),
                       " between {",
                       cmp.class1.text,
                       "} vs. {",
                       cmp.class2.text,
                       "}?",
                       sep="")
    else if(Groupings()[[1]] == "Num")
      statement<-paste("In the context of {",
                       ctx.items.text,
                       "}, is there a difference in ",
                       toupper(tgt.attr),
                       " when comparing the samples on ",
                       toupper(cmp.attr),
                       " between {",
                       cmp.class1.text,
                       "} vs. {",
                       cmp.class2.text,
                       "}?",
                       sep="")
    return(statement)
  })
  
  #=============================================#
  #============4. Contexted data================#
  #=============================================#
  
  # display contexted data
  output$ctx.data<-renderTable({
    data.frame(Data2()[[1]][1:input$ctxRows,])
  },digits=3)
  
  # download contexted data
  output$ctx.download<-downloadHandler(
    filename = function() {paste("Redhyte_contexted_data", '.csv', sep='') },
    content = function(file) {
      write.csv(Data2()[[1]], file, row.names=FALSE)
    }
  )
  
  #=============================================#
  #============5. Test diagnostics==============#
  #=============================================#
  
  #*********************************************#
  #***************REACTIVE**********************#
  #*********************************************#
  
  Test<-reactive({
    if(Table()[["tab.type"]] == "Comparison"){
      test.type<-"t.test"
      test<-t.test(Data2()[[1]][,input$targetAttr]~Data2()[[1]]$cmp.class)
      if(length(input$whichcmpclassesX) > 1 || length(input$whichcmpclassesY) > 1){
        second.test.type<-"collapsed.chi.sq"
      }
      else second.test.type<-"chi.sq"
    }
    else if(Table()[["tab.type"]] == "Contingency"){
      test<-chisq.test(Table()[[1]])
      if(length(input$whichcmpclassesX) > 1 || length(input$whichcmpclassesY) > 1){
        test.type<-"collapsed.chi.sq"
      }
      else
        test.type<-"chi.sq"
      second.test.type<-NULL
    }
    return(list(test=test,
                test.type=test.type,
                second.test.type=second.test.type))
  })
  
  #*********************************************#
  #***************END REACTIVE******************#
  #*********************************************#
  
  output$hypothesis.statement.td<-renderText({
    if(is.null(input$targetAttr) || is.null(input$comparingAttr)) return("")
    
    tgt.attr<-input$targetAttr
    cmp.attr<-input$comparingAttr
    
    tgt.class1<-input$whichtgtclassesA # <--- could be NULL if Atgt is numerical
    tgt.class2<-input$whichtgtclassesB # <--- could be NULL
    cmp.class1<-input$whichcmpclassesX
    cmp.class2<-input$whichcmpclassesY
    
    ctx.attr    <-input$ctxAttr
    ctx.items   <-input$ctxItems # in the format of Actx = vctx
    
    ctx.items.text<-paste(ctx.items, collapse=" & ")
    tgt.class1.text<-paste(tgt.class1,collapse=" & ")
    tgt.class2.text<-paste(tgt.class2,collapse=" & ")
    cmp.class1.text<-paste(cmp.class1,collapse=" & ")
    cmp.class2.text<-paste(cmp.class2,collapse=" & ")
    
    if(Groupings()[[1]] == "Cate")
      statement<-paste("In the context of {",
                       ctx.items.text,
                       "},is there a difference in ",
                       toupper(tgt.attr),
                       " between {",
                       tgt.class1.text,
                       "} vs. {",
                       tgt.class2.text,
                       "} when comparing the samples on ",
                       toupper(cmp.attr),
                       " between {",
                       cmp.class1.text,
                       "} vs. {",
                       cmp.class2.text,
                       "}?",
                       sep="")
    else if(Groupings()[[1]] == "Num")
      statement<-paste("In the context of {",
                       ctx.items.text,
                       "}, is there a difference in ",
                       toupper(tgt.attr),
                       " when comparing the samples on ",
                       toupper(cmp.attr),
                       " between {",
                       cmp.class1.text,
                       "} vs. {",
                       cmp.class2.text,
                       "}?",
                       sep="")
    return(statement)
  })
  
  # Test diagnostics for numerical Atgt:
  # -> S-W test for normality of group 1 of Acmp
  # -> S-W test for normality of group 2 of Acmp
  # -> F-test for equal variances
  # -> M-W test for either of the both fails
  # -> display flat table
  # -> do flat chi-sq test, find top contributor
  # -> M-H test for 3rd attribute association (assumes no 3-way interaction)

  # === Continuous target attribute === #
  
  output$text.SWtest<-renderText({
    if(Test()[["test.type"]] == "t.test"){
      return("Normality Checks:")
    }
  })
  output$SWtest.cmp1<-renderTable({
    if(Test()[["test.type"]] == "t.test"){
      df<-Data2()[[1]][,c(input$targetAttr,"cmp.class")]
      numSam<-length(which(df$cmp.class == "1"))
      if(numSam >= 3 && numSam <=5000){
        test<-shapiro.test(df[which(df$cmp.class == "1"),input$targetAttr])
        
        stats<-test$statistic
        pvalue<-test$p.value
        method<-test$method
        
        returnMe<-as.data.frame(c(as.character(method),
                                  as.character(round(stats,3)),
                                  as.character(pvalue)))
        rownames(returnMe)<-c("Method","Test statistic","p-value")
        colnames(returnMe)<-paste("Shapiro-Wilk test for normality of ",
                                  input$targetAttr,
                                  " in ",
                                  input$comparingAttr,
                                  " = {",
                                  Groupings()[["Acmp.names"]][1],
                                  "}",sep="")
        returnMe
      }
      else if(numSam < 3){
        returnMe<-data.frame("Insufficient support for Shapiro Wilk test")
        colnames(returnMe)<-paste("Shapiro-Wilk test for normality of ",
                                  input$targetAttr,
                                  " in ",
                                  input$comparingAttr,
                                  " = {",
                                  Groupings()[["Acmp.names"]][1],
                                  "}",sep="")
        returnMe
        
      }
      else if(numSam > 5000){
        returnMe<-data.frame("Subpopulation too large for Shapiro Wilk test")
        colnames(returnMe)<-paste("Shapiro-Wilk test for normality of ",
                                  input$targetAttr,
                                  " in ",
                                  input$comparingAttr,
                                  " = {",
                                  Groupings()[["Acmp.names"]][1],
                                  "}",sep="")
        returnMe
      }
    }
  })
  output$SWtest.cmp2<-renderTable({
    if(Test()[["test.type"]] == "t.test"){
      df<-Data2()[[1]][,c(input$targetAttr,"cmp.class")]
      numSam<-length(which(df$cmp.class == "2"))
      if(numSam >= 3 && numSam <= 5000){
        test<-shapiro.test(df[which(df$cmp.class == "2"),input$targetAttr])
        
        stats<-test$statistic
        pvalue<-test$p.value
        method<-test$method
        
        returnMe<-as.data.frame(c(as.character(method),
                                  as.character(round(stats,3)),
                                  as.character(pvalue)))
        rownames(returnMe)<-c("Method","Test statistic","p-value")
        colnames(returnMe)<-paste("Shapiro-Wilk test for normality of ",
                                  input$targetAttr,
                                  " in ",
                                  input$comparingAttr,
                                  " = {",
                                  Groupings()[["Acmp.names"]][2],
                                  "}",sep="")
        returnMe
      }
      else if(numSam < 3){
        returnMe<-data.frame("Insufficient support for Shapiro Wilk test")
        colnames(returnMe)<-paste("Shapiro-Wilk test for normality of ",
                             input$targetAttr,
                             " in ",
                             input$comparingAttr,
                             " = {",
                             Groupings()[["Acmp.names"]][2],
                             "}",sep="")
        returnMe
      }
      else if(numSam > 5000){
        returnMe<-data.frame("Subpopulation too large for Shapiro Wilk test")
        colnames(returnMe)<-paste("Shapiro-Wilk test for normality of ",
                             input$targetAttr,
                             " in ",
                             input$comparingAttr,
                             " = {",
                             Groupings()[["Acmp.names"]][2],
                             "}",sep="")
        returnMe
      }
    }
  })
  #===#
  output$text.Ftest<-renderText({
    if(Test()[["test.type"]] == "t.test"){
      return("Equal variance check:")
    }
  })
  output$Ftest<-renderTable({
    if(Test()[["test.type"]] == "t.test"){
      df<-Data2()[[1]][,c(input$targetAttr,"cmp.class")]
      
      test<-var.test(df[which(df$cmp.class == "1"),input$targetAttr],
                     df[which(df$cmp.class == "2"),input$targetAttr])
      
      stats<-test$statistic
      pvalue<-test$p.value
      method<-test$method
      
      returnMe<-as.data.frame(c(as.character(method),
                                as.character(round(stats,3)),
                                as.character(pvalue)))
      rownames(returnMe)<-c("Method","Test statistic","p-value")
      colnames(returnMe)<-"F-test for equal variances"
      returnMe
    }
  })
  #===#
  output$text.MWtest<-renderText({
    if(Test()[["test.type"]] == "t.test"){
      
      df<-Data2()[[1]][,c(input$targetAttr,"cmp.class")]
      
      if(var.test(df[which(df$cmp.class == "1"),input$targetAttr],
                  df[which(df$cmp.class == "2"),input$targetAttr])$p.value <= input$p.significant ||
           shapiro.test(df[which(df$cmp.class == "1"),input$targetAttr])$p.value <= input$p.significant ||
           shapiro.test(df[which(df$cmp.class == "2"),input$targetAttr])$p.value <= input$p.significant){
        return("Non-parametric test:")
      }
    }
  })
  output$MWtest<-renderTable({
    if(Test()[["test.type"]] == "t.test"){
      
      df<-Data2()[[1]][,c(input$targetAttr,"cmp.class")]
      
      if(var.test(df[which(df$cmp.class == "1"),input$targetAttr],
                  df[which(df$cmp.class == "2"),input$targetAttr])$p.value <= input$p.significant ||
           shapiro.test(df[which(df$cmp.class == "1"),input$targetAttr])$p.value <= input$p.significant ||
           shapiro.test(df[which(df$cmp.class == "2"),input$targetAttr])$p.value <= input$p.significant){
        
        test<-wilcox.test(df[which(df$cmp.class == "1"),input$targetAttr],
                          df[which(df$cmp.class == "2"),input$targetAttr])
        
        stats<-test$statistic
        pvalue<-test$p.value
        method<-test$method
        
        returnMe<-as.data.frame(c(as.character(method),
                                  as.character(round(stats,3)),
                                  as.character(pvalue)))
        rownames(returnMe)<-c("Method","Test statistic","p-value")
        colnames(returnMe)<-"Non-parametric Wilcoxon rank sum test on means"
        returnMe
      }
    }
  })
  #===#
  output$text.MHtest.num<-renderText({
    if(Test()[["test.type"]] == "t.test"){
      return("Cochran-Mantel-Haenszel test:")
    }
  })
  output$MHtest.num<-renderTable({
    if(Groupings()[["Atgt.type"]] == "Num"){
      
      df<-Data2()[[1]]
      attr.type<-Data2()[[2]]
      
      #change all numerical attributes to categorical
      #discretise by the mean
      mean.discre<-function(an.attr){
        m<-mean(df[,an.attr])
        new.col<-sapply(df[,an.attr],
                        FUN=function(x){
                          if(x>=m) return("above/equal mean")
                          else return("below mean")})
        return(new.col)
      }
      
      which.are.num<-which(attr.type == "Num")
      for(an.attr in which.are.num)
        df[,an.attr]<-mean.discre(an.attr)
      
      df.dis<-df # discretized
      #str(df.dis)
      # now, for each attribute that is not the Atgt or Acmp,
      # use that attribute to be 3rd attribute
      # stratify the data according to the classes of the 3rd attribute
      # and create a 2 x 2 x K table for each attribute
      # finally, do a MH-test
      
      MH.df<-NULL
      for(j in seq(ncol(df.dis))){
        if(colnames(df.dis)[j] != "tgt.class" 
           && colnames(df.dis)[j] != "cmp.class"
           && colnames(df.dis)[j] != input$targetAttr
           && colnames(df.dis)[j] != input$comparingAttr){
          if(!any(colnames(df.dis)[j] == input$ctxAttr)){
            
            df.tmp<-df.dis[,"tgt.class"]
            df.tmp<-cbind(df.tmp,df.dis[,"cmp.class"])
            df.tmp<-cbind(df.tmp,df.dis[,j])
            
            df.tmp<-data.frame(df.tmp)
            tab<-table(df.tmp)
            sup<-as.vector(tab)
            
            if(any(sup <= 0) || length(sup) %% 4 != 0) # should be multiples of 4
              MH.df<-rbind(MH.df,c(colnames(df.dis)[j],
                                   "Insufficient",
                                   "Insufficient"))
            else if(length(sup) <= 4)
              MH.df<-rbind(MH.df,c(colnames(df.dis)[j],
                                   "k = 1",
                                   "k = 1"))
            else{
              test<-mantelhaen.test(tab)
              stats<-test$statistic
              pvalue<-test$p.value
              
              MH.df<-rbind(MH.df,c(colnames(df.dis)[j],
                                   round(stats,3),
                                   round(pvalue,3)))
            }
          }
        }
      }
    colnames(MH.df)<-c("3rd attribute","Cochran-Mantel-Haenszel Chi-squared","p-value")
    return(MH.df)
    }
  })
  #===#
  output$text.flat.table.num<-renderText({
    if(Test()[["test.type"]] == "t.test"
       && Test()[["second.test.type"]] == "collapsed.chi.sq"){
      return("Flat contingency table:")
    }
  })
  output$flat.table.num<-renderTable({
    if(Test()[["test.type"]] == "t.test"
       && Test()[["second.test.type"]] == "collapsed.chi.sq"){
      tab.df<-Table()[["tab.df"]][,c(2:3)] # ony Acmp and tgt.class, no cmp.class
      tab<-table(tab.df)
      colnames(tab)<-Groupings()[["Atgt.names"]]
      return(tab)
    }
  })
  #===#
  output$text.flat.chi.sq.num<-renderText({
    if(Test()[["test.type"]] == "t.test"
       && Test()[["second.test.type"]] == "collapsed.chi.sq"){
      return("Chi-squared test on flat contingency table:")
    }
  })
  output$flat.chi.sq.num<-renderTable({
    if(Test()[["test.type"]] == "t.test"
       && Test()[["second.test.type"]] == "collapsed.chi.sq"){
      tab.df<-Table()[["tab.df"]][,c(2:3)] # ony Acmp and tgt.class, no cmp.class
      tab<-table(tab.df)
      test<-chisq.test(tab)
      stats<-test$statistic
      pvalue<-test$p.value
      method<-test$method
      
      returnMe<-as.data.frame(c(as.character(method),
                                as.character(round(stats,3)),
                                as.character(pvalue)))
      rownames(returnMe)<-c("Method","Test statistic","p-value")
      colnames(returnMe)<-"Flat chi-squared test on discretized target attribute"
      returnMe
    }
  })
  #===#
  output$text.chi.sq.top.contributor<-renderText({
    if(Test()[["test.type"]] == "t.test"
       && Test()[["second.test.type"]] == "collapsed.chi.sq"){
      return("Chi-squared contributions:")
    }
  })
  output$chi.sq.top.contributor<-renderTable({
    if(Test()[["test.type"]] == "t.test"
       && Test()[["second.test.type"]] == "collapsed.chi.sq"){
      tab.df<-Table()[["tab.df"]][,c(2:3)] # only Acmp and tgt.class
      # not using Atgt,cmp.class
      tab<-table(tab.df)
      test<-chisq.test(tab)
      o<-test$observed
      e<-test$expected
      #vtgt<-colnames(o)[which(colnames(o) == "1")] # vtgt is tgt.class == 1
      #cmp.classes<-rownames(e) # <--- want to compute top contributor for Acmp,
      # for vtgt only
      chisq.contri<-cbind(o[,1],
                          e[,1],
                          ((((o-e)^2)/e)[,1])/test$statistic * 100)
      colnames(chisq.contri)<-c("Observed",
                                "Expected",
                                "Chi-squared contributions (%)")
      return(chisq.contri)
    }
  })

  # === Categorical target attribute === #
  
  # Test diagnostics for categorical Acmp, collapsed chi-sq
  # -> display flat table
  # -> do flat chi-sq test, find top contributor
  # -> M-H test for 3rd attribute assoication (assumes no 3-way interaction)
  
  output$text.flat.table.cate<-renderText({
    if(Test()[["test.type"]] == "collapsed.chi.sq"){
      return("Flat contingency table:")
    }
  })
  output$flat.table.cate<-renderTable({
    if(Test()[["test.type"]] == "collapsed.chi.sq"){
      tab.df<-Table()[["tab.df"]][,c(2:3)] # ony Acmp and tgt.class
      tab<-table(tab.df)
      
      colnames(tab)<-Groupings()[["Atgt.names"]]
      return(tab)
    }
  })
  #===#
  output$text.flat.chi.sq.cate<-renderText({
    if(Test()[["test.type"]] == "collapsed.chi.sq"){
      return("Chi-squared test on flat contingency table:")
    }
  })
  output$flat.chi.sq.cate<-renderTable({
    if(Test()[["test.type"]] == "collapsed.chi.sq"){
      
      tab.df<-Table()[["tab.df"]][,c(2:3)] # ony Acmp and tgt.class
      tab<-table(tab.df)
      test<-chisq.test(tab)
      stats<-test$statistic
      pvalue<-test$p.value
      method<-test$method
      
      returnMe<-as.data.frame(c(as.character(method),
                                as.character(round(stats,3)),
                                as.character(pvalue)))
      rownames(returnMe)<-c("Method","Test statistic","p-value")
      colnames(returnMe)<-"Flat chi-squared test"
      returnMe
    }
  })
  #===#
  output$text.chi.sq.top.cate<-renderText({
    if(Test()[["test.type"]] == "collapsed.chi.sq"){
      return("Chi-squared contributions:")
    }
  })
  output$chi.sq.top.cate<-renderTable({
    if(Test()[["test.type"]] == "collapsed.chi.sq"){
      tab.df<-Table()[["tab.df"]][,c(2:3)] # only Acmp and tgt.class
      # not using Atgt,cmp.class
      tab<-table(tab.df)
      test<-chisq.test(tab)
      o<-test$observed
      e<-test$expected
      vtgt<-colnames(o)[which(colnames(o) == "1")] # vtgt is tgt.class == 1
      cmp.classes<-rownames(e) # <--- want to compute top contributor for Acmp,
      # for vtgt only
      chisq.contri<-cbind(o[,1],
                          e[,1],
                          ((((o-e)^2)/e)[,1])/test$statistic * 100)
      colnames(chisq.contri)<-c("Observed",
                                "Expected",
                                "Chi-squared contributions (%)")
      return(chisq.contri)
    }
  })
  #===#
  output$text.MHtest.cate<-renderText({
    if(Test()[["test.type"]] == "collapsed.chi.sq" || Test()[["test.type"]] == "chi.sq"){
      return("Cochran-Mantel-Haenszel test:")
    }
  })
  output$MHtest.cate<-renderTable({
    
    if(Groupings()[["Atgt.type"]] == "Cate"){
      df<-Data2()[[1]]
      attr.type<-Data2()[[2]]
      
      #change all numerical attributes to categorical
      #discretise by the mean
      mean.discre<-function(an.attr){
        m<-mean(df[,an.attr])
        new.col<-sapply(df[,an.attr],
                        FUN=function(x){
                          if(x>=m) return("above/equal mean")
                          else return("below mean")})
        return(new.col)
      }
      
      which.are.num<-which(attr.type == "Num")
      for(an.attr in which.are.num)
        df[,an.attr]<-mean.discre(an.attr)
      
      df.dis<-df # discretized
      
      # now, for each attribute that is not the Atgt or Acmp,
      # use that attribute to be the 3rd attribute
      # stratify the data according to the classes of the 3rd attribute
      # and create a 2 x 2 x K table for each attribute
      # finally, do a MH-test
      
      MH.df<-NULL
      for(j in seq(ncol(df.dis))){
        if(colnames(df.dis)[j] != "tgt.class" 
           && colnames(df.dis)[j] != "cmp.class"
           && colnames(df.dis)[j] != input$targetAttr
           && colnames(df.dis)[j] != input$comparingAttr){
          if(!any(colnames(df.dis)[j] == input$ctxAttr)){
            
            df.tmp<-df.dis[,"tgt.class"]
            df.tmp<-cbind(df.tmp,df.dis[,"cmp.class"])
            df.tmp<-cbind(df.tmp,df.dis[,j])
            
            df.tmp<-data.frame(df.tmp)
            tab<-table(df.tmp)
            
            sup<-as.vector(tab)
            
            if(any(sup <= 0) || length(sup) %% 4 != 0)
              MH.df<-rbind(MH.df,c(colnames(df.dis)[j],
                                   "Insufficient",
                                   "Insufficient"))
            else if(length(sup) <= 4)
              MH.df<-rbind(MH.df,c(colnames(df.dis)[j],
                                   "k = 1",
                                   "k = 1"))
            else{
              test<-mantelhaen.test(tab)
              stats<-test$statistic
              pvalue<-test$p.value
              
              MH.df<-rbind(MH.df,c(colnames(df.dis)[j],
                                   round(stats,3),
                                   round(pvalue,3)))
            }
          }
        }
      }
      colnames(MH.df)<-c("3rd attribute","Cochran-Mantel-Haenszel Chi-squared","p-value")
      return(MH.df)
    }
  })

  #*********************************************#
  #***************REACTIVE**********************#
  #*********************************************#
  
  # Objective of Data3(): discretize all attributes for ctx mining

  Data3<-reactive({
    df<-Data2()[[1]]
    attr.type<-Data2()[[2]]
    
    #change all numerical attributes to categorical before hypothesis mining
    #discretise by the mean
    mean.discre<-function(an.attr){
      m<-mean(df[,an.attr])
      new.col<-sapply(df[,an.attr],
                      FUN=function(x){
                        if(x>=m) return("above/equal mean")
                        else return("below mean")})
      return(new.col)
    }
    
    which.are.num<-which(attr.type == "Num")
    for(an.attr in which.are.num)
      df[,an.attr]<-mean.discre(an.attr)
    
    attr.type<-rep("Cate",length(attr.type))
    
    # number of classes
    numCl<-NULL
    for(j in seq(ncol(df))){
      if(attr.type[j] == "Cate") numCl<-c(numCl,length(unique(df[,j])))
      else if(attr.type[j] == "Num") numCl<-c(numCl,NA)
    }
    
    return(list(df,attr.type,numCl))
  })
  
  #*********************************************#
  #***************END REACTIVE******************#
  #*********************************************#
  
  #=============================================#
  #============6. Context mining================#
  #=============================================#
  
  output$hypothesis.statement.cm<-renderText({
    
    if(is.null(input$targetAttr) || is.null(input$comparingAttr)) return("")
    
    tgt.attr<-input$targetAttr
    cmp.attr<-input$comparingAttr
    
    tgt.class1<-input$whichtgtclassesA # <--- could be NULL if Atgt is numerical
    tgt.class2<-input$whichtgtclassesB # <--- could be NULL
    cmp.class1<-input$whichcmpclassesX
    cmp.class2<-input$whichcmpclassesY
    
    ctx.attr    <-input$ctxAttr
    ctx.items   <-input$ctxItems # in the format of Actx = vctx
    
    ctx.items.text<-paste(ctx.items, collapse=" & ")
    tgt.class1.text<-paste(tgt.class1,collapse=" & ")
    tgt.class2.text<-paste(tgt.class2,collapse=" & ")
    cmp.class1.text<-paste(cmp.class1,collapse=" & ")
    cmp.class2.text<-paste(cmp.class2,collapse=" & ")
    
    if(Groupings()[[1]] == "Cate")
      statement<-paste("In the context of {",
                       ctx.items.text,
                       "}, is there a difference in ",
                       toupper(tgt.attr),
                       " between {",
                       tgt.class1.text,
                       "} vs. {",
                       tgt.class2.text,
                       "} when comparing the samples on ",
                       toupper(cmp.attr),
                       " between {",
                       cmp.class1.text,
                       "} vs. {",
                       cmp.class2.text,
                       "}?",
                       sep="")
    else if(Groupings()[[1]] == "Num")
      statement<-paste("In the context of {",
                       ctx.items.text,
                       "}, is there a difference in ",
                       toupper(tgt.attr),
                       #" between {above/equal mean} vs. {below mean}",
                       " when comparing the samples on ",
                       toupper(cmp.attr),
                       " between {",
                       cmp.class1.text,
                       "} vs. {",
                       cmp.class2.text,
                       "}?",
                       sep="")
    return(statement)
  })
  
  # === Attribute to exclude === #
  
  output$attr.to.exclude<-renderUI({
    cinitial.attr<-c(input$targetAttr,
                     input$comparingAttr,
                     input$ctxAttr,
                     "tgt.class",
                     "cmp.class")
    
    to.mine.from<-colnames(Data3()[[1]])[which(colnames(Data3()[[1]]) %in% cinitial.attr == FALSE)]
    
    checkboxGroupInput("exclude.attr",
                       "Which attribute to exclude from context mining?",
                       choices=to.mine.from)
  }) #return: input$exclude.attr
  
  #*********************************************#  
  #***************REACTIVE**********************#
  #*********************************************#
  
  minedAttributes<-reactive({
    set.seed(1234)
    #isolate({start.mining<-input$start.ctx.mining}) #isolate was here
    #if(start.mining == TRUE){
    df<-Data3()[[1]]
    
    #need to convert the character attributes to factors first before building models
    #initial data input options use stringsAsFactors=FALSE (see doc.txt)
    which.are.char<-which(Data3()[[2]] == "Cate") # all are categorical
    df[,which.are.char]<-lapply(df[,which.are.char],factor)
    
    # formulate the formulae for random forest models
    
    # start with Atgt
    # find all the predictors to be used in models
    # attributes to exclude
    # -> Atgt
    # -> Acmp
    # -> all Actx from Cinitial
    # -> input$exclude.attr
    predictors<-colnames(df)[which(colnames(df) != input$comparingAttr)]
    
    predictors<-intersect(predictors,
                          colnames(df)[which(colnames(df) != input$targetAttr)])
    
    predictors<-intersect(predictors,
                          colnames(df)[which(colnames(df) != "tgt.class")])
    
    predictors<-intersect(predictors,
                          colnames(df)[which(colnames(df) != "cmp.class")])
    
    predictors<-intersect(predictors,
                          colnames(df)[which(colnames(df) %in% input$ctxAttr == FALSE)])
    
    predictors<-intersect(predictors,
                          colnames(df)[which(colnames(df) %in% input$exclude.attr == FALSE)])
    
    fm.tgt<-paste(" ",predictors,sep="",collapse="+")
    fm.tgt<-as.formula(paste("tgt.class","~",fm.tgt,sep=""))
    
    # now formulate for Acmp
    fm.cmp<-paste(" ",predictors,sep="",collapse="+")
    fm.cmp<-as.formula(paste("cmp.class","~",fm.cmp,sep=""))
    
    #print(fm.tgt)
    #print(fm.cmp)
    
    # take the first k attributes, consider them shortlisted
    if(length(predictors) < input$top.k)
      k<-length(predictors)
    else
      k<-input$top.k
    
    #print(k)
    
    withProgress(session, {
      setProgress(message="Mining context...",detail="This might take a while...")
      # construct models
      run.time.tgt<-system.time(
        mod.tgt<-randomForest(formula=fm.tgt,
                              data=df,
                              importance=TRUE))[3]
      setProgress(message="Target model constructed!",detail = "Constructing comparing model...")
      run.time.cmp<-system.time(
        mod.cmp<-randomForest(formula=fm.cmp,
                              data=df,
                              importance=TRUE))[3]
      setProgress(message="Comparing model constructed.")
      
      # now evaluate whether the models has been accurate
      # three things to consider:
      #  @ whether Atgt is numerical or categorical
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
      
      # 211014: using adjusted geometric mean as a start
      # need to first figure out which class is less
      # arbitrarily consider 3:1 as class-imbalance
      # AGm = (Gm+SP*Nn)/(1+Nn),
      # where SP refers to the specificity, which is the sensitivity for the negative class
      # Nn refers to proportion of the data that belongs to the more abundant class
      
      # mod.tgt first
      sup.tgt1<-tmp.cm.tgt[1,1]+tmp.cm.tgt[1,2]
      sup.tgt2<-tmp.cm.tgt[2,1]+tmp.cm.tgt[2,2]
      if(sup.tgt1/sup.tgt2 >= input$class.ratio){
        # tgt1 is more abundant => tgt1 is -ve
        # tgt2 is less abundant => tgt2 is +ve
        se<-tmp.cm.tgt[2,2]/(tmp.cm.tgt[2,2]+tmp.cm.tgt[2,1])
        sp<-tmp.cm.tgt[1,1]/(tmp.cm.tgt[1,1]+tmp.cm.tgt[1,2])
        gm<-sqrt(se*sp)
        nn<-sup.tgt1/(sup.tgt1+sup.tgt2)
        acc.tgt<-(gm+sp*nn)/(1+nn)
      }
      else if(sup.tgt2/sup.tgt1 >= input$class.ratio){
        # tgt2 is more abundant => tgt2 is -ve
        # tgt1 is less abundant => tgt1 is +ve
        sp<-tmp.cm.tgt[2,2]/(tmp.cm.tgt[2,2]+tmp.cm.tgt[2,1])
        se<-tmp.cm.tgt[1,1]/(tmp.cm.tgt[1,1]+tmp.cm.tgt[1,2])
        gm<-sqrt(se*sp)
        nn<-sup.tgt2/(sup.tgt1+sup.tgt2)
        acc.tgt<-(gm+sp*nn)/(1+nn)
      }
      
      # now for mod.cmp
      sup.cmp1<-tmp.cm.cmp[1,1]+tmp.cm.cmp[1,2]
      #print(sup.cmp1)
      sup.cmp2<-tmp.cm.cmp[2,1]+tmp.cm.cmp[2,2]
      #print(sup.cmp2)
      if(sup.cmp1/sup.cmp2 >= input$class.ratio){
        #print("class1")
        # cmp1 is more abundant => cmp1 is -ve
        # cmp2 is less abundant => cmp2 is +ve
        se<-tmp.cm.cmp[2,2]/(tmp.cm.cmp[2,2]+tmp.cm.cmp[2,1])
        sp<-tmp.cm.cmp[1,1]/(tmp.cm.cmp[1,1]+tmp.cm.cmp[1,2])
        gm<-sqrt(se*sp)
        nn<-sup.cmp1/(sup.cmp1+sup.cmp2)
        acc.cmp<-(gm+sp*nn)/(1+nn)
      }
      else if(sup.cmp2/sup.cmp1 >= input$class.ratio){
        #print("class2")
        # cmp2 is more abundant => cmp2 is -ve
        # cmp1 is less abundant => cmp1 is +ve
        sp<-tmp.cm.cmp[2,2]/(tmp.cm.cmp[2,2]+tmp.cm.cmp[2,1])
        se<-tmp.cm.cmp[1,1]/(tmp.cm.cmp[1,1]+tmp.cm.cmp[1,2])
        gm<-sqrt(se*sp)
        nn<-sup.cmp2/(sup.cmp1+sup.cmp2)
        acc.cmp<-(gm+sp*nn)/(1+nn)
      }
      
      print(paste("acc.tgt: ",acc.tgt,sep=""))
      print(paste("acc.cmp: ",acc.cmp,sep=""))
      
      # compare the accuracies of the models with the default accuracy threshold
      # defined in related_codes/settings.R
      # 3 cases:
      # -> acc.tgt >= acc.rf.default && acc.cmp >= acc.rf.default
      # -> acc.tgt >= acc.rf.default && acc.cmp <  acc.rf.default
      # -> acc.tgt <  acc.rf.default && acc.cmp >= acc.rf.default
      
      mined.attr<-NULL
      
      if(acc.tgt >= input$acc.rf.default && acc.cmp < input$acc.rf.default){
        if(k==1) mined.attr<-predictors
        else{
          mined.attr<-names(sort(mod.tgt$importance[,"MeanDecreaseAccuracy"],decreasing=TRUE))[seq(k)]        
          names(mined.attr)<-paste(mined.attr,".tgt",sep="") # adding a tail ".tgt" or ".cmp"
        }
      }
      else if(acc.tgt < input$acc.rf.default && acc.cmp >= input$acc.rf.default){
        if(k==1) mined.attr<-predictors
        else{
          mined.attr<-names(sort(mod.cmp$importance[,"MeanDecreaseAccuracy"],decreasing=TRUE))[seq(k)]
          names(mined.attr)<-paste(mined.attr,".cmp",sep="")
        }
      }
      else if(acc.tgt >= input$acc.rf.default && acc.cmp >= input$acc.rf.default){
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
        
        mined.attr<-names(mda.both)[seq(k)]
        
        # function to remove the tails ".tgt" and ".cmp"
        remove.tail<-function(s){
          last.dot<-regexpr("\\.[^\\.]*$", s)
          return(substr(s,1,last.dot-1))
        }
        mined.attr<-sapply(mined.attr,FUN=remove.tail)
        
        #print(mined.attr)
        
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
        
        # special case when k = 1, and both models are accurate
        if(k == 1)
          mined.attr<-predictors
      }
      
      #**console**#
      print(paste("mined.attr: ",names(mined.attr)))
      
      cm.tgt<-mod.tgt$confusion
      cm.cmp<-mod.cmp$confusion
      
      # naming the confusion matrices rendered in UI correctly
      rownames.cm.tgt<-sapply(Groupings()[[2]],
                          FUN=function(x){
                            return(paste("Actual: ",x,sep=""))
                          })
      colnames.cm.tgt<-sapply(Groupings()[[2]],
                              FUN=function(x){
                                return(paste("Predicted: ",x,sep=""))
                              })
      rownames.cm.cmp<-sapply(Groupings()[[3]],
                              FUN=function(x){
                                return(paste("Actual: ",x,sep=""))
                              })
      colnames.cm.cmp<-sapply(Groupings()[[3]],
                              FUN=function(x){
                                return(paste("Predicted: ",x,sep=""))
                              })
      
      #print(rownames.cm.tgt)
      rownames(cm.tgt)<-rownames.cm.tgt
      colnames(cm.tgt)[1:2]<-colnames.cm.tgt
      rownames(cm.cmp)<-rownames.cm.cmp
      colnames(cm.cmp)[1:2]<-colnames.cm.cmp
      
      if(!is.null(mined.attr)) return(list(cm.tgt,
                                           cm.cmp,
                                           mined.attr=mined.attr,
                                           run.time.tgt=run.time.tgt,
                                           run.time.cmp=run.time.cmp,
                                           mod.tgt=mod.tgt,
                                           mod.cmp=mod.cmp))
      
      #both mod.tgt and mod.cmp are inaccurate, therefore no mined attributes
      else return(list(cm.tgt,
                       cm.cmp,
                       mined.attr=NULL,
                       run.time.tgt=run.time.tgt,
                       run.time.cmp=run.time.cmp,
                       mod.tgt=mod.tgt,
                       mod.cmp=mod.cmp))
      
    })
    #}
  })
  
  #*********************************************#
  #***************END REACTIVE******************#
  #*********************************************#
  
  # === Mined context attributes === #
  
  output$run.time.tgt<-renderText({
    return(paste("Runtime for target model: ",round(minedAttributes()[["run.time.tgt"]],3),sep=""))
  })
  output$run.time.cmp<-renderText({
    return(paste("Runtime for comparing model: ",round(minedAttributes()[["run.time.cmp"]],3),sep=""))
  })

  # confusion matrices
  output$cm.tgt<-renderTable({
    minedAttributes()[[1]]
  })
  output$cm.cmp<-renderTable({
    minedAttributes()[[2]]
  })
  
  # list of mined attributes
  output$minedAttr<-renderTable({
    if(is.null(minedAttributes()[[3]])){
      tmp<-data.frame("No significant context attributes were found")
      colnames(tmp)<-""
      return(tmp)
    }
    
    else if(length(minedAttributes()[[3]]) == 1){
      df<-data.frame(minedAttributes()[[3]])
      colnames(df)<-"Mined context attributes"
      return(df)
    }
    df<-data.frame(minedAttributes()[[3]])
    
    # function to retrieve which model did the attributes come from
    remove.head<-function(s){
      last.dot<-regexpr("\\.[^\\.]*$", s)
      return(substr(s,last.dot+1,nchar(s)))
    }
    from.which.model<-sapply(rownames(df),FUN=remove.head)
    for(i in 1:length(from.which.model)){
      if(from.which.model[i] == "tgt") from.which.model[i]<-"Target"
      else if(from.which.model[i] == "cmp") from.which.model[i]<-"Comparing"
    }
    
    df<-cbind(df,from.which.model)
    colnames(df)<-c("Mined context attributes","From which model?")
    rownames(df)<-NULL
    df
  })
  
  # === Variable importance === #
  
  # variable importance plot of RF models
  output$VIplot.tgt<-renderPlot({
    if(length(minedAttributes()[[3]]) > 1)
      varImpPlot(minedAttributes()[["mod.tgt"]],main="Variable Importance for Target Model")
  })
  output$VIplot.cmp<-renderPlot({
    if(length(minedAttributes()[[3]]) > 1)
    varImpPlot(minedAttributes()[["mod.cmp"]],main="Variable Importance for Comparing Model")
  })
  
  # === Visualization === #
  
  #visualization of the mined context attrtibutes
  output$minedAttrCtrl<-renderUI({
    selectizeInput("mined.attr",
                   "Which mined attribute?",
                   minedAttributes()[[3]])
  }) #return: input$mined.attr
  
  # contingency table of initial hypothesis in viz of mined attributes
  output$contTable.ctx<-renderTable({
    tab<-Table()[[1]]
    
    if(Table()[["tab.type"]] == "Contingency"){
      append.col<-c((tab[1,1]+tab[1,2]),
                    (tab[2,1]+tab[2,2]))
      append.row<-c((tab[1,1]+tab[2,1]),
                    (tab[1,2]+tab[2,2]),
                    sum(tab))
      append.col<-round(append.col,2)
      append.row<-round(append.row,2)
      
      # include cell proportions in rendered table
      cell.proportions<-c(tab[1,1]/(tab[1,1]+tab[1,2]),
                          tab[1,2]/(tab[1,1]+tab[1,2]),
                          tab[2,1]/(tab[2,1]+tab[2,2]),
                          tab[2,2]/(tab[2,1]+tab[2,2]))
      cell.proportions<-round(cell.proportions,2)
      
      tab[1,1]<-paste(tab[1,1]," (",cell.proportions[1],")",sep="")
      tab[1,2]<-paste(tab[1,2]," (",cell.proportions[2],")",sep="")
      tab[2,1]<-paste(tab[2,1]," (",cell.proportions[3],")",sep="")
      tab[2,2]<-paste(tab[2,2]," (",cell.proportions[4],")",sep="")
      
      tab<-cbind(tab,append.col)
      tab<-rbind(tab,append.row)
      
      colnames(tab)[ncol(tab)]<-rownames(tab)[nrow(tab)]<-"Total"
      
      return(tab)
    }
    else if(Table()[["tab.type"]] == "Comparison"){
      cont.tab<-Table()[["cont.tab"]]
      append.col<-c((cont.tab[1,1]+cont.tab[1,2])/sum(cont.tab),
                    (cont.tab[2,1]+cont.tab[2,2])/sum(cont.tab))
      tab<-cbind(tab,
                 c(cont.tab[1,1]+cont.tab[1,2],cont.tab[2,1]+cont.tab[2,2]),
                 append.col)
      colnames(tab)[ncol(tab)-1]<-"Support"
      colnames(tab)[ncol(tab)]<-"Proportions"
      return(tab)
      
    }
  })
  output$contTable2.ctx<-renderTable({
    if(Table()[["tab.type"]] == "Comparison"){
      tab<-Table()[["cont.tab"]]
      append.col<-c((tab[1,1]+tab[1,2]),
                    (tab[2,1]+tab[2,2]))
      append.row<-c((tab[1,1]+tab[2,1]),
                    (tab[1,2]+tab[2,2]),
                    sum(tab))
      append.col<-round(append.col,2)
      append.row<-round(append.row,2)
      
      # include cell proportions in rendered table
      cell.proportions<-c(tab[1,1]/(tab[1,1]+tab[1,2]),
                          tab[1,2]/(tab[1,1]+tab[1,2]),
                          tab[2,1]/(tab[2,1]+tab[2,2]),
                          tab[2,2]/(tab[2,1]+tab[2,2]))
      cell.proportions<-round(cell.proportions,2)
      
      tab[1,1]<-paste(tab[1,1]," (",cell.proportions[1],")",sep="")
      tab[1,2]<-paste(tab[1,2]," (",cell.proportions[2],")",sep="")
      tab[2,1]<-paste(tab[2,1]," (",cell.proportions[3],")",sep="")
      tab[2,2]<-paste(tab[2,2]," (",cell.proportions[4],")",sep="")
      
      tab<-cbind(tab,append.col)
      tab<-rbind(tab,append.row)
      colnames(tab)[ncol(tab)]<-rownames(tab)[nrow(tab)]<-"Total"
      return(tab)
    }
  })

  #render the plot for one mined attribute indicated by user
  output$mined.attr.viz<-renderPlot({
    if(is.null(minedAttributes()[[3]])) return(NULL)
    tgt.attr<-input$targetAttr
    cmp.attr<-input$comparingAttr
    mined.attr<-input$mined.attr
    
    #grab the relevant data
    df.to.plot<-Data2()[[1]][,c(tgt.attr,cmp.attr,mined.attr,"tgt.class","cmp.class")]
    
    str(df.to.plot)
    
    #**console**#
    #print(paste("ncol(df.to.plot): ",ncol(df.to.plot)))
    par(mfrow=c(2,2))

    #need to consider whether Atgt is numerical or categorical
    #check this using the Data()[[2]]
    for(j in seq(1:2)){
      for(i in seq(1,2)){
        
        tgt.classes<-c("above/equal mean","below mean")
        
        if(Data2()[[2]][tgt.attr] == "Cate"){
          #grab the subset of data
          rows.to.plot<-intersect(
            which(df.to.plot[,"tgt.class"] == i),
            which(df.to.plot[,"cmp.class"] == j))
        }
        else if(Data2()[[2]][[tgt.attr]] == "Num"){
          rows.to.plot<-intersect(
            which(df.to.plot[,"tgt.class"] == tgt.classes[i]),
            which(df.to.plot[,"cmp.class"] == j))
        }
        plot.dat<-df.to.plot[rows.to.plot,mined.attr]
        str(plot.dat)
        
        if(Data()[[2]][mined.attr] == "Cate"){
          
          str(data.frame(table(plot.dat)))
          
          barplot(data.frame(table(plot.dat))[,2],
                  names.arg=data.frame(table(plot.dat))[,1],
                  las=2,
                  cex.names=0.9,
                  main=paste(Groupings()[[2]][i], Groupings()[[3]][j], sep="&"))
        }
        else if(Data()[[2]][mined.attr] == "Num")
          hist(plot.dat,main=paste(Groupings()[[2]][i], Groupings()[[3]][j], sep="&"),
               xlab=mined.attr)
      }
    }
  })
  
  #=============================================#
  #============7. Hypothesis mining=============#
  #=============================================#

  #*********************************************#
  #***************REACTIVE**********************#
  #*********************************************#
  
  Hypotheses<-reactive({
    
    tgt.attr<-input$targetAttr
    cmp.attr<-input$comparingAttr
    mined.attr<-minedAttributes()[[3]]
    
    df<-Data3()[[1]][,c(tgt.attr,cmp.attr,"cmp.class","tgt.class",c(mined.attr))]
    
    # function to compute proportions, given a 2x2 table
    compute.prop<-function(tab){
      p1<-tab[1,1]/sum(tab[1,1],tab[1,2])
      p2<-tab[2,1]/sum(tab[2,1],tab[2,2])
      return(c(p1,p2))
    }
    
    # function to compute support, given a 2x2 table
    compute.sup<-function(tab){
      c11<-tab[1,1]
      c12<-tab[1,2]
      c21<-tab[2,1]
      c22<-tab[2,2]
      n1<-c11+c12
      n2<-c21+c22
      return(c(c11=c11,c12=c12,c21=c21,c22=c22,n1=n1,n2=n2))
    }
    
    # function to compute indp factor, given a 2x2 table
    compute.indp<-function(tab){
     sum<-sum(tab)
     c11<-tab[1,1]
     c12<-tab[1,2]
     c21<-tab[2,1]
     c22<-tab[2,2]
     i1<-(c11/((c11+c12)*(c11+c21)))*sum
     i2<-(c21/((c11+c21)*(c21+c22)))*sum
     return(c(i1,i2))
    }
    
    # compute initial proportions
    cont.tab<-Table()[["cont.tab"]]
    initial.p1<-compute.prop(cont.tab)[1]
    initial.p2<-compute.prop(cont.tab)[2]
    # compute initial n
    initial.n1<-compute.sup(cont.tab)["n1"]
    initial.n2<-compute.sup(cont.tab)["n2"]
    # compute initial i
    initial.i1<-compute.indp(cont.tab)[1]
    initial.i2<-compute.indp(cont.tab)[2] # <--- speed can be improved here
    
    #**console**#
    #print(paste("initial.p1: ",initial.p1))
    #print(paste("initial.p2: ",initial.p2))
    #print(paste("initial.n1: ",initial.n1))
    #print(paste("initial.n2: ",initial.n2))
    
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
    
    # implemented this to facilitate the implementation of
    # considering hyptheses for pairwise context items
    # might be too many, ask prof wong first
    foo<-outer(prop.df.names,
               prop.df.names,
               paste,sep=" & ")
    foo<-foo[lower.tri(foo)]
    #print(foo)
    
    prop.df<-data.frame(cbind(rep(NA,length(prop.df.names)),
                              rep(NA,length(prop.df.names))),
                        row.names=prop.df.names)
    colnames(prop.df)<-c("p1prime","p2prime")
    
    i<-1
    for(an.item in rownames(prop.df)){
      Actx<-unlist(strsplit(an.item,"="))[1]
      vctx<-unlist(strsplit(an.item,"="))[2]
      
      rows.to.prop<-which(df[,Actx] == vctx)
      df.to.prop<-df[rows.to.prop,c("cmp.class","tgt.class")]
      tab.to.prop<-table(df.to.prop)
      
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
        
        prop.df$i1prime[i]<-compute.indp(tab.to.prop)[1]
        prop.df$i2prime[i]<-compute.indp(tab.to.prop)[2]
      }
      
      else{ # 040415: code can be further improved here
        # even if there arent 4 cells in the contingency table
        # it would still be interesting to take a look
        # at the counts, rather than setting them to NA
        prop.df$c11[i]<-prop.df$c12[i]<-NA
        prop.df$c21[i]<-prop.df$c22[i]<-NA
        prop.df$n1prime[i]<-prop.df$n2prime[i]<-NA
        prop.df$i1prime[i]<-prop.df$i2prime[i]<-NA
      }
      i<-i+1
    }
    
    # function to compute difflift
    compute.dl<-function(prop.vec){
      return((prop.vec[1]-prop.vec[2])/(initial.p1-initial.p2))
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
    
    # function to compute Independence Lift
    compute.il<-function(prop.vec){
      i1prime<-prop.vec[1]
      i2prime<-prop.vec[2]
      return((i1prime-i2prime)/(initial.i1-initial.i2))
    }
    
    prop.df$difflift<-apply(prop.df[,c("p1prime","p2prime")],
                            MARGIN=1,
                            FUN=compute.dl)
    
    prop.df$contri<-apply(prop.df[,c("p1prime","p2prime","n1prime","n2prime")],
                          MARGIN=1,
                          FUN=compute.contri)
    
    prop.df$SR<-sapply(prop.df[,"difflift"],
                       FUN=function(x){
                         if(is.na(x)) return(FALSE)
                         else if(x<0) return(TRUE)
                         else if(x>=0) return(FALSE)
                       })
    
    prop.df$indplift<-apply(prop.df[,c("i1prime","i2prime")],
                      MARGIN=1,
                      FUN=compute.il)
    
    # 210115: including new version of indplift: adjusted indplift
    tmp<-compute.sup(cont.tab)
    prob.T1<-(tmp[["c11"]]+tmp[["c21"]])/(tmp[["n1"]]+tmp[["n2"]])
    
    prop.df$adj.indplift<-with(prop.df,
                               difflift
                               *abs(1-(prob.T1/((c11+c21)/(n1prime+n2prime)))))
    #print(prop.df$indplift.adj)
    
    # now, append the chi-squared test stats and p-values
    
    for(i in seq(nrow(prop.df))){
      
      if(!is.na(prop.df$difflift[i])){
        Actx<-prop.df$Actx[i]
        vctx<-prop.df$vctx[i]
        
        # extract the subset of data
        rows<-which(df[,Actx] == vctx)
        tmp.df<-df[rows,c("cmp.class","tgt.class")]
        
        # run chisq.test on the contingency table
        tmp.tab<-table(tmp.df)
        test<-chisq.test(tmp.tab)
        
        # extract test stats and p-value and append to master data.frame
        prop.df$stats[i]<-test$statistic
        prop.df$pvalue[i]<-test$p.value
      }
      else{
        prop.df$stats[i]<-NA
        prop.df$pvalue[i]<-NA
      }
    }
    
    #consider minimum support 
    for(i in seq(nrow(prop.df))){
      sup<-prop.df[i,c("c11","c12","c21","c22")]
      
      if(any(sup < input$min.sup.cij) == TRUE ||
           any(is.na(sup)) == TRUE)
        prop.df$sufficient[i]<-FALSE
      else prop.df$sufficient[i]<-TRUE
    }
    
    #correct for multiple testing using Bonferroni correction
    prop.df$adj.pvalue<-p.adjust(prop.df$pvalue, method = "bonferroni")
    # sort
    # prop.df<-prop.df[with(prop.df,order(-sufficient,difflift,contri,adj.indplift,indplift,adj.pvalue)),]
    
    #print(prop.df$i1prime)
    #print(prop.df$i2prime)
    return(prop.df)
  })
  
  #*********************************************#
  #***************END REACTIVE******************#
  #*********************************************#
  
  # === Mined hypotheses === #
  
  # render the Hypotheses master data frame
  output$hypotheses<-renderTable({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      prop.df<-Hypotheses()
      prop.df<-subset(prop.df,select=c(Actx,vctx,
                                       sufficient,SR,
                                       c11,c12,c21,c22,
                                       n1prime,n2prime,
                                       p1prime,p2prime,
                                       i1prime,i2prime,
                                       difflift,contri,indplift,adj.indplift,
                                       stats,pvalue,adj.pvalue))
    }
  },digits=3)
  
  # === Select context item === #
  
  # summary of difflift and contribution for attributes as a whole
  output$analyse.summary<-renderTable({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      prop.df<-Hypotheses()
      actx<-unique(prop.df$Actx)
      summary.df<-data.frame(difflift=numeric(),
                             contri=numeric())
      SP.vec<-NULL
      for(a.ctx.attr in actx){
        # extract the difflift's and contributions
        all.dl<-prop.df$difflift[which(prop.df$Actx == a.ctx.attr)]
        all.contri<-prop.df$contri[which(prop.df$Actx == a.ctx.attr)]
        
        # find the mean for each Actx
        dl<-mean(abs(all.dl),na.rm=TRUE)
        contri<-mean(abs(all.contri),na.rm=TRUE)
        
        # remove NAs, look for Simpson's Paradox
        all.dl[is.na(all.dl)]<-0
        if(all(all.dl < 0)) SP.vec<-c(SP.vec,TRUE)
        else SP.vec<-c(SP.vec,FALSE)
        
        if(is.nan(dl)) SP.vec[length(SP.vec)]<-FALSE
        
        summary.df<-rbind(summary.df,c(dl,contri))
      }
      summary.df<-cbind(summary.df,SP.vec)
      rownames(summary.df)<-actx
      colnames(summary.df)<-c("Mean difference lift","Mean contribution","Simpson's paradox")
      return(summary.df)
    }
  })
  
  output$analyse.ctrl<-renderUI({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      selectizeInput("analyse.which.item","Select context item to analyse",rownames(Hypotheses()))
    }
  }) # return: input$analyse.which.item
  output$analyse.sort.ctrl.one<-renderUI({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      selectizeInput("analyse.sort.one","Sort first by?",
                     c("sufficient","SR","difflift","contri","indplift","adj.indplift","pvalue","adj.pvalue"))
    }
  }) # return: input$analyse.sort.one
  output$analyse.sort.ctrl.two<-renderUI({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      selectizeInput("analyse.sort.two","Then by?",
                     c("sufficient","SR","difflift","contri","indplift","adj.indplift","pvalue","adj.pvalue"),
                     selected="indplift")
    }
  }) # return: input$analyse.sort.two
  
  # render the Hypotheses master data frame, sorted according to user
  output$analyse.hypothesis<-renderTable({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      prop.df<-subset(Hypotheses(),select=c(sufficient,SR,difflift,contri,indplift,adj.indplift,pvalue,adj.pvalue))
      
      sort.first.by<-which(colnames(prop.df) == input$analyse.sort.one)
      then.by<-which(colnames(prop.df) == input$analyse.sort.two)
      
      # 1: sufficient   (sort *descendingly*)
      # 2: SR           (sort *descendingly*)
      # 3: difflift     (sort ascendingly)
      # 4; contri       (sort ascendingly)
      # 5: indplift     (sort ascendingly)
      # 6: adj.indplift (sort ascendingly)
      # 7: pvalue       (sort ascendingly)
      # 8: adj.pvalue   (sort ascendingly)
      
      # first case: {1 or 2, 1 or 2}
      if((sort.first.by == 1 || sort.first.by == 2) && (then.by == 1 || then.by == 2))
        prop.df<-prop.df[order(-prop.df[,sort.first.by],-prop.df[,then.by]),]
      
      # second case: {1 or 2, not 1 and 2}
      else if((sort.first.by == 1 || sort.first.by == 2) && (then.by != 1 && then.by != 2))
          prop.df<-prop.df[order(-prop.df[,sort.first.by],prop.df[,then.by]),]
      
      # third case: {not 1 and 2, 1 or 2}
      else if((sort.first.by != 1 && sort.first.by != 2) && (then.by == 1 || then.by == 2))
          prop.df<-prop.df[order(prop.df[,sort.first.by],-prop.df[,then.by]),]
      
      # at this point, both must be neither 1 or 2
      else prop.df<-prop.df[order(prop.df[,sort.first.by],prop.df[,then.by]),]
      
      prop.df<-subset(prop.df,
                      select=c(sufficient,SR,difflift,contri,indplift,adj.indplift,pvalue,adj.pvalue))
      
      colnames(prop.df)<-c("Sufficient","Simpson's Reversal","Difference lift",
                           "Contribution","Independence lift","Adjusted independence lift","p-value","Adjusted p-value")
      
      return(prop.df)
    }
  })
  
  # === Hypothesis analysis === #
  
  # display initial hypothesis and test
  output$analyse.contTable<-renderTable({
    tab<-Table()[[1]]
    
    if(Table()[["tab.type"]] == "Contingency"){
      append.col<-c((tab[1,1]+tab[1,2]),
                    (tab[2,1]+tab[2,2]))
      append.row<-c((tab[1,1]+tab[2,1]),
                    (tab[1,2]+tab[2,2]),
                    sum(tab))
      append.col<-round(append.col,2)
      append.row<-round(append.row,2)
      
      # include cell proportions in rendered table
      cell.proportions<-c(tab[1,1]/(tab[1,1]+tab[1,2]),
                          tab[1,2]/(tab[1,1]+tab[1,2]),
                          tab[2,1]/(tab[2,1]+tab[2,2]),
                          tab[2,2]/(tab[2,1]+tab[2,2]))
      cell.proportions<-round(cell.proportions,2)
      
      tab[1,1]<-paste(tab[1,1]," (",cell.proportions[1],")",sep="")
      tab[1,2]<-paste(tab[1,2]," (",cell.proportions[2],")",sep="")
      tab[2,1]<-paste(tab[2,1]," (",cell.proportions[3],")",sep="")
      tab[2,2]<-paste(tab[2,2]," (",cell.proportions[4],")",sep="")
      
      tab<-cbind(tab,append.col)
      tab<-rbind(tab,append.row)
      
      colnames(tab)[ncol(tab)]<-rownames(tab)[nrow(tab)]<-"Total"
      
      return(tab)
    }
    else if(Table()[["tab.type"]] == "Comparison"){
      tab<-Table()[["cont.tab"]]
      append.col<-c((tab[1,1]+tab[1,2]),
                    (tab[2,1]+tab[2,2]))
      append.row<-c((tab[1,1]+tab[2,1]),
                    (tab[1,2]+tab[2,2]),
                    sum(tab))
      append.col<-round(append.col,2)
      append.row<-round(append.row,2)
      
      # include cell proportions in rendered table
      cell.proportions<-c(tab[1,1]/(tab[1,1]+tab[1,2]),
                          tab[1,2]/(tab[1,1]+tab[1,2]),
                          tab[2,1]/(tab[2,1]+tab[2,2]),
                          tab[2,2]/(tab[2,1]+tab[2,2]))
      cell.proportions<-round(cell.proportions,2)
      
      tab[1,1]<-paste(tab[1,1]," (",cell.proportions[1],")",sep="")
      tab[1,2]<-paste(tab[1,2]," (",cell.proportions[2],")",sep="")
      tab[2,1]<-paste(tab[2,1]," (",cell.proportions[3],")",sep="")
      tab[2,2]<-paste(tab[2,2]," (",cell.proportions[4],")",sep="")
      
      tab<-cbind(tab,append.col)
      tab<-rbind(tab,append.row)
      colnames(tab)[ncol(tab)]<-rownames(tab)[nrow(tab)]<-"Total"
      return(tab)
    }
  })
  output$analyse.initialTest<-renderTable({
    # check the type of table
    if(Groupings()[[1]] == "Cate"){
      test<-chisq.test(Table()[[1]]) #chisq.test() works on the table itself
      stats<-test$statistic
      pvalue<-test$p.value
      method<-test$method
      
      returnMe<-as.data.frame(c(as.character(method),
                                as.character(round(stats,3)),
                                as.character(pvalue)))
      rownames(returnMe)<-c("Method","Test statistic","p-value")
      colnames(returnMe)<-"Initial chi-squared test on contingency table"
      returnMe
    }
    else if(Groupings()[[1]] == "Num"){
      test<-chisq.test(Table()[["cont.tab"]]) #chisq.test() works on the table itself
      stats<-test$statistic
      pvalue<-test$p.value
      method<-test$method
      
      returnMe<-as.data.frame(c(as.character(method),
                                as.character(round(stats,3)),
                                as.character(pvalue)))
      rownames(returnMe)<-c("Method","Test statistic","p-value")
      colnames(returnMe)<-"Chi-squared test on contingency table"
      returnMe
    }
  })
  output$analyse.hypothesis.statement.initial<-renderText({
    if(is.null(input$targetAttr) || is.null(input$comparingAttr)) return("")
    
    tgt.attr<-input$targetAttr
    cmp.attr<-input$comparingAttr
    
    tgt.class1<-input$whichtgtclassesA # <--- could be NULL if Atgt is num
    tgt.class2<-input$whichtgtclassesB # <--- could be NULL
    cmp.class1<-input$whichcmpclassesX
    cmp.class2<-input$whichcmpclassesY
    
    ctx.attr    <-input$ctxAttr
    ctx.items   <-input$ctxItems # in the format of Actx = vctx
    
    ctx.items.text<-paste(ctx.items, collapse=" & ")
    tgt.class1.text<-paste(tgt.class1,collapse=" & ")
    tgt.class2.text<-paste(tgt.class2,collapse=" & ")
    cmp.class1.text<-paste(cmp.class1,collapse=" & ")
    cmp.class2.text<-paste(cmp.class2,collapse=" & ")
    
    if(Groupings()[[1]] == "Cate")
      statement<-paste("In the context of {",
                       ctx.items.text,
                       "}, is there a difference in ",
                       toupper(tgt.attr),
                       " between {",
                       tgt.class1.text,
                       "} vs. {",
                       tgt.class2.text,
                       "} when comparing the samples on ",
                       toupper(cmp.attr),
                       " between {",
                       cmp.class1.text,
                       "} vs. {",
                       cmp.class2.text,
                       "}?",
                       sep="")
    else if(Groupings()[[1]] == "Num")
      statement<-paste("In the context of {",
                       ctx.items.text,
                       "}, is there a difference in ",
                       toupper(tgt.attr),
                       " when comparing the samples on ",
                       toupper(cmp.attr),
                       " between {",
                       cmp.class1.text,
                       "} vs. {",
                       cmp.class2.text,
                       "}?",
                       sep="")
    return(statement)
  })
  
  # display mined hypothesis and test based on selected item
  output$analyse.cont.tab<-renderTable({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      item<-input$analyse.which.item
      Actx<-unlist(strsplit(item,"="))[1]
      vctx<-unlist(strsplit(item,"="))[2]
      
      df<-Data3()[[1]][,c("tgt.class","cmp.class",Actx)]
      
      rows.to.prop<-which(df[,Actx] == vctx)
      df.to.prop<-df[rows.to.prop,c("cmp.class","tgt.class")]
      tab<-table(df.to.prop)
      if(nrow(tab)*ncol(tab) == 4){
        rownames(tab)<-Groupings()[[3]]
        colnames(tab)<-Groupings()[[2]]
        
        append.col<-c((tab[1,1]+tab[1,2]),
                      (tab[2,1]+tab[2,2]))
        append.row<-c((tab[1,1]+tab[2,1]),
                      (tab[1,2]+tab[2,2]),
                      sum(tab))
        
        append.col<-round(append.col,2)
        append.row<-round(append.row,2)
        
        # include cell proportions in rendered table
        cell.proportions<-c(tab[1,1]/(tab[1,1]+tab[1,2]),
                            tab[1,2]/(tab[1,1]+tab[1,2]),
                            tab[2,1]/(tab[2,1]+tab[2,2]),
                            tab[2,2]/(tab[2,1]+tab[2,2]))
        cell.proportions<-round(cell.proportions,2)
        
        tab[1,1]<-paste(tab[1,1]," (",cell.proportions[1],")",sep="")
        tab[1,2]<-paste(tab[1,2]," (",cell.proportions[2],")",sep="")
        tab[2,1]<-paste(tab[2,1]," (",cell.proportions[3],")",sep="")
        tab[2,2]<-paste(tab[2,2]," (",cell.proportions[4],")",sep="")
        
        tab<-cbind(tab,append.col)
        tab<-rbind(tab,append.row)
        colnames(tab)[ncol(tab)]<-rownames(tab)[nrow(tab)]<-"Total"
        return(tab)
      }
    }
  })
  output$analyse.test<-renderTable({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      item<-input$analyse.which.item
      Actx<-unlist(strsplit(item,"="))[1]
      vctx<-unlist(strsplit(item,"="))[2]
      
      df<-Data3()[[1]][,c("tgt.class","cmp.class",Actx)]
      
      rows.to.prop<-which(df[,Actx] == vctx)
      df.to.prop<-df[rows.to.prop,c("cmp.class","tgt.class")]
      tab<-table(df.to.prop)
      if(nrow(tab)*ncol(tab) == 4){
        test<-chisq.test(tab)
        stats<-test$statistic
        pvalue<-test$p.value
        method<-test$method
        
        returnMe<-as.data.frame(c(as.character(method),
                                  as.character(round(stats,3)),
                                  as.character(pvalue)))
        
        rownames(returnMe)<-c("Method","Test statistic","p-value")
        colnames(returnMe)<-paste("Chi-squared test on mined hypothesis: ",item,sep="")
        return(returnMe)
      }
      else{
        tab<-data.frame("Insufficient support for hypothesis")
        colnames(tab)<-""
        return(tab)
      }
    }
  })
  output$analyse.hypothesis.statement<-renderText({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      if(is.null(input$targetAttr) || is.null(input$comparingAttr)) return("")
      
      tgt.attr<-input$targetAttr
      cmp.attr<-input$comparingAttr
      
      tgt.class1<-input$whichtgtclassesA # <--- could be NULL if Atgt is num
      tgt.class2<-input$whichtgtclassesB # <--- could be NULL
      cmp.class1<-input$whichcmpclassesX
      cmp.class2<-input$whichcmpclassesY
      
      ctx.attr    <-input$ctxAttr
      ctx.items   <-input$ctxItems # in the format of Actx = vctx
      
      ctx.items.text<-paste(ctx.items, collapse=" & ")
      tgt.class1.text<-paste(tgt.class1,collapse=" & ")
      tgt.class2.text<-paste(tgt.class2,collapse=" & ")
      cmp.class1.text<-paste(cmp.class1,collapse=" & ")
      cmp.class2.text<-paste(cmp.class2,collapse=" & ")
      
      # add in the ctx item to analyse
      if(ctx.items.text == "") ctx.items.text<-paste(input$analyse.which.item, collapse=" & ")
      else ctx.items.text<-paste(ctx.items.text,input$analyse.which.item, sep=" & ")
      
      if(Groupings()[[1]] == "Cate")
        statement<-paste("In the context of {",
                         ctx.items.text,
                         "}, is there a difference in ",
                         toupper(tgt.attr),
                         " between {",
                         tgt.class1.text,
                         "} vs. {",
                         tgt.class2.text,
                         "} when comparing the samples on ",
                         toupper(cmp.attr),
                         " between {",
                         cmp.class1.text,
                         "} vs. {",
                         cmp.class2.text,
                         "}?",
                         sep="")
      else if(Groupings()[[1]] == "Num")
        statement<-paste("In the context of {",
                         ctx.items.text,
                         "}, is there a difference in ",
                         toupper(tgt.attr),
                         " when comparing the samples on ",
                         toupper(cmp.attr),
                         " between {",
                         cmp.class1.text,
                         "} vs. {",
                         cmp.class2.text,
                         "}",
                         sep="")
      return(statement)
    }
  })
  
  # chi-sq top contribution
  output$text.analyse.flat.table<-renderText({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      if((Test()[["test.type"]] == "t.test" && Test()[["second.test.type"]] == "collapsed.chi.sq")
         || Test()[["test.type"]] == "collapsed.chi.sq"){
        return("Flat contingency table of mined hypothesis:")
      }
    }
  })
  output$analyse.flat.table<-renderTable({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      if((Test()[["test.type"]] == "t.test" && Test()[["second.test.type"]] == "collapsed.chi.sq")
         || Test()[["test.type"]] == "collapsed.chi.sq"){
        
        item<-input$analyse.which.item
        Actx<-unlist(strsplit(item,"="))[1]
        vctx<-unlist(strsplit(item,"="))[2]
        
        df<-Data3()[[1]][,c("tgt.class",input$comparingAttr,Actx)]
        
        rows.to.prop<-which(df[,Actx] == vctx)
        df.to.prop<-df[rows.to.prop,c(input$comparingAttr,"tgt.class")]
        tab<-table(df.to.prop)
        if(nrow(tab)*ncol(tab) > 4){
          colnames(tab)<-Groupings()[["Atgt.names"]]
          return(tab)
        }
      }
    }
  })
  #===#
  output$text.analyse.flat.chi.sq<-renderText({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      if((Test()[["test.type"]] == "t.test" && Test()[["second.test.type"]] == "collapsed.chi.sq")
         || Test()[["test.type"]] == "collapsed.chi.sq"){
        return("Chi-squared test on flat contingency table:")
      }
    }
  })
  output$analyse.flat.chi.sq<-renderTable({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      if((Test()[["test.type"]] == "t.test" && Test()[["second.test.type"]] == "collapsed.chi.sq")
         || Test()[["test.type"]] == "collapsed.chi.sq"){
        
        item<-input$analyse.which.item
        Actx<-unlist(strsplit(item,"="))[1]
        vctx<-unlist(strsplit(item,"="))[2]
        
        df<-Data3()[[1]][,c("tgt.class",input$comparingAttr,Actx)]
        
        rows.to.prop<-which(df[,Actx] == vctx)
        df.to.prop<-df[rows.to.prop,c(input$comparingAttr,"tgt.class")]
        tab<-table(df.to.prop)
        
        if(nrow(tab)*ncol(tab) > 4){
          # flat chi-sq
          test<-chisq.test(tab)
          stats<-test$statistic
          pvalue<-test$p.value
          method<-test$method
          
          returnMe<-as.data.frame(c(as.character(method),
                                    as.character(round(stats,3)),
                                    as.character(pvalue)))
          rownames(returnMe)<-c("Method","Test statistic","p-value")
          colnames(returnMe)<-paste("Flat chi-squared test on mined hypothesis: ",item,sep="")
          returnMe
        }
      }
    }
  })
  #===#
  output$text.analyse.chi.sq.top.cont<-renderText({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      if((Test()[["test.type"]] == "t.test" && Test()[["second.test.type"]] == "collapsed.chi.sq")
         || Test()[["test.type"]] == "collapsed.chi.sq"){
        return("Chi-squared contributions:")
      }
    }
  })
  output$analyse.chi.sq.top.cont<-renderTable({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      if((Test()[["test.type"]] == "t.test" && Test()[["second.test.type"]] == "collapsed.chi.sq")
         || Test()[["test.type"]] == "collapsed.chi.sq"){
        
        item<-input$analyse.which.item
        Actx<-unlist(strsplit(item,"="))[1]
        vctx<-unlist(strsplit(item,"="))[2]
        
        df<-Data3()[[1]][,c("tgt.class",input$comparingAttr,Actx)]
        
        rows.to.prop<-which(df[,Actx] == vctx)
        df.to.prop<-df[rows.to.prop,c(input$comparingAttr,"tgt.class")]
        tab<-table(df.to.prop)
        if(nrow(tab)*ncol(tab) > 4){
          test<-chisq.test(tab)
          o<-test$observed
          e<-test$expected
          #vtgt<-colnames(o)[which(colnames(o) == "1")] # vtgt is tgt.class == 1
          #cmp.classes<-rownames(e) # <--- want to compute top contributor for Acmp,
          # for vtgt only
          chisq.contri<-cbind(o[,1],
                              e[,1],
                              ((((o-e)^2)/e)[,1])/test$statistic * 100)
          colnames(chisq.contri)<-c("Observed",
                                    "Expected",
                                    "Chi-squared contributions (%)")
          return(chisq.contri)
        }
      }
    }
  })
  
  # === Hypothesis mining metrics === #
  
  output$analyse.plot.metric.ctrl.one<-renderUI({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      selectizeInput("plot.what.metric.one",
                     "Select a hypothesis mining metric to plot",
                     c("difflift","contri","indplift","adj.indplift","stats","pvalue","adj.pvalue"))
    }
  }) # return: input$plot.what.metric.one
  output$analyse.plot.metric.ctrl.two<-renderUI({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      selectizeInput("plot.what.metric.two",
                     "Select another",
                     c("difflift","contri","indplift","adj.indplift","stats","pvalue","adj.pvalue"))
    }
  }) # return: input$plot.what.metric.two
  output$analyse.metric.plot<-renderPlot({
    if(!is.null(minedAttributes()[["mined.attr"]])){
      prop.df<-Hypotheses()
      plot(prop.df[,input$plot.what.metric.one]~prop.df[,input$plot.what.metric.two],
           ylab=input$plot.what.metric.one,xlab=input$plot.what.metric.two)
      abline(h=0)
      abline(v=0)
      
      if(input$plot.what.metric.one == "pvalue" || input$plot.what.metric.one == "adj.pvalue")
        abline(h=0.05)
      if(input$plot.what.metric.two == "pvalue" || input$plot.what.metric.two == "adj.pvalue")
        abline(v=0.05)
    }
  })
  
  #=============================================#
  #============8. Session log===================#
  #=============================================#
  
  #*********************************************#
  #***************REACTIVE**********************#
  #*********************************************#
  
  Settings<-reactive({
    # general
    log.timestamp<-date()
    
    # data
    log.filename<-as.character(input$datFile[[1]])
    log.header<-input$datHeader
    log.sep<-input$datSep
    log.quote<-input$datQuote
    log.maxClass<-input$maxClass
    
    # initial test
    if(Groupings()[["Atgt.type"]] == "Cate"){
      log.Atgt.type<-"Categorical"
      log.Atgt<-paste("{",
                      input$targetAttr,
                      "=",
                      Groupings()[["Atgt.names"]][1],
                      "} vs. {",
                      input$targetAttr,
                      "=",
                      Groupings()[["Atgt.names"]][2],
                      "}",sep="")
    }
    else if(Groupings()[["Atgt.type"]] == "Num"){
      log.Atgt.type<-"Numerical"
      log.Atgt<-input$targetAttr
    }
    log.Acmp<-paste("{",
                    input$comparingAttr,
                    "=",
                    Groupings()[["Acmp.names"]][1],
                    "} vs. {",
                    input$comparingAttr,
                    "=",
                    Groupings()[["Acmp.names"]][2],
                    "}",sep="")
    if(!is.null(input$ctxItems)) log.Actx<-paste(input$ctxItems,collapse=", ")
    else log.Actx<-""
    
    # test diagnostics
    log.p.significant<-input$p.significant
    
    # context mining
    log.acc.rf.default<-input$acc.rf.default
    log.top.k<-input$top.k
    log.class.ratio<-input$class.ratio
    log.mined.attr<-paste(minedAttributes()[[3]],collapse=", ")
    
    # hypothesis mining
    log.min.sup.cij<-input$min.sup.cij
    
    col1<-c("Session",
            rep("Data",5),
            rep("Initial test",4),
            rep("Test diagnostics",1),
            rep("Context mining",4),
            rep("Hypothesis mining",1))
    col2<-c("Timestamp",
            "Filename",
            "Header has attribute names",
            "Separator",
            "Quotes",
            "Maximum number of classes in categorical attributes",
            "Target attribute",
            "Target attribute type",
            "Comparing attribute",
            "Context items",
            "p-value threshold",
            "Random forest accuracy threshold",
            "Number of context attributes to shortlist",
            "Class ratio threshold for class-imbalance learning",
            "Mined attributes",
            "Min support for each cell")
    col3<-c(log.timestamp,
            log.filename,
            log.header,
            log.sep,
            log.quote,
            log.maxClass,
            log.Atgt,
            log.Atgt.type,
            log.Acmp,
            log.Actx,
            log.p.significant,
            log.acc.rf.default,
            log.top.k,
            paste("1 : ",log.class.ratio),
            log.mined.attr,
            log.min.sup.cij)
    
    log<-data.frame(cbind(col1,col2,col3))
    colnames(log)<-c("","","Settings")
    rownames(log)<-NULL
    return(log)
  })
  
  #*********************************************#
  #***************END REACTIVE******************#
  #*********************************************#
  
  output$session.log<-renderTable({
    return(Settings())
  })
  
  output$log.download<-downloadHandler(
    filename = function(){paste("Redhyte_session_log_", date(), '.csv',sep='')},
    content = function(file){
      write.table(Settings(),file,quote=FALSE,row.names=FALSE,na="NA",sep=" :: ",col.names=FALSE)
    }
  )
  
  #=============================================#
  #============9. About Redhyte=================#
  #=============================================#
  
  #*********************************************#
  #***************REACTIVE**********************#
  #*********************************************#
  
  output$about.text<-reactive({
    htmlCode<-paste("
      <h4>What?</h4>
      <h5><strong>
        Redhyte is short for <q>An Interactive Platform for <em>R</em>apid <em>E</em>xploration of <em>D</em>ata and <em>Hy</em>pothesis <em>Te</em>sting</q>.
        Fundamentally, Redhyte is a hypothesis mining system where users start off with an initial domain knowledge-driven question,
        which Redhyte uses to mine for relevant and interesting hypotheses that deepens the user's understanding of his or her data.
        In addition, Redhyte provides basic functionalities for data visualizations, checking of parametric test assumptions and data manipulation.
      </strong></h5>

      <h4>How?</h4>
      <h5><strong>
        As the term suggests, hypothesis mining is concerned with the search of interesting hypotheses from a given dataset. 
        In order to do so, Redhyte puts together the user's domain knowledge, the well-established framework of statistical hypothesis testing,
        and classification techniques from data mining.
        To evaluate the interestingness of mined hypotheses, Redhyte utilises a set of hypothesis mining metrics 
        so as to divert the user's attention to the most interesting collection of hypotheses mined by Redhyte.
      </strong></h5>
      <h5><strong>
        Redhyte was developed using and powered by the statistical programming language 
        <a href=http://www.r-project.org/ target=_blank>R</a>, with the actualization of this user interface made possible
        by the R <a href=http://shiny.rstudio.com/ target=_blank>shiny</a> package.
      </strong></h5>

      <h4>Why?</h4>
      <h5><strong>
        Hypothesis testing is a technique used by many non-statistician data analysts -  
        the idea of comparing lung cancer incidence between two subpopulations, say smokers and non-smokers, is intuitive and easy to understand. 
        It is also easy to search through a small dataset of, say, 10 variables (e.g. in an epidemiological study) 
        and identify any existing statistically and practically significant phenomena and trends. 
        However in the current Big Data era, the search for statistical and practical significance becomes a non-trivial task - 
        formulating and testing a small hypothesis in a large dataset is both wasteful and flawed.
      </strong></h5>
      <h5><strong>
        Using data mining techniques, Redhyte aims to regard hypothesis testing in a more comprehensive manner. 
        The objective of Redhyte is to identify, based on the initial domain knowledge-driven question that the user had in mind, 
        practical and insightful hypotheses.
      </strong></h5>

      <h4>Who?</h4>
      <h5><strong>
          Redhyte was developed by 
          <a href=http://sg.linkedin.com/in/tohweizhong target=_blank>Wei Zhong Toh</a>, 
          <a href=http://www.comp.nus.edu.sg/~wongls/ target=_blank>Limsoon Wong</a>, and 
          <a href=http://www.stat.nus.edu.sg/~stackp/ target=_blank>Kwok Pui Choi</a> at the 
          <a href=http://www.nus.edu.sg/ target=_blank>National University of Singapore</a>, 
          <a href=http://www.science.nus.edu.sg target=_blank>Faculty of Science</a> and 
          <a href=http://www.comp.nus.edu.sg/ target=_blank>School of Computing</a>, 
          and is part of Toh's undergraduate Honours work.
      </strong></h5>

      <h4>Where?</h4>
      <font size=3><ul>
        <li><a href=https://tohweizhong.shinyapps.io/redhyte/ target=_blank>Redhyte web app</a></li>
        <li><a href=https://github.com/tohweizhong/redhyte target=_blank>GitHub repo</a></li>

        <li>Example datasets to try Redhyte out with:</li>
        <ul>
          <li><a href=https://dl.dropboxusercontent.com/u/36842028/linkouts/datasets/ucbMelted.csv target=_blank>UC Berkeley Admission Bias dataset</a>, distributed with R
              (<a href=http://stat.ethz.ch/R-manual/R-devel/library/datasets/html/UCBAdmissions.html target=_blank>Description of dataset</a>, some prior data cleaning was done)</li>          
          <li><a href=https://dl.dropboxusercontent.com/u/36842028/linkouts/datasets/adult.txt target=_blank>US Census dataset by UCI Machine Learning Repo</a>
              (<a href=https://archive.ics.uci.edu/ml/datasets/Adult target=_blank>Description of dataset</a>)</li>
          <li><a href=https://dl.dropboxusercontent.com/u/36842028/linkouts/datasets/mushroom_expanded.txt target=_blank>Mushroom dataset by UCI Machine Learning Repo</a>
              (<a href=https://archive.ics.uci.edu/ml/datasets/mushroom target=_blank>Description of dataset</a>)</li>
          <li><a href=https://dl.dropboxusercontent.com/u/36842028/linkouts/datasets/arrhythmia_nameless.txt target=_blank>Arrhythmia dataset by UCI Machine Learning Repo</a>
              (<a href=https://archive.ics.uci.edu/ml/datasets/Arrhythmia target=_blank>Description of dataset</a>, some prior data cleaning was done)</li>
          <li><a href=https://dl.dropboxusercontent.com/u/36842028/linkouts/datasets/titanicMelted.csv target=_blank>Titanic dataset</a>, distributed with R
              (<a href=http://stat.ethz.ch/R-manual/R-devel/library/datasets/html/Titanic.html target=_blank>Description of dataset</a>, some prior data cleaning was done)</li>
        </ul>
      </ul></font>

      "
      ,sep="")
  })
  
  #*********************************************#
  #***************END REACTIVE******************#
  #*********************************************#
  
  # observer to update all the navlistPanel selection
  observe({
    print(input$theTabs)
    if(input$theTabs != "2. Data viz")
      updateTabsetPanel(session,"viz",selected="Select attributes")
    if(input$theTabs != "3. Initial test")
      updateTabsetPanel(session,"initial",selected="Target attribute")
    if(input$theTabs != "5. Test diagnostics")
      updateTabsetPanel(session,"diag",selected="Continuous target attribute")
    if(input$theTabs != "6. Context mining")
      updateTabsetPanel(session,"ctx",selected="Attributes to exclude")
    if(input$theTabs != "7. Hypothesis mining")
      updateTabsetPanel(session,"hypo",selected="Select context item")
  })
}) #end shinyServer