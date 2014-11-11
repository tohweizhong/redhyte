require(randomForest)
source("related_codes/settings.R")

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
                 #row.names=ifelse(input$datRownames,1,NA),
                 sep=input$datSep,
                 quote=input$datQuote,
                 stringsAsFactors=F)
    #this part is ok, based on the response I got from stackoverflow
    #the NULL that appears after
    
    #print(str(df))
    
    #is apparently the property of print.default()

    #checking the variable type of the attributes: continuous or categorical
    #and number of classes for cate. attr.
    
    if(input$datTranspose == TRUE) df<-t(df)
    
    typ<-NULL
    numCl<-NULL
    for(i in seq(ncol(df))){
      if(is.numeric(df[,i])){ # <--- POSSIBLE SOURCE OF BUG, BECAUSE THIS IS A LITTLE HARD-CODING HERE
        if(length(unique(df[,i]))>5){
          typ<-c(typ,"Cont")
          numCl<-c(numCl,NA)
        }
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
    if(is.null(Data()[1])) return(NULL)
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
    if(Data()[[2]][input$viz.which.attr2]=="Cont"){
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
    if(Data()[[2]][input$viz.which.attr1]=="Cont")
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
    if(Data()[[2]][input$viz.which.attr2]=="Cont")
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
  
  # plotting boxplots
  output$viz.boxplot1<-renderPlot({
    if(Data()[[2]][input$viz.which.attr1]=="Cont")
      boxplot(Data()[[1]][,input$viz.which.attr1])
  }) # not used
  output$viz.boxplot2<-renderPlot({
    if(Data()[[2]][input$viz.which.attr2]=="Cont")
      boxplot(Data()[[1]][,input$viz.which.attr2])
  }) # not used
  
  # plotting scatterplot, boxplot, or spineplot
  output$viz.scatterplot<-renderPlot({
    type1<-Data()[[2]][input$viz.which.attr1]
    type2<-Data()[[2]][input$viz.which.attr2]
    
    if(type1 == "Cont" && type2 == "Cont")
    plot(Data()[[1]][,input$viz.which.attr2]~
           Data()[[1]][,input$viz.which.attr1],
         xlab=input$viz.which.attr1,
         ylab=input$viz.which.attr2)
    else if(type1 == "Cont" && type2 =="Cate")
      boxplot(Data()[[1]][,input$viz.which.attr1]~
                Data()[[1]][,input$viz.which.attr2],
              las=2,
              xlab=input$viz.which.attr2,
              ylab=input$viz.which.attr1)
    else if(type1 == "Cate" && type2 == "Cont")
      boxplot(Data()[[1]][,input$viz.which.attr2]~
                Data()[[1]][,input$viz.which.attr1],
              las=2,
              xlab=input$viz.which.attr1,
              ylab=input$viz.which.attr2)
    else{
      par(las=1)
      spineplot(as.factor(Data()[[1]][,input$viz.which.attr1])~
                  as.factor(Data()[[1]][,input$viz.which.attr2]),
                xlab=input$viz.which.attr2,
                ylab=input$viz.which.attr1)
    }
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
    which.are.cate<-which(Data()[[2]] == "Cate")
    
    selectizeInput("comparingAttr",
                   "Indicate comparing attribute (Must be categorical)",
                   colnames(Data()[[1]])[which.are.cate])
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
  
#   Data1.1<-reactive({
#     
#     cur.df<-Data()[[1]]
#     
#     rows.tgt<-NULL
#     print(input$targetAttr)
#     if(!is.null(input$targetAttr)){
#       if(!is.null(input$whichtgtclassesA))
#         rows.tgt<-c(rows.tgt,which(cur.df[,input$targetAttr] %in% input$whichtgtclassesA))
#       if(!is.null(input$whichtgtclassesB))
#         rows.tgt<-c(rows.tgt,which(cur.df[,input$targetAttr] %in% input$whichtgtclassesB))
#     }
#     rows.cmp<-NULL
#     if(!is.null(input$comparingAttr)){
#       if(!is.null(input$whichcmpclassesX))
#         rows.cmp<-c(rows.cmp,which(cur.df[,input$comparingAttr] %in% input$whichcmpclassesX))
#       if(!is.null(input$whichcmpclassesY))
#         rows.cmp<-c(rows.cmp,which(cur.df[,input$comparingAttr] %in% input$whichcmpclassesY))
#     }
#     rows.ctx<-NULL
#     if(!is.null(input$ctxItems)){
#       for(an.item in input$ctxItems){
#         
#         Actx<-unlist(strsplit(an.item,"="))[1]
#         vctx<-unlist(strsplit(an.item,"="))[2]
#         rows.ctx<-c(rows.ctx,which(cur.df[,Actx] %in% vctx))
#       }
#     }
#     rows<-unique(c(rows.tgt,rows.cmp,rows.ctx))
#     
#     str(rows.tgt)
#     str(rows.cmp)
#     str(rows.ctx)
#     str(rows)
#     
#     if(is.null(rows)) return(cur.df)
#     else if(!is.null(rows)) return(cur.df[rows,])
#   })
  
  #target and comparing control
  output$test.tgt.class.ctrl1<-renderUI({
    if(Data()[[2]][input$targetAttr] == "Cate"){
      .choices<-unique(Data()[[1]][,input$targetAttr])
      .names<-NULL
      for(c in .choices){
        sup<-length(which(Data()[[1]][,input$targetAttr] == c))
        .names<-c(.names,paste(c," (",sup,")",sep=""))
      }
      names(.choices)<-.names
      
      checkboxGroupInput("whichtgtclassesA",
                         "Indicate which target attribute classes to form group A",
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
                         "Indicate which target attribute classes to form group B",
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
                         "Indicate which comparing attribute class to form group X",
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
                         "Indicate which comparing attribute classes to form group Y",
                         choices=c(sort(.choices)))
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
    
  }) #return: input$ctxItems

  #***************REACTIVE**********************#
  
#   Cinitial<-reactive({
#     # retrieve attributes
#     tgt.attr<-input$targetAttr
#     cmp.attr<-input$comparingAttr
#     ctx.attr<-input$ctxAttr
#     
#     # retrieve items
#     tgt.class1<-input$whichtgtclassesA
#     tgt.class2<-input$whichtgtclassesB
#     cmp.class1<-input$whichcmpclassesX
#     cmp.class2<-input$whichcmpclassesY
#     ctx.items <-input$ctxItems
#     
#     # remove support at end of string
#     remove.sup<-function(s){return(unlist(strsplit(s," [()]"))[1])}
#     tgt.class1<-sapply(tgt.class1,FUN=remove.sup)
#     tgt.class2<-sapply(tgt.class2,FUN=remove.sup)
#     cmp.class1<-sapply(cmp.class1,FUN=remove.sup)
#     cmp.class2<-sapply(cmp.class2,FUN=remove.sup)
#     ctx.items <-sapply(ctx.items, FUN=remove.sup)
#     
#     return(list(tgt.attr=tgt.attr,cmp.attr=cmp.attr,ctx.attr=ctx.attr,
#                 tgt.class1=tgt.class1,tgt.class2=tgt.class2,
#                 cmp.class1=cmp.class1,cmp.class2=cmp.class2,
#                 ctx.items=ctx.items))
#   })
  
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
  
  #***************REACTIVE**********************#
  
  # The objectives of Data2() are:
  #  -> subsetting the data based on the user's initial context
  #  -> if Atgt is continuous, include a binary attribute based on
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
  #  1. Data2()[[1]] is the data itself, including the mean cutoff attribute if Atgt is cont
  #  2. Data2()[[2]] is the type of variable: continuous or categorical
  #  3. Data2()[[3]] is the number of classes for categorical attributes, NA for cont.
  
  Data2<-reactive({
    
    # three steps:
    # 1. if Atgt is cont, add the tgt.class attribute based on mean first
    # 2. subset the data to include the required rows, based on tgt, cmp and ctx items
    # 3. finally, add tgt.class for cate Atgt and cmp.class based on tgt and cmp items
    
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
    
    # step one: if Atgt is cont, add the tgt.class attribute based on mean first
    if(Groupings()[[1]] == "Cont"){
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
    # tgt.class and cmp.class do not contain the Atgt.names and Acmp.names
    # as per in Groupings()
    # they only contain "above/equal mean"/"below mean" or "1"/"2"
    
    #add the attribute type for the cutoff attribute
    attr.type<-Data()[[2]]
    attr.type<-c(attr.type,"Cate","Cate")
    
    #**console**#
    print(paste("nrow(dfWithCtx): ",nrow(dfWithCtx)))

    return(list(dfWithCtx,attr.type,Data()[[3]]))
    #081014: context bug resolved
  })
  
  #***************END REACTIVE*******************#
  
  # display contexted data
  output$ctx.data<-renderTable({
    data.frame(Data()[[1]][1:input$ctxRows,])
  },digits=3)

  #***************REACTIVE**********************#

  # Table() consists of *THREE OR FOUR* things at the moment
  #  1. Table()[["means.df"]] is the comparison table for cont Atgt
  #  2. Table()[["cont.tab"]]] is the contingency table, for both cont or cate Atgt
  #  3. Table()[["tab.type"]] is the type of (primary) table
  #  4. Table()[["tab.df"]] is the data.frame that was tabulated with 4 columns: Atgt, Acmp, tgt.class, cmp.class
  
  # generate contingency table if target attribute is cate.
  # else generate a comparison table and a contingency table
  
  # reactive wrapper for table
  Table<-reactive({
    # retrieve the relevant data
    
    df<-Data2()[[1]][c(input$targetAttr,input$comparingAttr,"tgt.class","cmp.class")]
    
    #is the target attribute continuous or categorical?
    if(Data2()[[2]][input$targetAttr] == "Cate"){
      tab<-table(df[,c("cmp.class","tgt.class")])
      # cmp.class is rows, tgt.class is columns
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
      # the contingency table based on tgt.class = {"above/equal mean", "below mean"}
      # 131014: for now only the data.frame of means will be displayed in the UI
      
      # first, the data.frame of means and std deviation
      mean1<-mean(df[which(df$cmp.class == "1"),input$targetAttr])
      mean2<-mean(df[which(df$cmp.class == "2"),input$targetAttr])
      
      sd1<-sd(df[which(df$cmp.class == "1"),input$targetAttr])
      sd2<-sd(df[which(df$cmp.class == "2"),input$targetAttr])
      
      means.df<-data.frame(c(mean1,mean2))
      rownames(means.df)<-c(paste(input$whichcmpclassesX,collapse="&"),
                            paste(input$whichcmpclassesY,collapse="&"))
      
      means.df<-cbind(means.df,c(sd1,sd2))
      
      colnames(means.df)<-c(paste("means of ",input$targetAttr,sep=""),
                            paste("sd of ",input$targetAttr,sep=""))
      
      # next, the contingency table
      tab<-table(df[,c("cmp.class","tgt.class")])
      rownames(tab)<-Groupings()[[3]] # Acmp, cmp.class
      colnames(tab)<-Groupings()[[2]] # Atgt, tgt.class
      
      return(list(means.df=means.df,
                  cont.tab=tab,
                  tab.type="Comparison",
                  tab.df=df))
    }
  })

  #***************END REACTIVE*******************#
  
  #render the comparison or contingency table
  output$contTable<-renderTable({
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
                                 as.character(pvalue)))
      rownames(returnMe)<-c("Method","Test statistic","p-value")
      colnames(returnMe)<-"Initial chi-squared test on contingency table"
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
                                as.character(pvalue)))
      rownames(returnMe)<-c("Method","Test statistic","p-value")
      colnames(returnMe)<-"Initial t-test on means"
      returnMe
    }
  })

  # render contingency table for continuous Atgt as well
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

  output$initialTest2<-renderTable({
    
    if(Groupings()[[1]] == "Cont"){
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




  output$hypothesis.statement.it<-renderText({
    if(is.null(input$targetAttr) || is.null(input$comparingAttr)) return("")
    
    tgt.attr<-input$targetAttr
    cmp.attr<-input$comparingAttr
    
    tgt.class1<-input$whichtgtclassesA # <--- could be NULL if Atgt is cont
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
    else if(Groupings()[[1]] == "Cont")
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
  output$hypothesis.statement.td<-renderText({
    if(is.null(input$targetAttr) || is.null(input$comparingAttr)) return("")
    
    tgt.attr<-input$targetAttr
    cmp.attr<-input$comparingAttr
    
    tgt.class1<-input$whichtgtclassesA # <--- could be NULL if Atgt is cont
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
    else if(Groupings()[[1]] == "Cont")
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
  output$hypothesis.statement.cm<-renderText({
    
    if(is.null(input$targetAttr) || is.null(input$comparingAttr)) return("")
    
    tgt.attr<-input$targetAttr
    cmp.attr<-input$comparingAttr
    
    tgt.class1<-input$whichtgtclassesA # <--- could be NULL if Atgt is cont
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
    else if(Groupings()[[1]] == "Cont")
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
  
  #=============================================#
  #============4. Test diagnostics==============#
  #=============================================#

  # test diagnostics for t-test and ANOVA only  
  # not implemented yet
  
  #***************REACTIVE**********************#
  
  Test<-reactive({
    if(Table()[["tab.type"]] == "Comparison"){
      test.type<-"t.test"
      test<-t.test(Data2()[[1]][,input$targetAttr]~Data2()[[1]]$cmp.class)
    }
    else if(Table()[["tab.type"]] == "Contingency"){
      test<-chisq.test(Table()[[1]])
      if(length(input$whichcmpclassesX) > 1 || length(input$whichcmpclassesY) > 1){
        test.type<-"collapsed.chi.sq"
      }
      else
        test.type<-"chi.sq"
    }
    return(list(test=test,test.type=test.type))
  })
  
  #***************END REACTIVE*******************#

  output$KStest<-renderTable({
    if(Test()[["test.type"]] == "t.test"){
      df<-Data2()[[1]][,c(input$targetAttr,"cmp.class")]
      
      test<-ks.test(df[which(df$cmp.class == "1"),input$targetAttr],
                    df[which(df$cmp.class == "2"),input$targetAttr])
      
      stats<-test$statistic
      pvalue<-test$p.value
      method<-test$method
      
      returnMe<-as.data.frame(c(as.character(method),
                                as.character(round(stats,3)),
                                as.character(pvalue)))
      rownames(returnMe)<-c("Method","Test statistic","p-value")
      colnames(returnMe)<-"Kolmogorov-Smirnov test for normality"
      returnMe
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
  output$MWtest<-renderTable({
    if(Test()[["test.type"]] == "t.test"){
      
      df<-Data2()[[1]][,c(input$targetAttr,"cmp.class")]
      
      if(var.test(df[which(df$cmp.class == "1"),input$targetAttr],
                  df[which(df$cmp.class == "2"),input$targetAttr])$p.value <= p.significant ||
           ks.test(df[which(df$cmp.class == "1"),input$targetAttr],
                   df[which(df$cmp.class == "2"),input$targetAttr])$p.value <= p.significant){
        
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

  output$flat.table<-renderTable({
    if(Test()[["test.type"]] == "collapsed.chi.sq"){
      tab.df<-Table()[["tab.df"]][,c(2:3)] # ony Acmp and tgt.class
      tab<-table(tab.df)
      
      colnames(tab)<-Groupings()[["Atgt.names"]]
      return(tab)
    }
  })
  output$flat.chi.sq<-renderTable({
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
  output$chi.sq.top<-renderTable({
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
                          (((o-e)^2)/e)[,1])
      colnames(chisq.contri)<-c("Observed",
                                "Expected",
                                "Chi-squared contribution")
      return(chisq.contri)
    }
  })

  #***************REACTIVE**********************#
  
  # Objective of Data3(): discretize all attributes for ctx mining

  Data3<-reactive({
    df<-Data2()[[1]]
    attr.type<-Data2()[[2]]
    
    #change all continuous attributes to categorical before hypothesis mining
    #discretise by the mean
    mean.discre<-function(an.attr){
      m<-mean(df[,an.attr])
      new.col<-sapply(df[,an.attr],
                      FUN=function(x){
                        if(x>=m) return("above/equal mean")
                        else return("below mean")})
      return(new.col)
    }
    
    which.are.cont<-which(attr.type == "Cont")
    for(an.attr in which.are.cont)
      df[,an.attr]<-mean.discre(an.attr)
    
    attr.type<-rep("Cate",length(attr.type))
    
    return(list(df,attr.type,Data2()[[3]]))
  })
  
  #=============================================#
  #============5. Context mining================#
  #=============================================#
  
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
    
    print(fm.tgt)
    print(fm.cmp)
    
    # take the first k attributes, consider them shortlisted
    if(length(predictors) < top.k)
      k<-length(predictors)
    else
      k<-top.k
    
    print(k)
    
    withProgress(session, {
      setProgress(message="Mining context...",detail="This might take a while...")
      # construct models
      run.time.tgt<-system.time(
        mod.tgt<-randomForest(formula=fm.tgt,
                              data=df,
                              importance=TRUE))[3]
      setProgress(message="Target model constructed.",detail = "Constructing comparing model...")
      run.time.cmp<-system.time(
        mod.cmp<-randomForest(formula=fm.cmp,
                              data=df,
                              importance=TRUE))[3]
      setProgress(message="Comparing model constructed.")
      
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
      
      # 211014: using adjusted geometric mean as a start
      # need to first figure out which class is less
      # arbitrarily consider 5:1 as class-imbalance
      # AGm = (Gm+SP*Nn)/(1+Nn),
      # where SP refers to the specificity, which is the sensitivity for the negative class
      # Nn refers to proportion of the data that belongs to the more abundant class
      
      # mod.tgt first
      sup.tgt1<-tmp.cm.tgt[1,1]+tmp.cm.tgt[1,2]
      sup.tgt2<-tmp.cm.tgt[2,1]+tmp.cm.tgt[2,2]
      if(sup.tgt1/sup.tgt2 >= class.ratio){ # class.ratio defined in settings.R
        # tgt1 is more abundant => tgt1 is -ve
        # tgt2 is less abundant => tgt2 is +ve
        se<-tmp.cm.tgt[2,2]/(tmp.cm.tgt[2,2]+tmp.cm.tgt[2,1])
        sp<-tmp.cm.tgt[1,1]/(tmp.cm.tgt[1,1]+tmp.cm.tgt[1,2])
        gm<-sqrt(se*sp)
        nn<-sup.tgt1/(sup.tgt1+sup.tgt2)
        acc.tgt<-(gm+sp*nn)/(1+nn)
      }
      else if(sup.tgt2/sup.tgt1 >= class.ratio){
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
      sup.cmp2<-tmp.cm.cmp[2,1]+tmp.cm.cmp[2,2]
      if(sup.cmp1/sup.cmp2 >= class.ratio){ # class.ratio defined in settings.R
        # cmp1 is more abundant => cmp1 is -ve
        # cmp2 is less abundant => cmp2 is +ve
        se<-tmp.cm.cmp[2,2]/(tmp.cm.cmp[2,2]+tmp.cm.cmp[2,1])
        sp<-tmp.cm.cmp[1,1]/(tmp.cm.cmp[1,1]+tmp.cm.cmp[1,2])
        gm<-sqrt(se*sp)
        nn<-sup.cmp1/(sup.cmp1+sup.cmp2)
        acc.cmp<-(gm+sp*nn)/(1+nn)
      }
      else if(sup.cmp2/sup.cmp1 >= class.ratio){
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
        
        mined.attr<-names(mda.both)[seq(k)]
        
        # function to remove the tails ".tgt" and ".cmp"
        remove.tail<-function(s){
          last.dot<-regexpr("\\.[^\\.]*$", s)
          return(substr(s,1,last.dot-1))
        }
        mined.attr<-sapply(mined.attr,FUN=remove.tail)
        
        print(mined.attr)
        
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
      
      rownames(cm.tgt)<-colnames(cm.tgt)[1:2]<-Groupings()[[2]]
      rownames(cm.cmp)<-colnames(cm.cmp)[1:2]<-Groupings()[[3]]
      
      if(!is.null(mined.attr)) return(list(cm.tgt,
                                           cm.cmp,
                                           mined.attr,
                                           run.time.tgt=run.time.tgt,
                                           run.time.cmp=run.time.cmp))
      
      #both mod.tgt and mod.cmp are inaccurate, therefore no mined attributes
      else return(list(cm.tgt,
                       cm.cmp,
                       NULL,
                       run.time.tgt=run.time.tgt,
                       run.time.cmp=run.time.cmp))
    })
  })
  
  #*********************************************#
  
  output$run.time.tgt<-renderText({
    return(paste("Run time for target model: ",round(minedAttributes()[["run.time.tgt"]],3),sep=""))
  })
  output$run.time.cmp<-renderText({
    return(paste("Run time for comparing model: ",round(minedAttributes()[["run.time.cmp"]],3),sep=""))
  })

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
    print(paste("ncol(df.to.plot): ",ncol(df.to.plot)))
    par(mfrow=c(2,2))

    #need to consider whether Atgt is continuous or categorical
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
        else if(Data2()[[2]][[tgt.attr]] == "Cont"){
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
        else if(Data()[[2]][mined.attr] == "Cont")
          hist(plot.dat,main=paste(Groupings()[[2]][i], Groupings()[[3]][j], sep="&"))
      }
    }
  })
  
  #=============================================#
  #============5. Hypothesis mining=============#
  #=============================================#

  #***************REACTIVE**********************#

  Hypotheses<-reactive({
    
    tgt.attr<-input$targetAttr
    cmp.attr<-input$comparingAttr
    mined.attr<-minedAttributes()[[3]]
    
    df<-Data3()[[1]][,c(tgt.attr,cmp.attr,"cmp.class","tgt.class",c(mined.attr))]
    
    #function to compute proportions, given a 2x2 table
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
    
    # compute initial proportions
    cont.tab<-Table()[["cont.tab"]]
    initial.p1<-compute.prop(cont.tab)[1]
    initial.p2<-compute.prop(cont.tab)[2]
    # compute initial n
    initial.n1<-compute.sup(cont.tab)["n1"]
    initial.n2<-compute.sup(cont.tab)["n2"]

    #**console**#
    print(paste("initial.p1: ",initial.p1))
    print(paste("initial.p2: ",initial.p2))
    print(paste("initial.n1: ",initial.n1))
    print(paste("initial.n2: ",initial.n2))
    
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
      }
      
      else{
        prop.df$c11[i]<-prop.df$c12[i]<-NA
        prop.df$c21[i]<-prop.df$c22[i]<-NA
        prop.df$n1prime[i]<-prop.df$n2prime[i]<-NA
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
      
      if(any(sup < min.sup.cij) == TRUE ||
           any(is.na(sup)) == TRUE)
        prop.df$sufficient[i]<-FALSE
      else prop.df$sufficient[i]<-TRUE
    }
    
    #correct for multiple testing using Bonferroni correction
    prop.df$pvalue.adj<-p.adjust(prop.df$pvalue, method = "bonferroni")
    # sort
    prop.df<-prop.df[with(prop.df,order(-sufficient,difflift,contri,pvalue.adj)),]
    return(prop.df)
  })
  
  output$hypotheses<-renderTable({
    Hypotheses()
  },digits=3)


  #=============================================#
  #============5. Hypothesis analysis===========#
  #=============================================#

  output$analyse.ctrl<-renderUI({
    selectizeInput("analyse.which.item","Select context item",rownames(Hypotheses()))
  })
  output$analyse.hypothesis<-renderTable({
    prop.df<-subset(Hypotheses(),select=c(sufficient,SP,difflift,contri,pvalue,pvalue.adj))
    prop.df<-prop.df[with(prop.df,order(-sufficient,difflift,contri,pvalue.adj)),]
    return(prop.df)
  })

  output$analyse.hypothesis.statement.initial<-renderText({
    if(is.null(input$targetAttr) || is.null(input$comparingAttr)) return("")
    
    tgt.attr<-input$targetAttr
    cmp.attr<-input$comparingAttr
    
    tgt.class1<-input$whichtgtclassesA # <--- could be NULL if Atgt is cont
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
    else if(Groupings()[[1]] == "Cont")
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
                                as.character(pvalue)))
      rownames(returnMe)<-c("Method","Test statistic","p-value")
      colnames(returnMe)<-"Initial chi-squared test on contingency table"
      returnMe
    }
    else if(Groupings()[[1]] == "Cont"){
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
  # based on selected item
  output$analyse.cont.tab<-renderTable({
        
    print(input$analyse.which.item)
    
    item<-input$analyse.which.item
    Actx<-unlist(strsplit(item,"="))[1]
    vctx<-unlist(strsplit(item,"="))[2]
    
    df<-Data3()[[1]][,c("tgt.class","cmp.class",Actx)]
    
    rows.to.prop<-which(df[,Actx] == vctx)
    df.to.prop<-df[rows.to.prop,c("cmp.class","tgt.class")]
    tab<-table(df.to.prop)
    
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
  })
  output$analyse.test<-renderTable({
    item<-input$analyse.which.item
    Actx<-unlist(strsplit(item,"="))[1]
    vctx<-unlist(strsplit(item,"="))[2]
    
    df<-Data3()[[1]][,c("tgt.class","cmp.class",Actx)]
    
    rows.to.prop<-which(df[,Actx] == vctx)
    df.to.prop<-df[rows.to.prop,c("cmp.class","tgt.class")]
    tab<-table(df.to.prop)
    
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
  })
  output$analyse.hypothesis.statement<-renderText({
    if(is.null(input$targetAttr) || is.null(input$comparingAttr)) return("")
  
    tgt.attr<-input$targetAttr
    cmp.attr<-input$comparingAttr
  
    tgt.class1<-input$whichtgtclassesA # <--- could be NULL if Atgt is cont
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
    else if(Groupings()[[1]] == "Cont")
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
  })

  Settings<-reactive({
    # data
    log.filename<-as.character(input$datFile[[1]])
    log.header<-input$datHeader
    log.sep<-input$datSep
    log.quote<-input$datQuote

    # test diagnostics
    log.p.significant<-p.significant
    
    # context mining
    log.acc.rf.default<-acc.rf.default
    log.top.k<-top.k
    log.class.ratio<-class.ratio
    log.mined.attr<-paste(minedAttributes()[[3]],collapse=", ")
    
    # hypothesis mining
    log.min.sup.cij<-min.sup.cij
    
    col1<-c(rep("Data",4),
            rep("Test diagnostics",1),
            rep("Context mining",4),
            rep("Hypothesis mining",1))
    col2<-c("Filename",
            "Header has attribute names",
            "Separator",
            "Quotes",
            "p-value threshold",
            "Random forest accuracy threshold",
            "Number of context attributes to shortlist",
            "Class ratio threshold for class-imbalance learning",
            "Mined attributes",
            "Min support for each cell")
    col3<-c(log.filename,
            log.header,
            log.sep,
            log.quote,
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

  output$session.log<-renderTable({
    return(Settings())
  })

}) #end shinyServer