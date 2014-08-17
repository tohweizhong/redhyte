require(shiny)

titlePNG<-"images/title.png"
logoPNG<-"images/logo.png"

shinyServer(function(input,output){
  
  #fancy work here
  #render images
  output$titlePNG<-renderImage({
    list(src=titlePNG, alt=NULL)
  },deleteFile=FALSE)
  
  output$logoPNG<-renderImage({
    list(src=logoPNG, alt=NULL)
  },deleteFile=FALSE)
  
  #=============================================#
  #=============================================#
  #=============================================#
  
  #grabbing the data
  #Data() consists of *THREE* things at the moment
  # 1. Data()[[1]] is the data itself
  # 2. Data()[[2]] is the type of variable: continuous or categorical
  # 3. Data()[[3]] is the number of classes for categorical attributes, NA for cont.
  
  #here begins the real work..
  
  #===============REACTIVE======================#

  Data<-reactive({
    datFile<-input$datFile #still just the file name
    if (is.null(datFile)) return(NULL)
    df<-read.csv(datFile$datapath,header=input$datHeader,sep=input$datSep,quote=input$datQuote)
    
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
  #==============1. Uploaded data===============#
  #=============================================#
  
  #first tab panel: "1. Uploaded data"
  
  #displaying a preview of the data
  #10 rows, all columns
  output$dataPreview<-renderTable({
    if (is.null(Data()[1])) return(NULL)
    data.frame(Data()[[1]][1:10,])
  },digits=3)
  
  #=============================================#
  #==============1. Data viz====================#
  #=============================================#
  
  #second tab panel: "2. Data visualization"
  
  #create two dropdown boxes to choose two variables,
  #display boxplot and histogram for simple eda
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
    if(Data()[[2]][input$whichAttr1]=="Cont"){
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
  
  
#=============================================#
#==============3. Hypo testing================#
#=============================================#
  
  #third tab panel: "3. Hypothesis testing"
  
  #dropdown boxes
  output$targetCtrl<-renderUI({
    selectizeInput("targetAttr","Indicate target attribute (May be continuous or categorical)",colnames(Data()[[1]]))
  })
  
  output$comparingCtrl<-renderUI({
    selectizeInput("comparingAttr","Indicate comparing attribute (Must be categorical)",colnames(Data()[[1]]))
  })
  
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

  #display boxplot stats (Tukey's five)
  output$targetTukeyfive<-renderTable({
    qt<-as.data.frame(fivenum(Data()[[1]][,input$targetAttr]))
    rownames(qt)<-c("Min","25%","Median","75%","Max")
    colnames(qt)<-NULL
    qt
  })
  
  output$comparingTukeyfive<-renderTable({
    qt<-as.data.frame(fivenum(Data()[[1]][,input$comparingAttr]))
    rownames(qt)<-c("Min","25%","Median","75%","Max")
    colnames(qt)<-NULL
    qt
  })

  #generate contingency table if target attribute is cate.
  #else generate a comparison table

  #===============REACTIVE======================#

  #reactive wrapper for table
  Table<-reactive({
    #retrieve the relevant data
    df<-Data()[[1]][c(input$targetAttr,input$comparingAttr)]
    
    #is the target attribute continuous or categorical?
    if(Data()[[2]][input$targetAttr] == "Cate")
      return(list(table(df),"Contingency",length(unique(df[,input$comparingAttr]))))
    
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
      return(list(data.frame(means),"Comparison",length(cl)))
    }
  })

  output$contTable<-renderTable({
    Table()[[1]]
  })

  #first test
  output$firstTest<-renderTable({

    #check the type of table
    if(Table()[[2]] == "Contingency"){
      test<-chisq.test(Table()[[1]])
      stats<-test$statistic
      pvalue<-test$p.value
      method<-test$method
      
      returnMe<-as.data.frame(c(as.character(method),
                                 as.character(round(stats,3)),
                                 as.character(round(pvalue,5))
                                 ))
      rownames(returnMe)<-c("Method","Test-statistic","p-value")
      colnames(returnMe)<-NULL
      returnMe
    }
    else if(Table()[[2]] == "Comparison"){
      if(Table()[[3]] == 2){ #t-test
        test<-t.test(Table()[[1]])
        stats<-test$statistic
        pvalue<-test$p.value
        method<-test$method
        
        returnMe<-as.data.frame(c(as.character(method),
                                  as.character(round(stats,3)),
                                  as.character(round(pvalue,5))
        ))
        rownames(returnMe)<-c("Method","Test-statistic","p-value")
        colnames(returnMe)<-NULL
        returnMe
      }
      else{ #anova
        return(NULL) #not implemented yet
      }
    }
  })


})