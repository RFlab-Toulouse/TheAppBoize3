options(shiny.maxRequestSize=60*1024^2) 
source("global.R")
#options(xtable.include.colnames=T)
#options(xtable.include.rownames=T)

shinyServer(function(input, output,session) {
  
  output$modelUploaded <- reactive({
    return(!is.null(input$modelfile))
  })
  outputOptions(output, 'modelUploaded', suspendWhenHidden=FALSE)
  
  output$fileUploaded <- reactive({
    return(!is.null(input$learningfile))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$image1<-renderImage({return (list(src="pictures/Logo I2MC.jpg", 
                                           contentType="image/jpeg",
                                           width=300,
                                           height=200,
                                           alt="I2MC logo"))},deleteFile = F)
  output$image2<-renderImage({return (list(src="pictures/rflabxx.png", 
                                           contentType="image/png",
                                           width=600,
                                           height=200,
                                           alt="RFlab logo"))},deleteFile = F)
  output$image3<-renderImage({return (list(src="pictures/structurdata2.jpg", 
                                           contentType="image/jpeg",
                                           width=600,
                                           height=300,
                                           alt="structure data"))},deleteFile = F)
  
  output$fileUploadedval <- reactive({
    return( !is.null(DATA()$VALIDATION))
  })
  outputOptions(output, 'fileUploadedval', suspendWhenHidden=FALSE)
  
  output$modelUploadedval <- reactive({
    return(!is.null(DATA()$VALIDATION))
  })
  outputOptions(output, 'modelUploadedval', suspendWhenHidden=FALSE)
  
  #Save state#############  
  state <- reactiveValues()
  observe({
    importparameters <<- list("learningfile"=input$learningfile,"validationfile"=input$validationfile,"modelfile"=input$modelfile,"extension" = input$filetype,
                              "NAstring"=input$NAstring,"sheetn"=input$sheetn,"skipn"=input$skipn,"dec"=input$dec,"sep"=input$sep,
                              "transpose"=input$transpose,"zeroegalNA"=input$zeroegalNA,confirmdatabutton=input$confirmdatabutton)
    
    selectdataparameters<<-list("prctvalues"=input$prctvalues,"selectmethod"=input$selectmethod,"NAstructure"=input$NAstructure,"structdata"=input$structdata,
                                "thresholdNAstructure"=input$thresholdNAstructure,"maxvaluesgroupmin"=input$maxvaluesgroupmin,"minvaluesgroupmax"=input$minvaluesgroupmax)
    
    transformdataparameters<<-list("log"=input$log,"logtype"=input$logtype,"standardization"=input$standardization,"arcsin"=input$arcsin,"rempNA"=input$rempNA)
    
    
    testparameters<<-list("SFtest"=input$SFtest,"test"=input$test,"adjustpv"=input$adjustpv,"thresholdpv"=input$thresholdpv,"thresholdFC"=input$thresholdFC)
    
    modelparameters<<-list("modeltype"=input$model,"invers"=input$invers,"thresholdmodel"=input$thresholdmodel,
                           "fs"=input$fs,"adjustval"=input$adjustval)
    parameters<-list("importparameters"=importparameters,"selectdataparameters"=selectdataparameters,
                     "transformdataparameters"=transformdataparameters,"testparameters"=testparameters,"modelparameters"=modelparameters)
    data<-DATA()
    selectdata<-SELECTDATA()
    transformdata<-TRANSFORMDATA()
    test<-TEST()
    model<-MODEL()
    settingstable<-statetable()
    isolate(state<<-list("parameters"=parameters,"data"=data,"selectdata"=selectdata,"transformdata"=transformdata,"test"=test,"model"=model,"settingstable"=settingstable)) 
  })
  
  output$savestate <- downloadHandler(
    filename <- function(){
      paste("model.RData")
    },
    content = function(file) { 
      save(state, file = file)
    }
  )
  observe({
    if(input$confirmdatabutton!=0 & !is.null(input$modelfile)){
      print("update")
      dataaaa<<-DATA()
      # sendConfetti(
      #   colors = list("#DAB436", "#36DA62", "#365CDA", "#DA36AE")
      # )
      # message("You have sent confetti")
      
      # session$sendCustomMessage(type = "evaljs", message = list(code = "triggerFireworks();"))
      updateNumericInput(session, "prctvalues", value = DATA()$previousparameters$selectdataparameters$prctvalues)
      updateRadioButtons(session,"selectmethod",selected =  DATA()$previousparameters$selectdataparameters$selectmethod)
      updateCheckboxInput(session ,"NAstructure",value=DATA()$previousparameters$selectdataparameters$NAstructure)
      updateRadioButtons(session,"structdata",selected=DATA()$previousparametersselectdataparameters$parameters$structdata)
      updateNumericInput(session, "maxvaluesgroupmin", value = DATA()$previousparametersselectdataparameters$parameters$maxvaluesgroupmin)
      updateNumericInput(session, "minvaluesgroupmax", value = DATA()$previousparametersselectdataparameters$parameters$minvaluesgroupmax)
      updateNumericInput(session, "thresholdNAstructure", value = DATA()$previousparameters$selectdataparameters$thresholdNAstructure)
      
      updateRadioButtons(session,"rempNA",selected=DATA()$previousparameters$transformdataparameters$rempNA)
      updateCheckboxInput(session ,"log",value=DATA()$previousparameters$transformdataparameters$log)
      updateRadioButtons(session ,"logtype",selected=DATA()$previousparameters$transformdataparameters$logtype)
      updateCheckboxInput(session ,"standardization",value=DATA()$previousparameters$transformdataparameters$standardization)
      updateCheckboxInput(session ,"arcsin",value=DATA()$previousparameters$transformdataparameters$arcsin)
      
      updateRadioButtons(session,"test",selected=DATA()$previousparameters$testparameters$test)
      updateNumericInput(session, "thresholdFC", value = DATA()$previousparameterstestparameters$parameters$thresholdFC)
      updateNumericInput(session, "thresholdpv", value = DATA()$previousparameterstestparameters$parameters$thresholdpv)
      updateCheckboxInput(session ,"adjustpval",value=DATA()$previousparameterstestparameters$parameters$adjustpval)
      updateCheckboxInput(session ,"SFtest",value=DATA()$previousparameterstestparameters$parameters$SFtest)
      
      updadeNumericInput(session, "n_iterations", value = DATA()$previousparameters$bootstrapparameters$n_iterations)
      updateNumericInput(session, "sample_percentage", value = DATA()$previousparameters$bootstrapparameters$sample_percentage)
      updateRadioButtons(session,"bootstrap_test",selected=DATA()$previousparameters$bootstrapparameters$bootstrap_test)
      updateNumericInput(session, "bootstrap_thresholdFCOOB", value = DATA()$previousparameters$bootstrapparameters$bootstrap_thresholdFCOOB)
      updateNumericInput(session, "bootstrap_thresholdpvOOB", value = DATA()$previousparameters$bootstrapparameters$bootstrap_thresholdpvOOB)
      updateCheckboxInput(session ,"bootstrap_adjustpvOOB",value=DATA()$previousparameters$bootstrapparameters$bootstrap_adjustpvOOB)
      updateNumericInput(session, "stability_threshold", value = DATA()$previousparameters$bootstrapparameters$stability_threshold)
      updateRadioButtons(session,"seuil_method",selected=DATA()$previousparameters$bootstrapparameters$seuil_method)
      
      # ceci permet de mettre à jour les radio buttons et les inputs numériques
      updateRadioButtons(session,"stats_method",selected=DATA()$previousparameters$bootstrapparameters$stats_method)
      
      updateRadioButtons(session,"model",selected=DATA()$previousparameters$modelparameters$modeltype)
      updateNumericInput(session, "thresholdmodel", value = DATA()$previousparameters$modelparameters$thresholdmodel)
      updateCheckboxInput(session ,"fs",value=DATA()$previousparameters$modelparameters$fs)
      
      updateCheckboxInput(session ,"adjustval",value=DATA()$previousparameters$modelparameters$adjustval)
      updateCheckboxInput(session ,"invers",value=DATA()$previousparameters$modelparameters$invers)
      
    }
  })
  
  statetable <- reactive({
    table <- matrix(data = "",nrow = 20,ncol=11)
    if((input$confirmdatabutton!=0 & !is.null(input$modelfile))){
      learningfile<-DATA()$previousparameters$importparameters$learningfile
    }
    else{learningfile<-input$learningfile}
    
    table[1,1:9]<-c("#","Extensionfile","decimal character","separator character","NA string","sheet number","skip lines","consider NA as 0","transpose")
    table[2,1:9]<-c("import parameters",learningfile$type,input$dec,input$sep,input$NAstring,
                    input$sheetn,input$skipn,input$zeroegalNA,input$transpose)
    
    table[3,]<-c("#","name learning file", "number of rows", "number of columns", paste("number of ",levels(DATA()$LEARNING[,1])[1]),
                 paste("number of ",levels(DATA()$LEARNING[,1])[2]),"name validation file", "number of rows", "number of columns", paste("number of ",levels(DATA()$VALIDATION[,1])[1]),
                 paste("number of ",levels(DATA()$VALIDATION[,1])[2]))
    table[4,]<-c("main results",learningfile$name,dim(DATA()$LEARNING)[1],dim(DATA()$LEARNING)[2],nll(sum(DATA()$LEARNING[,1]==levels(DATA()$LEARNING[,1])[1])),
                 nll(sum(DATA()$LEARNING[,1]==levels(DATA()$LEARNING[,1])[2])),nll(input$validationfile$name),nll(dim(DATA()$VALIDATION)[1]),
                 nll(dim(DATA()$VALIDATION)[2]),nll(sum(DATA()$VALIDATION[,1]==levels(DATA()$VALIDATION[,1])[1])),
                 nll(sum(DATA()$VALIDATION[,1]==levels(DATA()$VALIDATION[,1])[2])))
    table[5,1:8]<-c("#","percentage of values minimum","method of selection","select features structured","search structur in",
                    "threshold p-value of proportion test", "maximum % values of the min group","minimum % values of the max group")
    table[6,1:8]<-c("select parameters",selectdataparameters[[1]],selectdataparameters[[2]],selectdataparameters[[3]],
                    selectdataparameters[[4]],selectdataparameters[[5]],selectdataparameters[[6]],selectdataparameters[[7]])
    table[7,1:3]<-c("#","number of feature selected","number of feature structured")
    table[8,1:3]<-c("main results",dim(SELECTDATA()$LEARNINGSELECT)[2]-1,nll(dim(SELECTDATA()$STRUCTUREDFEATURES)[2]))
    table[9,1:5]<-c("#","remplace NA by","transformation log","strandardisation","arcsin transformation")
    if(transformdataparameters[[1]]=="FALSE"){logprint<-"FALSE"}
    else{logprint<-transformdataparameters[[2]]}
    table[10,1:5]<-c("transform parameters",transformdataparameters[[5]],logprint,transformdataparameters[[3]],transformdataparameters[[4]])
    table[11,1]<-c("#")
    table[12,1]<-c("main results")
    table[13,1:5]<-c("#","test","use Bonferroni adjustment","threshold of significativity","Fold change threshold")
    table[14,1:5]<-c("test parameters",input$test,input$adjustpv,input$thresholdpv,input$thresholdFC)
    table[15,1:2]<-c("#","number of differently expressed features")
    table[16,1:2]<-c("main results",dim(TEST()$LEARNINGDIFF)[2]-1)
    
    if(input$model!="nomodel"){
      table[17,1:6]<-c("#","model type","cut-off of the model","feature selection","apply model on validation","invers groups")
      table[18,1:6]<-c("model parameters",input$model,input$thresholdmodel,input$fs,input$adjustval,input$invers)
      table[19,1:8]<-c("#","number of features","AUC learning","sensibility learning","specificity learning","AUC validation","sensibility validation","specificity validation")
      #       line20<<-c("main results",dim(MODEL()$DATALEARNINGMODEL$learningmodel)[2]-1,
      #                  as.numeric(auc(roc(MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$scorelearning))),
      #                  sensibility(MODEL()$DATALEARNINGMODEL$reslearningmodel$predictclasslearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning),
      #                  specificity(MODEL()$DATALEARNINGMODEL$reslearningmodel$predictclasslearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning),
      #                  as.numeric(auc(roc(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval))),
      #                  sensibility(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$predictclassval),
      #                  specificity(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$predictclassval)
      #       )
      table[20,1:5]<-c("main results",dim(MODEL()$DATALEARNINGMODEL$learningmodel)[2]-1,
                       round(as.numeric(auc(roc(MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$scorelearning))),digits = 3),
                       sensibility(MODEL()$DATALEARNINGMODEL$reslearningmodel$predictclasslearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning),
                       specificity(MODEL()$DATALEARNINGMODEL$reslearningmodel$predictclasslearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning)
      )
      if(input$adjustval){
        table[20,6:8]<-c(round(as.numeric(auc(roc(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval))),digits = 3),
                         sensibility(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$predictclassval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval),
                         specificity(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$predictclassval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval)
        )
      }
    }
    return(table)
    
  }) 
  
  output$savestatetable<- downloadHandler(
    filename = function() { paste('settingstable', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   statetable(), file,cnames=F,rnames=F) })
  
  ############
  output$namefilelearn<-renderText({
    namelearn<-input$learningfile$name
  })
  output$dim1learn<-renderText({
    di1<-dim(x = DATA()$LEARNING)[1]  
  })
  output$dim2learn<-renderText({
    di2<-dim(x = DATA()$LEARNING)[2]-1  
  })
  output$namefileval<-renderText({
    nameval<-input$validationfile$name
  })  
  output$dim1val<-renderText({
    di1<-dim(x = DATA()$VALIDATION)[1]  
  })
  output$dim2val<-renderText({
    di2<-dim(x = DATA()$VALIDATION)[2]  
  })  
  
  #si erreur envoyÃÂÃÂ© pb import
  DATA <- reactive({
    importparameters <<- list("learningfile" = input$learningfile, 
                              "validationfile" = input$validationfile,
                              "modelfile" = input$modelfile,
                              "extension" = input$filetype,
                              "NAstring" = input$NAstring,
                              "sheetn" = input$sheetn,
                              "skipn" = input$skipn,
                              "dec" = input$dec,
                              "sep" = input$sep,
                              "transpose" = input$transpose,
                              "zeroegalNA"= input$zeroegalNA, 
                              confirmdatabutton = input$confirmdatabutton,
                              invers = input$invers)
    
    out<-tryCatch(importfunction(importparameters),error=function(e) e )
    #      if(any(class(out)=="error"))print("error")
    #      else{resimport<-out}
    validate(need(any(class(out)!="error"),"error import"))
    resimport<<-out
    #resimport<-importfunction(importparameters)
    list(LEARNING=resimport$learning, 
         VALIDATION=resimport$validation,
         previousparameters=resimport$previousparameters  
         #          LEVELS=resimport$lev
    )
  })
  # 
  # output$JDDlearn = DT::renderDataTable({
  #   learning<-DATA()$LEARNING
  #   validate(need(!is.null(learning),"problem import"))
  #   colmin<-min(ncol(learning),100)
  #   rowmin<-min(nrow(learning),100)
  #   cbind(Names=rownames(learning[1:rowmin,1:colmin]),learning[1:rowmin,1:colmin])
  #   },
  #   options = list(    "orderClasses" = F,
  #                      "responsive" = F,
  #                      "pageLength" = 10)
  #   )
  
  
  output$JDDlearn =  DT::renderDataTable({
    learning <- DATA()$LEARNING
    validate(need(!is.null(learning),"problem import"))
    colmin<-min(ncol(learning),100)
    rowmin<-min(nrow(learning),100)
    cbind(Names=rownames(learning[1:rowmin,1:colmin]),learning[1:rowmin,1:colmin]) %>% DT::datatable(option = list(
      "orderClasses" = F,
      "responsive" = F,
      "pageLength" = 10,
      "scrollX" = TRUE,
      "scrollY" = "400px",
      "autoWidth" = TRUE
    )) # %>% DT::formatStyle(columns = 1, fontWeight = 'bold')
    # },
    # options = list(    "orderClasses" = F,
    #                    "responsive" = F,
    #                    "pageLength" = 10
  })
  
  output$downloaddataJDDlearn <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   DATA()$LEARNING, file) })
  
  
  output$JDDval = DT::renderDataTable({
    validation<-DATA()$VALIDATION
    validate(need(!is.null(validation),"problem import"))
    colmin<-min(ncol(validation),100)
    rowmin<-min(nrow(validation),100)
    # cbind(Names=rownames(validation[1:rowmin,1:colmin]),validation[1:rowmin,1:colmin])},
    # options = list(    "orderClasses" = F,
    #                    "responsive" = F,
    #                    "pageLength" = 10)
    
    cbind(Names=rownames(validation[1:rowmin,1:colmin]),validation[1:rowmin,1:colmin]) %>% DT::datatable(option = list(
      "orderClasses" = F,
      "responsive" = F,
      "pageLength" = 10,
      "scrollX" = TRUE,
      "scrollY" = "400px",
      "autoWidth" = TRUE
    ))
    
  }) 
  
  output$downloaddataJDDval <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   DATA()$VALIDATION, file) }
  )
  
  
  #################
  SELECTDATA <- reactive({
    selectdataparameters<<-list("prctvalues"=input$prctvalues,"selectmethod"=input$selectmethod,"NAstructure"=input$NAstructure,"structdata"=input$structdata,
                                "thresholdNAstructure"=input$thresholdNAstructure,"maxvaluesgroupmin"=input$maxvaluesgroupmin,"minvaluesgroupmax"=input$minvaluesgroupmax)
    validate(need(selectdataparameters$prctvalues>=0 &selectdataparameters$prctvalues<=100,"%  NA has to be between 0 and 100"))
    validate(need(input$minvaluesgroupmax>=0 &input$minvaluesgroupmax<=100 & input$maxvaluesgroupmin>=0 &input$maxvaluesgroupmin<=100,"% threshold has to be between 0 and 100"),
             need(input$thresholdNAstructure>0,input$thresholdNAstructure<1,"threshold of the pvalue has to be between 0 and 1"))
    learning <<- DATA()$LEARNING
    validate(need(input$confirmdatabutton!=0,"Importation of datas has to be confirmed"))
    
    validate(need(length(levels(learning[,1])) == 2, "number of groups is not equal to 2"))
    resselectdata <<- selectdatafunction(learning = learning, selectdataparameters = selectdataparameters)
    list(LEARNINGSELECT = resselectdata$learningselect,STRUCTUREDFEATURES=resselectdata$structuredfeatures,DATASTRUCTUREDFEATURES=resselectdata$datastructuredfeatures,selectdataparameters)
  })
  
  #####
  #Selection Output
  #####
  output$downloaddataselect<- downloadHandler(
    filename = function() { paste('Dataselect', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(SELECTDATA()$LEARNINGSELECT, file)
    }
  )
  
  output$nvarselect=renderText({
    di1<-dim(x = SELECTDATA()$LEARNINGSELECT)[2]-1  
  })
  
  # fonction d'affichage des variables selectionnees
  output$heatmapNA <- renderPlot({
    learningselect <- SELECTDATA()$LEARNINGSELECT
    heatmapNA(toto = learningselect)
  })
  
  # fonction d'export des variables selectionnees
  output$downloadplotheatmapNA = downloadHandler(
    filename = function() { 
      paste('graph','.',input$paramdownplot, sep='') 
    },
    content = function(file) {
      ggsave(file, plot =    heatmapNA(toto =SELECTDATA()$LEARNINGSELECT), 
             device = input$paramdownplot)
    },
    contentType=NA)
  
  # fonction d'export des variables selectionnees
  output$downloaddataheatmapNA <- downloadHandler(
    filename = function() { paste('dataset distribution of NA', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(as.data.frame(heatmapNA(toto =SELECTDATA()$LEARNINGSELECT,graph = F)), file)
    }
  )
  
  # observe({
  #   req(heatmapNA(toto =SELECTDATA()$LEARNINGSELECT,graph = F))
  #   print(class(heatmapNA(toto =SELECTDATA()$LEARNINGSELECT,graph = F)))
  # })
  
  # fonction d'affichage de la distribution des NA
  output$plotNA<-renderPlot({
    learningselect <-  SELECTDATA()$LEARNINGSELECT
    learning<-DATA()$LEARNING
    distributionvalues(toto = learning,prctvaluesselect =input$prctvalues/100,nvar = ncol(learningselect) ,ggplot =  T)  
  })
  
  # fonction d'export de la distribution des NA
  output$downloadplotNA = downloadHandler(
    filename = function() { 
      paste('graph','.',input$paramdownplot, sep='') 
    },
    content = function(file) {
      ggsave(file, plot =         distributionvalues(toto = DATA()$LEARNING,prctvaluesselect =input$prctvalues/100,nvar = ncol(SELECTDATA()$LEARNINGSELECT) ,ggplot =  T), 
             device = input$paramdownplot)},contentType=NA)
  
  output$downloaddataplotNA <- downloadHandler( 
    filename = function() {
      paste('dataset', '.',input$paramdowntable, sep='') 
    },
    content = function(file) {
      downloaddataset(distributionvalues(toto = DATA()$LEARNING,prctvaluesselect =input$prctvalues/100,nvar = ncol(SELECTDATA()$LEARNINGSELECT) ,ggplot =  T,graph = F)  , file)
    }
  )
  
  # fonction d'affichage du nombre de variables selectionnees
  output$nstructuredfeatures <- renderText({
    ncol(SELECTDATA()$STRUCTUREDFEATURES)
  })
  
  # fonction d'affichage de la structure des NA
  output$heatmapNAstructure <- renderPlot({
    group<<-DATA()$LEARNING[,1]
    structuredfeatures<<-SELECTDATA()$STRUCTUREDFEATURES
    heatmapNA(toto=cbind(group,structuredfeatures))            
    #else{errorplot(text = " No NA's structure")}
  })
  
  # fonction d'export de la structure des NA
  output$downloadstructur = downloadHandler(
    filename = function() { 
      paste('graph','.',input$paramdownplot, sep='') 
    },
    content = function(file) {
      ggsave(file, plot = heatmapNA(cbind(DATA()$LEARNING[,1],SELECTDATA()$STRUCTUREDFEATURES)), 
             device = input$paramdownplot)
    },
    contentType=NA)
  
  output$downloaddatastructur <- downloadHandler( 
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(SELECTDATA()$STRUCTUREDFEATURES, file)
    }
  ) 
  
  #####
  
  # fonction d'affichage de la structure des NA 
  TRANSFORMDATA <- reactive({
    learningselect<<-SELECTDATA()$LEARNINGSELECT
    structuredfeatures<<-SELECTDATA()$STRUCTUREDFEATURES
    datastructuresfeatures<<-SELECTDATA()$DATASTRUCTUREDFEATURES
    transformdataparameters<<-list("log"=input$log,"logtype"=input$logtype,"standardization"=input$standardization,"arcsin"=input$arcsin,"rempNA"=input$rempNA)
    validate(need(ncol(learningselect)>0,"No select dataset"))
    if(transformdataparameters$rempNA%in%c("pca","missforest")){
      validate(need(min(apply(X = learningselect,MARGIN = 2,FUN = function(x){sum(!is.na(x))}))>1,"not enough data for pca estimation"))
    } 
    learningtransform <- transformdatafunction(learningselect = learningselect,structuredfeatures = structuredfeatures,
                                               datastructuresfeatures =   datastructuresfeatures,transformdataparameters = transformdataparameters)
    
    list(LEARNINGTRANSFORM = learningtransform,
         transformdataparameters = transformdataparameters)
  })
  
  ##
  output$downloaddatatransform<- downloadHandler(
    filename = function() { paste('Transformdata', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(TRANSFORMDATA()$LEARNINGTRANSFORM, file)
    }
  )
  
  output$plotheatmaptransformdata<-renderPlot({
    learningtransform<-TRANSFORMDATA()$LEARNINGTRANSFORM
    heatmapplot(toto =learningtransform,ggplot = T,scale=F)
  })
  
  output$downloadplotheatmap = downloadHandler(
    filename = function() { 
      paste0('graph','.',input$paramdownplot, sep='') 
    },
    content = function(file) {
      ggsave(file, plot =    heatmapplot(toto =TRANSFORMDATA()$LEARNINGTRANSFORM,ggplot = T,scale=F), 
             device = input$paramdownplot)},
    contentType=NA)
  
  output$downloaddataheatmap <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(as.data.frame(heatmapplot(toto =TRANSFORMDATA()$LEARNINGTRANSFORM,ggplot = T,scale=F,graph=F)), file)
    })
  
  output$plotmds<-renderPlot({
    learningtransform<-TRANSFORMDATA()$LEARNINGTRANSFORM
    mdsplot(toto = learningtransform,ggplot=T)
  })
  output$downloadplotmds = downloadHandler(
    filename = function() { 
      paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
      ggsave(file, plot =        mdsplot(toto = TRANSFORMDATA()$LEARNINGTRANSFORM,ggplot=T),  device = input$paramdownplot)},
    contentType=NA)
  
  output$downloaddatamds <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(    mdsplot(toto = TRANSFORMDATA()$LEARNINGTRANSFORM,ggplot=T,graph=F), file)
    })
  
  # fonction d'affichage de l'histogramme des variables 
  output$plothist<-renderPlot({
    learningtransform<-TRANSFORMDATA()$LEARNINGTRANSFORM
    histplot(toto=learningtransform)
  })
  
  # fonction d'export de l'histogramme des variables
  output$downloadplothist = downloadHandler(
    filename = function() { 
      paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
      ggsave(file, plot =         histplot(toto=TRANSFORMDATA()$LEARNINGTRANSFORM),  device = input$paramdownplot)},
    contentType=NA)
  
  output$downloaddatahist <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(histplot(toto=TRANSFORMDATA()$LEARNINGTRANSFORM,graph=F), file)
    })
  
  # Variables réactives pour stocker les fréquences bootstrap et stats complètes
  # Cache pour les fréquences (calcul coûteux)
  bootstrap_frequencies <- reactiveVal(NULL)
  bootstrap_stats_complete <- reactiveVal(NULL)
  bootstrap_frequencies_cache_key <- reactiveVal("")
  
  # Cache pour les résultats finaux (calcul rapide)
  bootstrap_results_cache <- reactiveVal(NULL)
  bootstrap_results_cache_key <- reactiveVal("")
  
  # Fonction pour générer une clé de cache
  # Clé de cache pour les fréquences (SANS les paramètres de seuil)
  generate_frequencies_cache_key <- function() {
    paste(
      "bootstrap_freq_",
      input$prctvalues, "_",
      input$selectmethod, "_",
      input$NAstructure, "_",
      input$structdata, "_",
      input$thresholdNAstructure, "_",
      input$maxvaluesgroupmin, "_",
      input$minvaluesgroupmax, "_",
      input$rempNA, "_",
      input$log, "_",
      input$logtype, "_",
      input$standardization, "_",
      input$arcsin, "_",
      input$bootstrap_test, "_",
      input$bootstrap_thresholdFCOOB, "_",
      input$bootstrap_thresholdpvOOB, "_",
      input$bootstrap_adjustpvOOB, "_",
      input$n_iterations, "_",
      input$sample_percentage
    )
  }
  
  # Clé de cache pour les résultats finaux (AVEC les paramètres de seuil)
  generate_results_cache_key <- function() {
    paste(
      "bootstrap_results_",
      input$prctvalues, "_",
      input$selectmethod, "_",
      input$NAstructure, "_",
      input$structdata, "_",
      input$thresholdNAstructure, "_",
      input$maxvaluesgroupmin, "_",
      input$minvaluesgroupmax, "_",
      input$rempNA, "_",
      input$log, "_",
      input$logtype, "_",
      input$standardization, "_",
      input$arcsin, "_",
      input$bootstrap_test, "_",
      input$bootstrap_thresholdFCOOB, "_",
      input$bootstrap_thresholdpvOOB, "_",
      input$bootstrap_adjustpvOOB, "_",
      input$n_iterations, "_",
      input$sample_percentage, "_",
      input$seuil_method, "_",
      input$stability_threshold
    )
  }
  
  # Fonction helper pour obtenir les bons paramètres selon la méthode
  # get_test_parameters <- function() {
  #   cat("get_test_parameters appelée - Mode :", TEST()$mode, "\n")
  #   
  #   if(TEST()$mode == "bootstrap") {
  #     cat("  Utilisation des paramètres bootstrap\n")
  #     list(
  #       thresholdFC = input$bootstrap_thresholdFCOOB,
  #       thresholdpv = input$bootstrap_thresholdpvOOB
  #     )
  #   } else {
  #     cat("  Utilisation des paramètres standard\n")
  #     list(
  #       thresholdFC = input$thresholdFC,
  #       thresholdpv = input$thresholdpv
  #     )
  #   }
  # }
  # 
  # Nettoyer les fréquences quand les données changent
  observeEvent(input$confirmdatabutton, {
    bootstrap_frequencies(NULL)
    bootstrap_stats_complete(NULL)
    bootstrap_frequencies_cache_key("")
    bootstrap_results_cache_key("")
    # Réinitialiser aussi les paramètres précédents
    previous_params(list())
    previous_bootstrap_params(list())
    # Reset du cache TEST
    test_result_cache(NULL)
    last_test_params(list())
    # cat(" Fréquences bootstrap réinitialisées (nouvelles données)\n")
  })
  
  # Nettoyer les fréquences quand les paramètres critiques changent
  observeEvent(c(input$rempNA, input$log, input$logtype, input$standardization, input$arcsin,
                 input$bootstrap_test, input$bootstrap_thresholdFCOOB, 
                 input$bootstrap_thresholdpvOOB, input$bootstrap_adjustpvOOB,
                 input$n_iterations, input$sample_percentage), {
                   # Obtenir les valeurs actuelles
                   current_params <- list(
                     rempNA = input$rempNA,
                     log = input$log,
                     logtype = input$logtype,
                     standardization = input$standardization,
                     arcsin = input$arcsin
                   )
                   
                   # Comparer avec les valeurs précédentes
                   prev_params <- previous_params()
                   
                   # Seulement réinitialiser si les paramètres ont vraiment changé (et pas au démarrage)
                   if (length(prev_params) > 0 && !identical(current_params, prev_params)) {
                     # cat(" Réinitialisation du cache - Paramètres modifiés :\n")
                     # cat("  rempNA:", input$rempNA, "\n")
                     # cat("  log:", input$log, "\n")
                     # cat("  logtype:", input$logtype, "\n")
                     # cat("  standardization:", input$standardization, "\n")
                     # cat("  arcsin:", input$arcsin, "\n")
                     # cat("  Stack trace pour debug :\n")
                     # cat("    ", paste(sys.calls(), collapse = " -> "), "\n")
                     bootstrap_frequencies(NULL)
                     bootstrap_stats_complete(NULL)
                     bootstrap_frequencies_cache_key("")
                     bootstrap_results_cache_key("")
                     # cat(" Fréquences bootstrap réinitialisées (paramètres modifiés)\n")
                   }
                   
                   # Mettre à jour les valeurs précédentes
                   previous_params(current_params)
                 })
  
  # Variables pour suivre les valeurs précédentes des paramètres
  previous_params <- reactiveVal(list())
  
  # Variables pour suivre les valeurs précédentes des paramètres bootstrap
  previous_bootstrap_params <- reactiveVal(list())
  
  # Nettoyer les fréquences quand les paramètres bootstrap changent (SANS les paramètres de seuil)
  observeEvent(c(input$bootstrap_test, input$bootstrap_thresholdFCOOB, 
                 input$bootstrap_thresholdpvOOB, input$bootstrap_adjustpvOOB,
                 input$n_iterations, input$sample_percentage), {
                   # Obtenir les valeurs actuelles (SANS les paramètres de seuil)
                   current_bootstrap_params <- list(
                     bootstrap_test = input$bootstrap_test,
                     bootstrap_thresholdFCOOB = input$bootstrap_thresholdFCOOB,
                     bootstrap_thresholdpvOOB = input$bootstrap_thresholdpvOOB,
                     bootstrap_adjustpvOOB = input$bootstrap_adjustpvOOB,
                     n_iterations = input$n_iterations,
                     sample_percentage = input$sample_percentage
                   )
                   
                   # Comparer avec les valeurs précédentes
                   prev_bootstrap_params <- previous_bootstrap_params()
                   
                   # Seulement réinitialiser si les paramètres ont vraiment changé (et pas au démarrage)
                   if (length(prev_bootstrap_params) > 0 && !identical(current_bootstrap_params, prev_bootstrap_params)) {
                     # cat(" Réinitialisation du cache - Paramètres bootstrap modifiés :\n")
                     # cat("  bootstrap_test:", input$bootstrap_test, "\n")
                     # cat("  bootstrap_thresholdFCOOB:", input$bootstrap_thresholdFCOOB, "\n")
                     # cat("  bootstrap_thresholdpvOOB:", input$bootstrap_thresholdpvOOB, "\n")
                     # cat("  bootstrap_adjustpvOOB:", input$bootstrap_adjustpvOOB, "\n")
                     # cat("  n_iterations:", input$n_iterations, "\n")
                     # cat("  sample_percentage:", input$sample_percentage, "\n")
                     bootstrap_frequencies(NULL)
                     bootstrap_stats_complete(NULL)
                     bootstrap_frequencies_cache_key("")
                     bootstrap_results_cache_key("")
                     # cat(" Fréquences bootstrap réinitialisées (paramètres bootstrap modifiés)\n")
                   }
                   
                   # Mettre à jour les valeurs précédentes
                   previous_bootstrap_params(current_bootstrap_params)
                 })
  
  # APPROCHE RADICALE : Pas de gestion spéciale des paramètres de seuil
  # Le reactive TEST() se déclenche automatiquement quand les paramètres changent
  observeEvent(c(input$seuil_method, input$stability_threshold), ignoreInit = TRUE, {
    # cat(" Paramètres de seuil modifiés\n")
    # cat("  seuil_method:", input$seuil_method, "\n")
    # cat("  stability_threshold:", input$stability_threshold, "\n")
    # cat("  Le reactive TEST() se déclenchera automatiquement\n")
  })
  
  # Nettoyer manuellement
  observeEvent(input$clear_cache, {
    bootstrap_frequencies(NULL)
    bootstrap_stats_complete(NULL)
    bootstrap_frequencies_cache_key("")
    bootstrap_results_cache_key("")
    # Réinitialiser aussi les paramètres précédents
    previous_params(list())
    previous_bootstrap_params(list())
    showNotification("Bootstrap frequencies reset!", type = "message")
  })
  
  # Debug des fréquences
  observeEvent(input$debug_cache, {
    if (is.null(bootstrap_frequencies())) {
      showNotification("No bootstrap frequency in cache", type = "message")
    } else {
      showNotification("Bootstrap frequencies available in cache", type = "message")
    }
  })
  
  # Variables pour optimiser la réactivité
  last_test_params <- reactiveVal(list())
  test_result_cache <- reactiveVal(NULL)
  force_test_update <- reactiveVal(0)  # Pour forcer le déclenchement du reactive TEST
  
  # APPROCHE RADICALE : Pas de cache, calcul direct
  # Les fréquences sont recalculées à chaque fois, mais c'est plus fiable
  BOOTSTRAP_FREQUENCIES <- reactive({
    req(input$stats_method == 'bootstrap')
    req(input$bootstrap_test != "notestOOB")
    req(input$n_iterations)
    req(input$sample_percentage)
    
    # cat(" BOOTSTRAP_FREQUENCIES() - Calcul direct (pas de cache)\n")
    
    # Calculer les fréquences à chaque fois
    testparameters <<- list("SFtest"=input$SFtest,
                            "test"=input$bootstrap_test,
                            "adjustpval"=input$bootstrap_adjustpvOOB,
                            "thresholdpv"=input$bootstrap_thresholdpvOOB,
                            "thresholdFC"=input$bootstrap_thresholdFCOOB,
                            "invers" = input$invers)
    learningtransform <<- TRANSFORMDATA()$LEARNINGTRANSFORM
    
    # Calculer les fréquences
    res <- determiner_modele_moyen(
      data = learningtransform,
      n_iterations = input$n_iterations,
      testparameters = testparameters,
      sample_frac = input$sample_percentage / 100,
      random_state = 0,
      session = session
    )
    
    # cat("Fréquences calculées :", length(res$stats_selection$peptide_frequencies), "variables\n")
    return(res$stats_selection$peptide_frequencies)
    })
  
  #### fonction pour faire le test statistique et la selection des variables
  TEST <- reactive({
    # Optimisation : ne se déclencher que si les paramètres critiques changent --> systeme cache
    req(input$stats_method, input$seuil_method)
    
    # Forcer le déclenchement quand les paramètres de seuil changent
    force_test_update()
    
    # Créer une clé unique pour les paramètres actuels
    current_params <- list(
      mode = input$stats_method,
      test = if(input$stats_method == 'standard') input$test else input$bootstrap_test,
      adjustpval = if(input$stats_method == 'standard') input$adjustpv else input$bootstrap_adjustpvOOB,
      thresholdpv = if(input$stats_method == 'standard') input$thresholdpv else input$bootstrap_thresholdpvOOB,
      thresholdFC = if(input$stats_method == 'standard') input$thresholdFC else input$bootstrap_thresholdFCOOB
    )
    
    # Ajouter les paramètres de seuil pour le mode bootstrap
    if(input$stats_method == 'bootstrap') {
      current_params$seuil_method <- input$seuil_method
      current_params$stability_threshold <- input$stability_threshold
    }
    
    # Vérifier si les paramètres ont changé
    last_params <- last_test_params()
    #cat(" Debug TEST() - Comparaison des paramètres :\n")
    # cat("  Paramètres actuels :", paste(names(current_params), "=", unlist(current_params), collapse = ", "), "\n")
    # cat("  Paramètres précédents :", paste(names(last_params), "=", unlist(last_params), collapse = ", "), "\n")
    # cat("  Paramètres identiques :", identical(current_params, last_params), "\n")
    # cat("  Cache disponible :", !is.null(test_result_cache()), "\n")
    
    if (identical(current_params, last_params) && !is.null(test_result_cache())) {
      # cat("Utilisation du cache TEST - paramètres identiques\n")
      return(test_result_cache())
    }
    
    # Debug détaillé pour comprendre pourquoi le reactive se déclenche
    # cat(" TEST() reactive déclenché - Mode :", input$stats_method, " - ", Sys.time(), "\n")
    # cat("  Paramètres changés :", !identical(current_params, last_params), "\n")
    # cat("  Force update counter :", force_test_update(), "\n")
    if(input$stats_method == 'bootstrap') {
      # cat("  Méthode de seuil :", input$seuil_method, "\n")
      # cat("  Seuil de stabilité :", input$stability_threshold, "\n")
      # cat("  Fréquences en cache :", !is.null(bootstrap_frequencies()), "\n")
      # cat("  Raison du déclenchement :\n")
      if (!identical(current_params, last_params)) {
        #cat("    - Paramètres critiques modifiés\n")
      } else {
        #cat("    - Cache vide ou invalide\n")
      }
    }
    
    if(input$stats_method == 'standard'){
      # req(TRANSFORMDATA()$LEARNINGTRANSFORM)
      testparameters <<- list("SFtest"=input$SFtest,
                              "test"=input$test,
                              "adjustpval"=input$adjustpv,
                              "thresholdpv"=input$thresholdpv,
                              "thresholdFC"=input$thresholdFC,
                              "invers"=input$invers)
      learningtransform <<- TRANSFORMDATA()$LEARNINGTRANSFORM
      restest <<- testfunction(tabtransform = learningtransform,
                               testparameters = testparameters )
      validate(need(testparameters$thresholdFC >= 0, "threshold Foldchange has to be positive"))
      validate(need(testparameters$thresholdpv >= 0 & testparameters$thresholdpv <=1 ,"p-value has to be between 0 and 1"))
      
      # Debug pour vérifier l'ajustement de p-value
      # cat(" Debug ajustement p-value (standard) :\n")
      # cat("  Ajustement activé :", testparameters$adjustpval, "\n")
      # cat("  Nombre de variables :", if(!is.null(restest$useddata)) nrow(restest$useddata) else 0, "\n")
      if(!is.null(restest$useddata) && nrow(restest$useddata) > 0) {
        # cat("  P-values min :", min(restest$useddata$pval), "\n")
        # cat("  P-values max :", max(restest$useddata$pval), "\n")
        # cat("  Variables significatives (p < 0.05) :", sum(restest$useddata$pval < 0.05), "\n")
      } else {
        # cat("  Aucune donnée disponible\n")
      }
      
      result <- list(mode = "standard",
           LEARNINGDIFF = restest$tabdiff,
           DATATEST = restest$datatest,
           HYPOTHESISTEST = restest$hypothesistest, #GROUP=restest$group,
           USEDDATA = restest$useddata,
           testparameters = restest$testparameters)
      
      # Mettre en cache le résultat
      test_result_cache(result)
      last_test_params(current_params)
      
      result
    }else if (input$stats_method == 'bootstrap' & input$bootstrap_test != "notestOOB"){
      # cat(" DÉBUT BOOTSTRAP - ", Sys.time(), "\n")
      # cat("faire les test pour la méthode Bootstrap \n ")
      
      # Générer les clés de cache
      current_freq_cache_key <- generate_frequencies_cache_key()
      current_results_cache_key <- generate_results_cache_key()
      # 
      # cat(" Clé de cache fréquences :", substr(current_freq_cache_key, 1, 100), "...\n")
      # cat(" Clé de cache résultats :", substr(current_results_cache_key, 1, 100), "...\n")
      # cat(" Fréquences en cache :", !is.null(bootstrap_frequencies()) && bootstrap_frequencies_cache_key() == current_freq_cache_key, "\n")
      # cat(" Résultats en cache :", !is.null(bootstrap_results_cache()) && bootstrap_results_cache_key() == current_results_cache_key, "\n")
      
      # LOGIQUE SIMPLIFIÉE : Calcul direct des fréquences
      # cat(" Calcul des fréquences via BOOTSTRAP_FREQUENCIES()\n")
      
      # Calculer les fréquences
      freq <- BOOTSTRAP_FREQUENCIES()
      
      # cat("Fréquences calculées :", length(freq), "variables\n")
      # cat(" Méthode de seuil actuelle :", input$seuil_method, "\n")
      
      # Appliquer la méthode de seuil actuelle
      if (input$seuil_method == "elbow") {
        seuil <- find_elbow_point(freq)[1]
        selected_vars <- names(freq)[freq >= seuil]
      } else if (input$seuil_method == "gmm") {
        seuil <- find_gmm_threshold(freq)
        selected_vars <- names(freq)[freq >= seuil]
      } else if (input$seuil_method == "inflection") {
        seuil <- find_inflection_point(freq)
        selected_vars <- names(freq)[freq >= seuil]
      } else if (input$seuil_method == "intervalle_confiance") {
        # Pour l'intervalle de confiance, on utilise une approche simplifiée
        # On prend les 10% et 90% percentiles
        lower <- quantile(freq, 0.1)
        upper <- quantile(freq, 0.9)
        selected_vars <- names(freq)[freq >= lower & freq <= upper]
        seuil <- c(lower, upper)
        # cat("Intervalle de confiance calculé (percentiles 10-90)\n")
      } else if (input$seuil_method == "manual") {
        seuil <- input$stability_threshold
        selected_vars <- names(freq)[freq >= seuil]
      }
      
      if (!is.null(freq)) {
        # Construire les résultats avec les fréquences en cache
      testparameters <<- list("SFtest"=input$SFtest,
                              "test"=input$bootstrap_test,
                              "adjustpval"=input$bootstrap_adjustpvOOB,
                              "thresholdpv"=input$bootstrap_thresholdpvOOB,
                              "thresholdFC"=input$bootstrap_thresholdFCOOB,
                              "invers" = input$invers)
      learningtransform <<- TRANSFORMDATA()$LEARNINGTRANSFORM
        
        if (length(selected_vars) > 1) {
          indvar <- (colnames(learningtransform) %in% selected_vars)
          indvar[1] <- TRUE
          tabdiff <- learningtransform[, indvar, drop = FALSE]
          datatest <- diffexptestOOB(tabdiff, test = testparameters$test)
          logFC <- datatest[, 5]
          if (testparameters$adjustpval) { pval <- datatest[, 3] }
          else { pval <- datatest[, 2] }
          useddata <- data.frame(
            "names" = datatest[, 1],
            "pval" = pval,
            "logFC" = datatest[, 5],
            "mean1" = datatest[, 9],
            "mean2" = datatest[, 10]
          )
        } else {
          tabdiff <- data.frame()
          datatest <- data.frame()
          useddata <- data.frame()
        }
        
        restest <- list(
          tabdiff = tabdiff,
          datatest = datatest,
          hypothesistest = data.frame(),
          useddata = useddata,
                               testparameters = testparameters, 
          VARIABLES_FREQ = freq,
          ALL_SELECTED_VARS = list(),  # Simplifié
          SELECTED_VARS = selected_vars,
          SEUIL = seuil,
          stats_selection = list()  # Simplifié
        )
        
        showNotification(paste("Frequencies retrieved from cache, threshold applied :", input$seuil_method),
                         type = "default", 
                         duration = 3)
      }
      
      # Debug : vérifier que restest est défini
      if (!exists("restest") || is.null(restest)) {
        # cat("ERREUR : restest n'est pas défini !\n")
        # cat("Cache key actuelle :", current_cache_key, "\n")
        # cat("Cache key stockée :", bootstrap_cache_key(), "\n")
        # cat("Fréquences en cache :", !is.null(bootstrap_frequencies()), "\n")
        return(NULL)
      }
      
      validate(need(testparameters$thresholdFC >= 0, "threshold Foldchange has to be positive"))
      validate(need(testparameters$thresholdpv >= 0 & testparameters$thresholdpv <=1 ,"p-value has to be between 0 and 1"))
      
      result <- list(
        mode = "bootstrap",
        completed = TRUE,
        n_iterations = input$n_iterations, # nombre d'iterations
        LEARNINGDIFF = restest$tabdiff, # tableau des differents variables
        DATATEST = restest$datatest,  # tableau des resultats du test
        HYPOTHESISTEST = restest$hypothesistest, # tableau des resultats du test
        USEDDATA = restest$useddata, # tableau des resultats du test
        testparameters = restest$testparameters, # parametres du test
        VARIABLES_FREQ = restest$VARIABLES_FREQ, # tableau des frequences des variables
        SELECTED_VARS = restest$SELECTED_VARS, # tableau des variables selectionnees
        SEUIL = restest$SEUIL, # seuil de selection
        stats_selection = restest$stats_selection # tableau des statistiques de selection
      )
      
      # Mettre en cache le résultat
      test_result_cache(result)
      last_test_params(current_params)
      
      result
    }
  })
  
  # SUPPRESSION DE L'OBSERVE PROBLÉMATIQUE
  # L'observe causait une boucle infinie de réactivité
  # Les résultats bootstrap sont maintenant affichés directement dans le reactive TEST
  
  
  output$downloadddatadiff<- downloadHandler(
    filename = function() { paste('Datadiff', '.', input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(TEST()$LEARNINGDIFF, file)
    }
  )
  output$downloaddatastatistics<- downloadHandler(
    filename = function() { paste('Datastatistics', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(TEST()$DATATEST, file)
    }
  )
  output$positif<-renderText({
    res<-levels(DATA()$LEARNING[,1])[1]
  })
  output$negatif<-renderText({
    res<-levels(DATA()$LEARNING[,1])[2]
  })
  
  
  # Variable réactive pour stocker le volcano plot standard
  volcano_plot_standard_obj <- reactive({
    req(TEST()$USEDDATA)
    req(TEST()$DATATEST)
    req(nrow(TEST()$USEDDATA) > 0)
    req(TEST()$mode == "standard")
    
    tryCatch({
      useddata <- TEST()$USEDDATA
      datatest <- TEST()$DATATEST
      
      # En mode standard, vérifier la structure
      if(ncol(useddata) >= 3 && ncol(datatest) >= 5) {
        colnames(useddata)[3] <- colnames(datatest)[5]
      }
      
      # Utiliser les paramètres standard
      thresholdFC <- input$thresholdFC
      thresholdpv <- input$thresholdpv
      
      volcanoplot(logFC = useddata[,3],
                  pval = useddata$pval,
                  thresholdFC = thresholdFC,
                  thresholdpv = thresholdpv,
                  completedata = useddata[,1:3])
    }, error = function(e) {
      errorplot(text = paste("Erreur volcano plot standard :", e$message))
    })
  })
  
  # Output pour l'analyse standard
  output$volcanoplot_standard <- renderPlot({
    req(TEST()$DATATEST)
    req(TEST()$USEDDATA)
    req(TEST()$mode == "standard")
    
    # Debug pour voir la structure des données
    # cat(" Debug volcanoplot_standard :\n")
    # cat("  Mode :", TEST()$mode, "\n")
    # cat("  Nrow USEDDATA :", nrow(TEST()$USEDDATA), "\n")
    # cat("  Ncol USEDDATA :", ncol(TEST()$USEDDATA), "\n")
    # cat("  Nrow DATATEST :", nrow(TEST()$DATATEST), "\n")
    # cat("  Ncol DATATEST :", ncol(TEST()$DATATEST), "\n")
    # cat("  Colonnes USEDDATA :", paste(colnames(TEST()$USEDDATA), collapse = ", "), "\n")
    # cat("  Colonnes DATATEST :", paste(colnames(TEST()$DATATEST), collapse = ", "), "\n")
    
    if(nrow(TEST()$USEDDATA) == 0){
      errorplot(text = "No differentially expressed variables")
    }else{
    datatest<<-TEST()$DATATEST
    useddata<<-TEST()$USEDDATA
      
      # En mode standard, vérifier la structure
      if(ncol(useddata) >= 3 && ncol(datatest) >= 5) {
    colnames(useddata)[3]<-colnames(datatest)[5]
      } else {
        cat(" Structure de données inattendue en mode standard\n")
        return(errorplot(text = "Structure de données inattendue"))
      }
      
      # Utiliser les paramètres standard
      thresholdFC <- input$thresholdFC
      thresholdpv <- input$thresholdpv
      
      # cat("  Paramètres utilisés - thresholdFC:", thresholdFC, "thresholdpv:", thresholdpv, "\n")
      
      tryCatch({
        plot_obj <- volcanoplot(logFC = useddata[,3],
                                pval = useddata$pval,
                                thresholdFC = thresholdFC,
                                thresholdpv = thresholdpv,
                                completedata = useddata[,1:3]
        )
        print(plot_obj)  # Afficher le plot
      }, error = function(e) {
        # cat("Erreur dans volcanoplot_standard :", e$message, "\n")
        errorplot(text = paste("Erreur volcanoplot :", e$message))
      })
    }
  })
  
  # Variable réactive pour stocker le volcano plot bootstrap
  volcano_plot_bootstrap_obj <- reactive({
    req(TEST()$USEDDATA)
    req(TEST()$DATATEST)
    req(nrow(TEST()$USEDDATA) > 0)
    req(TEST()$mode == "bootstrap")
    
    tryCatch({
      useddata <- TEST()$USEDDATA
      datatest <- TEST()$DATATEST
      
      # Gérer les colonnes pour bootstrap
      colnames(useddata)[3] <- colnames(datatest)[5]
      
      # Utiliser les paramètres bootstrap
      thresholdFC <- input$bootstrap_thresholdFCOOB
      thresholdpv <- input$bootstrap_thresholdpvOOB
      
      volcanoplot(logFC = useddata[,3],
                  pval = useddata$pval,
                  thresholdFC = thresholdFC,
                  thresholdpv = thresholdpv,
                  completedata = useddata[,1:3])
    }, error = function(e) {
      errorplot(text = paste("Erreur volcano plot bootstrap :", e$message))
    })
  })
  
  # Output pour l'analyse bootstrap
  output$volcanoplot_bootstrap <- renderPlot({
    # cat(" volcanoplot_bootstrap déclenché - ", Sys.time(), "\n")
    req(TEST()$DATATEST)
    req(TEST()$USEDDATA)
    req(TEST()$mode == "bootstrap")
    
    # Debug pour voir la structure des données
    # cat(" Debug volcanoplot_bootstrap :\n")
    # cat("  Mode :", TEST()$mode, "\n")
    # cat("  Nrow USEDDATA :", nrow(TEST()$USEDDATA), "\n")
    # cat("  Ncol USEDDATA :", ncol(TEST()$USEDDATA), "\n")
    # cat("  Nrow DATATEST :", nrow(TEST()$DATATEST), "\n")
    # cat("  Ncol DATATEST :", ncol(TEST()$DATATEST), "\n")
    # cat("  Colonnes USEDDATA :", paste(colnames(TEST()$USEDDATA), collapse = ", "), "\n")
    # cat("  Colonnes DATATEST :", paste(colnames(TEST()$DATATEST), collapse = ", "), "\n")
    
    if(nrow(TEST()$USEDDATA) == 0){
      errorplot(text = "No differentially expressed variables")
    }else{
      datatest<<-TEST()$DATATEST
      useddata<<-TEST()$USEDDATA
      
      # Gérer les colonnes pour bootstrap
      colnames(useddata)[3]<-colnames(datatest)[5]
      
      # Utiliser les paramètres bootstrap
      thresholdFC <- input$bootstrap_thresholdFCOOB
      thresholdpv <- input$bootstrap_thresholdpvOOB
      
      # cat("  Paramètres utilisés - thresholdFC:", thresholdFC, "thresholdpv:", thresholdpv, "\n")
      
      # Debug des données avant l'appel à volcanoplot
      # cat("  Données pour volcanoplot_bootstrap :\n")
      # cat("    logFC (colonne 3) :", head(useddata[,3]), "...\n")
      # cat("    pval :", head(useddata$pval), "...\n")
      # cat("    completedata :", head(useddata[,1:3]), "...\n")
      
      tryCatch({
        plot_obj <- volcanoplot(logFC = useddata[,3],
                                pval = useddata$pval,
                                thresholdFC = thresholdFC,
                                thresholdpv = thresholdpv,
                                completedata = useddata[,1:3]
        )
        # cat("  volcanoplot_bootstrap exécuté avec succès\n")
        # cat("   Type de l'objet plot :", class(plot_obj), "\n")
        # cat("   Dimensions de l'objet plot :", if(is.null(dim(plot_obj))) "NULL" else dim(plot_obj), "\n")
        print(plot_obj)  # Afficher le plot
      }, error = function(e) {
        # cat(" Erreur dans volcanoplot_bootstrap :", e$message, "\n")
        errorplot(text = paste("Erreur volcanoplot :", e$message))
      })
    }
  })
  
  # Output pour téléchargement volcanoplot standard
  output$downloadvolcanoplot_standard = downloadHandler(
    filename = function() {paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
      req(TEST()$USEDDATA$logFC, TEST()$USEDDATA$pval, volcano_plot_standard_obj())
      # Utiliser les paramètres standard
      thresholdFC <- input$thresholdFC
      thresholdpv <- input$thresholdpv
      
      ggsave(file, plot =  volcano_plot_standard_obj(),
             # plot = volcanoplot(logFC = TEST()$USEDDATA$logFC,
             #                          pval = TEST()$USEDDATA$pval,
             #                          thresholdFC = thresholdFC,
             #                          thresholdpv = thresholdpv ,
             #                          completedata=TEST()$DATATEST ) ,  
             device = input$paramdownplot)},
    contentType=NA
  )
  
  # Output pour téléchargement données volcanoplot standard
  output$downloaddatavolcanoplot_standard<- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      # Utiliser les paramètres standard
      thresholdFC <- input$thresholdFC
      thresholdpv <- input$thresholdpv
      
      downloaddataset(volcanoplot(logFC =TEST()$USEDDATA$logFC,pval = TEST()$USEDDATA$pval,thresholdFC = thresholdFC,
                                  thresholdpv = thresholdpv ,completedata=TEST()$DATATEST,graph=F ), file) }
  )
  
  # Output pour téléchargement volcanoplot bootstrap
  output$downloadvolcanoplot_bootstrap = downloadHandler(
    filename = function() {paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
      req(volcano_plot_bootstrap_obj())
      # Utiliser les paramètres bootstrap
      thresholdFC <- input$bootstrap_thresholdFCOOB
      thresholdpv <- input$bootstrap_thresholdpvOOB
      
      ggsave(file, plot = volcano_plot_bootstrap_obj(), 
             # plot = volcanoplot(logFC =TEST()$USEDDATA$logFC,
             #                          pval = TEST()$USEDDATA$pval,
             #                          thresholdFC = thresholdFC,
             #                          thresholdpv = thresholdpv ,
             #                          completedata=TEST()$DATATEST ) , 
             device = input$paramdownplot)},
    contentType=NA
  )
  
  # Output pour téléchargement données volcanoplot bootstrap
  output$downloaddatavolcanoplot_bootstrap<- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      # les paramètres bootstrap
      thresholdFC <- input$bootstrap_thresholdFCOOB
      thresholdpv <- input$bootstrap_thresholdpvOOB
      
      downloaddataset(volcanoplot(logFC = TEST()$USEDDATA$logFC,
                                  pval = TEST()$USEDDATA$pval,
                                  thresholdFC = thresholdFC,
                                  thresholdpv = thresholdpv ,
                                  completedata = TEST()$DATATEST,graph=F ), 
                      file) }
  )
  
  output$nvarselect2<-renderText({
    di1<-dim(x = SELECTDATA()$LEARNINGSELECT)[2]-1  
  })  
  
  output$nvarselect2OOB<-renderText({
    di1<-dim(x = SELECTDATA()$LEARNINGSELECT)[2]-1  
  })
  
  
  output$nbdiff <- renderText({
    req(TEST()$mode == "standard")
    tryCatch({
      req(TEST()$LEARNINGDIFF)
    nbdiff = positive(ncol(TEST()$LEARNINGDIFF)-1)
      
      # Debug pour vérifier l'impact de l'ajustement
      # cat(" Debug nbdiff :\n")
      # cat("  Nombre de variables dans LEARNINGDIFF :", ncol(TEST()$LEARNINGDIFF)-1, "\n")
      # cat("  Ajustement p-value activé :", TEST()$testparameters$adjustpval, "\n")
      # cat("  Seuil p-value :", TEST()$testparameters$thresholdpv, "\n")
      # cat("  Seuil FC :", TEST()$testparameters$thresholdFC, "\n")
      
      return(nbdiff)
    }, error = function(e) {
      # cat("Erreur dans nbdiff :", e$message, "\n")
      return(0) 
    })
  })
  
  output$nbdiffOOB <- renderText({
    tryCatch({
      req(TEST()$LEARNINGDIFF)
      req(TEST()$mode == "bootstrap")
      nbdiffOOB = positive(ncol(TEST()$LEARNINGDIFF)-1)
    }, error = function(e) {
      # cat("Erreur dans nbdiffOOB :", e$message, "\n")
      return(0) 
    })
    
  })
  
  barplottest_standard_obj =  reactive({
    req(TEST()$LEARNINGDIFF)
    req(TEST()$USEDDATA)
    req(TEST()$mode == "standard")
    
    learningdiff<<-TEST()$LEARNINGDIFF
    useddata<<-TEST()$USEDDATA
    if(nrow(learningdiff)!=0){
      tryCatch({
        plot_obj <- barplottest(feature = useddata$names,
                  logFC=useddata$logFC,
                  levels=levels(learningdiff[,1]),
                  pval=useddata$pval,
                  mean1=useddata$mean1,
                  mean2=useddata$mean2,
                  thresholdpv=input$thresholdpv,
                  thresholdFC=input$thresholdFC,
                  graph=T,maintitle="Mean by group for differentially expressed variables")
        
        print(plot_obj)  # Afficher le plot
      }, error = function(e) {
        # cat(" Erreur dans barplottest_standard :", e$message, "\n")
        errorplot(text = paste("Erreur barplottest :", e$message))
      })
    }
    else{errorplot(text = "No differently expressed ")}
    
  })
  
  # fonction d'affichage du barplot des differents variables
  # Output pour l'analyse standard
  output$barplottest_standard <- renderPlot({
    req(TEST()$LEARNINGDIFF)
    req(TEST()$USEDDATA)
    req(TEST()$mode == "standard")
    
    learningdiff<<-TEST()$LEARNINGDIFF
    useddata<<-TEST()$USEDDATA
    if(nrow(learningdiff)!=0){
      # Utiliser les paramètres standard
      thresholdFC <- input$thresholdFC
      thresholdpv <- input$thresholdpv
      
      # cat("  Paramètres utilisés - thresholdFC:", thresholdFC, "thresholdpv:", thresholdpv, "\n")
      tryCatch({
        plot_obj <- barplottest(feature=useddata$names,
                                logFC=useddata$logFC,
                                levels=levels(learningdiff[,1]),
                                pval=useddata$pval,
                                mean1=useddata$mean1,
                                mean2=useddata$mean2,
                                thresholdpv=thresholdpv,
                                thresholdFC=thresholdFC,
                                graph=T,maintitle="Mean by group for differentially expressed variables")
        
        print(plot_obj)  # Afficher le plot
      }, error = function(e) {
        # cat("Erreur dans barplottest_standard :", e$message, "\n")
        errorplot(text = paste("Erreur barplottest :", e$message))
      })
    }
    else{errorplot(text = "No differently expressed ")}
    
  })
  
  # Output pour l'analyse bootstrap
  output$barplottest_bootstrap <- renderPlot({
    cat(" barplottest_bootstrap déclenché - ", Sys.time(), "\n")
    req(TEST()$LEARNINGDIFF)
    req(TEST()$USEDDATA)
    req(TEST()$mode == "bootstrap")
    
    # Debug pour voir la structure des données
    # cat(" Debug barplottest_bootstrap :\n")
    # cat("  Mode :", TEST()$mode, "\n")
    # cat("  Nrow LEARNINGDIFF :", nrow(TEST()$LEARNINGDIFF), "\n")
    # cat("  Ncol LEARNINGDIFF :", ncol(TEST()$LEARNINGDIFF), "\n")
    # cat("  Nrow USEDDATA :", nrow(TEST()$USEDDATA), "\n")
    # cat("  Ncol USEDDATA :", ncol(TEST()$USEDDATA), "\n")
    
    learningdiff<<-TEST()$LEARNINGDIFF
    useddata<<-TEST()$USEDDATA
    if(nrow(learningdiff)!=0){
      # Utiliser les paramètres bootstrap
      thresholdFC <- input$bootstrap_thresholdFCOOB
      thresholdpv <- input$bootstrap_thresholdpvOOB
      
      # cat("  Paramètres utilisés - thresholdFC:", thresholdFC, "thresholdpv:", thresholdpv, "\n")
      
      # Debug des données avant l'appel à barplottest
      # cat("  Données pour barplottest_bootstrap :\n")
      # cat("    feature :", head(useddata$names), "...\n")
      # cat("    logFC :", head(useddata$logFC), "...\n")
      # cat("    levels :", levels(learningdiff[,1]), "\n")
      # cat("    pval :", head(useddata$pval), "...\n")
      # cat("    mean1 :", head(useddata$mean1), "...\n")
      # cat("    mean2 :", head(useddata$mean2), "...\n")
      # 
      tryCatch({
        plot_obj <- barplottest(feature=useddata$names,
                                logFC=useddata$logFC,
                                levels=levels(learningdiff[,1]),
                                pval=useddata$pval,
                                mean1=useddata$mean1,
                                mean2=useddata$mean2,
                                thresholdpv=thresholdpv,
                                thresholdFC=thresholdFC,
                                graph=T,maintitle="Mean by group for differentially expressed variables")
        # cat("  barplottest_bootstrap exécuté avec succès\n")
        # cat("  Type de l'objet plot :", class(plot_obj), "\n")
        # cat(" Dimensions de l'objet plot :", if(is.null(dim(plot_obj))) "NULL" else dim(plot_obj), "\n")
        print(plot_obj)  # Afficher le plot
      }, error = function(e) {
        #cat(" Erreur dans barplottest_bootstrap :", e$message, "\n")
        errorplot(text = paste("Erreur barplottest :", e$message))
      })
    }
    else{errorplot(text = "No differently expressed ")}
    
  })
  
  
  # Output pour téléchargement barplottest standard
  output$downloadbarplottest_standard = downloadHandler(
    filename = function() {paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
      req(barplottest_standard_obj())
      # Utiliser les bons paramètres selon la méthode
      # params <- get_test_parameters()
      # thresholdFC <- params$thresholdFC
      # thresholdpv <- params$thresholdpv
      
      ggsave(file, plot =  barplottest_standard_obj(), 
             # plot = barplottest(feature=TEST()$USEDDATA$names,
             #                          logFC=TEST()$USEDDATA$logFC,
             #                          levels=levels(TEST()$LEARNINGDIFF[,1]),
             #                          pval=TEST()$USEDDATA$pval,
             #                          mean1=TEST()$USEDDATA$mean1,
             #                          mean2=TEST()$USEDDATA$mean2,
             #                          thresholdpv=thresholdpv,
             #                          thresholdFC=thresholdFC,
             #                          graph=T,maintitle="Mean by group for differentially expressed variables"),  
             device = input$paramdownplot)},
    contentType=NA)
  
  
  # Output pour téléchargement données barplottest standard
  output$downloaddatabarplottest_standard <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      # Utiliser les paramètres standard
      thresholdFC <- input$thresholdFC
      thresholdpv <- input$thresholdpv
      
      downloaddataset(barplottest(feature=TEST()$USEDDATA$names,logFC=TEST()$USEDDATA$logFC,levels=levels(TEST()$LEARNINGDIFF[,1]),pval=TEST()$USEDDATA$pval,mean1=TEST()$USEDDATA$mean1,mean2=TEST()$USEDDATA$mean2,thresholdpv=thresholdpv,
                                  thresholdFC=thresholdFC,maintitle="Mean by group for differentially expressed variables",graph=F), file) }
  )
  
  # Output pour téléchargement barplottest bootstrap
  output$downloadbarplottest_bootstrap = downloadHandler(
    filename = function() {paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
      # Utiliser les paramètres bootstrap
      thresholdFC <- input$bootstrap_thresholdFCOOB
      thresholdpv <- input$bootstrap_thresholdpvOOB
      
      ggsave(file, plot = barplottest(feature=TEST()$USEDDATA$names,logFC=TEST()$USEDDATA$logFC,levels=levels(TEST()$LEARNINGDIFF[,1]),pval=TEST()$USEDDATA$pval,mean1=TEST()$USEDDATA$mean1,mean2=TEST()$USEDDATA$mean2,thresholdpv=thresholdpv,
                                      thresholdFC=thresholdFC,graph=T,maintitle="Mean by group for differentially expressed variables"),  device = input$paramdownplot)},
    contentType=NA)
  
  # Output pour téléchargement données barplottest bootstrap
  output$downloaddatabarplottest_bootstrap <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      # Utiliser les paramètres bootstrap
      thresholdFC <- input$bootstrap_thresholdFCOOB
      thresholdpv <- input$bootstrap_thresholdpvOOB
      
      downloaddataset(barplottest(feature=TEST()$USEDDATA$names,logFC=TEST()$USEDDATA$logFC,levels=levels(TEST()$LEARNINGDIFF[,1]),pval=TEST()$USEDDATA$pval,mean1=TEST()$USEDDATA$mean1,mean2=TEST()$USEDDATA$mean2,thresholdpv=thresholdpv,
                                  thresholdFC=thresholdFC,maintitle="Mean by group for differentially expressed variables",graph=F), file) }
  )
  
  # output$dataconditiontest=renderDataTable({
  #   hypothesistest<-TEST()$hypothesistest},options = list("orderClasses" = F, 
  #                                                         "responsive" = F,
  #                                                         "pageLength" = 10))
  output$plottestSF = renderPlot({
    req(TEST()$HYPOTHESISTEST)
    hypothesistest<-TEST()$HYPOTHESISTEST 
    req(TEST()$mode == "standard")
    print(hypothesistest)
    
    # Vérifier si hypothesistest est vide
    if (nrow(hypothesistest) == 0) {
      # Créer un plot vide avec un message
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Aucun test d'hypothèse disponible\n(les tests Shapiro et Fisher sont maintenant activés par défaut)", 
           cex = 1.2, col = "red")
      return()
    }
    
    #validate(need(nrow(hypothesistest)>0,"No features selected"))
    barplottestSF(hypothesistest)
  })
  output$downloadplottestSF = downloadHandler(
    filename = function() {paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
      req(TEST()$HYPOTHESISTEST)
      hypothesistest <- TEST()$HYPOTHESISTEST
      if (nrow(hypothesistest) == 0) {
        # Créer un plot vide avec un message
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Aucun test d'hypothèse disponible\n(les tests Shapiro et Fisher sont maintenant activés par défaut)", 
                   size = 5, color = "red") +
          theme_void()
        ggsave(file, plot = p, device = input$paramdownplot)
      } else {
        ggsave(file, plot = barplottestSF(hypothesistest), device = input$paramdownplot)
      }
    },
    contentType=NA)
  output$downloaddatatestSF <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      req(TEST()$HYPOTHESISTEST)
      hypothesistest <- TEST()$HYPOTHESISTEST
      if (nrow(hypothesistest) == 0) {
        # Créer un data frame vide avec un message
        empty_df <- data.frame(
          Message = "Aucun test d'hypothèse disponible (les tests Shapiro et Fisher sont maintenant activés par défaut)"
        )
        downloaddataset(empty_df, file)
      } else {
        downloaddataset(barplottestSF(hypothesistest, graph=F), file)
      }
    })
  
  
  
  ######
  MODEL<-reactive({
    # Dépendance explicite à TEST() pour assurer la réactivité
    TEST()
    req(input$thresholdmodel)
    
    # Correction de la condition pour gérer correctement les modes standard et bootstrap
    # cat(" MODEL() reactive déclenché - Mode :", input$stats_method, "\n")
    
    if(input$stats_method == 'standard' && input$test == "notest"){
      learningmodel<<-TRANSFORMDATA()$LEARNINGTRANSFORM
      # cat("Utilisation des données transformées (mode standard, no test)\n")
    } else {
      learningmodel<<-TEST()$LEARNINGDIFF
      # cat("Utilisation des données de TEST() - Nombre de variables :", ncol(learningmodel)-1, "\n")
    }
    validation<<-DATA()$VALIDATION
    datastructuresfeatures<<-SELECTDATA()$DATASTRUCTUREDFEATURES
    transformdataparameters<<-TRANSFORMDATA()$transformdataparameters
    learningselect<-SELECTDATA()$LEARNINGSELECT
    modelparameters<<-list("modeltype"=input$model,"invers"=F,"thresholdmodel"=input$thresholdmodel,
                           "fs"=input$fs,"adjustval"=input$adjustval)
    print(ncol(learningmodel))
    validate(need(ncol(learningmodel)>1,"Not enough features"))
    
    
    resmodel<<-modelfunction(learningmodel = learningmodel,
                             validation = validation,
                             modelparameters = modelparameters,
                             transformdataparameters = transformdataparameters,
                             datastructuresfeatures =  datastructuresfeatures,
                             learningselect = learningselect)
    
    list("DATALEARNINGMODEL" = resmodel$datalearningmodel,
         "MODEL"=resmodel$model,
         "DATAVALIDATIONMODEL"= resmodel$datavalidationmodel,
         "GROUPS"=resmodel$groups,
         "modelparameters" = resmodel$modelparameters)
    
  })
  
  observe({
    if (input$model=="svm") { updateNumericInput(session, "thresholdmodel", value = 0)} 
    else if (input$model=="randomforest"){  updateNumericInput(session, "thresholdmodel", value = 0.5)}
  })
  
  ####
  output$downloaddatalearning <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   MODEL()$DATALEARNINGMODEL$learningmodel, file) })
  
  
  output$plotmodeldecouvroc <- renderPlot({
    datalearningmodel<<-MODEL()$DATALEARNINGMODEL
    ROCcurve(validation = datalearningmodel$reslearningmodel$classlearning,decisionvalues =  datalearningmodel$reslearningmodel$scorelearning)
  })
  output$youndendecouv<-renderTable({
    datalearningmodel<<-MODEL()$DATALEARNINGMODEL
    resyounden<-younden(datalearningmodel$reslearningmodel$classlearning, 
                        datalearningmodel$reslearningmodel$scorelearning)
    resyounden<-data.frame(resyounden)
    colnames(resyounden)<-c("")
    rownames(resyounden)<-c("younden","sensibility younden","specificity younden","threshold younden")
    
    resyounden
  },include.rownames=TRUE)
  
  output$downloadplotdecouvroc = downloadHandler(
    filename = function() {paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
      ggsave(file, plot =  ROCcurve(validation = datalearningmodel$reslearningmodel$classlearning,
                                    decisionvalues =  datalearningmodel$reslearningmodel$scorelearning),  device = input$paramdownplot)},
    contentType=NA)
  
  output$downloaddatadecouvroc <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(ROCcurve(validation = datalearningmodel$reslearningmodel$classlearning,decisionvalues =  datalearningmodel$reslearningmodel$scorelearning,graph=F), file) })
  
  output$plotmodeldecouvbp <- renderPlot({
    datalearningmodel<<-MODEL()$DATALEARNINGMODEL
    scoremodelplot(class =datalearningmodel$reslearningmodel$classlearning ,
                   score =datalearningmodel$reslearningmodel$scorelearning,
                   names=rownames(datalearningmodel$reslearningmodel),
                   threshold =input$thresholdmodel ,
                   type =input$plotscoremodel,
                   graph = T,
                   printnames=input$shownames1)
  })
  output$downloadplotmodeldecouvbp = downloadHandler(
    filename = function() {paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
      ggsave(file, plot =scoremodelplot(class =datalearningmodel$reslearningmodel$classlearning, 
                                        score =datalearningmodel$reslearningmodel$scorelearning,
                                        names=rownames(datalearningmodel$reslearningmodel),
                                        threshold =input$thresholdmodel ,
                                        type =input$plotscoremodel,graph = T),  
             device = input$paramdownplot)},
    contentType=NA)
  
  output$downloaddatamodeldecouvbp <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   scoremodelplot(class =datalearningmodel$reslearningmodel$classlearning ,score =datalearningmodel$reslearningmodel$scorelearning,names=rownames(datalearningmodel$reslearningmodel),
                                        threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = F), file) })
  output$nbselectmodel<-renderText({
    datalearningmodel<-MODEL()$DATALEARNINGMODEL
    ncol(datalearningmodel$learningmodel)-1
  })
  
  output$tabmodeldecouv<-renderTable({
    datalearningmodel<-MODEL()$DATALEARNINGMODEL
    as.data.frame.matrix(table(datalearningmodel$reslearningmodel$predictclasslearning,datalearningmodel$reslearningmodel$classlearning ))
  },include.rownames=TRUE)
  
  output$sensibilitydecouv<-renderText({
    datalearningmodel<-MODEL()$DATALEARNINGMODEL
    sensibility(predict = datalearningmodel$reslearningmodel$predictclasslearning,
                class = datalearningmodel$reslearningmodel$classlearning)
  })
  
  output$specificitydecouv<-renderText({
    datalearningmodel<-MODEL()$DATALEARNINGMODEL
    specificity(predict = datalearningmodel$reslearningmodel$predictclasslearning,
                class = datalearningmodel$reslearningmodel$classlearning )
  })
  
  
  output$downloaddatavalidation <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset( data.frame("Class"=MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,
                                  MODEL()$DATAVALIDATIONMODEL$validationmodel,
                                  check.names = F), file) })
  
  
  output$plotmodelvalroc <- renderPlot({
    datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
    ROCcurve(validation =  datavalidationmodel$resvalidationmodel$classval,decisionvalues =  datavalidationmodel$resvalidationmodel$scoreval)
  })
  
  output$downloadplotvalroc = downloadHandler(
    filename = function() {paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
      ggsave(file, plot =ROCcurve(validation =  MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,decisionvalues =  MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval),  device = input$paramdownplot)},
    contentType=NA)
  
  output$downloaddatavalroc <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   ROCcurve(validation =  MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,decisionvalues =  MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval,graph=F ), file) 
    })
  
  output$plotmodelvalbp <- renderPlot({
    datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
    scoremodelplot(class = datavalidationmodel$resvalidationmodel$classval ,
                   score =datavalidationmodel$resvalidationmodel$scoreval,
                   names=rownames(datavalidationmodel$resvalidationmodel),
                   threshold =input$thresholdmodel,
                   type =input$plotscoremodel
                   ,graph = T,
                   printnames=input$shownames1)
  })
  
  output$downloadplotmodelvalbp = downloadHandler(
    filename = function() {paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
      ggsave(file, plot =scoremodelplot(class = MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval ,
                                        score = MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval,
                                        names = rownames(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel),
                                        threshold = input$thresholdmodel ,
                                        type = input$plotscoremodel,graph = T),  
             device = input$paramdownplot)},
    contentType=NA)
  
  output$downloaddatamodelvalbp <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   scoremodelplot(class = MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval ,
                                        score =MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval,
                                        names=rownames(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel),
                                        threshold =input$thresholdmodel ,
                                        type =input$plotscoremodel,
                                        graph = F), 
                         file) }
    )
  
  output$youndenval<-renderTable({
    datavalidationmodel<<-MODEL()$DATAVALIDATIONMODEL
    resyounden<-younden(datavalidationmodel$resvalidationmodel$classval,datavalidationmodel$resvalidationmodel$scoreval )
    resyounden<-data.frame(resyounden)
    colnames(resyounden)<-c("")
    rownames(resyounden)<-c("younden","sensibility younden","specificity younden","threshold younden")
    resyounden
  },include.rownames=TRUE)
  
  output$tabmodelval<-renderTable({ 
    datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
    as.data.frame.matrix(table(datavalidationmodel$resvalidationmodel$predictclassval, datavalidationmodel$resvalidationmodel$classval))
  },include.rownames=TRUE)
  output$sensibilityval<-renderText({
    datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
    sensibility(predict = datavalidationmodel$resvalidationmodel$predictclassval,class = datavalidationmodel$resvalidationmodel$classval)
  })
  output$specificityval<-renderText({
    datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
    specificity(predict = datavalidationmodel$resvalidationmodel$predictclassval,class =  datavalidationmodel$resvalidationmodel$classval)
  })
  ####Detail of the model
  output$summarymodel<-renderPrint({
    model<-print(MODEL()$MODEL)
  })
  output$plotimportance<-renderPlot({
    model<<-MODEL()$MODEL
    learningmodel<<-MODEL()$DATALEARNINGMODEL$learningmodel
    modeltype<<-input$model
    importanceplot(model = model,learningmodel = learningmodel,modeltype =modeltype,graph=T )
  })
  output$downloadplotimportance = downloadHandler(
    filename = function() {paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
      ggsave(file, plot =  importanceplot(model = MODEL()$MODEL,learningmodel = MODEL()$DATALEARNINGMODEL$learningmodel,modeltype =input$model,graph=T ),  device = input$paramdownplot)},
    contentType=NA)
  
  output$downloaddataplotimportance <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(     importanceplot(model = MODEL()$MODEL,learningmodel = MODEL()$DATALEARNINGMODEL$learningmodel,modeltype =input$model,graph=F ), file) })
  
  ####Test prameters
  output$testNAstructure<- reactive({
    if("TRUE"%in%input$NAstructuretest ){test<-as.logical(TRUE)}
    else{test<-as.logical(FALSE)}
    return(test)
  })
  outputOptions(output, 'testNAstructure', suspendWhenHidden=FALSE)
  
  # fonction d'affichage des parametres de test
  TESTPARAMETERS <- eventReactive(input$tunetest, { 
    prctvaluestest<-seq(input$prctvaluestest[1],input$prctvaluestest[2],by = 5)
    listparameters<<-list("prctvalues"= prctvaluestest,
                          "selectmethod"=input$selectmethodtest,
                          "NAstructure"=as.logical(input$NAstructuretest),
                          "thresholdNAstructure"=input$thresholdNAstructuretest,
                          "structdata"=input$structdatatest,
                          "maxvaluesgroupmin"=input$maxvaluesgroupmintest,
                          "minvaluesgroupmax"=input$minvaluesgroupmaxtest,
                          "rempNA"=input$rempNAtest,
                          "log"=as.logical(input$logtest),
                          "logtype"=input$logtypetest,
                          "standardization"=as.logical(input$standardizationtest),
                          "arcsin"=as.logical(input$arcsintest),
                          "test"=input$testtest,
                          "adjustpv"=as.logical(input$adjustpvtest),
                          "thresholdpv"=input$thresholdpvtest,
                          "thresholdFC"=input$thresholdFCtest,
                          "model"=input$modeltest,
                          "thresholdmodel"=0,
                          "fs" = as.logical(input$fstest))
    length(listparameters$prctvalues)
    validate(need( sum(do.call(rbind, lapply(listparameters, FUN=function(x){length(x)==0})))==0,"One of the parameters is empty"))
    tabparameters<<-constructparameters(listparameters)
    tabparameters$thresholdmodel[which(tabparameters$model=="randomforest")]<-0.5
    validation<<-DATA()$VALIDATION
    learning<<-DATA()$LEARNING
    tabparametersresults<<-testparametersfunction(learning,validation,tabparameters)
    #clean useless columns
    if(length(which(apply(X = tabparametersresults,MARGIN=2,function(x){sum(is.na(x))})==nrow(tabparametersresults)))!=0){
      tabparametersresults<-tabparametersresults[,-which(apply(X = tabparametersresults,MARGIN=2,function(x){sum(is.na(x))})==nrow(tabparametersresults))]
    }
    return(tabparametersresults)
    
    #     if(sum(listparameters$NAstructure)==0){tabparametersresults<-
    #       tabparametersresults[,-c("thresholdNAstructure","structdata")]
    #     }
    
  })
  # output$testtabparameters<- reactive({
  #   if(!tabparameters ){test<-as.logical(FALSE)}
  #   else{test<-as.logical(TRUE)}
  #   return(test)
  # })
  # outputOptions(output, 'testNAstructure', suspendWhenHidden=FALSE)
  
  # fonction d'affichage des parametres de test
  output$tabtestparameters <- DT::renderDataTable({
    req(TESTPARAMETERS())
    resparameters <<- TESTPARAMETERS()
    df <- cbind(Names = rownames(resparameters), resparameters)
    DT::datatable(df,
                  options = list(
                    orderClasses = FALSE,
                    responsive = FALSE,
                    pageLength = 100
                  )
    )
  })
  
  
  
  # output$tabtestparameters<-renderDataTable({
  #   resparameters<<-TESTPARAMETERS()
  #   cbind(Names=rownames(resparameters),resparameters)},
  #   options = list(    "orderClasses" = F,
  #                      "responsive" = F,
  #                      "pageLength" = 100
  #                      #          ,rowCallback = I('
  #                      # function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {$("td:eq(1)", nRow).css("color", "red");}'
  #                      # )
  #   )
  # )
  
  output$downloadtabtestparameters <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   TESTPARAMETERS(), file) })
  
  output$download_selected_peptides <- downloadHandler(
    filename = function() { paste('selected_peptides.csv') },
    content = function(file) {
      write.csv(TEST()$useddata, file, row.names = FALSE)
    }
  )
  
  # Variable réactive pour stocker le graphique de stabilité
  stability_plot_obj <- reactive({
    req(TEST()$VARIABLES_FREQ)
    if(!is.null(TEST()$VARIABLES_FREQ)) {
    freq <- TEST()$VARIABLES_FREQ
      n_iter <- input$n_iterations
    df <- data.frame(
      peptide = names(freq),
      percentage = as.numeric(freq) / n_iter * 100
    )
      df$selected <- ifelse(df$peptide %in% TEST()$SELECTED_VARS, "Selected", "Not selected")
    ggplot(df, aes(x = reorder(peptide, percentage), y = percentage, fill = selected)) +
      geom_bar(stat = "identity") +
        #coord_flip() + # pour inverser les axes
        labs(title = "Stability of peptide selection",
             x = "Peptides", y = "Percentage of selection (%)", fill = "Statut") +
      theme_minimal() +
        theme(
          axis.text.x = element_blank(),
          legend.text = element_text(size = 12, face = 'bold'),
          legend.title = element_text(size = 15, face = 'bold'),
          plot.title = element_text(face = 'bold', size = 20),
          axis.title.x = element_text(face = 'bold', size = 20),
          axis.title.y = element_text(face = 'bold', size = 20,
                                      margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),  # Ajout de padding
          axis.text.y = element_text(size = 20, face = 'bold'),
          panel.grid.major.x = element_blank(),  # Supprimer les lignes de grille horizontales
          panel.grid.minor.x = element_blank()   # Supprimer les lignes de grille mineures
        ) +
        scale_fill_manual(values = c("Selected" = "steelblue", 
                                     "Not selected" = "lightgray")) +
      geom_hline(yintercept = (as.numeric(TEST()$SEUIL) / n_iter * 100),
                 color = "red", linetype = "dashed", size = 1)
      
    }else{
      errorplot(text = "No differently expressed ")
    }
  })
  
  #output$stability_plot <- renderPlotly({
  output$stability_plot <- renderPlot({
    tryCatch({
      req(stability_plot_obj())
      print(stability_plot_obj())
      #print(ggplotly(stability_plot_obj()))
    }, error = function(e) {
      # errorplot(text = paste("Erreur dans le graphique de stabilité :", e$message))
      print(paste("Erreur dans le graphique de stabilité :", e$message))
    })
  })
  
  output$selected_peptides_table <- renderDataTable({
    if (TEST()$mode == "bootstrap") {
      req(TEST()$useddata)
      df <- TEST()$useddata
    } else {
      req(TEST()$USEDDATA) 
      df <- TEST()$USEDDATA
    }
    datatable(df,
              options = list(pageLength = 15, scrollX = TRUE, orderClasses = TRUE),
              colnames = c("Peptide", "p-value", "logFC", "Moyenne groupe 1", "Moyenne groupe 2"),
              rownames = FALSE
    )
  })
  
  output$elbow_stats <- renderText({
    req(TEST()$VARIABLES_FREQ)
    freq <- TEST()$VARIABLES_FREQ
    seuil <- TEST()$SEUIL
    n_total <- length(freq)
    n_selected <- length(TEST()$SELECTED_VARS)
    stats <- TEST()$stats_selection
    # Vérifier si les stats sont disponibles
    mean_peptides_text <- if (!is.null(stats$mean_peptides) && is.numeric(stats$mean_peptides)) {
      paste("• Moyenne de peptides sélectionnés par itération :", round(stats$mean_peptides, 1))
    } else {
      "• Moyenne de peptides sélectionnés par itération : Non disponible"
    }
    
    # ci_text <- if (!is.null(stats$confidence_interval) && is.numeric(stats$confidence_interval)) {
    #   paste("• Intervalle de confiance (IC 95%) :", paste(round(stats$confidence_interval, 1), collapse = " - "))
    # } else {
    #   "• Intervalle de confiance (IC 95%) : Non disponible"
    # }
    
    paste(
      " Statistiques du bootstrap :",
      "",
      paste("• Peptides analysed :", n_total),
      paste("• Selection threshold :", seuil),
      paste("• Selected peptides :", n_selected),
      #mean_peptides_text,
      # ci_text,
      sep = "\n"
    )
  })
  
  output$bootstrap_frequency_plot <- renderPlot({
    req(TEST()$VARIABLES_FREQ)
    freq <- TEST()$VARIABLES_FREQ
    barplot(freq, las = 2, col = "skyblue",
            main = "Fréquence de sélection des peptides (bootstrap)",
            ylab = "Nombre de sélections", 
            xlab = "Peptides")
    abline(h = TEST()$SEUIL, col = "red", lty = 2, lwd = 2)
  })
  
  # fonction d'affichage de l'état de la méthode bootstrap :  complete ou non 
  output$bootstrap_completed <- reactive({
    req(TEST())
    # cat("le Test est complete : ", TEST()$completed , "\n")
    return(TEST()$completed)
  })
  outputOptions(output, 'bootstrap_completed', suspendWhenHidden=FALSE)
  
  # Fonction pour afficher le statut du cache
  output$cache_status <- renderText({
    if (input$stats_method == 'bootstrap') {
      cache_objects <- ls(pattern = "^bootstrap_cache_", envir = .GlobalEnv)
      if (length(cache_objects) > 0) {
        paste("Cache actif :", length(cache_objects), "résultat(s) en cache")
      } else {
        "Cache vide"
      }
    } else {
      ""
    }
  })
  
  # Output pour le graphique des fréquences
  output$elbow_frequency_plot <- renderPlot({
    req(TEST()$VARIABLES_FREQ)
    # plots <- create_elbow_plots_pure(TEST()$VARIABLES_FREQ)
    # plots$frequency_plot
    # find_elbow_point(TEST()$VARIABLES_FREQ, plot = TRUE)
    plot_elbow_detection(TEST()$VARIABLES_FREQ, plot = TRUE)
  })
  
  # Output pour le graphique des distances (votre deuxième graphique)
  output$elbow_distance_plot <- renderPlot({
    req(TEST()$VARIABLES_FREQ)
    values  =  find_elbow_point(TEST()$VARIABLES_FREQ, plot = TRUE)
    # plots <- create_elbow_plots_pure(TEST()$VARIABLES_FREQ, 
    #                                  elbow_value =values[1] , 
    #                                  elbow_index =  values[2], 
    #                                  distances = values[3])
    # plots$distance_plot
    plot_elbow_distance_detection(TEST()$VARIABLES_FREQ, plot = TRUE)
  })
  
  # Output pour le graphique GMM (modèle de mélange gaussien)
  output$gmm_threshold_plot <- renderPlot({
    req(TEST()$VARIABLES_FREQ)
    tryCatch({
      plot_gmm_threshold_detection(TEST()$VARIABLES_FREQ, plot = TRUE)
    }, error = function(e) {
      # En cas d'erreur (par exemple si mclust n'est pas installé)
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Erreur GMM:", e$message, "\n\nInstallez le package 'mclust':\ninstall.packages('mclust')"),
           cex = 1.2, col = "red")
    })
  })
  
  # Output pour les statistiques GMM
  output$gmm_stats <- renderText({
    req(TEST()$VARIABLES_FREQ)
    tryCatch({
      stats <- get_gmm_stats(TEST()$VARIABLES_FREQ)
      paste(
        " GMM statistics :",
        "",
        paste("• Optimal threshold :", round(stats$threshold, 3)),
        paste("• Average component 1 (μ₁) :", round(stats$means[1], 3)),
        paste("• Average component 2 (μ₂) :", round(stats$means[2], 3)),
        paste("• Component variance 1 (σ₁²) :", round(stats$variances[1], 3)),
        paste("• Component variance 2 (σ₂²) :", round(stats$variances[2], 3)),
        paste("• Component weight 1 (π₁) :", round(stats$weights[1], 3)),
        paste("• Component weight 2 (π₂) :", round(stats$weights[2], 3)),
        paste("• BIC criterion :", round(stats$bic, 2)),
        "",
        # " Le seuil optimal est calculé à l'intersection des deux composantes gaussiennes.",
        "The optimal threshold is calculated as the midpoint between the two Gaussian means.",
        sep = "\n"
      )
    }, error = function(e) {
      paste("Erreur lors du calcul des statistiques GMM :", e$message, "\n\nInstallez le package 'mclust': install.packages('mclust')")
    })
  })
  
  output$table_peptides =  renderDataTable({
    req(TEST()$VARIABLES_FREQ)
    
    df_freq = data.frame(
      peptide = names(TEST()$VARIABLES_FREQ),
      frequencies = as.numeric(TEST()$VARIABLES_FREQ) #, 
      #percentage =  as.numeric(TEST()$VARIABLES_FREQ) / TEST()$n_iterations * 100
    )
    # filter les peptides stables avec le seuil 
    stable_df = df_freq #[df_freq$frequencies >= TEST()$SEUIL, ]
    #stable_df$percentage <- round(stable_df$percentage, 1)
    
    datatable(stable_df,
              options = list(pageLength = 15, scrollX = TRUE, orderClasses = TRUE),
              colnames = c("Peptide", "Nb sélections"
                           #, "% stabilité"
              ),
              rownames = FALSE
    ) 
    # %>%
    #   formatStyle("percentage",
    #               background = styleColorBar(range(stable_df$percentage), "lightblue"),
    #               backgroundSize = "100% 90%",
    #               backgroundRepeat = "no-repeat",
    #               backgroundPosition = "center"
    #   )
  })
  
  
  observe({
    req(TEST()$VARIABLES_FREQ)
    df_freq = data.frame(
      peptide = names(TEST()$VARIABLES_FREQ),
      frequencies = as.numeric(TEST()$VARIABLES_FREQ) 
    )
    # filter les peptides stables avec le seuil 
    stable_peptides = df_freq[df_freq$frequencies >= TEST()$SEUIL, ]
    print(stable_peptides)
    
  })
  
  # Table des peptides stables
  output$stable_peptides_table <- renderDataTable({
    req(TEST()$VARIABLES_FREQ)
    
    # Utiliser le nombre d'itérations depuis l'input
    n_iterations <- input$n_iterations
    df_freq = data.frame(
      peptide = names(TEST()$VARIABLES_FREQ),
      frequencies = as.numeric(TEST()$VARIABLES_FREQ), 
      percentage = as.numeric(TEST()$VARIABLES_FREQ) / n_iterations * 100
    )
    # filter les peptides stables avec le seuil 
    stable_df = df_freq[df_freq$frequencies >= TEST()$SEUIL, ]
    stable_df$percentage <- round(stable_df$percentage, 1)
    
    datatable(stable_df,
              options = list(pageLength = 15, scrollX = TRUE, orderClasses = TRUE),
              colnames = c("Peptide", "Nb sélections", "% stabilité"),
              rownames = FALSE
    ) %>%
      formatStyle("percentage",
                  background = styleColorBar(range(stable_df$percentage), "lightblue"),
                  backgroundSize = "100% 90%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      )
  })
  
  output$download_stability_plot <- downloadHandler(
    filename = function() { paste('stability_plot', '.', input$paramdownplot, sep='') },
    content = function(file) {
      ggsave(file, plot = stability_plot_obj(), device = input$paramdownplot, width = 12, height = 8, dpi = 300)
    },
    contentType = NA
  )
  
  # Downloads pour bootstrap
  output$download_stability_data <- downloadHandler(
    filename = function() { paste('stability_results', '.', input$paramdowntable, sep='') },
    content = function(file) {
      req(TEST()$VARIABLES_FREQ)
      
      downloaddataset(data.frame(peptide = names(TEST()$VARIABLES_FREQ),
                                 frequencies = as.numeric(TEST()$VARIABLES_FREQ)),
                      file)
    }
  )
  
  output$download_stable_peptides <- downloadHandler(
    filename = function() { paste('stable_peptides', '.', input$paramdowntable, sep='') },
    content = function(file) {
      df_peptides_Final = data.frame(peptide = names(TEST()$VARIABLES_FREQ),
                                     frequencies = as.numeric(TEST()$VARIABLES_FREQ)
      )
      df_peptides_Final = df_peptides_Final[df_peptides_Final$frequencies >= TEST()$SEUIL, ]
      downloaddataset(df_peptides_Final, file)
    }
  )
  
  output$table_peptides_all_freqencies =  downloadHandler(
    filename = function() { paste('all frequencies of peptides', '.', input$paramdowntable, sep='') },
    content = function(file) {
      df_peptides = data.frame(peptide = names(TEST()$VARIABLES_FREQ),
                               frequencies = as.numeric(TEST()$VARIABLES_FREQ)
      )
      downloaddataset(df_peptides, file)
    }
  )
  
  
  output$saveTableselectedpeptides =  downloadHandler(
    filename = function() { paste('data of selected peptides', '.', input$paramdowntable, sep='') },
    content = function(file) {
      req(TEST()$VARIABLES_FREQ)
      df_peptides_Final = data.frame(peptide = names(TEST()$VARIABLES_FREQ),
                                     frequencies = as.numeric(TEST()$VARIABLES_FREQ)
      )
      df_peptides_Final = df_peptides_Final[df_peptides_Final$frequencies >= TEST()$SEUIL, ]
      downloaddataset(df_peptides_Final, file)
    }
  )
  
  
  output$download_elbow_frequency_plot <- downloadHandler(
    filename = function() { paste('elbow_frequency_plot', '.', input$paramdownplot, sep='') },
    content = function(file) {
      req(TEST()$VARIABLES_FREQ)
      # plots <- plot_elbow_detection(TEST()$VARIABLES_FREQ, file)
      ggsave(file, plot = plot_elbow_detection(TEST()$VARIABLES_FREQ), 
             # plot =  find_elbow_point(TEST()$VARIABLES_FREQ, plot = TRUE),
             device = input$paramdownplot, width = 10, height = 6, dpi = 300)
    },
    contentType = NA
  )
  
  
  output$download_elbow_distance_plot <- downloadHandler(
    filename = function() { paste('elbow_distance_plot', '.', input$paramdownplot, sep='') },
    content = function(file) {
      req(TEST()$VARIABLES_FREQ)
      values  =  find_elbow_point(TEST()$VARIABLES_FREQ, plot = TRUE)
      
      ggsave(file, plot =  plot_elbow_distance_detection(TEST()$VARIABLES_FREQ, plot = TRUE),
             device = input$paramdownplot, width = 10, height = 6, dpi = 300)
    },
    contentType = NA
  )
  
  # Handler de téléchargement pour le graphique GMM
  output$download_gmm_threshold_plot <- downloadHandler(
    filename = function() { paste('gmm_threshold_plot', '.', input$paramdownplot, sep='') },
    content = function(file) {
      req(TEST()$VARIABLES_FREQ)
      tryCatch({
        ggsave(file, plot = plot_gmm_threshold_detection(TEST()$VARIABLES_FREQ, plot = TRUE),
               device = input$paramdownplot, width = 12, height = 8, dpi = 300)
      }, error = function(e) {
        # En cas d'erreur, créer un graphique d'erreur
        png(file, width = 800, height = 600)
        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Erreur GMM:", e$message, "\n\nInstallez le package 'mclust':\ninstall.packages('mclust')"),
             cex = 1.2, col = "red")
        dev.off()
      })
    },
    contentType = NA
  )
  
  
  output$plot_selected_peptideBar = renderPlotly({
    req(TEST()$VARIABLES_FREQ, TEST()$SEUIL)
    df_freq = data.frame(
      peptide = names(TEST()$VARIABLES_FREQ),
      frequencies = as.numeric(TEST()$VARIABLES_FREQ) 
    )
    
    df_freq = df_freq[df_freq$frequencies >= TEST()$SEUIL, ]
    #df_freq = df_freq[order(df_freq$frequencies, decreasing = TRUE),  ]
    
    # Graphique des fréquences : peptides sélectionnés
    ggplot(df_freq, aes(x = reorder(peptide, frequencies), y = frequencies)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Peptide Selection Frequency",
           x = "Selected Peptides", 
           y = "Number of Selections") +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(), 
        plot.title = element_text(face = 'bold', size = 20),
        axis.title.x = element_text(face = 'bold', size = 20),
        axis.title.y = element_text(face = 'bold', size = 20,
                                    margin = ggplot2::margin(t = 0, r = 50, b = 0, l = 0)), 
        axis.text.y = element_text(size = 20, face = 'bold')
      )
  })
  
  
  output$download_plot_selected_peptideBar = downloadHandler(
    filename = function() { paste('selected_peptideBar_plot', '.', input$paramdownplot, sep='') },
    content = function(file) {
      req(TEST()$VARIABLES_FREQ, TEST()$SEUIL)
      df_freq = data.frame(
        peptide = names(TEST()$VARIABLES_FREQ),
        frequencies = as.numeric(TEST()$VARIABLES_FREQ)
      )
      
      df_freq = df_freq[df_freq$frequencies >= TEST()$SEUIL, ]
      
      ggsave(file, plot = ggplot(df_freq, aes(x = reorder(peptide, -frequencies), y = frequencies)) +
               geom_bar(stat = "identity", fill = "steelblue") +
               labs(title = "Peptide selection frequency",
                    x = "selected Peptides", 
                    y = "Number of selections") +
               theme_minimal() +
               theme(axis.text.x = element_blank(), 
                     plot.title =  element_text(face =  'bold', size =  20),
                     axis.title.x = element_text(face =  'bold', size = 20),
                     axis.title.y = element_text(face =  'bold', size = 20,
                                                 margin = ggplot2::margin(t = 0, r = 50, b = 0, l = 0)),
                     axis.text.y =  element_text(size = 20, face =  'bold')), 
             device = input$paramdownplot, width = 10, height = 6, dpi = 300)
    },
    contentType=NA
  )
  
  # Observer pour afficher l'alerte de succès pendant 3 secondes
  observeEvent(TEST(), {
    req(TEST())
    req(TEST()$completed == TRUE)
    showElement("alert_success")
    shinyjs::delay(3000, hideElement("alert_success")) 
  })
  
  
}) 

