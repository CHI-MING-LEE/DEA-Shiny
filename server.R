library(shiny)
library(Benchmarking)
library(DT)
library(ggplot2)
library(shinydashboard)


function(input, output,session){
  
  options(scipen=999)
  Sys.setlocale(locale = "zh_TW.UTF8")
  
  checkclass<-function(x){
    cl<-NULL
    for(i in 1:ncol(x)){
      cl<-c(cl,class(x[,i]))
    }
    cl
  }
  
  # è³‡æ?™è?€??–reactive----
    deadata <- reactive({
      inputhisfile <- input$file1        
      if(is.null(inputhisfile)){
        return(NULL) 
      }else{
        read.csv(inputhisfile$datapath, fileEncoding = "BIG5", header = T, stringsAsFactors = F)
      }
    })
    
    # è³‡æ?™å?è?•ç??
    XYpre <- reactive({
        if(is.null(deadata())){
            return(NULL)
        }else{
            ARData <- deadata()
            ARDataColName <- colnames(ARData)[-1] # ç¬¬ä?€è¡Œç‚ºprjcode 
            Xcol <- na.omit(as.numeric(strsplit(paste(input$X)," ")[[1]]))
            Ycol <- na.omit(as.numeric(strsplit(paste(input$Y)," ")[[1]]))
            lenX <- length(Xcol)
            lenY <- length(Ycol)
            
            LC <- c("lenX" = lenX, "lenY" = lenY)
            NM <- ARDataColName
            
            return(list(Xcol,Ycol,LC,NM))
        }
    })
    
    # è³‡æ?™è¼¸?…¥?€ä?ç…§è®Šæ•¸?•¸??å?•é?‹UI
    observeEvent(input$vGo,{
      inFile <- input$file1
      if (is.null(inFile)){
        showModal(modalDialog(
          "è«‹å?ˆåŒ¯?…¥è³‡æ??",
          easyClose = T
          ,footer = modalButton("OK")
        ))
      }else if(any(duplicated(as.numeric(na.omit(as.numeric(strsplit(paste(input$X,input$Y)," ")[[1]])))))){
        showModal(modalDialog(
          "æ¬„ä?é?è??",
          easyClose = T
          ,footer = modalButton("OK")
        ))
      }else if(input$X==""|input$Y==""){
        showModal(modalDialog(
          "è®Šæ•¸æ¬„ä?ä?èƒ½ç©ºç™½",
          easyClose = T
          ,footer = modalButton("OK")
        ))
      }else{
        colchoosed<-setdiff(as.numeric(strsplit(paste(input$X,input$Y)," ")[[1]]),NA)
        if(any(colchoosed %in% which(checkclass(deadata())=="character"))){
          showModal(modalDialog(
            "?ª?ƒ½?¸??‡æ•¸å­—å?‹æ?‹æ?„ä??",
            easyClose = T
            ,footer = modalButton("OK")
          ))
        }else if(any(!colchoosed %in% 1:ncol(deadata()))){
          showModal(modalDialog(
            "è¼¸å…¥??„æ?„ä?è?…å‡ºè³‡æ?™æ?„ä?æ•¸",
            easyClose = T
            ,footer = modalButton("OK")
          ))
        }else{
        
          if(input$mode==1){
            return(NULL)
          }else{
              # Input lower
              output$Iratiolower <- renderUI({
                XYpre <- XYpre()
                ARDataColName <- XYpre[[4]]
                LC <- XYpre[[3]]
                if(LC["lenX"]==1){
                  return(NULL)
                }else if(LC["lenX"]>=2){
                  lapply(1:(LC["lenX"]-1), function(i){
                    div(style="display:inline-block",numericInput(inputId = paste0("ILower",i),
                        label = paste(combn(ARDataColName,2)[1,1:(LC["lenX"]-1)],
                                      combn(ARDataColName,2)[2,1:(LC["lenX"]-1)],sep="/")[i],value=0.5))
                  })
                }
              })
              # Input upper
              output$Iratioupper <- renderUI({
                XYpre <- XYpre()
                LC <- XYpre[[3]]
                ARDataColName <- XYpre[[4]][(LC["lenX"]+1):length(XYpre[[4]])] # for output
                if(LC["lenX"]==1){
                  return(NULL)
                }else if(LC["lenX"]>=2){
                  lapply(1:(LC["lenX"]-1), function(i){
                    div(style="display:inline-block",numericInput(inputId = paste0("IUpper",i), 
                        label = paste(combn(ARDataColName,2)[1,1:(LC["lenX"]-1)],
                                      combn(ARDataColName,2)[2,1:(LC["lenX"]-1)],sep="/")[i],value=2))
                  })
                }
              })
              # Output lower
              output$Oratiolower <- renderUI({
                XYpre <- XYpre()
                LC <- XYpre[[3]]
                ARDataColName <- XYpre[[4]][(LC["lenX"]+1):length(XYpre[[4]])] # for output
                if(LC["lenY"]==1){
                  return(NULL)
                }else if(LC["lenY"]>=2){
                  lapply(1:(LC["lenY"]-1), function(i){
                    div(style="display:inline-block",numericInput(inputId = paste0("OLower",i), 
                        label = paste(combn(ARDataColName,2)[1,1:(LC["lenY"]-1)],
                                      combn(ARDataColName,2)[2,1:(LC["lenY"]-1)],sep="/")[i],value=0.5))
                  })
                }
              })
              
              combn(c(3,4),2)
              
              # Output upper
              output$Oratioupper <- renderUI({
                XYpre <- XYpre()
                ARDataColName <- XYpre[[4]]
                LC <- XYpre[[3]]
                if(LC["lenY"]==1){
                  return(NULL)
                }else if(LC["lenY"]>=2){
                  lapply(1:(LC["lenY"]-1), function(i){
                    div(style="display:inline-block",numericInput(inputId = paste0("OUpper",i), 
                        label = paste(combn(ARDataColName,2)[1,1:(LC["lenY"]-1)],
                                      combn(ARDataColName,2)[2,1:(LC["lenY"]-1)],sep="/")[i],value=2))
                  })
                }
              })
          }
        }
      }
    })
    # è³‡æ?™æª¢è¦?----
    output$viewdata<-DT::renderDataTable({
      if (is.null(deadata())){
        return(NULL)
      }else{
        datatable(deadata(),
                  options = list(searching = F, filter = "top",
                                 lengthMenu = list(c(-1, 20), c('20','All')),
                                 pageLength = 20))
      }
    })
    
    # è¨ˆç?—å?Œç•¢è¦–ç??+è·³è?‰Panel
    observeEvent(input$run,{
      if(!is.null(deadata()) & !is.null(XYpre)){
          showModal(modalDialog(
            div("å°ˆæ?ˆç›¸å°æ?ˆç?‡è?ˆç?—å?Œç•¢ï¼?", style = "font-size:200%"),
            easyClose = T, footer = modalButton("OK")))
        #è·³è?‰panel ----
          updateTabsetPanel(session, "tabset",selected = "result")
      }else{
        showModal(modalDialog(
          div("è«‹æª¢?Ÿ¥è³‡æ?™è¼¸?…¥?˜¯?¦æ­?ç¢ºå?Œæ•´ï¼?", style = "font-size:200%"),
          easyClose = T
          ,footer = modalButton("OK")
        ))
      }
    })
    
    # DEAè¨ˆç?—æ¨¡çµ?----
    calculation <- eventReactive(input$run,{
          # Reactive Data
          ARData <- deadata()
          
          # Data Preprocess
          XYpre <- XYpre()
          ARDataColName <- XYpre[[4]] # ç¬¬ä?€è¡Œç‚ºprjcode(å·²åœ¨XYpre?Ž»?™¤)
          LC <- XYpre[[3]]
          Xcol <- XYpre[[1]] 
          Ycol <- XYpre[[2]]

          # å»ºç?‹DEA AR Model
          if(input$mode == 1){
              CCR <- dea.dual(X=ARData[,Xcol],Y=ARData[,Ycol],RTS="crs")
              CCR.Result <- cbind(ARData[,1],"?›¸å°æ?ˆç??" = round(CCR$eff,10), round(CCR$u,10), round(CCR$v,10))
              colnames(CCR.Result) <- c("Project Code", "?›¸å°æ?ˆç??", ARDataColName[Xcol-1], ARDataColName[Ycol-1] )
              return(CCR.Result)
          }else{
            # è·‘CCR-AR-Iï¼Œä?”æ?æ ¼?ƒ½è¦å¡«
            # ??–å‡ºinput?€outputæ¬Šé??
            Inum <- 1:(LC["lenX"]-1)
            Onum <- 1:(LC["lenY"]-1)
            
            # ??ˆå?‡æ?Šé?æ?“å‡ºè®Šæ?vector
            lowerI <- 0; upperI <- 0
            for(t in 1:Inum){
              lowerI <- c(lowerI,input[[paste0("ILower",t)]])
              upperI <- c(upperI,input[[paste0("IUpper",t)]])
            }
            lowerO <- 0; upperO <- 0
            for(tt in 1:Onum){
              lowerO <- c(lowerO,input[[paste0("ILower",tt)]])
              upperO <- c(upperO,input[[paste0("IUpper",tt)]])
            }
            
            # è®Šæ?çŸ©?™£;row1?˜¯lower, row2?˜¯upper
            InputWeight <- rbind(lowerI[-1], upperI[-1])
            OutputWeight <- rbind(lowerO[-1], upperO[-1])

            Ivector <- c()
            for(h in 1:(LC["lenX"]-1)){
              Ivector <- c(Ivector,InputWeight[,h])
            }
            Ovector <- c()
            for(k in 1:(LC["lenX"]-1)){
              Ovector <- c(Ovector,OutputWeight[,k])
            }

            # ?€™ç?†ç?„æ?Šé?å?…é?ˆç”±RenderUI??–å??
            dual.AR <- matrix(c(Ivector,Ovector), nrow=LC["lenX"]+LC["lenY"]-2, ncol=2, byrow=TRUE)
            E.AR <- dea.dual(X=ARData[,Xcol],Y=ARData[,Ycol],RTS="crs", DUAL=dual.AR)
            
            AR.Result <- cbind(ARData[,1],"?›¸å°æ?ˆç??" = round(E.AR$eff,10), round(E.AR$u,10), round(E.AR$v,10))
            colnames(AR.Result) <- c("Project Code", "?›¸å°æ?ˆç??", ARDataColName[Xcol-1], ARDataColName[Ycol-1] )
            
            return(AR.Result)
          }
    })
    
    # DEAçµæ?œoutput----
    output$Efficiency <- DT::renderDataTable({
        if (is.null(calculation())){
          return(NULL)
        }else{
          datatable(calculation(),
                    options = list(searching = F, filter = "top",
                                   lengthMenu = list(c(20, -1), c('20','All')),
                                   pageLength = 20))
        }
    })
    
    # ?„²å­˜ç?æ??----
    output$SaveNum <- downloadHandler(
        filename = function() { paste0("DEA_Result - " ,Sys.Date(), ".csv") },
        content = function(file) {
          if(is.null(calculation())){
            return(NULL)
          }else{
            write.csv(calculation(), file)
          }
          
        }
    )
    
    # ??–è¡¨??ˆç¾----
    output$viewplot <- renderPlot({
      if(is.null(XYpre())){
        return(NULL)
      }else{
        # Reactive Data
        ARData <- deadata()
        
        # Data Preprocess
        XYpre <- XYpre()
        ARDataColName <- XYpre[[4]] # ç¬¬ä?€è¡Œç‚ºprjcode(å·²åœ¨XYpre?Ž»?™¤)
        LC <- XYpre[[3]]
        Xcol <- XYpre[[1]]
        Ycol <- XYpre[[2]]
        
        weight <- apply(as.matrix(calculation()),2,as.numeric)
        
        # ?Ÿ©?™£?›¸ä¹˜ä¸¦??“å?è?’ç?šç‚º??†æ•¸
        Xm <- apply(as.matrix(ARData[,Xcol]),2,as.numeric)
        Ym <- apply(as.matrix(ARData[,Ycol]),2,as.numeric)
        
        XMatrix <- Xm %*% t(weight[,(1:LC["lenX"])+2])
        YMatrix <- Ym %*% t(weight[,((3+LC["lenX"]):ncol(weight))])
        for(u in 1:nrow(ARData)){
          ARData$XAxis[u] <- XMatrix[u,u] 
          ARData$YAxis[u] <- YMatrix[u,u]
        }
        fmt_dcimals <- function(decimals=0){
          # return a function responpsible for formatting the 
          # axis labels with a given number of decimals 
          function(x) as.character(round(x,decimals))
        }
        ggplot(ARData, aes(x = XAxis, y = YAxis)) + geom_point() + geom_text(aes(label=ARData[,1]),hjust=-0.5, vjust=-0.5) + 
          scale_y_continuous(labels=fmt_dcimals(10))
      }
    })
    
}
