# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

library("xtable")

source("auxi.R")

function(input, output,session) {
  values <- reactiveValues(default = 0)
  
  dat <- reactive(data.frame())
  
  
  filedata <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    dd<-read.csv(inFile$datapath, header = input$header,
                 sep = input$sep, quote = input$quote)
    ## write.csv(file="conf.csv",dd,row.names = FALSE)
    # dist1<-dist(mymatrix(dd,1))
    #dmatAbs<-matrix_distance(dd, Abstract)
    #dmatTit<-matrix_distance(path, Title, tfidf = TRUE, distanceC = FALSE)
    #dmatKey<-matrix_distance(path, Keywords, tfidf = TRUE, distanceC = FALSE)
    dist_abs<-mymatrix(dd,"Abstract",TFIDF = input$tfidf_a,distanceC=input$metric_a)
    dist_tit<-mymatrix(dd,"Title",TFIDF = input$tfidf_t, distanceC=input$metric_t)
    dist_key<-mymatrix(dd,"Keywords",TFIDF = input$tfidf_k, distanceC=input$metric_k)
    
    
    w1<-input$title
    w2<-input$keywords
    w3<-input$abstract
    if (w1==0 && w2 ==0 && w3 ==0)
    {
      w1=1
      w2=1
      w3=1
    }
    wn1<-w1/(w1+w2+w3)
    wn2<-w2/(w1+w2+w3)
    wn3<-w3/(w1+w2+w3)
    dist_tot<-dist_abs*wn3+dist_tit*wn1+dist_key*wn2
    mm<-as.matrix(dist_tot)
    write.table(mm,file = "matriz_distancias.csv",row.names=FALSE, col.names=FALSE, sep=",")
    
    #comando<-paste("dst_matrix.py")
    #python.load(comando)
    
    dd
  })
  
  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    
    dat<-filedata()
    
    if (!input$showabstractr) 
      dat<-dat[,1:2]
    dat
  },include.rownames=TRUE)
  
  output$summary<-
    renderPrint({
      dat<-filedata()
      paste( "Number of Papers:", nrow(dat))
    })
  
  output$summary1<-
    renderPrint({
      dat<-filedata()
      paste( "Number of Papers:", nrow(dat))
    })
  #########################
  dendo<-reactive({
    kk<-input$title
    kk1<-input$abstract
    kk2<-input$keywords ### sirve para indicar que se ejecute si cambian esos valores
    kk3<-input$tabs
    kk4<-input$tfidf_a
    kk4<-input$metric_a
    kk4<-input$tfidf_k
    kk4<-input$metric_k
    kk4<-input$tfidf_t
    kk4<-input$metric_t
    
    read.csv("matriz_distancias.csv",header = FALSE)
    
  })
  
  output$plot<-renderPlot({
    mat<-dendo()
    dst1<-as.dist(mat)
    k<-input$ses
    plot_dendo(dst1, k)
    #rc<-hclust(dst1)
    #vwt<-read.table(file="wt.txt")
    #plot(rc,main="Dendrogram",xlab="Dendrogram of Papers",ylab="Dist")
    
  })
  
  #########################
  
  mymds<-reactive({
    kk<-input$title
    kk1<-input$abstract
    kk2<-input$keywords ### sirve para indicar que se ejecute si cambian esos valores
    kk3<-input$tabs
    kk4<-input$tfidf_a
    kk4<-input$metric_a
    kk4<-input$tfidf_k
    kk4<-input$metric_k
    kk4<-input$tfidf_t
    kk4<-input$metric_t
    
    read.csv("matriz_distancias.csv",header = FALSE)
    #print ("condition")
    #print (output$gocumpute1==TRUE)
    
  })
  
  output$plotmds<-renderPlot({
    a2 <-  mymds()
    fit <- cmdscale(a2, eig=TRUE, k=2) # k es la dimensi?n
    fit # view results
    
    # plot solution 
    x <- fit$points[,1]
    y <- fit$points[,2]
    
    #if (gocompute1) {
    #  plot(y, x, xlab="Coordinate 1", ylab="Coordinate 2",  main="Metric  MDS", col=as.integer(r2), pch=19)
    #  text(y, x, pos = 4, labels = c(1:length(a2)), cex=.7)
    #} else {
    plot(y, x, xlab="Coordinate 1", ylab="Coordinate 2",  main="Metric  MDS", pch=19)
    text(y, x, pos = 4, labels = c(1:length(a2)), cex=.7)
      
    #}
  })
  
  #########################
  
  # dendo2<-reactive({
  #    matr<-read.csv("matriz_distancias.csv",header = FALSE)
  #    limt<-100
  #    matr<-matr[1:limt,1:limt]
  #    hmat<-as.matrix(scale(matr))
  #    mm<-heatmap(hmat,
  #                col = topo.colors(200, alpha=0.5),
  #                Colv=F, scale="none")
  #    mm
  #  })
  
  # output$plot2<-renderPlot({
  #    dendo2()
  #  },height = 600, width = 600)
  
  #########################
  output$wordmap<-renderPlot({
    
    
    mdat<-filedata()
    set.seed(1) # para que siempre salga el mismo..
    dm<-mymatrixbag(mdat,3)
    
    wordcloud(dm$word, dm$freq, scale=c(6,0.4), max.words=150, 
              random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, 
              colors=brewer.pal(8, 'Dark2'))
    
  },height = 500, width = 500)
  
  output$value<- renderText({ "jjj" })
  
  #########################
  
  
  observeEvent(input$goeq,{
    numses<-input$ses
    dat<-filedata()
    tam<-nrow(dat)
    distses<-rep(tam%/%numses,numses)
    r<-tam%%numses
    if (r>0) 
      if (r<(numses-1)) distses<-c(distses[1:(numses-1)],(tam%%numses+distses[1])) else 
        for (i in (1:r)) distses[i]<-1+distses[i]
    
    name<-paste(distses)
    updateTextInput(session, "dist", value=name)
    
  })
  
  
  
  observeEvent(input$gocompute,{
    dat<-filedata()

    listap<-schedule(input$dist)
    r2<-listap[,2]
   
    output$plotmds<-renderPlot({
      a2 <-  mymds()
      fit <- cmdscale(a2, eig=TRUE, k=2) # k es la dimensi?n
      fit # view results
      
      # plot solution 
      x <- fit$points[,1]
      y <- fit$points[,2]
      
      #if (gocompute1) {
      plot(y, x, xlab="Coordinate 1", ylab="Coordinate 2",  main="Metric  MDS", col=as.integer(r2), pch=19)
      text(y, x, pos = 4, labels = c(1:length(a2)), cex=.7)})
      
    if (length(listap)>1)
    {
      datfilt<-data.frame(id=listap[,1],paper=dat[,"Title"][listap[,1]],cluster=as.integer(listap[,2]))
      datfilt<-datfilt[with(datfilt, order(cluster)), ]
      # ldf<-split(datfilt,datfilt$cluster)
      #numses<-max(lista)
      # for(i in 1:length(ldf)) 
      output$distpapers <-  renderTable({datfilt})
      #gocompute1<-TRUE
      output$gocumpute1 <- renderText({'Done'})
      outputOptions(output, "gocumpute1", suspendWhenHidden=FALSE)
    }
    else output$distpapers <-renderText(paste("Sum of sizes does not correspond to number of papers",length(dat[,1])))
   #################################################
     
    
  })
  
  
  observeEvent(input$savecsv,{
    dat<-filedata()
    listap<-schedule(input$dist)
    if (length(listap)>1)
    {
    listap<-schedule(input$dist)
    datfilt<-data.frame(id=listap[,1],paper=dat[,"Title"][listap[,1]],cluster=as.integer(listap[,2]))
    datfilt<-datfilt[with(datfilt, order(cluster)), ]
    # ldf<-split(datfilt,datfilt$cluster)
    #numses<-max(lista)
    # for(i in 1:length(ldf)) 
    write.csv(file="sessions.csv",datfilt,row.names = FALSE)
    }
  })
  
  
  observeEvent(input$title,{    dat<-filedata()  })
  observeEvent(input$keywords,{    dat<-filedata()  })
  observeEvent(input$abstract,{    dat<-filedata()  })
  observeEvent(input$tabs,{    dat<-filedata()  })
  observeEvent(input$tfidf_a,{    dat<-filedata()  })
  observeEvent(input$metric_a,{    dat<-filedata()  })
  observeEvent(input$tfidf_t,{    dat<-filedata()  })
  observeEvent(input$metric_t,{    dat<-filedata()  })
  observeEvent(input$tfidf_k,{    dat<-filedata()  })
  observeEvent(input$metric_k,{    dat<-filedata()  })
  
  
  
  observeEvent(input$file1,{
    output$warnstat <- renderText({'Done'})
    outputOptions(output, "warnstat", suspendWhenHidden=FALSE)
    
  })
  
  }