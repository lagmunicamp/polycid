library(shiny)
library(pcaMethods)
library(ggplot2)
library(DT)
library(NbClust)
library(dplyr)
library(shinycssloaders)
source("auxiliar_scripts.R",local=T)

shinyServer(function(input, output){
  
  #Project Menu
  output$image_menu1 <- renderUI({
    fluidRow(
      column(12,valueBox(width = NULL, "Welcome Breeder!", "polyCID - Polyploid Contaminant Identification", color="navy",icon = icon("pagelines"))),
      column(12,box(width = NULL, title=NULL, solidHeader = TRUE, "polyCID is an user friendly R shiny app based on preprocessed GBS data for detection and classification of putative contaminants in biparental polyploid populations, including apomictic clones, self-fertilization, half-siblings and/or full contaminants individuals. You can explore our simulated data to learn how to interact with the tool, as well as access, modify all datasets and adapt them to your research needs. By going through the workflow below, you will become familiar with all the steps involved in the analysis. Enjoy it!"
      )),
      column(12,renderImage({return(list(src = "www/workflow.jpg",filetype = "image/png",width = "70%",height = "110%",style="display: block; margin-left: auto; margin-right: auto;"))},deleteFile=FALSE)
      ))
  })
  
  #Menu of analyses
  output$image_menu2 <- renderUI({
    tabBox(title = "polyCID",id = "ttabs",width = 12,
           tabPanel("Input Data",uiOutput("page1")),
           tabPanel("Selecting Parents",uiOutput("page2")),
           tabPanel("PCA",uiOutput("page3")),
           tabPanel("GA-I",uiOutput("page4a")),
           tabPanel("GA-II",uiOutput("page4b")),
           tabPanel("GA-III",uiOutput("page4c")),
           tabPanel("Contaminant Identification",uiOutput("page4")),
           tabPanel("Results",uiOutput("page5"))
    )
  })
  
  #Page 1 - Input Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Marker data
  markers <- reactiveValues()
  
  output$page1 <- renderUI({
    markers$snps <- read.csv("data/markers.csv",row.names=1)
    markers$GAs <- read.csv("data/gas.csv",row.names=1)
    
    tabPanel("Description",fluidRow(
      column(12,
             box(title = "SNP dataset", solidHeader = TRUE, status = "primary", "The analysis will be performed with the supplied dataset. If you wish to analyse another dataset, select the option ", strong("add dataset"), " or if you wish to check the data format, click on ", strong("download dataset"),".",br(),br(),
                fluidRow(
                  column(6,downloadButton("downloadData","Download SNP dataset")),
                  column(6,checkboxInput(inputId = "example", label = "Add dataset", value = FALSE))
                )
             ),
             uiOutput("insertion")
      ),
      column(7, actionButton("go", "Analyse", width = "200px"))
    ))
  })
  
  #Data input
  output$insertion <- renderUI({
    if(input$example){
      tabPanel("Description1",fluidRow(
        box(title = "SNP Data Input", background = "yellow", strong("ATTENTION:"), " The file must contain the population with SNP data in a ", strong("comma-separated values (CSV)"), " format. Each row corresponds to an individual and each column to a SNP.",br(),
            fluidRow(
              column(6,fileInput('file1',"",accept=c('csv', '.csv','.CSV','.txt'), multiple = FALSE)),
              column(6,selectizeInput('ploidy',"Ploidy",choices = c(4,6),selected=4))
            )
        ),
        box(title = "GA dataset", solidHeader = TRUE, status = "primary", "Would you like to insert GA values already calculated? If you wish to check the data format, click on ",strong("download GA dataset"),".",br(),br(),
            fluidRow(
              column(6,downloadButton("downloadDataGAs","Download GA dataset")),
              column(6,checkboxInput(inputId = "gas", label = "Insert GA values", value = FALSE)))
        ),
        uiOutput("insertion_gas")
      ))
    }  
  })
  
  output$insertion_gas <- renderUI({
    if(input$gas){
      tabPanel("Description2",fluidRow(
        box(title = "GA Data Input", width=5, background = "red", strong("ATTENTION:"), " The file must contain the GA values in a ", strong("comma-separated values (CSV)"), " format. Each row corresponds to an individual and each column to the GA measures.",
            fluidRow(column(6,fileInput('file1_ga',"",accept=c('csv', '.csv','.CSV','.txt'), multiple = FALSE))))
      ))
    }
  })
  
  #Downloading data
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("SNPData_",Sys.Date(),".csv",sep="")
    },
    content = function(file){
      write.csv(markers$snps,file)
    }
  )
  
  output$downloadDataGAs <- downloadHandler(
    filename = function(){
      paste("GAData_",Sys.Date(),".csv",sep="")
    },
    content = function(file){
      write.csv(markers$GAs,file)
    }
  )
  
  #Page 2 - Selecting Parents %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  output$page2 <- renderUI({
    fluidRow(
      column(12,
             box(title = "Selecting Parents", solidHeader = TRUE, status = "primary", "Select the progeny parents. For performing the contaminant identification, it is ", strong("mandatory"), " to define the parents in the buttons below.")
      ),
      column(12, uiOutput("table"))
    )
  })
  
  #Rendering table
  output$table <- renderUI({
    if(input$go){
      uiOutput("table_snps", width = "100%")
    }
    else{
      tabPanel("Caution",fluidRow(
        box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("Input Data")," panel.")
      ))
    }
  })
  
  #Showing table
  output$table_snps <- renderUI({
    if((!is.null(input$file1)) & (input$example)){
      markers$snps <- read.csv(input$file1$datapath,row.names=1)
    }
    else{
      markers$snps <- read.csv("data/markers.csv",row.names=1)
    }
    
    fluidPage(
      tabPanel("Parents",fluidRow(
        column(2,selectizeInput("parent1","Parent 1",selected="P1",choices = row.names(markers$snps))),
        column(2,selectizeInput("parent2","Parent 2",selected="P2",choices = row.names(markers$snps))),
        column(2,sliderInput("parent1_clones","Clones for Parent 1",min=0,max=10,value=0,step=1)),
        column(2,sliderInput("parent2_clones","Clones for Parent 2",min=0,max=10,value=0,step=1)),
        column(2, actionButton("go_prog", "Select", width = "100px"))
      )),
      fluidRow(dataTableOutput("table_show"))
    )
  })
  
  output$table_show <- DT::renderDataTable(datatable(markers$snps[,1:6]),options = list(info = FALSE, dom = "ftp"))
  
  #Page 3 - Principal Component Analysis %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  output$page3 <- renderUI({
    if(input$go){
      if(input$go_prog){
        tabPanel("Run",fluidRow(
          column(12,box(title = "Principal Component Analysis", solidHeader = TRUE, status = "primary", "This multivariate technique might indicate contaminant patterns into the dataset. Try to insert parent clones into ", strong("Selecting Parents"), " panel.")),
          column(12, withSpinner(plotOutput("pca1", width = "90%"))),
          column(6, box(title = strong("Attention"), background = "yellow", "Would you like to run the analysis for ",strong("contaminant identification"),"?"),actionButton("go_pca", "Yes!", width = "100px")),
          column(6,downloadButton("PCADownload","Download PCA plot"))
          )
        )
      }
      else{
        tabPanel("Caution",fluidRow(
          box(title = strong("Attention"), background = "yellow", "You need to confirm the parents in the ",strong("Selecting Parents")," panel.")
        ))
      }
    }
    else{
      tabPanel("Caution",fluidRow(
        box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("Input Data")," panel.")
      ))
    }
  })
  
  output$pca1 <- renderPlot({
    parents <- c(input$parent1,input$parent2)
    clones_prog <- c(input$parent1_clones,input$parent2_clones)
    markers_pca <- markers$snps
    population <- row.names(markers_pca)
    types <- rep("Hybrid",length(population))
    types[population %in% input$parent1] <- "Parent 1"
    types[population %in% input$parent2] <- "Parent 2"
    
    for(i in 1:2){
      p <- parents[i]
      c <- clones_prog[i]
      
      if(c > 0){
        clones <- data.frame(matrix(rep(as.numeric(markers_pca[row.names(markers_pca) %in% p,]),times=c),nrow=c,byrow=T))
        clones <- data.frame(error_na(clones,0.05,0.05))
        names(clones) <- names(markers_pca)
        markers_pca <- rbind(markers_pca,clones)
        types <- c(types,rep("Artificial Clone",times=c))
      }
    }
    
    markers$pca <- markers_pca
    tpcaNipals <- pca(markers_pca, method = "nipals", nPcs = 5, evalPcs = 1:5)
    tscores <- data.frame(cbind(scores(tpcaNipals),as.data.frame(types)))
    colnames(tscores)[6] <- as.character("type")
    row.names(tscores) <- row.names(markers_pca)
    markers$pca_results <- tscores
    markers$tpcaNipals <- tpcaNipals
    
    markers$PCAplot <- ggplot(markers$pca_results,aes(x = PC1, y = PC2)) + geom_point(aes(col=type),alpha=0.7,size=3)+
      theme_bw() +
      theme(legend.title=element_blank()) + 
      xlab(paste("PC1 (", round(tpcaNipals@R2[1] * 100,2), "% of the variance explained)")) +
      ylab(paste("PC2 (", round(tpcaNipals@R2[2] * 100,2), "% of the variance explained)")) + 
      scale_color_brewer(palette = "Paired")
    
    print(markers$PCAplot)
  }, res = 96)
  
  #Downloading plot
  output$PCADownload <- downloadHandler(
    filename = function(){
      paste("PCAPlot_",Sys.Date(),".png",sep="")
    },
    content = function(file){
      png(file,pointsize=1,res=300,width=2000,height=1000)
      print(markers$PCAplot)
      dev.off()
    }
  )
  
  #Page 4a - GA-I %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  output$page4a <- renderUI({
    if(input$go){
      if(input$go_prog){
        if(input$go_pca){
          tabPanel("Run",fluidRow(
            withSpinner(uiOutput("contaminants_GAI"))
          ))
        }
        else{
          tabPanel("Caution",fluidRow(
            box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("Principal Component Analysis")," panel.")
          ))
        }
      }
      else{
        tabPanel("Caution",fluidRow(
          box(title = strong("Attention"), background = "yellow", "You need to confirm the parents in the ",strong("Selecting Parents")," panel.")
        ))
      }
    }
    else{
      tabPanel("Caution",fluidRow(
        box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("Input Data")," panel.")
      ))
    }
  })
  
  output$contaminants_GAI <- renderUI({
    
    #GAs
    if(!input$example){
      markers$GAs <- read.csv("data/gas.csv",row.names=1)
    }
    else{
      if(input$gas){
        markers$GAs <- read.csv(input$file1_ga$datapath,row.names=1)
      }
      else{
        markers$GAs <- calculateGAs(markers$snps,input$parent1,input$parent2,input$ploidy)
      }
    }
    
    #Clusters
    auxiliar_clusters <- findClusters(markers$GAs)
    markers$clusters <- auxiliar_clusters[[1]]
    markers$indexes_clusters <- auxiliar_clusters[[2]]
    
    #GA tables
    markers$GA_Clusters <- data.frame(markers$GAs,markers$clusters)
    
    tabPanel("GA-I",fluidRow(
      column(8,box(title = "Genotype Analysis I", solidHeader = TRUE, status = "primary", "The GA-I measure indicates apomictic clones (AC) into the individuals. Select the thresholds for excluding clusters as  ", strong("AC contaminants"),".")),column(4,tableOutput("table_indexes"))),
    fluidRow(
      column(8, align="center", plotOutput("histGAIa",width="90%")
      ),
      column(4,selectizeInput("ClusterScheme","Clustering Scheme Selection",choices=c("Clusters1","Clusters2","Clusters3"),selected="Clusters1")),
      column(4,sliderInput("GAIa","GA-I Threshold for Parent 1",min=0,max=1,value=0.75,step=0.05)),
      column(4,helpText("Select the minimum threshold for considering an individual as an apomictic clone from parent 1. All the individuals with GA-I values bigger than the threshold selected will be discarded.",br())),
      column(4,downloadButton("HistGAIaDownload","Download Histogram Plot")),
      column(8, align="center", plotOutput("histGAIb",width="90%")),
      column(4,sliderInput("GAIb","GA-I Threshold for Parent 2",min=0,max=1,value=0.75,step=0.05)),
      column(4,helpText("Select the minimum threshold for considering an individual as an apomictic clone from parent 2. All the individuals with GA-I values bigger than the threshold selected will be discarded.",br())),
      column(4,downloadButton("HistGAIbDownload","Download Histogram Plot")),
      column(12,box(title = strong("Attention"), background = "yellow", "Would you like to use the selected parameters?"),
             actionButton("removeGAI", "Yes!", width = "100px"))
    ))
    
  })
  
  output$table_indexes <- renderTable(markers$indexes_clusters)
  
  output$histGAIa <- renderPlot({
    choice <- sym(input$ClusterScheme)
    markers$GAIa_Hist <- ggplot(markers$GA_Clusters, aes(x=GAIa,fill=as.factor(!!choice),color=as.factor(!!choice))) +
      geom_histogram(alpha=0.5, position="identity", bins=300, center=0.5) +
      theme_bw() +
      theme(legend.title=element_blank())+
      scale_x_continuous(limits=c(0,1)) +
      labs(title="Histogram of GA-I (P1)", y="Count of Samples", caption="") + 
      geom_vline(xintercept = input$GAIa, color = "red", linetype = 3) +
      scale_color_brewer(palette = "Spectral") +
      scale_fill_brewer(palette = "Spectral")
    print(markers$GAIa_Hist)
  }, res = 96)
  
  output$histGAIb <- renderPlot({
    choice <- sym(input$ClusterScheme)
    markers$GAIb_Hist <-ggplot(markers$GA_Clusters, aes(x=GAIb,fill=as.factor(!!choice),color=as.factor(!!choice))) +
      geom_histogram(alpha=0.5, position="identity", bins=300, center=0.5) +
      theme_bw() +
      theme(legend.title=element_blank())+
      scale_x_continuous(limits=c(0,1)) +
      labs(title="Histogram of GA-I (P2)", y="Count of Samples", caption="") + 
      geom_vline(xintercept = input$GAIb, color = "red", linetype = 3) +
      scale_color_brewer(palette = "Spectral") +
      scale_fill_brewer(palette = "Spectral")
    print(markers$GAIb_Hist)
  }, res = 96)
  
  output$HistGAIaDownload <- downloadHandler(
    filename = function(){
      paste("HistGAIaPlot_",Sys.Date(),".png",sep="")
    },
    content = function(file){
      png(file,pointsize=1,res=300,width=2000,height=1000)
      print(markers$GAIa_Hist)
      dev.off()
    }
  )
  
  output$HistGAIbDownload <- downloadHandler(
    filename = function(){
      paste("HistGAIbPlot_",Sys.Date(),".png",sep="")
    },
    content = function(file){
      png(file,pointsize=1,res=300,width=2000,height=1000)
      print(markers$GAIb_Hist)
      dev.off()
    }
  )
  
  #Page 4b - GA-II %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  output$page4b <- renderUI({
    if(input$go){
      if(input$go_prog){
        if(input$go_pca){
          if(input$removeGAI){
            tabPanel("Run",fluidRow(
              uiOutput("contaminants_GAII")
            ))
          }
          else{
            tabPanel("Caution",fluidRow(
              box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("GA-I")," panel.")
            ))
          }
        }
        else{
          tabPanel("Caution",fluidRow(
            box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("Principal Component Analysis")," panel.")
          ))
        }
      }
      else{
        tabPanel("Caution",fluidRow(
          box(title = strong("Attention"), background = "yellow", "You need to confirm the parents in the ",strong("Selecting Parents")," panel.")
        ))
      }
    }
    else{
      tabPanel("Caution",fluidRow(
        box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("Input Data")," panel.")
      ))
    }
  })
  
  output$contaminants_GAII <- renderUI({
    fluidRow(
      column(12,uiOutput("ContType2")),
      column(12,box(title = strong("Attention"), background = "yellow", "Would you like to use the selected parameters?"),
             actionButton("removeGAII", "Yes!", width = "100px"))
    )
    
  })
  
  output$ContType2 <- renderUI({
    #Taking AC contaminants
    clusters <- markers$GA_Clusters[,input$ClusterScheme]
    markers_acs <- c()
    for(i in levels(as.factor(clusters))){
      median1 <- median(markers$GA_Clusters$GAIa[clusters == i])
      median2 <- median(markers$GA_Clusters$GAIb[clusters == i])
      if(median1 > input$GAIa){
        markers_acs <- c(markers_acs,row.names(markers$GA_Clusters)[clusters == i])
      }
      if(median2 > input$GAIb){
        markers_acs <- c(markers_acs,row.names(markers$GA_Clusters)[clusters == i])
      }
    }
    markers$acs <- markers_acs
    
    tabPanel("Contaminant 2",fluidRow(
      column(12,box(title = "Genotype Analysis II", solidHeader = TRUE, status = "primary", "The GA-II measure indicates self-fertilization of one of the parentals (SP). Select the thresholds for excluding clusters as  ", strong("SP contaminants"),".")),
      column(8, align="center", plotOutput("histGAIIa",width="90%")),
      column(4,sliderInput("GAIIa","GA-II Threshold for Parent 1",min=0,max=1,value=0.75,step=0.05)),
      column(4,helpText("Select the minimum threshold for considering an individual as originated from self-fertilization of parent 1. All the individuals with GA-II values bigger than the threshold selected will be discarded.",br())),
      column(4,downloadButton("HistGAIIaDownload","Download Histogram Plot")),
      column(8, align="center", plotOutput("histGAIIb",width="90%")),
      column(4,sliderInput("GAIIb","GA-II Threshold for Parent 2",min=0,max=1,value=0.75,step=0.05)),
      column(4,helpText("Select the minimum threshold for considering an individual as originated from self-fertilization of parent 2. All the individuals with GA-II values bigger than the threshold selected will be discarded.",br())),
      column(4,downloadButton("HistGAIIbDownload","Download Histogram Plot"))
    ))
  })
  
  output$histGAIIa <- renderPlot({
    choice <- sym(input$ClusterScheme)
    
    #Removing contaminants 1
    data_plot <- markers$GA_Clusters[!row.names(markers$GA_Clusters) %in% markers$acs,]
    
    markers$GAIIa_Hist <-ggplot(data_plot, aes(x=GAIIa,fill=as.factor(!!choice),color=as.factor(!!choice))) +
      geom_histogram(alpha=0.5, position="identity", bins=300, center=0.5) +
      theme_bw() +
      theme(legend.title=element_blank())+
      scale_x_continuous(limits=c(0,1)) +
      labs(title="Histogram of GA-II (P2)", y="Count of Samples", caption="") + 
      geom_vline(xintercept = input$GAIIa, color = "red", linetype = 3) +
      scale_color_brewer(palette = "Spectral") +
      scale_fill_brewer(palette = "Spectral")
    print(markers$GAIIa_Hist)
  }, res = 96)
  
  output$histGAIIb <- renderPlot({
    choice <- sym(input$ClusterScheme)
    
    #Removing contaminants 1
    data_plot <- markers$GA_Clusters[!row.names(markers$GA_Clusters) %in% markers$acs,]
    
    markers$GAIIb_Hist <-ggplot(data_plot, aes(x=GAIIb,fill=as.factor(!!choice),color=as.factor(!!choice))) +
      geom_histogram(alpha=0.5, position="identity", bins=300, center=0.5) +
      theme_bw() +
      theme(legend.title=element_blank())+
      scale_x_continuous(limits=c(0,1)) +
      labs(title="Histogram of GA-II (P2)", y="Count of Samples", caption="") + 
      geom_vline(xintercept = input$GAIIb, color = "red", linetype = 3) +
      scale_color_brewer(palette = "Spectral") +
      scale_fill_brewer(palette = "Spectral")
    print(markers$GAIIb_Hist)
  }, res = 96)

  output$HistGAIIaDownload <- downloadHandler(
    filename = function(){
      paste("HistGAIIaPlot_",Sys.Date(),".png",sep="")
    },
    content = function(file){
      png(file,pointsize=1,res=300,width=2000,height=1000)
      print(markers$GAIIa_Hist)
      dev.off()
    }
  )
  
  output$HistGAIIbDownload <- downloadHandler(
    filename = function(){
      paste("HistGAIIbPlot_",Sys.Date(),".png",sep="")
    },
    content = function(file){
      png(file,pointsize=1,res=300,width=2000,height=1000)
      print(markers$GAIIb_Hist)
      dev.off()
    }
  )
  
  #Page 4c - GA-III %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  output$page4c <- renderUI({
    if(input$go){
      if(input$go_prog){
        if(input$go_pca){
          if(input$removeGAI){
            if(input$removeGAII){
              tabPanel("Run",fluidRow(
                uiOutput("contaminants_GAIII")
              ))
            }
            else{
              tabPanel("Caution",fluidRow(
                box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("GA-II")," panel.")
              ))
            }
          }
          else{
            tabPanel("Caution",fluidRow(
              box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("GA-I")," panel.")
            ))
          }
        }
        else{
          tabPanel("Caution",fluidRow(
            box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("Principal Component Analysis")," panel.")
          ))
        }
      }
      else{
        tabPanel("Caution",fluidRow(
          box(title = strong("Attention"), background = "yellow", "You need to confirm the parents in the ",strong("Selecting Parents")," panel.")
        ))
      }
    }
    else{
      tabPanel("Caution",fluidRow(
        box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("Input Data")," panel.")
      ))
    }
  })
  
  output$contaminants_GAIII <- renderUI({
    fluidRow(
      column(12,uiOutput("ContType3")),
      column(12,box(title = strong("Attention"), background = "yellow", "Would you like to use the selected parameters?"),
             actionButton("removeGAIII", "Yes!", width = "100px"))
    )
  })
  
  output$ContType3 <- renderUI({
    #Taking SP contaminants
    markers_sps <- c()
    data_aux <- markers$GA_Clusters[!row.names(markers$GA_Clusters) %in% markers$acs,]
    clusters <- data_aux[,input$ClusterScheme]
    
    for(i in levels(as.factor(clusters))){
      median1 <- median(data_aux$GAIIa[clusters == i])
      median2 <- median(data_aux$GAIIb[clusters == i])
      if(median1 > input$GAIIa){
        markers_sps <- c(markers_sps,row.names(data_aux)[clusters == i])
      }
      if(median2 > input$GAIIb){
        markers_sps <- c(markers_snps,row.names(data_aux)[clusters == i])
      }
    }
    
    markers$sps <- markers_sps
    data_aux2 <- markers$GA_Clusters[!(row.names(markers$GA_Clusters) %in% markers$acs | row.names(markers$GA_Clusters) %in% markers$sps),]
    clusters <- unique(data_aux2[,input$ClusterScheme])
    
    #Identificate
    #Identificate FS and HS contaminants
    #Taking others
    medians <- c()
    put_remov <- c()
    mmm <- data.frame()
    data_aux3 <- data.frame()
    
    for(i in clusters){
      gas3_aux <- markers$GA_Clusters$GAIII[markers$GA_Clusters[,input$ClusterScheme] == i]
      mmm <- rbind(mmm,c(i,median(gas3_aux),min(gas3_aux),max(gas3_aux)))
      put_remov <- c(put_remov,i)
    }
    
    colnames(mmm) <- c("cluster", "median", "min", "max")
    mmm <- arrange(mmm, median)
    
    keep_hp <- mmm$cluster[1]
    
    if(length(put_remov) > 1){
      for(Z in 2:dim(mmm)[1])
        if(mmm$min[Z] < mmm$max[Z-1]){
          keep_hp <- c(keep_hp, mmm$cluster[Z])
        } else {
          break
        }
    }
    
    removal_hsfc <- c(setdiff(put_remov, keep_hp))
    markers$hsfc_clusters <- removal_hsfc
    markers$hsfc <- row.names(markers$GA_Clusters[markers$GA_Clusters[,input$ClusterScheme] %in% markers$hsfc_clusters,])
    
    tabPanel("Contaminant 2",fluidRow(
      column(12,box(title = "Genotype Analysis III", solidHeader = TRUE, status = "primary", "The GA-III measure indicates full contaminants (FC) and half-siblings (HS). Click on the checkbox to keep or not  putative ", strong("FC and HS contaminants"),". Histograms far from the main group is an indication of such contamination.")),
      column(8, align="center", plotOutput("histGAIII", width="90%")),
      column(4,checkboxGroupInput("GAIIIContamination","GA-III Contamination Groups",choices = clusters, selected = markers$hsfc_clusters)),
      column(4,helpText("Mark/dismark groups which must not be considered FC and HS contaminants",br())),
      column(4,downloadButton("HistGAIIIDownload","Download Histogram Plot"))
    ))
  })
  
  output$histGAIII <- renderPlot({
    choice <- sym(input$ClusterScheme)
    
    #Removing contaminants 1 and 2
    data_plot <- markers$GA_Clusters[!((row.names(markers$GA_Clusters) %in% markers$acs)|(row.names(markers$GA_Clusters) %in% markers$sps)),]
    
    markers$GAIII_Hist <- ggplot(data_plot, aes(x=GAIII,fill=as.factor(!!choice),color=as.factor(!!choice))) +
      geom_histogram(alpha=0.5, position="identity", bins=300) +
      theme_bw() + 
      theme(legend.title=element_blank())+
     # scale_x_continuous(limits=c(0,1)) +
      labs(title="Histogram of GA-III", y="Count of Samples", caption="") + 
      scale_color_brewer(palette = "Spectral") +
      scale_fill_brewer(palette = "Spectral")
    
    print(markers$GAIII_Hist)
  }, res=96)
  
  output$HistGAIIIDownload <- downloadHandler(
    filename = function(){
      paste("HistGAIIIPlot_",Sys.Date(),".png",sep="")
    },
    content = function(file){
      png(file,pointsize=1,res=300,width=2000,height=1000)
      print(markers$GAIII_Hist)
      dev.off()
    }
  )
  
  #Page 4 - Check %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  output$page4 <- renderUI({
    if(input$go){
      if(input$go_prog){
        if(input$go_pca){
          if(input$removeGAI){
            if(input$removeGAII){
              if(input$removeGAIII){
                tabPanel("Run",fluidRow(
                  uiOutput("contaminants_all")
                ))
              }
              else{
                tabPanel("Caution",fluidRow(
                  box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("GA-III")," panel.")
                ))
              }
            }
            else{
              tabPanel("Caution",fluidRow(
                box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("GA-II")," panel.")
              ))
            }
          }
          else{
            tabPanel("Caution",fluidRow(
              box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("GA-I")," panel.")
            ))
          }
        }
        else{
          tabPanel("Caution",fluidRow(
            box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("Principal Component Analysis")," panel.")
          ))
        }
      }
      else{
        tabPanel("Caution",fluidRow(
          box(title = strong("Attention"), background = "yellow", "You need to confirm the parents in the ",strong("Selecting Parents")," panel.")
        ))
      }
    }
    else{
      tabPanel("Caution",fluidRow(
        box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("Input Data")," panel.")
      ))
    }
  })
  
  output$contaminants_all <- renderUI({
    acs <- c(markers$acs)
    sps <- c(markers$sps)
    hsfc <- c(markers$hsfc)
    contaminants_final <- c(acs,sps,hsfc)
    contaminants_final_types <- c(rep("AC",length(acs)),rep("SP",length(sps)),rep("HS/FC",length(hsfc)))
    markers$table_cont <- data.frame()
    
    if(length(contaminants_final) >= 1){
      markers$table_cont <- data.frame(Contaminants=contaminants_final,Types=contaminants_final_types)
    }
    
    tabPanel("Contaminants",fluidRow(
      column(12,box(title = "Contaminant Identification", solidHeader = TRUE, status = "primary", "Based on the selected parameters, the PCA with ", strong("contaminant information")," is displayed. If you wish to reconsider an individual removal, uncheck its name on the checkbox table below.")),
      column(12, align="center", withSpinner(plotOutput("pca2", width = "90%"))),
      column(12, align="center", downloadButton("PCA2Download","Download PCA Plot")),
      column(12, HTML("<br/>")),
      column(12, uiOutput("cont_check"))
    ))
  })
  
  output$pca2 <- renderPlot({
    pca_data2 <- markers$pca_results
    pca_data2$type <- as.character(markers$pca_results$type)
    pca_data2$type[row.names(pca_data2) %in% markers$acs] <- "Apomictic Clone"
    pca_data2$type[row.names(pca_data2) %in% markers$sps] <- "Self Fertilization"
    pca_data2$type[row.names(pca_data2) %in% markers$hsfc] <- "Full Cont/Half Sibling"
    pca_data2$type <- as.factor(pca_data2$type)
    
    markers$PCAplot2 <- ggplot(pca_data2,aes(x = PC1, y = PC2)) + geom_point(aes(col=type),alpha=0.7,size=3)+
      theme_bw() +
      theme(legend.title=element_blank()) + 
      xlab(paste("PC1 (", round(markers$tpcaNipals@R2[1] * 100,2), "% of the variance explained)")) +
      ylab(paste("PC2 (", round(markers$tpcaNipals@R2[2] * 100,2), "% of the variance explained)")) + 
      scale_color_brewer(palette = "Paired")
    print(markers$PCAplot2)
  }, res = 96)
  
  output$cont_check <- renderUI({
    tabPanel("CheckContam",fluidRow(
        column(12, align="center", box(width=12,title="Apomictic Clones", solidHeader = F, status = "primary", checkboxGroupInput("AC",NULL,markers$acs,selected=markers$acs,inline=T))),
        column(12, align="center", box(width=12,title="Self-fertilization", solidHeader = F, status = "primary", checkboxGroupInput("SP",NULL,markers$sps,selected=markers$sps,inline=T))),
        column(12, align="center", box(width=12,title="Full Contaminant/Half Sibling", solidHeader = F, status = "primary", checkboxGroupInput("HSFC",NULL,markers$hsfc,selected=markers$hsfc,inline=T))),
        column(12,box(title = strong("Attention"), background = "yellow", "Would you like to discard the individuals selected?"),actionButton("removeContFinal", "Yes!", width = "100px"))
      ))
  })
  
  #Downloading plot
  output$PCA2Download <- downloadHandler(
    filename = function(){
      paste("PCAPlot_",Sys.Date(),".png",sep="")
    },
    content = function(file){
      png(file,pointsize=1,res=300,width=2000,height=1000)
      print(markers$PCAplot2)
      dev.off()
    }
  )
  
  #Page 5 - Final Results %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  output$page5 <- renderUI({
    if(input$go){
      if(input$go_prog){
        if(input$go_pca){
          if(input$removeGAI){
            if(input$removeGAII){
              if(input$removeGAIII){
                if(input$removeContFinal){
                  uiOutput("finalResults")
                }
                else{
                  tabPanel("Caution",fluidRow(
                    box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("Contaminant Identification")," panel.")
                  ))
                }
              }
              else{
                tabPanel("Caution",fluidRow(
                  box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("GA-III")," panel.")
                ))
              }
            }
            else{
              tabPanel("Caution",fluidRow(
                box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("GA-II")," panel.")
              ))
            }
          }
          else{
            tabPanel("Caution",fluidRow(
              box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("GA-I")," panel.")
            ))
          }
        }
        else{
          tabPanel("Caution",fluidRow(
            box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("Principal Component Analysis")," panel.")
          ))
        }
      }
      else{
        tabPanel("Caution",fluidRow(
          box(title = strong("Attention"), background = "yellow", "You need to confirm the parents in the ",strong("Selecting Parents")," panel.")
        ))
      }
    }
    else{
      tabPanel("Caution",fluidRow(
        box(title = strong("Attention"), background = "yellow", "You need to confirm the analysis in the ",strong("Input Data")," panel.")
      ))
    }
  })
  
  output$finalResults <- renderUI({
    tabPanel("Run",fluidRow(
      column(12,box(title = "Principal Component Analysis", solidHeader = TRUE, status = "primary", "This is the final PCA without the ", strong("contaminants"), " removed. Below you can download all the calculated measures of polyCID and also the final marker dataset filtered.")),
      column(12, withSpinner(plotOutput("pcaFinal", width = "90%"))),
      column(12, align="center", downloadButton("PCADownloadFinal","Download PCA plot")),
      column(12, HTML("<br/>")),
      column(6, box(width=12, title = strong("Download Calculations"), background = "yellow", "For downloading intermediate results for further calculations, select the buttons below.",HTML("<br/><br/>"),downloadButton("DownloadGAS_Final","Download GA measures calculated"),HTML("<br/><br/>"),downloadButton("DownloadClusters_Final","Download Cluster Results"))),
      column(6, box(width=12, title = strong("Download Final Dataset"), background = "yellow", "For downloading the final dataset without contaminants, select the buttons below.", HTML("<br/><br/>"), downloadButton("DownloadFinalData","Download the Final Marker Dataset")))
    )
    )
  })
  
  output$pcaFinal <- renderPlot({
    parents <- c(input$parent1,input$parent2)
    markers_pca <- markers$snps
    population <- row.names(markers_pca)
    types <- rep("Hybrid",length(population))
    types[population %in% input$parent1] <- "Parent 1"
    types[population %in% input$parent2] <- "Parent 2"
    contaminants <- c(input$AC,input$SP,input$HSFC)
    markers_pca <- markers_pca[!population %in% contaminants, ]
    types <- types[!population %in% contaminants]
    markers$Final <- markers_pca
    
    markers$pcaFinal <- markers_pca
    tpcaNipals <- pca(markers_pca, method = "nipals", nPcs = 5, evalPcs = 1:5)
    tscores <- data.frame(cbind(scores(tpcaNipals),as.data.frame(types)))
    colnames(tscores)[6] <- as.character("type")
    row.names(tscores) <- row.names(markers_pca)
    
    markers$PCAplotFinal <- ggplot(tscores,aes(x = PC1, y = PC2)) + geom_point(aes(col=type),alpha=0.7,size=3)+
      theme_bw() +
      theme(legend.title=element_blank()) + 
      xlab(paste("PC1 (", round(tpcaNipals@R2[1] * 100,2), "% of the variance explained)")) +
      ylab(paste("PC2 (", round(tpcaNipals@R2[2] * 100,2), "% of the variance explained)")) + 
      scale_color_brewer(palette = "Paired")
    
    print(markers$PCAplotFinal)
  }, res = 96)
  
  output$PCADownloadFinal <- downloadHandler(
    filename = function(){
      paste("PCAPlotFinal_",Sys.Date(),".png",sep="")
    },
    content = function(file){
      png(file,pointsize=1,res=300,width=2000,height=1000)
      print(markers$PCAplotFinal)
      dev.off()
    }
  )
  
  #Downloading data
  output$DownloadFinalData <- downloadHandler(
    filename = function(){
      paste("SNPDataFinal_",Sys.Date(),".csv",sep="")
    },
    content = function(file){
      write.csv(markers$Final,file)
    }
  )
  
  output$DownloadGAS_Final <- downloadHandler(
    filename = function(){
      paste("GAData_",Sys.Date(),".csv",sep="")
    },
    content = function(file){
      write.csv(markers$GAs,file)
    }
  )
  
  output$DownloadClusters_Final <- downloadHandler(
    filename = function(){
      paste("ClusterData_",Sys.Date(),".csv",sep="")
    },
    content = function(file){
      write.csv(markers$clusters,file)
    }
  )
  
})
