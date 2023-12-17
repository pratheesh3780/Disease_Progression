lpsfe_server <- function(input, output,session) {
  
  
  
  ########################## file_upload
  csvfile_sfe <- shiny::reactive({
    csvfile_sfe <- input$file1_sfe
    if (is.null(csvfile_sfe)){return(NULL)}
    dt_sfe <- read.csv(csvfile_sfe$datapath, header=input$header_sfe, sep=",")
    dt_sfe
  })
  
  output$var_sfe  <- shiny::renderUI({
    if(is.null(input$file1_sfe$datapath)){
      return()
    }
    else{
      list ( shiny::radioButtons("trt_sfe", "Select the treatment column", choices = names(csvfile_sfe())),
        shiny::radioButtons("sev_sfe", "Select the disease sevierity column)", choices = names(csvfile_sfe())),
            shiny::radioButtons("time_sfe", "Select the time/period column", choices = names(csvfile_sfe())),
            shiny:: numericInput("initial_sfe", "Initial disease intensity (y0):", 0.01),
            shiny:: numericInput("infr_sfe", "Apparent infection rate (r):", 0.3),
            shiny:: numericInput("k_sfe", "Maximum disease intensity:", 0.8),
            shiny:: numericInput("maxitr_sfe", "Maximum Iterations:", 500),
            shinyWidgets::actionBttn(
              inputId = "submit_sfe",
              label = "Analyze!",
              color = "danger",
              style = "jelly"
            )
      )
    }
  })
  
  
  ############### plotting DPC
  plotInput_basicplot_sfe <- shiny::reactive({
    if (is.null(input$file1_sfe$datapath)) {
      return()
    }
    if (is.null(input$submit_sfe)) {
      return()
    }
    if (input$submit_sfe > 0) {
      x <- as.matrix(csvfile_sfe()[, input$time_sfe])
      y <- as.matrix(csvfile_sfe()[, input$sev_sfe])
      z <- as.matrix(csvfile_sfe()[, input$trt_sfe])
      sfe_data <- data.frame(
        time = x,
        sev = y,
        factor = z
      )
      n<-nlevels(as.factor(sfe_data$factor))
  p<-ggplot(aes(time,sev),data = sfe_data)+
    geom_line()+
    geom_point()+
    theme_minimal()+
    labs(x=input$xlab_sfe, 
         y= input$ylab_sfe)+
    ggtitle(input$title_sfe)+
    facet_wrap(~factor,ncol=n)
    }
    if (is.null(input$theme_sfe)) {
      return(p)
    }
    if (input$theme_sfe == "normal") {
      q <- p + theme_bw()
    } else if (input$theme_sfe == "economist") {
      q <- p + ggthemes::theme_economist() 
    } else if (input$theme_sfe == "grey") {
      q <- p + theme_gray() 
    } 
    else if (input$theme_sfe == "minimal") {
      q <- p + theme_minimal()
    }else if (input$theme_sfe == "light") {
      q <- p + theme_light()
    }
    else if (input$theme_sfe == "void") {
      q <- p + theme_void()
    }
    else if (input$theme_sfe == "tufte") {
      q <- p + ggthemes::theme_tufte()
    }
    else if (input$theme_sfe == "stata") {
      q <- p + ggthemes::theme_stata()
    }
    else if (input$theme_sfe == "wsj") {
      q <- p + ggthemes::theme_wsj()
    }
    else if (input$theme_sfe == "calc") {
      q <- p + ggthemes::theme_calc()
    }
    else if (input$theme_sfe == "hc") {
      q <- p + ggthemes::theme_hc()
    }
  q
    
    
    
    
  })
  
  ########## plotting end
  ################################plot output 
  output$sfe_basicplot <- shiny::renderPlot( {
    plotInput_basicplot_sfe()
  },
  bg = "transparent"
  ) 
  
###############  
  
  ############## control panel for basic plot
  output$cp_sfe <- shiny::renderUI({
    if (is.null(input$file1_sfe$datapath)){return()}
    if (is.null(input$submit_sfe)){return()}
    if (input$submit_sfe > 0) {
      list(
        fluidRow(
          column(4,
                 shiny::textInput("xlab_sfe", "Enter required x-axis title", "Time (Days)")
          ),
          column(4,
                 shiny::textInput("ylab_sfe", "Enter required y-axis title", "Disease Severity")
          ),
          column(4,
                 shiny::textInput("title_sfe", "Enter required title", "Disease progress curve")
          ),
          column(
            4,
            shiny::selectInput(
              "theme_sfe", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          )
        )
        )
      
      
      }
  })

  
  ############## control panel for model plot
  output$cp_modplot_sfe <- shiny::renderUI({
    if (is.null(input$file1_sfe$datapath)){return()}
    if (is.null(input$submit_sfe)){return()}
    if (input$submit_sfe > 0) {
      list(
        fluidRow(
          column(4,
                 shiny::textInput("xlabm_sfe", "Enter required x-axis title", "Time (Days)")
          ),
          column(4,
                 shiny::textInput("ylabm_sfe", "Enter required y-axis title", "Disease Severity")
          ),
          column(
            4,
            shiny::selectInput(
              "themem_sfe", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          ),
          column(
            4,
            shiny::selectInput(
              "model_sfe", "Choose the model to fit:",
              list("Gompertz","Monomolecular", "Logistic")
            )
          )
        )
      )
      
      
    }
  })
  
  
 ########################## modelling
  ########### Model fitting
  
  observeEvent(input$submit_sfe, {
    x <- as.matrix(csvfile_sfe()[, input$time_sfe])
    y <- as.matrix(csvfile_sfe()[, input$sev_sfe])
    z <- as.matrix(csvfile_sfe()[, input$trt_sfe])
    sfe_data <- data.frame(
      time = x,
      sev = y,
      factor = z
    )
  
    # warning appear in console due to this
    model_multi<-fit_multi(time_col="time",
                           intensity_col="sev",
                           data=sfe_data,
                           strata_cols="factor",
                          starting_par = list(
                           y0 = as.numeric(input$initial_sfe),
                           r = as.numeric(input$infr_sfe),
                           K = as.numeric(input$k_sfe)
                           ),
                           maxiter=as.numeric(input$maxitr_sfe),
                           nlin = TRUE,
                           estimate_K = TRUE)


    output$sfe_stats <- renderTable({
      stats_sfe<-model_multi$Parameters
      entry_sfe <- c("factor","model", "CCC", "r_squared","RSE","best_model")
      new_stats_sfe<-stats_sfe[, (names(stats_sfe) %in% entry_sfe)]
      colnames(new_stats_sfe)<-c("Factor","Model","CCC","R\u00B2","RMSE","Rank")
      new_stats_sfe
    },
    digits = 3,
    caption = ("<b> Population Model fit statistics </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
    )
    output$sfe_infection_rate <- renderTable({
      rate_sfe<-model_multi$Parameters
      drops <- c("CCC", "r_squared","RSE","best_model","df","y0_ci_lwr","y0_ci_upr","r_ci_lwr","r_ci_upr","K_ci_lwr","K_ci_upr")
      new_rate_sfe<-rate_sfe[, !(names(rate_sfe) %in% drops)]
      colnames(new_rate_sfe)<-c("Factor","Model","y0","SE(y0)","r","SE(r)","K","SE(K)")
      new_rate_sfe
    },
    digits = 4,
    caption = ("<b> Estimate of Infection rate (r), Initial disease intensity (y0), Maximum disease intensity (K) </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
    )
    
    output$sfe_initial_inoculum <- renderTable({
      ci_sfe<-model_multi$Parameters
      entry_ci <- c("factor","model","r_ci_lwr","r_ci_upr")
      new_ci_sfe<-ci_sfe[, (names(ci_sfe) %in% entry_ci)]
      colnames(new_ci_sfe)<-c("Factor","Model","Lower(r)","Upper(r)")
      new_ci_sfe
    },
    digits = 4,
    caption = ("<b> 95% confidence interval of infection rate (r) estimates</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
    ) 
    
    output$sfe_initial_inoculum1 <- renderTable({
      ci_sfe2<-model_multi$Parameters
      entry_ci2 <- c("factor","model","y0_ci_lwr","y0_ci_upr")
      new_ci_sfe2<-ci_sfe2[, (names(ci_sfe2) %in% entry_ci2)]
      colnames(new_ci_sfe2)<-c("Factor","Model","Lower(y0)","Upper(y0)")
      new_ci_sfe2
    },
    digits = 4,
    caption = ("<b> 95% confidence interval of initial inoculum (y0) estimates</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
    ) 
    
    
    
    output$sfe_initial_inoculum2 <- renderTable({
      ci_sfe1<-model_multi$Parameters
      entry_ci1 <- c("factor","model","K_ci_lwr","K_ci_upr")
      new_ci_sfe1<-ci_sfe1[, (names(ci_sfe1) %in% entry_ci1)]
      colnames(new_ci_sfe1)<-c("Factor","Model","Lower(K)","Upper(K)")
      new_ci_sfe1
    },
    digits = 4,
    caption = ("<b> 95% confidence interval of K</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
    ) 
    
    output$sfe_AUDPC <- renderTable({
      audc_sfe<-sfe_data %>% 
        group_by(factor)%>%
        summarise(audpc=AUDPC(time=time,y=sev),
                  audps=AUDPS(time=time,y=sev),
        )
      audc_sfe<-as.data.frame(audc_sfe)
      audc_sfe
    },
    digits = 3,
    caption = ("<b> Area under disease progress curve </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
    ) 
    
    output$sfe_modelplot <- shiny::renderPlot({
      if (!is.null(input$model_sfe)){
        p<-model_multi$Data %>%
          filter(model==as.character(input$model_sfe))%>%
          ggplot(aes(color=factor))+
          geom_point(aes(time,y))+
          geom_line(aes(time, predicted),linewidth=0.6)+
          labs(x=input$xlabm_sfe, 
               y= input$ylabm_sfe)+
          ggtitle(input$model_sfe)+
          theme_minimal()
      }
      if (is.null(input$themem_sfe)) {
        return(p)
      }
      if (input$themem_sfe == "normal") {
        q <- p + theme_bw()
      } else if (input$themem_sfe == "economist") {
        q <- p + ggthemes::theme_economist() 
      } else if (input$themem_sfe == "grey") {
        q <- p + theme_gray() 
      } 
      else if (input$themem_sfe == "minimal") {
        q <- p + theme_minimal()
      }else if (input$themem_sfe == "light") {
        q <- p + theme_light()
      }
      else if (input$themem_sfe == "void") {
        q <- p + theme_void()
      }
      else if (input$themem_sfe == "tufte") {
        q <- p + ggthemes::theme_tufte()
      }
      else if (input$themem_sfe == "stata") {
        q <- p + ggthemes::theme_stata()
      }
      else if (input$theme_sfe == "wsj") {
        q <- p + ggthemes::theme_wsj()
      }
      else if (input$themem_sfe == "calc") {
        q <- p + ggthemes::theme_calc()
      }
      else if (input$themem_sfe == "hc") {
        q <- p + ggthemes::theme_hc()
      }
      q
      
    },
    bg = "transparent"
    )
    
    
})

  
  ################  

  
  
  
  ############################# download data set
  output$data_set_sfe <- shiny::renderUI({
    
    list(
      shiny::selectInput(
        "filenames_sfe", "Choose a dataset:",
        list.files(
          pattern = c("single_factor.csv")
        )
      ),
      shiny::downloadButton("downloadData_sfe", label = "Download csv file", class = "butt11",)
    )
    
    
    
  })
  
  datasetInput = shiny::reactive({
    switch(input$filenames_sfe,
           filenames_sfe
    )
  })
  
  output$downloadData_sfe = shiny::downloadHandler(
    filename = function() {
      input$filenames_sfe
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_sfe, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  )
  ######################### end data set download
}