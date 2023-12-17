lptfe_server <- function(input, output,session) {
  
  
  
  ########################## file_upload
  csvfile_tfe <- shiny::reactive({
    csvfile_tfe <- input$file1_tfe
    if (is.null(csvfile_tfe)){return(NULL)}
    dt_tfe <- read.csv(csvfile_tfe$datapath, header=input$header_tfe, sep=",")
    dt_tfe
  })
  
  output$var_tfe  <- shiny::renderUI({
    if(is.null(input$file1_tfe$datapath)){
      return()
    }
    else{
      list ( shiny::radioButtons("trt1_tfe", "Select Factor1", choices = names(csvfile_tfe())),
             shiny::radioButtons("trt2_tfe", "Select Factor2", choices = names(csvfile_tfe())),
             shiny::radioButtons("sev_tfe", "Select the disease sevierity column)", choices = names(csvfile_tfe())),
             shiny::radioButtons("time_tfe", "Select the time/period column", choices = names(csvfile_tfe())),
             shiny:: numericInput("initial_tfe", "Initial disease intensity (y0):", 0.01),
             shiny:: numericInput("infr_tfe", "Apparent infection rate (r):", 0.3),
             shiny:: numericInput("k_tfe", "Maximum disease intensity:", 0.8),
             shiny:: numericInput("maxitr_tfe", "Maximum Iterations:", 500),
             shinyWidgets::actionBttn(
               inputId = "submit_tfe",
               label = "Analyze!",
               color = "danger",
               style = "jelly"
             )
      )
    }
  })
  
  
  ############### plotting DPC
  plotInput_basicplot_tfe <- shiny::reactive({
    if (is.null(input$file1_tfe$datapath)) {
      return()
    }
    if (is.null(input$submit_tfe)) {
      return()
    }
    if (input$submit_tfe > 0) {
      x <- as.matrix(csvfile_tfe()[, input$time_tfe])
      y <- as.matrix(csvfile_tfe()[, input$sev_tfe])
      z <- as.matrix(csvfile_tfe()[, input$trt1_tfe])
      t <- as.matrix(csvfile_tfe()[, input$trt2_tfe])
      tfe_data <- data.frame(
        time = x,
        sev = y,
        factor1_tfe = z,
        factor2_tfe = t
      )
      n2<-nlevels(as.factor(tfe_data$factor2_tfe))
      p<-ggplot(aes(time, sev, color = factor1_tfe),data = tfe_data)+
        geom_line()+
        geom_point()+
        theme_minimal()+
        labs(x=input$xlab_tfe, 
             y= input$ylab_tfe)+
        ggtitle(input$title_tfe)+
        facet_wrap(~factor2_tfe, ncol = n2)
   
    }
    if (is.null(input$theme_tfe)) {
      return(p)
    }
    if (input$theme_tfe == "normal") {
      q <- p + theme_bw()
    } else if (input$theme_tfe == "economist") {
      q <- p + ggthemes::theme_economist() 
    } else if (input$theme_tfe == "grey") {
      q <- p + theme_gray() 
    } 
    else if (input$theme_tfe == "minimal") {
      q <- p + theme_minimal()
    }else if (input$theme_tfe == "light") {
      q <- p + theme_light()
    }
    else if (input$theme_tfe == "void") {
      q <- p + theme_void()
    }
    else if (input$theme_tfe == "tufte") {
      q <- p + ggthemes::theme_tufte()
    }
    else if (input$theme_tfe == "stata") {
      q <- p + ggthemes::theme_stata()
    }
    else if (input$theme_tfe == "wsj") {
      q <- p + ggthemes::theme_wsj()
    }
    else if (input$theme_tfe == "calc") {
      q <- p + ggthemes::theme_calc()
    }
    else if (input$theme_tfe == "hc") {
      q <- p + ggthemes::theme_hc()
    }
    q
    
    
    
    
  })
  
  ########## plotting end
  ################################plot output 
  output$tfe_basicplot <- shiny::renderPlot( {
    plotInput_basicplot_tfe()
  },
  bg = "transparent"
  ) 
  
  ###############  
  
  ############## control panel for basic plot
  output$cp_tfe <- shiny::renderUI({
    if (is.null(input$file1_tfe$datapath)){return()}
    if (is.null(input$submit_tfe)){return()}
    if (input$submit_tfe > 0) {
      list(
        fluidRow(
          column(4,
                 shiny::textInput("xlab_tfe", "Enter required x-axis title", "Time (Days)")
          ),
          column(4,
                 shiny::textInput("ylab_tfe", "Enter required y-axis title", "Disease Severity")
          ),
          column(4,
                 shiny::textInput("title_tfe", "Enter required title", "Disease progress curve")
          ),
          column(
            4,
            shiny::selectInput(
              "theme_tfe", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          )
        )
      )
      
      
    }
  })
  
  
  ############## control panel for model plot
  output$cp_modplot_tfe <- shiny::renderUI({
    if (is.null(input$file1_tfe$datapath)){return()}
    if (is.null(input$submit_tfe)){return()}
    if (input$submit_tfe > 0) {
      list(
        fluidRow(
          column(4,
                 shiny::textInput("xlabm_tfe", "Enter required x-axis title", "Time (Days)")
          ),
          column(4,
                 shiny::textInput("ylabm_tfe", "Enter required y-axis title", "Disease Severity")
          ),
          column(
            4,
            shiny::selectInput(
              "themem_tfe", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          ),
          column(
            4,
            shiny::selectInput(
              "model_tfe", "Choose the model to fit:",
              list("Gompertz","Monomolecular", "Logistic")
            )
          )
        )
      )
      
      
    }
  })
  
  
  ########################## modelling
  ########### Model fitting
  
  observeEvent(input$submit_tfe, {
    x <- as.matrix(csvfile_tfe()[, input$time_tfe])
    y <- as.matrix(csvfile_tfe()[, input$sev_tfe])
    z <- as.matrix(csvfile_tfe()[, input$trt1_tfe])
    t <- as.matrix(csvfile_tfe()[, input$trt2_tfe])
    tfe_data <- data.frame(
      time = x,
      sev = y,
      factor1_tfe = z,
      factor2_tfe = t
    )
    
    # warning appear in console due to this
    
    model_multi2<-fit_multi(time_col="time",
                            intensity_col="sev",
                            data= tfe_data,
                            strata_cols=c("factor1_tfe","factor2_tfe") ,
                            starting_par = list(
                              y0 = as.numeric(input$initial_tfe),
                              r = as.numeric(input$infr_tfe),
                              K = as.numeric(input$k_tfe)
                            ),
                            maxiter=as.numeric(input$maxitr_tfe),
                            nlin = TRUE,
                            estimate_K = TRUE)
  
    
    output$tfe_stats <- renderTable({
      stats_tfe<-model_multi2$Parameters
      entry_tfe <- c("factor1_tfe","factor2_tfe","model", "CCC", "r_squared","RSE","best_model")
      new_stats_tfe<-stats_tfe[, (names(stats_tfe) %in% entry_tfe)]
      colnames(new_stats_tfe)<-c("Factor1","Factor2","Model","CCC","R\u00B2","RMSE","Rank")
      new_stats_tfe
    },
    digits = 3,
    caption = ("<b> Population Model fit statistics </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
    )
    
    
    output$tfe_infection_rate <- renderTable({
      rate_tfe<-model_multi2$Parameters
      drops <- c("y0","y0_se","K","K_se","CCC", "r_squared","RSE","best_model","df","y0_ci_lwr","y0_ci_upr","r_ci_lwr","r_ci_upr","K_ci_lwr","K_ci_upr")
      new_rate_tfe<-rate_tfe[, !(names(rate_tfe) %in% drops)]
      colnames(new_rate_tfe)<-c("Factor1","Factor2","Model","r","SE(r)")
      new_rate_tfe
    },
    digits = 4,
    caption = ("<b> Estimate of Infection rate (r),, Maximum disease intensity (K) </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
    )
    
    output$tfe_infection_rate2 <- renderTable({
      rate_tfe<-model_multi2$Parameters
      drops <- c("r","r_se","K","K_se","CCC", "r_squared","RSE","best_model","df","y0_ci_lwr","y0_ci_upr","r_ci_lwr","r_ci_upr","K_ci_lwr","K_ci_upr")
      new_rate_tfe<-rate_tfe[, !(names(rate_tfe) %in% drops)]
      colnames(new_rate_tfe)<-c("Factor1","Factor2","Model","y0","SE(y0)")
      new_rate_tfe
    },
    digits = 4,
    caption = ("<b> Estimate of Initial disease intensity (y0)</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
    )
    
    output$tfe_infection_rate3 <- renderTable({
      rate_tfe<-model_multi2$Parameters
      drops <- c("y0","y0_se","r","r_se","CCC", "r_squared","RSE","best_model","df","y0_ci_lwr","y0_ci_upr","r_ci_lwr","r_ci_upr","K_ci_lwr","K_ci_upr")
      new_rate_tfe<-rate_tfe[, !(names(rate_tfe) %in% drops)]
      colnames(new_rate_tfe)<-c("Factor1","Factor2","Model","K","SE(K)")
      new_rate_tfe
    },
    digits = 4,
    caption = ("<b> Estimate of Initial disease intensity (y0)</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
    )
    
    
    
    
    output$tfe_initial_inoculum <- renderTable({
      ci_tfe<-model_multi2$Parameters
      entry_ci <- c("factor1_tfe","factor2_tfe","model","r_ci_lwr","r_ci_upr")
      new_ci_tfe<-ci_tfe[, (names(ci_tfe) %in% entry_ci)]
      colnames(new_ci_tfe)<-c("Factor1","Factor2","Model","Lower(r)","Upper(r)")
      new_ci_tfe
    },
    digits = 4,
    caption = ("<b> 95% confidence interval of infection rate (r) estimates</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
    ) 
    
    output$tfe_initial_inoculum1 <- renderTable({
      ci_tfe2<-model_multi2$Parameters
      entry_ci2 <- c("factor1_tfe","factor2_tfe","model","y0_ci_lwr","y0_ci_upr")
      new_ci_tfe2<-ci_tfe2[, (names(ci_tfe2) %in% entry_ci2)]
      colnames(new_ci_tfe2)<-c("Factor1","Factor2","Model","Lower(y0)","Upper(y0)")
      new_ci_tfe2
    },
    digits = 4,
    caption = ("<b> 95% confidence interval of initial inoculum (y0) estimates</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
    ) 
    
    
    output$tfe_initial_inoculum2 <- renderTable({
      ci_tfe1<-model_multi2$Parameters
      entry_ci1 <- c("factor1_tfe","factor2_tfe","model","K_ci_lwr","K_ci_upr")
      new_ci_tfe1<-ci_tfe1[, (names(ci_tfe1) %in% entry_ci1)]
      colnames(new_ci_tfe1)<-c("Factor1","Factor2","Model","Lower(K)","Upper(K)")
      new_ci_tfe1
    },
    digits = 4,
    caption = ("<b> 95% confidence interval of K</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
    ) 
    
    output$tfe_AUDPC <- renderTable({
      audc_tfe<-tfe_data %>% 
        group_by(factor1_tfe,factor2_tfe)%>%
        summarise(audpc=AUDPC(time=time,y=sev),
                  audps=AUDPS(time=time,y=sev),
        )
      audc_tfe<-as.data.frame(audc_tfe)
      audc_tfe
    },
    digits = 3,
    caption = ("<b> Area under disease progress curve </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
    ) 
    
    output$tfe_modelplot <- shiny::renderPlot({
      if (!is.null(input$model_tfe)){
        n2<-nlevels(as.factor(tfe_data$factor2_tfe))
        p<-model_multi2$Data %>%
          filter(model==as.character(input$model_tfe))%>%
          ggplot(aes(color=factor1_tfe))+
          geom_point(aes(time,y))+
          geom_line(aes(time, predicted),linewidth=0.6)+
          labs(x=input$xlabm_tfe, 
               y= input$ylabm_tfe)+
          ggtitle(input$model_tfe)+
          facet_wrap(~factor2_tfe,ncol=n2)+
          theme_minimal()
      }
      if (is.null(input$themem_tfe)) {
        return(p)
      }
      if (input$themem_tfe == "normal") {
        q <- p + theme_bw()
      } else if (input$themem_tfe == "economist") {
        q <- p + ggthemes::theme_economist() 
      } else if (input$themem_tfe == "grey") {
        q <- p + theme_gray() 
      } 
      else if (input$themem_tfe == "minimal") {
        q <- p + theme_minimal()
      }else if (input$themem_tfe == "light") {
        q <- p + theme_light()
      }
      else if (input$themem_tfe == "void") {
        q <- p + theme_void()
      }
      else if (input$themem_tfe == "tufte") {
        q <- p + ggthemes::theme_tufte()
      }
      else if (input$themem_tfe == "stata") {
        q <- p + ggthemes::theme_stata()
      }
      else if (input$theme_tfe == "wsj") {
        q <- p + ggthemes::theme_wsj()
      }
      else if (input$themem_tfe == "calc") {
        q <- p + ggthemes::theme_calc()
      }
      else if (input$themem_tfe == "hc") {
        q <- p + ggthemes::theme_hc()
      }
      q
      
    },
    bg = "transparent"
    )
    
    
  })
  
  
  ################  
  
  
  
  
  ############################# download data set
  output$data_set_tfe <- shiny::renderUI({
    
    list(
      shiny::selectInput(
        "filenames_tfe", "Choose a dataset:",
        list.files(
          pattern = c("two_factor.csv")
        )
      ),
      shiny::downloadButton("downloadData_tfe", label = "Download csv file", class = "butt11",)
    )
    
    
    
  })
  
  datasetInput = shiny::reactive({
    switch(input$filenames_tfe,
           filenames_tfe
    )
  })
  
  output$downloadData_tfe = shiny::downloadHandler(
    filename = function() {
      input$filenames_tfe
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_tfe, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  )
  ######################### end data set download
}