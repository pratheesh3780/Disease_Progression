lpsle_server <- function(input, output,session) {
  
  
  
  ########################## file_upload
  csvfile_lpsle <- shiny::reactive({
    csvfile_lpsle <- input$file1_lpsle
    if (is.null(csvfile_lpsle)){return(NULL)}
    dt_lpsle <- read.csv(csvfile_lpsle$datapath, header=input$header_lpsle, sep=",")
    dt_lpsle
  })
  
  output$var_lpsle  <- shiny::renderUI({
    if(is.null(input$file1_lpsle$datapath)){
      return()
    }
    else{
      list (shiny::radioButtons("sev_lpsle", "Select the disease sevierity column)", choices = names(csvfile_lpsle())),
            shiny::radioButtons("time_lpsle", "Select the time/period column", choices = names(csvfile_lpsle())),
            shiny:: numericInput("initial_lpsle", "Initial disease intensity (y0):", 0.01),
            shiny:: numericInput("infr_lpsle", "Apparent infection rate (r):", 0.3),
            shiny:: numericInput("k_lpsle", "Maximum disease intensity:", 0.8),
            shiny:: numericInput("maxitr_lpsle", "Maximum Iterations:", 50),
            shinyWidgets::actionBttn(
              inputId = "submit_lpsle",
              label = "Analyze!",
              color = "danger",
              style = "jelly"
            )
      )
    }
  })
  
  
  ############### plotting DPC
  plotInput_basicplot_lpsle <- shiny::reactive({
    if (is.null(input$file1_lpsle$datapath)) {
      return()
    }
    if (is.null(input$submit_lpsle)) {
      return()
    }
    if (input$submit_lpsle > 0) {
      x <- as.matrix(csvfile_lpsle()[, input$time_lpsle])
      y <- as.matrix(csvfile_lpsle()[, input$sev_lpsle])
      lpsle_data <- data.frame(
        time = x,
        sev = y
      )
      p<-ggplot(aes(time,sev),data = lpsle_data)+
        geom_line()+
        geom_point()+
        theme_minimal()+
        labs(x=input$xlab_lpsle, 
             y= input$ylab_lpsle)+
        ggtitle(input$title_lpsle) 
    }
    if (is.null(input$theme_lpsle)) {
      return(p)
    }
    if (input$theme_lpsle == "normal") {
      q <- p + theme_bw()
    } else if (input$theme_lpsle == "economist") {
      q <- p + ggthemes::theme_economist() 
    } else if (input$theme_lpsle == "grey") {
      q <- p + theme_gray() 
    } 
    else if (input$theme_lpsle == "minimal") {
      q <- p + theme_minimal()
    }else if (input$theme_lpsle == "light") {
      q <- p + theme_light()
    }
    else if (input$theme_lpsle == "void") {
      q <- p + theme_void()
    }
    else if (input$theme_lpsle == "tufte") {
      q <- p + ggthemes::theme_tufte()
    }
    else if (input$theme_lpsle == "stata") {
      q <- p + ggthemes::theme_stata()
    }
    else if (input$theme_lpsle == "wsj") {
      q <- p + ggthemes::theme_wsj()
    }
    else if (input$theme_lpsle == "calc") {
      q <- p + ggthemes::theme_calc()
    }
    else if (input$theme_lpsle == "hc") {
      q <- p + ggthemes::theme_hc()
    }
    q
    
    
    
    
  })
  
  ########## plotting end
  ################################plot output 
  output$lpsle_basicplot <- shiny::renderPlot( {
    plotInput_basicplot_lpsle()
  },
  bg = "transparent"
  ) 
  
  ###############  
  
  ############## control panel for basic plot
  output$cp_lpsle <- shiny::renderUI({
    if (is.null(input$file1_lpsle$datapath)){return()}
    if (is.null(input$submit_lpsle)){return()}
    if (input$submit_lpsle > 0) {
      list(
        fluidRow(
          column(4,
                 shiny::textInput("xlab_lpsle", "Enter required x-axis title", "Time (Days)")
          ),
          column(4,
                 shiny::textInput("ylab_lpsle", "Enter required y-axis title", "Disease Severity")
          ),
          column(4,
                 shiny::textInput("title_lpsle", "Enter required title", "Disease progress curve")
          ),
          column(
            4,
            shiny::selectInput(
              "theme_lpsle", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          )
        )
      )
      
      
    }
  })
  
  
  ############## control panel for model plot
  output$cp_modplot_lpsle <- shiny::renderUI({
    if (is.null(input$file1_lpsle$datapath)){return()}
    if (is.null(input$submit_lpsle)){return()}
    if (input$submit_lpsle > 0) {
      list(
        fluidRow(
          column(4,
                 shiny::textInput("xlabm_lpsle", "Enter required x-axis title", "Time (Days)")
          ),
          column(4,
                 shiny::textInput("ylabm_lpsle", "Enter required y-axis title", "Disease Severity")
          ),
          column(
            4,
            shiny::selectInput(
              "themem_lpsle", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          ),
          column(
            4,
            shiny::selectInput(
              "model_lpsle", "Choose the model to fit:",
              list("Gompertz","Monomolecular", "Logistic")
            )
          )
        )
      )
      
      
    }
  })
  
  
  ########################## modelling
  ########### Model fitting
  
  observeEvent(input$submit_lpsle, {
    x <- as.matrix(csvfile_lpsle()[, input$time_lpsle])
    y <- as.matrix(csvfile_lpsle()[, input$sev_lpsle])
    lpsle_data <- data.frame(
      time = x,
      sev = y
    )
    
    ## warning appear in console due to this
    models_lpsle<-epifitter::fit_nlin2(exp1$time,
                                       exp1$sev,
                                       starting_par = list(
                                         y0 = as.numeric(input$initial_lpsle), 
                                         r = as.numeric(input$infr_lpsle),
                                       K= as.numeric(input$k_lpsle)),
                                       maxiter = as.numeric(input$maxitr_lpsle))
    
    output$lpsle_stats <- renderTable({
      stats<-models_lpsle$Stats
      stats
    },
    digits = 3,
    caption = ("<b> Population Model fit statistics </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE
    ) 
    
    output$lpsle_infection_rate <- renderTable({
      inf_rate<-models_lpsle$`Infection rate`
      inf_rate
    },
    digits = 4,
    caption = ("<b> Estimate of Infection rate (r) </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE
    )
    
    output$lpsle_initial_inoculum <- renderTable({
      initial_inoculum<-models_lpsle$`Initial inoculum`
      initial_inoculum
    },
    digits = 4,
    caption = ("<b> Estimate of Initial inoculum (y0) </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE
    ) 
    
    output$lpsle_max_disease <- renderTable({
      lpsle_max_disease<-models_lpsle$`Maximum disease intensity`
      lpsle_max_disease
    },
    digits = 4,
    caption = ("<b> Estimate of maximum disease intensity (K)</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE
    )
    
    
    
    
    output$lpsle_AUDPC <- renderTable({
      AC_lpsle<-AUDPC(time = lpsle_data$time, y = lpsle_data$sev, y_proportion = TRUE)
      AS_lpsle<-AUDPS(time = lpsle_data$time, y = lpsle_data$sev, y_proportion = TRUE)
      area_data_lpsle <- data.frame(
        `Area under curve` = AC_lpsle,
        `Area under stairs` = AS_lpsle
      )
      area_data_lpsle
    },
    digits = 3,
    caption = ("<b> Area under disease progress curve </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
    ) 
    
    
    output$lpsle_modelplot <- shiny::renderPlot({
      if (!is.null(input$model_lpsle)){
        p<-epifitter::plot_fit(models_lpsle,models =as.character(input$model_lpsle))+
          theme_minimal()+
          labs(x=input$xlabm_lpsle, 
               y= input$ylabm_lpsle)
      }
      if (is.null(input$themem_lpsle)) {
        return(p)
      }
      if (input$themem_lpsle == "normal") {
        q <- p + theme_bw()
      } else if (input$themem_lpsle == "economist") {
        q <- p + ggthemes::theme_economist() 
      } else if (input$themem_lpsle == "grey") {
        q <- p + theme_gray() 
      } 
      else if (input$themem_lpsle == "minimal") {
        q <- p + theme_minimal()
      }else if (input$themem_lpsle == "light") {
        q <- p + theme_light()
      }
      else if (input$themem_lpsle == "void") {
        q <- p + theme_void()
      }
      else if (input$themem_lpsle == "tufte") {
        q <- p + ggthemes::theme_tufte()
      }
      else if (input$themem_lpsle == "stata") {
        q <- p + ggthemes::theme_stata()
      }
      else if (input$theme_lpsle == "wsj") {
        q <- p + ggthemes::theme_wsj()
      }
      else if (input$themem_lpsle == "calc") {
        q <- p + ggthemes::theme_calc()
      }
      else if (input$themem_lpsle == "hc") {
        q <- p + ggthemes::theme_hc()
      }
      q
      
    },
    bg = "transparent"
    )
  })
  
  
  ################  
  
  
  
  
  ############################# download data set
  output$data_set_lpsle <- shiny::renderUI({
    
    list(
      shiny::selectInput(
        "filenames_lpsle", "Choose a dataset:",
        list.files(
          pattern = c("single_level.csv")
        )
      ),
      shiny::downloadButton("downloadData_lpsle", label = "Download csv file", class = "butt11",)
    )
    
    
    
  })
  
  datasetInput = shiny::reactive({
    switch(input$filenames_lpsle,
           filenames_lpsle
    )
  })
  
  output$downloadData_lpsle = shiny::downloadHandler(
    filename = function() {
      input$filenames_lpsle
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_lpsle, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  )
  ######################### end data set download
}