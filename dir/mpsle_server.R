mpsle_server <- function(input, output,session) {
  
  
  
  ########################## file_upload
  csvfile_sle <- shiny::reactive({
    csvfile_sle <- input$file1_sle
    if (is.null(csvfile_sle)){return(NULL)}
    dt_sle <- read.csv(csvfile_sle$datapath, header=input$header_sle, sep=",")
    dt_sle
  })
  
  output$var_sle  <- shiny::renderUI({
    if(is.null(input$file1_sle$datapath)){
      return()
    }
    else{
      list (shiny::radioButtons("sev_sle", "Select the disease sevierity column)", choices = names(csvfile_sle())),
            shiny::radioButtons("time_sle", "Select the time/period column", choices = names(csvfile_sle())),
            shiny:: numericInput("initial_sle", "Initial disease intensity (y0):", 0.01),
            shiny:: numericInput("infr_sle", "Apparent infection rate (r):", 0.3),
            shiny:: numericInput("maxitr_sle", "Maximum Iterations:", 50),
            shinyWidgets::actionBttn(
              inputId = "submit_sle",
              label = "Analyze!",
              color = "danger",
              style = "jelly"
            )
      )
    }
  })
  
  
  ############### plotting DPC
  plotInput_basicplot <- shiny::reactive({
    if (is.null(input$file1_sle$datapath)) {
      return()
    }
    if (is.null(input$submit_sle)) {
      return()
    }
    if (input$submit_sle > 0) {
      x <- as.matrix(csvfile_sle()[, input$time_sle])
      y <- as.matrix(csvfile_sle()[, input$sev_sle])
      sle_data <- data.frame(
        time = x,
        sev = y
      )
  p<-ggplot(aes(time,sev),data = sle_data)+
    geom_line()+
    geom_point()+
    theme_minimal()+
    labs(x=input$xlab_sle, 
         y= input$ylab_sle)+
    ggtitle(input$title_sle) 
    }
    if (is.null(input$theme_sle)) {
      return(p)
    }
    if (input$theme_sle == "normal") {
      q <- p + theme_bw()
    } else if (input$theme_sle == "economist") {
      q <- p + ggthemes::theme_economist() 
    } else if (input$theme_sle == "grey") {
      q <- p + theme_gray() 
    } 
    else if (input$theme_sle == "minimal") {
      q <- p + theme_minimal()
    }else if (input$theme_sle == "light") {
      q <- p + theme_light()
    }
    else if (input$theme_sle == "void") {
      q <- p + theme_void()
    }
    else if (input$theme_sle == "tufte") {
      q <- p + ggthemes::theme_tufte()
    }
    else if (input$theme_sle == "stata") {
      q <- p + ggthemes::theme_stata()
    }
    else if (input$theme_sle == "wsj") {
      q <- p + ggthemes::theme_wsj()
    }
    else if (input$theme_sle == "calc") {
      q <- p + ggthemes::theme_calc()
    }
    else if (input$theme_sle == "hc") {
      q <- p + ggthemes::theme_hc()
    }
  q
    
    
    
    
  })
  
  ########## plotting end
  ################################plot output 
  output$sle_basicplot <- shiny::renderPlot( {
    plotInput_basicplot()
  },
  bg = "transparent"
  ) 
  
###############  
  
  ############## control panel for basic plot
  output$cp_sle <- shiny::renderUI({
    if (is.null(input$file1_sle$datapath)){return()}
    if (is.null(input$submit_sle)){return()}
    if (input$submit_sle > 0) {
      list(
        fluidRow(
          column(4,
                 shiny::textInput("xlab_sle", "Enter required x-axis title", "Time (Days)")
          ),
          column(4,
                 shiny::textInput("ylab_sle", "Enter required y-axis title", "Disease Severity")
          ),
          column(4,
                 shiny::textInput("title_sle", "Enter required title", "Disease progress curve")
          ),
          column(
            4,
            shiny::selectInput(
              "theme_sle", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          )
        )
        )
      
      
      }
  })

  
  ############## control panel for model plot
  output$cp_modplot <- shiny::renderUI({
    if (is.null(input$file1_sle$datapath)){return()}
    if (is.null(input$submit_sle)){return()}
    if (input$submit_sle > 0) {
      list(
        fluidRow(
          column(4,
                 shiny::textInput("xlabm_sle", "Enter required x-axis title", "Time (Days)")
          ),
          column(4,
                 shiny::textInput("ylabm_sle", "Enter required y-axis title", "Disease Severity")
          ),
          column(
            4,
            shiny::selectInput(
              "themem_sle", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          ),
          column(
            4,
            shiny::selectInput(
              "model_sle", "Choose the model to fit:",
              list("Gompertz","Exponential","Monomolecular", "Logistic")
            )
          )
        )
      )
      
      
    }
  })
  
  
 ########################## modelling
  ########### Model fitting
  
  observeEvent(input$submit_sle, {
    x <- as.matrix(csvfile_sle()[, input$time_sle])
    y <- as.matrix(csvfile_sle()[, input$sev_sle])
    sle_data <- data.frame(
      time = x,
      sev = y
    )
  
    ## warning appear in console due to this
    models<-epifitter::fit_nlin(sle_data$time,
                                sle_data$sev,
                                starting_par = list(
                                  y0 = as.numeric(input$initial_sle), 
                                  r = as.numeric(input$infr_sle)),
                                maxiter = as.numeric(input$maxitr_sle))

    
    output$sle_stats <- renderTable({
      stats<-models$Stats
      stats
    },
    digits = 3,
    caption = ("<b> Population Model fit statistics </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE
    ) 
    
    output$sle_infection_rate <- renderTable({
      inf_rate<-models$`Infection rate`
      inf_rate
    },
    digits = 4,
    caption = ("<b> Estimate of Infection rate </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE
    )
    
    output$sle_initial_inoculum <- renderTable({
      initial_inoculum<-models$`Initial inoculum`
      initial_inoculum
    },
    digits = 4,
    caption = ("<b> Estimate of Initial inoculum </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE
    ) 
   
    output$sle_AUDPC <- renderTable({
      AC_sle<-AUDPC(time = sle_data$time, y = sle_data$sev, y_proportion = TRUE)
      AS_sle<-AUDPS(time = sle_data$time, y = sle_data$sev, y_proportion = TRUE)
      area_data_sle <- data.frame(
        `Area under curve` = AC_sle,
        `Area under stairs` = AS_sle
      )
      area_data_sle
      },
    digits = 3,
    caption = ("<b> Area under disease progress curve </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
    ) 
    
    
    output$sle_modelplot <- shiny::renderPlot({
      if (!is.null(input$model_sle)){
      p<-epifitter::plot_fit(models,models =as.character(input$model_sle))+
        theme_minimal()+
        labs(x=input$xlabm_sle, 
             y= input$ylabm_sle)
      }
      if (is.null(input$themem_sle)) {
        return(p)
      }
      if (input$themem_sle == "normal") {
        q <- p + theme_bw()
      } else if (input$themem_sle == "economist") {
        q <- p + ggthemes::theme_economist() 
      } else if (input$themem_sle == "grey") {
        q <- p + theme_gray() 
      } 
      else if (input$themem_sle == "minimal") {
        q <- p + theme_minimal()
      }else if (input$themem_sle == "light") {
        q <- p + theme_light()
      }
      else if (input$themem_sle == "void") {
        q <- p + theme_void()
      }
      else if (input$themem_sle == "tufte") {
        q <- p + ggthemes::theme_tufte()
      }
      else if (input$themem_sle == "stata") {
        q <- p + ggthemes::theme_stata()
      }
      else if (input$theme_sle == "wsj") {
        q <- p + ggthemes::theme_wsj()
      }
      else if (input$themem_sle == "calc") {
        q <- p + ggthemes::theme_calc()
      }
      else if (input$themem_sle == "hc") {
        q <- p + ggthemes::theme_hc()
      }
      q
      
    },
    bg = "transparent"
    )
})

  
  ################  

  
  
  
  ############################# download data set
  output$data_set_sle <- shiny::renderUI({
    
    list(
      shiny::selectInput(
        "filenames_sle", "Choose a dataset:",
        list.files(
          pattern = c("single_level.csv")
        )
      ),
      shiny::downloadButton("downloadData11", label = "Download csv file", class = "butt11",)
    )
    
    
    
  })
  
  datasetInput = shiny::reactive({
    switch(input$filenames_sle,
           filenames_sle
    )
  })
  
  output$downloadData11 = shiny::downloadHandler(
    filename = function() {
      input$filenames_sle
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_sle, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  )
  ######################### end data set download
}