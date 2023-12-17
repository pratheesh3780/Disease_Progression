
############ ui
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "grapesDPA"), # dashboard Head
  
  
  dashboardSidebar( # Sidebarmenu
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("home")),
      menuItem("Read me", tabName = "read", icon = icon("book")),
      menuItem("Single level Experiment", tabName = "sle", icon = icon("magnifying-glass"),
               menuItem("Maximum potential", 
                        tabName = "mp_sle",icon = icon("paper-plane")),
               menuItem("Limited potential", 
                        tabName = "lp_sle",icon = icon("paper-plane"))
      ),
      menuItem("Single Factor Experiment",tabName = "sfe", icon = icon("magnifying-glass"),
               menuItem("Non-linear estimation", 
                        tabName = "lp_sfe",icon = icon("sliders"))
      ),
      menuItem("Two Factor Experiment",tabName = "tfe", icon = icon("magnifying-glass"),
               menuItem("Non-linear estimation", 
                        tabName = "lp_tfe",icon = icon("sliders"))
      ),
      div(
        style = "display: flex; justify-content: center;",
        img(
          src = "logo.png", align = "center",
          width = "200", height = "200"
        )
      ),
      h6(
        tags$div(
          style = "text-align: center; color: goldenrod;",
          tags$br(),
          "Developed by:",
          tags$br(),
          tags$b("Dr. Pratheesh P. Gopinath"),
          tags$br(),
          tags$b("Assistant Professor & Head,"),
          tags$br(),
          tags$b("Agricultural Statistics,"),
          tags$br(),
          tags$b("Kerala Agricultural University"),
          tags$br(),
          tags$br()
        )
      )
    )
  ),
  dashboardBody(
    tabItems(
      # draw_ebp tab content
    tabItem(tabName = "read",fluidPage(
      withMathJax(includeMarkdown("README.md"))
    )),
      tabItem(tabName = "mp_sle",mpsle_ui()),
      tabItem(tabName = "lp_sle",lpsle_ui()),
      tabItem(tabName = "lp_sfe",lpsfe_ui()),
      tabItem(tabName = "lp_tfe",lptfe_ui()),
      
      
      # home tab content
      tabItem(
        tabName = "Home",
        tags$h3(
          HTML("<b> Introduction to grapesDPA </b>")
        ),
        tags$p(
          HTML("<p style='text-align: justify;'>
Welcome to grapesDPA - the General R-based Analysis Platform Empowered by Shiny, designed specifically for Disease Progress Analysis (DPA) in agriculture. In the realm of agricultural research, decoding disease progression data is as pivotal as conducting experiments in the field. Our mission is to provide a comprehensive platform that facilitates the analysis, visualization, and comprehension of disease progression dynamics.
</p>
<p style='text-align: justify;'>
grapesDPA stands as a gateway to the future of Disease Progress Analysis. With an array of tools tailored for single-level experiments, single-factor experiments, and two-factor experiments, grapesDPA empowers researchers to harness the power of R for insightful analysis and visualization of disease progression data. Our commitment is to be your steadfast ally in translating complex data into meaningful, actionable insights.
</p>
<p style='text-align: justify;'>
Within grapesDPA, we offer a visual landscape enriched with diverse charts, plots, and graphs, each telling a unique story from your disease progression experiments. Our interface strikes a harmonious balance between sophistication and user-friendliness, ensuring that the depth of your data effortlessly shines through.
</p>
<p style='text-align: justify;'>
grapesDPA operates as an open-source software, governed by the <b>GNU Public License Version 3</b>. We highly value your feedback and encourage users to contribute, share concerns, or propose enhancements. Your engagement is pivotal in refining and evolving grapesDPA for an enhanced disease progression analysis experience.
</p>
<p style='text-align: right;'>
Your friendly neighbourhood statistician
</p>")
        ),
        div(
          style = "display: flex; justify-content: center;",
          tags$h3(
            HTML("<b></b>")
          )
        ),
        tags$br(),
        div(
          style = "display: flex; justify-content: center;",
          img(
            src = "logo.png", align = "center",
            width = "200", height = "200"
          )
        )
      )
    )
  )
)