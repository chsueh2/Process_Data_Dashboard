# shinydashboard app to extract and plot unit process data


# 01 setup ----------------------------------------------------------------


# load packages and functions
if (!require("pacman")) utils::install.packages("here", dependencies = TRUE)
source(here::here("00 preloads.R"))


# read data for test purpose
#df <- readRDS(here("SIR_summary.rds"))



# 02 ui header ------------------------------------------------------------


# header
header <- dashboardHeader(
  # Disable header
  #disable = TRUE
  
  title = "Unit Process",
  titleWidth = 300,
  
  # A notification drop down menu
  dropdownMenu(
    type = "notifications",
    notificationItem(
      text = "Question or suggestion?",
      icon = icon("poo-storm"),
      href = "mailto: chsueh2@ncsu.edu"
    )
  )
)



# 03 ui sidebar -----------------------------------------------------------



# sidebar
sidebar <- dashboardSidebar(
  # set width of sidebar
  width = 300,
  
  # Side panel for About tab
  conditionalPanel(
    "input.Tabset == `Tab0`",
    wellPanel(
      tags$h4("About:"),
      tags$h5(
        "Click on tab `About` to show this tutorial in the side panel."
      ),
      hr(),
      tags$h4("Data: Tab 1A and 1B"),
      tags$h5(
        "On tab 1A, upload one of the raw process files in `data` folder, ",
        "and then select range to subset a run (unit process).",
        p(),
        "This app automatically selects the first run and align the process time.",
        "You are welcomed to extract different runs with customized process time offset. ",
        "Click to extract the run and download the data (automatically switch to tab 1B)."
      ),
      hr(),
      tags$h4("Data Exploration: Tab 2A and 2B"),
      tags$h5(
        "On tab 2A, upload one or multiple unit process files ",
        "(the one you extract on tab 1B or those provided in `runs` folder), ",
        "and then select variables of interest.",
        p(),
        "For example, try comparing run1 run2 and run3 (csv files) ",
        "and choose these variables: CCH_1T, VA1, VA2, VCH, VCT, VTC.",
        p(),
        "Plot CCH_1T on panel 1; VA1 and VA2 on panel 2; VCH on panel 3; VCT and VTC on panel 4. ",
        p(),
        "Tab 2A shows an interactive plot for the selected variables and Tab 2B shows the panel plots.",
        "You can customize the labels, download the dataset and export the plot."
      ),
      hr(),
      tags$h4("Modeling: Tab 3A, 3B and 3C"),
      tags$h5(
        "Tab 3A describes the machine learning methods used to classify process stages",
        p(),
        "After clicking on `Fit Models` button, the summary of model fittings and plots ",
        "will be showed on Tab 3B.",
        p(),
        "Use Tab 3C to make predictions."
      )
    )
  ),  
  
  
  # Side panel for Data tabs (1A and 1B)
  conditionalPanel(
    "input.Tabset == `Tab1` || input.Tabset == `Tab2`",
    
    # read a log file
    fileInput(
      "t1_file", 
      "Choose a raw process file (`data` folder)", 
      multiple = FALSE, 
      accept = c(".csv")),
    
    # select row range
    uiOutput("t1_slider.range"),
    # select row to align
    uiOutput("t1_slider.align"),
    # select time offset
    uiOutput("t1_num.align"),
    # extract the run
    uiOutput("t1_btn.extract")
  ),
  
  
  # Side panel for Data Exploration tabs (2A and 2B)
  conditionalPanel(
    "input.Tabset == `Tab3` || input.Tabset == `Tab4`",
    
    # read files
    fileInput(
      "t3_file", 
      "Choose one or multiple unit process files (`runs` folder)", 
      multiple = TRUE, 
      accept = c(".csv")),
    
    # select process time range
    uiOutput("t3_slider.range"),
    # panel settings
    uiOutput("t4_select.vars.y1"),
    uiOutput("t4_text.lab.y1"),
    uiOutput("t4_select.vars.y2"),
    uiOutput("t4_text.lab.y2"),
    uiOutput("t4_select.vars.y3"),
    uiOutput("t4_text.lab.y3"),
    uiOutput("t4_select.vars.y4"),
    uiOutput("t4_text.lab.y4")
  ),
  
  
  # Side panel for Modeling tabs (3A, 3B and 3C)
  conditionalPanel(
    "input.Tabset == `Tab5` || input.Tabset == `Tab6` || input.Tabset == `Tab7`",
    
    h4("Split training and test sets"),
    # select training/test splitting ratio
    uiOutput("t5_slider.split"),
    
    hr(),   
    h4("Cross Validation"),
    # select cv folds
    uiOutput("t5_slider.folds"),
    # select cv repeats
    uiOutput("t5_slider.repeats"),
    
    hr(),   
    h4("Multinomial Logistic Regression"),
    # Hyperparameter tuning for Multinomial Logistic Regression
    uiOutput("t5_slider.model1"),
    
    h4("KNN"),
    # Hyperparameter tuning for KNN
    uiOutput("t5_slider.model2"),
    
    h4("Random Forests"),
    # Hyperparameter tuning for Random Forests
    uiOutput("t5_slider.model3"),
    
    hr(),    
    # extract the run
    uiOutput("t5_btn.fit")
  )
  
  
  # Test structure
  #uiOutput("radioButtons"),
  
  # Coating Dates
  #uiOutput("checkBoxGroup")
  
)



# 04 ui body --------------------------------------------------------------


# body
body <- dashboardBody(
  
  # changing theme
  shinyDashboardThemes(theme = "grey_light"),
  
  # color
  tags$head(
    tags$style(HTML("
        .shiny-output-error-validation {
          color: #ff0000;
          font-weight: bold;
        }
      "))
  ),
  
  # tabs on main panel
  tabBox(
    id = "Tabset", selected = "Tab0", 
    height = "800px", width = 12,
    tabPanel(
      title = "About", value = "Tab0",
      tags$h3("Purpose"),
      tags$div(
        "The goal is to classify various process stages on production tools ",
        "using process data from R&D tools.",
        "This app explores raw process data, extract unit process data, ",
        "compare run to run with EDA and panel plots, ",
        "and train models to identify process stages in production runs."
      ),
      tags$h3("Data Source"),
      tags$div(
        "The (raw) process data are from thin-film deposition tools used in semicondutor fabs. ",
        "On R&D tools, research equipments like mass spectrometers are installed ",
        "to monitor chemical compositions inside the process reactors and chambers. ",
        "With these, we can easily identify different process stages. ",
        "But on production tools, these in-situ analyzers are not available. ",
        "Therefore, process scientists need to develope a method to identify the process stages ",
        "based on the process data from R&D tools.",
        tags$p(),
        "Sorry I cannot provide links to the data source (located on the tools) ",
        "but some old process data are provided for tests (saved in `data` and `runs` folders).",
        tags$p(),
        "Also, in this demo, ",
        "the proprietary process models are replaced with ",
        "a simple modeling with temperatures, pressures and gas flows."
      ),
      tags$h4("Raw process data"),
      tags$div(
        "These are raw data generated directly from either R&D tools or production tools. ",
        "From R&D tools, process stages are labeled and recorded. ",
        "From production tools, process stages have to be identified using models. ",
        "Some raw process files are provided in `data` folder.",
        "With this app, they can be subsetted to get a unit process data of a single run."
      ),
      tags$h4("Unit process data"),
      tags$div(
        "These are process data extracted from raw process data (mentioned above). ",
        "Each of them is one single process run. ",
        "We often exam and compare them run-to-run to study process abnormalities. ",
        "You can use this app to subset a raw process file to get unit process data.  or ",
        "use the provided unit process files saved in `runs` folder to test this app."
      ),
      tags$h3("API Workflow"),
      tags$ol(
        tags$li("About: This page"), 
        tags$li("Data: Load raw process data and analyze the runs"), 
        tags$li("Data Extraction: Subset and save unit process data"), 
        tags$li("Plot Setup: Choose variables of interest and configure plot pannels"), 
        tags$li("Data Exploration and Plot: EDA and pannel plots on unit process data"), 
        tags$li("Model Info: Explain models used to identify process stages"), 
        tags$li("Model Fitting: Fit model based on method users choose"), 
        tags$li("Predcition: Make prediction on process stages")
      ),
      tags$p(),
      tags$img(
        src = "https://www.forgenano.com/wp-content/uploads/2017/06/ALD-art-768x289.png", 
        width = "600px",
        alt = "Source: www.forgenano.com"
      ),
      tags$div("(Source: www.forgenano.com)")  
    ),
    tabPanel(
      title = "1A: Data", value = "Tab1",
      DTOutput("t1_table.ref")
    ),
    tabPanel(
      title = "1B: Data Extraction", value = "Tab2", 
      uiOutput("t2_btn.download"),
      DTOutput("t2_table.run")
    ),
    tabPanel(
      title = "2A: Plot setup", value = "Tab3",
      uiOutput("t3_select.vars"),
      plotlyOutput("t3_plotly", height = "700px")
    ),
    tabPanel(
      title = "2B: Data Exploration", value = "Tab4",
      uiOutput("t4_btn.download"),
      uiOutput("t4_check.downloadAll"),
      plotOutput("t4_plot"),
      uiOutput("t4_select.type"),
      uiOutput("t4_table"),
      uiOutput("t4_select.type_graph"),
      plotOutput("t4_graph", height = "200px")
    ),
    tabPanel(
      title = "3A: Modeling Info", value = "Tab5",
      withMathJax(),
      tags$h3("Model a Categorical Response"),
      tags$div(
        "The response we want model is `process stage` which is a categorical variable ",
        "with more than 2 levels. Unlike the binary response we learned from the class ",
        "we cannot use the simple logistic regression (`glm` with `binomial` family). "
      ),
      tags$h3("Multinomial Logistic Regression"),
      tags$div("Multinomial Logistic Regression is used to model nominal outcomes. "),
      helpText("If the catgorical response has \\(k\\) categories, then \\(k-1\\) sets of different
                relative risks are necessary to fully characterize it:"),
      helpText("Model 1: \\(\\log\\frac{P(y = y_1)}{P(y = y_0)}=b_{10} + b_{11}X_1 + b_{12}X_2+ ... \\)"),
      helpText("Model 2: \\(\\log\\frac{P(y = y_2)}{P(y = y_0)}=b_{20} + b_{21}X_1 + b_{22}X_2+ ... \\)"),
      helpText("Model 3: \\(\\log\\frac{P(y = y_3)}{P(y = y_0)}=b_{30} + b_{31}X_1 + b_{32}X_2+ ... \\)"),
      helpText("\\(\\vdots\\)"),
      helpText("where \\(y_0\\) is the reference (base) category and \\(y \\in \\{y_0, y_1,... y_{k-1}\\}\\)."),
      tags$div(
        "In this app, we use `multinom()` from `nnet` package to fit a multinomial logistic regression model.",
        "Pros of the logistic regression includes: ",
        "(1) it is simple to implement, ",
        "(2) easy to update and expand. ",
        "However, not all problem are sovable using this algorithm."),
      tags$h3("K-Nearest Neighbors"),
      tags$div(
        "The goal of KNN is to find the nearest neighbors and classify the observation based on",
        "the majority class of its neighbors.",
        "There are different distance metircs used to find the neighbors. "),
      helpText("One of them is Minkowski distance which is given by ",
               "\\(d = (\\sum_{i=1}^{n} |x_i-y_i|)^{1/p}\\).", 
               "In this app, we use Euclidean distance which is a special case with ",
               "\\( p = 2\\)."),
      tags$div(
        "KNN algorithm is very intuitive and simple which makes it easy to implement and extend. ",
        "It doesn't has any assumption and has only one hyperparameter fot tuning. ",
        "The major disavanatge of using KNN is its slow speed when the size of data is large. ",
        "Also it can be difficult to deal with missing value."),
      tags$h3("Random Forests"),
      tags$div(
        "As one of the popular ensemble learning method nowaday, ",
        "Random Forests inherit the advanatges of tree-based methods including ",
        "automatically taking account of interaction and missing values.",
        "The bagging (bootstrap aggregation) helps grow trees ",
        "with weak predictors and thus reduce variance by average trees like: "),
      helpText("\\(Var(\\hat{Y})= \\sigma^2 / \\sqrt{n} \\)"),
      tags$div(
        "The price of using Random Forest is the loss of interpretation of the model. ",
        "Also, the computation is a greedy algorithm .")
    ),
    tabPanel(
      title = "3B: Model Fitting", value = "Tab6",
      tags$h3("In modeling tabs 3B and 3C, the iris dataset is used to replace the process data.
              The modeling workflow remains unchanged."),
      tableOutput("t6_table"),
      uiOutput("t6_select.model"),
      plotOutput("t6_plot", height = "400px"),
      verbatimTextOutput("t6_model")
    ),
    tabPanel(
      title = "3C: Prediction", value = "Tab7",
      uiOutput("t7_select.model"),
      uiOutput("num1"),
      uiOutput("num2"),
      uiOutput("num3"),
      uiOutput("num4"),
      uiOutput("t7_prediction")
    )
  )
)



# 05 ui -------------------------------------------------------------------

# ui
ui <- dashboardPage(header, sidebar, body)



# 06 - server -------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100*1024^2)
  
  
  # 07 - server - tab1 and tab2 ---------------------------------------------
  
  t1_rxdf.raw <- reactive({
    req(input$t1_file)
    # read the selected data file
    input$t1_file$datapath %>% 
      set_names(input$t1_file$name) %>% 
      map_dfr(ToolLog_read.log, .id = "File") 
  })
  
  
  t1_rxdf.ref <- reactive({
    # track changes of CurrentState and save in a reference table 
    t1_rxdf.raw() %>% 
      ToolLog_track.change(CurrentState)
  })
  
  
  output$t1_table.ref <- renderDT({
    validate(
      need(input$t1_file, "Select a raw process file (`data` folder) to start")
    )
    
    t1_rxdf.ref() %>%
      #select(-rowID) %>% 
      datatable(
        options = list(
          pageLength = -1, info = FALSE,
          lengthMenu = list(c(12, -1), c("12", "All")),
          scrollY = "700px", #scrollX = TRUE, 
          order = list(list(1, 'asc')),
          rownames= FALSE
        )
      )
  })
  
  
  output$t1_slider.range <- renderUI({
    max <- t1_rxdf.ref() %>% nrow()
    sliderInput(
      "t1_row.range", "Select row range of a selected run", 
      min = 1, max = max, value = c(6, 11), 
      step = 1, round = TRUE, ticks = FALSE, dragRange = TRUE
    )
  })
  
  
  output$t1_slider.align <- renderUI({
    req(input$t1_row.range)
    min <- input$t1_row.range[1]
    max <- input$t1_row.range[2]
    sliderInput(
      "t1_row.align", "Select row to align the process time", 
      min = min, max = max, value = min + 1, 
      step = 1, round = TRUE, ticks = FALSE
    )
  })
  
  
  output$t1_num.align <- renderUI({
    req(input$t1_row.align)
    numericInput("t1_time.align", "Process time offset [sec]", value = 1000)
  })
  
  
  t1_rx.text <- reactive({
    req(input$t1_row.align)
    paste(
      "Click to extract from rowID",
      t1_rxdf.ref()$rowID[input$t1_row.range[1]],
      "to",
      t1_rxdf.ref()$rowID.last[input$t1_row.range[2]],
      "<br/> Align process time =",
      input$t1_time.align,
      "at rowID",
      t1_rxdf.ref()$rowID[input$t1_row.align]
    )
    
  })
  
  
  output$t1_btn.extract <- renderUI({
    req(input$t1_row.align)
    actionButton(
      "t1_extract", HTML(t1_rx.text()), 
      width = "90%", class = "btn-block",
      style = "height:60px"
    )
  })
  
  
  t2_rxdf.run <- eventReactive(input$t1_extract, {
    row.start <- t1_rxdf.ref()$rowID[input$t1_row.range[1]]
    row.end <- t1_rxdf.ref()$rowID.last[input$t1_row.range[2]]
    row.align <- t1_rxdf.ref()$rowID[input$t1_row.align]
    
    t1_rxdf.raw() %>% 
      ToolLog_extract.align(
        row.start = row.start, row.end = row.end, 
        row.align = row.align, time.align = input$t1_time.align
      )
  })
  
  
  output$t2_table.run <- renderDT({
    validate(
      need(input$t1_file, "Select a raw process file (`data` folder) to start"),
      need(input$t1_extract, "Then extract unit process data.")
    )
    
    t2_rxdf.run() %>%
      select(rowID, `Process Time [s]`, DateTime, Batch, CurrentState) %>% 
      datatable(
        options = list(
          pageLength = -1, info = FALSE,
          lengthMenu = list(c(12, -1), c("12", "All")),
          scrollY = "700px", #scrollX = TRUE, 
          order = list(list(1, 'asc')),
          rownames= FALSE
        )
      )
  })
  
  
  # switch to panel 2
  observeEvent(
    input$t1_extract, {
      updateTabsetPanel(session, "Tabset", selected = "Tab2")
      output$t2_btn.download <- renderUI({
        downloadButton("t2_download", "Click here to download")
      })
    }
  )
  
  
  # download extracted run log data
  output$t2_download <- downloadHandler(
    filename = "run log data.csv",
    content = function(file){
      write_csv(t2_rxdf.run(), file)    
    },
    contentType = "text/csv"
  )
  
  
  
  # 08 server - tab3 and tab4 -----------------------------------------------
  
  t3_rxdf.raw <- reactive({
    req(input$t3_file)
    # read the selected data files
    input$t3_file$datapath %>% 
      set_names(input$t3_file$name) %>% 
      map_dfr(read_csv, .id = "File") 
  })
  
  
  output$t3_slider.range <- renderUI({
    min <- t3_rxdf.raw()$`Process Time [s]` %>% min()
    max <- t3_rxdf.raw()$`Process Time [s]` %>% max()
    numericRangeInput(
      "t3_time.range", "Select range of process time [s]", 
      #min = min, max = max, 
      value = c(min, max) 
      #step = 1, round = TRUE, ticks = FALSE
    )
  })
  
  
  output$t3_select.vars <- renderUI({
    validate(
      need(input$t3_file, "Select one or multiple unit process files (`runs` folder) to start")
    )
    pickerInput(
      "t3_vars", "Variables of interest",  
      # list all numeric columns
      choices = t3_rxdf.raw() %>% select(where(is.numeric)) %>% names() %>% sort(),
      width = "100%", inline = FALSE,
      options = list(`live-search` = TRUE),
      multiple = TRUE
    )
  })
  
  
  output$t3_plotly <- renderPlotly({
    req(input$t3_time.range, input$t3_vars)
    t3_rxdf.raw() %>% 
      ToolLog_plot(vars.y = input$t3_vars, range.x = input$t3_time.range) %>% 
      ggplotly() %>% 
      plotly::layout(
        #title = title,
        #scene = plotly_layout.scene,
        # force to show lengend even if there is only one group
        showlegend = TRUE,
        #legend = list(x = 0, y = .95, orientation = "h")
        legend = list(x = 0, y = -110)
      )
  })
  
  
  # setting for panels
  # panel 1 (bottom)
  output$t4_select.vars.y1 <- renderUI({
    req(input$t3_vars)
    pickerInput(
      "t4_vars.y1", "Panel 1 (bottom): Main Variable",  
      choices = input$t3_vars %>% sort(),
      multiple = FALSE
    )
  })
  
  output$t4_text.lab.y1 <- renderUI({
    req(input$t4_vars.y1)
    textInput(
      "t4_lab.y1", "Panel 1 (bottom): Input to Customize Label ",
      value = paste(input$t4_vars.y1, collapse = ", ")
    )
  })
  
  
  # panel 2
  output$t4_select.vars.y2 <- renderUI({
    req(input$t4_vars.y1)
    pickerInput(
      "t4_vars.y2", "Panel 2: Single or Multiple Variables",  
      choices = input$t3_vars %>% sort(),
      multiple = TRUE
    )
  })
  
  output$t4_text.lab.y2 <- renderUI({
    req(input$t4_vars.y1, input$t4_vars.y2)
    textInput(
      "t4_lab.y2", "Panel 2: Input to Customize Label",
      value = paste(input$t4_vars.y2, collapse = ", ")
    )
  })
  
  
  # panel 3
  output$t4_select.vars.y3 <- renderUI({
    req(input$t4_vars.y1, input$t4_vars.y2)
    pickerInput(
      "t4_vars.y3", "Panel 3: Single or Multiple Variables",  
      choices = input$t3_vars %>% sort(),
      multiple = TRUE
    )
  })
  
  output$t4_text.lab.y3 <- renderUI({
    req(input$t4_vars.y1, input$t4_vars.y2, input$t4_vars.y3)
    textInput(
      "t4_lab.y3", "Panel 3: Input to Customize Label",
      value = paste(input$t4_vars.y3, collapse = ", ")
    )
  })
  
  
  # panel 4 (top)
  ## default = panel 3
  output$t4_select.vars.y4 <- renderUI({
    req(input$t4_vars.y1, input$t4_vars.y2, input$t4_vars.y3)
    pickerInput(
      "t4_vars.y4", "Panel 4 (top, optional): Variables",  
      choices = input$t3_vars %>% sort(),
      selected = input$t4_vars.y3,
      multiple = TRUE
    )
  })
  
  output$t4_text.lab.y4 <- renderUI({
    req(input$t4_vars.y1, input$t4_vars.y2, input$t4_vars.y3, input$t4_vars.y4)
    textInput(
      "t4_lab.y4", "Panel 4 (top, optional): Input to Customize Label",
      value = paste(input$t4_vars.y4, collapse = ", ")
    )
  })
  
  
  # switch to panel 4
  observeEvent(
    input$t4_lab.y4, {
      updateTabsetPanel(session, "Tabset", selected = "Tab4")
      output$t4_btn.download <- renderUI({
        downloadButton("t4_download", "Click here to download")
      })
      output$t4_check.downloadAll <- renderUI({
        checkboxInput("t4_downloadAll", "Download all variables")
      })
    }
  )
  
  
  # plot panels
  output$t4_plot <- renderPlot({
    validate(
      need(input$t3_file, "Select one or multiple unit process files (`runs` folder) to start"),
      need(
        input$t4_vars.y3, 
        "Select variables of interest on tab 2A and vairables for panel 1, 2 and 3 on side panel to continue.")
    )
    
    req(input$t4_vars.y1, input$t4_vars.y2, input$t4_vars.y3, input$t4_vars.y4)
    
    t3_rxdf.raw() %>%
      ToolLog_plot.panel(
        input$t4_vars.y1, input$t4_vars.y2, input$t4_vars.y3, input$t4_vars.y4,
        input$t4_lab.y1, input$t4_lab.y2, input$t4_lab.y3, input$t4_lab.y4,
        range.x = input$t3_time.range, solid.line = TRUE
      )
  })
  
  
  # type of summary table
  output$t4_select.type <- renderUI({
    req(input$t4_vars.y1)
    pickerInput(
      "t4_type", paste("Numeric summary of main variable ", input$t4_vars.y1),  
      choices = c("min", "max", "mean", "std"),
      selected = "mean",
      multiple = FALSE
    )
  })
  
  
  # summary table
  output$t4_table <- renderTable({
    req(input$t4_vars.y1)
    
    t3_rxdf.raw() %>%
      select(File, !!sym(input$t4_vars.y1)) %>% 
       group_by(File) %>% 
       summarise(
         min = min(!!sym(input$t4_vars.y1)),
         max = max(!!sym(input$t4_vars.y1)),
         mean = mean(!!sym(input$t4_vars.y1)),
         std = sd(!!sym(input$t4_vars.y1))
       ) %>% 
      select(File, !!sym(input$t4_type))
  }) 
  
  
  # type of summary graph
  output$t4_select.type_graph <- renderUI({
    req(input$t4_vars.y1)
    pickerInput(
      "t4_type_plot", paste("Graphic summary of main variable ", input$t4_vars.y1),  
      choices = c("histogram", "density"),
      selected = "histogram",
      multiple = FALSE
    )
  })
  
  # summary graph
  output$t4_graph <- renderPlot({
    req(input$t4_vars.y1)
    
    p1 <- t3_rxdf.raw() %>%
      select(File, !!sym(input$t4_vars.y1)) %>% 
      ggplot(aes(x = !!sym(input$t4_vars.y1)))
    
    if(input$t4_type_plot == "boxplot") {
      p1 <- p1 + geom_boxplot()
    } else if (input$t4_type_plot == "histogram") {
      p1 <- p1 + geom_histogram()
    } else if (input$t4_type_plot == "density") {
      p1 <- p1 + geom_density()
    }

    p1 + facet_wrap(vars(File), nrow = 1)
  })
  
  
  
  # download time aligned log data
  output$t4_download <- downloadHandler(
    filename = "time aligned log data.csv",
    content = function(file){
      
      if(input$t4_downloadAll == TRUE){
        df <- t3_rxdf.raw()
      } else {
        df <- t3_rxdf.raw() %>%
          select(File, `Process Time [s]`, CurrentState, all_of(input$t3_vars))
      }
      
      df <- df %>% 
        mutate(File = tools::file_path_sans_ext(basename(File))) %>% 
        relocate(File, `Process Time [s]`, CurrentState)
      
      write_csv(df, file)
    },
    contentType = "text/csv"
  )
  

  # 09 server - tab5, tab6 and tab7 -----------------------------------------
  
  # use iris for modeling section
  data("iris")
  df <- iris %>% 
    mutate(Species = factor(Species)) %>% 
    relocate(Species)  
  
  
  # select training/test splitting ratio
  output$t5_slider.split <- renderUI({
    sliderInput(
      "t5_split.p", "Select percentage of training set split", 
      min = 50, max = 80, 
      value = 70 
      #step = 1, round = TRUE, ticks = FALSE
    )
  })
  
  
  # select cv folds
  output$t5_slider.folds <- renderUI({
    sliderInput(
      "t5_folds", "Select number of folds for cross validation", 
      min = 3, max = 15, 
      value = 10 
      #step = 1, round = TRUE, ticks = FALSE
    )
  })
  
  # select cv repeats
  output$t5_slider.repeats <- renderUI({
    sliderInput(
      "t5_repeats", "Select number of repeats for cross validation", 
      min = 1, max = 5, 
      value = 3 
      #step = 1, round = TRUE, ticks = FALSE
    )
  })
  
  
  # select range of hyperparameters for Multinomial Logistic Regression
  output$t5_slider.model1 <- renderUI({
    sliderInput(
      "t5_model1", "Select range of tuning parameter: weight decay", 
      min = 1, max = 20, value = c(1, 15), 
      step = 1, round = TRUE, ticks = FALSE
    )
  })
  
  # select range of hyperparameters for KNN
  output$t5_slider.model2 <- renderUI({
    sliderInput(
      "t5_model2", "Select range of tuning parameter: # of neighbors", 
      min = 1, max = 20, value = c(1, 15), 
      step = 1, round = TRUE, ticks = FALSE
    )
  })
  
  
  # select range of hyperparameters for Random Forests
  output$t5_slider.model3 <- renderUI({
    sliderInput(
      "t5_model3", "Select range of tuning parameter: # of randomly selected predictors", 
      min = 1, max = 20, value = c(1, 15), 
      step = 1, round = TRUE, ticks = FALSE
    )
  })
  
  
  # fit model button
  output$t5_btn.fit <- renderUI({
    actionButton(
      "t5_fit", HTML("Fit Models<br>(This takes a minute to run!)"), 
      width = "90%", class = "btn-block",
      style = "height:60px"
    )
  })
  
  
  # fit models after button is clicked
  t5_rxlst <- eventReactive(input$t5_fit, {
    # split train/test sets
    set.seed(2022)
    trainIndex <- createDataPartition(df$Species, p = input$t5_split.p/100, list = FALSE)
    df_train <- df[trainIndex, ]
    df_test <- df[-trainIndex, ]
    
    # initiate a list to save the results
    fittings <- list()
    
    # fit_model 1 - Multinomial Logistic Regression
    name <- "Multinomial Logistic Regression"
    method <-  "multinom"
    tuneGrid <- expand.grid(decay  = input$t5_model1[1]:input$t5_model1[2])
    # fit the model
    fitting <- fit_model(
      Species ~ ., df_train, df_test, method = method, name = name,
      trControl = trainControl(method = "repeatedcv", number = input$t5_folds, repeats = input$t5_repeats),
      tuneGrid = tuneGrid,
      trace = FALSE)
    # save the fittings
    fittings <- append(fittings, list(fitting))
    
    # fit_model 2 - KNN
    name <- "KNN Euclidean"
    method <-  "knn"
    tuneGrid <- expand.grid(k = input$t5_model2[1]:input$t5_model2[2])
    # fit the model
    fitting <- fit_model(
      Species ~ ., df_train, df_test, method = method, name = name,
      trControl = trainControl(method = "repeatedcv", number = input$t5_folds, repeats = input$t5_repeats),
      tuneGrid = tuneGrid)
    # save the fittings
    fittings <- append(fittings, list(fitting))
    
    # fit_model 3 - Random Forests
    name <- "Random Forests"
    method <-  "rf"
    tuneGrid <- expand.grid(mtry = input$t5_model3[1]:input$t5_model3[2])
    # fit the model
    fitting <- fit_model(
      Species ~ ., df_train, df_test, method = method, name = name,
      trControl = trainControl(method = "repeatedcv", number = input$t5_folds, repeats = input$t5_repeats),
      tuneGrid = tuneGrid)
    # save the fittings
    fittings <- append(fittings, list(fitting))
    
    fittings
  })
  
  
  
  # switch to panel 6
  observeEvent(
    t5_rxlst(), {
      updateTabsetPanel(session, "Tabset", selected = "Tab6")
    }
  )
  
  # show fit summary table with best hyperparameters and metrics
  output$t6_table <- renderTable({
    validate(
      need(input$t5_fit, "Fit the models to continue for model comparison and predictions.")
    )
    
    df <- tibble()
    for(i in 1:3){ df <- bind_rows(df, t5_rxlst()[[i]]$result) }
    df
  }) 
  
  
  # select which model's hyperparameter tuning plot
  output$t6_select.model <- renderUI({
    req(t5_rxlst())
    pickerInput(
      "t6_model", "Hyperparameter tuning plot for the model:",  
      choices = list(
        "Multinomial Logistic Regression" = 1, 
        "KNN Euclidean" = 2,
        "Random Forests" = 3),
      selected = 1,
      multiple = FALSE
    )
  })
  
  
  # hyperparameter tuning plot of the selected model
  output$t6_plot <- renderPlot({
    req(t5_rxlst())
    plot_modelinfo(t5_rxlst()[[as.numeric(input$t6_model)]]$fit)
  })
  
  
  # show fit summary of the selected model
  output$t6_model <- renderPrint({
    req(t5_rxlst())
    
    summary(t5_rxlst()[[as.numeric(input$t6_model)]]$fit)
  })
  
  
  
  # select which model to make prediction
  output$t7_select.model <- renderUI({
    validate(
      need(input$t5_fit, "Fit the models to continue for model comparison and predictions.")
    )
    
    req(t5_rxlst())
    pickerInput(
      "t7_model", "Choose model to make prediction:",  
      choices = list(
        "Multinomial Logistic Regression" = 1, 
        "KNN Euclidean" = 2,
        "Random Forests" = 3),
      selected = 1,
      multiple = FALSE
    )
  })
  
  # numericInput("num1", label = names(iris)[1], value = 1),
  # predictors
  output$num1 <- renderUI({
    req(t5_rxlst())
    numericInput("num1_value", label = names(iris)[1], value = 1)
  })
  output$num2 <- renderUI({
    req(t5_rxlst())
    numericInput("num2_value", label = names(iris)[2], value = 1)
  })
  output$num3 <- renderUI({
    req(t5_rxlst())
    numericInput("num3_value", label = names(iris)[3], value = 1)
  })
  output$num4 <- renderUI({
    req(t5_rxlst())
    numericInput("num4_value", label = names(iris)[4], value = 1)
  })
  
  # prediction
  output$t7_prediction <- renderUI({
    req(t5_rxlst())
    pred <- predict(
      t5_rxlst()[[as.numeric(input$t7_model)]]$fit,
      newdata = data.frame(
        Sepal.Length = input$num1_value,
        Sepal.Width = input$num2_value,
        Petal.Length = input$num3_value,
        Petal.Width = input$num4_value))
    h4(paste("Prediction = ", pred))
  })
  
    
}



# 10 app ------------------------------------------------------------------


# Run the application 
shinyApp(ui = ui, server = server) %>% runApp(launch.browser = TRUE)

