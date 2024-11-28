library(rsconnect)  # For deploying Shiny apps to shinyapps.io or other servers
library(shiny)       # Core library for building interactive web applications
library(bs4Dash)     # For creating dashboards using Bootstrap 4 with Shiny
#library(fresh)      # For customizing the theme
library(highcharter) # For creating interactive charts using Highcharts


#theme <- create_theme(
#  bs4dash_color(
#    lime = "#52A1A5",
#    olive = "#4A9094",
#    purple = "#8965CD"
#  ),
#  bs4dash_status(
#    primary = "#E1EDED",
#    info = "#E4E4E4"
#  )
#)

ui <- dashboardPage(
#  freshTheme = theme,
  help = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  title = "CPI Dashboard",
  header = dashboardHeader(
    title = dashboardBrand(
      title = "CPI Dashboard",
      image = "png-transparent.png"
    )
  ),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem(
        "Home",
        tabName = "home",
        icon = icon("home")
      ),
      menuItem(
        "Overview",
        tabName = "overview",
        icon = icon("bar-chart")
      ),
      menuItem(
        "Trend Analysis",
        tabName = "trend_analysis",
        icon = icon("line-chart")
      ),
      menuItem(
        "Resources",
        tabName = "resources",
        icon = icon("info"),
        
        menuSubItem(
          "Data",
          tabName = "data",
          icon = icon("database")
        ),
        menuSubItem(
          "Methodology",
          tabName = "methodology",
          icon = icon("book")
        )
      )
    )
  ),
  
  controlbar = dashboardControlbar(
    conditionalPanel(
      #Add a condition so that the filters below are only visible when the Overview tab is active.
      condition = "input.sidebarMenuid === 'overview'",
      div(
        style = "margin-bottom: 10px; padding: 5px;",
        selectInput(
          inputId = "select_year",
          label = "Select Year",
          choices = NULL,
          selected = NULL,
          width = "100%"
        )
      ),
      
      div(
        style = "margin-bottom: 10px; padding: 5px;",
        selectInput(
          inputId = "select_quarter",
          label = "Select Quarter",
          choices = c("Q1", "Q2", "Q3", "Q4"),
          width = "100%"
        )
      ),
      
      div(
        style = "padding: 5px;",
        selectInput(
          inputId = "select_location",
          label = "Select Location",
          choices = NULL,
          width = "100%"
        )
      )
    )
  ),
  
  
  footer = dashboardFooter(
    right = "© 2024 Vanuatu Bureau of Statistics. All rights reserved."
  ),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        
        jumbotron(
          title = "Welcome!!",
          status = "info",
          lead = "Welcome to the CPI Dashboard",
          "Explore insights and trends in the Consumer Price Index (CPI) data for Vanuatu. 
          Use the navigation menu to access different sections.",
          btnName = NULL
        ),
        
        fluidRow(
          userBox(
            collapsible = FALSE,
            title = userDescription(
              title = "Mr. Herman Tevilili",
              subtitle = "Statistician - Justice & Culture",
              image = "herman.jpg",
              type = 1
            ),
            status = "purple"
          ),
          box(
            width = 6,
            collapsible = FALSE,
            "This dashboard was created using R-Shiny as part of the Vanuatu Bureau of Statistics (VBoS) Dashboard Competition. It presents the Consumer Price Index (CPI) data for Vanuatu, offering valuable insights and trends for users. The interactive features of the dashboard enable in-depth exploration of the data, making it a powerful tool for understanding economic indicators."
          )
        )
      ),
      
      tabItem(
        tabName = "overview",
        
        fluidRow(
          column(
            width = 4,
            infoBoxOutput("cpi_change_quarter", width = 12)
          ),
          
          column(
            width = 4,
            infoBoxOutput("cpi_change_annual", width = 12)
          ),
          
          column(
            width = 4,
            infoBoxOutput("underlying_inflation", width = 12)
          )
        ),
        
        fluidRow(
          
          sortable(
            width = 6,
            
            box(
              title = uiOutput("dynamic_box_title"),
              width = 12,
              collapsible = FALSE,
              maximizable = TRUE,
              uiOutput("ribbon_ui"),
              highchartOutput("bar_chart")
            )
          ),
          
          sortable(
            width = 6,
            
            box(
              title = "Box 4",
              width = 12,
              collapsible = FALSE,
              maximizable = TRUE
              
            )
          )
          
        )
        
      ),
      
      tabItem(
        tabName = "trend_analysis",
        fluidRow(
          
            box(
              title = "Trend Analysis",
              
              width = 12,
              collapsible = FALSE,
              maximizable = TRUE,
              label = boxLabel(
                text = "info",
                status = "primary",
                tooltip = "Explore quarterly and annual CPI trends."
              ),
              
              ## Tab box ----
              tabBox(
                width = 12,
                type = "tabs",
                status = "olive",
                solidHeader = TRUE,
                
                tabPanel(
                  "Quarterly",
                  highchartOutput("quarterly_trend_chart")
                  
                ),
                tabPanel(
                  "Annual"
                )
                
              )
            )
        )
      ),
      
      tabItem(
        tabName = "trend_analysis"
      ),
      tabItem(
        tabName = "data",
        DTOutput("data_table")
      ),
      
      tabItem(
        tabName = "methodology",
        
        fluidRow(
          box(
            title = "Methodology Overview",
            width = 12,
            status = "info",
            collapsible = FALSE,
            solidHeader = TRUE,
            p("This section provides detailed insights into the technical implementation of the CPI Dashboard. 
        The app is designed to be fully reproducible and dynamically responsive to new data. 
        Whenever there is an update to the CPI data, the only requirement is to update the underlying 
        SQLite database. The dashboard, including charts, tables, and descriptive text, automatically 
        adapts to reflect the updated data.")
          )
        ),
        
        fluidRow(
          box(
            title = "Libraries Used",
            width = 6,
            status = "primary",
            collapsible = TRUE,
            solidHeader = TRUE,
            tags$ul(
              tags$li(strong("shiny:"), " Core library for building interactive web applications."),
              tags$li(strong("bs4Dash:"), " Enables the creation of aesthetically pleasing dashboards using Bootstrap 4."),
              tags$li(strong("highcharter:"), " Facilitates the creation of dynamic and interactive charts."),
              tags$li(strong("rsconnect:"), " Used for deploying the app to cloud platforms like shinyapps.io."),
              tags$li(strong("DBI & RSQLite:"), " Provide database connectivity and querying capabilities."),
              tags$li(strong("dplyr:"), " Streamlines data manipulation and filtering."),
              tags$li(strong("DT:"), " Renders interactive data tables.")
            )
          ),
          
          box(
            title = "UI Design with bs4Dash",
            width = 6,
            status = "success",
            collapsible = TRUE,
            solidHeader = TRUE,
            p("The app's UI is structured using ", strong("bs4Dash"), " components such as boxes, tabs, and ribbons."),
            tags$ul(
              tags$li(strong("dashboardPage:"), " Defines the structure of the dashboard, including the header, sidebar, and body."),
              tags$li(strong("box:"), " Encapsulates content like visualizations and text blocks."),
              tags$li(strong("tabBox:"), " Organizes content into tabs for better navigation."),
              tags$li(strong("ribbon:"), " Adds dynamic visual context to charts.")
            ),
            pre(
              code("ribbon(
  text = paste(input$select_quarter, input$select_year),
  color = 'olive'
)")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Data Management & Reproducibility",
            width = 6,
            status = "warning",
            collapsible = TRUE,
            solidHeader = TRUE,
            p("The app connects to an SQLite database to fetch CPI data. 
         This setup ensures that updates are seamless—simply replace or update the database, 
         and the dashboard automatically adapts to reflect the new data."),
            tags$ul(
              tags$li("Database integration is achieved using ", strong("DBI"), " and ", strong("RSQLite"), "."),
              tags$li("Data manipulation is performed with ", strong("dplyr"), "."),
              tags$li("Filters dynamically update visualizations and outputs without additional manual intervention.")
            ),
            pre(
              code("db_connection <- dbConnect(RSQLite::SQLite(), 'cpi.db')
expenditure_data <- dbReadTable(db_connection, 'exp_comp')
dbDisconnect(db_connection)")
            )
          ),
          
          box(
            title = "Dynamic Charts & Tables",
            width = 6,
            status = "danger",
            collapsible = TRUE,
            solidHeader = TRUE,
            p("The app uses ", strong("highcharter"), " and ", strong("DT"), " to create interactive charts and tables. 
         These components are directly tied to the reactive data pipeline, ensuring that changes in the 
         database are immediately reflected in the visualizations and data tables."),
            pre(
              code("output$bar_chart <- renderHighchart({
  data <- filtered_data() %>%
    mutate(Value = round(Value * 100, 1)) %>%
    filter(!is.na(Value))
  
  hchart(
    data,
    type = 'bar',
    hcaes(x = ExpenditureGroup, y = Value)
  ) %>% 
    hc_chart(inverted = TRUE) %>% 
    hc_xAxis(title = list(text = 'Expenditure Group'), categories = data$ExpenditureGroup) %>%
    hc_yAxis(title = list(text = 'CPI Change (%)'), gridLineWidth = 1) %>%
    hc_tooltip(pointFormat = 'CPI Change: <b>{point.y}%</b>')
})")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Deployment",
            width = 12,
            status = "secondary",
            collapsible = TRUE,
            solidHeader = TRUE,
            p("The app is deployable using ", strong("rsconnect"), ". Ensure all dependencies are installed before deployment. 
        The reproducibility design ensures minimal maintenance even after deployment."),
            pre(
              code("rsconnect::deployApp()")
            )
          )
        )
      )
      
      
    )
  )
)


server <- function(input, output, session) {
  
  # Load necessary libraries
  library(DBI)
  library(RSQLite)
  library(highcharter)
  library(dplyr)
  library(DT)
  
  # Connect to SQLite database
  db_connection <- dbConnect(RSQLite::SQLite(), "cpi.db")
  
  # Load dataset
  expenditure_data <- dbReadTable(db_connection, "exp_comp")
  
  # Disconnect from the database
  dbDisconnect(db_connection)
  
  # Extract unique years and locations
  unique_years <- sort(unique(expenditure_data$Year), decreasing = TRUE)
  unique_locations <- unique(expenditure_data$Location)
  
  # Populate year and location filters
  updateSelectInput(session, "select_year", choices = unique_years, selected = unique_years[1])
  updateSelectInput(session, "select_location", choices = unique_locations) 
  
  # Filter data reactively
  filtered_data <- reactive({
    req(input$select_year, input$select_location, input$select_quarter)
    # Map "Q1", "Q2", etc. to the corresponding quarter in the dataset
    quarter_map <- c("Q1" = "1st", "Q2" = "2nd", "Q3" = "3rd", "Q4" = "4th")
    filtered_data <- expenditure_data %>%
      filter(Year == input$select_year, Location == input$select_location, 
             Quarter == quarter_map[input$select_quarter])
    return(filtered_data)
  })
  
  # Update box title dynamically
  output$dynamic_box_title <- renderUI({
    paste("Quarterly CPI Changes for", input$select_location, "in", input$select_year, "during", input$select_quarter)
  })
  
  # Quarterly CPI Change
  cpi_data_quarter <- expenditure_data[expenditure_data$Indicator == "Quarter on Quarter Percentage Change" & 
                                         expenditure_data$ExpenditureGroup == "All Groups", ]
  cpi_data_quarter <- cpi_data_quarter[order(cpi_data_quarter$id, decreasing = TRUE), ]  # Order by id in descending order
  
  # Filter the data by selected quarter
  cpi_data_quarter_filter <- reactive({
    req(input$select_location, input$select_quarter)  # Ensure location and quarter are selected before proceeding
    quarter_map <- c("Q1" = "1st", "Q2" = "2nd", "Q3" = "3rd", "Q4" = "4th")
    filtered_data <- expenditure_data %>%
      filter(Indicator == "Quarter on Quarter Percentage Change", 
             Year == input$select_year,
             Location == input$select_location,
             Quarter == quarter_map[input$select_quarter]) %>%
      arrange(id)
    return(filtered_data)
  })
  
  # Find the row with the highest id for the quarter
  highest_id_quarter_row <- cpi_data_quarter[which.max(cpi_data_quarter$id), ]  # Find the row with the highest id
  
  # Extract the Quarter and Year for the highest id
  # Map the Quarter to Q1, Q2, Q3, or Q4
  quarter_display <- ifelse(
    highest_id_quarter_row$Quarter == "1st", "Q1",
    ifelse(
      highest_id_quarter_row$Quarter == "2nd", "Q2",
      ifelse(
        highest_id_quarter_row$Quarter == "3rd", "Q3",
        ifelse(
          highest_id_quarter_row$Quarter == "4th", "Q4",
          NA  # Handle unexpected cases
        )
      )
    )
  )
  
  # Combine Quarter and Year for display
  quarter_text <- paste0(quarter_display, " ", highest_id_quarter_row$Year)
  
  # Dynamically update the ribbon text based on user input
  output$ribbon_ui <- renderUI({
    ribbon(
      text = paste(input$select_quarter, input$select_year),  # Use user input directly
      color = "olive"
    )
  })
  
  output$data_table <- renderDT({
    
    expenditure_data %>% 
      select(Year, Location, ExpenditureGroup, Indicator, Value, Quarter) %>% 
      unique() %>% 
      rename(
        `Expenditure Group` = ExpenditureGroup
      )
    
  })
  
  # Generate the horizontal bar chart with Highcharter based on quarter filter
  output$bar_chart <- renderHighchart({
    data <- cpi_data_quarter_filter() %>%
      mutate(Value = round(Value * 100, 1)) %>%
      filter(!is.na(Value))
    
    hchart(
      data,
      type = "bar",
      hcaes(x = ExpenditureGroup, y = Value)
    ) %>% 
      hc_chart(inverted = TRUE) %>% 
      hc_xAxis(title = list(text = "Expenditure Group"), categories = data$ExpenditureGroup) %>%
      hc_yAxis(title = list(text = "CPI Change (%)"), gridLineWidth = 1) %>%
      hc_tooltip(pointFormat = "CPI Change: <b>{point.y}%</b>") %>% 
      hc_subtitle(text = "Source: Vanuatu Bureau of Statistics") %>% 
      hc_add_theme(hc_theme_google()) %>% 
      hc_exporting(enabled = TRUE) %>%  # Enable export options
      hc_chart(zoomType = "xy") 
  })
  
  #Trend analysis
  
  # Filter data for "Quarter on Quarter Percentage Change"
  quarterly_trend_data <- expenditure_data %>%
    filter(Indicator == "Quarter on Quarter Percentage Change") %>%
    arrange(Location, Year, Quarter)
  
  # Create a reactive dataset to apply user-selected filters dynamically
  filtered_quarterly_trend_data <- reactive({
    req(input$select_year)  # Ensure year input is selected
    quarterly_trend_data %>%
      filter(Year == input$select_year) %>%
      mutate(
        Quarter = factor(
          Quarter,
          levels = c("1st", "2nd", "3rd", "4th"),
          labels = c("Q1", "Q2", "Q3", "Q4")
        )
      )
  })
  
  # Render the line chart
  output$quarterly_trend_chart <- renderHighchart({
    # Get the filtered data
    data <- filtered_quarterly_trend_data()
    
    # Create the highchart object
    hchart(
      data,
      type = "line",
      hcaes(
        x = Quarter,
        y = Value,
        group = Location
      )
    ) %>%
      hc_title(
        text = paste("Quarterly CPI Trends for", input$select_year)
      ) %>%
      hc_xAxis(
        title = list(text = "Quarter"),
        categories = c("Q1", "Q2", "Q3", "Q4")
      ) %>%
      hc_yAxis(
        title = list(text = "CPI Change (%)"),
        labels = list(format = "{value}%"),
        gridLineWidth = 1
      ) %>%
      hc_tooltip(
        shared = TRUE,
        pointFormat = "<b>{series.name}:</b> {point.y:.2f}%<br>"
      ) %>%
      hc_plotOptions(
        line = list(
          marker = list(
            enabled = TRUE
          )
        )
      ) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_subtitle(text = "Source: Vanuatu Bureau of Statistics")
  })
  
  
  
}

shinyApp(ui, server)

