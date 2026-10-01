# ============================================================
# SALES PERFORMANCE DASHBOARD
# Professional Light Theme - R Shiny
# ============================================================

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(bslib)

# ============================================================
# LOAD DATA
# ============================================================

sales_data <- read_excel("Amazon 2_Raw.xlsx")

# Convert column names
names(sales_data) <- as.character(names(sales_data))

# Convert data types
sales_data$Sales <- as.numeric(sales_data$Sales)
sales_data$Profit <- as.numeric(sales_data$Profit)
sales_data$Quantity <- as.numeric(sales_data$Quantity)

sales_data$`Order Date` <- as.Date(
  sales_data$`Order Date`
)

sales_data$Category <- as.character(
  sales_data$Category
)

sales_data$Geography <- as.character(
  sales_data$Geography
)

sales_data$`Product Name` <- as.character(
  sales_data$`Product Name`
)


# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  # ----------------------------------------------------------
  # CUSTOM CSS
  # ----------------------------------------------------------
  
  tags$head(
    
    tags$style(HTML("

      body {
        background-color: #F5F7FB;
        font-family: 'Segoe UI', Arial, sans-serif;
        color: #30384D;
      }

      .container-fluid {
        padding-left: 35px;
        padding-right: 35px;
      }

      /* HEADER */

      .dashboard-header {
        background: #FFFFFF;
        padding: 25px 30px;
        border-radius: 14px;
        margin-bottom: 22px;
        box-shadow: 0 3px 15px rgba(45,55,72,0.06);
        border-left: 5px solid #6C7AE0;
      }

      .dashboard-title {
        font-size: 28px;
        font-weight: 700;
        color: #30384D;
        margin-bottom: 5px;
      }

      .dashboard-subtitle {
        color: #7B8499;
        font-size: 14px;
      }

      /* FILTER BAR */

      .filter-card {
        background: #FFFFFF;
        padding: 20px 22px;
        border-radius: 14px;
        margin-bottom: 22px;
        box-shadow: 0 3px 15px rgba(45,55,72,0.05);
      }

      .filter-title {
        font-size: 15px;
        font-weight: 600;
        color: #4B556B;
        margin-bottom: 15px;
      }

      .form-control {
        border: 1px solid #E2E6EF;
        border-radius: 8px;
        box-shadow: none;
      }

      .form-control:focus {
        border-color: #9AA7E8;
        box-shadow: 0 0 0 2px rgba(108,122,224,0.10);
      }

      /* KPI CARDS */

      .kpi-card {
        background: #FFFFFF;
        border-radius: 14px;
        padding: 20px;
        margin-bottom: 20px;
        min-height: 125px;
        box-shadow: 0 3px 15px rgba(45,55,72,0.06);
        border: 1px solid #EEF0F5;
      }

      .kpi-label {
        color: #858EA3;
        font-size: 13px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }

      .kpi-value {
        color: #30384D;
        font-size: 25px;
        font-weight: 700;
        margin-top: 8px;
      }

      .kpi-icon {
        float: right;
        width: 42px;
        height: 42px;
        border-radius: 10px;
        text-align: center;
        padding-top: 9px;
        font-size: 20px;
      }

      .icon-blue {
        background: #EEF2FF;
      }

      .icon-green {
        background: #ECF8F2;
      }

      .icon-purple {
        background: #F3EEFF;
      }

      .icon-orange {
        background: #FFF5E8;
      }

      /* SECTION CARDS */

      .chart-card {
        background: #FFFFFF;
        border-radius: 14px;
        padding: 20px;
        margin-bottom: 22px;
        box-shadow: 0 3px 15px rgba(45,55,72,0.05);
        border: 1px solid #EEF0F5;
      }

      .chart-title {
        font-size: 16px;
        font-weight: 650;
        color: #3E465A;
        margin-bottom: 15px;
      }

      /* BUTTON */

      .download-btn {
        background-color: #6C7AE0 !important;
        border: none !important;
        border-radius: 8px !important;
        padding: 9px 18px !important;
        color: white !important;
      }

      .reset-btn {
        background-color: #EEF1F7 !important;
        border: none !important;
        border-radius: 8px !important;
        color: #596275 !important;
      }

      /* TABLE */

      .dataTables_wrapper {
        color: #555F73;
      }

      table.dataTable thead th {
        background-color: #F5F7FB !important;
        color: #596275 !important;
        border-bottom: 1px solid #E4E7EF !important;
      }

      /* REMOVE SIDEBAR STYLE */

      .well {
        background-color: transparent;
        border: none;
        box-shadow: none;
      }

    "))
  ),
  
  
  # ==========================================================
  # HEADER
  # ==========================================================
  
  div(
    class = "dashboard-header",
    
    div(
      class = "dashboard-title",
      "Sales Performance Dashboard"
    ),
    
    div(
      class = "dashboard-subtitle",
      "Interactive overview of sales, profit, products and geographical performance"
    )
  ),
  
  
  # ==========================================================
  # FILTER BAR
  # ==========================================================
  
  div(
    class = "filter-card",
    
    div(
      class = "filter-title",
      "FILTERS"
    ),
    
    fluidRow(
      
      column(
        3,
        
        selectInput(
          "category",
          "Category",
          choices = NULL
        )
      ),
      
      column(
        3,
        
        selectInput(
          "geography",
          "Geography",
          choices = NULL
        )
      ),
      
      column(
        4,
        
        dateRangeInput(
          "date",
          "Order Date",
          start = min(
            sales_data$`Order Date`,
            na.rm = TRUE
          ),
          end = max(
            sales_data$`Order Date`,
            na.rm = TRUE
          )
        )
      ),
      
      column(
        2,
        
        br(),
        
        downloadButton(
          "downloadData",
          "Download Data",
          class = "download-btn"
        ),
        
        br(),
        br(),
        
        actionButton(
          "reset",
          "Reset Filters",
          class = "reset-btn"
        )
      )
    )
  ),
  
  
  # ==========================================================
  # KPI CARDS
  # ==========================================================
  
  fluidRow(
    
    column(
      3,
      
      div(
        class = "kpi-card",
        
        div(
          class = "kpi-icon icon-blue",
          "₹"
        ),
        
        div(
          class = "kpi-label",
          "Total Sales"
        ),
        
        div(
          class = "kpi-value",
          textOutput("totalSales")
        )
      )
    ),
    
    column(
      3,
      
      div(
        class = "kpi-card",
        
        div(
          class = "kpi-icon icon-green",
          "↗"
        ),
        
        div(
          class = "kpi-label",
          "Total Profit"
        ),
        
        div(
          class = "kpi-value",
          textOutput("totalProfit")
        )
      )
    ),
    
    column(
      3,
      
      div(
        class = "kpi-card",
        
        div(
          class = "kpi-icon icon-purple",
          "▣"
        ),
        
        div(
          class = "kpi-label",
          "Quantity Sold"
        ),
        
        div(
          class = "kpi-value",
          textOutput("totalQuantity")
        )
      )
    ),
    
    column(
      3,
      
      div(
        class = "kpi-card",
        
        div(
          class = "kpi-icon icon-orange",
          "🛒"
        ),
        
        div(
          class = "kpi-label",
          "Total Orders"
        ),
        
        div(
          class = "kpi-value",
          textOutput("totalOrders")
        )
      )
    )
  ),
  
  
  # ==========================================================
  # SALES TREND
  # ==========================================================
  
  div(
    class = "chart-card",
    
    div(
      class = "chart-title",
      "Sales Trend"
    ),
    
    plotlyOutput(
      "salesTrend",
      height = "360px"
    )
  ),
  
  
  # ==========================================================
  # CATEGORY + GEOGRAPHY
  # ==========================================================
  
  fluidRow(
    
    column(
      6,
      
      div(
        class = "chart-card",
        
        div(
          class = "chart-title",
          "Sales by Category"
        ),
        
        plotlyOutput(
          "categoryChart",
          height = "330px"
        )
      )
    ),
    
    column(
      6,
      
      div(
        class = "chart-card",
        
        div(
          class = "chart-title",
          "Sales by Geography"
        ),
        
        plotlyOutput(
          "geographyChart",
          height = "330px"
        )
      )
    )
  ),
  
  
  # ==========================================================
  # TOP PRODUCTS
  # ==========================================================
  
  div(
    class = "chart-card",
    
    div(
      class = "chart-title",
      "Top 10 Products by Sales"
    ),
    
    plotlyOutput(
      "productChart",
      height = "450px"
    )
  ),
  
  
  # ==========================================================
  # DATA TABLE
  # ==========================================================
  
  div(
    class = "chart-card",
    
    div(
      class = "chart-title",
      "Sales Details"
    ),
    
    DTOutput(
      "salesTable"
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # FILTER OPTIONS
  # ----------------------------------------------------------
  
  updateSelectInput(
    session,
    "category",
    choices = c(
      "All",
      sort(
        unique(
          sales_data$Category
        )
      )
    ),
    selected = "All"
  )
  
  
  updateSelectInput(
    session,
    "geography",
    choices = c(
      "All",
      sort(
        unique(
          sales_data$Geography
        )
      )
    ),
    selected = "All"
  )
  
  
  # ----------------------------------------------------------
  # FILTERED DATA
  # ----------------------------------------------------------
  
  filteredData <- reactive({
    
    df <- sales_data
    
    if (
      !is.null(input$category) &&
      input$category != "All"
    ) {
      
      df <- df[
        df$Category == input$category,
        ,
        drop = FALSE
      ]
    }
    
    
    if (
      !is.null(input$geography) &&
      input$geography != "All"
    ) {
      
      df <- df[
        df$Geography == input$geography,
        ,
        drop = FALSE
      ]
    }
    
    
    if (!is.null(input$date)) {
      
      df <- df[
        df$`Order Date` >= input$date[1] &
          df$`Order Date` <= input$date[2],
        ,
        drop = FALSE
      ]
    }
    
    df
  })
  
  
  # ----------------------------------------------------------
  # RESET
  # ----------------------------------------------------------
  
  observeEvent(
    input$reset,
    {
      
      updateSelectInput(
        session,
        "category",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "geography",
        selected = "All"
      )
      
      updateDateRangeInput(
        session,
        "date",
        
        start = min(
          sales_data$`Order Date`,
          na.rm = TRUE
        ),
        
        end = max(
          sales_data$`Order Date`,
          na.rm = TRUE
        )
      )
    }
  )
  
  
  # ==========================================================
  # KPI 1
  # ==========================================================
  
  output$totalSales <- renderText({
    
    value <- sum(
      filteredData()$Sales,
      na.rm = TRUE
    )
    
    paste0(
      "$",
      format(
        round(value, 2),
        big.mark = ",",
        nsmall = 2
      )
    )
  })
  
  
  # ==========================================================
  # KPI 2
  # ==========================================================
  
  output$totalProfit <- renderText({
    
    value <- sum(
      filteredData()$Profit,
      na.rm = TRUE
    )
    
    paste0(
      "$",
      format(
        round(value, 2),
        big.mark = ",",
        nsmall = 2
      )
    )
  })
  
  
  # ==========================================================
  # KPI 3
  # ==========================================================
  
  output$totalQuantity <- renderText({
    
    value <- sum(
      filteredData()$Quantity,
      na.rm = TRUE
    )
    
    format(
      value,
      big.mark = ","
    )
  })
  
  
  # ==========================================================
  # KPI 4
  # ==========================================================
  
  output$totalOrders <- renderText({
    
    value <- nrow(
      filteredData()
    )
    
    format(
      value,
      big.mark = ","
    )
  })
  
  
  # ==========================================================
  # SALES TREND
  # ==========================================================
  
  output$salesTrend <- renderPlotly({
    
    df <- filteredData()
    
    validate(
      need(
        nrow(df) > 0,
        "No data available for selected filters."
      )
    )
    
    monthly <- df %>%
      
      mutate(
        Month = format(
          `Order Date`,
          "%Y-%m"
        )
      ) %>%
      
      group_by(Month) %>%
      
      summarise(
        Sales = sum(
          Sales,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    
    p <- ggplot(
      monthly,
      aes(
        x = Month,
        y = Sales,
        group = 1,
        text = paste(
          "Month:",
          Month,
          "<br>Sales:",
          round(Sales, 2)
        )
      )
    ) +
      
      geom_line(
        linewidth = 1.1
      ) +
      
      geom_point(
        size = 2.5
      ) +
      
      theme_minimal(
        base_size = 12
      ) +
      
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank()
      )
    
    
    ggplotly(
      p,
      tooltip = "text"
    ) %>%
      
      layout(
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        hoverlabel = list(
          bgcolor = "#30384D",
          font = list(
            color = "white"
          )
        )
      )
  })
  
  
  # ==========================================================
  # CATEGORY
  # ==========================================================
  
  output$categoryChart <- renderPlotly({
    
    df <- filteredData()
    
    category <- df %>%
      
      group_by(Category) %>%
      
      summarise(
        Sales = sum(
          Sales,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    
    p <- ggplot(
      category,
      aes(
        x = reorder(
          Category,
          Sales
        ),
        y = Sales,
        text = paste(
          "Category:",
          Category,
          "<br>Sales:",
          round(Sales, 2)
        )
      )
    ) +
      
      geom_col(
        width = 0.65
      ) +
      
      coord_flip() +
      
      theme_minimal(
        base_size = 12
      ) +
      
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title = element_blank()
      )
    
    
    ggplotly(
      p,
      tooltip = "text"
    ) %>%
      
      layout(
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF"
      )
  })
  
  
  # ==========================================================
  # GEOGRAPHY
  # ==========================================================
  
  output$geographyChart <- renderPlotly({
    
    df <- filteredData()
    
    geography <- df %>%
      
      group_by(Geography) %>%
      
      summarise(
        Sales = sum(
          Sales,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      
      arrange(
        desc(Sales)
      )
    
    
    p <- ggplot(
      geography,
      aes(
        x = reorder(
          Geography,
          Sales
        ),
        y = Sales,
        text = paste(
          "Geography:",
          Geography,
          "<br>Sales:",
          round(Sales, 2)
        )
      )
    ) +
      
      geom_col(
        width = 0.65
      ) +
      
      coord_flip() +
      
      theme_minimal(
        base_size = 12
      ) +
      
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title = element_blank()
      )
    
    
    ggplotly(
      p,
      tooltip = "text"
    ) %>%
      
      layout(
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF"
      )
  })
  
  
  # ==========================================================
  # TOP PRODUCTS
  # ==========================================================
  
  output$productChart <- renderPlotly({
    
    df <- filteredData()
    
    products <- df %>%
      
      group_by(
        `Product Name`
      ) %>%
      
      summarise(
        Sales = sum(
          Sales,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      
      arrange(
        desc(Sales)
      ) %>%
      
      slice_head(
        n = 10
      )
    
    
    p <- ggplot(
      products,
      aes(
        x = reorder(
          `Product Name`,
          Sales
        ),
        y = Sales,
        text = paste(
          "Product:",
          `Product Name`,
          "<br>Sales:",
          round(Sales, 2)
        )
      )
    ) +
      
      geom_col(
        width = 0.65
      ) +
      
      coord_flip() +
      
      theme_minimal(
        base_size = 12
      ) +
      
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title = element_blank()
      )
    
    
    ggplotly(
      p,
      tooltip = "text"
    ) %>%
      
      layout(
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF"
      )
  })
  
  
  # ==========================================================
  # DATA TABLE
  # ==========================================================
  
  output$salesTable <- renderDT({
    
    datatable(
      filteredData(),
      
      filter = "top",
      
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      
      rownames = FALSE
    )
  })
  
  
  # ==========================================================
  # DOWNLOAD
  # ==========================================================
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "Sales_Performance_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      
      write.csv(
        filteredData(),
        file,
        
        row.names = FALSE
      )
    }
  )
}


# ============================================================
# RUN APP
# ============================================================

shinyApp(ui = ui,server = server)
