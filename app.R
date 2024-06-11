#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(hlt.falcon)

# data <- load_libraries_and_data()


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("FDA Table 03"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          fileInput("upload", "Upload a file"),
          selectInput("table_type", "Table Type", choices = c("22", "03")),
          downloadButton("download", "Download rtf")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("datatbl")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    data <- reactive({
      req(input$upload)
      data_list <- readRDS(input$upload$datapath)
      df <- data_list$df
      adsl <- data_list$adsl
      list(df = df, adsl = adsl)
    })

    table <- reactive({
      if (input$table_type == "22") {
        create_table_22(data()$df, data()$adsl)
      } else if (input$table_type == "03") {
        create_table_03(adsl = data()$adsl)
      }
    })
    
    output$datatbl <- renderTable({
      table()
    })

    output$download <- downloadHandler(
      filename = function() {
        if (input$table_type == "22") {
          "fda-table-22.rtf"
        } else if (input$table_type == "03") {
          "fda-table-03.rtf"
        }
      },
      content = function(file) {
        df_tbl <- if (input$table_type == "22") {
          table_22()
        } else if (input$table_type == "03") {
          table_03()
        }
        column_labels <- if (input$table_type == "22") {
          c("访视 \n治疗组", "异常无临床意义 \nN = 134", "异常有临床意义 \nN = 132", "正常 \nN = 134")
        } else if (input$table_type == "03") {
          c("访视 \n治疗组", "异常无临床意义 \nN = 134", "异常有临床意义 \nN = 132", "正常 \nN = 134")
        }
        report_path <- create_sassy_report(
          df_tbl = df_tbl,
          headers = "用药前后基线情况-HLT",
          titles = "表格14.3.4.2 用药前后体格检查参数临床评估的交叉表 — 安全性分析集",
          footnotes = c("数据来源：列表16.2.x", "注：百分比计算的分母基于安全性分析集的受试者人数。", "[1] 依从性 = 实际用药/计划用药量x100%。"),
          page_header = c("[Mock-up TLF shells (CN)", "Statistical Analysis Tables and Figures, List (Chinese Version)]", "[TP-HLT-BS-004,V1.0,15Mar2024]"),
          page_footer = c("Associated Process:", "关联流程：SOP-HLT-BS-001", "Confidentiality保密"),
          column_labels = column_labels,
          file = file
        )
      }
    )
}




# Run the application
shinyApp(ui = ui, server = server)
