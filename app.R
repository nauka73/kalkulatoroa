library(shiny)

# Загрузите модель логистической регрессии
logistic_model <- readRDS("logistic_model_acute_appendicitis.rds")

# Создайте интерфейс Shiny
ui <- fluidPage(
  titlePanel("Калькулятор прогнозирования развития острого аппендицита у детей"),
  sidebarLayout(
    sidebarPanel(
      selectInput("тошнота", "Тошнота", choices = c("нет" = 0, "да" = 1)),
      numericInput("лейкоциты", "Лейкоциты (10^9/л)", value = 0),
      numericInput("лимфоциты", "Лимфоциты (10^9/л)", value = 0),
      numericInput("возраст", "Возраст (лет)", value = 0),
      selectInput("узи.более.6.мм", "УЗИ более 6 мм", choices = c("нет" = 0, "да" = 1)),
      selectInput("рвота", "Рвота", choices = c("нет" = 0, "да" = 1)),
      actionButton("predict", "Прогнозировать")
    ),
    mainPanel(
      htmlOutput("result")
    )
  )
)

# Создайте сервер Shiny
server <- function(input, output) {
  observeEvent(input$predict, {
    # Получите значения из ввода
    nausea <- as.numeric(input$тошнота)
    wbc <- input$лейкоциты
    lymphocytes <- input$лимфоциты
    age <- input$возраст
    ultrasound <- as.numeric(input$узи.более.6.мм)
    vomiting <- as.numeric(input$рвота)
    
    # Создайте новый набор данных для прогноза
    new_data <- data.frame(
      тошнота = nausea,
      лейкоциты = wbc,
      лимфоциты = lymphocytes,
      возраст = age,
      узи.более.6.мм = ultrasound,
      рвота = vomiting
    )
    
    # Рассчитайте вероятность
    predicted_prob <- predict(logistic_model, newdata = new_data, type = "response")
    predicted_prob <- predicted_prob * 100
    
    # Определение риска и цвета
    if (predicted_prob < 41) {
      risk <- "Низкий риск"
      text_color <- "green"
      bg_color <- "lightgreen"
    } else if (predicted_prob < 75) {
      risk <- "Средний риск"
      text_color <- "orange"
      bg_color <- "yellow"
    } else {
      risk <- "Высокий риск"
      text_color <- "red"
      bg_color <- "lightcoral"
    }
    
    # Вывод результата с рамкой и подложкой
    output$result <- renderUI({
      HTML(paste(
        "<div style='border: 2px solid ", text_color, "; padding: 10px; display: inline-block; background-color:", bg_color, ";'>",
        "<span style='color:", text_color, ";'>", risk, "</span>",
        "</div>"
      ))
    })
  })
}

# Запустите приложение Shiny
shinyApp(ui = ui, server = server)

# Создайте сервер Shiny
server <- function(input, output) {
  observeEvent(input$predict, {
    # Получите значения из ввода
    nausea <- as.numeric(input$тошнота)
    wbc <- input$лейкоциты
    lymphocytes <- input$лимфоциты
    age <- input$возраст
    ultrasound <- as.numeric(input$узи.более.6.мм)
    vomiting <- as.numeric(input$рвота)
    
    # Создайте новый набор данных для прогноза
    new_data <- data.frame(
      тошнота = nausea,
      лейкоциты = wbc,
      лимфоциты = lymphocytes,
      возраст = age,
      узи.более.6.мм = ultrasound,
      рвота = vomiting
    )
    
    # Рассчитайте вероятность
    predicted_prob <- predict(logistic_model, newdata = new_data, type = "response")
    predicted_prob <- predicted_prob * 100
    
    # Определение риска и цвета
    if (predicted_prob < 41) {
      risk <- "Низкий риск"
      text_color <- "green"
      bg_color <- "lightgreen"
    } else if (predicted_prob < 75) {
      risk <- "Средний риск"
      text_color <- "orange"
      bg_color <- "yellow"
    } else {
      risk <- "Высокий риск"
      text_color <- "red"
      bg_color <- "lightcoral"
    }
    
    # Вывод результата с рамкой и подложкой
    output$result <- renderUI({
      HTML(paste(
        "<div style='border: 2px solid ", text_color, "; padding: 10px; display: inline-block; background-color:", bg_color, ";'>",
        "<span style='color:", text_color, ";'>", risk, "</span>",
        "</div>"
      ))
    })
  })
}

# Запустите приложение Shiny
shinyApp(ui = ui, server = server)
