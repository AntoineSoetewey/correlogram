#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(corrplot)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  h1("Correlogram"),
  h4(tags$a(href = "https://www.antoinesoetewey.com/", "Antoine Soetewey")),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "source",
        label = "File:",
        choices = c(
          "mtcars dataset" = "mtcars",
          "iris dataset" = "iris"
        )
      ),
      HTML(paste0("(", tags$a(href = "https://www.antoinesoetewey.com/contact/", "Contact me"), " if you need your own file to be processed.)")),
      hr(),
      conditionalPanel(
        condition = "input.source == 'own'",
        textAreaInput("text", "Enter text", rows = 7)
      ),
      # Wrap the file input in a conditional panel
      conditionalPanel(
        # The condition should be that the user selects
        # "file" from the radio buttons
        condition = "input.source == 'file'",
        fileInput("file", "Select a file")
      ),
      radioButtons(
        "method", "Correlation method:",
        c(
          "Pearson" = "pearson",
          "Spearman" = "spearman",
          "Kendall" = "kendall"
        )
      ),
      hr(),
      sliderInput("sig.level",
        "Significance level for the correlation test:",
        min = 0,
        max = 1,
        value = 0.05
      ),
      hr(),
      selectInput(
        "order", "Order of the variables:",
        c(
          "Original order" = "original",
          "Angular order of the eigenvectors" = "AOE",
          "First principal component order" = "FPC",
          "Hierarchical clustering order" = "hclust",
          "Alphabetical order" = "alphabet"
        )
      ),
      hr(),
      checkboxInput(
        "diag",
        "Display the correlation coefficients on the diagonal?",
        FALSE
      ),
      hr(),
      radioButtons(
        "type", "Type:",
        c(
          "Upper" = "upper",
          "Lower" = "lower",
          "Full" = "full"
        )
      ),
      hr(),
      sliderInput("tl.srt",
        "Rotation of the labels:",
        min = 0,
        max = 90,
        value = 45
      ),
      hr(),
      HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/correlogram/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/correlogram/blob/master/app.R">code</a>. Back to <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a>.</p>')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      br(),
      htmlOutput("text"),
      # br(),
      # br(),
      # tags$a(href="https://www.antoinesoetewey.com/", "Back to www.antoinesoetewey.com"),
      br(),
      br()
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data_source <- reactive({
    if (input$source == "mtcars") {
      data <- mtcars
    } else if (input$source == "iris") {
      data <- iris[, 1:4]
    } else if (input$source == "file") {
      data <- input_file()
    }
    return(data)
  })

  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })

  output$distPlot <- renderPlot({
    create_corrplot <- function(data,
                                method = "pearson",
                                sig.level = 0.05,
                                order = "original",
                                diag = FALSE,
                                type = "upper",
                                tl.srt = 90,
                                number.font = 1,
                                number.cex = 1,
                                mar = c(0, 0, 0, 0),
                                language = "french") {
      # data_incomplete <- data_source
      data <- data[complete.cases(data), ]
      mat <- cor(data, method = method)
      cor.mtest <- function(mat, method) {
        mat <- as.matrix(mat)
        n <- ncol(mat)
        p.mat <- matrix(NA, n, n)
        diag(p.mat) <- 0
        for (i in 1:(n - 1)) {
          for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], method = method)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
          }
        }
        colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
        p.mat
      }
      p.mat <- cor.mtest(data, method = method)
      col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
      corrplot(mat,
        method = "color",
        col = col(200),
        number.font = number.font,
        mar = mar,
        number.cex = number.cex,
        type = type,
        order = order,
        addCoef.col = "black", # Ajout du coefficient de corrélation
        tl.col = "black",
        tl.srt = tl.srt, # Rotation des etiquettes de textes
        # Combiner avec le niveau de significativité
        p.mat = p.mat,
        sig.level = sig.level,
        insig = "blank",
        # Cacher les coefficients de corrélation sur la diagonale
        diag = diag
      )
    }
    create_corrplot(
      data = data_source(),
      method = input$method,
      sig.level = input$sig.level,
      order = input$order,
      diag = input$diag,
      type = input$type,
      tl.srt = input$tl.srt
    )
  })
  output$text <- renderText({
    paste0("Missing values in the dataset are automatically removed. This correlogram is based on ", nrow(data_source()), " observations. Positive correlations are displayed in blue and negative correlations in red. The intensity of the color is proportional to the correlation coefficient. The color legend shows the correlation coefficients and the corresponding colors. As a reminder, a negative correlation implies that the two variables considered vary in opposite directions, that is, if one variable increases the other decreases and vice versa. A positive correlation implies that the two variables considered vary in the same direction, that is, if one variable increases the other increases and if one variable decreases the other decreases as well. Finally, the white boxes indicate that the correlation is not significantly different from 0 at the ", input$sig.level * 100, "% level (tested via a correlation test), which means that there is no ", strong("linear"), " relationship between the two variables considered.")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
