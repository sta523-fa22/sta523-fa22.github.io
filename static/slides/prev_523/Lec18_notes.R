library(shiny)
library(dplyr)
library(ggplot2)


pal = c("#7fc97f", "#beaed4", "#dfc086")
pal_names = c("Green", "Purple", "Orange")

shinyApp(
  ui = fluidPage(
    title = "Beta-Binomial",
    titlePanel("Beta-Binomial Visualizer"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        h4("Data:"),
        sliderInput("x", "# of heads", min=0, max=100, value=10),
        sliderInput("n", "# of flips", min=0, max=100, value=20),
        h4("Prior:"),
        numericInput("alpha", "Prior # of head", min=0, value=5),
        numericInput("beta", "Prior # of tails", min=0, value=5),
        h4("Options:"),
        checkboxInput("options", "Show Options", value = FALSE),
        conditionalPanel(
          "input.options == true",
          checkboxInput("bw", "Use theme_bw", value = FALSE),
          checkboxInput("facet", "Use facets", value = FALSE),
          selectInput("prior", "Color for prior", choices = pal_names, selected = pal_names[1]),
          selectInput("likelihood", "Color for likelihood", choices = pal_names, selected = pal_names[2]),
          selectInput("posterior", "Color for posterior", choices = pal_names, selected = pal_names[3])
        )
        
      ),
      mainPanel = 
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Plot", plotOutput("plot")
            ),
            tabPanel(
              "Table", tableOutput("table")
            )
          )
        )
    )
  ),
  server = function(input, output, session) {
    
    observeEvent(
      input$n,
      {
        updateSliderInput(session, "x", max = input$n)
      }
    )
    
    observeEvent(
      input$prior,
      {
        choices = c(input$prior, input$likelihood, input$posterior)
        if (input$prior == input$likelihood) {
          updateSelectInput(session, "likelihood", selected = setdiff(pal_names, choices))
        }  
        if (input$prior == input$posterior) {
          updateSelectInput(session, "posterior", selected = setdiff(pal_names, choices))
        }   
      }
    )
    
    observeEvent(
      input$likelihood,
      {
        choices = c(input$likelihood, input$likelihood, input$posterior)
        if (input$likelihood == input$prior) {
          updateSelectInput(session, "prior", selected = setdiff(pal_names, choices))
        }  
        if (input$likelihood == input$posterior) {
          updateSelectInput(session, "posterior", selected = setdiff(pal_names, choices))
        }   
      }
    )
    
    observeEvent(
      input$posterior,
      {
        choices = c(input$prior, input$likelihood, input$posterior)
        if (input$posterior == input$prior) {
          updateSelectInput(session, "prior", selected = setdiff(pal_names, choices))
        }  
        if (input$posterior == input$likelihood) {
          updateSelectInput(session, "likelihood", selected = setdiff(pal_names, choices))
        }   
      }
    )
    
    
    output$plot = renderPlot({
      
      color_choices = c(
        which(pal_names == input$prior),
        which(pal_names == input$likelihood),
        which(pal_names == input$posterior)
      )
      
      color_pal = pal[color_choices]
      
      
      d = tibble(
        p = seq(0, 1, length.out = 1000)
      ) %>%
        mutate(
          prior = dbeta(p, input$alpha, input$beta),
          likelihood = dbinom(input$x, size = input$n, prob = p),
          posterior = dbeta(p, input$alpha + input$x, input$beta + input$n - input$x)
        ) %>%
        tidyr::gather(
          distribution,
          density,
          prior, likelihood, posterior
        ) %>%
        mutate(
          distribution = forcats::as_factor(distribution)
        )
      
      g = ggplot(d, aes(x=p, y=density, color=distribution)) +
        geom_line(size=2) + 
        scale_color_manual(values = color_pal)
      
      if (input$bw)
        g = g + theme_bw()
      
      if (input$facet) 
        g = g + facet_wrap(~distribution)
      
      g
    })
  }
)