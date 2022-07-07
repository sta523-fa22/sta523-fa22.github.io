library(shiny)
library(tidyverse)

## ABC - trunc norm

shinyApp(
  ui = fluidPage(
    titlePanel("Beta-Binomial Visualizer"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        h4("Data:"),
        sliderInput("x", "# of heads", min=0, max=100, value=10),
        sliderInput("n", "# of flips", min=0, max=100, value=20),
        h4("Prior:"),
        selectInput("prior", "Select prior", choices = c("Beta"="beta", "Trunc Norm"="tnorm")),
        uiOutput("prior_params"),
        h4("ABC:"),
        numericInput("nsim", "# of simulations", value = 10000),
        numericInput("nmin", "Min # of posterior draws", value = 1000)
      ),
      mainPanel = mainPanel(
        plotOutput("plot"),
        textOutput("summary")
      )
    )
  ),
  server = function(input, output, session) {
    
    observe({
      updateSliderInput(session, "x", max = input$n)
    }) %>%
      bindEvent(input$n)
    
    observe({
      if (input$prior == "beta") {
        output$prior_params = renderUI({
          list(
            numericInput("alpha", "Prior # of heads", min=0, value=5),
            numericInput("beta", "Prior # of tails", min=0, value=5)
          )
        })
      } else if (input$prior == "tnorm") {
        output$prior_params = renderUI({
          list(
            numericInput("mean", "Prior mean", min=0, max=1, value=0.5),
            numericInput("sd", "Prior sd", min=0, value=0.1)
          )
        })
      } else {
        output$prior_params = renderUI({})
      }
    })
    
    
    abc = reactive({
      print("abc")
      
      if (input$prior == "beta") {
        req(input$alpha, input$beta)
        prior = rbeta(input$nsim, input$alpha, input$beta)
      } else if (input$prior == "tnorm") {
        req(input$mean, input$sd)
        prior = truncnorm::rtruncnorm(n = input$nsim, a=0, b=1, mean = input$mean, sd = input$sd)
      } else {
        stop()
      }
      
      like_sim = rbinom(input$nsim, size = input$n, prob = prior)
      
      posterior = prior[ like_sim == input$x ]
      
      list(
        prior = prior,
        like_sim = like_sim,
        posterior = posterior
      )
    })
    
    
    abc_dens = reactive({
      d_prior = density(abc()$prior)
      d_post = density(abc()$posterior)
      
      bind_rows(
        tibble(
            distribution = "prior",
            p = d_prior$x,
            density = d_prior$y
        ),
        tibble(
          distribution = "likelihood",
          p = seq(0, 1, length.out = 1000)
        ) %>%
          mutate(
            density = dbinom(input$x, size = input$n, prob = p),
            density = density / sum(density / n())
          ),
        tibble(
          distribution = "posterior",
          p = d_post$x,
          density = d_post$y
        )
      )
    })
    
    
    output$summary = renderText({
      glue::glue(
        "Ran {input$nsim} generative simulations and obtained {length(abc()$posterior)} ",
        "posterior samples.\n Efficency of {100*length(abc()$posterior) / input$nsim}%."
      )
    })
    
    output$plot = renderPlot({
      abc_dens() %>%
        ggplot(aes(x=p, y=density, color=forcats::as_factor(distribution))) +
        geom_line(size=2) + 
        scale_color_manual(values = c("#7fc97f", "#beaed4", "#dfc086", "#e78ac3")) +
        labs(color = "Distribution") +
        xlim(0,1)
    })
  }
)

