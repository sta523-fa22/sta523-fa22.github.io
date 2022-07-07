library(shiny)
library(tidyverse)

## ABC - proof of concept

shinyApp(
  ui = fluidPage(
    titlePanel("Beta-Binomial Visualizer"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        h4("Data:"),
        sliderInput("x", "# of heads", min=0, max=100, value=10),
        sliderInput("n", "# of flips", min=0, max=100, value=20),
        h4("Prior:"),
        numericInput("alpha", "Prior # of heads", min=0, value=5),
        numericInput("beta", "Prior # of tails", min=0, value=5),
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

    d = reactive({
      tibble(
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
        group_by(distribution) %>%
        mutate(
          density = density / sum(density / n())
        )
    })
    
    abc_post = reactive({
      req(input$nsim, input$nmin)
      
      prior = rbeta(input$nsim, input$alpha, input$beta)
      
      gen_proc_sims = rbinom(input$nsim, size = input$n, prob = prior)
      
      posterior = prior[ gen_proc_sims == input$x ]
      
      validate(
        need(length(posterior) > input$nmin, "Insufficient posterior draws, try increasing # of sims.")
      )
      
      posterior
    })
    
    
    output$summary = renderText({
      glue::glue(
        "Ran {input$nsim} generative simulations and obtained {length(abc_post())} ",
        "posterior samples.\n Efficency of {100*length(abc_post()) / input$nsim}%."
      )
    })
    
    output$plot = renderPlot({
      abc_post_dens = density(abc_post())
      
      
      d() %>%
        bind_rows(
          tibble(
            distribution = "posterior (ABC)",
            p = abc_post_dens$x,
            density = abc_post_dens$y
          )
        ) %>%
        ggplot(aes(x=p, y=density, color=forcats::as_factor(distribution))) +
        geom_line(size=2) + 
        scale_color_manual(values = c("#7fc97f", "#beaed4", "#dfc086", "#e78ac3")) +
        labs(color = "Distribution")
    })
  }
)

