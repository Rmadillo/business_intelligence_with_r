#############################################################
# Risk Explorer Shiny App
# Dwight Barry, PhD | Enterprise Analytics
# November 2015 - Version 0.9 - Proof of concept
# May 2016 - Version 1.2 - expert opinion only
# Changes from 1.0: added data input and mix/max adjustment
# Changes from 1.1: substituted function for mc2d rpert,
#                   removed min values from inputs
#############################################################


# To have RStudio recognize this as a Shiny app,
# create a (sub)directory for this app, and rename
# it as app.R


##### Global #####

# Load Packages
library(shiny)
library(ggplot2)
library(htmlTable)
library(mc2d)


#######################################################

##### User Interface #####

ui = shinyUI(fluidPage(
  # Title
  titlePanel("Expert Opinion Risk Explorer"),
  
  sidebarLayout(
    # Sidebar Panel
    
    sidebarPanel(
      # Allow user to input betaPERT parameters
      
      # Enter lowest possible value
      numericInput(
        "low",
        label = "Enter Lowest Possible Value:",
        value = 0,
        step = 0.1
      ),
      
      # Enter most likely value (mode/maximum density)
      numericInput(
        "mode",
        label = "Enter Most Likely Value (Mode):",
        value = 10,
        step = 0.1
      ),
      
      # Enter highest possible value
      numericInput(
        "high",
        label = "Enter Highest Possible Value:",
        value = 25,
        step = 0.1
      ),
      
      # Produce empirical histogram/density plot
      numericInput(
        "bins",
        label = "Bin Width (0 for density only):",
        min = 0,
        value = 0.1,
        step = 0.1
      ),
      
      # User inputs for min/max percent adjustment
      numericInput(
        "minperc",
        label = "Minimum adjustment (percent below minimum):",
        min = 0,
        max = 1,
        value = 0
      ),
      
      numericInput(
        "maxperc",
        label = "Maximum adjustment (percent above maximum):",
        min = 0,
        max = 1,
        value = 0
      ),
      
      # Show betaPERT values
      htmlOutput("maxdens"),
      
      br(),
      
      # User inputs for simulation
      numericInput(
        "reps",
        label = "Simulation Replications:",
        min = 1,
        value = 10000
      ),
      
      sliderInput(
        "gamma",
        label = HTML(
          "BetaPERT Distribution Uncertainty Parameter (&gamma;): <br>
          <small><i>4 is default PERT
          distribution</i></small>"
        ),
        min = 1,
        max = 10,
        value = 4,
        step = 1
        ),
      
      submitButton("Submit")
      
      ),
    
    # Main panel
    mainPanel(
      
      # Simulation histogram/density plot
      plotOutput(outputId = "dist_plot"),
      
      # Output of simulation cdf plot click
      
      HTML(
        "<br><b>Click on the CDF line for a given x-axis value, 
        then click <i>Submit</i> to obtain the probability estimate.</b><br>"
      ),
      
      br(),
      
      # Simulation cdf plot
      plotOutput(outputId = "cdf_plot", click = "plot_click"),
      
      # Click output results
      htmlOutput("info")
      
      )
    
  )
  
))


#######################################################

##### Server #####

server = shinyServer(function(input, output) {
  
  # Simulation distribution plot
  output$dist_plot = renderPlot({
    Value = c(input$low, input$high, input$mode)
    df = data.frame(Value)
    
    sim_df = data.frame(Value = rpert(
      input$reps,
      (min(df$Value) - min(df$Value) * input$minperc),
      input$mode,
      (max(df$Value) + max(df$Value) * input$maxperc),
      input$gamma))
    
    ggplot(sim_df, aes(Value)) +
      ggtitle("Simulation Distribution") +
      ylab("Density / Count") +
      geom_histogram(
        aes(y = ..density..),
        binwidth = input$bins,
        col = "blue",
        fill = "blue",
        alpha = 0.2,
        na.rm = T) +
      xlim((min(df$Value) - min(df$Value) * input$minperc),
           (max(df$Value) + max(df$Value) * input$maxperc)) +
      geom_density(
        col = "blue",
        fill = "blue",
        alpha = 0.2,
        na.rm = T) +
      theme(
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
  })
  
  # Simulation CDF plot
  output$cdf_plot = renderPlot({
    Value = c(input$low, input$high, input$mode)
    df = data.frame(Value)
    
    sim_df = data.frame(Value = rpert(
      input$reps,
      (min(df$Value) - min(df$Value) * input$minperc),
      input$mode,
      (max(df$Value) + max(df$Value) * input$maxperc),
      input$gamma
    ))
    
    ggplot(sim_df, aes(Value)) +
      ggtitle("Simulation CDF") +
      ylab("Probability") +
      xlim((min(df$Value) - min(df$Value) * input$minperc),
           (max(df$Value) + max(df$Value) * input$maxperc)) +
      stat_ecdf(lwd = 2, na.rm = T) +
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
    
  })
  
  # Plot-click result for simulation
  output$info = renderText({
    paste0(
      "<i>The probability of obtaining a Value less than
      or equal to ",
      txtRound(input$plot_click$x, 1),
      " is about <b>",
      txtRound(input$plot_click$y, 2),
      "</b>.</i>"
    )
    
  })
  
  # Details on betaPERT parameters
  output$maxdens = renderText({
    Value = c(input$low, input$high, input$mode)
    df = data.frame(Value)
    
    sim_df = data.frame(Value = rpert(
      input$reps,
      (min(df$Value) - min(df$Value) * input$minperc),
      input$mode,
      (max(df$Value) + max(df$Value) * input$maxperc),
      input$gamma
    ))
    
    
    paste0(
      "<b>BetaPERT Inputs:</b><br><i>Simulation mode: ",
      txtRound(input$mode, 1),
      ".<br>Simulation minimum: ",
      txtRound((min(df$Value) - min(df$Value) * input$minperc), 1),
      "<br>Simulation maximum: ",
      txtRound(max(df$Value) + max(df$Value) * input$maxperc, 1),
      "</i>."
    )
    
  })
  
})


#######################################################

##### App #####

# Return the Shiny app
shinyApp(ui = ui, server = server)

#######################################################
