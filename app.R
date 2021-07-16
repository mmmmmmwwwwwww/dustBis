
library("emdbook")
library("shiny")
library("ggplot2")
library(plotly)

server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot

  output$textOutput2 <- renderText({ "This chart shows how area coverage evolves through time." })

  observeEvent(input$info, {
    showModal(modalDialog(
      title = HTML('<div style="font-szie:20px;font-weight:bold;text-align:center">Information on Inputs</div>'),
      HTML('<table class="table_sample" style="text-align:right"> <thead><tr><th style="text-align:right">Term</th><th style="text-align:right">Information</th></tr></thead><tr><td>Area Coverage Limit</td><td>The maximum deposition that you consider unacceptable. The calculator will predict how long it will take to reach this limit. Research suggests that humans can identify a dirty surface, in ideal conditions, at 5% area coverage. </td></tr><tr><td>Particle Concentration</td><td>The number of particles of the chosen size that are suspended in the air. Values for PM<sub>10</sub> and PM<sub>2.5</sub> can commonly be obtained from online data sources from many locations. </td></tr><tr><td>Turbulence Parameter</td><td>A measure of how fast air is moving in the space of interest. By choosing the options "Steady Air" to "Windy" this value is changed from 0.001 1/s to 60 1/s. These values are, unfortunately, very difficult to obtain experimentally and seldom reported in the literature. However, they are essential for the calculation. </td></tr></table>')
    ))
  })
  
  output$textOutput4 <- renderText({ 'This chart shows the deposition velocity as a function of particle size. Please notice that turbulence increases the deposition rate of fine particles. The box indicates the range of diameters represented by the selected particle type.' })

  output$textOutput <- renderText({

    A1 = 1.257;
    A2 = 0.400;
    A3 = 0.55;
    # Cunningham slip correction factor constants (Davies, 1945) */
    T = 300;
    p = 101300;
    kb = 1.3806e-23;
    dvisf = 1.98E-05;
    kvisf = 1.568E-05;
    rhof = 1;
    g = 9.31;

    dp <- as.numeric(input$type)
    K <- as.numeric(input$K)
    rho <- input$rho
    C <- input$C
    kpi <- input$kpi / 100;

    lambda = kb * T / (sqrt(2) * pi * (dp * 1e-6) ^ 2 * p) # Mean free path
    cs = 1 + ((2 * lambda) / (dp * 1e-6)) * (A1 + A2 * exp(-(A3 * (dp * 1e-6)) / lambda))
    D_brown = (cs * kb * T) / (2 * pi * (dp * 1e-6) * dvisf)

    Ar = (g * (dp * 1e-6) ^ 3 * rhof * (rho - rhof)) / (dvisf ^ 2) #Archimides number
    Re = (-3.809 + (3.809 ^ 2 + 1.832 * (Ar ^ 0.50)) ^ 0.50) ^ 2
    vs = (Re * dvisf) / (dp * 1e-6 * rhof)
    # Deposition Velocity
    vdep = vs / (1 - exp((-pi / 2) * (vs / sqrt(K * D_brown))))

    particle_mass = rho * ((4 / 3) * pi * (dp * 1e-6) ^ (3));
    particle_area = (1 / 2) * pi * (dp * 1e-6) ^ (2);
    kg_c = C * 1e-6;
    n_c = kg_c / particle_mass;
    A = particle_area * n_c * vdep * 3600 * 24 * 7;
    # in m2/week
    time = kpi / A;
    #in weeks
    paste("The area coverage limit will be achieved in ", format(round(time, 2), nsmall = 2), "weeks.")
  
  })

  output$distPlot2 <- renderPlotly({

    A1 = 1.257;
    A2 = 0.400;
    A3 = 0.55;
    # Cunningham slip correction factor constants (Davies, 1945) */
    T = 300;
    p = 101300;
    kb = 1.3806e-23;
    dvisf = 1.98E-05;
    kvisf = 1.568E-05;
    rhof = 1;
    g = 9.31;

    dp <- as.numeric(input$type)
    K <- as.numeric(input$K)
    rho <- input$rho
    C <- input$C
    kpi <- input$kpi / 100;

    lambda = kb * T / (sqrt(2) * pi * (dp * 1e-6) ^ 2 * p) # Mean free path
    cs = 1 + ((2 * lambda) / (dp * 1e-6)) * (A1 + A2 * exp(-(A3 * (dp * 1e-6)) / lambda))
    D_brown = (cs * kb * T) / (2 * pi * (dp * 1e-6) * dvisf)

    Ar = (g * (dp * 1e-6) ^ 3 * rhof * (rho - rhof)) / (dvisf ^ 2) #Archimides number
    Re = (-3.809 + (3.809 ^ 2 + 1.832 * (Ar ^ 0.50)) ^ 0.50) ^ 2
    vs = (Re * dvisf) / (dp * 1e-6 * rhof)
    # Deposition Velocity
    vdep = vs / (1 - exp((-pi / 2) * (vs / sqrt(K * D_brown))))

    particle_mass = rho * ((4 / 3) * pi * (dp * 1e-6) ^ (3));
    particle_area = (1 / 2) * pi * (dp * 1e-6) ^ (2);
    kg_c = C * 1e-6;
    n_c = kg_c / particle_mass;
    A = particle_area * n_c * vdep * 3600 * 24 * 7;
    # in m2/week
    time = kpi / A;
    #in weeks

    #vd1(i,j) = sqrt(D_brown+K_e(j));

    T = seq(0, 5, 0.1)

    AA = matrix(0, length(T), 1)
    
    for (j in 2:length(T)) {
      AA[j] = AA[j - 1] + A * (1 - AA[j - 1]) * 0.1;
    }

    #AA = A*T
    SET = rep(kpi, length(T))

    df <- data.frame(x1 = T,
                 y1 = AA * 100, y2 = SET * 100)

    ggplotly(ggplot(df, aes(x1)) +
    geom_line(aes(y = y1, colour = "Area Coverage", group = 1, text = paste(" Area Coverage:", "<br>", "Time:", x1, "<br>", "Area Coverage:", round(y1, digits = 1))), size = 1.5) +
    geom_line(aes(y = y2, colour = "Area Coverage Limit", group = 2, text = paste(" Area Coverage Limit:", "<br>", "Time:", x1, "<br>", "Area Coverage:", y2)), size = 1.5) +
    scale_colour_manual("", breaks = c("Area Coverage", "Area Coverage Limit"), values = c("hotpink1", "cyan1")) +
    xlab("Time (weeks)") +
    ylab("Area Coverage (%)") +
    coord_cartesian(ylim = c(0, 50)), tooltip = "text") %>% config(displaylogo = FALSE)

  })

  output$distPlot3 <- renderPlotly({

    A1 = 1.257;
    A2 = 0.400;
    A3 = 0.55;
    # Cunningham slip correction factor constants (Davies, 1945) */
    T = 300;
    p = 101300;
    kb = 1.3806e-23;
    dvisf = 1.98E-05;
    kvisf = 1.568E-05;
    rhof = 1;
    g = 9.31;

    K <- as.numeric(input$K)
    rho <- input$rho
    C <- input$C
    kpi <- input$kpi / 100;

    dp <- lseq(0.01, 100, length = 1000)

    lambda = kb * T / (sqrt(2) * pi * (dp * 1e-6) ^ 2 * p) # Mean free path
    cs = 1 + ((2 * lambda) / (dp * 1e-6)) * (A1 + A2 * exp(-(A3 * (dp * 1e-6)) / lambda))
    D_brown = (cs * kb * T) / (2 * pi * (dp * 1e-6) * dvisf)

    Ar = (g * (dp * 1e-6) ^ 3 * rhof * (rho - rhof)) / (dvisf ^ 2) #Archimides number
    Re = (-3.809 + (3.809 ^ 2 + 1.832 * (Ar ^ 0.50)) ^ 0.50) ^ 2
    vs = (Re * dvisf) / (dp * 1e-6 * rhof)
    # Deposition Velocity
    vdep = vs / (1 - exp((-pi / 2) * (vs / sqrt(K * D_brown))))
    vdep_walls = (2 / pi) * sqrt(D_brown * K)
    #vd1(i,j) = sqrt(D_brown+K_e(j));

    df <- data.frame(Diameter = dp,
                     Deposition_Velocity = vdep, Deposition_Velocity2 = vdep_walls)

    x1 = 0.01
    x2 = as.numeric(input$type) * 2
    y1 = 1e-8
    y2 = 1

    ggplotly(ggplot(data = df, aes(x = Diameter)) + 
             geom_line(aes(y = Deposition_Velocity, group = 1, colour = "Deposition on Horizontal Surfaces", text = paste(" Diameter:", round(Diameter, digits = 4), "<br>", "Deposition on Horizontal Surfaces:", round(Deposition_Velocity, digits = 8))), size = 1.5) +
             geom_line(aes(x = Diameter, y = Deposition_Velocity2, group = 2, colour = "Deposition on Walls", text = paste(" Diameter:", round(Diameter, digits = 4), "<br>", "Deposition on Walls:", round(Deposition_Velocity2, digits = 10))), size = 1.5) +
             scale_colour_manual("", breaks = c("Deposition on Horizontal Surfaces", "Deposition on Walls"), values = c("magenta1", "gold1")) +
             geom_rect(data = df, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "lightblue", alpha = 0.5) +
             scale_y_log10(limits = c(1e-8, 1)) +
             scale_x_log10(limits = c(0.01, 200)) +
             xlab("Diameter (\U00B5m)") +
             ylab("Deposition Velocity (m/s)"), tooltip = "text") %>% config(displaylogo = FALSE)

  })

}
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  tags$head(tags$style(HTML('#textOutput{background:#d9edf7;color:#3a87ad;border-color:#bce8f1;font-size:20px;padding: 8px 35px 8px 14px;margin-bottom: 18px;text-shadow: 0 1px 0 rgba(255, 255, 255, 0.5);-webkit-border-radius: 4px;-moz-border-radius: 4px;border-radius: 4px;text-align:left;}'))),
  tags$head(tags$style(HTML('#textOutput2{background:#d9edf7;color:#3a87ad;border-color:#bce8f1;font-size:22px;padding: 8px 35px 8px 14px;margin-bottom: 18px;text-shadow: 0 1px 0 rgba(255, 255, 255, 0.5);-webkit-border-radius: 4px;-moz-border-radius: 4px;border-radius: 4px;text-align:center;margin-top:62px}'))),
  tags$head(tags$style(HTML('#textOutput4{background:#d9edf7;color:#3a87ad;border-color:#bce8f1;font-size:18px;padding: 8px 35px 8px 14px;margin-bottom: 18px;text-shadow: 0 1px 0 rgba(255, 255, 255, 0.5);-webkit-border-radius: 4px;-moz-border-radius: 4px;border-radius: 4px;text-align:justify;margin-top:20px}'))),
  tags$head(tags$style(HTML('.nav {margin-left:2vw;margin-bottom:5vh;font-size:2.4vmin;font-color:#0d6efd}'))),
  tags$head(tags$style(HTML('.tabbable > .nav > li[class=active] > a {background-color: #0d6efd;color: #FFF;}'))),
  tags$head(tags$style(HTML('.btn{display: inline-block;font-weight: 400;text-align: center;white-space: nowrap;vertical-align: middle;color: #fff;background-color: #343a40;border-color;user-select: none;border: 1px solid transparent;transition: color .15s ease-in-out,background-color .15s ease-in-out,border-color .15s ease-in-out,box-shadow .15s ease-in-out;margin-bottom:10px}'))),
  tags$head(tags$style(HTML('.table_sample{font-size:16px;font-size:16px;border: 1px solid;border-collapse: collapse;width:100%!important;color:white;background-color:black;border-color:#32383e;}'))),
  tags$head(tags$style(HTML('.table_sample td,th{border-bottom: 1px solid #32383e!important;padding-right: 12px;padding-left: 12px;text-align:justify!important}'))),
  tags$head(tags$style(HTML('.link{color: #00ffff;}'))),
  tags$head(tags$style(HTML('.link:hover{color: #ffffff;text-decoration: none;}'))),
# App title ----
  titlePanel(HTML('<div style ="background-color: #4d4e4c;color: #ffffff;padding: 15px;text-align: center;margin-top: 15px;margin-bottom:10vh"><h1>Open Visible Deposition </h1><p style="font-size:20px;line-height: 1.5">A tool to experiment with the causes of dust deposition <br> Developed by <a href=" https://www.ucl.ac.uk/bartlett/heritage/people/dr-josep-grau-bove/" class="link" style="font-size: 20px;"> Josep Grau-Bove </a> and <a href=" https://www.linkedin.com/in/wu-meng-516297167/" class="link" style="font-size: 20px;"> Meng Wu </a></p></div>'), windowTitle = 'Open Visible Deposition'),

# App title ----

# Sidebar layout with input and output definitions ----
  sidebarLayout(

# Sidebar panel for inputs ----
    sidebarPanel(

      textOutput("textOutput"),
      textOutput("textOutput3"),

      HTML('<div class="form-group shiny-input-container">
	            <label class="control-label" id="type-label" for="type">Type of Particle</label>
	            <div>
	              <select id="type"><option value="0.05" selected>PM\U2080\U2024\U2081</option>
	              <option value="0.5">PM\U2081</option>
	              <option value="1">PM\U2082\U2024\U2085</option>
	             <option value="5">PM\U23E8</option></select>
	             <script type="application/json" data-for="type" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
	            </div>
	          </div>'),

      actionButton("info", "Info on Inputs"),

      sliderInput(inputId = "kpi",label = "Area Coverage Limit (5%)",min = 0,max = 100,value = 5),

      sliderInput(inputId = "C",label = div(HTML("Particle Concentration (\U00B5g/m<sup>3</sup>)")),min = 1,max = 200,value = 15),

      # Input: Slider for the number of bins ----
      selectInput('K', 'Turbulence Parameter', choices = list("Steady air" = 0.001, "Natural convection" = 0.1, "Just noticeable ventilation" = 1, "Fan, strong ventilation" = 10, "Windy" = 60)),

      sliderInput(inputId = "rho",label = div(HTML('Particle Density (kg/m<sup>3</sup>)')),min = 1000,max = 3000,value = 1500)
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      tabsetPanel(

        tabPanel("Evolution of Area Coverage", plotlyOutput("distPlot2"),textOutput("textOutput2")),

        tabPanel("Deposition by Particle Size", plotlyOutput("distPlot3"),textOutput("textOutput4"))

        # Output: Histogram ----

      )

    )

  ),
  HTML('<hr style="height:1px;border:0;background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));">'),
  HTML('<a href="https://www.ucl.ac.uk/bartlett/heritage/" class="logo" style="text-align:center;display:block;padding-bottom:30px;padding-top:10px"> <img src="https://pbs.twimg.com/profile_images/1207240033008259072/VARrHLtb_400x400.jpg" width="150" height="150" alt=""></a>')
)
shinyApp(ui = ui, server = server)
