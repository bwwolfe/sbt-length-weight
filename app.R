library(sf)
library(shiny)
library(ggplot2)
CCSBTs <- readRDS("data/CCSBT_areas.RDS")
the_map <- readRDS("data/the_map.RDS")

#area labels
area.labels <-
  apply(st_centroid(CCSBTs)["geometry"], 1,
        \(x)cbind(unlist(x))) |>
  t() |>
  as.data.frame() |> setNames(c("x", "y")) |> data.frame(Area = CCSBTs$Area)

#sf:::plot.sf(CCSBT[CCSBT$Area %in% 1:8,"Area"],col = cols, add = T)

lw.equations <-
  read.csv("data/CCSBT_LW_conversions.csv", skip = 4)

FLrange <- list(min = 0, max = 250)

season.names <-
  c("1 (Jan.-Mar.)",
    "2 (Apr.-Jun.)",
    "3 (Jul.-Sep.)",
    "4 (Oct.-Dec.)")

region <- reactiveValues(Area = 4)

ui <-
  fluidPage(
    tags$head(
      includeCSS("www/CSS.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "www/CSS.css"),
      tags$style(
        HTML("#weightTable {align-self:center;}
              #FL {  height: 58px; font-size: 34px}
              #quarter {font-size: 18px;}
              input {font-family: 'Courier New'; font-weight:bold;}"
        )
      )
    ),
    titlePanel("Southern Bluefin Tuna length to weight calculator"),
    fluidRow(
      column(
        3,align = "left",
        div(
          style = "white-space: nowrap;
                  font-size:26px;
               padding-top:20px;",
          numericInput(
            inputId = "FL",
            label = "Fork Length (cm)",
            min = FLrange$min,
            max = FLrange$max,
            step = 5,
            value = 130,
            width = "45%"
          )
        ),
        div(
          style = "font-size:24px;
            padding-top:5px;",
          radioButtons(
            inputId = "quarter",
            "Quarter",
            choiceNames = season.names,
            choiceValues = 1:4, inline = TRUE
          )
        ),
        span(style = "font-size:22px;padding:0px;", "Select region:"),
        div(style = "padding:0px;vertical-align:top", plotOutput("plot", click = "plot_click"))
      ),
      column(
        4, align = "center",
        div(style = "font-size:24px;color:#efb50d;padding-top:15px;padding-bottom:0px;padding-left:30px;",
            textOutput("curveHeader")),
        div(
          style = "
        padding:0px;
        font-size:24px;color:#274276;justify-content:center;
          border:0px;",
          tableOutput("weightTable"),
        ),
        div(plotOutput("curve", click = "curve_click"))
      )),
      fluidRow(HTML(
          "<em>Length to Processed weight parameters agreed at 1994 SBT trilateral workshop on Age and Growth,
             Hobart,<br>17th Jan-4th Feb, 1994. From CSIRO database.</em>"
        
      )
    )
  )


server <- function(input, output, session) {
  FL <- reactive({
    input$FL
  })
  observeEvent(input$plot_click,
               {
                 xy.click <-
                   st_within(st_point(unlist(input$plot_click[1:2])), CCSBTs)[[1]]
                 if (isTRUE(xy.click %in% 1:8))
                   region$Area <- xy.click
               })
  params <- reactive({
    region$Area
    param.names <-
      c(ifelse(input$FL >= 130,
               "A_ADULT", "A_JUV"),
        ifelse(FL() >= 130,
               "B_ADULT", "B_JUV"))
    lw.equations[lw.equations$STATISTICAL_AREA == region$Area &
                   lw.equations$QUARTER == input$quarter, param.names]
  })
  
  output$weightTable <-
    renderTable({
      params()[[1]] * FL() ^ params()[[2]]
      dressed.weight <- params()[[1]] * FL() ^ params()[[2]]
      digits <- ifelse(FL() < 25, 2, 1)
      paste("Dressed weight:", round(dressed.weight, digits), "kg")
      whole.weight <- dressed.weight * 1.15
      paste("Whole weight:", round(whole.weight, digits), "kg")
      
      data.frame(Estimated = c("Dressed:", "Whole:"),
                 weight = c(paste(round(
                   dressed.weight, digits
                 ), "kg"),
                 paste(round(
                   whole.weight, digits
                 ), "kg")))
      
    }, colnames = FALSE,
    align = "lr")
  
  output$curveHeader <-
    renderText({
      paste0("Region ", region$Area, ", ", "Q", input$quarter)
    })
  output$curve <-
    renderPlot({
      quart <- input$quarter
      curve_data <-
        data.frame(
          FL = FLrange$min:FLrange$max,
          Weight = params()[[1]] * (FLrange$min:FLrange$max) ^ params()[[2]]
        )
      par(pty = "s", mai = c(1,1,0.2,1))
      plot(
        type = "l",
        curve_data$FL,
        curve_data$Weight,
        col = "red2",
        lwd = 2,
        xlim = c(0, 225),
        ylim = c(0, 180),
        xlab = "Fork length (cm)",
        ylab = "Estimated weight (kg)",
        cex.main = 1.3,
        cex.axis = 1.3,
        cex.lab = 1.4,
        yaxs = "i",
        xaxs = "i", family = "mono"
      )
      box(which = "plot", lwd = 2.5, col = "#1b2d50")
      lines(
        x = c(FL(), FL()),
        y = c(0, params()[[1]] * FL() ^ params()[[2]]),
        lty = 2,
        lwd = 0.5
      )
      lines(
        x = c(0, FL()),
        y = c(params()[[1]] * FL() ^ params()[[2]], params()[[1]] * FL() ^ params()[[2]]),
        lty = 2,
        lwd = 0.5
      )
      points(
        cex = 1.5,
        pch = 16,
        col = "#1b2d50",
        x = FL(),
        y = params()[[1]] * FL() ^ params()[[2]]
      )
    })
  
  output$plot <- renderPlot({
    par(mar = c(10,0.5,0.25,0.5))
    plot(
      NA,
      type = "n",
      xlim = c(80, 185),
      ylim = c(-45,-10),
      xaxt =  "n",
      yaxt = "n",
      xlab = "",
      ylab = ""
    )
    rect(par("usr")[1],
         par("usr")[3],
         par("usr")[2],
         par("usr")[4],
         col = "lightskyblue1")
    par(new = TRUE)
    
    plot(
      the_map["geometry"],
      xlim = c(80, 185),
      ylim = c(-45,-10),
      col = "forestgreen",
      
    )
    cols <- rep(alpha("gold2", 0.3), 8)
    cols[region$Area] <- "#2178f0bb"
    
    sf:::plot.sf(
      CCSBTs["geometry"],
      lwd = 1.5,
      lty = 3,
      add = T,
      col = cols
    )
    text(area.labels, cex = 3.2, vfont = c("sans serif", "bold"))
    graphics::box(which = "plot", lwd = 2)
  })
}

shinyApp(ui, server)
