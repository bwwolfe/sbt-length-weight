library(sf)
library(rnaturalearthhires)
library(nzsf)
library(shiny)

CCSBTs <- CCSBT[CCSBT$Area %in% 1:8, ] |> st_make_valid() |>
  st_union(by_feature = TRUE) |> st_shift_longitude()

the_map <- rnaturalearth::ne_countries(returnclass = "sf")


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
    tags$head(#includeCSS("www/CSS.css"),
      tags$style(HTML( "#FL {  height: 48px; font-size: 40px}
                        #quarter {font-size: 18px; }"
                        ))),
    titlePanel("Southern Bluefin Tuna fork length-to-weight calculator"),
    fluidRow(
     column(6,  
      plotOutput("plot", click = "plot_click")),
     column(6,
      plotOutput("curve", click = "curve_click"))
    ),
    
    fluidRow( column(1),
              column(3,
                     radioButtons(
                       inputId = "quarter",
                       "Quarter",
                       choiceNames = season.names,
                       choiceValues = 1:4)
    ),
    column(
        3,
        div(style = "font-size:18px;
            padding-top:10px",
            numericInput(
          inputId = "FL",
          label = "Fork Length (cm)",
          min = FLrange$min,
          max = FLrange$max,
          step = 5,
          value = 100, width = "150px"
        ))),
    column(4,
        div(style="
        padding-top:0px;
        font-size: 24px;
        border: 0px;",
        tableOutput("weightTable"),
        ))),
    fluidRow(HTML("<em>Length to Processed weight parameters agreed at 1994 SBT trilateral workshop on Age and Growth,
             Hobart, 17th Jan-4th Feb, 1994. From CSIRO database.</em>"))
  )

server <- function(input, output, session) {
  FL <- reactive({input$FL})
  observeEvent(input$plot_click,
               {
              
                xy.click <- st_within(st_point(unlist(input$plot_click[1:2])), CCSBTs)[[1]]
                if(isTRUE( xy.click %in% 1:8 )) region$Area <- xy.click
               }
               )
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
                 weight = c(paste(round(dressed.weight, digits), "kg"),
                          paste(round(whole.weight, digits), "kg"))
                 )
      
    }, #colnames = FALSE,
    align = "lr")
  
  
  output$curve <-
    renderPlot({
      quart <- input$quarter
      curve_data <-
        data.frame(
          FL = FLrange$min:FLrange$max,
          Weight = params()[[1]] * (FLrange$min:FLrange$max) ^ params()[[2]]
        )
      par(mar = c(8,4,4,2) + 0.1)
      plot(type = "l",
        curve_data$FL,
        curve_data$Weight,
        col = "red3",lwd = 1.5, 
        xlim = c(0, 210),
        ylim = c(0, 180),
        xlab = "Fork length (cm)",
        ylab = "Estimated weight (kg)",
        main = paste0("Region: ", region$Area, ",", "Quarter: ", quart),
        cex.main = 1.75,
        cex.axis = 1.3,
        cex.lab = 1.4
      )
      points(
        cex = 1.4, pch = 16,
        c = "black",
        x = FL(),
        y = params()[[1]] * FL() ^ params()[[2]]
      )
    })
  
  output$plot <- renderPlot({
    plot(NA, type = "n",
         xlim = c(80, 185),
         ylim = c(-45, -10),
         xaxt =  "n", yaxt = "n",
         xlab = "", ylab = "")
    rect(par("usr")[1], par("usr")[3],
                         par("usr")[2], par("usr")[4],
                         col = "lightskyblue1")
    par(new = TRUE)
    
    plot(
      the_map["geometry"],
      xlim = c(80, 185),
      ylim = c(-45, -10),
      col = "forestgreen",
      main = "Select region:",
      cex.main = 2
      
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
    text(area.labels, cex = 3)
    graphics::box(which = "plot", lwd = 2)
  })
  
}

runApp(shinyApp(ui, server))
