

library(shiny)
library(tidyverse)
library(bslib)

find_breaks_speed <- function(avg_quality, n_rolls, ismod6){
    temp <- (((avg_quality * 3) + 3) * n_rolls) + ismod6
    speed <- floor(temp)
    excess <- temp %% floor(speed)
    
    qualitysum <- (avg_quality*n_rolls)
    next_min <- floor((qualitysum * 3) + ((n_rolls+1) * 3) + ismod6)
    next_max <- floor(((qualitysum+1) * 3) + ((n_rolls+1) * 3) + ismod6)
    
    #lowest value to hit each speed value
    gap <- next_max - next_min
    slice_cutoffs <- data.frame()
    for(g in 0:gap){
        x <- next_max - g
        slice_q <- (((x) - 3*(n_rolls+1) - ismod6)/3) - qualitysum
        slice_q <- pmax(slice_q, 0)
        slice_cum_prob <- 1-slice_q
        tempdf <- data.frame(speed = x, speed_increase = x - speed,
                             min_slice_quality = slice_q,
                             cumulative_prob = slice_cum_prob,
                             prior_speed = speed,
                             prior_excess = excess)
        slice_cutoffs <- bind_rows(slice_cutoffs, tempdf)
    }
    
    slice_cutoffs<- slice_cutoffs %>% 
        mutate(exact_prob = (lag(min_slice_quality, 1,
                                 default = 1)-min_slice_quality)) %>% 
        select(speed, speed_increase, min_slice_quality, 
               exact_prob, cumulative_prob, 
               prior_speed, prior_excess)
    
    # print(c("speed" = speed, "excess_speed" = excess,
    #          "next_min" = next_min, "next_max" = next_max))
    return(slice_cutoffs)
}

ui <- function(request){
    
    fluidPage(
        theme = bs_theme(version = 5, bootswatch = "minty"),
        
        verticalLayout(
            h1("Mod slice probabilities"),
            p(strong("Based on current understanding of prior slice average quality")),
            p("Mods slices apply to a random stat with a random quality (0-1).",
              "However, for stats like speed that quality has to be converted to an integer",
              " and that leaves some excess quality that is retained until the next time that same stat gets rolled.",
              " Using the Average Slice Quality metric shown in the calibration screen, you can know which mods ",
              "have more excess quality, and therefore have better chances to hit a +6 speed roll"),
            # ,
            # br(),
            # tags$div(
            #     HTML(
            #         '<a href="https://www.patreon.com/bePatron?u=79802259" data-patreon-widget-type="become-patron-button">Become a Patron!</a><script async src="https://c6.patreon.com/becomePatronButton.bundle.js"></script>'
            #     ),
        # "Given site traffic I've upgraded to paid hosting. Consider supporting keeping my work ad-free and open to anyone:",
        # hr(),
        #flowLayout(
        h4(textOutput(outputId = "top_text1")),
        h4(textOutput(outputId = "top_text2"))
        #    )
        ,
        br(),
        
        sidebarLayout(
            sidebarPanel(
                width = 3,
                radioButtons(inputId = "sixdot",
                             label = "",
                             choices = c("6-dot", "5-dot")),
                numericInput(inputId = "n_rolls",
                             label = "Number of rolls already on speed",
                             value = 4,
                             min = 1,
                             max = 4,
                             step = 1),
                numericInput(inputId = "avg_quality",
                             label = "Avg Slice Quality on speed",
                             value = 0.55,
                             min = 0,
                             max = 1,
                             step = 0.01),
                p("Get URL for page with current settings:"),
                bookmarkButton(title = "Get URL for page with current settings"),
                hr(),
                h5("My other work:"),
                # strong(tags$div(
                #     HTML(
                #         '<a href="https://www.patreon.com/bePatron?u=79802259">My Patreon</a>'
                #     )
                # )),
                #br(),
                tags$div(
                    HTML(
                        '<a href="https://lysiasthefox.shinyapps.io/swgoh_base_stat_explorer">Base Stat Explorer</a>'
                    )
                ),
                #" - All character stats: g13-R9",
                #br(),
                tags$div(HTML('<a href="https://drive.google.com/file/d/1-uIB2D5iN30pDgQAucWCmI-XPRRIX0X7/view?usp=sharing">Fleet Reinforcement Priority (download)</a>')),
                tags$div(HTML('<a href="https://lysiasthefox.shinyapps.io/FleetReinforcementPriority/">Fleet Reinforcement Priority (Live)</a>')),
                
                
                
                
            ),
            mainPanel(
                width = 9,
                # h1("Mod slice probabilities"),
                # br(),
                # p(strong("Based on current theoretical understanding of prior slice average quality")),
                # 
                # hr(),
                # 
                plotOutput(outputId = "p1",
                           height = "auto")
            )
        ),
        
        hr(),
        p("Theory discovered & described by wookietown"),
        p("Additional investigation & validation by Brill, Taliana, and the Grand Arena Science discord"),
        p("Calculator created by LysiasTheFox"),
        verbatimTextOutput(outputId = "raw_df")
        
    )
    )
}

server <- function(input, output, session) {
    
    is6dot <- reactive({
        input$sixdot == "6-dot"
    })
    
    df_breaks <- reactive({
        find_breaks_speed(avg_quality = input$avg_quality, 
                          n_rolls = input$n_rolls, 
                          ismod6 = is6dot())
    })
    
    top_text1 <- reactive({
        paste0("Current speed = ", df_breaks() %>% .$prior_speed %>% .[1])
    })
    
    output$top_text1 <- renderText(top_text1())
    
    top_text2 <- reactive({
        paste0("Current \"excess quality\" = ", 
               df_breaks() %>% .$prior_excess %>% .[1] %>% round(.,2))
    })
    
    output$top_text2 <- renderText(top_text2())
    
    
    p1 <- reactive({
        df_breaks() %>% 
            ggplot(aes(x = speed, y = exact_prob)) +
            #geom_point() +
            geom_col(aes(fill = speed)) +
            scale_fill_gradient(low = "#d11614", high = "#2b7bc5") +
            #geom_line() +
            geom_text(aes(label = paste0(round(exact_prob,3)*100, "%")),
                      position = position_nudge(y=0.03),
                      size = 16/.pt) +
            geom_point(aes(y = cumulative_prob)) +
            geom_line(aes(y = cumulative_prob),
                      alpha = 0.4) +
            geom_text(data = . %>% filter(cumulative_prob > min(cumulative_prob)),
                      aes(y = cumulative_prob,
                          label = paste0(round(cumulative_prob,3)*100, "%")),
                      position = position_nudge(y=0.03),
                      size = 16/.pt) +
            labs(y = "Probability",
                 x = "Speed after slice",
                 title = "Probability for next speed slice",
                 subtitle = "Probability for X speed or higher (line)\nOr for X speed eactly (bar)") +
            theme_minimal() +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.text = element_text(size = 14),
                  axis.title = element_text(face = "bold", size = 18),
                  plot.title = element_text(face = "bold", size = 18),
                  plot.subtitle = element_text(size = 16),
                  legend.position = "none"
            )
    })
    
    # plot_height_fn <- reactive(
    #     input$plot_height
    # )
    
    
    # plot1 <- reactive({
    #     renderPlot(
    #     p1(),
    #     height = plot_height_fn()
    # )
    # })
    # 
    # output$p1 <- plot1()
    
    output$p1 <- 
        renderPlot({
            p1()
        }, height = function() {session$clientData$output_p1_width},
        res = 72)
    
    
    output$raw_df <- 
        renderPrint({
            df_breaks() %>% 
                select(speed, speed_increase, 
                       min_slice_quality, exact_prob, cumulative_prob) %>% 
                mutate(min_slice_quality = round(min_slice_quality,3),
                       exact_prob = round(exact_prob, 3),
                       cumulative_prob = round(cumulative_prob, 3)) %>% 
                arrange(speed) %>% 
                #column_to_rownames(var = speed) %>% 
                print()
        })
    
}


shinyApp(ui = ui, server = server,  enableBookmarking = "url")
