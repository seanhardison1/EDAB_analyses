library(shinydashboard)
library(DT)
library(shiny)
library(dygraphs)
#setwd("c:/users/sean.hardison/documents/NEIEA_app")
source("NEIEA_source_functions.R")
ui <- fluidPage(
  
  dashboardPage(
    dashboardHeader(title = "NEFSC NEIEA Data"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Aggregated Commercial Data", tabName = "agg_comm_ts", icon = icon("usd",lib = "glyphicon")),
        menuItem("Species-level Commercial Data", tabName = "comm_ts", icon = icon("usd",lib = "glyphicon")),
        menuItem("Aggregated Survey Data", tabName = "agg_survey_ts", icon = icon("signal",lib = "glyphicon")),
        menuItem("Species-level Survey Data", tabName = "survey_ts", icon = icon("signal",lib = "glyphicon")),
        menuItem("SOE Indicator Data", tabName = "soe_ts", icon = icon("signal",lib = "glyphicon"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "survey_ts",
                fluidRow(column(2,selectInput('survey_fields',"Data Fields",choices = survey_fields)),
                         column(2,selectInput('EPU','EPU', choices = list("GOM","GB","MAB"), selected = "GOM")),
                         column(2,selectInput('season','Season', choices = list("Fall","Spring"), selected = "Fall")),
                         # column(12, align = "center",
                         #        h4(textOutput("var_name"))),
                         column(11,splitLayout(cellWidths = c("50%","50%"),
                                               dygraphOutput("survey_ts"),
                                               dygraphOutput("survey_prop")))
                         
                ),
                fluidRow(column(2,selectInput('survey_fields2',"Data Fields",choices = survey_fields)),
                         column(2,selectInput('EPU2','EPU', choices = list("GOM","GB","MAB"), selected = "GOM")),
                         column(2,selectInput('season2','Season', choices = list("Fall","Spring"), selected = "Fall")),
                         # column(12, align = "center",
                         #        h4(textOutput("var_name"))),
                         column(11,splitLayout(cellWidths = c("50%","50%"),
                                               dygraphOutput("survey_ts2"),
                                               dygraphOutput("survey_prop2")))
                         
                )
        ),
        tabItem(tabName = "agg_survey_ts",
                fluidRow(column(2,selectInput('agg_survey_fields',"Data Fields",choices = agg_survey_fields)),
                         column(2,selectInput('EPU3','EPU', choices = list("GOM","GB","MAB"), selected = "GOM")),
                         column(2,selectInput('season3','Season', choices = list("Fall","Spring"), selected = "Fall")),
                         # column(12, align = "center",
                         #        h4(textOutput("var_name"))),
                         column(11,splitLayout(cellWidths = c("50%", "50%"),
                                               dygraphOutput("agg_survey_ts"),
                                               dygraphOutput("agg_survey_prop")))
                ),
                fluidRow(column(2,selectInput('agg_survey_fields2',"Data Fields",choices = agg_survey_fields)),
                         column(2,selectInput('EPU4','EPU', choices = list("GOM","GB","MAB"), selected = "GOM")),
                         column(2,selectInput('season4','Season', choices = list("Fall","Spring"), selected = "Fall")),
                         # column(12, align = "center",
                         #        h4(textOutput("var_name"))),
                         column(11,splitLayout(cellWidths = c("50%", "50%"),
                                               dygraphOutput("agg_survey_ts2"),
                                               dygraphOutput("agg_survey_prop2")))
                )
        ),
        tabItem(tabName = "comm_ts",
                fluidRow(column(2,selectInput('comm_fields',"Data Fields",choices = survey_fields)),
                         column(2,selectInput('EPU5','EPU', choices = list("GOM","GB","MAB"), selected = "GOM")),
                         column(2,selectInput('comm_var','Variable', choices = list("USD","Metric tons (MT)"), selected = "Metric tons (MT)")),
                         column(11,
                                align = "center",
                                offset = 1,
                                dygraphOutput("comm_ts"))
                ),
                fluidRow(column(2,selectInput('comm_fields2',"Data Fields",choices = survey_fields)),
                         column(2,selectInput('EPU6','EPU', choices = list("GOM","GB","MAB"), selected = "GOM")),
                         column(2,selectInput('comm_var2','Variable', choices = list("USD","Metric tons (MT)"), selected = "Metric tons (MT)")),
                         column(11,
                                align = "center",
                                offset = 1,
                                dygraphOutput("comm_ts2"))
                )
        ),
        tabItem(tabName = "agg_comm_ts",
                fluidRow(column(2,selectInput('agg_comm_fields',"Data Fields",choices = agg_survey_fields)),
                         column(2,selectInput('EPU7','EPU', choices = list("GOM","GB","MAB"), selected = "GOM")),
                         column(2,selectInput('agg_comm_var','Variable', choices = list("USD","Metric tons (MT)"), selected = "Metric tons (MT)")),
                         column(11,splitLayout(cellWidths = c("50%", "50%"),
                                               dygraphOutput("agg_comm_ts"),
                                               dygraphOutput("agg_comm_prop")))
                ),
                fluidRow(column(2,selectInput('agg_comm_fields2',"Data Fields",choices = agg_survey_fields)),
                         column(2,selectInput('EPU8','EPU', choices = list("GOM","GB","MAB"), selected = "GOM")),
                         column(2,selectInput('agg_comm_var2','Variable', choices = list("USD","Metric tons (MT)"), selected = "Metric tons (MT)")),
                         column(11,splitLayout(cellWidths = c("50%", "50%"),
                                               dygraphOutput("agg_comm_ts2"),
                                               dygraphOutput("agg_comm_prop2")))
                )
        ),
        tabItem(tabName = "soe_ts",
                fluidRow(column(2, selectizeInput('soe_fields', "Type to search:", choices = soe_fields,
                                                  selected = NULL, multiple = FALSE, options = NULL)),
                         column(11,splitLayout(cellWidths = c("50%", "50%"),
                                               dygraphOutput("soe_ts"),
                                               dataTableOutput("soe_summary"))
                                )
                )
        )
      )
      
    )
  )
)

server <- function(input, output, session) {
  output$var_name <- renderText({
    input$survey_fields
  })
  
  #----------------------Individual species time series---------------------#
  #Abundance
  output$survey_ts <- renderDygraph({
    neiea_vis(dat = survey_biomass, 
              agg = F, var = input$survey_fields, surv_season = input$season,
              epu = input$EPU)
  })
  
  #Proportion of group biomass
  output$survey_prop <- renderDygraph({
    species_summary_plot(dat = out,
                         var = input$survey_fields,
                         surv_season = input$season,
                         epu = input$EPU)
  })
  
  # output$summary1 <- renderText({
  #   var <- input$survey_fields
  #   surv_season <- input$season
  #   epu <- input$EPU
  #   final_year <- max(na.omit(out[out$Prop.table.comname == var &
  #                                   out$Prop.table.season == surv_season &
  #                                   out$Prop.table.EPU == epu,]$Prop.table.YEAR))
  #   years <- seq(final_year - 20, final_year, 1)
  #   
  #   out <- out %>% filter(Prop.table.YEAR >= years[1])
  #   mean_prop <- mean(na.omit(out[out$Prop.table.comname == var &
  #                                   out$Prop.table.season == surv_season &
  #                                   out$Prop.table.EPU == epu,]$Prop.table.proportion))
  #   
  #   
  #   
  #   group <- unique(na.omit(out[out$Prop.table.comname == var &
  #                                 out$Prop.table.season == surv_season &
  #                                 out$Prop.table.EPU == epu,]$Prop.table.Group))
  #   
  #   paste0("In the ",paste(tolower(surv_season))," between ",years[1]," and ",final_year,",",var," averaged ",
  #          round(mean_prop,3)*100,"% of surveyed ",group, " biomass in ",epu)
  # })
  
  #Abundance 2
  output$survey_ts2 <- renderDygraph({
    neiea_vis(dat = survey_biomass, 
              agg = F, var = input$survey_fields2, surv_season = input$season2,
              epu = input$EPU2)
  })
  
  #Proportion of group biomass 2
  output$survey_prop2 <- renderDygraph({
    species_summary_plot(dat = out,
                         var = input$survey_fields2,
                         surv_season = input$season2,
                         epu = input$EPU2)
  })
  
  # output$summary2 <- renderText({
  #   var <- input$survey_fields2
  #   surv_season <- input$season2
  #   epu <- input$EPU2
  #   final_year <- max(na.omit(out[out$Prop.table.comname == var &
  #                                   out$Prop.table.season == surv_season &
  #                                   out$Prop.table.EPU == epu,]$Prop.table.YEAR))
  #   years <- seq(final_year - 20, final_year, 1)
  #   
  #   out <- out %>% filter(Prop.table.YEAR >= years[1])
  #   mean_prop <- mean(na.omit(out[out$Prop.table.comname == var &
  #                                   out$Prop.table.season == surv_season &
  #                                   out$Prop.table.EPU == epu,]$Prop.table.proportion))
  #   
  #   
  #   
  #   group <- unique(na.omit(out[out$Prop.table.comname == var &
  #                                 out$Prop.table.season == surv_season &
  #                                 out$Prop.table.EPU == epu,]$Prop.table.Group))
  #   
  #   paste0("In the ",paste(tolower(surv_season))," between ",years[1]," and ",final_year,",",var," averaged ",
  #          round(mean_prop,3)*100,"% of surveyed ",group, " biomass in ",epu)
  #   })
  
  #----------------------Aggregated survey biomass---------------------#
  #Abundance
  output$agg_survey_ts <- renderDygraph({
    neiea_vis(dat = survey_biomass, 
              agg = T, var = input$agg_survey_fields, surv_season = input$season3,
              epu = input$EPU3)
  })
  
  output$agg_survey_prop <- renderDygraph({
    ts_summary_plot(out, epu = input$EPU3, var = input$agg_survey_fields,
                    surv_season = input$season3)
  })
  
  #Abundance 2
  output$agg_survey_ts2 <- renderDygraph({
    neiea_vis(dat = survey_biomass, 
              agg = T, var = input$agg_survey_fields2, surv_season = input$season4,
              epu = input$EPU4)
  })
  
  output$agg_survey_prop2 <- renderDygraph({
    ts_summary_plot(out, epu = input$EPU4, var = input$agg_survey_fields2,
                    surv_season = input$season4)
  })
  #----------------------Commercial data by individual species---------------------#
  #1
  output$comm_ts <- renderDygraph({
    if (input$comm_var == "Metric tons (MT)"){
      neiea_vis(dat = neiea_comm_landings, 
                agg = F, var = input$comm_fields, comm = T,
                epu = input$EPU5)
      
    } else if (input$comm_var == "USD"){
      neiea_vis(dat = neiea_comm_landings, 
                agg = F, var = input$comm_fields, comm = T,
                USD = T,
                epu = input$EPU5)
    }
  })

  #2
  output$comm_ts2 <- renderDygraph({
    if (input$comm_var2 == "Metric tons (MT)"){
      neiea_vis(dat = neiea_comm_landings, 
                agg = F, var = input$comm_fields2, comm = T,
                epu = input$EPU6)
      
    } else if (input$comm_var2 == "USD"){
      neiea_vis(dat = neiea_comm_landings, 
                agg = F, var = input$comm_fields2, comm = T,
                USD = T,
                epu = input$EPU6)
    }
      })
  
  #----------------------Commercial data by aggregate group---------------------#
  #1
  output$agg_comm_ts <- renderDygraph({
    if (input$agg_comm_var == "Metric tons (MT)"){
      neiea_vis(dat = neiea_comm_landings, 
                agg = T, var = input$agg_comm_fields, comm = T,
                epu = input$EPU7)
      
    } else if (input$agg_comm_var == "USD"){
      neiea_vis(dat = neiea_comm_landings, 
                agg = T, var = input$agg_comm_fields, comm = T,
                USD = T,
                epu = input$EPU7)
    }
  })
  
  output$agg_comm_prop <- renderDygraph({
    if (input$agg_comm_var == "Metric tons (MT)"){
      comm_vis(comm_prop_data, var = input$agg_comm_fields, usd = F, epu = input$EPU7)
    } else if (input$agg_comm_var == "USD"){
      comm_vis(comm_prop_data, var = input$agg_comm_fields, usd = T, epu = input$EPU7)
    }
  })
  
  #2
  output$agg_comm_ts2 <- renderDygraph({
    if (input$agg_comm_var2 == "Metric tons (MT)"){
      neiea_vis(dat = neiea_comm_landings, 
                agg = T, var = input$agg_comm_fields2, comm = T,
                epu = input$EPU8)
      
    } else if (input$agg_comm_var2 == "USD"){
      neiea_vis(dat = neiea_comm_landings, 
                agg = T, var = input$agg_comm_fields2, comm = T,
                USD = T,
                epu = input$EPU8)
    }
  })
  
  output$agg_comm_prop2 <- renderDygraph({
    if (input$agg_comm_var2 == "Metric tons (MT)"){
      comm_vis(comm_prop_data, var = input$agg_comm_fields2, usd = F, epu = input$EPU8)
    } else if (input$agg_comm_var == "USD"){
      comm_vis(comm_prop_data, var = input$agg_comm_fields2, usd = T, epu = input$EPU8)
    }
  })
  
  output$soe_ts <- renderDygraph({
    soe_plot(dat = SOE.data.2018, var = input$soe_fields)
  })
  
  output$soe_summary <- renderDT(
    soe_plot(dat = SOE.data.2018, var = input$soe_fields, summ = T), options = list(autowidth = T)

    )


}

# Run the application 
shinyApp(ui = ui, server = server)


