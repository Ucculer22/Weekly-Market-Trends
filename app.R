# Heading ---------------------------------------------------------------------------------------------------------------------------------------
# Author: Christian Schmidt
# File: Master Home Appliance
# Last Updated: 11-NOV-2019



# R packages, will be used to help load and create data tables to manipulate in the app ---------------------------------------------------------
library(tidyverse)
library(data.table)
library(lubridate)
library(shinydashboard)
library(shinyWidgets)
library(shiny)
library(plotly)
library(DT)



# Download Quants and Merge Data ----------------------------------------------------------------------------------------------------

# Clear memory to ensure large files are loaded properly
gc()
## Increase memory limit to arbitrary large number to avoid errors when loading csv files
memory.limit(size=56000)

## Load both MDA AND SDA Quant files & Time map + World Map used in the database for Countries

DT_MDA <- fread("GfK-HomeAppliancesWeeklyWithTickers_MDA-20211202.csv") # This should be the latest MDA quant file
DT_SDA <- fread("GfK-HomeAppliancesWeeklyWithTickers_SDA-20211202.csv") # This should be the latest SDA quant file
timemap <- fread("datamap.csv") #this is the data mapping file
beepr::beep()

# Remove footer from both files (last row of data)

DT_MDA <- DT_MDA[-nrow(DT_MDA),] 
DT_SDA <- DT_SDA[-nrow(DT_SDA),]


# Convert to period format and merge and store latest period

DT_MDA[, Date := ymd(PERIOD)]
DT_SDA[, Date := ymd(PERIOD)]
timemap[, Date := dmy(PERIOD)]


# merge timemap for both MDA and SDA quants

DT_MDA <- merge(DT_MDA, timemap[,.(Date, YEAR, MONTH, WEEK, QUARTER)], by="Date", all.x=TRUE, all.y=FALSE)
DT_SDA <- merge(DT_SDA, timemap[,.(Date, YEAR, MONTH, WEEK, QUARTER)], by="Date", all.x=TRUE, all.y=FALSE)

maxC_MDA <- max(DT_MDA$Date)
maxC_SDA <- max(DT_SDA$Date)

# Check that dates are the same for internal verification
maxC_SDA == maxC_MDA
DTmaxC <- timemap[Date == maxC_MDA]

# limit size to recent data from this year and the last 4 years
filter_years = c(DTmaxC$YEAR,DTmaxC$YEAR-1, DTmaxC$YEAR-2, DTmaxC$YEAR-3, DTmaxC$YEAR-4)

DT_MDA = DT_MDA[YEAR %in% c(filter_years)]
DT_SDA = DT_SDA[YEAR %in% c(filter_years)]

# bind both tables together and arrange by data
DTnew <- rbind(DT_MDA,DT_SDA) %>% 
    arrange(., Date)

# Remove both oversize tables to streamline process
# remove(DT_SDA,DT_MDA)

# Reduce columns to only include information relevant to vertical
# removed BRAND To reduce time of computation
DTnew <- DTnew[,.(Date, YEAR, QUARTER, MONTH, WEEK, PRODUCTGROUP, FORMFACTOR, COMPANY, 
                  BRANDGROUP, REGION, COUNTRY, SALES_UNITS, UNSUBSIDIZED_VALUE_USD,
                  UNSUBSIDIZED_VALUE_EUR, UNSUBSIDIZED_VALUE_LOCAL)]

# Rename columns and clean up table for easier manipulation

DTnew <- DTnew %>% rename("Product_Segment" := PRODUCTGROUP,
                          "Product_Type" := FORMFACTOR,
                          "Company" := COMPANY,
                          "Brand_Group" := BRANDGROUP,
                          #"Sub_Brand" := BRAND,
                          "Region" := REGION,
                          "Country" := COUNTRY,
                          "Year" := YEAR,
                          "Q" := QUARTER,
                          "Mon" := MONTH,
                          "Wk" := WEEK,
                          "Units" := SALES_UNITS,
                          "Revenue_USD" := UNSUBSIDIZED_VALUE_USD,
                          "Revenue_EUR" := UNSUBSIDIZED_VALUE_EUR,
                          "Revenue_Local" := UNSUBSIDIZED_VALUE_LOCAL)



# Create the all important unique value lists for each column so the app knows what the default choices are -------------------------------------

# Create lists of unique value
# This will be used in situations for later analysis/default settings for inputs

list_column_names <- colnames(DTnew) %>% sort()
list_product_segments <- unique(DTnew$Product_Segment) %>% sort()
list_product_type <- unique(DTnew$Product_Type) %>% sort()
list_regions <- unique(DTnew$Region) %>% sort()
list_country <- unique(DTnew$Country) %>% sort()
list_brand_group <- unique(DTnew$Brand_Group) %>% sort()
#list_sub_brand <- unique(DTnew$Sub_Brand) %>% sort()
list_companies <- unique(DTnew$Company) %>% sort()
list_year <- unique(DTnew$Year)
list_quarter <- unique(DTnew$Q)
list_month <- unique(DTnew$Mon)
list_week <- unique(DTnew$Wk)

Weeks_filter <- DTnew %>% filter(Year %in% DTmaxC) %>% dplyr::select(Wk)
Weeks_filter <- unique(Weeks_filter$Wk)


# ----------------------------------- Data Processing -------------------------------------------------------------------------------------------

# Helper functions to make the UI streamed-line

#function for updating selection of countries based on regions
country_selector_filter <- function(select_region) {
    Country_List = DTnew %>% 
        dplyr::select(Region, Country) %>% 
        distinct(Region, Country) %>% 
        arrange(Region, Country) %>%
        filter(Region %in% c(select_region))
    
    c(Country_List$Country)
}

# function for updating selection of product types based on product segments
    # Will only use for Topline Chart
product_type_selector_filter <- function(select_product_type) {
    Product_Type_List = DTnew %>% 
        dplyr::select(Product_Segment, Product_Type) %>% 
        distinct(Product_Segment, Product_Type) %>% 
        arrange(Product_Segment, Product_Type) %>%
        filter(Product_Segment %in% c(select_product_type))
    
    c(Product_Type_List$Product_Type)
}

# Attempt to streamline the y/y calculation
yoy_calculation <- function(var,year){
    output <- if_else(year == (lag(year) + 1), ((var/lag(var)) - 1)*100, NA_real_)
    return(output)
}

# y/y filters
filter_years_yoy = c(DTmaxC$YEAR,DTmaxC$YEAR-1, DTmaxC$YEAR-2, DTmaxC$YEAR-3)

# Clean up database function for charts

clean_up_database <- function(df){
    # This is to return Year value to a factor for easier data manipulation and modeling.
    df$Year <- as.factor(df$Year)
    # Remove the last year to only include y/y data.
    df = df %>% filter(Year %in% c(filter_years_yoy))
    
    # Clean up decimals for percentages and whole numbers
    df = as_tibble(df) %>% mutate_at(c(vars(`YoY_Units`,`YoY_Revenue_USD`,`YoY_Revenue_EUR`)), round, 2)
    
    df = as_tibble(df) %>% mutate_at(c(vars(`Units`,`Revenue_USD`,`Revenue_EUR`,`ASP_USD`,`ASP_EUR`)), round, 0)
}


# Topline calculations function ----------------

topline_calculations_YoY <- function(product_segment, product_type, company, region, country){
    Topline_Table <- DTnew %>% filter(`Product_Segment` %in% c(product_segment),
                                      `Product_Type` %in% c(product_type),
                                      `Company` %in% c(company),
                                      `Region` %in% c(region),
                                      `Country` %in% c(country)) 
    
    Topline_Table <- Topline_Table %>% dplyr::select(Date,Year,Q,Mon,Wk,Units,`Revenue_USD`,`Revenue_EUR`)
    
    Topline_Table <- Topline_Table[,.(Units = sum(Units), `Revenue_USD` = sum(`Revenue_USD`),`Revenue_EUR` = sum(`Revenue_EUR`),
                                      `ASP_USD` = (sum(`Revenue_USD`)/sum(Units)),
                                      `ASP_EUR` = (sum(`Revenue_EUR`)/sum(Units))), 
                                   by=.(Date,Year,Q,Mon,Wk)]
    
    # Fix year issue for calculating y/y 
    Topline_Table$Year <- as.numeric(as.character(Topline_Table$Year))
 
    Topline_Table <- Topline_Table %>%
        group_by(`Wk`,`Mon`,`Q`) %>%
        mutate("YoY_Units" = yoy_calculation(`Units`,`Year`),
               "YoY_Revenue_USD" = yoy_calculation(`Revenue_USD`,`Year`),
               "YoY_Revenue_EUR" = yoy_calculation(`Revenue_EUR`,`Year`))%>%
        group_by(Year)
    
    # clean-up
    clean_up_database(Topline_Table)

}

# Share calculations function ----------------------

share_calculations <- function(companies,countries,regions,products){
    # Create simplified data table for the share amount
    # Fixed share by filtering Table_Main, but then fixing by removing company as a filter option
    Share_Table <- DTnew %>% filter(`Country` %in% c(countries),`Product_Type` %in% c(products), 
                                    `Region` %in% c(regions))
    Share_Table <- Share_Table[,.(Units = sum(Units), Revenue_USD = sum(Revenue_USD),
                                  Revenue_EUR = sum(Revenue_EUR), `ASP EUR`=(sum(Revenue_EUR)/sum(Units)),
                                  `ASP USD`= (sum(Revenue_USD)/sum(Units))), by=.(Date,Year,Q,Mon,Wk)]
    
    
    Share_Table_Company <- DTnew %>% filter(`Company` %in% c(companies),`Country` %in% c(countries),
                                            `Product_Type` %in% c(products), `Region` %in% c(regions))
    Share_Table_Company = Share_Table_Company[,.(Units = sum(Units), Revenue_USD = sum(Revenue_USD), Revenue_EUR = sum(Revenue_EUR),
                                                 `ASP USD` = (sum(Revenue_USD)/sum(Units)), `ASP EUR` = (sum(Revenue_EUR)/sum(Units))), 
                                              by=.(Company,Date,Year,Q,Mon,Wk)]
    
    # Add company function for this section
    Share_Table = inner_join(Share_Table_Company,Share_Table, by=c("Date","Year","Q","Mon","Wk"))
    Share_Table = Share_Table[Year %in% c(filter_years_yoy)]

    
    Share_Table = Share_Table %>% rename("C_Units" := Units.x,
                                         "C_Value_USD" := Revenue_USD.x,
                                         "C_Value_EUR" := Revenue_EUR.x,
                                         "C_ASP_USD" = `ASP USD.x`,
                                         "C_ASP_EUR" = `ASP EUR.x`,
                                         "T_Units" := Units.y,
                                         "T_Value_USD" := Revenue_USD.y,
                                         "T_Value_EUR" := Revenue_EUR.y,
                                         "T_ASP_EUR" := `ASP EUR.y`,
                                         "T_ASP_USD" := `ASP USD.y`)
    Share_Table = Share_Table %>% mutate("C_Share_Units" := `C_Units`/`T_Units`*100,
                                         "C_Share_USD" := `C_Value_USD`/`T_Value_USD`*100,
                                         "C_Share_EUR" := `C_Value_EUR`/`T_Value_EUR`*100)
   
    Share_Table = as_tibble(Share_Table) %>% mutate_at(c(vars(`C_Units`,
                                                              `C_Value_USD`,
                                                              `C_Value_EUR`,
                                                              `C_ASP_USD`,
                                                              `C_ASP_EUR`,
                                                              `T_Units`,
                                                              `T_Value_USD`,
                                                              `T_Value_EUR`,
                                                              `T_ASP_USD`,
                                                              `T_ASP_EUR`)), round, 0)
    
    Share_Table = as_tibble(Share_Table) %>% mutate_at(c(vars(`C_Share_Units`,
                                                              `C_Share_USD`,
                                                              `C_Share_EUR`)), round, 2)
    
}   

pickerInput_function <- function(input_id,input_label,input_list){
    pickerInput(inputId = input_id, label = input_label,
                choices = c(input_list),
                selected = c(input_list),
                multiple = TRUE, width = '95%',
                choicesOpt = list(style = rep_len("color:black;", length(input_list))),
                options = pickerOptions(actionsBox = TRUE,
                                        liveSearch = TRUE,
                                        size = 8,
                                        virtualScroll = TRUE)
    )
}


#------------------------------------ End of Data Processing -----------------------------------------------------------------------------------




#------------------------------------ UI --------------------------------------------------------------------------------------------------------

# choices for axis inputs

topline_choices = c("Units","Revenue_USD","Revenue_EUR",
                   "ASP_USD","ASP_EUR",
                   "YoY_Units","YoY_Revenue_USD","YoY_Revenue_EUR")

share_choices = c("C_Share_Units","C_Share_USD","C_Share_EUR")


ui <- dashboardPage(
    skin = "yellow",
    title = "GfK",
    
    
    # Header --------------------------------------
    
    
    dashboardHeader(
        title = "Home Appliance", 
        titleWidth = 300,
        dropdownMenu(
            type = "notifications",
            headerText = tags$b("DISCLAIMER"),
            icon = icon("exclamation-circle",lib = "font-awesome"), 
            badgeStatus = "info",
            notificationItem(status = "danger",icon=icon("warning"),text = tags$div(
                style = "padding-right: 10px; padding-left: 10px; display: inline-block; vertical-align: middle;",
                tags$a(href="https://www.gfk.com/products/gfk-boutique-research",
                       "These reports are the property of GfK Boutique Research Inc."), 
                " and are protected by U.S. and international copyright law.,",tags$br(),tags$br(),
                " Licensees agree not to disclose, disseminate, or otherwise distribute this report",
                " in whole or in part without prior written agreement with GfK.",tags$br(),tags$br(),"Violation of these terms",
                " may result in cancellation of services and forfeiture of fees paid.",tags$br(),tags$br()
            )
            
            )
            
        ),
        
        tags$li(
            a(
                strong("ABOUT GfK"),
                height = 40,
                href = "https://www.gfk.com/en-us/products/gfk-boutique-research",
                title = "",
                target = "_blank"
            ),
            class = "dropdown"
        )
    ),
    
    
    # Sidebar --------------------------------------
    
    
    dashboardSidebar(width = 300,
        sidebarMenu(
            menuItem("Topline Chart", tabName = "topline_numbers_tab", icon = icon("globe-americas", lib = "font-awesome")),
            menuItem("Topline Settings", tabName = "topline_settings_tab", icon = icon("sliders", lib = "font-awesome"),
                     
                     
                     pickerInput_function("topline_product_segments","Product Segments",list_product_segments),
                     pickerInput_function("topline_product_types","Product Types",list_product_type),
                     pickerInput_function("topline_companies","Companies",list_companies),
                     pickerInput_function("topline_regions","Regions",list_regions),
                     pickerInput_function("topline_countries","Countries",list_country),
                     
                     actionButton(inputId = "topline_button", label = "Update"),
                     div(style="text-align:center",br(),'Click "Update" to apply changes to',br(),'Chart and Table',br())),

            menuItem("Share Chart", tabName = "share_chart_tab", icon = icon("chart-line", lib = "font-awesome")),
            menuItem("Share Chart Settings", tabName = "share_chart_settings", icon = icon("sliders", lib = "font-awesome"),
                     
                     pickerInput_function("share_chart_companies","Companies",list_companies),
                     pickerInput_function("share_chart_product_types","Product Types",list_product_type),
                     pickerInput_function("share_chart_regions","Regions",list_regions),
                     pickerInput_function("share_chart_countries","Countries",list_regions),
                   
                     actionButton(inputId = "share_button", label = "Update"),
                     
                     div(style="text-align:center",br(),'Click "Update" to apply changes to',br(),'Chart and Table',br())),
            
            menuItem("Share Company Chart", tabName = "share_company_chart_tab", icon = icon("chart-line", lib = "font-awesome")),
            menuItem("Share Company Chart Settings", tabName = "share_company_chart_settings", icon = icon("sliders", lib = "font-awesome"),
                     
                     pickerInput_function("share_company_chart_companies","Companies",list_companies),
                     pickerInput_function("share_company_chart_product_types","Product Types",list_product_type),
                     pickerInput_function("share_company_chart_regions","Regions",list_regions),
                     pickerInput_function("share_company_chart_countries","Countries",list_regions),
                     
                     actionButton(inputId = "share_company_button", label = "Update"),
                     
                     div(style="text-align:center",br(),'Click "Update" to apply changes to',br(),'Chart and Table',br())
                     )
            )
        ),
    
    
    # Body --------------------------------------
    
    # May not need to include column width at all
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "topline_numbers_tab",
                    fluidRow(box(width = 900,
                                 pickerInput(inputId = "y_axis_topline", label = "y-axis",
                                             multiple = FALSE,
                                             choices = topline_choices,
                                             selected = "Units",
                                             options = pickerOptions(actionsBox = TRUE)
                                             ),
                                 plotlyOutput("topline_chart", height = "900px")
                               )
                    ),
                    fluidRow(box(width = 900,
                                 dataTableOutput('topline_dt_table')
                                    )
                             )
            ),
            tabItem(tabName = "share_chart_tab",
                    fluidRow(box(width = 900,
                                 pickerInput(inputId = "y_axis_share", label = "y-axis",
                                             multiple = FALSE,
                                             choices = share_choices,
                                             selected = "C_Share_Units"
                                     
                                 ),
                                 plotlyOutput("share_chart", height = "900px")
                             )
                             ),
                    fluidRow(box(width = 900,
                                 dataTableOutput("share_dt_table")
                                 
                             )
                             )
                
            ),
            tabItem(tabName = "share_company_tab",
                    fluidRow(box(width = 900,
                                 pickerInput(inputId = "y_axis_company_share", label = "y-axis",
                                             multiple = FALSE,
                                             choices = share_choices,
                                             selected = "C_Share_Units"
                                             
                                 ),
                                 plotlyOutput("share_company_chart", height = "900px")
                             )
                    ),
                    fluidRow(box(width = 900,
                                 dataTableOutput("share_company_dt_table")
                                 )
                             )
                 
                 
                 
                    )
        
        
            )
        )
)



# ------------------------------------------ End of UI -----------------------------------------------------------------------------------------



# ------------------------------------------ Server --------------------------------------------------------------------------------------------

server <- function(input, output, session) {

    # -------------------------------------- ObserveEvents - pickerInput
    
    # Topline
    observeEvent(input$topline_regions, {
        updateSelectInput(session, "topline_countries", 
                          choices = c(country_selector_filter(input$topline_regions)), 
                          selected = c(country_selector_filter(input$topline_regions)))
    })
    
    observeEvent(input$topline_product_segments, {
        updateSelectInput(session, "topline_product_types", 
                          choices = c(product_type_selector_filter(input$topline_product_segments)), 
                          selected = c(product_type_selector_filter(input$topline_product_segments)))
    })
    
    # Share
    observeEvent(input$share_chart_regions, {
        updateSelectInput(session, "share_chart_countries", 
                          choices = c(country_selector_filter(input$share_chart_regions)), 
                          selected = c(country_selector_filter(input$share_chart_regions)))
    })
    

    # -------------------------------------- Render Charts and Tables

    # Topline Tab
    

    output$topline_chart <- renderPlotly({
        
        input$topline_button
        
        topline_df <- isolate(topline_calculations_YoY(input$topline_product_segments,
                                               input$topline_product_types,
                                               input$topline_companies,
                                               input$topline_regions,
                                               input$topline_countries))
        
        pal_year <- c("#ff6200","#ffd793","#9f6603","#6b6b6b","#adadad")
        pal_year <- setNames(pal_year, c(DTmaxC$YEAR,DTmaxC$YEAR-1, DTmaxC$YEAR-2, DTmaxC$YEAR-3, DTmaxC$YEAR-4))
        
        fig <- plot_ly(topline_df, x = ~Wk, y = ~get(input$y_axis_topline), type = 'scatter', mode= 'lines',
                color = ~Year, colors = pal_year, text = ~Year, 
                hovertemplate = paste0(
                    '<b>%{text}</b><br>',
                    '<b>Wk:</b>: %{x}<br>',
                    input$y_axis_topline,': %{y:,.0f}<br>'))
        
        y <- list(title = '')
        
        fig <- fig %>% layout(yaxis = y, showlegend = TRUE)
        
        fig

    })
    
    output$topline_dt_table <- renderDataTable({

        input$topline_button
        
        topline_df <- isolate(topline_calculations_YoY(input$topline_product_segments,
                                               input$topline_product_types,
                                               input$topline_companies,
                                               input$topline_regions,
                                               input$topline_countries))
        
        DT::datatable(topline_df,class = 'cell-border stripe',
                      options = list(initComplete = JS("function(settings, json) {",
                                                       "$('body').css({'font-family': 'Calibri'});","}")),
                      rownames = FALSE,
                      style = 'default',
                      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                                        'Table 1: Table for Topline Chart')) %>%
            formatRound(c('Units'),digits = 0, interval = 3, mark = ',',dec.mark = getOption("OutDec")) %>%
            formatCurrency(c('Revenue_USD','ASP_USD'),currency = '$', interval = 3, mark = ",", 
                           digits = 0, dec.mark = getOption("OutDec"), before = TRUE) %>%
            formatCurrency(c('Revenue_EUR','ASP_EUR'),currency = '€', interval = 3, mark = ",", 
                           digits = 0, dec.mark = getOption("OutDec"), before = TRUE) %>%
            formatString(c('YoY_Units','YoY_Revenue_USD','YoY_Revenue_EUR'), prefix = "% ")
            
    })
    
    
    # Share Chart Tab
    
    share_chart_df <- reactive(share_calculations(input$share_chart_companies,
                                                  input$share_chart_countries,
                                                  input$share_chart_regions,
                                                  input$share_chart_product_types))
    
    output$share_chart <- renderPlotly({
        
        input$share_button
        
        share_chart_df <- isolate(share_calculations(input$share_chart_companies,
                                                      input$share_chart_countries,
                                                      input$share_chart_regions,
                                                      input$share_chart_product_types))
        
        fig <- plot_ly(share_chart_df, x = ~Date, y = ~get(input$y_axis_share), 
                       type = 'scatter', mode = 'lines',
                       color = ~Company, text = ~Company, name = ~Company,
                       hovertemplate = paste0(
                           '<b>%{text}</b><br>',
                           '<b>Date:</b> %{x}<br>',
                           input$y_axis_share,': %{y:,.2f}<br>'))
        
        y <- list(title = '',tickprefix = "% ")
        
        fig <- fig %>% layout(yaxis = y, showlegend = TRUE)
        fig
        
    })
    
    output$share_dt_table <- renderDataTable({
        
        input$share_button
        
        share_chart_df <- isolate(share_calculations(input$share_chart_companies,
                                                      input$share_chart_countries,
                                                      input$share_chart_regions,
                                                      input$share_chart_product_types))
        
        DT::datatable(share_chart_df,class = 'cell-border stripe',
                      options = list(initComplete = JS("function(settings, json) {",
                                                       "$('body').css({'font-family': 'Calibri'});","}")),
                      rownames = FALSE,
                      style = 'default',
                      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                                        'Table 1: Table for Topline Chart')) %>%
            formatRound(c('C_Units',
                          'T_Units'),
                        digits = 0, interval = 3, mark = ',',dec.mark = getOption("OutDec")) %>%
            formatCurrency(c('C_Value_USD',
                             'C_ASP_USD',
                             'T_Value_USD',
                             'T_ASP_USD'),
                           currency = '$', interval = 3, mark = ",", 
                           digits = 0, dec.mark = getOption("OutDec"), before = TRUE) %>%
            formatCurrency(c('C_Value_EUR',
                             'C_ASP_EUR',
                             'T_Value_EUR',
                             'T_ASP_EUR'),
                           currency = '€', interval = 3, mark = ",", 
                           digits = 0, dec.mark = getOption("OutDec"), before = TRUE) %>%
            formatString(c('C_Share_Units',
                           'C_Share_USD',
                           'C_Share_EUR'), 
                         prefix = "% ")
        
    })
    
    
}    
# ------------------------------------------ End of Server -------------------------------------------------------------------------------------



# Run the ShinyApp

shinyApp(ui, server)
