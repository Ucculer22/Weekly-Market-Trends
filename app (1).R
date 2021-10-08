# Heading ---------------------------------------------------------------------------------------------------------------------------------------
# Author: Christian Schmidt
# File: Master Home Appliance
# Last Updated: 6/29/2021



# R packages, will be used to help load and create data tables to manipulate in the app ---------------------------------------------------------
library(tidyverse)
library(data.table)
library(lubridate)
library(shinydashboard)
library(shinyWidgets)
library(shiny)
library(plotly)
library(DT)



# Download the two Quant files for MDA + SDA ----------------------------------------------------------------------------------------------------

# Clear memory to ensure large files are loaded properly
gc()

## Increase memory limit to arbitrary large number to avoid errors when loading csv files

memory.limit(size=56000)

## Load both MDA AND SDA Quant files & Time map + World Map used in the database for Countries

DT_MDA <- fread("GfK-HomeAppliancesWeeklyWithTickers_MDA-20210921.csv") # This should be the latest MDA quant file
DT_SDA <- fread("GfK-HomeAppliancesWeeklyWithTickers_SDA-PersonalCare-20210921.csv") # This should be the latest SDA quant file
timemap <- fread("datamap.csv") #this is the data mapping file

# Remove footer from both files (last row of data)

DT_MDA <- DT_MDA[1:(nrow(DT_MDA)-1),] 
DT_SDA <- DT_SDA[1:(nrow(DT_SDA)-1),]



# Merge All three files into one file, creating the master data_table ---------------------------------------------------------------------------

# Convert to period format and merge and store latest period

DT_MDA[, Date := ymd(PERIOD)]
DT_SDA[, Date := ymd(PERIOD)]
timemap[, Date := dmy(PERIOD)]
maxC_MDA <- max(DT_MDA$Date)
maxC_SDA <- max(DT_SDA$Date)

# Check that dates are the same for internal verification
maxC_SDA == maxC_MDA
DTmaxC <- timemap[Date == maxC_MDA]

# merge timemap for both MDA and SDA quants
DT_MDA <- merge(DT_MDA, timemap[,.(Date, YEAR, MONTH, WEEK, QUARTER)], by="Date", all.x=TRUE, all.y=FALSE)
DT_SDA <- merge(DT_SDA, timemap[,.(Date, YEAR, MONTH, WEEK, QUARTER)], by="Date", all.x=TRUE, all.y=FALSE)

# limit size to recent data from this year and the last 4 years
DT_MDA = DT_MDA[YEAR %in% c(DTmaxC$YEAR,DTmaxC$YEAR-1, DTmaxC$YEAR-2, DTmaxC$YEAR-3, DTmaxC$YEAR-4,DTmaxC$YEAR-5)]
DT_SDA = DT_SDA[YEAR %in% c(DTmaxC$YEAR,DTmaxC$YEAR-1, DTmaxC$YEAR-2, DTmaxC$YEAR-3, DTmaxC$YEAR-4,DTmaxC$YEAR-5)]

# bind both tables together and arrange by data
DTnew <- rbind(DT_MDA,DT_SDA) %>% 
    arrange(., Date)

# Remove both oversize tables to streamline process
remove(DT_SDA,DT_MDA)



# Further Clean-up and column name change to add readability ------------------------------------------------------------------------------------

# Reduce columns to only include information relevant to vertical

DTnew <- DTnew[,.(Date, YEAR, QUARTER, MONTH, WEEK, PRODUCTGROUP, FORMFACTOR, COMPANY, 
                  BRANDGROUP, BRAND, REGION, COUNTRY, SALES_UNITS, UNSUBSIDIZED_VALUE_USD,
                  UNSUBSIDIZED_VALUE_EUR, UNSUBSIDIZED_VALUE_LOCAL)]

# Rename columns and clean up table for easier manipulation

DTnew <- DTnew %>% rename("Product_Segment" := PRODUCTGROUP,
                          "Product_Type" := FORMFACTOR,
                          "Company" := COMPANY,
                          "Brand_Group" := BRANDGROUP,
                          "Sub_Brand" := BRAND,
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
#list_brand_group <- unique(DTnew$Brand_Group) %>% sort()
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
    
    # To get y/y calculations for Sales Units
    Topline_Table <- Topline_Table %>%
        group_by(`Wk`,`Mon`,`Q`) %>%
        mutate("YoY_Units" = if_else(`Year` == (lag(`Year`) + 1), ((`Units`/lag(`Units`)) - 1)*100, NA_real_))%>%
        group_by(Year)
    
    # To get y/y Calculations Revenue_USD
    Topline_Table <- Topline_Table %>%
        group_by(`Wk`,`Mon`,`Q`) %>%
        mutate("YoY_Revenue_USD" = if_else(`Year` == (lag(`Year`) + 1), ((`Revenue_USD`/lag(`Revenue_USD`)) - 1)*100, NA_real_))%>%
        group_by(Year)
    
    # To get y/y Calculations Revenue_EUR
    Topline_Table <- Topline_Table %>%
        group_by(`Wk`,`Mon`,`Q`) %>%
        mutate("YoY_Revenue_EUR" = if_else(`Year` == (lag(`Year`) + 1), ((`Revenue_EUR`/lag(`Revenue_EUR`)) - 1)*100, NA_real_))%>%
        group_by(Year)
    
    
    # This is to return Year value to a factor for easier data manipulation and modeling.
    Topline_Table$Year <- as.factor(Topline_Table$Year)
    # Remove the last year to only include y/y data.
    Topline_Table = Topline_Table %>% filter(Year %in% c(DTmaxC$YEAR,DTmaxC$YEAR-1, DTmaxC$YEAR-2, DTmaxC$YEAR-3, DTmaxC$YEAR-4))
    
    # Clean up decimals for percentages and whole numbers
    Topline_Table = as_tibble(Topline_Table) %>% mutate_at(c(vars(`YoY_Units`,
                                                                  `YoY_Revenue_USD`,
                                                                  `YoY_Revenue_EUR`)), round, 2)
    
    Topline_Table = as_tibble(Topline_Table) %>% mutate_at(c(vars(`Units`,
                                                                  `Revenue_USD`,
                                                                  `Revenue_EUR`,
                                                                  `ASP_USD`,
                                                                  `ASP_EUR`)), round, 0)
}



# Bubble calculations function -----------------

Bubble_Chart_function <- function(product_segment, company, region, country){
    Bubble_Chart_Data <- DTnew %>% filter(Product_Segment %in% c(product_segment),
                                          `Company` %in% c(company),
                                          `Region` %in% c(region),
                                          `Country` %in% c(country),
                                          `Wk` %in% c(Weeks_filter),
                                          `Q` %in% DTmaxC$QUARTER)
    
    # dplyr::select will be used in the shinyApp to filter based on a new set of inputs.
    # I think y/y vs. Units might be a better idea for a bubble chart, but seems excessive unless I want to re-write the code above.
    Bubble_Chart_Data <- Bubble_Chart_Data[,.(Units = sum(Units), `Revenue_USD` = sum(`Revenue_USD`),`Revenue_EUR` = sum(`Revenue_EUR`),
                                              `ASP_USD` = (sum(`Revenue_USD`)/sum(Units)),
                                              `ASP_EUR` = (sum(`Revenue_EUR`)/sum(Units))),
                                           by=.(Year,Product_Type)]
    
    Bubble_Chart_Data_Share <- Bubble_Chart_Data[,.(Unit_Share = sum(Units),
                                                    `Revenue_USD_Share` = sum(`Revenue_USD`),
                                                    `Revenue_EUR_Share` = sum(`Revenue_EUR`)),
                                                 by=.(Year)]
    
    # Fix year issue for calculating y/y 
    Bubble_Chart_Data$Year <- as.numeric(as.character(Bubble_Chart_Data$Year))
    
    # To get y/y calculations for Sales Units
    Bubble_Chart_Data <- Bubble_Chart_Data %>%
        group_by(`Product_Type`) %>%
        mutate("YoY_Units" = if_else(`Year` == (lag(`Year`) + 1), ((`Units`/lag(`Units`)) - 1)*100, NA_real_))%>%
        group_by(Year)
    
    # To get y/y Calculations Revenue_USD
    Bubble_Chart_Data <- Bubble_Chart_Data %>%
        group_by(`Product_Type`) %>%
        mutate("YoY_Revenue_USD" = if_else(`Year` == (lag(`Year`) + 1), ((`Revenue_USD`/lag(`Revenue_USD`)) - 1)*100, NA_real_))%>%
        group_by(Year)
    
    # To get y/y Calculations Revenue_EUR
    Bubble_Chart_Data <- Bubble_Chart_Data %>%
        group_by(`Product_Type`) %>%
        mutate("YoY_Revenue_EUR" = if_else(`Year` == (lag(`Year`) + 1), ((`Revenue_EUR`/lag(`Revenue_EUR`)) - 1)*100, NA_real_))%>%
        group_by(Year)
    
    # Join after getting share numbers for the chart
    Bubble_Chart_Data = inner_join(Bubble_Chart_Data_Share,Bubble_Chart_Data, by=c("Year"))
    
    # Create new rows and get rid of sum totals, we will turn them into percentages
    Bubble_Chart_Data <- Bubble_Chart_Data %>% 
        mutate("Mix_Units" := `Units`/`Unit_Share`*100,
               "Mix_USD" := `Revenue_USD`/`Revenue_USD_Share`*100,
               "Mix_EUR" := `Revenue_EUR`/`Revenue_EUR_Share`*100) %>%
        dplyr::select(-`Unit_Share`,-`Revenue_USD_Share`,-`Revenue_EUR_Share`)
    
    # This is to return Year value to a factor for easier data manipulation and modeling.
    Bubble_Chart_Data$Product_Type <- as.factor(Bubble_Chart_Data$Product_Type)
    Bubble_Chart_Data$Year <- as.factor(Bubble_Chart_Data$Year)
    # Remove the last year to only include y/y data.
    Bubble_Chart_Data = Bubble_Chart_Data %>% filter(Year %in% c(DTmaxC$YEAR,DTmaxC$YEAR-1, DTmaxC$YEAR-2, DTmaxC$YEAR-3, DTmaxC$YEAR-4))
    
    # Clean up decimals for percentages and whole numbers
    Bubble_Chart_Data = as_tibble(Bubble_Chart_Data) %>% mutate_at(c(vars(`YoY_Units`,
                                                                          `YoY_Revenue_USD`,
                                                                          `YoY_Revenue_EUR`,
                                                                          `Mix_Units`,
                                                                          `Mix_USD`,
                                                                          `Mix_EUR`)), round, 2)
    Bubble_Chart_Data = as_tibble(Bubble_Chart_Data) %>% mutate_at(c(vars(`Units`,
                                                                          `Revenue_USD`,
                                                                          `Revenue_EUR`,
                                                                          `ASP_USD`,
                                                                          `ASP_EUR`)), round, 0)
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
    Share_Table = Share_Table[Year %in% c(DTmaxC$YEAR,DTmaxC$YEAR-1, DTmaxC$YEAR-2, DTmaxC$YEAR-3, DTmaxC$YEAR-4)]

    
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
    Share_Table = Share_Table[,c(1:11,17,18,19,12:16)]
    
}   

#------------------------------------ End of Data Processing -----------------------------------------------------------------------------------



#------------------------------------ UI --------------------------------------------------------------------------------------------------------

# choices for axis inputs

topline_choices = c("Units","Revenue_USD","Revenue_EUR",
                   "ASP_USD","ASP_EUR",
                   "YoY_Units","YoY_Revenue_USD","YoY_Revenue_EUR")

bubble_choices = c("Units","Revenue_USD","Revenue_EUR",
                   "ASP_USD","ASP_EUR",
                   "YoY_Units","YoY_Revenue_USD","YoY_Revenue_EUR",
                   "Mix_Units","Mix_USD","Mix_EUR")

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
                     
                     pickerInput(inputId = "topline_product_segments",
                                 label = "Product Segments",
                                 choices = c(list_product_segments),
                                 selected = c(list_product_segments),
                                 multiple = TRUE, width = '95%',
                                 # Style the options in the dropdown menu to be more clearer
                                 choicesOpt = list(style = rep_len("color:black;", length(list_product_segments))),
                                 # Options for the dropdown menu
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 3,
                                                         virtualScroll = TRUE)
                                 ),
                     
                     pickerInput(inputId = "topline_product_types", label = "Product Types",
                                 choices = c(list_product_type),
                                 selected = c(list_product_type),
                                 multiple = TRUE, width = '95%',
                                 choicesOpt = list(style = rep_len("color:black;", length(list_product_type))),
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 8,
                                                         virtualScroll = TRUE)
                                 ),
                     
                     pickerInput(inputId = "topline_companies", label = "Companies",
                                 choices = c(list_companies), 
                                 selected = c(list_companies),
                                 multiple = TRUE, width = '95%',
                                 choicesOpt = list(style = rep_len("color:black;", length(list_companies))),
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 8,
                                                         virtualScroll = TRUE)
                                 ),
                                 
                     pickerInput(inputId = "topline_regions", label = "Regions",
                                 choices = c(list_regions), 
                                 selected = c(list_regions),
                                 multiple = TRUE, width = '95%',
                                 choicesOpt = list(style = rep_len("color:black;", length(list_regions))),
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 8,
                                                         virtualScroll = TRUE)
                                 ),
                     
                     pickerInput(inputId = "topline_countries", label = "Countries",
                                 choices = c(list_country), 
                                 selected = c(list_country),
                                 multiple = TRUE, width = '95%',
                                 choicesOpt = list(style = rep_len("color:black;", length(list_country))),
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 8,
                                                         virtualScroll = TRUE)
                                 ),
                     
                     actionButton(inputId = "topline_button", label = "Update"),
                     
                     div(style="text-align:center",br(),'Click "Update" to apply changes to',br(),'Chart and Table',br())
                     
                     ),
            
            menuItem("Bubble Chart", tabName = "bubble_chart_tab", icon = icon("soap", lib = "font-awesome")),
            menuItem("Bubble Settings", tabName = "bubble_chart_settings", icon = icon("sliders", lib = "font-awesome"),
                     
                     pickerInput(inputId = "bubble_chart_product_segments", label = "Product Segments",
                                 choices = c(list_product_segments), 
                                 selected = c(list_product_segments),
                                 multiple = TRUE, width = '95%',
                                 choicesOpt = list(style = rep_len("color:black;", length(list_product_segments))),
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 3,
                                                         virtualScroll = TRUE)
                                 ),
                     
                     pickerInput(inputId = "bubble_chart_companies", label = "Companies",
                                 choices = c(list_companies), selected = c(list_companies),
                                 multiple = TRUE, width = '95%',
                                 choicesOpt = list(style = rep_len("color:black;", length(list_companies))),
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 8,
                                                         virtualScroll = TRUE)
                                 ),
                     
                     pickerInput(inputId = "bubble_chart_regions", label = "Regions",
                                 choices = c(list_regions), 
                                 selected = c(list_regions),
                                 multiple = TRUE, width = '95%',
                                 choicesOpt = list(style = rep_len("color:black;", length(list_regions))),
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 8,
                                                         virtualScroll = TRUE)
                                 ),
                     
                     pickerInput(inputId = "bubble_chart_countries", label = "Countries",
                                 choices = c(list_country), selected = c(list_country),
                                 multiple = TRUE, width = '95%',
                                 choicesOpt = list(style = rep_len("color:black;", length(list_country))),
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 8,
                                                         virtualScroll = TRUE)
                                 ),
                     
                     actionButton(inputId = "bubble_button", label = "Update"),
                     
                     div(style="text-align:center",br(),'Click "Update" to apply changes to',br(),'Chart and Table',br())
                     
                     ),
            menuItem("Share Chart", tabName = "share_chart_tab", icon = icon("chart-line", lib = "font-awesome")),
            menuItem("Share Chart Settings", tabName = "share_chart_settings", icon = icon("sliders", lib = "font-awesome"),
                     
                     pickerInput(inputId = "share_chart_companies", label = "Companies",
                                 choices = c(list_companies), selected = c(list_companies),
                                 multiple = TRUE, width = '95%',
                                 choicesOpt = list(style = rep_len("color:black;", length(list_companies))),
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 8,
                                                         virtualScroll = TRUE)
                     ),
                     
                     pickerInput(inputId = "share_chart_product_types", label = "Product Types",
                                 choices = c(list_product_type),
                                 selected = c(list_product_type),
                                 multiple = TRUE, width = '95%',
                                 choicesOpt = list(style = rep_len("color:black;", length(list_product_type))),
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 8,
                                                         virtualScroll = TRUE)
                     ),
                     
                     pickerInput(inputId = "share_chart_regions", label = "Regions",
                                 choices = c(list_regions), 
                                 selected = c(list_regions),
                                 multiple = TRUE, width = '95%',
                                 choicesOpt = list(style = rep_len("color:black;", length(list_regions))),
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 8,
                                                         virtualScroll = TRUE)
                     ),
                     
                     pickerInput(inputId = "share_chart_countries", label = "Countries",
                                 choices = c(list_country), selected = c(list_country),
                                 multiple = TRUE, width = '95%',
                                 choicesOpt = list(style = rep_len("color:black;", length(list_country))),
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 8,
                                                         virtualScroll = TRUE)
                     ),
                     
                     actionButton(inputId = "share_button", label = "Update"),
                     
                     div(style="text-align:center",br(),'Click "Update" to apply changes to',br(),'Chart and Table',br())
                     
                     ),
            
            menuItem("Share Company Chart", tabName = "share_company_chart_tab", icon = icon("chart-line", lib = "font-awesome")),
            menuItem("Share Company Chart Settings", tabName = "share_company_chart_settings", icon = icon("sliders", lib = "font-awesome"),
                     
                     pickerInput(inputId = "share_company_chart_companies", label = "Companies",
                                 choices = c(list_companies), selected = c(list_companies),
                                 multiple = TRUE, width = '95%',
                                 choicesOpt = list(style = rep_len("color:black;", length(list_companies))),
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 8,
                                                         virtualScroll = TRUE)
                     ),
                     
                     pickerInput(inputId = "share_company_chart_product_types", label = "Product Types",
                                 choices = c(list_product_type),
                                 selected = c(list_product_type),
                                 multiple = TRUE, width = '95%',
                                 choicesOpt = list(style = rep_len("color:black;", length(list_product_type))),
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 8,
                                                         virtualScroll = TRUE)
                     ),
                     
                     pickerInput(inputId = "share_company_chart_regions", label = "Regions",
                                 choices = c(list_regions), 
                                 selected = c(list_regions),
                                 multiple = TRUE, width = '95%',
                                 choicesOpt = list(style = rep_len("color:black;", length(list_regions))),
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 8,
                                                         virtualScroll = TRUE)
                     ),
                     
                     pickerInput(inputId = "share_company_chart_countries", label = "Countries",
                                 choices = c(list_country), selected = c(list_country),
                                 multiple = TRUE, width = '95%',
                                 choicesOpt = list(style = rep_len("color:black;", length(list_country))),
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         size = 8,
                                                         virtualScroll = TRUE)
                     ),
                     
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
            tabItem(tabName = "bubble_chart_tab",
                    fluidRow(box(width = 900,
                                 pickerInput(inputId = "y_axis_bubble", label = "y-axis",
                                             multiple = FALSE,
                                             choices = bubble_choices,
                                             selected = "Revenue_USD",
                                             options = pickerOptions(actionsBox = TRUE)
                                             ),
                                 pickerInput(inputId = "x_axis_bubble", label = "x-axis",
                                             multiple = FALSE,
                                             choices = bubble_choices,
                                             selected = "YoY_Revenue_USD",
                                             options = pickerOptions(actionsBox = TRUE)
                                             ),
                                 pickerInput(inputId = "bubble_axis_bubble", label = "Bubble",
                                             multiple = FALSE,
                                             choices = bubble_choices,
                                             selected = "ASP_USD",
                                             options = pickerOptions(actionsBox = TRUE)
                                             )
                                 )
                             ),
                    fluidRow(box(width = 900,
                                 plotlyOutput("bubble_chart")
                             )
                             ),
                    fluidRow(box(width = 900,
                                 dataTableOutput("bubble_dt_table")
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
    
    # Bubble
    observeEvent(input$bubble_chart_regions, {
        updateSelectInput(session, "bubble_chart_countries", 
                          choices = c(country_selector_filter(input$bubble_chart_regions)), 
                          selected = c(country_selector_filter(input$bubble_chart_regions)))
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
    
    # Bubble Chart Tab
    

    
    output$bubble_chart <- renderPlotly({
        
        input$bubble_button
            
        bubble_chart_df <- isolate(Bubble_Chart_function(input$bubble_chart_product_segments,
                                                 input$bubble_chart_companies,
                                                 input$bubble_chart_regions,
                                                 input$bubble_chart_countries))
        
        z = input$bubble_axis_bubble
        
        desired_maximum_marker_size <- 40
        your_list_of_size_values <- bubble_chart_df %>% dplyr::select(all_of(z))
        your_list_of_size_values <- as.data.frame(your_list_of_size_values)
        your_list_of_size_values <- your_list_of_size_values[,1]
        sizeref <- 2.0 * max(your_list_of_size_values) / (desired_maximum_marker_size**2)
        
        fig <- plot_ly(bubble_chart_df, 
                       x = ~get(input$x_axis_bubble),
                       y = ~get(input$y_axis_bubble), 
                       text = ~Product_Type, color = ~Year, type = "scatter", mode = 'markers',
                       marker = list(size = ~your_list_of_size_values, opacity = 0.5, 
                                     sizemode = 'area', sizeref=~sizeref))
        
        y <- list(title = '')
        x <- list(title = '')
        
        fig <- fig %>% layout(title = paste0(DTmaxC$QUARTER,' Comparison up to Wk: ',max(Weeks_filter)),
                              xaxis = x,
                              yaxis = y, 
                              showlegend = TRUE)
        
        fig
    })
    
    output$bubble_dt_table <- renderDataTable({
        
        input$bubble_button
        
        bubble_chart_df <- isolate(Bubble_Chart_function(input$bubble_chart_product_segments,
                                                 input$bubble_chart_companies,
                                                 input$bubble_chart_regions,
                                                 input$bubble_chart_countries))
        
        DT::datatable(bubble_chart_df,class = 'cell-border stripe',
                      options = list(initComplete = JS("function(settings, json) {",
                                                       "$('body').css({'font-family': 'Calibri'});","}")),
                      rownames = FALSE,
                      style = 'default',
                      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                                        'Table 2: Product Type Table for Bubble Chart')) %>%
            formatRound(c('Units'),digits = 0, interval = 3, mark = ',',dec.mark = getOption("OutDec")) %>%
            formatCurrency(c('Revenue_USD','ASP_USD'),currency = '$', interval = 3, mark = ",", 
                           digits = 0, dec.mark = getOption("OutDec"), before = TRUE) %>%
            formatCurrency(c('Revenue_EUR','ASP_EUR'),currency = '€', interval = 3, mark = ",", 
                           digits = 0, dec.mark = getOption("OutDec"), before = TRUE) %>%
            formatString(c('YoY_Units','YoY_Revenue_USD','YoY_Revenue_EUR',
                           'Mix_Units','Mix_USD','Mix_EUR'), prefix = "% ")
        
        
        
        
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
