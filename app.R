library(shiny)
library(readr)
library(dplyr)
library(lubridate)

perdiemrates.df <- read_csv("dod_perdiem_rates.csv", 
                              col_types = cols(`Effective Date` = col_skip(), 
                                               `Season Begin` = col_skip(),
                                               `Season End` = col_skip(), 
                                               seasonBegin = col_date(format = "%Y-%m-%d"), 
                                               seasonEnd = col_date(format = "%Y-%m-%d"),
                                               YYYYMM = col_date(format = "%Y-%m-%d")))

# make a season interval column
perdiemrates.df$seasonInterval <- c(rep(NA, dim(perdiemrates.df)[1]))

perdiemrates.df$seasonInterval <- interval(perdiemrates.df$seasonBegin,
                                             perdiemrates.df$seasonEnd)

perdiemrates.df$seasonInterval[
  which(perdiemrates.df$seasonBegin > perdiemrates.df$seasonEnd)] <-
  int_flip(perdiemrates.df$seasonInterval[
    which(perdiemrates.df$seasonBegin > perdiemrates.df$seasonEnd)])

lodging_getter <- function(lodging.date, location.date.row){
  lodging <- if_else(lodging.date %within% location.date.row$seasonInterval[1],
                     location.date.row$Lodging[1], location.date.row$Lodging[2])
  return(lodging)
}

mie_getter <- function(lodging.date, location.date.row){
  lodging <- if_else(lodging.date %within% location.date.row$seasonInterval[1],
                     location.date.row$mie[1], location.date.row$mie[2])
  return(lodging)
}

#perdiemrates.df$YYYMM <- ymd(paste(perdiemrates.df$YYYYMM, rep("01", dim(perdiemrates.df)[1])))

#perdiemrates.df$seasonInterval <- perdiemrates.df$seasonBegin %--% perdiemrates.df$seasonEnd

# ui and input. vars are: svmPresent, dependents, depsOver12, depsUnder12,
#                          cookingFacilities, checkInDate, checkOutDate
#####
ui <- pageWithSidebar(
               headerPanel("OCONUS TLA Calculator"),
               sidebarPanel(
                 tableOutput("entitlementsTable"),
                 checkboxInput(inputId = "svmPresent",
                             label = "Service Member Present",
                             value = TRUE),
                numericInput(inputId = "depsOver12",
                             label = "How many Dependents in TLA are 12 or older?",
                             value = 0,
                             min = 0),
                numericInput(inputId = "depsUnder12",
                             label = "How many Dependents in TLA are under 12 years old",
                             value = 0,
                             min = 0),
                checkboxInput(inputId = "cookingFacilities",
                              label = "Is there a kitchen with a cook stove, table/counter,
                              refrigerator, sink, water, table, chairs, and utensils?",
                              value = FALSE),
                dateRangeInput(inputId = "dates", 
                          label = "Check-In and Check-Out Dates",
                          start = Sys.Date(),
                          end = Sys.Date() + 1,
                          separator = "to"),
                numericInput(inputId = "actualLodging",
                             label = "Actual Lodging Cost (Including Taxes) Per Day",
                             value = 0.00,
                             min = 0.00),
                selectInput(inputId = "stateCountry",
                            label = "State / Country",
                            choices = perdiemrates.df$`State/Country`),
                uiOutput("citySelector"),
                a(href = "https://www.defensetravel.dod.mil/site/perdiemCalc.cfm",
                  "DoD Per Diem Rates Official Website"),
                p(""),
                a(href = "https://www.defensetravel.dod.mil/site/faqtla.cfm",
                  "TLA Frequently Asked Questions"),
                p(""),
                a(href = "https://www.defensetravel.dod.mil/Docs/perdiem/Computing_TLA.pdf",
                  "TLA Instruction (.pdf download)"),
                p(""),
                p("This tool is for reference only. Consult your PSD or CPPA for actual 
                   TLA reimbursement rates."),
                p("Effective 01 July 2020 - updated 29 June 2020"),
                p("Only rates within the past 12 months are included in this app."),
                p("Report errors to: brandon.strain@fe.navy.mil")
                ), # end sidebarpanel
    mainPanel = tableOutput('ratesTable')
    ) # end pagewithsidebar
                
#####


server <- function(input, output) {
  
output$citySelector <- renderUI({
  selectInput(inputId = "Location",
              label = "City",
              choices = perdiemrates.df$Location[which(
                perdiemrates.df$`State/Country` == input$stateCountry)])
  })
  
pct_svmOnly <- reactive({ #svm only
                 ifelse(input$svmPresent == TRUE &&
                          (input$depsOver12 + input$depsUnder12) == 0,
                        0.65, 0)
               }) # percentage with only svm
         
pct_depsOnly <- reactive({ #no svm: only dependent permutations
                 ifelse(
                   input$svmPresent == FALSE,
                   ifelse(
                     (input$depsOver12 + input$depsUnder12) == 1,
                     0.65,
                     ifelse(
                       (input$depsOver12 + input$depsUnder12) == 2,
                       1,
                       ifelse(
                         (input$depsOver12 == 1 &&
                            input$depsUnder12 > 1),
                         1 + (input$depsUnder12 - 1)*0.25,
                         ifelse(
                           input$depsOver12 >= 2,
                           1 + (input$depsUnder12 * 0.25) + (input$depsOver12 - 2) * 0.35,
                           0
                         )
                       )
                     )
                   ),
                 0)
               }) # percentage without svm
           
pct_svmDeps <- reactive({
                 ifelse(
                   input$svmPresent == TRUE,
                   ifelse(
                     (input$depsOver12 + input$depsUnder12) == 1,
                     1,
                     ifelse(
                       input$depsOver12 >= 1,
                       1 + 0.25*input$depsUnder12 + 0.35*(input$depsOver12 - 1),
                       ifelse(
                         input$depsOver12 == 0 && input$depsUnder12 >= 1,
                         1 + 0.25*(input$depsUnder12 - 1),
                         0
                       )
                     )
                   ),
                 0)
               }) # percentage with svm and dependents
               
pctApply <- reactive({
  pct_depsOnly() + pct_svmOnly() + pct_svmDeps()
  }) # final TLA percentage multiplier
               
date.sequence <- reactive({
  seq(input$dates[1], input$dates[2], by = "days")
})

location.set <- reactive({
  location.set <- subset(perdiemrates.df[which(
    perdiemrates.df$Location == input$Location),])
  location.set <- subset(location.set[which(
    location.set$YYYYMM %in% floor_date(date.sequence(), unit = "month")),])
  })

maxLodging.vector <- reactive({
  lodging.vector <- rep("Please Enter a Location", length(date.sequence()))
   
  for(i in 1:length(date.sequence()))
    
  lodging.vector[i] <- lodging_getter(date.sequence()[i], location.set()[which(
     location.set()$YYYYMM %in% floor_date(date.sequence()[i], unit = "month")),])
  
  lodging.vector
})

maxLodging <- reactive({ # this takes the maxMIE.vector and multiplies it by the relevant
  # applicable percentage according to number of dependents, cooking, etc
  lodging.vector.modified <- as.numeric(maxLodging.vector())
  for(i in 1:length(date.sequence()))
    lodging.vector.modified[i] <-
      ifelse(i == 1, # if it is the first day of TLA
             lodging.vector.modified[i] * # take the first day MIE value and multiply by...
               pctApply(), # percentage applicable
             ifelse(i == max(length(date.sequence())), # the last day of TLA
                    lodging.vector.modified[i] * # take the last day MIE value and mutiply by...
                      pctApply(), # percentage applicable
                    ifelse(i > 1, # any day not the first or last of TLA
                           lodging.vector.modified[i] * # take the day's MIE value and multiply by...
                             pctApply(),
                           "Error"
                    )))
  lodging.vector.modified
  
})

maxMIE.vector <- reactive({ # this calculates the vector of maximum MIE rates
  
  mie.vector <- rep("PLease Enter a Location", length(date.sequence()))
  
  for(i in 1:length(date.sequence()))
    
    mie.vector[i] <- mie_getter(date.sequence()[i], location.set()[which(
      location.set()$YYYYMM %in% floor_date(date.sequence()[i], unit = "month")),])
  
  mie.vector
})

maxMIE <- reactive({ # this takes the maxMIE.vector and multiplies it by the relevant
                     # applicable percentage according to number of dependents, cooking, etc
  mie.vector.modified <- as.numeric(maxMIE.vector())
  for(i in 1:length(date.sequence()))
    mie.vector.modified[i] <-
      ifelse(i == 1, # if it is the first day of TLA
             mie.vector.modified[i] * # take the first day MIE value and multiply by...
               pctApply(), # percentage applicable
             ifelse(i == max(length(date.sequence())), # the last day of TLA
                    mie.vector.modified[i] * # take the last day MIE value and mutiply by...
                      pctApply(), # percentage applicable
                    ifelse(i > 1, # any day not the first or last of TLA
                           mie.vector.modified[i] * # take the day's MIE value and multiply by...
                             ifelse(input$cookingFacilities == TRUE, 0.5, 1) *
                             pctApply(),
                           "Error"
                             )))
  
  mie.vector.modified
  
})

actualLodging <- reactive({
  lodging.vector <- rep(input$actualLodging, length(date.sequence()))
  
})

entitlements <- reactive({
  entitlements.vector <- rep("", length(date.sequence()))
  entitlements.vector[1] <- paste("$", format(round(sum(maxMIE()), 2), nsmall = 2))
  entitlements.vector[2] <- paste("$",
                                  ifelse( # this is the statement that pays sailors
                                          # less than what they have to pay for the
                                          # TLA hotel if the hotel charges
                                          # more than the entitlement
                                    sum(actualLodging()) > sum(maxLodging()),
                                        paste(
                                          format(round(sum(actualLodging()), 2), nsmall = 2),
                                        "Your actual lodging cost exceeds your entitlement. Contact your Admin Department or PSD about this issue."
                                        ),
                                    format(round(sum(actualLodging()), 2), nsmall = 2)
                                    ))
  entitlements.vector
})

output$entitlementsTable <- renderTable({
  matrix(ncol = 2,
         nrow = 1,
         dimnames = list(NULL,
                         c("Total M&IE Entitlement",
                           "Total Lodging Entitlement")),
         c( # this is a vector of the columns
           entitlements()[1],
           entitlements()[2]))
})

### everything above this line works ###

output$ratesTable <- renderTable({
  matrix(ncol = 4,
         nrow = length(date.sequence()),
         dimnames = list(NULL,
                         c("Date","Maximum M&IE","Maximum Lodging",
                           "Actual Lodging")),
         c( # this is a vector of the columns
           #column 1: dates
           as.character(seq(input$dates[1], input$dates[2], by = "days")),
           # column 2: maximum M&IE
           paste(rep("$", length(date.sequence)),
                 format(round(maxMIE(), digits = 2),
                        nsmall = 2)),
           # column 3: maximum lodging
           paste(rep("$", length(date.sequence)),
                 format(round(maxLodging(), digits = 2),
                        nsmall = 2)),
           # column 4: actual lodging
           paste(rep("$",length(date.sequence)),
                 format(
                   round(actualLodging(), digits = 2),
                   nsmall = 2))
         )
  )
  
})
               
               
}


shinyApp(ui = ui, server = server)
