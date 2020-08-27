library(shiny)
library(DT)
data<-data.frame(firstname=c('Albert','Alan','Annabel','Torsten'),lastname=c('Anderson','Brown','Davies','Ford'))
options(DT.options = JS("'deferRender': true"))


ui <- fluidPage(dataTableOutput('search_table'))

server<-function(input, output, session){
  output$search_table<- renderDataTable(datatable(data))
}

shinyApp(ui = ui, server = server)