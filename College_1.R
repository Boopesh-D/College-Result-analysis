library(reshape2)
library(RMySQL)
library(sqldf)
library(ggplot2)
library(plotly)
mydb = dbConnect(MySQL(), user='root', password='', dbname='college', host='localhost')

dbListFields(mydb, 'apr_18')
aa = dbSendQuery(mydb, " SELECT * from apr_18 ")
apr18 = fetch(aa, n=-1)
head(apr18)


dbListFields(mydb, 'apr_17')
ab = dbSendQuery(mydb, " SELECT * from apr_17 ")
apr17 = fetch(ab, n=-1)
head(apr17)

dbListFields(mydb, 'apr_16')
ab = dbSendQuery(mydb, " SELECT * from apr_16 ")
apr16 = fetch(ab, n=-1)
head(apr16)


dbListFields(mydb, 'apr_17')
ab = dbSendQuery(mydb, " SELECT * from apr_15 ")
apr15 = fetch(ab, n=-1)
head(apr15)

dbListFields(mydb, 'apr_14')
ab = dbSendQuery(mydb, " SELECT * from apr_14 ")
apr14 = fetch(ab, n=-1)
head(apr14)

dbListFields(mydb, 'nov_15')
nb = dbSendQuery(mydb, " SELECT * from nov_15 ")
nov15 = fetch(nb, n=-1)
head(nov15)

dbListFields(mydb, 'nov_16')
nb = dbSendQuery(mydb, " SELECT * from nov_16 ")
nov16 = fetch(nb, n=-1)
head(nov16)

dbListFields(mydb, 'nov_17')
nb = dbSendQuery(mydb, " SELECT * from nov_17 ")
nov17 = fetch(nb, n=-1)
head(nov17)

dbListFields(mydb, 'nov_18')
nb = dbSendQuery(mydb, " SELECT * from nov_18 ")
nov18 = fetch(nb, n=-1)
head(nov18)

dbListFields(mydb, 'nov_14')
nb = dbSendQuery(mydb, " SELECT * from nov_14 ")
nov14 = fetch(nb, n=-1)
head(nov18)

dbListFields(mydb, 'yeartab')
nb = dbSendQuery(mydb, " SELECT NAME,TNEA,YEAR from yeartab ")
yt = fetch(nb, n=-1)
head(yt)


#aq<-rbind(apr18,apr17)
#head(aq)



aq <- do.call("rbind", list(apr18, apr17, apr16, apr15,apr14,nov14,nov15,nov16,nov17,nov18))
head(aq)

k<-merge(x = aq, y = yt, by = c("NAME","TNEA"), all = TRUE)
head(k)

z<-sqldf("select DISTINCT YEAR from k ")
z


L<-sqldf("SELECT TNEA,YEAR,NAME,DISTRICT,SEM,TOTAL,PASS,PERCENTAGE FROM k ORDER BY NAME")
head(L)

L$PASS<-as.numeric(L$PASS)
L$PERCENTAGE<-as.numeric(L$PERCENTAGE)

L$TOTAL<-as.numeric(L$TOTAL)
L$YEAR<-as.numeric(L$YEAR)




avg1<-sqldf("SELECT TNEA,YEAR,NAME,DISTRICT,AVG(TOTAL),AVG(PERCENTAGE),SUM(PASS)/SUM(TOTAL) FROM L GROUP BY NAME")
write.csv(avg,"COLLEGE_AVG.csv")


library(reshape)
TA<-reshape(L, idvar = c("TNEA","YEAR","NAME","DISTRICT"), timevar = "SEM", direction = "wide")
head(TA)
TA<-sqldf("SELECT TNEA,YEAR,NAME,DISTRICT,TOTAL.APR14,PASS.APR14,PERCENTAGE.APR14,
                                          TOTAL.NOV14,PASS.NOV14,PERCENTAGE.NOV14,
                                          TOTAL.APR15,PASS.APR15,PERCENTAGE.APR15,
                                          TOTAL.NOV15,PASS.NOV15,PERCENTAGE.NOV15,
                                          TOTAL.APR16,PASS.APR16,PERCENTAGE.APR16,
                                          TOTAL.NOV16,PASS.NOV16,PERCENTAGE.NOV16,
                                          TOTAL.APR17,PASS.APR17,PERCENTAGE.APR17,
                                          TOTAL.NOV17,PASS.NOV17,PERCENTAGE.NOV17,
                                          TOTAL.APR18,PASS.APR18,PERCENTAGE.APR18,
                                          TOTAL.NOV18,PASS.NOV18,PERCENTAGE.NOV18
                                           FROM TA")


write.csv(TA,"COLLEGE.csv")



AVG<-sqldf("SELECT TNEA,YEAR,NAME,DISTRICT,TOTAL.APR14,PASS.APR14,avg(PERCENTAGE.APR14),
                                          TOTAL.NOV14,PASS.NOV14,avg(PERCENTAGE.NOV14),
                                          TOTAL.APR15,PASS.APR15,avg(PERCENTAGE.APR15),
                                          TOTAL.NOV15,PASS.NOV15,avg(PERCENTAGE.NOV15),
                                          TOTAL.APR16,PASS.APR16,avg(PERCENTAGE.APR16),
                                          TOTAL.NOV16,PASS.NOV16,avg(PERCENTAGE.NOV16),
                                          TOTAL.APR17,PASS.APR17,avg(PERCENTAGE.APR17),
                                          TOTAL.NOV17,PASS.NOV17,avg(PERCENTAGE.NOV17),
                                          TOTAL.APR18,PASS.APR18,avg(PERCENTAGE.APR18),
                                          TOTAL.NOV18,PASS.NOV18,avg(PERCENTAGE.NOV18) FROM TT")

write.csv(AVG,"COLLEGE_TOTAL")
#T1<-dcast(L1, TNEA+YEAR+NAME+DISTRICT ~ SEM, value.var = "TOTAL")
#head(TT)

head(L)
nms <- names(L)
nms
ui <- fluidPage(
  
  headerPanel(""),
  sidebarPanel(
    sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(L),
                value = 1000, step = 500, round = 0),
    selectInput('x', 'X', choices = nms, selected = "PASS"),
    selectInput('y', 'Y', choices = nms, selected = "PERCENTAGE"),
    checkboxGroupInput('color', 'color', choices = nms, selected = "DISTRICT"),
    selectInput('frame', 'frame', choices = nms, selected = "SEM"),
    selectInput('size', 'size', choices = nms, selected = "TOTAL"),
    selectInput('ids', 'ids', choices = nms, selected = "NAME"),
    
    selectInput('facet_row', 'Facet Row', c(None = '.', nms)),
    selectInput('facet_col', 'Facet Column', c(None = '.', nms))
    #sliderInput('plotHeight', 'Height of plot (in pixels)', 
    #           min = 100, max = 2000, value = 1000)
  ),
  mainPanel(
    plotlyOutput('trendPlot', height = "900px")
  )
)

server <- function(input, output) {
  
  #add reactive data information. Dataset = built in diamonds data
  d <- reactive({
    L[sample(nrow(L), input$sampleSize),]
  })
  
  output$trendPlot <- renderPlotly({
    
    # build graph with ggplot syntax
    #p <- ggplot(d(), aes_string(x = input$x, y = input$y, color = input$color)) + 
    #  geom_point()
    
    
    p <- ggplot(d(), aes_string(x = input$x, y = input$y, color = input$color)) +
      geom_point(aes_string(size = input$size,frame = input$frame,ids = input$ids))
    #geom_line()
    
    #p <- ggplot(d(), aes_string(x = input$x, y = input$y,color= input$color,group = input$color)) + 
    # geom_bar(aes(fill = input$color),width = 0.5,stat="identity")
    
    
    
    # if at least one facet column/row is specified, add it
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .') p <- p + facet_grid(facets)
    
    ggplotly(p) 
    #layout(height = input$plotHeight, autosize=TRUE)
    
  })
  
}

shinyApp(ui, server)












