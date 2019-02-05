library(shiny)
library(ggplot2) #the graphs
library(lubridate) #mdy

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #data import men's marathon and women's marathon
  marathon.df = read.csv("bothMarathon.csv")
  marathon.df1 = marathon.df
  marathon = subset(marathon.df, GENDER == 'Male')
  marathon.df2 = marathon
  wmarathon = subset(marathon.df, GENDER == 'Female')
  wmarathon.df = wmarathon
  
  #data import men's half marathon and women's half marathon
  halfMarathon.df = read.csv("halfMarathon.csv")
  halfMarathon.df1 = halfMarathon.df
  halfMarathon = subset(halfMarathon.df, GENDER == 'MALE')
  halfMarathon.df2 = halfMarathon
  whalfMarathon = subset(halfMarathon.df, GENDER == 'FEMALE')
  whalfMarathon.df = whalfMarathon
  
  #data import men's 10k and women's 10k
  tenk.df = read.csv("10k.csv")
  tenk.df1 = tenk.df
  tenk = subset(tenk.df, GENDER == 'MALE')
  tenk.df2 = tenk
  wtenk = subset(tenk.df, GENDER == 'FEMALE')
  wtenk.df = wtenk
  
  #data import men's 5k and women's 5k
  fivek.df = read.csv("5k.csv")
  fivek.df1 = fivek.df
  fivek = subset(fivek.df, GENDER == 'MALE')
  fivek.df2 = fivek
  wfivek = subset(fivek.df, GENDER == 'FEMALE')
  wfivek.df = wfivek
  
  #set marathon dates in decreasing order
  marathon$DATE = mdy(marathon$DATE)
  dq = marathon[order(marathon$DATE,decreasing =T ),]
  wmarathon$DATE = mdy(wmarathon$DATE)
  dq = wmarathon[order(wmarathon$DATE,decreasing=T),]
  marathon.df$DATE = mdy(marathon.df$DATE)
  dq = marathon.df[order(marathon.df$DATE,decreasing =T ),]
  
  #set half marathon dates in decreasing order
  halfMarathon$DATE = mdy(halfMarathon$DATE)
  dq = halfMarathon[order(halfMarathon$DATE,decreasing =T ),]
  whalfMarathon$DATE = mdy(whalfMarathon$DATE)
  dq = whalfMarathon[order(whalfMarathon$DATE,decreasing=T),]
  halfMarathon.df$DATE = mdy(halfMarathon.df$DATE)
  dq = halfMarathon.df[order(halfMarathon.df$DATE,decreasing =T ),]
  
  #set 10k dates in decreasing order
  tenk$DATE = mdy(tenk$DATE)
  dq = tenk[order(tenk$DATE,decreasing =T ),]
  wtenk$DATE = mdy(wtenk$DATE)
  dq = wtenk[order(wtenk$DATE,decreasing=T),]
  tenk.df$DATE = mdy(tenk.df$DATE)
  dq = tenk.df[order(tenk.df$DATE,decreasing =T ),]
  
  #set 5k dates in decreasing order
  fivek$DATE = mdy(fivek$DATE)
  dq = tenk[order(fivek$DATE,decreasing =T ),]
  wfivek$DATE = mdy(wfivek$DATE)
  dq = wfivek[order(wfivek$DATE,decreasing=T),]
  fivek.df$DATE = mdy(fivek.df$DATE)
  dq = fivek.df[order(fivek.df$DATE,decreasing =T ),]
  
  #convert marathon string times into numerics 
  x = paste(marathon$TIME)
  x = as.POSIXct(strptime(x, "%H:%M:%S"))
  marathon$TIME = x 
  
  x = paste(wmarathon$TIME)
  x = as.POSIXct(strptime(x, "%H:%M:%S"))
  wmarathon$TIME = x 
  
  x = paste(marathon.df$TIME)
  x = as.POSIXct(strptime(x, "%H:%M:%S"))
  marathon.df$TIME = x 
  
  #convert half marathon string times into numerics 
  x = paste(halfMarathon$TIME)
  x = as.POSIXct(strptime(x, "%H:%M:%S"))
  halfMarathon$TIME = x 
  
  x = paste(whalfMarathon$TIME)
  x = as.POSIXct(strptime(x, "%H:%M:%S"))
  whalfMarathon$TIME = x 
  
  x = paste(halfMarathon.df$TIME)
  x = as.POSIXct(strptime(x, "%H:%M:%S"))
  halfMarathon.df$TIME = x 
  
  #convert 10k string times into numerics 
  x = paste(tenk$TIME)
  x = as.POSIXct(strptime(x, "%M:%S"))
  tenk$TIME = x 
  
  x = paste(wtenk$TIME)
  x = as.POSIXct(strptime(x, "%M:%S"))
  wtenk$TIME = x 
  
  x = paste(tenk.df$TIME)
  x = as.POSIXct(strptime(x, "%M:%S"))
  tenk.df$TIME = x 
  
  #convert 5k string times into numerics 
  x = paste(fivek$TIME)
  x = as.POSIXct(strptime(x, "%M:%S"))
  fivek$TIME = x 
  
  x = paste(wfivek$TIME)
  x = as.POSIXct(strptime(x, "%M:%S"))
  wfivek$TIME = x 
  
  x = paste(fivek.df$TIME)
  x = as.POSIXct(strptime(x, "%M:%S"))
  fivek.df$TIME = x 
  
  output$distPlot = renderPlot({
    
    # ---------------------  MARATHON PLOTS ---------------------
    # men's marathon plot  
    marathonGraph = ggplot(data = marathon, aes(x = DATE, y = TIME)) +  geom_line(color = "steelblue") + geom_point() +
      theme(axis.text.x = element_text(angle=70, vjust=0.6))  +
      labs(title="Men's Marathon Record Over Time", x= "DATE", y ="FINISHING TIME") 
    
    
    # women's marathon plot
    wmarathonGraph = ggplot(data = wmarathon, aes(x = DATE, y = TIME, group = 1)) +  geom_line(color = "deeppink") + geom_point() +
      theme(axis.text.x = element_text(angle=70, vjust=0.6))  +
      labs(title="Women's Marathon Record Over Time", x= "DATE", y ="FINISHING TIME")
    
    # both men's and women's marathon plot
    bothmarathonGraph = ggplot(data = marathon.df, aes(x=DATE, y =TIME,col=GENDER)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle=70, vjust=0.6))  +
      labs(title="Men's and Women's Marathon Records Over Time", x= "DATE", y ="FINISHING TIME") 
    
    # --------------------- HALF MARATHON PLOTS ---------------------
    
    # men's half marathon plot
    halfmarathonGraph = ggplot(data = halfMarathon, aes(x = DATE, y = TIME, group = 1)) +  geom_line(color = "steelblue") + geom_point() +
      theme(axis.text.x = element_text(angle=70, vjust=0.6))  +
      labs(title="Men's Half Marathon Record Over Time", x= "DATE", y ="FINISHING TIME") 
    
    # women's half marathon plot
    whalfmarathonGraph = ggplot(data = whalfMarathon, aes(x = DATE, y = TIME, group = 1)) +  geom_line(color = "deeppink") + geom_point() +
      theme(axis.text.x = element_text(angle=70, vjust=0.6))  +
      labs(title="Womens's Half Marathon Record Over Time", x= "DATE", y ="FINISHING TIME") 
    
    # both men's and women's half marathon plot
    bothhalfmarathon = ggplot(data = halfMarathon.df, aes(x=DATE, y =TIME,col=GENDER)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle=70, vjust=0.6))  +
      labs(title="Men's and Women's Half Marathon Records Over Time", x= "DATE", y ="FINISHING TIME") 
    
    # ----------------- 10K PLOTS ----------------------
    
    tenkGraph = ggplot(data = tenk, aes(x = DATE, y = TIME, group = 1)) +  geom_line(color = "steelblue") + geom_point() +
      theme(axis.text.x = element_text(angle=70, vjust=0.6))  +
      labs(title="Men's 10K Record Over Time", x= "DATE", y ="FINISHING TIME") 
    
    # women's half marathon plot
    wtenkGraph = ggplot(data = wtenk, aes(x = DATE, y = TIME, group = 1)) +  geom_line(color = "deeppink") + geom_point() +
      theme(axis.text.x = element_text(angle=70, vjust=0.6))  +
      labs(title="Womens's 10K Record Over Time", x= "DATE", y ="FINISHING TIME") 
    
    # both men's and women's half marathon plot
    bothtenkGraph = ggplot(data = tenk.df, aes(x=DATE, y =TIME,col=GENDER)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle=70, vjust=0.6))  +
      labs(title="Men's and Women's 10K Records Over Time", x= "DATE", y ="FINISHING TIME") 
    
    # ----------------- 5K PLOTS ----------------------
    
    fivekGraph = ggplot(data = fivek, aes(x = DATE, y = TIME, group = 1)) +  geom_line(color = "steelblue") + geom_point() +
      theme(axis.text.x = element_text(angle=70, vjust=0.6))  +
      labs(title="Men's 5K Record Over Time", x= "DATE", y ="FINISHING TIME") 
    
    # women's half marathon plot
    wfivekGraph = ggplot(data = wfivek, aes(x = DATE, y = TIME, group = 1)) +  geom_line(color = "deeppink") + geom_point() +
      theme(axis.text.x = element_text(angle=70, vjust=0.6))  +
      labs(title="Womens's 5K Record Over Time", x= "DATE", y ="FINISHING TIME") 
    
    # both men's and women's half marathon plot
    bothfivekGraph = ggplot(data = fivek.df, aes(x=DATE, y =TIME,col=GENDER)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle=70, vjust=0.6))  +
      labs(title="Men's and Women's 5K Records Over Time", x= "DATE", y ="FINISHING TIME") 
    
    # ----------------- PRINT PLOTS --------------------
    
    # men's and women's marathon prints
    if(input$plotg == "Marathon" & input$rd1 == "Male") print(marathonGraph)
    if(input$plotg == "Marathon" & input$rd1 == "Female") print(wmarathonGraph)
    if(input$plotg == "Marathon" & input$rd1 == "Both") print(bothmarathonGraph)
    
    
    # men's and women's half marathon prints
    if(input$plotg == "Half Marathon" & input$rd1 == "Male") print(halfmarathonGraph) 
    if(input$plotg == "Half Marathon" & input$rd1 == "Female") print(whalfmarathonGraph)
    if(input$plotg == "Half Marathon" & input$rd1 == "Both") print(bothhalfmarathon)
    
    # men's and women's 10K prints
    if(input$plotg == "10,000 Meters" & input$rd1 == "Male") print(tenkGraph) 
    if(input$plotg == "10,000 Meters" & input$rd1 == "Female") print(wtenkGraph)
    if(input$plotg == "10,000 Meters" & input$rd1 == "Both") print(bothtenkGraph)
    
    # men's and women's 5K prints
    if(input$plotg == "5,000 Meters" & input$rd1 == "Male") print(fivekGraph) 
    if(input$plotg == "5,000 Meters" & input$rd1 == "Female") print(wfivekGraph)
    if(input$plotg == "5,000 Meters" & input$rd1 == "Both") print(bothfivekGraph)
  })
   
  # ------------------------ ADDITIONAL ANALYTICS ------------------
  
  
  # ------------------------ NATIONALITY BAR GRAPH/PIE CHART -------
  
  output$pie = renderPlot({
  
    #MARATHON
    
    if(input$plotg == "Marathon" & input$rd1 == "Male" & input$additional == "Nationality Bar Graph") {
      x = barplot(table(marathon$COUNTRY), xaxt="n",col="steelblue",main="Number of Marathon Records Per Country")
      labs = paste(names(table(marathon$COUNTRY)), "")
      text(cex=.9, x=x-0, y=-1, labs, xpd=TRUE, srt=75)
    }
    
    if(input$plotg == "Marathon" & input$rd1 == "Male" & input$additional == "Nationality Pie Chart") {
      pie(with(marathon,table(COUNTRY)),col=1:14,main = "Number of Marathon Records Per Country")
    }
    
    if(input$plotg == "Marathon" & input$rd1 == "Female" & input$additional == "Nationality Bar Graph") {
      x = barplot(table(wmarathon$COUNTRY), xaxt="n",col="deeppink",main="Number of Marathon Records Per Country")
      labs = paste(names(table(wmarathon$COUNTRY)), "")
      text(cex=.9, x=x-0, y=-1, labs, xpd=TRUE, srt=75)
    }
    
    if(input$plotg == "Marathon" & input$rd1 == "Female" & input$additional == "Nationality Pie Chart") {
      pie(with(wmarathon,table(COUNTRY)),col=1:14,main = "Number of Marathon Records Per Country")
    }
    
    if(input$plotg == "Marathon" & input$rd1 == "Both" & input$additional == "Nationality Bar Graph") {
      x = barplot(table(marathon.df1$COUNTRY), xaxt="n",col="purple",main="Number of Marathon Records Per Country")
      labs = paste(names(table(marathon.df1$COUNTRY)), "")
      text(cex=.9, x=x-0, y=-1, labs, xpd=TRUE, srt=75)
    }
    
    if(input$plotg == "Marathon" & input$rd1 == "Both" & input$additional == "Nationality Pie Chart") {
      pie(with(marathon.df1,table(COUNTRY)),col=1:14,main = "Number of Marathon Records Per Country")
    }
    
    #HALF MARATHON
    
    if(input$plotg == "Half Marathon" & input$rd1 == "Male" & input$additional == "Nationality Bar Graph") {
      x = barplot(table(halfMarathon$COUNTRY), xaxt="n",col="steelblue",main="Number of Half Marathon Records Per Country")
      labs = paste(names(table(halfMarathon$COUNTRY)), "")
      text(cex=.9, x=x-0, y=-1, labs, xpd=TRUE, srt=75)
    }
    
    if(input$plotg == "Half Marathon" & input$rd1 == "Male" & input$additional == "Nationality Pie Chart") {
      pie(with(halfMarathon,table(COUNTRY)),col=1:14,main = "Number of Half Marathon Records Per Country")
    }
    
    if(input$plotg == "Half Marathon" & input$rd1 == "Female" & input$additional == "Nationality Bar Graph") {
      x = barplot(table(whalfMarathon$COUNTRY), xaxt="n",col="deeppink",main="Number of Half Marathon Records Per Country")
      labs = paste(names(table(whalfMarathon$COUNTRY)), "")
      text(cex=.9, x=x-0, y=-1, labs, xpd=TRUE, srt=75)
    }
    
    if(input$plotg == "Half Marathon" & input$rd1 == "Female" & input$additional == "Nationality Pie Chart") {
      pie(with(whalfMarathon,table(COUNTRY)),col=1:14,main = "Number of Half Marathon Records Per Country")
    }
    
    if(input$plotg == "Half Marathon" & input$rd1 == "Both" & input$additional == "Nationality Bar Graph") {
      x = barplot(table(halfMarathon.df1$COUNTRY), xaxt="n",col="purple",main="Number of Half Marathon Records Per Country")
      labs = paste(names(table(halfMarathon.df1$COUNTRY)), "")
      text(cex=.9, x=x-0, y=-1, labs, xpd=TRUE, srt=75)
    }
    
    if(input$plotg == "Half Marathon" & input$rd1 == "Both" & input$additional == "Nationality Pie Chart") {
      pie(with(halfMarathon.df1,table(COUNTRY)),col=1:14,main = "Number of Half Marathon Records Per Country")
    }
    
    #10K
    
    if(input$plotg == "10,000 Meters" & input$rd1 == "Male" & input$additional == "Nationality Bar Graph") {
      x = barplot(table(tenk$COUNTRY), xaxt="n",col="steelblue",main="Number of 10K Records Per Country")
      labs = paste(names(table(tenk$COUNTRY)), "")
      text(cex=.9, x=x-0, y=-1, labs, xpd=TRUE, srt=75)
    }
    
    if(input$plotg == "10,000 Meters" & input$rd1 == "Male" & input$additional == "Nationality Pie Chart") {
      pie(with(tenk,table(COUNTRY)),col=1:14,main = "Number of 10K Records Per Country")
    }
    
    if(input$plotg == "10,000 Meters" & input$rd1 == "Female" & input$additional == "Nationality Bar Graph") {
      x = barplot(table(wtenk$COUNTRY), xaxt="n",col="deeppink",main="Number of 10K Records Per Country")
      labs = paste(names(table(wtenk$COUNTRY)), "")
      text(cex=.9, x=x-0, y=-1, labs, xpd=TRUE, srt=75)
    }
    
    if(input$plotg == "10,000 Meters" & input$rd1 == "Female" & input$additional == "Nationality Pie Chart") {
      pie(with(wtenk,table(COUNTRY)),col=1:14,main = "Number of 10K Records Per Country")
    }
    
    if(input$plotg == "10,000 Meters" & input$rd1 == "Both" & input$additional == "Nationality Bar Graph") {
      x = barplot(table(tenk.df1$COUNTRY), xaxt="n",col="purple",main="Number of 10k Records Per Country")
      labs = paste(names(table(tenk.df1$COUNTRY)), "")
      text(cex=.9, x=x-0, y=-1, labs, xpd=TRUE, srt=75)
    }
    
    if(input$plotg == "10,000 Meters" & input$rd1 == "Both" & input$additional == "Nationality Pie Chart") {
      pie(with(tenk.df1,table(COUNTRY)),col=1:14,main = "Number of 10K Records Per Country")
    }
    
    #5K
    
    if(input$plotg == "5,000 Meters" & input$rd1 == "Male" & input$additional == "Nationality Bar Graph") {
      x = barplot(table(fivek$COUNTRY), xaxt="n",col="steelblue",main="Number of 5K Records Per Country")
      labs = paste(names(table(fivek$COUNTRY)), "")
      text(cex=.9, x=x-0, y=-1, labs, xpd=TRUE, srt=75)
    }
    
    if(input$plotg == "5,000 Meters" & input$rd1 == "Male" & input$additional == "Nationality Pie Chart") {
      pie(with(fivek,table(COUNTRY)),col=1:14,main = "Number of 5K Records Per Country")
    }
    
    if(input$plotg == "5,000 Meters" & input$rd1 == "Female" & input$additional == "Nationality Bar Graph") {
      x = barplot(table(wfivek$COUNTRY), xaxt="n",col="deeppink",main="Number of 5K Records Per Country")
      labs = paste(names(table(wfivek$COUNTRY)), "")
      text(cex=.9, x=x-0, y=-1, labs, xpd=TRUE, srt=75)
    }
    
    if(input$plotg == "5,000 Meters" & input$rd1 == "Female" & input$additional == "Nationality Pie Chart") {
      pie(with(wfivek,table(COUNTRY)),col=1:14,main = "Number of 5K Records Per Country")
    }
    
    if(input$plotg == "5,000 Meters" & input$rd1 == "Both" & input$additional == "Nationality Bar Graph") {
      x = barplot(table(fivek.df1$COUNTRY), xaxt="n",col="purple",main="Number of 5k Records Per Country")
      labs = paste(names(table(fivek.df1$COUNTRY)), "")
      text(cex=.9, x=x-0, y=-1, labs, xpd=TRUE, srt=75)
    }
    
    if(input$plotg == "5,000 Meters" & input$rd1 == "Both" & input$additional == "Nationality Pie Chart") {
      pie(with(fivek.df1,table(COUNTRY)),col=1:14,main = "Number of 5K Records Per Country")
    }
    
  })
  
  # ------------------------ DATA TABLES ---------------------------
  
  output$data = renderDataTable({
    if(input$plotg == "Marathon" & input$rd1 == "Male" & input$table == TRUE) marathon.df2
    else if(input$plotg == "Marathon" & input$rd1 == "Female" & input$table == TRUE) wmarathon.df
    else if(input$plotg == "Marathon" & input$rd1 == "Both" & input$table == TRUE) marathon.df1
    else if(input$plotg == "Half Marathon" & input$rd1 == "Male" & input$table == TRUE) halfMarathon.df2
    else if(input$plotg == "Half Marathon" & input$rd1 == "Female" & input$table == TRUE) whalfMarathon.df
    else if(input$plotg == "Half Marathon" & input$rd1 == "Both" & input$table == TRUE) halfMarathon.df1
    else if(input$plotg == "10,000 Meters" & input$rd1 == "Male" & input$table == TRUE) tenk.df2
    else if(input$plotg == "10,000 Meters" & input$rd1 == "Female" & input$table == TRUE) wtenk.df
    else if(input$plotg == "10,000 Meters" & input$rd1 == "Both" & input$table == TRUE) tenk.df1
    else if(input$plotg == "5,000 Meters" & input$rd1 == "Male" & input$table == TRUE) fivek.df2
    else if(input$plotg == "5,000 Meters" & input$rd1 == "Female" & input$table == TRUE) wfivek.df
    else if(input$plotg == "5,000 Meters" & input$rd1 == "Both" & input$table == TRUE) fivek.df1
  })
  
})
