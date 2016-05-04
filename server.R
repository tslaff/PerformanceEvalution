library(ggplot2)
library(quantmod)
library(lubridate)
library(depmixS4)
library(gridExtra)
library(repmis)

Correlation_Analysis<-function(data1,data2,lower_timeframe){
  
  Epoch_Start_1<-as.numeric(as.POSIXlt(as.character(data1[1,3]), format = "%m/%d/%y %H:%M",tz="GMT"))
  Epoch_End_1<-as.numeric(as.POSIXlt(as.character(data1[nrow(data1),4]), format = "%m/%d/%y %H:%M",tz="GMT"))
  
  Epoch_Start_2<-as.numeric(as.POSIXlt(as.character(data2[1,3]), format = "%m/%d/%y %H:%M",tz="GMT"))
  Epoch_End_2<-as.numeric(as.POSIXlt(as.character(data2[nrow(data2),4]), format = "%m/%d/%y %H:%M",tz="GMT"))
  
  Epoch_Start<-ifelse(Epoch_Start_1<Epoch_Start_2,Epoch_Start_1,Epoch_Start_2)
  Epoch_End<-ifelse(Epoch_End_1>Epoch_End_2,Epoch_End_1,Epoch_End_2)
  
  Sequence<-function(pp){
    switch(pp,
           "1h"=3600,
           "2h"=7200,
           "4h"=14400,
           "6h"=21600,
           "1d"=86400
    )   
  }
  Seq<-Sequence(lower_timeframe)
  Underlying<-seq(Epoch_Start,Epoch_End,by=Seq)
  
  Long_Trade_Positions_1<-which(data1[,2]=="Buy")
  Long_Open_1<-as.numeric(as.POSIXlt(as.character(data1[Long_Trade_Positions_1,3]), format = "%m/%d/%y %H:%M",tz="GMT"))
  Long_Close_1<-as.numeric(as.POSIXlt(as.character(data1[Long_Trade_Positions_1,4]), format = "%m/%d/%y %H:%M",tz="GMT"))
  
  Short_Trade_Positions_1<-which(data1[,2]=="Sell")
  Short_Open_1<-as.numeric(as.POSIXlt(as.character(data1[Short_Trade_Positions_1,3]), format = "%m/%d/%y %H:%M",tz="GMT"))
  Short_Close_1<-as.numeric(as.POSIXlt(as.character(data1[Short_Trade_Positions_1,4]), format = "%m/%d/%y %H:%M",tz="GMT"))
  
  Long_Trade_Positions_2<-which(data2[,2]=="Buy")
  Long_Open_2<-as.numeric(as.POSIXlt(as.character(data2[Long_Trade_Positions_2,3]), format = "%m/%d/%y %H:%M",tz="GMT"))
  Long_Close_2<-as.numeric(as.POSIXlt(as.character(data2[Long_Trade_Positions_2,4]), format = "%m/%d/%y %H:%M",tz="GMT"))
  
  Short_Trade_Positions_2<-which(data2[,2]=="Sell")
  Short_Open_2<-as.numeric(as.POSIXlt(as.character(data2[Short_Trade_Positions_2,3]), format = "%m/%d/%y %H:%M",tz="GMT"))
  Short_Close_2<-as.numeric(as.POSIXlt(as.character(data2[Short_Trade_Positions_2,4]), format = "%m/%d/%y %H:%M",tz="GMT"))
  
  Trade_Function<-function(data,open,close){
    Test_Function<-function(x,y,z){
      ifelse(x >= y && x <= z,x,0)   
    }
    sameList = lapply(data, function(x) mapply(Test_Function,x,open,close))
    Trade_Num<-t(as.data.frame(lapply(sameList,max)))
    Trades<-Trade_Num[-c(which(Trade_Num ==0))]
    Trade_Dates<-as.POSIXlt(Trades, origin="1970-01-01",tz="GMT")
    return(Trade_Dates)
    
  }
  
  
  Long_Trades_1<-Trade_Function(Underlying,Long_Open_1,Long_Close_1)
  Longs_1<-rep(1,length(Long_Trades_1))
  Long_Trade_Data_1<-data.frame(as.numeric(Long_Trades_1),Longs_1)
  colnames(Long_Trade_Data_1)<-c("Long_Trades","Longs")
  
  Long_Trades_2<-Trade_Function(Underlying,Long_Open_2,Long_Close_2)
  Longs_2<-rep(1,length(Long_Trades_2))
  Long_Trade_Data_2<-data.frame(as.numeric(Long_Trades_2),Longs_2)
  colnames(Long_Trade_Data_2)<-c("Long_Trades","Longs")
  
  Short_Trades_1<-Trade_Function(Underlying,Short_Open_1,Short_Close_1)
  Shorts_1<-rep(-1,length(Short_Trades_1))
  Short_Trade_Data_1<-data.frame(as.numeric(Short_Trades_1),Shorts_1)
  colnames(Short_Trade_Data_1)<-c("Short_Trades","Shorts")
  
  Short_Trades_2<-Trade_Function(Underlying,Short_Open_2,Short_Close_2)
  Shorts_2<-rep(-1,length(Short_Trades_2))
  Short_Trade_Data_2<-data.frame(as.numeric(Short_Trades_2),Shorts_2)
  colnames(Short_Trade_Data_2)<-c("Short_Trades","Shorts")
  
  
  Returns_1<-as.numeric(sub('\\$','',as.character(data1[,7])))
  Return_Data_1<-data.frame(as.numeric(as.POSIXlt(as.character(data1[,4]), format = "%m/%d/%y %H:%M",tz="GMT")),Returns_1)
  colnames(Return_Data_1)<-c("Trade_Close","Returns")
  
  Returns_2<-as.numeric(sub('\\$','',as.character(data2[,7])))
  Return_Data_2<-data.frame(as.numeric(as.POSIXlt(as.character(data2[,4]), format = "%m/%d/%y %H:%M",tz="GMT")),Returns_2)
  colnames(Return_Data_2)<-c("Trade_Close","Returns")
  
  
  newdata_1<-as.data.frame(Underlying)
  
  newdata_1$long <- Long_Trade_Data_1$Longs[pmatch(newdata_1$Underlying, Long_Trade_Data_1$Long_Trades)]
  newdata_1$short <- Short_Trade_Data_1$Shorts[pmatch(newdata_1$Underlying, Short_Trade_Data_1$Short_Trades)]
  newdata_1$Returns <- Return_Data_1$Returns[pmatch(newdata_1$Underlying, Return_Data_1$Trade_Close)]
  
  newdata_2<-as.data.frame(Underlying)
  
  newdata_2$long <- Long_Trade_Data_2$Longs[pmatch(newdata_2$Underlying, Long_Trade_Data_2$Long_Trades)]
  newdata_2$short <- Short_Trade_Data_2$Shorts[pmatch(newdata_2$Underlying, Short_Trade_Data_2$Short_Trades)]
  newdata_2$Returns <- Return_Data_2$Returns[pmatch(newdata_2$Underlying, Return_Data_2$Trade_Close)]
  
  Long_Na_1<-ifelse(is.na(newdata_1[,2])==TRUE,0,newdata_1[,2])
  Short_Na_1<-ifelse(is.na(newdata_1[,3])==TRUE,0,newdata_1[,3])
  Trade_Returns_1<-ifelse(is.na(newdata_1[,4])==TRUE,0,newdata_1[,4])
  
  Long_Na_2<-ifelse(is.na(newdata_2[,2])==TRUE,0,newdata_2[,2])
  Short_Na_2<-ifelse(is.na(newdata_2[,3])==TRUE,0,newdata_2[,3])
  Trade_Returns_2<-ifelse(is.na(newdata_2[,4])==TRUE,0,newdata_2[,4])
  
  Trade_Direction_1<-Long_Na_1+Short_Na_1
  Trade_Times_1<-as.POSIXct(newdata_1[,1], origin="1970-01-01",tz="GMT")
  Cumulative_Returns_1<-cumsum(Trade_Returns_1)
  
  Trade_Direction_2<-Long_Na_2+Short_Na_2
  Trade_Times_2<-as.POSIXct(newdata_2[,1], origin="1970-01-01",tz="GMT")
  Cumulative_Returns_2<-cumsum(Trade_Returns_2)
  
  Final_Trades_1<-data.frame(as.numeric(Trade_Times_1),Trade_Direction_1, Cumulative_Returns_1)
  Final_Trades_2<-data.frame(as.numeric(Trade_Times_2),Trade_Direction_2, Cumulative_Returns_2)
  
  Correlation<-cor(Final_Trades_1[,2],Final_Trades_2[,2],method="pearson")
  Category<-c("Correlation (Pearsons)","Strat 1 number of Trades","Strat 1 Cumulative Return ($)","Strat 2 Number of Trades","Strat 2 Cumulative Return ($)")
  Values <- as.character(c(round(Correlation,2),nrow(data1),sum(Returns_1),nrow(data2),sum(Returns_2)))
  Summary_Data<-data.frame(Category,Values)
  Plot_Data<-data.frame(Final_Trades_1,Final_Trades_2)
  colnames(Plot_Data)<-c("Open_1","Order_1","Returns_1","Open_2","Order_2","Returns_2")
  
  Final_List<-list(Plot_Data,Summary_Data)
  return(Final_List)
  
}

Plot_Strategies<-function(correlation_data){
  Function_Data1<-correlation_data
  Dates1<-as.POSIXct(Function_Data1[,1], origin="1970-01-01",tz="GMT")
  Dates2<-as.POSIXct(Function_Data1[,4], origin="1970-01-01",tz="GMT")
  Plots_Data<-data.frame(Function_Data1[,-c(1,4)],Dates1,Dates2)
  g<-ggplot(Plots_Data,aes(x=Dates1))
  gg<-g+geom_line(aes(y=Returns_1,color="Strategy_1"))
  ggg<-gg+geom_line(aes(y=Returns_2,color="Strategy_2"))
  gggg<-ggg+labs(title="Strategy Returns",x="Date",y="Cumulative Return ($)")
  ggggg<-gggg+scale_color_manual("",breaks=c("Strategy_1","Strategy_2"),values=c("Strategy_1"="blue","Strategy_2"="dark green"))
  return(ggggg)
}




# Shiny server function

shinyServer(function(input, output) {


  output$loadmessage<-renderText({
    
    if (input$loadsampleButton == 1){
      filename1 <-"Example_Strat_1.csv"
      mykey1 <- "0teh3k6hybmqruk"
      Strat1_Data<<-source_DropboxData(filename1,key=mykey1, sep=",", header=TRUE)
      
      filename2 <-"Example_Strat_2.csv"
      mykey2 <- "1qb4jyqfxoyr3ko"
      Strat2_Data<<-source_DropboxData(filename2,key=mykey2, sep=",", header=TRUE)
      
          if((is.null(Strat1_Data))||(is.null(Strat2_Data))){
            Data_message<-print('Sample data NOT loaded successfully')
          }
          else{
            Data_message<-print('Sample data successfully loaded')}
    }
    else{
      return()
    }
    
    Data_message
  })    


Correlation_Data<-reactive({
  
  if((is.null(Strat1_Data))||is.null(is.null(Strat2_Data))){
      Strat1 <- input$file1
      Strat2 <- input$file2
      
      if (is.null(Strat1))
        return(NULL)
      
      if (is.null(Strat2))
        return(NULL)
      
      Strat1_Data<-read.csv(Strat1$datapath,header=TRUE, sep=',')
      Strat2_Data<-read.csv(Strat2$datapath, header=TRUE, sep=',')
    }
  
  Strategy_Correlation<-Correlation_Analysis(Strat1_Data,Strat2_Data,input$period)
  
  Strategy_Correlation
    
  })
  

  
output$contents2<-renderTable({
    
    if (input$goButton == 0)
      return()
    
    Output_11<-Correlation_Data();
    Output_12<-Output_11[[2]]
    Output_12
    
  })

output$contents1<-renderPlot({
  
  if (input$goButton == 0)
    return()
  
  Output_11<-Correlation_Data();
  Output_2<-as.data.frame(Output_11[[1]])
  Function_Data<-Output_2
  Plot_Strategies(Function_Data)
})

output$loadmessage_ohlc<-renderText({
  if (input$loadsampleButton_ohlc == 1){
    filename_ohlc <-"Sample_Market_Data.csv"
    mykey_ohlc <- "sz9wgc4r1f3rwvr"
    OHLC_Data<<-source_DropboxData(filename_ohlc,key=mykey_ohlc, sep=",", header=TRUE)
    
    if((is.null(OHLC_Data))){
      OHLC_Data_message<-print('Sample data NOT loaded successfully')
    }
    else{
      OHLC_Data_message<-print('Sample data successfully loaded')}
  }
  else{
    return()
  }
  
  OHLC_Data_message
})


output$Market_Analysis_Test<-renderPlot({
  
  if (input$indicator_run == 0)
    return()
  
    if(is.null(OHLC_Data)){
    OHLC <- input$ohlc
    if (is.null(OHLC))
      return(NULL)
    OHLC_Data<-read.csv(OHLC$datapath, header=TRUE, sep=',')
    }
  
  Market_df<-OHLC_Data;
  Time<-as.POSIXlt(Market_df[,1], format = "%m/%d/%y %H:%M",tz="GMT")
  Market_xts<-as.xts(data.frame(Market_df[,2:5],row.names=Time))
  
  
   Indicator_1_Data<-function(HLC){
     switch(input$indicator_1,
           "1" = RSI(HLC[,4],n=input$indicator_1_period),
           "2" = CCI(HLC[,2:4],n=input$indicator_1_period),
           "3" = log(HLC[,4])-log(HLC[,2]),
           "4" = ATR(HLC[,2:4], n=input$indicator_1_period))
  }
  
  Indicator_2_Data<-function(HLC){
    switch(input$indicator_2,
           "1" = RSI(HLC[,4],n=input$indicator_2_period),
           "2" = CCI(HLC[,2:4],n=input$indicator_2_period),
           "3" = log(HLC[,4])-log(HLC[,2]),
           "4" = ATR(HLC[,2:4], n=input$indicator_2_period))
  }
  
  
  I1<-Indicator_1_Data(Market_xts);
  I2<-Indicator_2_Data(Market_xts);
  
  Indicator_Data<-na.omit(data.frame(I1,I2));
  col_n_1<-as.character("Ind_1");
  col_n_2<-as.character("Ind_2");
  names(Indicator_Data)[1]<-col_n_1;
  names(Indicator_Data)[2]<-col_n_2;
  
  Ind_1_Name_function<-function(ind_1){
    switch(ind_1,
           "1" = "RSI",
           "2" = "CCI",
           "3" = "Log Returns",
           "4" = "ATR")
  }
  
  Ind_2_Name_function<-function(ind_2){
    switch(ind_2,
           "1" = "RSI",
           "2" = "CCI",
           "3" = "Log Returns",
           "4" = "ATR")
  }
  
  Ind_1_Name<-paste(c(Ind_1_Name_function(input$indicator_1),as.character(input$indicator_1_period)),sep = " ",collapse = " ");
  Ind_2_Name<-paste(c(Ind_2_Name_function(input$indicator_2),as.character(input$indicator_2_period)),sep = " ",collapse = " ");
  
  HMM<-depmix(list(Ind_1~1,Ind_2~1),data=Indicator_Data,nstates=3,family=list(gaussian(),gaussian()))
  HMMfit<-fit(HMM, verbose = FALSE)
  HMMpost<-posterior(HMMfit)
  
  Post_Times<-row.names(as.data.frame(Indicator_Data))
  Post_Times_F<-ymd_hms(as.character(Post_Times))
  Plot_Data_HMM<-data.frame(Post_Times_F,HMMpost[,1],Indicator_Data[,1],Indicator_Data[,2]);
  colnames(Plot_Data_HMM)<-c("Date_1","Regime","Ind_1","Ind_2");
  
  HMM_Plot<-ggplot(data=Plot_Data_HMM,aes(x = Date_1,y = Regime))+geom_line(color="darkblue");
  HMM_Plota<-HMM_Plot+labs(title="Market Regime",x="Date",y="Regime");
  
  Ind_1_Plot<-ggplot(data=Plot_Data_HMM,aes(x = Date_1,y = Ind_1))+geom_line(color="darkgreen");
  Ind_1_Plota<-Ind_1_Plot+labs(title=paste(c(Ind_1_Name,"Plot"),sep = " "),x="Date",y=Ind_1_Name);
  
  Ind_2_Plot<-ggplot(data=Plot_Data_HMM,aes(x = Date_1,y = Ind_2))+geom_line(color="darkred");
  Ind_2_Plota<-Ind_2_Plot+labs(title=paste(c(Ind_2_Name,"Plot"),sep = " "),x="Date",y=Ind_2_Name);

  grid.arrange(HMM_Plota,Ind_1_Plota,Ind_2_Plota,ncol=1,nrow=3)
  
  })


  
})
