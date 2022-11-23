# step1
getwd()
weather.data <- read.csv("DailyDelhiClimateTrain.csv") 
#predicting data by keeping mean Temperature as independent and windspeed and mean pressure as dependent variables 
#libraries 
library(ggplot2) 
library(corrplot) 
library(modelr) 
#functions 
#linear regression function 
LinearRegression<-function(x,y,test.data){ 
  test.data<-test.df 
  #plot(x,y) 
  df<-data.frame(x,y,x,y,x,y) 
  names(df)<-c('x','y','x-x!','y-y!','x-x!^2','x-x1 * y-y1') 
  df 
  x.mean<-mean(df[,1]) 
  y.mean<-mean(df[,2]) 
  for( i in 1: nrow(df[1])){ 
    df[i,3]<-df[i,1]-x.mean 
    df[i,4]<-df[i,2]-y.mean 
  } 
  for( i in 1: nrow(df[1])){ 
    df[i,5]<-df[i,3]*df[i,3] 
    df[i,6]<-df[i,3]*df[i,4] 
  } 
  m<-sum(df[,6]) / sum(df[,5]) 
  c<-y.mean-m*x.mean 
  
  test.data<-data.frame(test.data,test.data[2]) 
  
  for(i in 1:nrow(test.data[1])){ 
    test.data[i,3]<-m*test.data[i,1]+c; 
  } 
  
  df.Rsq<-data.frame(test.data[,2],test.data[,3],test.data[,2],test.data[,3]) 
  
  for(i in 1:nrow(df.Rsq)){ 
    df.Rsq[i,3]<-(df.Rsq[i,2]-y.mean)^2 
    df.Rsq[i,4]<-(df.Rsq[i,1]-y.mean)^2 
  } 
  
  R.square<-sum(df.Rsq[3])/sum(df.Rsq[4]) 
  R.square 
  
  for(i in 1:nrow(test.data[2])){ 
    test.data[i,3]<-test.data[i,1]*m + c   
  } 
  plot(x=test.data[,1],y = test.data[,2]) 
  lines(x=test.data[,1],y = test.data[,3], col = "blue") 
  
  
  return(R.square) 
} 

#data preprocessing 
for(x in 2:ncol(weather.data)){ 
  colMean<-mean(weather.data[,x],na.rm = TRUE) 
  for(i in 1:nrow(weather.data[x])){ 
    if(weather.data[i,x] == 0){ 
      #      print(weather.data[i,x]) 
      weather.data[i,x]=colMean; 
    } 
  } 
} 

#removing outliers from the orginal data 
for(x in 2:ncol(weather.data)){ 
  boxplot(weather.data[x],horizontal=TRUE) 
  i<-1 
  Iqr1.5 = (1.5*IQR(weather.data[1:nrow(weather.data),x])) 
  Min=quantile(weather.data[1:nrow(weather.data),x],0.25) - Iqr1.5 
  Min 
  Max = quantile(weather.data[1:nrow(weather.data),x],0.75)+Iqr1.5 
  Max 
  while (i<=nrow(weather.data[x])) { 
    # print(weather.data[i,x]) 
    if(weather.data[i,x]<Min | weather.data[i,x]>Max){ 
      # print(data[i,x]) 
      weather.data<-weather.data[!(weather.data[x]==weather.data[i,x]),] 
      i<-i-1 
    } 
    i<-i+1 
  } 
  
  
} 
for (x in 2:ncol(weather.data)) { 
  # print(weather.data[x])  #boxplot after removing outliers 
  boxplot(weather.data[x],horizontal=TRUE) 
} 

#corelation 
for(i in 2:ncol(weather.data)){ 
  if(sd(weather.data[,i])==0){ 
    print(weather.data[i]) 
    weather.drop<-weather.data[i] 
  } 
} 

#weather.data<-weather.data[!weather.data %in% weather.drop] 
data_cor<-cor(weather.data[2:ncol(weather.data)],use="complete.obs") 
for(i in 1:ncol(data_cor)){ 
  for(j in i:ncol(data_cor)){ 
    if(i!=j){ 
      if(data_cor[i,j]>0.85 || data_cor[i,j]<(-0.85)){ 
        
        print(data_cor[i,j]) 
      } 
    } 
    
  } 
} 
corrplot(data_cor, method="circle") 
corrplot(data_cor, method="number") 
corrplot(data_cor, type="upper", order="hclust") 

weather.data<-subset(weather.data,select=-c(meanpressure)) 

#linear regression using inbiuilt function 
weather.test<-read.csv("DailyDelhiClimateTest.csv") 
x<-weather.test$meantemp 
y<-weather.test$humidity 
test.df<-data.frame(x,y) 

y<-weather.data$humidity 
x<-weather.data$meantemp 
weather.df<-data.frame(x,y) 

plot(x=weather.data$meantemp, y=weather.data$humidity, main="Humidity~Mean Temperature")  # scatterplot 
ggplot(weather.df,aes(x,y)) +
  geom_point()+
  geom_smooth(method='lm',formula ='y~x' , se=FALSE, color='turquoise4')+
  theme_minimal() +
  labs(x='Mean Temperature', y='Humidity', title='Linear Regression Plot') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))

weather.linearMod<-lm(y~x,data=weather.df) 
cat("Summary using Linear regression") 
print(weather.linearMod) 
summary(weather.linearMod) 
summary(weather.linearMod)$r.squared 
pred<-predict(weather.linearMod, test.df, interval = "confidence") 
plot(test.df) 
lines(y=pred[,1],x = test.df$x) 

ln.Rsq<-LinearRegression(weather.df[,1],weather.df[,2],test.df) 

#NON linear regression using inbuilt functions nls 

#for trainig data 
ggplot(weather.df, aes(x, y)) + 
  geom_point() + 
  stat_smooth(method = lm, se=FALSE,formula = y ~ poly(x, 5, raw = TRUE))+ 
  theme_minimal() + 
  labs(x='Mean Temperature', y='Wind Speed', title='Non-Linear Regression Plot') + theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 

weather.df.nls<-nls(y~(a*x^5 + b*x^4 + c*x^3 +d*x^2 +e*x +f),start = list(a = 5.095e-05,b =-6.327e-03,c= 2.929e-01,d =-6.267e+00,e =6.011e+01,f = -1.252e+02)) 
summary(weather.df.nls) 
modelr::rsquare(weather.df.nls,weather.df) 
pred<-predict(weather.df.nls,test.df, interval = "confidence")  
plot(test.df) 
lines(x = test.df[,1],y = pred) 



x<-weather.test$meantemp 
y<-weather.test$wind_speed 
test.df<-data.frame(x,y) 

x<-weather.data$meantemp 
y<-weather.data$wind_speed 
weather.df<-data.frame(x,y) 
plot(x=x, y=y, main="Humidity~Mean Temperature")  # scatterplot 
ggplot(weather.df,aes(x,y)) +
  geom_point()+
  geom_smooth(method='lm',formula ='y~x' , se=FALSE, color='turquoise4')+
  theme_minimal() +
  labs(x='Mean Temperature', y='Humidity', title='Linear Regression Plot') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))
weather.linearMod<-lm(y~x,data=weather.df) 
cat("Summary using Linear regression") 
print(weather.linearMod) 
summary(weather.linearMod) 
summary(weather.linearMod)$r.squared 

pred<-predict(weather.linearMod, test.df, interval = "confidence") 
plot(test.df) 
lines(y=pred[,1],x = test.df$x) 

ln.Rsq<-LinearRegression(weather.df[,1],weather.df[,2],test.df) 


ggplot(weather.df,aes(x,y)) + 
  geom_point()+ 
  geom_smooth(method='lm',formula ='y~x' , se=FALSE, color='turquoise4')+ 
  theme_minimal() + 
  labs(x='Mean Temperature', y='WindSpeed', title='Linear Regression Plot') + 
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))  

#NON linear regression using inbuilt functions nls 

#for trainig data 
ggplot(weather.df, aes(x, y)) + 
  geom_point() + 
  stat_smooth(method = lm, se=FALSE,formula = y ~ poly(x, 5, raw = TRUE))+ 
  theme_minimal() + 
  labs(x='Mean Temperature', y='Wind Speed', title='Non-Linear Regression Plot') + 
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 

weather.df.nls<-nls(y~(a*x^5 + b*x^4 + c*x^3 +d*x^2 +e*x +f), 
                    start = list(a =-3.351e-06  ,b =3.689e-04,c=-1.486e-02,d =2.707e-01  ,e =-2.066e+00,f =9.156e+00)) 
summary(weather.df.nls) 
modelr::rsquare(weather.df.nls,weather.df) 
pred<-predict(weather.df.nls,test.df, interval = "confidence")  
plot(test.df) 
lines(x = test.df[,1],y = pred)
