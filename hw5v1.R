

setwd("C:/Users/lou/Desktop/R/R exercises")


#quetion 1

data_th=read.csv("bodytemp-heartrate.csv")

##4 qqplots
##temperature qqplots
body_t<-data_th$body_temperature
male_b<-subset(body_t,data_th$gender==1)
female_b<-subset(body_t,data_th$gender==2)
##male temperature
qqnorm(male_b)
qqline(male_b)
##female temperature
qqnorm(female_b)
qqline(female_b)
#heart rate qqplots
body_hr<-data_th$heart_rate
male_hr<-subset(body_hr,data_th$gender==1)
female_hr<-subset(body_hr,data_th$gender==2)
##male heart rate
qqnorm(male_hr)
qqline(male_hr)
##female heart rate
qqnorm(female_hr)
qqline(female_hr)

#(a)
data1=data_th$body_temperature[which(data_th$gender==1)]
data2=data_th$body_temperature[which(data_th$gender==2)]

boxplot(data1,data2)

tresult1=t.test(data1,data2,alternative="two.sided",mu=0)

#p-value=0.02394<0.05 reject null hypothesis

#(b)
data1=data_th$heart_rate[which(data_th$gender==1)]
data2=data_th$heart_rate[which(data_th$gender==2)]

boxplot(data1,data2)

tresult2=t.test(data1,data2,alternative="two.sided",mu=0)

#p-value=0.5287>0.05 can't reject null hypothesis

#(c)

#########(I)##############
#scatter plot xlab is temperature, ylab is heart rate.
plot(data_th$body_temperature,data_th$heart_rate,
     xlab = "temperature", 
     ylab = "heart rate",
     main = "scatter plot of togegher gender12 ")
abline(lm(data_th$heart_rate ~ data_th$body_temperature))
#summary correlation
corresult=cor.test(data_th$body_temperature,data_th$heart_rate,alternative="two.sided")
#cor=0.253654,p-value=0.003591 reject null hypothesis,rho>0,correlation exist.

#########(II)#############
#scatter plot xlab is temperature, ylab is heart rate.
plot(data_th$body_temperature[which(data_th$gender==1)],data_th$heart_rate[which(data_th$gender==1)],
     xlab = "temperature", 
     ylab = "heart rate",
     main = "scatter plot of male ")
abline(lm(data_th$heart_rate[which(data_th$gender==1)] ~ data_th$body_temperature[which(data_th$gender==1)]))
#summary correlation
corresult1=cor.test(data_th$body_temperature[which(data_th$gender==1)],data_th$heart_rate[which(data_th$gender==1)],alternative="two.sided")
#cor=0.1953894,p-value= can't reject null hypothesis, accept rho=0.

###########(III)###########
#scatter plot xlab is temperature, ylab is heart rate.
plot(data_th$body_temperature[which(data_th$gender==2)],data_th$heart_rate[which(data_th$gender==2)],
     xlab = "temperature", 
     ylab = "heart rate",
     main = "scatter plot of female ")
abline(lm(data_th$heart_rate[which(data_th$gender==2)] ~ data_th$body_temperature[which(data_th$gender==2)]))
#summary correlation
corresult2=cor.test(data_th$body_temperature[which(data_th$gender==2)],data_th$heart_rate[which(data_th$gender==2)],alternative="two.sided")
#cor=0.2869312,p-value=0.02048 reject null hypothesis,rho>0.


#question 2

#bootstrap function

#myboot=function(data,n){
  
#  len=length(data)
  
#  datanew=data.frame(x=c(1:len))
  
#  for (i in 1:n){
#    datanew=data.frame(datanew,x=data[ceiling(runif(len,0,len))])
#  }
#  #delete the not needed collumn
#  datanew=datanew[,-1]
  
#  return(datanew)
  
#}
#function of using bootstrap to compute CI
boot.conf.interval = function(x,n){
  lamda.star = 1/mean(x)
  mean.boot.dist = replicate(1000,mean(rexp(n,lamda.star)))
  ci = sort(mean.boot.dist)[c(25,975)]
  return(ci)
}

#(a)
#pareameter
n=30
lamda=0.1

#simulation
numprocess=5000

c1_sum=0
c2_sum=0

for (i in 1:numprocess){
  
  x=rexp(n,lamda)
  
  #confidence interval 1
  t1=t.test(x,alternative="two.sided")
  c1_low=1/t1$conf.int[2]
  c1_high=1/t1$conf.int[1]
  
  if ((lamda<c1_high)&(lamda>c1_low)){
    c1_sum=c1_sum+1
  }else{
    c1_sum=c1_sum
  }
  
  #confidence interval 2
  #datanew=myboot(data,1000)
  #data1=as.numeric(colMeans(datanew))
  #t2=t.test(data1,alternative="two.sided")
  #c2_low=1/t2$conf.int[2]
  #c2_high=1/t2$conf.int[1]
  
  c2 = boot.conf.interval(x,30)
  if ((1/lamda<c2[2])&(1/lamda>c2[1])){
    c2_sum=c2_sum+1
  }else{
    c2_sum=c2_sum
  }
  
  print(i)

}

Procover1=c1_sum/numprocess
Procover2=c2_sum/numprocess


#(b)
numprocess=200

n_list=c(5,10,30,100)
lamda_list=c(0.01,0.1,1,10)
##use two matrix to store result of probabilities
Procovermatrix1=matrix(0,nrow=length(n_list),ncol=length(lamda_list))
Procovermatrix2=matrix(0,nrow=length(n_list),ncol=length(lamda_list))

for (k1 in 1:length(n_list)){
  
  for (k2 in 1:length(lamda_list)){
    
    n=n_list[k1]
    lamda=lamda_list[k2]
    
    
    
    c1_sum=0
    c2_sum=0
    
    for (i in 1:numprocess){
      
      data=rexp(n,lamda)
      
      #confidence interval 1
      t1=t.test(data,alternative="two.sided")
      c1_low=1/t1$conf.int[2]
      c1_high=1/t1$conf.int[1]
      
      if ((lamda<c1_high)&(lamda>c1_low)){
        c1_sum=c1_sum+1
      }else{
        c1_sum=c1_sum
      }
      
      #confidence interval 2
      #datanew=myboot(data,100)
      #data1=as.numeric(colMeans(datanew))
      #t2=t.test(data1,alternative="two.sided")
      #c2_low=1/t2$conf.int[2]
      #c2_high=1/t2$conf.int[1]
      c2 = boot.conf.interval(data,n)
      if ((1/lamda<c2[2])&(1/lamda>c2[1])){
        c2_sum=c2_sum+1
      }else{
        c2_sum=c2_sum
      }
      
      print(i)
      
    }
    
    #confidence interval 1
    Procovermatrix1[k1,k2]=c1_sum/numprocess
    #confidence interval 2
    Procovermatrix2[k1,k2]=c2_sum/numprocess
    
  }
}

#(c)


















