	##########################  
	# R code for Miniproject2#  
	##########################  
	  
	#1.  
	#a. Create a bar graph of the variable Maine.  
	#import data from roadrace.csv  
	data=read.csv("C:/Users/lou/Desktop/R/R exercises/roadrace.csv",na.strings ="*" )  
	  
	#get data of numbers of Away and Maine group and transfer it to length.  
	datax=c(length(which(data$Maine=="Away")),length(which(data$Maine=="Maine")))  
	labels=c("Away","Maine")  
	  
	#make a barplot  
	barplot(datax,names.arg=labels,xlab="Approach",ylab="Number",col="blue",main="Summary chart",border="black")  
	  
	  
	#b.Create two histograms the runners' times (given in minutes), one for the Maine group and   
	# the minitues histgram for the Away group.  
	Away_Time=c(data$Time..minutes.[which(data$Maine=="Away")])  
	hist(Away_Time,  
	     xlab = "Runners time in minutes",  
	     main = "Histogram of time of runners away from Maine",  
	     xlim = c(20,160),  
	     ylim = c(0,2000))  
	  
	# compute statistic values of Away_Time  
	mean(Away_Time)  
	sd(Away_Time)  
	median(Away_Time)  
	range(Away_Time)  
	IQR(Away_Time)  
	  
	  
	  
	#the minitues histgram for the maine group.  
	Maine_Time=c(data$Time..minutes.[which(data$Maine=="Maine")])  
	hist(Maine_Time,  
	     xlab = "Runners time in minutes",  
	     main = "Histogram of time of runners from Maine",  
	     xlim = c(20,160),  
	     ylim = c(0,2000))  
	  
	# compute statistic values of Maine_Time  
	mean(Maine_Time)  
	sd(Maine_Time)  
	median(Maine_Time)  
	range(Maine_Time)  
	IQR(Maine_Time)  
	  
	#c.Create a side-by-side boxplot the runners' times (given in minutes), one for the Maine group and  
	#the second for the Away group.  
	boxplot(Time..minutes. ~ Maine,   
	        data=data,   
	        varwidth=TRUE,   
	        xlab="Approach",   
	        ylab="Time",   
	        main="Side by side Boxplot")  
	  
	#d. Create a box plot of Ages of female and male  
	boxplot(Age ~ Sex,   
	        data=data,   
	        varwidth=TRUE,   
	        xlab="Sex",   
	        ylab="Age",   
	        main="Age and sex boxplot")  
	#Summary statisic of Sex and Age  
	#Male age  
	Male_Age=c(data$Age[which(data$Sex=="M")])  
	mean(Male_Age)  
	sd(Male_Age)  
	median(Male_Age)  
	range(Male_Age)  
	IQR(Male_Age)  
	  
	#Female age  
	Female_Age=c(data$Age[which(data$Sex=="F")])  
	mean(Female_Age)  
	sd(Female_Age)  
	median(Female_Age)  
	range(Female_Age)  
	IQR(Female_Age)  
	  
	  
	#2.  
	#Create a boxplot of Motorcycle accident of counties in California.  
	  
	#import data from motorcycle.csv  
	data1 = read.csv("C:/Users/lou/Desktop/R/R exercises/motorcycle.csv")  
	AccidentCount = data1$Fatal.Motorcycle.Accidents  
	  
	#create a barplot of accident counts.  
	boxplot(AccidentCount,  
	        varwidth=TRUE,  
	        main="boxplot of motorcycle accidents in California Counties",  
	        ylab="Times")  
	  
	#summary statistics  
	  
	mean(AccidentCount)  
	sd(AccidentCount)  
	median(AccidentCount)  
	range(AccidentCount)  
	IQR(AccidentCount)
