simulation<-function(signature1,signature2,a1,b2){
	setwd("/Users/Yixuan/Documents/PARK/simulations")
	df <- read.csv("PCAWG_signature_patterns_beta2.csv", header=TRUE)
	number <- sample (1:96,1, replace = TRUE)
	dice <- 1
	sig <- 0
	a <- 0
	b <- 0
	while (dice >= sig) {
		dice <- runif (1)
		number <- sample (1:96,1, replace = TRUE)
		sig <- df[number,signature1]
		t <- df[number,1] #type
	 	s <- df[number,2] #subtype
 		n <- df[number,3] #mutation number
		if (dice<=sig){
			type1<-c(as.character(t))
			subtype1<-c(as.character(s))
			number1<-c(as.character(n))
			a <- a+1}
	}
	while (a<a1) {
		number <- sample (1:96,1, replace = TRUE)
 		dice <- runif (1)
 		sig <- df[number,signature1] #signature 3
 		if (dice<sig){
 			a<-a+1
 			t <- df[number,1] 
 			s <- df[number,2]
 			n <- df[number,3]
 			type<-c(as.character(t))
			subtype<-c(as.character(s))
			number<-c(as.character(n))
 			type1<-append(type1,type)
 			subtype1<-append(subtype1,subtype)
 			number1 <-append(number1,number)}
	}
	number <- sample (1:96,1, replace = TRUE)
	dice <- 1
	sig <- 0
	while (dice >= sig) {
		number <- sample (1:96,1, replace = TRUE)
		dice <- runif (1)
		sig <- df[number,signature2] #signature 8
		t <- df[number,1] #type
	 	s <- df[number,2] #subtype
	 	n <- df[number,3] #mutation number
		if (dice<=sig){
			type17<-c(as.character(t))
			subtype17<-c(as.character(s))
			number17<-c(as.character(n))
			b <- b+1}
	}
	while (b<b2) {
		number <- sample (1:96,1, replace = TRUE)
	 	dice <- runif (1)
	 	sig <- df[number,signature2] #signature 8
	 	if (dice<sig){
 			b<-b+1
 			t <- df[number,1] 
 			s <- df[number,2]
 			n <- df[number,3] 		
 			type<-c(as.character(t))
			subtype<-c(as.character(s))
			number<-c(as.character(n))
 			type17<-append(type17,type)
 			subtype17<-append(subtype17,subtype)
  			number17 <-append(number17,number)}
	}
	sig1<-cbind(type1,subtype1,number1)
	sig17<-cbind(type17,subtype17,number17)

	rows.sig1 <-nrow(sig1)
	rows.sig17 <-nrow(sig17)
	if (rows.sig1>=rows.sig17) {
		diff <- rows.sig1-rows.sig17
		df.na <-matrix(NA,diff,ncol(sig17))
		colnames(df.na) <-colnames(signature1)
		k<-cbind(sig1,rbind(sig17,df.na))
	}
	rows.sig1 <-nrow(sig1)
	rows.sig17 <-nrow(sig17)
	if(rows.sig17>rows.sig1){
		diff<-rows.sig17-rows.sig1
		df.na<-matrix(NA,diff,ncol(sig1))
		colnames(df.na)<-colnames(signature2)
		k<-cbind(rbind(sig1,df.na),sig17)
	}
	write.csv(k, paste("sig",as.character(signature1),"_",as.character(a1),"_sig",as.character(signature2),"_",as.character(b2),".csv"))}

#simulation(4,24,100000,100000)
#simulation(4,24,80000,120000)
#simulation(4,24,60000,140000)
#simulation(4,24,40000,160000)
#simulation(4,24,20000,180000)
#simulation(4,24,0000,200000)
#simulation(4,24,120000,80000)
#simulation(4,24,140000,60000)
#simulation(4,24,160000,40000)
#simulation(4,24,180000,20000)
#simulation(4,24,200000,00000)
#simulation(6,14,100000,100000)
#simulation(6,14,100000,100000)
#simulation(6,14,80000,120000)
#simulation(6,14,60000,140000)
#simulation(6,14,40000,160000)
#simulation(6,14,20000,180000)
#simulation(6,14,0000,200000)
#simulation(6,14,120000,80000)
#simulation(6,14,140000,60000)
#simulation(6,14,160000,40000)
#simulation(6,14,180000,20000)
#simulation(6,14,200000,00000)
