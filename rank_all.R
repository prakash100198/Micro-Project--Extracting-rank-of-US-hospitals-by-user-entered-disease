rankall<-function(outcome,num="best"){
  if(num=="best"){ num<-1}
  rdng_csv<-read.csv("outcome-of-care-measures.csv",header = T,stringsAsFactors = F)

  if(outcome=="heart attack"){ a<-11} 
  else if(outcome=="heart failure"){ a<-17} 
  else if(outcome=="pneumonia"){ a<-23} 
  else{stop("invalid outcome")}
 
  #part1 
  factor_list<-levels(as.factor(rdng_csv[,7]))
  subset_data_frame<-list() 
  num1<-numeric(length(factor_list))
  
  for(i in 1:length(factor_list)){
    subset_data_frame[[i]]<-subset(rdng_csv,rdng_csv$State==factor_list[i])
    subset_data_frame[[i]]<-subset_data_frame[[i]][order(as.numeric(subset_data_frame[[i]][,a]),subset_data_frame[[i]][,2]),c(2,7,a)]
    
    subset_data_frame[[i]]<-subset_data_frame[[i]][complete.cases(as.numeric(subset_data_frame[[i]][,3])),]
    num1[i]<-nrow(subset_data_frame[[i]])
    }
  
  #part2
   final_data_frame<-data.frame(Hospital.Name=character(length(factor_list)),State=character(length(factor_list)),stringsAsFactors = F)   
   for(i in 1:length(factor_list)){  
 if(num=="worst")
    {final_data_frame$Hospital.Name[i]<-subset_data_frame[[i]][,1][num1[i]]}
    else
    {final_data_frame$Hospital.Name[i]<-subset_data_frame[[i]][,1][num]}
    
    final_data_frame$State[i]<-factor_list[i]
  }
  
  
  
  return(final_data_frame)
}
