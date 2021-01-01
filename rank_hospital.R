rankhospital <- function(state, outcome, num = "best") {
  if(num=="best"){ num<-1}
  rdng_csv<-read.csv("outcome-of-care-measures.csv",header = T,stringsAsFactors = F)
  st<-character(length = nrow(rdng_csv))
  for(i in 1:nrow(rdng_csv)){
    st[i]<-state
  }
  new_data_frame<-data.frame(st,rdng_csv$State,stringsAsFactors = F)
  comp<-new_data_frame[,1]==new_data_frame[,2]
  if(all(comp==FALSE)){stop("invalid state")}
  
  if(outcome=="heart attack"){
    new_set1<-subset(rdng_csv,rdng_csv[,7]==state)
     arr_new_set1<-new_set1[order(as.numeric(new_set1[,11]),new_set1[,2]),c(2,11)]
     without_nas_new_set1<-arr_new_set1[complete.cases(as.numeric(arr_new_set1[,2])),]
     if(num=="worst"){num<-nrow(without_nas_new_set1)}
     if(num>nrow(without_nas_new_set1)){return(NA)
                                            stop()}
     
     return(without_nas_new_set1[,1][num])
  }
  
  if(outcome=="heart failure"){
    new_set2<-subset(rdng_csv,rdng_csv[,7]==state)
    arr_new_set2<-new_set2[order(as.numeric(new_set2[,17]),new_set2[,2]),c(2,17)]
    without_nas_new_set2<-arr_new_set2[complete.cases(as.numeric(arr_new_set2[,2])),]
    if(num=="worst"){num<-nrow(without_nas_new_set2)}
    if(num>nrow(without_nas_new_set2)){return(NA)
                                           stop()}
    return(without_nas_new_set2[,1][num])
  }
  
  if(outcome=="pneumonia"){
    new_set3<-subset(rdng_csv,rdng_csv[,7]==state)
    arr_new_set3<-new_set3[order(as.numeric(new_set3[,23]),new_set3[,2]),c(2,23)]
    without_nas_new_set3<-arr_new_set3[complete.cases(as.numeric(arr_new_set3[,2])),]
    if(num=="worst"){num<-nrow(without_nas_new_set3)}
    if(num>nrow(without_nas_new_set3)){return(NA)
                                            stop()}
    return(without_nas_new_set3[,1][num])
  }
  
  else if(outcome!="heart attack"||outcome!="heart failure"||outcome!="pneumonia"){
    stop("invalid outcome")
  }
}

