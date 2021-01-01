best<-function(state,outcome){
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
    col_to_num<-as.numeric(new_set1[,11])
    list_best_hos1<-new_set1$Hospital.Name[col_to_num==min(col_to_num,na.rm = T)]
  sort_list1<-sort(list_best_hos1)
  return(sort_list1[1])
  }
  
  else if(outcome=="heart failure"){
    new_set2<-subset(rdng_csv,rdng_csv[,7]==state)
    col_to_num<-as.numeric(new_set2[,17])
    list_best_hos2<-new_set2$Hospital.Name[col_to_num==min(col_to_num,na.rm=T)]
    sort_list2<-sort(list_best_hos2)
    return(sort_list2[1])
    }
  
  else if(outcome=="pneumonia"){
    new_set3<-subset(rdng_csv,rdng_csv[,7]==state)
    col_to_num<-as.numeric(new_set3[,23])
    list_best_hos3<-new_set3$Hospital.Name[col_to_num==min(col_to_num,na.rm=T)]
    sort_list3<-sort(list_best_hos3)
    return(sort_list3[1])
    }
  
  else if(outcome!="heart attack"||outcome!="heart failure"||outcome!="pneumonia"){
    stop("invalid outcome")
  }
 

}
