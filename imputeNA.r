removed.na<-data[complete.cases(data),]
unique.groups <- unique(removed.na[,3:14])








imputeNA <- function (r,groups,columns) {
  
  if (sum(is.na(r))>0) {
  
  row<-r[columns]
  
  nns<-apply(groups,1,function(x,y) {
    
    sum(y[!is.na(y)]!=x[!is.na(y)])
    
  },y = row)
  
  nn <- unique.groups[names(sort(nns)[1]),]
  
  row[is.na(row)]<-unlist(nn[is.na(row)])
  r[columns]<-row
  }
  r
  
}