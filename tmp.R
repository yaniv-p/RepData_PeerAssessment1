m<-matrix(nrow = 17568,ncol = 2)
for (i in seq_along(MyDFNew$steps))
  { 
    m[i,1]=MyDFNew[i,1]
    if(is.na(MyDFNew[i,1]))
      {
      print(i); 
      print(MyDF_groupNew[i,2])
      m[i,2]=as.integer(MyDF_groupNew[i,2])
      } 
    else
      {
      print(i);
      print(MyDFNew[i,1])
      m[i,2]=MyDFNew[i,1]
      }
}
