library(rbenchmark)


a=500
b=700

df<- as.data.frame(matrix(seq_len(a*b), nrow=a))
df
ft<-function(x){
  return(var(x))
}

f1<-function(df){
  return(lapply(df,ft))
}
res<-as.list(rep(1,b))
f2<-function(df,res){
  for(i in 1:b){
    res[i]<-ft(df[,i])
  }
  return(res)
}


ben<-benchmark(f1(df),f2(df,res),replications=1039,
               columns=c('test','relative','elapsed','user.self','sys.self'))

ben