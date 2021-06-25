#0-1 Knapsack's Problem

library(GA)

item=c('raincoat','pocket knife','mineral water','gloves','sleeping bag','tent','portable stove','canned food','snacks')

weight=c(2,1,6,1,4,9,5,8,3)

survival=c(5,3,15,5,6,18,8,20,8)

data=data.frame(item,weight,survival)

max_weight=25


#1 means that we bring the item, while 0 means that we left the item

chromosomes=c(0,1,1,0,0,0,0,0,1)

data[chromosomes==1,]

#create the function that we want to optimize

fitness=function(x){
  
  current_survpoint=x%*%data$survival
  current_weight=x%*%data$weight
  
  if(current_weight>max_weight){
    return(0)
  }
  else{
    return(current_survpoint)
  }
}

GA=ga(type='binary',fitness=fitness,nBits=nrow(data),
      maxiter=30,popSize=50,seed=1234,keepBest=TRUE)

summary(GA)

plot(GA)

GA2=ga(type='binary',fitness=fitness,nBits=nrow(data),maxiter=50,popSize=50,seed=1234,keepBest=TRUE)

GA3=ga(type='binary',fitness=fitness,nBits=nrow(data),maxiter=100,popSize=50,seed=1234,keepBest=TRUE)

plot(GA2)

summary(GA2)

plot(GA3)

summary(GA3)
