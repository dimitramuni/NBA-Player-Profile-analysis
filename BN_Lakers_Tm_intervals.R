
'
Using LA Lakers gamelogs, seasons: 1999-00,2000-01,2001-02
Alternate Approach considering attempts(FGA,3PA,FTA),assists,block,steal,rebound 
'


Lakers_3yrs=read.csv('LALakers_3years.csv')
str(Lakers_3yrs)

##selecting subset of data for simple bayesian network
lakers_to_discretise_attempts=Lakers_3yrs[,c('Tm','FGA','FTA','X3PA','AST','TRB')]
str(lakers_to_discretise_attempts)

lakers_to_discretise_attempts=as.data.frame(apply(lakers_to_discretise_attempts,MARGIN=2,FUN=as.numeric))


lakers_discrete_attempts=discretize(lakers_to_discretise_attempts,method = 'hartemink')
#bn_discrete_attempts$Tm=as.factor(chicago_9798$Tm.x)
str(lakers_discrete_attempts)



# Expert BN-2 (simple)
simple_expert_dag2= model2network("[FTA][X3PA][AST][TRB][FGA|AST][Tm|FTA:X3PA:FGA:TRB]")
graphviz.plot(simple_expert_dag2)
bnlearn::score(simple_expert_dag2,data=lakers_discrete_attempts) # default BIC
cpdag(simple_expert_dag2)
vstructs(simple_expert_dag2)

## Strength Plot
p<-arc.strength(simple_expert_dag2,data=lakers_discrete_attempts,criterion = "x2")
strength.plot(simple_expert_dag2,
              strength = p )


# Computing accuracy of simple expert BN

ntrain<-203
ntest<-dim(Lakers_3yrs)[1]-ntrain


fitted_expertBN2<-bn.fit(simple_expert_dag2,data=lakers_discrete_attempts[1:ntrain,])
fitted_grain2<-as.grain(fitted_expertBN2)

team_points<-sort(unique(lakers_discrete_attempts$Tm))


### Traininig Accuracy

predictions_train<-matrix(NA,ncol=length(team_points),nrow=ntrain)
colnames(predictions_train)<-team_points

for (i in 1:dim(lakers_discrete_attempts[1:ntrain,])[1]) {
  
  
  bnn_net<-setEvidence(compile(fitted_grain2),nodes = colnames(lakers_discrete_attempts)[-1],
                       states = c(as.character(lakers_discrete_attempts$FGA[i]),
                                  as.character(lakers_discrete_attempts$FTA[i]),
                                  as.character(lakers_discrete_attempts$X3PA[i]),
                                  as.character(lakers_discrete_attempts$AST[i]),
                                  as.character(lakers_discrete_attempts$TRB[i]))) 
  
  x<-querygrain(bnn_net,'Tm')
  predictions_train[i,]=x$Tm
  
  
}
pred<-apply(predictions_train,MARGIN = 1,FUN = which.max)

tb<-table(team_points[pred],lakers_discrete_attempts[1:ntrain,]$Tm)
print(tb)
cat('accuracy',sum(diag(tb))/sum(tb))

##### Test Accuracy

predictions_test<-matrix(NA,ncol=length(team_points),nrow=ntest)
colnames(predictions_test)<-team_points

for (i in 1:dim(lakers_discrete_attempts[(ntrain+1):(ntrain+ntest),])[1]) {
  
  
  bnn_net<-setEvidence(compile(fitted_grain2),nodes = colnames(lakers_discrete_attempts)[-1],
                       states = c(as.character(lakers_discrete_attempts$FGA[i]),
                                  as.character(lakers_discrete_attempts$FTA[i]),
                                  as.character(lakers_discrete_attempts$X3PA[i]),
                                  as.character(lakers_discrete_attempts$AST[i]),
                                  as.character(lakers_discrete_attempts$TRB[i]))) 
  
  x<-querygrain(bnn_net,'Tm')
  predictions_test[i,]=x$Tm
  
  
}
pred<-apply(predictions_test,MARGIN = 1,FUN = which.max)

tb<-table(team_points[pred],lakers_discrete_attempts[(ntrain+1):(ntrain+ntest),]$Tm)
print(tb)
cat('accuracy',sum(diag(tb))/sum(tb))  
