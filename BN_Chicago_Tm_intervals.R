"
Bayesian Network for Basketball Analytics

References: i) https://lib.dr.iastate.edu/cgi/viewcontent.cgi?article=7962&context=etd
            ii) https://www.basketball-reference.com/
"
library(bnlearn)
library(gRain)
library(Rgraphviz)
library(graph)
library(grid)
library(snow)

#merging basic and advanced stat

basic=read.csv('chicago_9798_basic_gamelog.csv')
advanced=read.csv('chicago_9798_advanced_gamelog.csv')
chicago_9798=merge(basic,advanced,by = 'Date')

#selecting subset of data for simple bayesian network
bn_to_discretise=chicago_9798[,c('Tm.x','FG_per','FT_per','TRB_per','TS_per')]
bn_to_discretise$Tm<-as.numeric(bn_to_discretise$Tm.x)
bn_discrete=discretize(bn_to_discretise[,-1], breaks = 5)
#bn_discrete$Tm=as.numeric(chicago_9798$Tm.x)

# expert BN (simple)
simple_expert_dag= model2network("[FG_per][FT_per][TRB_per][TS_per|FG_per:FT_per][Tm|TS_per:TRB_per]")
graphviz.plot(simple_expert_dag)
bnlearn::score(simple_expert_dag,data=bn_discrete) # default BIC
cpdag(simple_expert_dag)
vstructs(simple_expert_dag)
p<-arc.strength(simple_expert_dag,data=bn_discrete,criterion = "x2")
strength.plot(simple_expert_dag,
              strength = p )

# naive BN
naive_dag=model2network("[FG_per][FT_per][TRB_per][TS_per][Tm|TS_per:TRB_per:FG_per:FT_per]")
graphviz.plot(naive_dag)
bnlearn::score(naive_dag,data=bn_discrete)
p<-arc.strength(naive_dag,data=bn_discrete,criterion = "x2")
strength.plot(naive_dag,
              strength = p )

#computing accuracy of simple expert BN

fitted_expertBN<-bn.fit(simple_expert_dag,data=bn_discrete)
fitted_grain<-as.grain(fitted_expertBN)

team_points<-sort(unique(bn_discrete$Tm))

predictions<-matrix(NA,ncol=length(team_points),nrow=dim(bn_discrete)[1])
colnames(predictions)<-team_points

for (i in 1:dim(bn_discrete)[1]) {
  for (j in team_points) {
    
    
    bn_net<-setEvidence(compile(fitted_grain),nodes = colnames(bn_discrete)[-5],
                states = c(as.character(bn_discrete$FG_per[i]),
                           as.character(bn_discrete$FT_per[i]),
                           as.character(bn_discrete$TRB_per[i]),
                           as.character(bn_discrete$TS_per[i])))
    print(bn_net)
    #vector_E[j]<-pEvidence(bn_net)
    #full_vector_e<-as.data.frame(vector_E[team_points])
    #rownames(full_vector_e)<-team_points
    #colnames(full_vector_e)<-c('pEvidence')
    
    x<-querygrain(bn_net,'Tm')
    predictions[i,]=x$Tm
    
    #print(full_vector_e)
    }
}

pred<-apply(predictions,MARGIN = 1,FUN = which.max)

tb<-table(team_points[pred],bn_discrete$Tm)
cat('accuracy',sum(diag(tb))/sum(tb))

###########################################################################§2

'
Alternate Approach considering attempts(FGA,3PA,FTA),assists,block,steal,rebound 
'

##selecting subset of data for simple bayesian network
bn_to_discretise_attempts=chicago_9798[,c('Tm.x','FGA','FTA','X3PA','AST','TRB')]

bn_to_discretise_attempts=as.data.frame(apply(bn_to_discretise_attempts,MARGIN=2,FUN=as.numeric))

bn_discrete_attempts=discretize(bn_to_discretise_attempts[,-1], breaks = 5)
#bn_discrete_attempts$Tm=as.factor(chicago_9798$Tm.x)
str(bn_discrete_attempts)



# Expert BN-2 (simple)
simple_expert_dag2= model2network("[FTA][X3PA][AST][TRB][FGA|AST][Tm|FTA:X3PA:FGA:TRB]")
graphviz.plot(simple_expert_dag2)
bnlearn::score(simple_expert_dag2,data=bn_discrete_attempts) # default BIC
cpdag(simple_expert_dag2)
vstructs(simple_expert_dag2)

## Strength Plot
p<-arc.strength(simple_expert_dag2,data=bn_discrete_attempts,criterion = "x2")
strength.plot(simple_expert_dag2,
              strength = p )





# Computing accuracy of simple expert BN

fitted_expertBN2<-bn.fit(simple_expert_dag2,data=bn_discrete_attempts)
fitted_grain2<-as.grain(fitted_expertBN2)

team_points<-sort(unique(bn_discrete_attempts$Tm))



predictions<-matrix(NA,ncol=length(team_points),nrow=dim(bn_discrete_attempts)[1])
colnames(predictions)<-team_points

for (i in 1:dim(bn_discrete_attempts)[1]) {
  
  
  bnn_net<-setEvidence(compile(fitted_grain2),nodes = colnames(bn_discrete_attempts)[-6],
                       states = c(as.character(bn_discrete_attempts$FGA[i]),
                                  as.character(bn_discrete_attempts$FTA[i]),
                                  as.character(bn_discrete_attempts$X3PA[i]),
                                  as.character(bn_discrete_attempts$AST[i]),
                                  as.character(bn_discrete_attempts$TRB[i]))) 
  
  x<-querygrain(bnn_net,'Tm')
  predictions[i,]=x$Tm
  
  
}
pred<-apply(predictions,MARGIN = 1,FUN = which.max)

tb<-table(team_points[pred],bn_discrete_attempts$Tm)
print(tb)
cat('accuracy',sum(diag(tb))/sum(tb))


#################################################################################################

##selecting subset of data for simple bayesian network
bn_to_discretise_attempts_steal_block=chicago_9798[,c('Tm.x','FGA','FTA','Pace','AST','TRB','STL','BLK')]
bn_to_discretise_attempts_steal_block$STLBLK_sum<-bn_to_discretise_attempts_steal_block$STL +bn_to_discretise_attempts_steal_block$BLK
bn_to_discretise_attempts_steal_block= bn_to_discretise_attempts_steal_block[,-c(7,8)]
str(bn_to_discretise_attempts_steal_block)

bn_to_discretise_attempts_steal_block=as.data.frame(apply(bn_to_discretise_attempts_steal_block,MARGIN=2,FUN=as.numeric))

bn_discrete_attempts_steal_block=discretize(bn_to_discretise_attempts_steal_block[,-1], breaks = 5)
bn_discrete_attempts_steal_block$Tm=as.factor(chicago_9798$Tm.x)
str(bn_discrete_attempts_steal_block)


# Expert BN-3 (simple)

simple_expert_dag3= model2network("[FTA][STLBLK_sum][AST][TRB][FGA|AST][Pace|FTA:FGA:TRB][Tm|Pace:STLBLK_sum]")
graphviz.plot(simple_expert_dag3)
bnlearn::score(simple_expert_dag3,data=bn_discrete_attempts_steal_block) # default BIC
cpdag(simple_expert_dag3)
vstructs(simple_expert_dag3)

## Strength Plot
p<-arc.strength(simple_expert_dag3,data=bn_discrete_attempts_steal_block,criterion = "x2")
strength.plot(simple_expert_dag3,
              strength = p )

## fitting and grain
fitted_expertBN3<-bn.fit(simple_expert_dag3,data=bn_discrete_attempts_steal_block)
fitted_grain3<-as.grain(fitted_expertBN3)

team_points<-sort(unique(bn_discrete_attempts_steal_block$Tm))



predictions<-matrix(NA,ncol=length(team_points),nrow=dim(bn_discrete_attempts_steal_block)[1])
colnames(predictions)<-team_points

for (i in 1:dim(bn_discrete_attempts_steal_block)[1]) {
  
  
  bnn_net<-setEvidence(compile(fitted_grain3),nodes = colnames(bn_discrete_attempts_steal_block)[-7],
                       states = c(as.character(bn_discrete_attempts_steal_block$FGA[i]),
                                  as.character(bn_discrete_attempts_steal_block$FTA[i]),
                                  as.character(bn_discrete_attempts_steal_block$Pace[i]),
                                  as.character(bn_discrete_attempts_steal_block$AST[i]),
                                  as.character(bn_discrete_attempts_steal_block$TRB[i]),
                                  as.character(bn_discrete_attempts_steal_block$STLBLK_sum[i]) )) 
  
  x<-querygrain(bnn_net,'Tm')
  predictions[i,]=x$Tm
  
  
}
pred<-apply(predictions,MARGIN = 1,FUN = which.max)

tb<-table(team_points[pred],bn_discrete_attempts_steal_block$Tm)
print(tb)
cat('accuracy',sum(diag(tb))/sum(tb))




