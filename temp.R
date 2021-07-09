basic=read.csv('chicago_9798_basic_gamelog.csv')
advanced=read.csv('chicago_9798_advanced_gamelog.csv')
chicago_9798=merge(basic,advanced,by = 'Date')

bn_to_discretise_attempts=chicago_9798[,c('Tm.x','FGA','FTA','X3PA','AST','TRB')]

#bn_to_discretise_attempts=as.data.frame(apply(bn_to_discretise_attempts,MARGIN=2,FUN=as.factor))
bn_to_discretise_attempts$Tm<-as.numeric(bn_to_discretise_attempts$Tm.x)
bn_to_discretise_attempts$FGA<-as.numeric(bn_to_discretise_attempts$FGA)
bn_to_discretise_attempts$FTA<-as.numeric(bn_to_discretise_attempts$FTA)
bn_to_discretise_attempts$X3PA<-as.numeric(bn_to_discretise_attempts$X3PA)
bn_to_discretise_attempts$AST<-as.numeric(bn_to_discretise_attempts$AST)
bn_to_discretise_attempts$TRB<-as.numeric(bn_to_discretise_attempts$FGA)
bn_to_discretise_attempts<-bn_to_discretise_attempts[,-1]
scaled_df<-as.data.frame(scale(bn_to_discretise_attempts[,-6]))
scaled_df$Tm<-bn_to_discretise_attempts$Tm
bn_to_discretise_attempts=scaled_df



bn_discrete_attempts=discretize(bn_to_discretise_attempts, breaks = 5)
#bn_discrete_attempts$Tm=as.factor(bn_discrete_attempts$Tm.x)
#bn_discrete_attempts=bn_discrete_attempts[,-1]
str(bn_discrete_attempts)
typeof(bn_discrete_attempts)


# Expert BN-2 (simple)
simple_expert_dag2= model2network("[FTA][AST][TRB][FGA|AST][Tm|FTA:FGA:TRB]")
graphviz.plot(simple_expert_dag2)
bnlearn::score(simple_expert_dag2,data=as.data.frame(bn_discrete_attempts[,-3])) # default BIC
cpdag(simple_expert_dag2)
vstructs(simple_expert_dag2)

## Strength Plot
p<-arc.strength(simple_expert_dag2,data=bn_discrete_attempts,criterion = "x2")
strength.plot(simple_expert_dag2,
              strength = p )





# Computing accuracy of simple expert BN

fitted_expertBN2<-compile(as.grain(bn.fit(simple_expert_dag2,data=bn_discrete_attempts[,-3])))
fitted_expertBN2
