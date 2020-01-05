



#Attempt igraph
#    plot(g2,layout=layout.fruchterman.reingold(g2),vertex.size=2,
 #        vertex.label=NA, edge.arrow.size=.025,vertex.color=DifficultyAttention)
#Edgelists
  #Rename edgelists
  Geph1.0<-get.edgelist(g0)
  Geph1.1<-get.edgelist(g1)
  Geph1.2<-get.edgelist(g2)
  Geph2.0<-get.edgelist(g10)
  Geph2.1<-get.edgelist(g11)
  Geph2.2<-get.edgelist(g12)
  #Reformat edgelists for Gephi
  GephiEdges1.0<-data.frame(Source=Geph1.0[,1], Target=Geph1.0[,2])
  GephiEdges1.1<-data.frame(Source=Geph1.1[,1], Target=Geph1.1[,2])
  GephiEdges1.2<-data.frame(Source=Geph1.2[,1], Target=Geph1.2[,2])
  GephiEdges2.0<-data.frame(Source=Geph2.0[,1], Target=Geph2.0[,2])
  GephiEdges2.1<-data.frame(Source=Geph2.1[,1], Target=Geph2.1[,2])
  GephiEdges2.2<-data.frame(Source=Geph2.2[,1], Target=Geph2.2[,2])
  #Write Edgelists to CSV
  write.csv(GephiEdges1.0, file="edges1.0.csv")
  write.csv(GephiEdges1.1, file="edges1.1.csv")
  write.csv(GephiEdges1.2, file="edges1.2.csv")
  write.csv(GephiEdges2.0, file="edges2.0.csv")
  write.csv(GephiEdges2.1, file="edges2.1.csv")
  write.csv(GephiEdges2.2, file="edges2.2.csv")
#Nodes covariates
  #Create a list of unique nodes
  GephiNodes1.0<-data.frame(ID=rownames(ADHDFriends58M0),DifficultyAttention=ADHDFriends58M0$DifficultyAttention)
  GephiNodes1.1<-data.frame(ID=rownames(ADHDFriends58M1),DifficultyAttention=ADHDFriends58M1$DifficultyAttention)
  GephiNodes1.2<-data.frame(ID=rownames(ADHDFriends58M2),DifficultyAttention=ADHDFriends58M2$DifficultyAttention, ChangeAttention=ADHDFriends58M2$DifficultyAttention-ADHDFriends58M0$DifficultyAttention, BinaryDifficultyAttention=ifelse(ADHDFriends58M2$DifficultyAttention>1,1,0), BinaryChangeAttention=ifelse(ADHDFriends58M2$DifficultyAttention-ADHDFriends58M0$DifficultyAttention>0,1,0), W1BinaryDifficultyAttention=ifelse(ADHDFriends58M0$DifficultyAttention>2,1,0))
  GephiNodes2.0<-data.frame(ID=rownames(ADHDFriends77M0),DifficultyAttention=ADHDFriends77M0$DifficultyAttention)
  GephiNodes2.1<-data.frame(ID=rownames(ADHDFriends77M1),DifficultyAttention=ADHDFriends77M1$DifficultyAttention)
  GephiNodes2.2<-data.frame(ID=rownames(ADHDFriends77M2),DifficultyAttention=ADHDFriends77M2$DifficultyAttention, ChangeAttention=ADHDFriends77M2$DifficultyAttention-ADHDFriends77M0$DifficultyAttention, BinaryDifficultyAttention=ifelse(ADHDFriends77M2$DifficultyAttention>1,1,0), BinaryChangeAttention=ifelse(ADHDFriends77M2$DifficultyAttention-ADHDFriends77M0$DifficultyAttention>0,1,0), W1BinaryDifficultyAttention=ifelse(ADHDFriends77M0$DifficultyAttention>2,1,0))
  #Write nodes to csv
  write.csv(GephiNodes1.0, file="nodes1.0.csv")
  write.csv(GephiNodes1.1, file="nodes1.1.csv")
  write.csv(GephiNodes1.2, file="nodes1.2.csv")
  write.csv(GephiNodes2.0, file="nodes2.0.csv")
  write.csv(GephiNodes2.1, file="nodes2.1.csv")
  write.csv(GephiNodes2.2, file="nodes2.2.csv")
  
  #table(GephiNodes2.2$DifficultyAttention)
  #table(GephiNodes1.2$DifficultyAttention)
  #table(GephiNodes2.2$ChangeAttention)
  #table(GephiNodes1.2$ChangeAttention)
  
#Magnifier affect
  mean(GephiNodes1.1$DifficultyAttention, na.rm=TRUE)
  mean(GephiNodes2.1$DifficultyAttention, na.rm=TRUE)
  mean(GephiNodes1.2$ChangeAttention, na.rm=TRUE)
  mean(GephiNodes2.2$ChangeAttention, na.rm=TRUE)
  
  