
#School 58 
  #Wave 1-2
    #Restrict data to saturated schools
      ADHDFriends58M1<-ADHDFriends[ADHDFriends$scid==58,]
      ADHDFriends58M0<-ADHDFriends0[ADHDFriends0$scid==58,]
    #Make both files have the same participants and create a flag to indicate which were not originally there
      #Unique aids
        MasterID0<-data.frame(aid=ADHDFriends58M0$aid)
        MasterID1<-data.frame(aid=ADHDFriends58M1$aid)
        MasterID<-unique(rbind(MasterID0,MasterID1))
        ADHDFriends58M1$Missing<-0
        ADHDFriends58M0$Missing<-0
        ADHDFriends58M1<-merge(ADHDFriends58M1,MasterID, by="aid", all.y=TRUE)
        ADHDFriends58M0<-merge(ADHDFriends58M0,MasterID, by="aid", all.y=TRUE)
        ADHDFriends58M1$Missing<-ifelse(is.na(ADHDFriends58M1$Missing),1,0)
        ADHDFriends58M0$Missing<-ifelse(is.na(ADHDFriends58M0$Missing),1,0)
    #Sort by aid
      ADHDFriends58M1<-ADHDFriends58M1[order(ADHDFriends58M1$aid),]
      ADHDFriends58M0<-ADHDFriends58M0[order(ADHDFriends58M0$aid),]
    #Remove sent ties that are not in AID
      ADHDFriends58M1<-transform(ADHDFriends58M1,
                             MF_AID1=ifelse(MF_AID1 %in% aid, MF_AID1, NA),
                             MF_AID2=ifelse(MF_AID2 %in% aid, MF_AID2, NA),
                             MF_AID3=ifelse(MF_AID3 %in% aid, MF_AID3, NA),
                             MF_AID4=ifelse(MF_AID4 %in% aid, MF_AID4, NA),
                             MF_AID5=ifelse(MF_AID5 %in% aid, MF_AID5, NA),
                             FF_AID1=ifelse(FF_AID1 %in% aid, FF_AID1, NA),
                             FF_AID2=ifelse(FF_AID2 %in% aid, FF_AID2, NA),
                             FF_AID3=ifelse(FF_AID3 %in% aid, FF_AID3, NA),
                             FF_AID4=ifelse(FF_AID4 %in% aid, FF_AID4, NA),
                             FF_AID5=ifelse(FF_AID5 %in% aid, FF_AID5, NA)
      )
      ADHDFriends58M0<-transform(ADHDFriends58M0,
                             MF_AID1=ifelse(MF1AID %in% aid, MF1AID, NA),
                             MF_AID2=ifelse(MF2AID %in% aid, MF2AID, NA),
                             MF_AID3=ifelse(MF3AID %in% aid, MF3AID, NA),
                             MF_AID4=ifelse(MF4AID %in% aid, MF4AID, NA),
                             MF_AID5=ifelse(MF5AID %in% aid, MF5AID, NA),
                             FF_AID1=ifelse(FF1AID %in% aid, FF1AID, NA),
                             FF_AID2=ifelse(FF2AID %in% aid, FF2AID, NA),
                             FF_AID3=ifelse(FF3AID %in% aid, FF3AID, NA),
                             FF_AID4=ifelse(FF4AID %in% aid, FF4AID, NA),
                             FF_AID5=ifelse(FF5AID %in% aid, FF5AID, NA)
      )
    #Remove complete isolates
      ADHDFriends58M0$NoFriends<-ifelse(is.na(ADHDFriends58M0$MF_AID1) & is.na(ADHDFriends58M0$MF_AID2) & is.na(ADHDFriends58M0$MF_AID3) & is.na(ADHDFriends58M0$MF_AID4) & is.na(ADHDFriends58M0$MF_AID5) & is.na(ADHDFriends58M0$FF_AID1) & is.na(ADHDFriends58M0$FF_AID2) & is.na(ADHDFriends58M0$FF_AID3) & is.na(ADHDFriends58M0$FF_AID4) & is.na(ADHDFriends58M0$FF_AID5),1,0)
      ADHDFriends58M1$NoFriends<-ifelse(is.na(ADHDFriends58M1$MF_AID1) & is.na(ADHDFriends58M1$MF_AID2) & is.na(ADHDFriends58M1$MF_AID3) & is.na(ADHDFriends58M1$MF_AID4) & is.na(ADHDFriends58M1$MF_AID5) & is.na(ADHDFriends58M1$FF_AID1) & is.na(ADHDFriends58M1$FF_AID2) & is.na(ADHDFriends58M1$FF_AID3) & is.na(ADHDFriends58M1$FF_AID4) & is.na(ADHDFriends58M1$FF_AID5),1,0)
      ADHDFriends58M0$NotFriended<-ifelse((ADHDFriends58M0$aid %in% ADHDFriends58M0$MF_AID1) | (ADHDFriends58M0$aid %in% ADHDFriends58M0$MF_AID2) | (ADHDFriends58M0$aid %in% ADHDFriends58M0$MF_AID3) | (ADHDFriends58M0$aid %in% ADHDFriends58M0$MF_AID4) | (ADHDFriends58M0$aid %in% ADHDFriends58M0$MF_AID5) | (ADHDFriends58M0$aid %in% ADHDFriends58M0$FF_AID1) | (ADHDFriends58M0$aid %in% ADHDFriends58M0$FF_AID2) | (ADHDFriends58M0$aid %in% ADHDFriends58M0$FF_AID3) | (ADHDFriends58M0$aid %in% ADHDFriends58M0$FF_AID4) | (ADHDFriends58M0$aid %in% ADHDFriends58M0$FF_AID5),0,1)
      ADHDFriends58M1$NotFriended<-ifelse((ADHDFriends58M1$aid %in% ADHDFriends58M1$MF_AID1) | (ADHDFriends58M1$aid %in% ADHDFriends58M1$MF_AID2) | (ADHDFriends58M1$aid %in% ADHDFriends58M1$MF_AID3) | (ADHDFriends58M1$aid %in% ADHDFriends58M1$MF_AID4) | (ADHDFriends58M1$aid %in% ADHDFriends58M1$MF_AID5) | (ADHDFriends58M1$aid %in% ADHDFriends58M1$FF_AID1) | (ADHDFriends58M1$aid %in% ADHDFriends58M1$FF_AID2) | (ADHDFriends58M1$aid %in% ADHDFriends58M1$FF_AID3) | (ADHDFriends58M1$aid %in% ADHDFriends58M1$FF_AID4) | (ADHDFriends58M1$aid %in% ADHDFriends58M1$FF_AID5),0,1)
      ADHDFriends58M0$Isolate<-ifelse(ADHDFriends58M0$NoFriends==1 & ADHDFriends58M1$NoFriends==1 & ADHDFriends58M0$NotFriended==1 & ADHDFriends58M1$NotFriended==1,1,0)
      ADHDFriends58M1$Isolate<-ifelse(ADHDFriends58M0$NoFriends==1 & ADHDFriends58M1$NoFriends==1 & ADHDFriends58M0$NotFriended==1 & ADHDFriends58M1$NotFriended==1,1,0)
      ADHDFriends58M0<-ADHDFriends58M0[ADHDFriends58M0$Isolate!=1,]
      ADHDFriends58M1<-ADHDFriends58M1[ADHDFriends58M1$Isolate!=1,]
    #Create Friendship Matrices
      #Reset row names
        rownames(ADHDFriends58M1)<-NULL
        rownames(ADHDFriends58M0)<-NULL
      #Extract Friendship data
        Friends58M1<-as.matrix(ADHDFriends58M1[,c("aid", "MF_AID1","MF_AID2","MF_AID3","MF_AID4","MF_AID5","FF_AID1","FF_AID2","FF_AID3","FF_AID4","FF_AID5")])
        Friends58M0<-as.matrix(ADHDFriends58M0[,c("aid", "MF_AID1","MF_AID2","MF_AID3","MF_AID4","MF_AID5","FF_AID1","FF_AID2","FF_AID3","FF_AID4","FF_AID5")])
      #Create edge list
        Friends58M11<-cbind(Friends58M1[,1], c(Friends58M1[,-1]))
        Friends58M00<-cbind(Friends58M0[,1], c(Friends58M0[,-1]))
      #Make each person tied to themselves (to make models work)
        uniqueaid<-unique(Friends58M11[,1])
        uniqueaid_df<-data.frame(V1=uniqueaid, V2=uniqueaid)
        Friends58M11<-rbind(Friends58M11,uniqueaid_df)
        uniqueaid0<-unique(Friends58M00[,1])
        uniqueaid_df0<-data.frame(V1=uniqueaid0, V2=uniqueaid0)
        Friends58M00<-rbind(Friends58M00,uniqueaid_df0)
      #Drop Missing ties
        Friends58M11<-Friends58M11[!(is.na(Friends58M11$V2)),]
        Friends58M00<-Friends58M00[!(is.na(Friends58M00$V2)),]
      #Create adjacency matrix from edge list
        #S58W1
            Friends58M11<-Friends58M11[order(Friends58M11$V1),]
            rownames(Friends58M11)<-NULL
            g=graph.data.frame(Friends58M11, directed=TRUE)
            g1<-simplify(g)
            S58FW1<-get.adjacency(g1,sparse=FALSE)
        #S58W0
            Friends58M00<-Friends58M00[order(Friends58M00$V1),]
            rownames(Friends58M00)<-NULL
            g0=graph.data.frame(Friends58M00, directed=TRUE)
            g0<-simplify(g0)
            S58FW0<-get.adjacency(g0,sparse=FALSE)
        
  #Wave 2-3
    #Restrict data to saturated schools
      ADHDFriends58ML1<-ADHDFriends[ADHDFriends$scid==58,]
      ADHDFriends58M2<-ADHDFriends2[ADHDFriends2$scid==58,]
    #Make both files have the same participants and create a flag to indicate which were not originally there
      #Unique aids
        MasterID2<-data.frame(aid=ADHDFriends58M2$aid)
        MasterID1<-data.frame(aid=ADHDFriends58ML1$aid)
        MasterID<-NULL
        MasterID<-unique(rbind(MasterID2,MasterID1))
        ADHDFriends58ML1$Missing<-0
        ADHDFriends58M2$Missing<-0
        ADHDFriends58ML1<-merge(ADHDFriends58ML1,MasterID, by="aid", all.y=TRUE)
        ADHDFriends58M2<-merge(ADHDFriends58M2,MasterID, by="aid", all.y=TRUE)
        ADHDFriends58ML1$Missing<-ifelse(is.na(ADHDFriends58ML1$Missing),1,0)
        ADHDFriends58M2$Missing<-ifelse(is.na(ADHDFriends58M2$Missing),1,0)
      #Sort by aid
        ADHDFriends58ML1<-ADHDFriends58ML1[order(ADHDFriends58ML1$aid),]
        ADHDFriends58M2<-ADHDFriends58M2[order(ADHDFriends58M2$aid),]
      #Remove sent ties that are not in AID
        ADHDFriends58ML1<-transform(ADHDFriends58ML1,
                                   MF_AID1=ifelse(MF_AID1 %in% aid, MF_AID1, NA),
                                       MF_AID2=ifelse(MF_AID2 %in% aid, MF_AID2, NA),
                                       MF_AID3=ifelse(MF_AID3 %in% aid, MF_AID3, NA),
                                       MF_AID4=ifelse(MF_AID4 %in% aid, MF_AID4, NA),
                                       MF_AID5=ifelse(MF_AID5 %in% aid, MF_AID5, NA),
                                       FF_AID1=ifelse(FF_AID1 %in% aid, FF_AID1, NA),
                                       FF_AID2=ifelse(FF_AID2 %in% aid, FF_AID2, NA),
                                       FF_AID3=ifelse(FF_AID3 %in% aid, FF_AID3, NA),
                                       FF_AID4=ifelse(FF_AID4 %in% aid, FF_AID4, NA),
                                       FF_AID5=ifelse(FF_AID5 %in% aid, FF_AID5, NA)
            )
            ADHDFriends58M2<-transform(ADHDFriends58M2,
                                       MF_AID1=ifelse(MF_AID1 %in% aid, MF_AID1, NA),
                                       MF_AID2=ifelse(MF_AID2 %in% aid, MF_AID2, NA),
                                       MF_AID3=ifelse(MF_AID3 %in% aid, MF_AID3, NA),
                                       MF_AID4=ifelse(MF_AID4 %in% aid, MF_AID4, NA),
                                       MF_AID5=ifelse(MF_AID5 %in% aid, MF_AID5, NA),
                                       FF_AID1=ifelse(FF_AID1 %in% aid, FF_AID1, NA),
                                       FF_AID2=ifelse(FF_AID2 %in% aid, FF_AID2, NA),
                                       FF_AID3=ifelse(FF_AID3 %in% aid, FF_AID3, NA),
                                       FF_AID4=ifelse(FF_AID4 %in% aid, FF_AID4, NA),
                                       FF_AID5=ifelse(FF_AID5 %in% aid, FF_AID5, NA)
            )
          #Remove complete isolates
            ADHDFriends58M2$NoFriends<-ifelse(is.na( ADHDFriends58M2$MF_AID1) & is.na( ADHDFriends58M2$MF_AID2) & is.na(ADHDFriends58M2$MF_AID3) & is.na(ADHDFriends58M2$MF_AID4) & is.na(ADHDFriends58M2$MF_AID5) & is.na(ADHDFriends58M2$FF_AID1) & is.na(ADHDFriends58M2$FF_AID2) & is.na(ADHDFriends58M2$FF_AID3) & is.na(ADHDFriends58M2$FF_AID4) & is.na(ADHDFriends58M2$FF_AID5),1,0)
            ADHDFriends58ML1$NoFriends<-ifelse(is.na(ADHDFriends58ML1$MF_AID1) & is.na(ADHDFriends58ML1$MF_AID2) & is.na(ADHDFriends58ML1$MF_AID3) & is.na(ADHDFriends58ML1$MF_AID4) & is.na(ADHDFriends58ML1$MF_AID5) & is.na(ADHDFriends58ML1$FF_AID1) & is.na(ADHDFriends58ML1$FF_AID2) & is.na(ADHDFriends58ML1$FF_AID3) & is.na(ADHDFriends58ML1$FF_AID4) & is.na(ADHDFriends58ML1$FF_AID5),1,0)
            ADHDFriends58M2$NotFriended<-ifelse((ADHDFriends58M2$aid %in% ADHDFriends58M2$MF_AID1) | (ADHDFriends58M2$aid %in% ADHDFriends58M2$MF_AID2) | (ADHDFriends58M2$aid %in% ADHDFriends58M2$MF_AID3) | (ADHDFriends58M2$aid %in% ADHDFriends58M2$MF_AID4) | (ADHDFriends58M2$aid %in% ADHDFriends58M2$MF_AID5) | (ADHDFriends58M2$aid %in% ADHDFriends58M2$FF_AID1) | (ADHDFriends58M2$aid %in% ADHDFriends58M2$FF_AID2) | (ADHDFriends58M2$aid %in% ADHDFriends58M2$FF_AID3) | (ADHDFriends58M2$aid %in% ADHDFriends58M2$FF_AID4) | (ADHDFriends58M2$aid %in% ADHDFriends58M2$FF_AID5),0,1)
            ADHDFriends58ML1$NotFriended<-ifelse((ADHDFriends58ML1$aid %in% ADHDFriends58ML1$MF_AID1) | (ADHDFriends58ML1$aid %in% ADHDFriends58ML1$MF_AID2) | (ADHDFriends58ML1$aid %in% ADHDFriends58ML1$MF_AID3) | (ADHDFriends58ML1$aid %in% ADHDFriends58ML1$MF_AID4) | (ADHDFriends58ML1$aid %in% ADHDFriends58ML1$MF_AID5) | (ADHDFriends58ML1$aid %in% ADHDFriends58ML1$FF_AID1) | (ADHDFriends58ML1$aid %in% ADHDFriends58ML1$FF_AID2) | (ADHDFriends58ML1$aid %in% ADHDFriends58ML1$FF_AID3) | (ADHDFriends58ML1$aid %in% ADHDFriends58ML1$FF_AID4) | (ADHDFriends58ML1$aid %in% ADHDFriends58ML1$FF_AID5),0,1)
            ADHDFriends58M2$Isolate<-ifelse(ADHDFriends58M2$NoFriends==1 & ADHDFriends58ML1$NoFriends==1 & ADHDFriends58M2$NotFriended==1 & ADHDFriends58ML1$NotFriended==1,1,0)
            ADHDFriends58ML1$Isolate<-ifelse(ADHDFriends58M2$NoFriends==1 & ADHDFriends58ML1$NoFriends==1 & ADHDFriends58M2$NotFriended==1 & ADHDFriends58ML1$NotFriended==1,1,0)
            ADHDFriends58M2<-ADHDFriends58M2[ADHDFriends58M2$Isolate!=1,]
            ADHDFriends58ML1<-ADHDFriends58ML1[ADHDFriends58ML1$Isolate!=1,]
            #Create Friendship Matrices
            #Reset row names
            rownames(ADHDFriends58ML1)<-NULL
            rownames(ADHDFriends58M2)<-NULL
            #Extract Friendship data
            Friends58M1<-as.matrix(ADHDFriends58ML1[,c("aid", "MF_AID1","MF_AID2","MF_AID3","MF_AID4","MF_AID5","FF_AID1","FF_AID2","FF_AID3","FF_AID4","FF_AID5")])
            Friends58M2<-as.matrix(ADHDFriends58M2[,c("aid", "MF_AID1","MF_AID2","MF_AID3","MF_AID4","MF_AID5","FF_AID1","FF_AID2","FF_AID3","FF_AID4","FF_AID5")])
            #Create edge list
            Friends58M11<-cbind(Friends58M1[,1], c(Friends58M1[,-1]))
            Friends58M22<-cbind(Friends58M2[,1], c(Friends58M2[,-1]))
            #Make each person tied to themselves (to make models work)
            uniqueaid<-unique(Friends58M11[,1])
            uniqueaid_df<-data.frame(V1=uniqueaid, V2=uniqueaid)
            Friends58M11<-rbind(Friends58M11,uniqueaid_df)
            uniqueaid2<-unique(Friends58M22[,1])
            uniqueaid_df2<-data.frame(V1=uniqueaid2, V2=uniqueaid2)
            Friends58M22<-rbind(Friends58M22,uniqueaid_df2)
            #Drop Missing ties
            Friends58M11<-Friends58M11[!(is.na(Friends58M11$V2)),]
            Friends58M22<-Friends58M22[!(is.na(Friends58M22$V2)),]
            #Create adjacency matrix from edge list
            #S58W1
            Friends58M11<-Friends58M11[order(Friends58M11$V1),]
            g=graph.data.frame(Friends58M11, directed=TRUE)
            g1<-simplify(g)
            S58LW1<-get.adjacency(g1,sparse=FALSE)
            #S58W2
            Friends58M22<-Friends58M22[order(Friends58M22$V1),]
            rownames(Friends58M22)<-NULL
            g2=graph.data.frame(Friends58M22, directed=TRUE)
            g2<-simplify(g2)
            S58LW2<-get.adjacency(g2,sparse=FALSE)
            
            
            
            
            #School 77 
            #Wave 1-2
            #Restrict data to saturated schools
            ADHDFriends77M1<-ADHDFriends[ADHDFriends$scid==77,]
            ADHDFriends77M0<-ADHDFriends0[ADHDFriends0$scid==77,]
            #Remove Respondents who are not in school roster
            #Make both files have the same participants and create a flag to indicate which were not originally there
            #Unique aids
            MasterID0<-data.frame(aid=ADHDFriends77M0$aid)
            MasterID1<-data.frame(aid=ADHDFriends77M1$aid)
            MasterID<-unique(rbind(MasterID0,MasterID1))
            ADHDFriends77M1$Missing<-0
            ADHDFriends77M0$Missing<-0
            ADHDFriends77M1<-merge(ADHDFriends77M1,MasterID, by="aid", all.y=TRUE)
            ADHDFriends77M0<-merge(ADHDFriends77M0,MasterID, by="aid", all.y=TRUE)
            ADHDFriends77M1$Missing<-ifelse(is.na(ADHDFriends77M1$Missing),1,0)
            ADHDFriends77M0$Missing<-ifelse(is.na(ADHDFriends77M0$Missing),1,0)
            #Sort by aid
            ADHDFriends77M1<-ADHDFriends77M1[order(ADHDFriends77M1$aid),]
            ADHDFriends77M0<-ADHDFriends77M0[order(ADHDFriends77M0$aid),]
            #Remove sent ties that are not in AID
            ADHDFriends77M1<-transform(ADHDFriends77M1,
                                       MF_AID1=ifelse(MF_AID1 %in% aid, MF_AID1, NA),
                                       MF_AID2=ifelse(MF_AID2 %in% aid, MF_AID2, NA),
                                       MF_AID3=ifelse(MF_AID3 %in% aid, MF_AID3, NA),
                                       MF_AID4=ifelse(MF_AID4 %in% aid, MF_AID4, NA),
                                       MF_AID5=ifelse(MF_AID5 %in% aid, MF_AID5, NA),
                                       FF_AID1=ifelse(FF_AID1 %in% aid, FF_AID1, NA),
                                       FF_AID2=ifelse(FF_AID2 %in% aid, FF_AID2, NA),
                                       FF_AID3=ifelse(FF_AID3 %in% aid, FF_AID3, NA),
                                       FF_AID4=ifelse(FF_AID4 %in% aid, FF_AID4, NA),
                                       FF_AID5=ifelse(FF_AID5 %in% aid, FF_AID5, NA)
            )
            ADHDFriends77M0<-transform(ADHDFriends77M0,
                                       MF_AID1=ifelse(MF1AID %in% aid, MF1AID, NA),
                                       MF_AID2=ifelse(MF2AID %in% aid, MF2AID, NA),
                                       MF_AID3=ifelse(MF3AID %in% aid, MF3AID, NA),
                                       MF_AID4=ifelse(MF4AID %in% aid, MF4AID, NA),
                                       MF_AID5=ifelse(MF5AID %in% aid, MF5AID, NA),
                                       FF_AID1=ifelse(FF1AID %in% aid, FF1AID, NA),
                                       FF_AID2=ifelse(FF2AID %in% aid, FF2AID, NA),
                                       FF_AID3=ifelse(FF3AID %in% aid, FF3AID, NA),
                                       FF_AID4=ifelse(FF4AID %in% aid, FF4AID, NA),
                                       FF_AID5=ifelse(FF5AID %in% aid, FF5AID, NA)
            )
            #Remove complete isolates
            ADHDFriends77M0$NoFriends<-ifelse(is.na(ADHDFriends77M0$MF_AID1) & is.na(ADHDFriends77M0$MF_AID2) & is.na(ADHDFriends77M0$MF_AID3) & is.na(ADHDFriends77M0$MF_AID4) & is.na(ADHDFriends77M0$MF_AID5) & is.na(ADHDFriends77M0$FF_AID1) & is.na(ADHDFriends77M0$FF_AID2) & is.na(ADHDFriends77M0$FF_AID3) & is.na(ADHDFriends77M0$FF_AID4) & is.na(ADHDFriends77M0$FF_AID5),1,0)
            ADHDFriends77M1$NoFriends<-ifelse(is.na(ADHDFriends77M1$MF_AID1) & is.na(ADHDFriends77M1$MF_AID2) & is.na(ADHDFriends77M1$MF_AID3) & is.na(ADHDFriends77M1$MF_AID4) & is.na(ADHDFriends77M1$MF_AID5) & is.na(ADHDFriends77M1$FF_AID1) & is.na(ADHDFriends77M1$FF_AID2) & is.na(ADHDFriends77M1$FF_AID3) & is.na(ADHDFriends77M1$FF_AID4) & is.na(ADHDFriends77M1$FF_AID5),1,0)
            ADHDFriends77M0$NotFriended<-ifelse((ADHDFriends77M0$aid %in% ADHDFriends77M0$MF_AID1) | (ADHDFriends77M0$aid %in% ADHDFriends77M0$MF_AID2) | (ADHDFriends77M0$aid %in% ADHDFriends77M0$MF_AID3) | (ADHDFriends77M0$aid %in% ADHDFriends77M0$MF_AID4) | (ADHDFriends77M0$aid %in% ADHDFriends77M0$MF_AID5) | (ADHDFriends77M0$aid %in% ADHDFriends77M0$FF_AID1) | (ADHDFriends77M0$aid %in% ADHDFriends77M0$FF_AID2) | (ADHDFriends77M0$aid %in% ADHDFriends77M0$FF_AID3) | (ADHDFriends77M0$aid %in% ADHDFriends77M0$FF_AID4) | (ADHDFriends77M0$aid %in% ADHDFriends77M0$FF_AID5),0,1)
            ADHDFriends77M1$NotFriended<-ifelse((ADHDFriends77M1$aid %in% ADHDFriends77M1$MF_AID1) | (ADHDFriends77M1$aid %in% ADHDFriends77M1$MF_AID2) | (ADHDFriends77M1$aid %in% ADHDFriends77M1$MF_AID3) | (ADHDFriends77M1$aid %in% ADHDFriends77M1$MF_AID4) | (ADHDFriends77M1$aid %in% ADHDFriends77M1$MF_AID5) | (ADHDFriends77M1$aid %in% ADHDFriends77M1$FF_AID1) | (ADHDFriends77M1$aid %in% ADHDFriends77M1$FF_AID2) | (ADHDFriends77M1$aid %in% ADHDFriends77M1$FF_AID3) | (ADHDFriends77M1$aid %in% ADHDFriends77M1$FF_AID4) | (ADHDFriends77M1$aid %in% ADHDFriends77M1$FF_AID5),0,1)
            ADHDFriends77M0$Isolate<-ifelse(ADHDFriends77M0$NoFriends==1 & ADHDFriends77M1$NoFriends==1 & ADHDFriends77M0$NotFriended==1 & ADHDFriends77M1$NotFriended==1,1,0)
            ADHDFriends77M1$Isolate<-ifelse(ADHDFriends77M0$NoFriends==1 & ADHDFriends77M1$NoFriends==1 & ADHDFriends77M0$NotFriended==1 & ADHDFriends77M1$NotFriended==1,1,0)
            ADHDFriends77M0<-ADHDFriends77M0[ADHDFriends77M0$Isolate!=1,]
            ADHDFriends77M1<-ADHDFriends77M1[ADHDFriends77M1$Isolate!=1,]
            #Create Friendship Matrices
            #Reset row names
            rownames(ADHDFriends77M1)<-NULL
            rownames(ADHDFriends77M0)<-NULL
            #Extract Friendship data
            Friends77M1<-as.matrix(ADHDFriends77M1[,c("aid", "MF_AID1","MF_AID2","MF_AID3","MF_AID4","MF_AID5","FF_AID1","FF_AID2","FF_AID3","FF_AID4","FF_AID5")])
            Friends77M0<-as.matrix(ADHDFriends77M0[,c("aid", "MF_AID1","MF_AID2","MF_AID3","MF_AID4","MF_AID5","FF_AID1","FF_AID2","FF_AID3","FF_AID4","FF_AID5")])
            #Create edge list
            Friends77M11<-cbind(Friends77M1[,1], c(Friends77M1[,-1]))
            Friends77M00<-cbind(Friends77M0[,1], c(Friends77M0[,-1]))
            #Make each person tied to themselves (to make models work)
            uniqueaid<-unique(Friends77M11[,1])
            uniqueaid_df<-data.frame(V1=uniqueaid, V2=uniqueaid)
            Friends77M11<-rbind(Friends77M11,uniqueaid_df)
            uniqueaid0<-unique(Friends77M00[,1])
            uniqueaid_df0<-data.frame(V1=uniqueaid0, V2=uniqueaid0)
            Friends77M00<-rbind(Friends77M00,uniqueaid_df0)
            #Drop Missing ties
            Friends77M11<-Friends77M11[!(is.na(Friends77M11$V2)),]
            Friends77M00<-Friends77M00[!(is.na(Friends77M00$V2)),]
            #Create adjacency matrix from edge list
            #S77W1
            Friends77M11<-Friends77M11[order(Friends77M11$V1),]
            rownames(Friends77M11)<-NULL
            g=graph.data.frame(Friends77M11, directed=TRUE)
            g1<-simplify(g)
            S77FW1<-get.adjacency(g1,sparse=FALSE)
            #S77W0
            Friends77M00<-Friends77M00[order(Friends77M00$V1),]
            rownames(Friends77M00)<-NULL
            g0=graph.data.frame(Friends77M00, directed=TRUE)
            g0<-simplify(g0)
            S77FW0<-get.adjacency(g0,sparse=FALSE)
            
            #Wave 2-3
            #Restrict data to saturated schools
            ADHDFriends77ML1<-ADHDFriends[ADHDFriends$scid==77,]
            ADHDFriends77M2<-ADHDFriends2[ADHDFriends2$scid==77,]
            #Remove Respondents who are not in school roster
            ADHDFriends77ML1<-ADHDFriends77ML1[ADHDFriends77ML1$aid >= 90000000,]
            ADHDFriends77M2<-ADHDFriends77M2[ADHDFriends77M2$aid >= 90000000,]
            #Make both files have the same participants and create a flag to indicate which were not originally there
            #Unique aids
            MasterID2<-data.frame(aid=ADHDFriends77M2$aid)
            MasterID1<-data.frame(aid=ADHDFriends77ML1$aid)
            MasterID<-NULL
            MasterID<-unique(rbind(MasterID2,MasterID1))
            ADHDFriends77ML1$Missing<-0
            ADHDFriends77M2$Missing<-0
            ADHDFriends77ML1<-merge(ADHDFriends77ML1,MasterID, by="aid", all.y=TRUE)
            ADHDFriends77M2<-merge(ADHDFriends77M2,MasterID, by="aid", all.y=TRUE)
            ADHDFriends77ML1$Missing<-ifelse(is.na(ADHDFriends77ML1$Missing),1,0)
            ADHDFriends77M2$Missing<-ifelse(is.na(ADHDFriends77M2$Missing),1,0)
            #Sort by aid
            ADHDFriends77ML1<-ADHDFriends77ML1[order(ADHDFriends77ML1$aid),]
            ADHDFriends77M2<-ADHDFriends77M2[order(ADHDFriends77M2$aid),]
            #Remove sent ties that are not in AID
            ADHDFriends77ML1<-transform(ADHDFriends77ML1,
                                       MF_AID1=ifelse(MF_AID1 %in% aid, MF_AID1, NA),
                                       MF_AID2=ifelse(MF_AID2 %in% aid, MF_AID2, NA),
                                       MF_AID3=ifelse(MF_AID3 %in% aid, MF_AID3, NA),
                                       MF_AID4=ifelse(MF_AID4 %in% aid, MF_AID4, NA),
                                       MF_AID5=ifelse(MF_AID5 %in% aid, MF_AID5, NA),
                                       FF_AID1=ifelse(FF_AID1 %in% aid, FF_AID1, NA),
                                       FF_AID2=ifelse(FF_AID2 %in% aid, FF_AID2, NA),
                                       FF_AID3=ifelse(FF_AID3 %in% aid, FF_AID3, NA),
                                       FF_AID4=ifelse(FF_AID4 %in% aid, FF_AID4, NA),
                                       FF_AID5=ifelse(FF_AID5 %in% aid, FF_AID5, NA)
            )
            ADHDFriends77M2<-transform(ADHDFriends77M2,
                                       MF_AID1=ifelse(MF_AID1 %in% aid, MF_AID1, NA),
                                       MF_AID2=ifelse(MF_AID2 %in% aid, MF_AID2, NA),
                                       MF_AID3=ifelse(MF_AID3 %in% aid, MF_AID3, NA),
                                       MF_AID4=ifelse(MF_AID4 %in% aid, MF_AID4, NA),
                                       MF_AID5=ifelse(MF_AID5 %in% aid, MF_AID5, NA),
                                       FF_AID1=ifelse(FF_AID1 %in% aid, FF_AID1, NA),
                                       FF_AID2=ifelse(FF_AID2 %in% aid, FF_AID2, NA),
                                       FF_AID3=ifelse(FF_AID3 %in% aid, FF_AID3, NA),
                                       FF_AID4=ifelse(FF_AID4 %in% aid, FF_AID4, NA),
                                       FF_AID5=ifelse(FF_AID5 %in% aid, FF_AID5, NA)
                                       )
            #Remove complete isolates
            ADHDFriends77M2$NoFriends<-ifelse(is.na( ADHDFriends77M2$MF_AID1) & is.na( ADHDFriends77M2$MF_AID2) & is.na(ADHDFriends77M2$MF_AID3) & is.na(ADHDFriends77M2$MF_AID4) & is.na(ADHDFriends77M2$MF_AID5) & is.na(ADHDFriends77M2$FF_AID1) & is.na(ADHDFriends77M2$FF_AID2) & is.na(ADHDFriends77M2$FF_AID3) & is.na(ADHDFriends77M2$FF_AID4) & is.na(ADHDFriends77M2$FF_AID5),1,0)
            ADHDFriends77ML1$NoFriends<-ifelse(is.na(ADHDFriends77ML1$MF_AID1) & is.na(ADHDFriends77ML1$MF_AID2) & is.na(ADHDFriends77ML1$MF_AID3) & is.na(ADHDFriends77ML1$MF_AID4) & is.na(ADHDFriends77ML1$MF_AID5) & is.na(ADHDFriends77ML1$FF_AID1) & is.na(ADHDFriends77ML1$FF_AID2) & is.na(ADHDFriends77ML1$FF_AID3) & is.na(ADHDFriends77ML1$FF_AID4) & is.na(ADHDFriends77ML1$FF_AID5),1,0)
            ADHDFriends77M2$NotFriended<-ifelse((ADHDFriends77M2$aid %in% ADHDFriends77M2$MF_AID1) | (ADHDFriends77M2$aid %in% ADHDFriends77M2$MF_AID2) | (ADHDFriends77M2$aid %in% ADHDFriends77M2$MF_AID3) | (ADHDFriends77M2$aid %in% ADHDFriends77M2$MF_AID4) | (ADHDFriends77M2$aid %in% ADHDFriends77M2$MF_AID5) | (ADHDFriends77M2$aid %in% ADHDFriends77M2$FF_AID1) | (ADHDFriends77M2$aid %in% ADHDFriends77M2$FF_AID2) | (ADHDFriends77M2$aid %in% ADHDFriends77M2$FF_AID3) | (ADHDFriends77M2$aid %in% ADHDFriends77M2$FF_AID4) | (ADHDFriends77M2$aid %in% ADHDFriends77M2$FF_AID5),0,1)
            ADHDFriends77ML1$NotFriended<-ifelse((ADHDFriends77ML1$aid %in% ADHDFriends77ML1$MF_AID1) | (ADHDFriends77ML1$aid %in% ADHDFriends77ML1$MF_AID2) | (ADHDFriends77ML1$aid %in% ADHDFriends77ML1$MF_AID3) | (ADHDFriends77ML1$aid %in% ADHDFriends77ML1$MF_AID4) | (ADHDFriends77ML1$aid %in% ADHDFriends77ML1$MF_AID5) | (ADHDFriends77ML1$aid %in% ADHDFriends77ML1$FF_AID1) | (ADHDFriends77ML1$aid %in% ADHDFriends77ML1$FF_AID2) | (ADHDFriends77ML1$aid %in% ADHDFriends77ML1$FF_AID3) | (ADHDFriends77ML1$aid %in% ADHDFriends77ML1$FF_AID4) | (ADHDFriends77ML1$aid %in% ADHDFriends77ML1$FF_AID5),0,1)
            ADHDFriends77M2$Isolate<-ifelse(ADHDFriends77M2$NoFriends==1 & ADHDFriends77ML1$NoFriends==1 & ADHDFriends77M2$NotFriended==1 & ADHDFriends77ML1$NotFriended==1,1,0)
            ADHDFriends77ML1$Isolate<-ifelse(ADHDFriends77M2$NoFriends==1 & ADHDFriends77ML1$NoFriends==1 & ADHDFriends77M2$NotFriended==1 & ADHDFriends77ML1$NotFriended==1,1,0)
            ADHDFriends77M2<-ADHDFriends77M2[ADHDFriends77M2$Isolate!=1,]
            ADHDFriends77ML1<-ADHDFriends77ML1[ADHDFriends77ML1$Isolate!=1,]
            #Create Friendship Matrices
            #Reset row names
            rownames(ADHDFriends77ML1)<-NULL
            rownames(ADHDFriends77M2)<-NULL
            #Extract Friendship data
            Friends77M1<-as.matrix(ADHDFriends77ML1[,c("aid", "MF_AID1","MF_AID2","MF_AID3","MF_AID4","MF_AID5","FF_AID1","FF_AID2","FF_AID3","FF_AID4","FF_AID5")])
            Friends77M2<-as.matrix(ADHDFriends77M2[,c("aid", "MF_AID1","MF_AID2","MF_AID3","MF_AID4","MF_AID5","FF_AID1","FF_AID2","FF_AID3","FF_AID4","FF_AID5")])
            #Create edge list
            Friends77M11<-cbind(Friends77M1[,1], c(Friends77M1[,-1]))
            Friends77M22<-cbind(Friends77M2[,1], c(Friends77M2[,-1]))
            #Make each person tied to themselves (to make models work)
            uniqueaid<-unique(Friends77M11[,1])
            uniqueaid_df<-data.frame(V1=uniqueaid, V2=uniqueaid)
            Friends77M11<-rbind(Friends77M11,uniqueaid_df)
            uniqueaid2<-unique(Friends77M22[,1])
            uniqueaid_df2<-data.frame(V1=uniqueaid2, V2=uniqueaid2)
            Friends77M22<-rbind(Friends77M22,uniqueaid_df2)
            #Drop Missing ties
            Friends77M11<-Friends77M11[!(is.na(Friends77M11$V2)),]
            Friends77M22<-Friends77M22[!(is.na(Friends77M22$V2)),]
            #Create adjacency matrix from edge list
            #S77W1
            Friends77M11<-Friends77M11[order(Friends77M11$V1),]
            rownames(Friends77M11)<-NULL
            g=graph.data.frame(Friends77M11, directed=TRUE)
            g1<-simplify(g)
            S77LW1<-get.adjacency(g1,sparse=FALSE)
            #S77W2
            Friends77M22<-Friends77M22[order(Friends77M22$V1),]
            rownames(Friends77M22)<-NULL
            g2=graph.data.frame(Friends77M22, directed=TRUE)
            g2<-simplify(g2)
            S77LW2<-get.adjacency(g2,sparse=FALSE)
            
            #        table(degree(g1, v = V(g1), mode = c("out"),
            #                loops = TRUE, normalized = FALSE))
            #         
            #         degree_distribution(g2, cumulative = FALSE)