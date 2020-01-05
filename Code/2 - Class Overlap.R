#Load file
  load("educov94.RData")
  load("educov95.RData")
  
#Convert to numeric
  educov94 <- data.frame(lapply(educov94, as.numeric), stringsAsFactors=FALSE)
  
#Create adjacency list with just students from school 58
  AL94<-educov94[educov94$ENSCLID4==58,]
  
#Remove all ties not in dataset
  AL94<-AL94[AL94$aid %in% ADHDFriends58M0$aid,]
  AL94<-AL94[AL94$ENAIDA4 %in% ADHDFriends58M0$aid,]
  
#Make distribution of ties less skewed
  AL94$temp<-log(AL94$ENCOW4+1)
  
#Bring in all missing people, and set their ties with all others at the median tie strength 
  temp<-unique(AL94$aid)
  temp1<-ADHDFriends58M0[!(ADHDFriends58M0$aid %in% AL94$aid),]$aid #missing from course data
  temp2<-NULL
  for (i in 1:length(temp1)) {
    temp3<-data.frame(V1=temp1[i],V2=ADHDFriends58M0$aid)
    temp4<-data.frame(V1=ADHDFriends58M0$aid,V2=temp1[i])
    temp2<-rbind(temp2,temp3,temp4)
  }
  temp2$V3<-median(AL94$temp)
    
#Remove duplicates generate by above
  temp2<-unique(temp2)
  
#Restrict AL94 to variables of interest, and bind with ties of missing data
  AL94<-data.frame(V1=AL94$aid,V2=AL94$ENAIDA4,V3=AL94$temp)
  AL94<-rbind(AL94,temp2)
  
#Sort and remove row names and self loops
  AL94<-AL94[order(AL94$V1),]
  rownames(AL94)<-NULL
  AL94<-AL94[AL94$V1!=AL94$V2,]
  
#Change IDs to index of unique rows
  #Sender IDs
    temp<-data.frame(V1=ADHDFriends58M0$aid,V4=1:nrow(ADHDFriends58M0))
    AL941<-merge(AL94,temp, by="V1", all.y=TRUE)
  #Receiver IDs
    temp<-data.frame(V2=ADHDFriends58M0$aid,V5=1:nrow(ADHDFriends58M0))
    AL942<-merge(AL941,temp, by="V2", all.y=TRUE)
  #Finish replacement  
    AL94<-data.frame(V1=AL942$V4,V2=AL942$V5,V3=AL942$V3)
    AL94<-AL94[order(AL94$V1),]
    AL941<-as.matrix(AL94)
    
#Convert to atomic format  
  a=AL94[,1]
  b=AL94[,2]
  c=AL94[,3]
  AL941<-cbind(a,b,c)

#Set to matrix (Can't use igraph because it takes too much memory)
  overlap58.94<-matrix(0, nrow=length(ADHDFriends58M0$aid), ncol=length(ADHDFriends58M0$aid))
  overlap58.94[AL941[,1:2]]<-AL941[,3]

#Iterate above over the following year and other schoold
  #School 58, year 2  
      #Convert to numeric
          educov95 <- data.frame(lapply(educov95, as.numeric), stringsAsFactors=FALSE)
      #Create adjacency list with just students from school 58
          AL95<-educov95[educov95$ENSCLID5==58,]
      #Remove all ties not in dataset
          AL95<-AL95[AL95$aid %in% ADHDFriends58M0$aid,]
          AL95<-AL95[AL95$ENAIDA5 %in% ADHDFriends58M0$aid,]
      #Make distribution of ties less skewed
          AL95$temp<-log(AL95$ENCOW5+1)
      #Bring in all missing people, and set their ties with all others at the median tie strength 
          temp<-unique(AL95$aid)
          temp1<-ADHDFriends58M0[!(ADHDFriends58M0$aid %in% AL95$aid),]$aid #missing from course data
          temp2<-NULL
          i=1
          for (i in 1:length(temp1)) {
            temp3<-data.frame(V1=temp1[i],V2=ADHDFriends58M0$aid)
            temp4<-data.frame(V1=ADHDFriends58M0$aid,V2=temp1[i])
            temp2<-rbind(temp2,temp3,temp4)
          }
          temp2$V3<-median(AL95$temp)
      #Remove duplicates generate by above
          temp2<-unique(temp2)
      #Restrict AL95 to variables of interest, and bind with ties of missing data
          AL95<-data.frame(V1=AL95$aid,V2=AL95$ENAIDA5,V3=AL95$temp)
          AL95<-rbind(AL95,temp2)
      #Sort and remove row names and self loops
          AL95<-AL95[order(AL95$V1),]
          rownames(AL95)<-NULL
          AL95<-AL95[AL95$V1!=AL95$V2,]
      #Change IDs to index of unique rows
          #Sender IDs
              temp<-data.frame(V1=ADHDFriends58M0$aid,V4=1:nrow(ADHDFriends58M0))
              AL951<-merge(AL95,temp, by="V1", all.y=TRUE)
          #Receiver IDs
              temp<-data.frame(V2=ADHDFriends58M0$aid,V5=1:nrow(ADHDFriends58M0))
              AL952<-merge(AL951,temp, by="V2", all.y=TRUE)
          #Finish replacement  
              AL95<-data.frame(V1=AL952$V4,V2=AL952$V5,V3=AL952$V3)
              AL95<-AL95[order(AL95$V1),]
              AL951<-as.matrix(AL95)
          #Convert to atomic format  
              a=AL95[,1]
              b=AL95[,2]
              c=AL95[,3]
              AL951<-cbind(a,b,c)
      #Set to matrix (Can't use igraph because it takes too much memory)
          overlap58.95<-matrix(0, nrow=length(ADHDFriends58M0$aid), ncol=length(ADHDFriends58M0$aid))
          overlap58.95[AL951[,1:2]]<-AL951[,3]
      
  #School 77 year 1
      #Convert to numeric
      educov94 <- data.frame(lapply(educov94, as.numeric), stringsAsFactors=FALSE)
      #Create adjacency list with just students from school 58
      AL94<-educov94[educov94$ENSCLID4==77,]
      #Remove all ties not in dataset
      AL94<-AL94[AL94$aid %in% ADHDFriends77M0$aid,]
      AL94<-AL94[AL94$ENAIDA4 %in% ADHDFriends77M0$aid,]
      #Make distribution of ties less skewed
      AL94$temp<-log(AL94$ENCOW4+1)
      #Bring in all missing people, and set their ties with all others at the median tie strength 
      temp<-unique(AL94$aid)
      temp1<-ADHDFriends77M0[!(ADHDFriends77M0$aid %in% AL94$aid),]$aid #missing from course data
      temp2<-NULL
      i=1
      for (i in 1:length(temp1)) {
        temp3<-data.frame(V1=temp1[i],V2=ADHDFriends77M0$aid)
        temp4<-data.frame(V1=ADHDFriends77M0$aid,V2=temp1[i])
        temp2<-rbind(temp2,temp3,temp4)
      }
      temp2$V3<-median(AL94$temp)
      #Remove duplicates generate by above
      temp2<-unique(temp2)
      #Restrict AL94 to variables of interest, and bind with ties of missing data
      AL94<-data.frame(V1=AL94$aid,V2=AL94$ENAIDA4,V3=AL94$temp)
      AL94<-rbind(AL94,temp2)
      #Sort and remove row names and self loops
      AL94<-AL94[order(AL94$V1),]
      rownames(AL94)<-NULL
      AL94<-AL94[AL94$V1!=AL94$V2,]
      #Change IDs to index of unique rows
      #Sender IDs
      temp<-data.frame(V1=ADHDFriends77M0$aid,V4=1:nrow(ADHDFriends77M0))
      AL941<-merge(AL94,temp, by="V1", all.y=TRUE)
      #Receiver IDs
      temp<-data.frame(V2=ADHDFriends77M0$aid,V5=1:nrow(ADHDFriends77M0))
      AL942<-merge(AL941,temp, by="V2", all.y=TRUE)
      #Finish replacement  
      AL94<-data.frame(V1=AL942$V4,V2=AL942$V5,V3=AL942$V3)
      AL94<-AL94[order(AL94$V1),]
      AL941<-as.matrix(AL94)
      #Convert to atomic format  
      a=AL94[,1]
      b=AL94[,2]
      c=AL94[,3]
      AL941<-cbind(a,b,c)
      is.atomic(AL941)
      #Set to matrix (Can't use igraph because it takes too much memory)
      overlap77.94<-matrix(0, nrow=length(ADHDFriends77M0$aid), ncol=length(ADHDFriends77M0$aid))
      overlap77.94[AL941[,1:2]]<-AL941[,3]
      
  #School 77, year 2  
      #Convert to numeric
      educov95 <- data.frame(lapply(educov95, as.numeric), stringsAsFactors=FALSE)
      #Create adjacency list with just students from school 77
      AL95<-educov95[educov95$ENSCLID5==77,]
      #Remove all ties not in dataset
      AL95<-AL95[AL95$aid %in% ADHDFriends77M0$aid,]
      AL95<-AL95[AL95$ENAIDA5 %in% ADHDFriends77M0$aid,]
      #Make distribution of ties less skewed
      AL95$temp<-log(AL95$ENCOW5+1)
      #Bring in all missing people, and set their ties with all others at the median tie strength 
      temp<-unique(AL95$aid)
      temp1<-ADHDFriends77M0[!(ADHDFriends77M0$aid %in% AL95$aid),]$aid #missing from course data
      temp2<-NULL
      i=1
      for (i in 1:length(temp1)) {
        temp3<-data.frame(V1=temp1[i],V2=ADHDFriends77M0$aid)
        temp4<-data.frame(V1=ADHDFriends77M0$aid,V2=temp1[i])
        temp2<-rbind(temp2,temp3,temp4)
      }
      temp2$V3<-median(AL95$temp)
      #Remove duplicates generate by above
      temp2<-unique(temp2)
      #Restrict AL95 to variables of interest, and bind with ties of missing data
      AL95<-data.frame(V1=AL95$aid,V2=AL95$ENAIDA5,V3=AL95$temp)
      AL95<-rbind(AL95,temp2)
      #Sort and remove row names and self loops
      AL95<-AL95[order(AL95$V1),]
      rownames(AL95)<-NULL
      AL95<-AL95[AL95$V1!=AL95$V2,]
      #Change IDs to index of unique rows
      #Sender IDs
      temp<-data.frame(V1=ADHDFriends77M0$aid,V4=1:nrow(ADHDFriends77M0))
      AL951<-merge(AL95,temp, by="V1", all.y=TRUE)
      #Receiver IDs
      temp<-data.frame(V2=ADHDFriends77M0$aid,V5=1:nrow(ADHDFriends77M0))
      AL952<-merge(AL951,temp, by="V2", all.y=TRUE)
      #Finish replacement  
      AL95<-data.frame(V1=AL952$V4,V2=AL952$V5,V3=AL952$V3)
      AL95<-AL95[order(AL95$V1),]
      AL951<-as.matrix(AL95)
      #Convert to atomic format  
      a=AL95[,1]
      b=AL95[,2]
      c=AL95[,3]
      AL951<-cbind(a,b,c)
      is.atomic(AL951)
      #Set to matrix (Can't use igraph because it takes too much memory)
      overlap77.95<-matrix(0, nrow=length(ADHDFriends77M0$aid), ncol=length(ADHDFriends77M0$aid))
      overlap77.95[AL951[,1:2]]<-AL951[,3]
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  educov95 <- read.xport('educov95.xpt')
  temp2 <- data.frame(lapply(educov95 , as.character), stringsAsFactors=FALSE)  # convert factors to character type
  educov95 <- data.frame(lapply(temp2, as.numeric), stringsAsFactors=FALSE)  # convert characters to numeric type
  temp <- merge(educov95,mastIDs58,by='AID')
  actors <- length(attr58$AID)
  newIDs <- 1:actors
  data2 <- recode(temp,attr58$AID,newIDs)
  sociom <- matrix(NA, nrow=actors, ncol=actors)
  for (i in 1:nrow(data2)) {
    if (data2[i,2] < 2000) {                  # if alter included in mastIDs
      sociom[data2[i,1], data2[i,2]]=data2[i,5]
    }
  }
  diag(sociom)<-0
  sociom <- (log(sociom+1))   #  distribution of course overlap is skewed
  sociom[is.na(sociom)] <- median(sociom,na.rm=T)   #  replace missing values with median (.0268)
  overlap58.95 <- sociom
  

# repeat for school 77
temp <- merge(educov94,mastIDs77,by='AID')
actors <- length(attr77$AID)
newIDs <- 1:actors
data2 <- recode(temp,attr77$AID,newIDs)
sociom <- matrix(NA, nrow=actors, ncol=actors)
for (i in 1:nrow(data2)) {
  if (data2[i,2] < 3000) {                  # if alter included in mastIDs
    sociom[data2[i,1], data2[i,2]]=data2[i,5]
  }
}
diag(sociom)<-0
sociom <- (log(sociom+1))   #  distribution of course overlap is skewed
sociom[is.na(sociom)] <- median(sociom,na.rm=T)   #  replace missing values with median (.0268)
overlap77.94 <- sociom

temp <- merge(educov95,mastIDs77,by='AID')
actors <- length(attr77$AID)
newIDs <- 1:actors
data2 <- recode(temp,attr77$AID,newIDs)
sociom <- matrix(NA, nrow=actors, ncol=actors)
for (i in 1:nrow(data2)) {
  if (data2[i,2] < 3000) {                  # if alter included in mastIDs
    sociom[data2[i,1], data2[i,2]]=data2[i,5]
  }
}
diag(sociom)<-0
sociom <- (log(sociom+1))   #  distribution of course overlap is skewed
sociom[is.na(sociom)] <- median(sociom,na.rm=T)   #  replace missing values with median (.0268)
overlap77.95 <- sociom


write.table(overlap58.94, file='overlap58.94.dat')
write.table(overlap58.95, file='overlap58.95.dat')
write.table(overlap77.94, file='overlap77.94.dat')
write.table(overlap77.95, file='overlap77.95.dat')

