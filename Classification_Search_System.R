library(openxlsx)
library(readxl)
library(tidyverse)
library(skimr)
library(DataExplorer)
library(caret)
library(pROC)
library(openxlsx)
library(dplyr)
library(foreign)
options(digits = 13)

# Order level Example
# Read the diagnostic information needed for Order retrieval
Anthrostagnic_epipedon<-read.dbf('*\\Anthrostagnic_epipedon\\Probability.dbf')
xy<-select(Anthrostagnic_epipedon,1,2)# Get XY coordinates
Anthrostagnic_epipedon<-select(Anthrostagnic_epipedon,3)# Read Probability
Hydragric_horizon<-read.dbf('*\\Hydragric_horizon\\Probability.dbf')
Hydragric_horizon<-select(Hydragric_horizon,3)# Read Probability
Claypan_0_125<-read.dbf('*\\Claypan_0_125\\Probability.dbf')
Claypan_0_125<-select(Claypan_0_125,3)# Read Probability
Argic_horizon<-read.dbf('*\\Argic_horizon\\Probability.dbf')
Argic_horizon<-select(Argic_horizon,3)# Read Probability
tree<-cbind(Anthrostagnic_epipedon,Hydragric_horizon,Claypan_0_125,Argic_horizon)
colnames(tree)<-c('Anthrostagnic_epipedon','Hydragric_horizon','Claypan_0_125','Argic_horizon')

# Retrieve Order type
tree$Anthrosols<-tree$Anthrostagnic_epipedon*tree$Hydragric_horizon
tree$Argosols<-apply(tree[c('Claypan_0_125','Argic_horizon')] ,1,max)
tree$A<-tree$Anthrosols
tree$B<-(1-tree$Anthrosols)*tree$Argosols
tree$c<-1-tree$A-tree$B
tree$c[tree$c<'0']<- 0
tree<-select(tree,7,8,9)
colnames(tree)<-c('Anthrosols','Argosols','Cambosols')
write.dbf(tree,'*\\Order.dbf')

#Categorization
number<-t(apply(tree,1,sort,decreasing=T)) #Get the numerical serial number (1 for the first variable in the original set, 2 for the second variable, 3 for the third variable)
colnames(number)<-c('num1','num2','num3')
label<-t(apply(tree,1,order,decreasing=T)) #Get the name of the variable corresponding to the serial number
colnames(label)<-c('V1','V2','V3')
res<-cbind(number,label)
res<-data.frame(res)
res$V1[res$V1=='1']<- 'Anthrosols'
res$V1[res$V1=='2']<- 'Argosols'
res$V1[res$V1=='3']<- 'Cambosols'
res$V2[res$V2=='1']<- 'Anthrosols'
res$V2[res$V2=='2']<- 'Argosols'
res$V2[res$V2=='3']<- 'Cambosols'
res$V3[res$V3=='1']<- 'Anthrosols'
res$V3[res$V3=='2']<- 'Argosols'
res$V3[res$V3=='3']<- 'Cambosols'
# Modify variable name
res<-cbind(res,xy) #Merge results and XY coordinates
write.dbf(res,'*\\Order_Result.dbf') 


# Suborder level Example
# Read the diagnostic information needed for Suborder retrieval
Soil_moisture_regime<-read.dbf('*\\Soil_moisture_regime\\Probability.dbf') 
Redox_0_50<-read.dbf('C:\\Users\\Administrator\\Desktop\\专家知识文献\\Redox_0_50\\Probability.dbf') 
panduan<-data.frame(Soil_moisture_regime$R2*Redox_0_50$Probability)
colnames(panduan)<-c('Probability')
Suborder<-c()
# Retrieve
Suborder$'Stagni_Anthrosols'<-tree$Anthrosols
Suborder$'Udic_Argosols'<-tree$Argosols
Suborder$'Aquic_Cambosols'<-tree$Cambosols*panduan$Probability
Suborder$'Udic_Cambosols '<-tree$Cambosols*(1-panduan$Probability)
Suborder<-data.frame(Suborder)
# Categorization
number<-t(apply(Suborder,1,sort,decreasing=T)) 
colnames(number)<-c('num1','num2','num3','num4')
label<-t(apply(Suborder,1,order,decreasing=T))
colnames(label)<-c('V1','V2','V3','V4')
res<-cbind(number,label)
res<-data.frame(res)
res$V1[res$V1=='1']<- 'Stagni_Anthrosols'
res$V1[res$V1=='2']<- 'Udic_Argosols'
res$V1[res$V1=='3']<- 'Aquic_Cambosols'
res$V1[res$V1=='4']<- 'Udic_Cambosols '
res$V2[res$V2=='1']<- 'Stagni_Anthrosols'
res$V2[res$V2=='2']<- 'Udic_Argosols'
res$V2[res$V2=='3']<- 'Aquic_Cambosols'
res$V2[res$V2=='4']<- 'Udic_Cambosols '
res$V3[res$V3=='1']<- 'Stagni_Anthrosols'
res$V3[res$V3=='2']<- 'Udic_Argosols'
res$V3[res$V3=='3']<- 'Aquic_Cambosols'
res$V3[res$V3=='4']<- 'Udic_Cambosols '
res<-cbind(res,xy) 
write.dbf(res,'*\\Suborder_Result.dbf') 


#Group level Example
Group<-c()
Gleyic_0_60<-read.dbf('*\\Gleyic_0_60\\Probability.dbf') 
DCB_iron<-read.dbf('*\\DCB_iron\\Probability.dbf') 
Claypan_0_125<-read.dbf('*\\Claypan_0_125\\Probability.dbf') 
colnames(panduan)<-c('Probability')
Basesaturation_pH<-read.dbf('*\\Basesaturation_pH\\Probability.dbf') 
Group$Gleyi-Stagnic_Anthrosols<-tree$Anthrosols*Gleyic_0_60$Probability 
Group$Fe_accumuli_Stagnic_Anthrosols<-(tree$Anthrosols*(1-Gleyic_0_60$Probability))*DCB_iron$Probability 
Group$Hapli_Stagnic_Anthrosols<-(tree$Anthrosols*(1-Gleyic_0_60$Probability))*(1-DCB_iron$Probability) 
Group$Claypani_Udic_Argosols<-tree$Argosols*Claypan_0_125$Probability 
Group$Hapli_Udic_Argosols<-tree$Argosols*(1-Claypan_0_125$Probability) 
Group$Ochri_Aquic_Cambosols<-tree$Cambosols*panduan$Probability 
Group$Acidi_Aquic_Cambosols<-(tree$Cambosols*(1-panduan$Probability))*Basesaturation_pH$Probability 
Group$Hapli_Udic_Cambosols <-(tree$Cambosols*(1-panduan$Probability))*(1-Basesaturation_pH$Probability) #Hapli_Udic_Cambosols 
Group<-data.frame(Group)
write.dbf(Group,'*\\Group.dbf')
# Retrieve
number<-t(apply(Group,1,sort,decreasing=T)) 
colnames(number)<-c('num1','num2','num3','num4','num5','num6','num7','num8')
label<-t(apply(Group,1,order,decreasing=T))
colnames(label)<-c('V1','V2','V3','V4','V5','V6','V7','V8')
res<-cbind(number,label)
res<-data.frame(res)
for (i in c(9:16)){
  res[,i][res[,i]=='1']='Gleyi_Stagnic_Anthrosols'
  res[,i][res[,i]=='2']='Fe_accumuli_Stagnic_Anthrosols'
  res[,i][res[,i]=='3']='Hapli_Stagnic_Anthrosols'
  res[,i][res[,i]=='4']='Claypani_Udic_Argosols'
  res[,i][res[,i]=='5']='Hapli_Udic_Argosols'
  res[,i][res[,i]=='6']='Ochri_Aquic_Cambosols'
  res[,i][res[,i]=='7']='Acidi_Aquic_Cambosols'
  res[,i][res[,i]=='8']='Hapli_Udic_Cambosols '}
write.dbf(res,'*\\Group.dbf')


#Subgroup level Example
Subgroup<-read.dbf('*\\Subgroup.dbf')
colnames(Subgroup)<-c('1','2','3','4','5','6','7','8')
Gleyic_60_100<-read.dbf('*\\Gleyic_60_100\\Probability.dbf') 
pH<-read.dbf('*\\pH\\Probability.dbf') 
Calcaric<-read.dbf('*\\Calcaric\\Probability.dbf') 
Basesaturation_pH<-read.dbf('*\\Basesaturation_pH\\Probability.dbf') 
Redox_50_100<-read.dbf('*\\Redox_50_100\\Probability.dbf') 
Subgroup<-c()
Subgroup$'Typic_Gleyi_Stagnic_Anthrosols'<-Subgroup$`1`*(1-DCB$Probability)
Subgroup$'Fe-accumulic_Gleyi_Stagnic_Anthrosols'<-Subgroup$`1`*DCB$Probability
Subgroup$'Endogleyic_Fe_accumuli_Stagnic_Anthrosols'<-Subgroup$`2`*Gleyic_60_100$Probability
Subgroup$'Typic_Fe_accumuli_Stagnic_Anthrosols'<-Subgroup$`2`*(1-Gleyic_60_100$Probability)
Subgroup$'Endogleyic_Hapli_Stagnic_Anthrosols'<-Subgroup$`3`*Gleyic_60_100$Probability
Subgroup$'Typic_Hapli_Stagnic_Anthrosols'<-Subgroup$`3`*(1-Gleyic_60_100$Probability)
Subgroup$'Eutric_Claypani_Udic_Argosols'<-Subgroup$`4`*pH$Probability
Subgroup$'Typic_Claypani_Udic_Argosols'<-Subgroup$`4`*(1-pH$Probability)
Subgroup$'Mottlic_Hapli_Udic_Argosols'<-Subgroup$`5`*tubaio100cm$Probability
Subgroup$'Typic_Hapli_Udic_Argosols'<-Subgroup$`5`*(1-tubaio100cm$Probability)
Subgroup$'Calcaric_Ochri_Aquic_Cambosols'<-Subgroup$`6`*Calcaric$Probability
Subgroup$'Acidi_Ochri_Aquic_Cambosols'<-Subgroup$`6`*(1-Calcaric$Probability)*Basesaturation_pH$Probability
Subgroup$'Typic_Ochri_Aquic_Cambosols'<-Subgroup$`6`*(1-Calcaric$Probability)*(1-Basesaturation_pH$Probability)
Subgroup$'Typic_Acidi_Aquic_Cambosols'<-Subgroup$`7`
Subgroup$'Mottlic_Hapli_Udic_Cambosols '<-Subgroup$`8`*Redox_50_100$Probability
Subgroup$'Typic_Hapli_Udic_Cambosols '<-Subgroup$`8`*(1-Redox_50_100$Probability)
Subgroup<-data.frame(Subgroup)

# Categorization
write.dbf(Subgroup,'*\\Subgroup.dbf')
number<-t(apply(Subgroup,1,sort,decreasing=T))
colnames(number)<-c('num1','num2','num3','num4','num5','num6','num7','num8','num9','num10','num11','num12','num13','num14','num15','num16')
label<-t(apply(Subgroup,1,order,decreasing=T))
colnames(label)<-c('V1','V2','V3','V4','V5','V6','V7','V8','V9','V10','V11','V12','V13','V14','V15','V16')
Subgrouppaixu<-cbind(number,label,xy)
for (i in c(17:32)){
  Subgrouppaixu[,i][Subgrouppaixu[,i]=='1']='Typic_Gleyi_Stagnic_Anthrosols'
  Subgrouppaixu[,i][Subgrouppaixu[,i]=='2']='Fe-accumulic_Gleyi_Stagnic_Anthrosols'
  Subgrouppaixu[,i][Subgrouppaixu[,i]=='3']='Endogleyic_Fe_accumuli_Stagnic_Anthrosols'
  Subgrouppaixu[,i][Subgrouppaixu[,i]=='4']='Typic_Fe_accumuli_Stagnic_Anthrosols'
  Subgrouppaixu[,i][Subgrouppaixu[,i]=='5']='Endogleyic_Hapli_Stagnic_Anthrosols'
  Subgrouppaixu[,i][Subgrouppaixu[,i]=='6']='Typic_Hapli_Stagnic_Anthrosols'
  Subgrouppaixu[,i][Subgrouppaixu[,i]=='7']='Eutric_Claypani_Udic_Argosols'
  Subgrouppaixu[,i][Subgrouppaixu[,i]=='8']='Typic_Claypani_Udic_Argosols'
  Subgrouppaixu[,i][Subgrouppaixu[,i]=='9']='Mottlic_Hapli_Udic_Argosols'
  Subgrouppaixu[,i][Subgrouppaixu[,i]=='10']='Typic_Hapli_Udic_Argosols'
  Subgrouppaixu[,i][Subgrouppaixu[,i]=='11']='Calcaric_Ochri_Aquic_Cambosols'
  Subgrouppaixu[,i][Subgrouppaixu[,i]=='12']='Acidi_Ochri_Aquic_Cambosols'
  Subgrouppaixu[,i][Subgrouppaixu[,i]=='13']='Typic_Ochri_Aquic_Cambosols'
  Subgrouppaixu[,i][Subgrouppaixu[,i]=='14']='Typic_Acidi_Aquic_Cambosols'
  Subgrouppaixu[,i][Subgrouppaixu[,i]=='15']='Mottlic_Hapli_Udic_Cambosols '
  Subgrouppaixu[,i][Subgrouppaixu[,i]=='16']='Typic_Hapli_Udic_Cambosols '
}
write.dbf(Subgrouppaixu,'*\\Subgroup.dbf')


#Retrieve family prefix
Subgroup<-read.dbf('*\\Subgroup.dbf')
Particle_size<-read.dbf('*\\Particle_size\\Probability.dbf')
Mineralogy<-read.dbf('*\\Mineralogy\\Probability.dbf')
Lithicity_acid<-read.dbf('*\\Lithicity_acid\\Probability.dbf')
xy<-select(Lithicity_acid,1,2)
Lithicity_acid<-select(Lithicity_acid,3,4,5)
colnames(Lithicity_acid)<-c('Calcaric','Nonacid','Acid')
Particle_size<-select(Particle_size,3,4,5,6,7,8,9)
colnames(Particle_size)<-c('Sandy','Fine','Fine_loamy','Loamy','Loamy_sandy','Loamy_fine')
Mineralogy<-select(Mineralogy,3,4,5,6,7,8,9)
colnames(Mineralogy)<-c('Micaceous_mixed','Silicon','Silicon_mixed','Mixed1','Kaolinitic_mixed','Lllite_mixed','Mixed2')

prefix<-c()
prefix$F1<-Particle_size$Fine*Mineralogy$Kaolinitic_mixed*Lithicity_acid$Calcaric
prefix$F2<-Particle_size$Fine*Mineralogy$Kaolinitic_mixed*Lithicity_acid$Acid
prefix$F3<-Particle_size$Fine*Mineralogy$Kaolinitic_mixed*Lithicity_acid$Nonacid
prefix$F4<-Particle_size$Fine*Mineralogy$Lllite_mixed*Lithicity_acid$Calcaric
prefix$F5<-Particle_size$Fine*Mineralogy$Lllite_mixed*Lithicity_acid$Acid
prefix$F6<-Particle_size$Fine*Mineralogy$Lllite_mixed*Lithicity_acid$Nonacid
prefix$F7<-Particle_size$Fine*Mineralogy$Mixed2*Lithicity_acid$Calcaric
prefix$F8<-Particle_size$Fine*Mineralogy$Mixed2*Lithicity_acid$Acid
prefix$F9<-Particle_size$Fine*Mineralogy$Mixed2*Lithicity_acid$Nonacid

prefix<-cbind(xy,prefix)
prefix<-data.frame(prefix)
write.dbf(prefix,'*\\family_prefix.dbf')
# 寻找最大值
number<-apply(prefix,1,max)
labels<-apply(prefix,1,which.max)
rs1<-cbind(number,labels)
rs1<-data.frame(rs1)

for (i in 1:3420432){
  j=rs1[i,2]
  prefix[i,j]=0
}

number<-apply(prefix,1,max)
labels<-apply(prefix,1,which.max)
rs2<-cbind(number,labels)
rs2<-data.frame(rs2)
colnames(rs1)<-c('num1','v1')
colnames(rs2)<-c('num2','v2')
rs<-cbind(rs1,rs2)

write.dbf(rs,'family_prefix2\\.dbf')