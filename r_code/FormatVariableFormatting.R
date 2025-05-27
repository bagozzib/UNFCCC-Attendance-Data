###
###This script corrects a small number of miscodings in the auxilliary group-type binary indicator variables included within our COP and pre-COP files
###

##############
####Set Up####
##############

#clear memory
rm( list=ls() )

#set seed
set.seed(50)

#set working directory
setwd("")

##read-in COP data##
COP<-read.csv("cops.cleaned.translated.csv",encoding="UTF-8")

#standardize entries
COP$Group_Type<-ifelse(COP$Group_Type=="InterGovernmental Organizations","Intergovernmental organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="INTERGOVERNMENTAL ORGANIZATIONS","Intergovernmental organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="ORGANISATIONS INTERGOUVERNEMENTALES","Intergovernmental organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="NON-GOVERNMENTAL ORGANIZATIONS","Non-governmental organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="ORGANISATIONS NON-GOUVERNEMENTALES","Non-governmental organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Non-Governmental Organizations","Non-governmental organizations",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="OBSERVER STATES","Observer States",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Observer states","Observer States",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters","Observer States",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Observer states and entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters.","Observer States",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Observer states and entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters\\.","Observer States",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="PARTIES","Parties",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="REPRESENTATIVES","Parties",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Representatives","Parties",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="REPRESENTANTS","Parties",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Representatives of specialized agencies and related organizations","Specialized agencies and related organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND RELATED ORGANIZATIONS","Specialized agencies and related organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="INSTITUTIONS SPECIALISEES ET AUTRES ORGANISATIONS","Specialized agencies and related organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Specialized Agencies and Related Organizations","Specialized agencies and related organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND RELATED ORGANIZATIONS","Specialized agencies and related organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND OTHER ORGANIZATIONS OF THE UNITED NATIONS SYSTEM","Specialized agencies and related organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Representatives of specialized agencies and related organizations","Specialized agencies and related organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="REPRESENTATIVES OF UNITED NATIONS SECRETARIAT UNITS AND BODIES","United Nations Secretariat units and bodies",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="NATIONS UNIES","United Nations Secretariat units and bodies",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Representatives of United Nations Secretariat units and bodies","United Nations Secretariat units and bodies",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="UN Secretariat units and Bodies","United Nations Secretariat units and bodies",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Representatives of United Nations secretariat units and bodies","United Nations Secretariat units and bodies",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="United Nations secretariat units and bodies","United Nations Secretariat units and bodies",COP$Group_Type)                  
COP$Group_Type<-ifelse(COP$Group_Type=="United Nations secretariat units and related bodies","United Nations Secretariat units and bodies",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Spec. agencies&rel.orgs","Specialized agencies and related organizations",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="United Nations specialized agencies and related organizations","United Nations Secretariat units and bodies",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="Intergovernmental orgs.","Intergovernmental organizations",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="Non-governmental orgs.","Non-governmental organizations",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="Party","Parties",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="Spec. agencies&rel.orgs","Specialized agencies and related organizations",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="UN secretariat units & bodies","United Nations Secretariat units and bodies",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="UN Secretariat units & bodies","United Nations Secretariat units and bodies",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="UN Secretariat units and bodies","United Nations Secretariat units and bodies",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="Spec. agencies & related orgs.","Specialized agencies and related organizations",COP$Group_Type)  
COP$Group_Type<-ifelse(COP$Group_Type=="Spec. agencies and related orgs","Specialized agencies and related organizations",COP$Group_Type) 
COP$Group_Type<-ifelse(is.na(COP$Group_Type),"Parties",COP$Group_Type) 

#create binary vars
COP$IGO<-0
COP$IGO[COP$Group=="Intergovernmental organizations"]<-1
COP$NGO<-0
COP$NGO[COP$Group=="Non-governmental organizations"]<-1
COP$Observer<-0
COP$Observer[COP$Group=="Observer States"]<-1
COP$Party<-0
COP$Party[COP$Group=="Parties"]<-1
COP$IO<-0
COP$IO[COP$Group=="Specialized agencies and related organizations"]<-1
COP$IO[COP$Group=="United Nations Secretariat units and bodies"]<-1

#fix a few cases
COP$IGO<-ifelse(COP$Delegation=="Kenya",0,COP$IGO)
COP$NGO<-ifelse(COP$Delegation=="Kenya",0,COP$NGO)
COP$NGO<-ifelse(COP$Delegation=="Kenya",0,COP$NGO)
COP$Party<-ifelse((COP$Delegation=="Turkey" & COP$Meeting=="COP 3"),0,COP$Party)
COP$Party<-ifelse((COP$Delegation=="Yugoslavia" & COP$Meeting=="COP 6"),0,COP$Party)
COP$Party<-ifelse((COP$Delegation=="Kenya" & COP$Party==0 & COP$Observer==0),1,COP$Party)
COP$Party<-ifelse((COP$Delegation=="Yugoslavia"  & COP$Party==0 & COP$Observer==0),1,COP$Party)
COP$Observer<-ifelse((COP$Delegation=="Turkey"  & COP$Party==0 & COP$Observer==0),1,COP$Observer)
COP$IGO<-ifelse(COP$Delegation=="Iucn-The World Conservation Union (Iucn)",0,COP$IGO)
COP$NGO<-ifelse(COP$Delegation=="Iucn-The World Conservation Union (Iucn)",1,COP$NGO)
COP$NGO<-ifelse(COP$Delegation=="International Centre for Research In Agroforestry",0,COP$NGO)
COP$IGO<-ifelse(COP$Delegation=="International Centre for Research In Agroforestry",1,COP$IGO)
COP$IGO<-ifelse(COP$Delegation=="International Center for Tropical Agriculture",0,COP$IGO)
COP$NGO<-ifelse(COP$Delegation=="International Center for Tropical Agriculture",1,COP$NGO)
COP$NGO<-ifelse(COP$Delegation=="Cultural and Technical Cooperation Agency (Acct)",0,COP$NGO)
COP$IGO<-ifelse(COP$Delegation=="Cultural and Technical Cooperation Agency (Acct)",1,COP$IGO)

#set working directory
setwd("")

#save final data
write.csv(COP,"cops.cleaned.translated.csv",row.names=FALSE,fileEncoding = "UTF-8")

#set working directory
setwd("")

##read-in COP data##
COP<-read.csv("cops.cleaned.csv",encoding="UTF-8")

#standardize entries
COP$Group_Type<-ifelse(COP$Group_Type=="InterGovernmental Organizations","Intergovernmental organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="INTERGOVERNMENTAL ORGANIZATIONS","Intergovernmental organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="ORGANISATIONS INTERGOUVERNEMENTALES","Intergovernmental organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="NON-GOVERNMENTAL ORGANIZATIONS","Non-governmental organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="ORGANISATIONS NON-GOUVERNEMENTALES","Non-governmental organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Non-Governmental Organizations","Non-governmental organizations",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="OBSERVER STATES","Observer States",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Observer states","Observer States",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters","Observer States",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Observer states and entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters.","Observer States",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Observer states and entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters\\.","Observer States",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="PARTIES","Parties",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="REPRESENTATIVES","Parties",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Representatives","Parties",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="REPRESENTANTS","Parties",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Representatives of specialized agencies and related organizations","Specialized agencies and related organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND RELATED ORGANIZATIONS","Specialized agencies and related organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="INSTITUTIONS SPECIALISEES ET AUTRES ORGANISATIONS","Specialized agencies and related organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Specialized Agencies and Related Organizations","Specialized agencies and related organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND RELATED ORGANIZATIONS","Specialized agencies and related organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND OTHER ORGANIZATIONS OF THE UNITED NATIONS SYSTEM","Specialized agencies and related organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Representatives of specialized agencies and related organizations","Specialized agencies and related organizations",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="REPRESENTATIVES OF UNITED NATIONS SECRETARIAT UNITS AND BODIES","United Nations Secretariat units and bodies",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="NATIONS UNIES","United Nations Secretariat units and bodies",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Representatives of United Nations Secretariat units and bodies","United Nations Secretariat units and bodies",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="UN Secretariat units and Bodies","United Nations Secretariat units and bodies",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Representatives of United Nations secretariat units and bodies","United Nations Secretariat units and bodies",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="United Nations secretariat units and bodies","United Nations Secretariat units and bodies",COP$Group_Type)                  
COP$Group_Type<-ifelse(COP$Group_Type=="United Nations secretariat units and related bodies","United Nations Secretariat units and bodies",COP$Group_Type)
COP$Group_Type<-ifelse(COP$Group_Type=="Spec. agencies&rel.orgs","Specialized agencies and related organizations",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="United Nations specialized agencies and related organizations","United Nations Secretariat units and bodies",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="Intergovernmental orgs.","Intergovernmental organizations",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="Non-governmental orgs.","Non-governmental organizations",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="Party","Parties",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="Spec. agencies&rel.orgs","Specialized agencies and related organizations",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="UN secretariat units & bodies","United Nations Secretariat units and bodies",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="UN Secretariat units & bodies","United Nations Secretariat units and bodies",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="UN Secretariat units and bodies","United Nations Secretariat units and bodies",COP$Group_Type) 
COP$Group_Type<-ifelse(COP$Group_Type=="Spec. agencies & related orgs.","Specialized agencies and related organizations",COP$Group_Type)  
COP$Group_Type<-ifelse(COP$Group_Type=="Spec. agencies and related orgs","Specialized agencies and related organizations",COP$Group_Type) 
COP$Group_Type<-ifelse(is.na(COP$Group_Type),"Parties",COP$Group_Type) 

#create binary vars
COP$IGO<-0
COP$IGO[COP$Group=="Intergovernmental organizations"]<-1
COP$NGO<-0
COP$NGO[COP$Group=="Non-governmental organizations"]<-1
COP$Observer<-0
COP$Observer[COP$Group=="Observer States"]<-1
COP$Party<-0
COP$Party[COP$Group=="Parties"]<-1
COP$IO<-0
COP$IO[COP$Group=="Specialized agencies and related organizations"]<-1
COP$IO[COP$Group=="United Nations Secretariat units and bodies"]<-1

#fix a few cases
COP$IGO<-ifelse(COP$Delegation=="Kenya",0,COP$IGO)
COP$NGO<-ifelse(COP$Delegation=="Kenya",0,COP$NGO)
COP$NGO<-ifelse(COP$Delegation=="Kenya",0,COP$NGO)
COP$Party<-ifelse((COP$Delegation=="Turkey" & COP$Meeting=="COP 3"),0,COP$Party)
COP$Party<-ifelse((COP$Delegation=="Yugoslavia" & COP$Meeting=="COP 6"),0,COP$Party)
COP$Party<-ifelse((COP$Delegation=="Kenya" & COP$Party==0 & COP$Observer==0),1,COP$Party)
COP$Party<-ifelse((COP$Delegation=="Yugoslavia"  & COP$Party==0 & COP$Observer==0),1,COP$Party)
COP$Observer<-ifelse((COP$Delegation=="Turkey"  & COP$Party==0 & COP$Observer==0),1,COP$Observer)
COP$IGO<-ifelse(COP$Delegation=="Iucn-The World Conservation Union (Iucn)",0,COP$IGO)
COP$NGO<-ifelse(COP$Delegation=="Iucn-The World Conservation Union (Iucn)",1,COP$NGO)
COP$NGO<-ifelse(COP$Delegation=="International Centre for Research In Agroforestry",0,COP$NGO)
COP$IGO<-ifelse(COP$Delegation=="International Centre for Research In Agroforestry",1,COP$IGO)
COP$IGO<-ifelse(COP$Delegation=="International Center for Tropical Agriculture",0,COP$IGO)
COP$NGO<-ifelse(COP$Delegation=="International Center for Tropical Agriculture",1,COP$NGO)
COP$NGO<-ifelse(COP$Delegation=="Cultural and Technical Cooperation Agency (Acct)",0,COP$NGO)
COP$IGO<-ifelse(COP$Delegation=="Cultural and Technical Cooperation Agency (Acct)",1,COP$IGO)

#set working directory
setwd("")

#save final data
write.csv(COP,"cops.cleaned.csv",row.names=FALSE,fileEncoding = "UTF-8")

##read-in PRECOP data##
PRECOP<-read.csv("precops.cleaned.translated.csv",encoding="UTF-8")

#standardize entries
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="InterGovernmental Organizations","Intergovernmental organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="INTERGOVERNMENTAL ORGANIZATIONS","Intergovernmental organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="ORGANISATIONS INTERGOUVERNEMENTALES","Intergovernmental organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="NON-GOVERNMENTAL ORGANIZATIONS","Non-governmental organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="ORGANISATIONS NON-GOUVERNEMENTALES","Non-governmental organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Non-Governmental Organizations","Non-governmental organizations",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="OBSERVER STATES","Observer States",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Observer states","Observer States",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters","Observer States",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Observer states and entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters.","Observer States",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Observer states and entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters\\.","Observer States",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="PARTIES","Parties",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="REPRESENTATIVES","Parties",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Representatives","Parties",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="REPRESENTANTS","Parties",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Representatives of specialized agencies and related organizations","Specialized agencies and related organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND RELATED ORGANIZATIONS","Specialized agencies and related organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="INSTITUTIONS SPECIALISEES ET AUTRES ORGANISATIONS","Specialized agencies and related organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Specialized Agencies and Related Organizations","Specialized agencies and related organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND RELATED ORGANIZATIONS","Specialized agencies and related organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND OTHER ORGANIZATIONS OF THE UNITED NATIONS SYSTEM","Specialized agencies and related organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Representatives of specialized agencies and related organizations","Specialized agencies and related organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="REPRESENTATIVES OF UNITED NATIONS SECRETARIAT UNITS AND BODIES","United Nations Secretariat units and bodies",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="NATIONS UNIES","United Nations Secretariat units and bodies",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Representatives of United Nations Secretariat units and bodies","United Nations Secretariat units and bodies",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="UN Secretariat units and Bodies","United Nations Secretariat units and bodies",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Representatives of United Nations secretariat units and bodies","United Nations Secretariat units and bodies",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="United Nations secretariat units and bodies","United Nations Secretariat units and bodies",PRECOP$Group_Type)                  
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="United Nations secretariat units and related bodies","United Nations Secretariat units and bodies",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Spec. agencies&rel.orgs","Specialized agencies and related organizations",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="United Nations specialized agencies and related organizations","United Nations Secretariat units and bodies",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Intergovernmental orgs.","Intergovernmental organizations",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Non-governmental orgs.","Non-governmental organizations",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Party","Parties",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Spec. agencies&rel.orgs","Specialized agencies and related organizations",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="UN secretariat units & bodies","United Nations Secretariat units and bodies",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="UN Secretariat units & bodies","United Nations Secretariat units and bodies",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="UN Secretariat units and bodies","United Nations Secretariat units and bodies",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Spec. agencies & related orgs.","Specialized agencies and related organizations",PRECOP$Group_Type)  
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Spec. agencies and related orgs","Specialized agencies and related organizations",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(is.na(PRECOP$Group_Type),"Parties",PRECOP$Group_Type) 

#create binary vars
PRECOP$IGO<-0
PRECOP$IGO[PRECOP$Group=="Intergovernmental organizations"]<-1
PRECOP$NGO<-0
PRECOP$NGO[PRECOP$Group=="Non-governmental organizations"]<-1
PRECOP$Observer<-0
PRECOP$Observer[PRECOP$Group=="Observer States"]<-1
PRECOP$Party<-0
PRECOP$Party[PRECOP$Group=="Parties"]<-1
PRECOP$IO<-0
PRECOP$IO[PRECOP$Group=="Specialized agencies and related organizations"]<-1
PRECOP$IO[PRECOP$Group=="United Nations Secretariat units and bodies"]<-1

#fix a few cases
PRECOP$IGO<-ifelse(PRECOP$Delegation=="Kenya",0,PRECOP$IGO)
PRECOP$NGO<-ifelse(PRECOP$Delegation=="Kenya",0,PRECOP$NGO)
PRECOP$NGO<-ifelse(PRECOP$Delegation=="Kenya",0,PRECOP$NGO)
PRECOP$Party<-ifelse((PRECOP$Delegation=="Turkey" & PRECOP$Meeting=="PRECOP 3"),0,PRECOP$Party)
PRECOP$Party<-ifelse((PRECOP$Delegation=="Yugoslavia" & PRECOP$Meeting=="PRECOP 6"),0,PRECOP$Party)
PRECOP$Party<-ifelse((PRECOP$Delegation=="Kenya" & PRECOP$Party==0 & PRECOP$Observer==0),1,PRECOP$Party)
PRECOP$Party<-ifelse((PRECOP$Delegation=="Yugoslavia"  & PRECOP$Party==0 & PRECOP$Observer==0),1,PRECOP$Party)
PRECOP$Observer<-ifelse((PRECOP$Delegation=="Turkey"  & PRECOP$Party==0 & PRECOP$Observer==0),1,PRECOP$Observer)
PRECOP$Delegation<-ifelse((PRECOP$Person_Name=="Emmanuel Katongo" & PRECOP$Meeting=="INC 9"),"Zambia",PRECOP$Delegation)
PRECOP$IGO<-ifelse((PRECOP$Person_Name=="Emmanuel Katongo" & PRECOP$Meeting=="INC 9"),0,PRECOP$IGO)
PRECOP$NGO<-ifelse((PRECOP$Person_Name=="Emmanuel Katongo" & PRECOP$Meeting=="INC 9"),0,PRECOP$NGO)
PRECOP$Delegation_COW<-ifelse((PRECOP$Person_Name=="Emmanuel Katongo" & PRECOP$Meeting=="INC 9"),551,PRECOP$Delegation_COW)
PRECOP$Delegation_ISO<-ifelse((PRECOP$Person_Name=="Emmanuel Katongo" & PRECOP$Meeting=="INC 9"),"ZMB",PRECOP$Delegation_ISO)
PRECOP$IGO<-ifelse(PRECOP$Delegation=="Iucn-The World Conservation Union (Iucn)",0,PRECOP$IGO)
PRECOP$NGO<-ifelse(PRECOP$Delegation=="Iucn-The World Conservation Union (Iucn)",1,PRECOP$NGO)
PRECOP$NGO<-ifelse(PRECOP$Delegation=="International Centre for Research In Agroforestry",0,PRECOP$NGO)
PRECOP$IGO<-ifelse(PRECOP$Delegation=="International Centre for Research In Agroforestry",1,PRECOP$IGO)
PRECOP$IGO<-ifelse(PRECOP$Delegation=="International Center for Tropical Agriculture",0,PRECOP$IGO)
PRECOP$NGO<-ifelse(PRECOP$Delegation=="International Center for Tropical Agriculture",1,PRECOP$NGO)
PRECOP$NGO<-ifelse(PRECOP$Delegation=="Cultural and Technical Cooperation Agency (Acct)",0,PRECOP$NGO)
PRECOP$IGO<-ifelse(PRECOP$Delegation=="Cultural and Technical Cooperation Agency (Acct)",1,PRECOP$IGO)

#set working directory
setwd("")

#save final data
write.csv(PRECOP,"precops.cleaned.translated.csv",row.names=FALSE,fileEncoding = "UTF-8")


##read-in PRECOP data##
PRECOP<-read.csv("precops.cleaned.csv",encoding="UTF-8")

#standardize entries
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="InterGovernmental Organizations","Intergovernmental organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="INTERGOVERNMENTAL ORGANIZATIONS","Intergovernmental organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="ORGANISATIONS INTERGOUVERNEMENTALES","Intergovernmental organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="NON-GOVERNMENTAL ORGANIZATIONS","Non-governmental organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="ORGANISATIONS NON-GOUVERNEMENTALES","Non-governmental organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Non-Governmental Organizations","Non-governmental organizations",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="OBSERVER STATES","Observer States",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Observer states","Observer States",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters","Observer States",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Observer states and entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters.","Observer States",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Observer states and entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters\\.","Observer States",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="PARTIES","Parties",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="REPRESENTATIVES","Parties",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Representatives","Parties",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="REPRESENTANTS","Parties",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Representatives of specialized agencies and related organizations","Specialized agencies and related organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND RELATED ORGANIZATIONS","Specialized agencies and related organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="INSTITUTIONS SPECIALISEES ET AUTRES ORGANISATIONS","Specialized agencies and related organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Specialized Agencies and Related Organizations","Specialized agencies and related organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND RELATED ORGANIZATIONS","Specialized agencies and related organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND OTHER ORGANIZATIONS OF THE UNITED NATIONS SYSTEM","Specialized agencies and related organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Representatives of specialized agencies and related organizations","Specialized agencies and related organizations",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="REPRESENTATIVES OF UNITED NATIONS SECRETARIAT UNITS AND BODIES","United Nations Secretariat units and bodies",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="NATIONS UNIES","United Nations Secretariat units and bodies",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Representatives of United Nations Secretariat units and bodies","United Nations Secretariat units and bodies",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="UN Secretariat units and Bodies","United Nations Secretariat units and bodies",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Representatives of United Nations secretariat units and bodies","United Nations Secretariat units and bodies",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="United Nations secretariat units and bodies","United Nations Secretariat units and bodies",PRECOP$Group_Type)                  
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="United Nations secretariat units and related bodies","United Nations Secretariat units and bodies",PRECOP$Group_Type)
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Spec. agencies&rel.orgs","Specialized agencies and related organizations",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="United Nations specialized agencies and related organizations","United Nations Secretariat units and bodies",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Intergovernmental orgs.","Intergovernmental organizations",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Non-governmental orgs.","Non-governmental organizations",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Party","Parties",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Spec. agencies&rel.orgs","Specialized agencies and related organizations",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="UN secretariat units & bodies","United Nations Secretariat units and bodies",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="UN Secretariat units & bodies","United Nations Secretariat units and bodies",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="UN Secretariat units and bodies","United Nations Secretariat units and bodies",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Spec. agencies & related orgs.","Specialized agencies and related organizations",PRECOP$Group_Type)  
PRECOP$Group_Type<-ifelse(PRECOP$Group_Type=="Spec. agencies and related orgs","Specialized agencies and related organizations",PRECOP$Group_Type) 
PRECOP$Group_Type<-ifelse(is.na(PRECOP$Group_Type),"Parties",PRECOP$Group_Type) 

#create binary vars
PRECOP$IGO<-0
PRECOP$IGO[PRECOP$Group=="Intergovernmental organizations"]<-1
PRECOP$NGO<-0
PRECOP$NGO[PRECOP$Group=="Non-governmental organizations"]<-1
PRECOP$Observer<-0
PRECOP$Observer[PRECOP$Group=="Observer States"]<-1
PRECOP$Party<-0
PRECOP$Party[PRECOP$Group=="Parties"]<-1
PRECOP$IO<-0
PRECOP$IO[PRECOP$Group=="Specialized agencies and related organizations"]<-1
PRECOP$IO[PRECOP$Group=="United Nations Secretariat units and bodies"]<-1

#fix a few cases
PRECOP$IGO<-ifelse(PRECOP$Delegation=="Kenya",0,PRECOP$IGO)
PRECOP$NGO<-ifelse(PRECOP$Delegation=="Kenya",0,PRECOP$NGO)
PRECOP$NGO<-ifelse(PRECOP$Delegation=="Kenya",0,PRECOP$NGO)
PRECOP$Party<-ifelse((PRECOP$Delegation=="Turkey" & PRECOP$Meeting=="PRECOP 3"),0,PRECOP$Party)
PRECOP$Party<-ifelse((PRECOP$Delegation=="Yugoslavia" & PRECOP$Meeting=="PRECOP 6"),0,PRECOP$Party)
PRECOP$Party<-ifelse((PRECOP$Delegation=="Kenya" & PRECOP$Party==0 & PRECOP$Observer==0),1,PRECOP$Party)
PRECOP$Party<-ifelse((PRECOP$Delegation=="Yugoslavia"  & PRECOP$Party==0 & PRECOP$Observer==0),1,PRECOP$Party)
PRECOP$Observer<-ifelse((PRECOP$Delegation=="Turkey"  & PRECOP$Party==0 & PRECOP$Observer==0),1,PRECOP$Observer)
PRECOP$Delegation<-ifelse((PRECOP$Person_Name=="Emmanuel Katongo" & PRECOP$Meeting=="INC 9"),"Zambia",PRECOP$Delegation)
PRECOP$IGO<-ifelse((PRECOP$Person_Name=="Emmanuel Katongo" & PRECOP$Meeting=="INC 9"),0,PRECOP$IGO)
PRECOP$NGO<-ifelse((PRECOP$Person_Name=="Emmanuel Katongo" & PRECOP$Meeting=="INC 9"),0,PRECOP$NGO)
PRECOP$Delegation_COW<-ifelse((PRECOP$Person_Name=="Emmanuel Katongo" & PRECOP$Meeting=="INC 9"),551,PRECOP$Delegation_COW)
PRECOP$Delegation_ISO<-ifelse((PRECOP$Person_Name=="Emmanuel Katongo" & PRECOP$Meeting=="INC 9"),"ZMB",PRECOP$Delegation_ISO)
PRECOP$IGO<-ifelse(PRECOP$Delegation=="Iucn-The World Conservation Union (Iucn)",0,PRECOP$IGO)
PRECOP$NGO<-ifelse(PRECOP$Delegation=="Iucn-The World Conservation Union (Iucn)",1,PRECOP$NGO)
PRECOP$NGO<-ifelse(PRECOP$Delegation=="International Centre for Research In Agroforestry",0,PRECOP$NGO)
PRECOP$IGO<-ifelse(PRECOP$Delegation=="International Centre for Research In Agroforestry",1,PRECOP$IGO)
PRECOP$IGO<-ifelse(PRECOP$Delegation=="International Center for Tropical Agriculture",0,PRECOP$IGO)
PRECOP$NGO<-ifelse(PRECOP$Delegation=="International Center for Tropical Agriculture",1,PRECOP$NGO)
PRECOP$NGO<-ifelse(PRECOP$Delegation=="Cultural and Technical Cooperation Agency (Acct)",0,PRECOP$NGO)
PRECOP$IGO<-ifelse(PRECOP$Delegation=="Cultural and Technical Cooperation Agency (Acct)",1,PRECOP$IGO)

#set working directory
setwd("")

#save final data
write.csv(PRECOP,"precops.cleaned.csv",row.names=FALSE,fileEncoding = "UTF-8")



