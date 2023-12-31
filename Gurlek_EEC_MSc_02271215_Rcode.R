# Packages: ####
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(pROC)
library(ade4)
library(arm)
library(glmtoolbox)
library(ggplot2)
library(regclass)

# Data frames: ####
#Trait data:
# Put the file path below 
data_trait<-read_xlsx("/Users/sinangurlek/Downloads/iCloud Drive (Arşiv)/Documents/IC/Project/MScProject_Data_SinanGurlek/FunTrait_data_SG.xlsx", sheet=1)

#IUCN Red List data 
# Put the file path below 
IUCN<-read_xlsx("/Users/sinangurlek/Downloads/iCloud Drive (Arşiv)/Documents/IC/Project/MScProject_Data_SinanGurlek/IUCN_data_SG.xlsx", sheet=1)
IUCN<-IUCN[,c(1,3)] #selecting important columns
colnames(IUCN)<-c("taxon","IUCN Rating") #changin column names

# Function for IUCN data conversion: ####

data_fin2<-as_tibble(merge(data_trait, IUCN, by="taxon")) #merging trait and IUCN RL data
data_fin3<-data_fin2[!duplicated(data_fin2$taxon),] #removing duplicated species names

# Creating a function that puts the IUCN RL assessments into binary variables for threatened and non-threatened (aditionally as continuous variable from 0 to 6, not used)
fun_IUCN<-function(a,b){
  data_new<-a
  data_new$conIUCN<-3
  data_new$stateIUCN<-3
  for(i in c(1:nrow(data_new))) {
    if (data_new[i,]$`IUCN Rating`=="LC"){
      data_new[i,]$conIUCN<-6
      data_new[i,]$stateIUCN<-0
    } else if (data_new[i,]$`IUCN Rating`=="NT"){
      data_new[i,]$conIUCN<-5
      data_new[i,]$stateIUCN<-0
    } else if (data_new[i,]$`IUCN Rating`=="VU"){
      data_new[i,]$conIUCN<-4
      data_new[i,]$stateIUCN<-1
    }else if (data_new[i,]$`IUCN Rating`=="DD"){
      data_new[i,]$conIUCN<-NA
      data_new[i,]$stateIUCN<-NA
    } else if (data_new[i,]$`IUCN Rating`=="EN"){
      data_new[i,]$conIUCN<-3
      data_new[i,]$stateIUCN<-1
    } else if (data_new[i,]$`IUCN Rating`=="CR"){
      data_new[i,]$conIUCN<-2
      data_new[i,]$stateIUCN<-1
    } else if (data_new[i,]$`IUCN Rating`=="EX"){
      data_new[i,]$conIUCN<-1
      data_new[i,]$stateIUCN<-1
    } else if (data_new[i,]$`IUCN Rating`=="Threatened"){
      data_new[i,]$conIUCN<--1
      data_new[i,]$stateIUCN<-1
    } else {
      data_new[i,]$conIUCN<-0
      data_new[i,]$stateIUCN<-NA
    }
  }
  assign(b, value=data_new, envir = .GlobalEnv)
} # a=df to be used, b=new df name

fun_IUCN(data_fin3, "data_fin4")

# Function for "sporophyte presence" filling: ####
# Creating a function that checks whether the sporophyte is present based on sporophyte-related traits
fun_Sporophyte<-function(a,b){
  datax<-a
  for (i in c(1:nrow(datax))){
    if (is.na(datax$`capsule min. length [mm]`[i])==FALSE ||is.na(datax$`capsule max. length [mm]`[i])==FALSE ||
        is.na(datax$`capsule min. width [mm]`[i])==FALSE ||is.na(datax$`capsule max. width [mm]`[i])==FALSE ||
        is.na(datax$`capsule shape`[i])==FALSE ||is.na(datax$`capsule on seta`[i])==FALSE ||
        is.na(datax$`seta shape`[i])==FALSE ||is.na(datax$`spore shape`[i])==FALSE ||
        is.na(datax$`spore ornamentation`[i])==FALSE || is.na(datax$`seta min. length [mm]`[i])==FALSE ||
        is.na(datax$`seta max. length [mm]`[i])==FALSE){
      datax$`sporophyte presence`[i]<-"sporophyte"
    } else {datax$`sporophyte presence`[i]<-datax$`sporophyte presence`[i]}
  }
  assign(b, datax, envir = .GlobalEnv)
} # a=df to be filled, b=new df name

fun_Sporophyte(data_fin4, "data_fin5")

# Function for continuous traits mean: ####

# Creating a function that takes minimum and maximum values of continuous variables and returns mean values for analysis
fun_ConMean<-function(a,b,c,d,e){
  new_data<-a
  new_data[,ncol(new_data) + 1] <-0
  colnames(new_data)[ncol(new_data)]<-c(d)
  for (i in c(1:nrow(new_data))){
    if (is.na(new_data[i,b])==TRUE && is.na(new_data[i,c])==TRUE){
      new_data[i,ncol(new_data)]<-NA
    } else {new_data[i,ncol(new_data)]<- sum(as.numeric(new_data[i,b]),as.numeric(new_data[i,c]))/2}
  }
  assign(e, value=new_data, envir = .GlobalEnv)
}# a=df to be used, b/c= column to be used, d=name of new column, e=name of the new df

fun_ConMean(data_fin5, 12, 13, "leaf_length(cm)", "data_fin6") #Repeating the same conversion for different functional traits
fun_ConMean(data_fin6, 10, 11, "leaf_width(cm)", "data_fin6")
fun_ConMean(data_fin6, 15, 16, "capsule_length(mm)", "data_fin6")
fun_ConMean(data_fin6, 17, 18, "capsule_width(mm)", "data_fin6")
fun_ConMean(data_fin6, 22, 23, "seta_length(mm)", "data_fin6")
fun_ConMean(data_fin6, 27, 28, "spore_diameter(microm)", "data_fin6")
fun_ConMean(data_fin6, 30, 31, "stem_length(mm)", "data_fin6")

# Function to fill NAs with absence: ####
# Creating a function that fills the empty rows as "no" for binary traits
fun_Absence<-function(a,b,c,d){
  data_new<-a
  for (i in c(1:nrow(data_new))){
    if (is.na(data_new[i,b])==TRUE){
      data_new[i,b]<-paste(c)
    }
  }
  assign(d, value=data_new, envir = .GlobalEnv)
}# a=df to be used, b=column number of the desired trait, c=word or number to be written, d=new df name

fun_Absence(data_fin6, 14, "no", "data_fin7") #Repeating for different functional traits
fun_Absence(data_fin7, 34, "no", "data_fin7")
fun_Absence(data_fin7, 29, "no", "data_fin7")

# Functions for cleaning the trait data: ####

# Function for changing two attributes into one and clusters if both are present
fun_MergeTrait2<-function(a,b,c,d,e,f){
  new_col<<-a[,b]
  pat<-c(c,d)
  for (i in c(1:nrow(new_col))){
    col_att<-strsplit(as.character(new_col[i,1]),split = ", ")
    col_att<-unlist(col_att)
    if (is.na(new_col[i,1])==TRUE){
      new_col[i,1]<<-NA
    } else if(str_detect(new_col[i,1], pattern=c)==TRUE && str_detect(new_col[i,1], pattern=d)==TRUE){
      filtered_elements <- col_att[!(col_att %in% c(d,c))]
      filtered_elements <- c(filtered_elements, e)
      new_string <- paste(filtered_elements, collapse = ", ")
      new_col[i,1]<<-new_string
    } else if (as.character(str_detect(new_col[i,1], pattern=c))==TRUE || as.character(str_detect(new_col[i,1], pattern=d))==TRUE){
      filtered_elements <- col_att[!(col_att %in% c(d,c))]
      filtered_elements <- c(filtered_elements, e)
      new_string <- paste(filtered_elements, collapse = ", ")
      new_col[i,1]<<-new_string
    }
  }
  a[,b]<-new_col[,1]
  dummy<-a
  assign(f, value=dummy, envir = .GlobalEnv)
}# a=initial df, b=column number to be coerced, c/d=attributes (chr) to be coerced, e=name of the coerced new attribute, f=name for the new df  

# Function similar to the one above, but only merges when both attributes are present
fun_MergeTrait3<-function(a,b,c,d,e,f){
  new_col<<-a[,b]
  pat<-c(c,d)
  for (i in c(1:nrow(new_col))){
    col_att<-strsplit(as.character(new_col[i,1]),split = ", ")
    col_att<-unlist(col_att)
    if (is.na(new_col[i,1])==TRUE){
      new_col[i,1]<<-NA
    } else if(str_detect(new_col[i,1], pattern=c)==TRUE && str_detect(new_col[i,1], pattern=d)==TRUE){
      filtered_elements <- col_att[!(col_att %in% c(d,c))]
      filtered_elements <- c(filtered_elements, e)
      new_string <- paste(filtered_elements, collapse = ", ")
      new_col[i,1]<<-new_string
    } 
  }
  a[,b]<-new_col[,1]
  dummy<-a
  assign(f, value=dummy, envir = .GlobalEnv)
}# a=initial df, b=column number to be coerced, c/d=attributes (chr) to be coerced, e=name of the coerced new attribute, f=name for the new df  

# Function that deletes an attribute with no substitution
fun_DeleteTrait<-function(a,b,c,d){
  new_col<<-a[,b]
  pat<-c(c)
  for (i in c(1:nrow(new_col))){
    col_att<-strsplit(as.character(new_col[i,1]),split = ", ")
    col_att<-unlist(col_att)
    if (is.na(new_col[i,1])==TRUE){
      new_col[i,1]<<-NA
    } else if(str_detect(new_col[i,1], pattern=pat)==TRUE){
      filtered_elements <- col_att[!(col_att %in% pat)]
      new_string <- paste(filtered_elements, collapse = ", ")
      new_col[i,1]<<-new_string
      if (new_col[i,1]==""){new_col[i,1]<<-NA} 
    } 
  }
  a[,b]<-new_col[,1]
  dummy<-a
  assign(d, value=dummy, envir = .GlobalEnv)
}# a=initial df, b=column number to be coerced, c=attributes (chr) to be deleted, d=new df name

# Trait data cleaning: ####
# For Substrate information # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
fun_MergeTrait2(data_fin7, 4, "terricolous","terricolouss", "terricolous", "data_fin8")
fun_MergeTrait2(data_fin8, 4, "base of trees","corticolous", "corticolous", "data_fin8")

unique(unlist(data_fin8$substrate))
unique(unlist(strsplit(data_fin8$substrate, split= ", ")))

# For the Stem growth # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
fun_MergeTrait2(data_fin8, 2, "prostrate","procumbent", "creeping", "data_fin9")
fun_MergeTrait2(data_fin9, 2, "pendent","creeping", "creeping", "data_fin9")
fun_MergeTrait2(data_fin9, 2, "decumbent","ascending", "ascending", "data_fin9")

fun_MergeTrait3(data_fin9, 2, "ascending","erect", "ascending-erect", "data_fin10")
fun_MergeTrait3(data_fin10, 2, "ascending","creeping", "ascending-creeping", "data_fin10")

unique(unlist(data_fin10$`stem growth`))
unique(unlist(strsplit(data_fin10$`stem growth`, split= ", ")))

table(data_fin10$`stem growth`)

# For  the Plant sex # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
fun_DeleteTrait(data_fin10, 5, "sterile", "data_fin10")

data_fin10$plantSex2<-"a"
for (i in c(1:nrow(data_fin10))){
  if (is.na(data_fin10[i,5])==TRUE){
    data_fin10$plantSex2[i]<-NA
  } else if (data_fin10[i,5]== "dioecious" || data_fin10[i,5]=="rhizautoicous"|| data_fin10[i,5]=="polyoicous"){
    data_fin10$plantSex2[i]<-"dioecious1"
  } else {data_fin10$plantSex2[i]<-"monoicous1"}
}

unique(unlist(data_fin10$`plantSex2`))
unique(unlist(strsplit(data_fin10$`plant sex`, split= ", ")))

table(data_fin10$`plantSex2`)

# For the Capsule shape # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
fun_DeleteTrait(data_fin10, 19, "urn", "data_fin11")
fun_MergeTrait2(data_fin11, 19, "pyriform", "ovate", "globose", "data_fin11")
fun_MergeTrait2(data_fin11, 19, "ovoid", "ovate", "globose", "data_fin11")
fun_MergeTrait2(data_fin11, 19, "eliptic", "spherical", "globose", "data_fin11")
fun_MergeTrait2(data_fin11, 19, "globose", "globose", "globose", "data_fin11")

fun_MergeTrait3(data_fin11, 19, "cylindric", "oblong", "oblong-cylindric", "data_fin11")
fun_MergeTrait3(data_fin11, 19, "oblong-cylindric", "oblong-cylindric", "oblong-cylindric", "data_fin11")

fun_MergeTrait3(data_fin11, 19, "cylindric", "globose", "sub-cylindric", "data_fin11")
fun_MergeTrait3(data_fin11, 19, "sub-cylindric", "sub-cylindric", "sub-cylindric", "data_fin11")

fun_MergeTrait3(data_fin11, 19, "oblong", "globose", "obovoid", "data_fin11")

unique(unlist(data_fin11$`capsule shape`))
unique(unlist(strsplit(data_fin11$`capsule shape`, split= ", ")))

table(data_fin11$`capsule shape`)

# For the Seta shape # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
fun_MergeTrait2(data_fin11, 21, "straight", "erect", "straight", "data_fin12")
fun_MergeTrait2(data_fin12, 21, "curved", "inclined", "curved", "data_fin12")
fun_MergeTrait3(data_fin12, 21, "straight", "curved", "straight-curved", "data_fin12")
fun_MergeTrait3(data_fin12, 21, "straight", "twisted", "straight-twisted", "data_fin12")

for (i in c(1:nrow(data_fin12))){
  if (is.na(data_fin12[i,21])==TRUE) {
    data_fin12[i,21]<-NA
  } else if (data_fin12[i,21]== "straight-curved, straight-twisted"){
    data_fin12[i,21]<-"twisted"
  } else {data_fin12[i,21]<-data_fin12[i,21]}
}

for (i in c(1:nrow(data_fin12))){
  if (is.na(data_fin12[i,21])==TRUE){
    data_fin12[i,21]<-NA
  } else if (data_fin12[i,21]=="curved"){
    data_fin12[i,21]<-"curved"
  } else if (data_fin12[i,21]=="twisted"){
    data_fin12[i,21]<-"curved"
  } else if (data_fin12[i,21]=="straight-curved"){
    data_fin12[i,21]<-"curved"
  } else if (data_fin12[i,21]=="straight-twisted"){
    data_fin12[i,21]<-"curved"
  } else if (data_fin12[i,21]=="straight"){
    data_fin12[i,21]<-"straight"
  } else if (data_fin12[i,21]=="exserted"){
    data_fin12[i,21]<-"exserted"
  } else (data_fin12[i,21]<-NA)
}

unique(unlist(data_fin12$`seta shape`))
unique(unlist(strsplit(data_fin12$`seta shape`, split= ", ")))

table(data_fin12$`seta shape`)

# For the Spore shape # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
fun_MergeTrait2(data_fin12, 25, "spherical", "round", "spherical", "data_fin13")
fun_MergeTrait2(data_fin13, 25, "ellipsoid", "ovoid", "ellipsoid", "data_fin13")
fun_MergeTrait2(data_fin13, 25, "trilete", "tetrad", "tetrahedral", "data_fin13")
fun_MergeTrait2(data_fin13, 25, "tetrahedral", "tetrahedral", "tetrahedral", "data_fin13")

unique(unlist(data_fin13$`spore shape`))
unique(unlist(strsplit(data_fin13$`spore shape`, split= ", ")))

table(data_fin13$`spore shape`, data_fin13$stateIUCN)

# For the Spore ornamentation # # # # # # # # # # # # # # # # # # # # # # # # # 
fun_DeleteTrait(data_fin13, 26, "spheric", "data_fin14")
fun_MergeTrait2(data_fin14, 26, "finely papillose", "smooth", "low_orn", "data_fin14")
fun_MergeTrait2(data_fin14, 26, "minutely papillose", "nearly smooth", "low_orn", "data_fin14")
fun_MergeTrait2(data_fin14, 26, "warty", "granular", "high_orn", "data_fin14")
fun_MergeTrait2(data_fin14, 26, "tuberculate", "spinulose", "high_orn", "data_fin14")
fun_MergeTrait2(data_fin14, 26, "papillose", "reticulate", "high_orn", "data_fin14")
fun_MergeTrait2(data_fin14, 26, "granulate", "high_orn", "high_orn", "data_fin14")
fun_MergeTrait3(data_fin14, 26, "high_orn", "high_orn", "high_orn", "data_fin14")
fun_MergeTrait3(data_fin14, 26, "low_orn", "low_orn", "low_orn", "data_fin15")

for (i in c(1:nrow(data_fin15))){
  if (is.na(data_fin15[i,26])==TRUE) {
    data_fin15[i,26]<-NA
  } else if (data_fin15[i,26]== "high_orn, low_orn"){
    data_fin15[i,26]<-"mid_orn"
  } else {data_fin15[i,26]<-data_fin15[i,26]}
}

unique(unlist(data_fin15$`spore ornamentation`))
unique(unlist(strsplit(data_fin15$`spore ornamentation`, split= ", ")))

table(data_fin15$`spore ornamentation`)

# For the Capsule on seta # # # # # # # # # # # # # # # # # # # # # # # # # # # 
fun_MergeTrait2(data_fin15, 20, "erect", "straight", "straight", "data_fin16")
fun_MergeTrait3(data_fin16, 20, "inclined", "horizontal", "arcuate", "data_fin16")
fun_MergeTrait3(data_fin16, 20, "inclined", "pendulose", "arcuate", "data_fin16")
fun_MergeTrait3(data_fin16, 20, "horizontal", "pendulose", "arcuate", "data_fin16")
fun_MergeTrait3(data_fin16, 20, "inclined", "arcuate", "arcuate", "data_fin16")
fun_MergeTrait3(data_fin16, 20, "horizontal", "arcuate", "arcuate", "data_fin16")
fun_MergeTrait3(data_fin16, 20, "pendulose", "arcuate", "arcuate", "data_fin16")
fun_MergeTrait3(data_fin16, 20, "arcuate", "arcuate", "arcuate", "data_fin16")
fun_MergeTrait3(data_fin16, 20, "inclined", "straight", "exserted", "data_fin16")
fun_MergeTrait3(data_fin16, 20, "horizontal", "straight", "exserted", "data_fin16")
fun_MergeTrait3(data_fin16, 20, "pendulose", "straight", "exserted", "data_fin16")
fun_MergeTrait3(data_fin16, 20, "pendulose", "exserted", "exserted", "data_fin16")
fun_MergeTrait3(data_fin16, 20, "horizontal", "exserted", "exserted", "data_fin16")
fun_MergeTrait3(data_fin16, 20, "inclined", "exserted", "exserted", "data_fin16")
fun_MergeTrait3(data_fin16, 20, "straight", "exserted", "exserted", "data_fin16")

fun_MergeTrait3(data_fin16, 20, "exserted", "immersed", "exserted", "data_fin17")
fun_MergeTrait3(data_fin17, 20, "straight", "immersed", "exserted", "data_fin17")
fun_MergeTrait3(data_fin17, 20, "exserted", "exserted", "exserted", "data_fin17")
fun_MergeTrait3(data_fin17, 20, "straight", "arcuate", "exserted", "data_fin17")
fun_MergeTrait3(data_fin17, 20, "exserted", "arcuate", "exserted", "data_fin17")

data_fin17$CapsuleonSeta2<-"a"

for (i in c(1:nrow(data_fin17))){
  if (is.na(data_fin17[i,20])==TRUE){
    data_fin17[i,48]<-NA
  } else if (data_fin17[i,20]=="straight"){
    data_fin17[i,48]<-"exserted"
  } else if (data_fin17[i,20]=="inclined"){
    data_fin17[i,48]<-"exserted"
  } else if (data_fin17[i,20]=="horizontal"){
    data_fin17[i,48]<-"exserted"
  } else if (data_fin17[i,20]=="pendulose"){
    data_fin17[i,48]<-"exserted"
  } else if (data_fin17[i,20]=="immersed"){
    data_fin17[i,48]<-"immersed"
  } else if (data_fin17[i,20]=="exserted"){
    data_fin17[i,48]<-"exserted"
  } else if (data_fin17[i,20]=="arcuate"){
    data_fin17[i,48]<-"exserted"
  }else (data_fin17[i,48]<-NA)
}

unique(unlist(data_fin17$`capsule on seta`))
unique(unlist(data_fin17$CapsuleonSeta2))
unique(unlist(strsplit(data_fin17$`capsule on seta`, split= ", ")))

table(data_fin17$`capsule on seta`)

# Factor conversion for sporophyte presence trait # # # # # # # # # # # # # # # 
data_fin17$`sporophyte presence`<-as.factor(data_fin17$`sporophyte presence`)
levels(data_fin17$`sporophyte presence`)<-c(0,1)

# Factor conversion for vegetative reproduction presence trait # # # # # # # # #
data_fin17$`vegetative reproduction presence`<-as.factor(data_fin17$`vegetative reproduction presence`)
levels(data_fin17$`vegetative reproduction presence`)<-c(0,1)

# Factor conversion for permanent protonema presence trait # # # # # # # # # # # 
data_fin17$`permanent protonema`<-as.factor(data_fin17$`permanent protonema`)
levels(data_fin17$`permanent protonema`)<-c(0,1)

# Factor conversion IUCN RL status # # # # # # # # # # # # # # # # # # # # # # # 
data_fin17$stateIUCN<-as.factor(data_fin17$stateIUCN)
levels(data_fin17$stateIUCN)<-c(0,1)

# Function for Substrate breadth: ####
# Creating a function that sums the different substrates a species can be found
fun_SubstrateBreadth<-function(a,b){
  datax<-a
  datax$SubstrateBreadth<-as.numeric(0)
  for(i in c(1:nrow(datax))){
    x1<-str_split(datax$substrate[i], pattern= ", ")
    x2<-unlist(x1)
    if (is.na(datax$substrate[i])==TRUE){
      datax$SubstrateBreadth[i]<-NA
    } else {
      datax$SubstrateBreadth[i]<-as.numeric(length(x2))
    }
  }
  assign(b, datax, envir = .GlobalEnv)
}#a=df to get Substrate breadth, b=name for the new df

fun_SubstrateBreadth(data_fin17, "data_fin18")

# Individual GLMs: ####
# Here, the final data cleaning was done 
data_glm<-data_fin18
data_glm2<-data_glm[complete.cases(data_glm$stateIUCN),] #deleting non-filled IUCN RL species
nrow(data_glm2)

# Trait table function used for categorical and binary functional traits:
table(data_glm2$CapsuleonSeta2, na.omit(data_glm2$stateIUCN))
length(na.omit(data_glm2$`sporophyte presence`))

# Mean, standard deviation, min. and max. formula used for continuous variables
a<-data_glm2$SubstrateBreadth
mean(a, na.rm=T)
sd(a, na.rm=T)
min(a, na.rm=T)
max(a, na.rm=T)
length(na.omit(a))

# Doing individual GLMs for each trait
glmX<-glm(stateIUCN~plantSex2, data=data_glm2, family=binomial())
glmX<-glm(stateIUCN~SubstrateBreadth, data=data_glm2, family=binomial())
glmX<-glm(stateIUCN~`vegetative reproduction presence`, data=data_glm2, family=binomial())
glmX<-glm(stateIUCN~`sporophyte presence`, data=data_glm2, family=binomial())
glmX<-glm(stateIUCN~`stem_length(mm)`, data=data_glm2, family=binomial())
glmX<-glm(stateIUCN~`stem growth`, data=data_glm2, family=binomial())
glmX<-glm(stateIUCN~`leaf_length(cm)`, data=data_glm2, family=binomial())
glmX<-glm(stateIUCN~`permanent protonema`, data=data_glm2, family=binomial())
glmX<-glm(stateIUCN~`capsule_length(mm)`, data=data_glm2, family=binomial())
glmX<-glm(stateIUCN~CapsuleonSeta2, data=data_glm2, family=binomial())
glmX<-glm(stateIUCN~`capsule shape`, data=data_glm2, family=binomial())
glmX<-glm(stateIUCN~`seta_length(mm)`, data=data_glm2, family=binomial())
glmX<-glm(stateIUCN~`seta shape`, data=data_glm2, family=binomial())
glmX<-glm(stateIUCN~`spore_diameter(microm)`, data=data_glm2, family=binomial())
glmX<-glm(stateIUCN~`spore shape`, data=data_glm2, family=binomial())
glmX<-glm(stateIUCN~`spore ornamentation`, data=data_glm2, family=binomial())

summary(glmX) # For coefficient estimates, AIC and df information
summary(glmX)$coefficients[,2]*1.96 # 95% confidence interval values
predictedX <- predict(glmX, data=glmX$model, type="response") #Predicting the IUCN RL status using the models
auc(glmX$model$stateIUCN, predictedX) # AUC calculation

# Checking correlation for categorical functional trait with more than 2 attributes:
# Post-hoc for Stem growth trait
contxY<-as.data.frame.matrix(table(data_glm2$`stem growth`, data_glm2$stateIUCN))
contxY1<-as.matrix(contxY)
pairwise.prop.test(contxY1,p.adjust.method = "none")

# Post-hoc for Capsule shape trait
contxZ<-as.data.frame.matrix(table(data_glm2$`capsule shape`, data_glm2$stateIUCN))
contxZ1<-as.matrix(contxZ)
pairwise.prop.test(contxZ1,p.adjust.method = "none")

# Post-hoc for Spore shape trait
contxW<-as.data.frame.matrix(table(data_glm2$`spore shape`, data_glm2$stateIUCN))
contxW1<-as.matrix(contxW)
pairwise.prop.test(contxW1,p.adjust.method = "none")

# Post-hoc for Spore ornamentation trait
contxQ<-as.data.frame.matrix(table(data_glm2$`spore ornamentation`, data_glm2$stateIUCN))
contxQ1<-as.matrix(contxQ)
pairwise.prop.test(contxQ1,p.adjust.method = "none")

# Relationships between traits (plant sex, sporophyte presence and vegetative reproduction): ####
# Chi-squarred for Vegetative reproduction and Sporophyte presence
contx1<-as.data.frame.matrix(table(data_glm2$`vegetative reproduction presence`, data_glm2$`sporophyte presence`))
chi_results1<-chisq.test(contx1)

# Chi-squarred for Vegetative reproduction and plantSex2
contx2<-as.data.frame.matrix(table(data_glm2$`vegetative reproduction presence`, data_glm2$plantSex2))
chi_results2<-chisq.test(contx2)
residuals(chi_results2)

# Chi-squarred for plantSex2 and Sporophyte presence
contx3<-as.data.frame.matrix(table(data_glm2$plantSex2, data_glm2$`sporophyte presence`))
chi_results3<-chisq.test(contx3)

# Chi-squarred for Vegetative reproduction and IUCN status
contx4<-as.data.frame.matrix(table(data_glm2$`vegetative reproduction presence`, data_glm2$stateIUCN))
chi_results4<-chisq.test(contx4)
residuals(chi_results2)

# Plot for GLM estimates (Figure 1): ####
x_values1 <- c(1, 2, 3, 4, 5,6,7,8,9,10,11,12,13)
estimates1 <-c(0.60, -1.05, -0.82, -1.56, -0.005, 1.08, 0.42, 1.17, 0.72,-1.58, -0.4,-0.91,0.736)
CI1 <- c(0.55, 0.52, 0.94, 0.67, 1.009, 1.15, 1.17, 1.34, 0.89, 1.89, 2.05,0.596,1.26)

x_values2 <- c(1, 2, 3, 4, 5,6,7,8,9,10,11,12,13)
estimates2 <-c(-0.0636,0.44,-1.41,-0.74,-0.181, -0.03725, 0.57, 0.00638, 0.69, -0.478, 0.051, 0.098, -1.298)
CI2 <- c(0.872,1.339,2.065,2.083,0.966,0.037,0.915,0.01805,3.03, 2.12, 2.27,0.8123, 2.094)

data_figure1 <- data.frame(x_values1, estimates1, CI1)
data_figure2 <- data.frame(x_values2, estimates2, CI2)

ggplot(data_figure1, aes(x = x_values1, y = estimates1)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimates1-CI1, ymax = estimates1+CI1), width = 0.2) +
  labs(x = "X-axis Label", y = "Estimates") +
  ggtitle("Estimates and 95% Confidence Intervals Table 1") +
  theme_minimal()

ggplot(data_figure2, aes(x = x_values2, y = estimates2)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimates2-CI2, ymax = estimates2+CI2), width = 0.2) +
  labs(x = "X-axis Label", y = "Estimates") +
  ggtitle("Estimates and 95% Confidence Intervals Table 2") +
  theme_minimal()

# GLM Model Selection: ####
# Model 1: Without sporophyte traits # # # # # # # # # # # # # # # # # # # # # #

glm_Max1<-glm(stateIUCN~plantSex2+`sporophyte presence`+scale(SubstrateBreadth), data=data_glm2, family=binomial())
summary(glm_Max1) #model summary for maximal model 1
summary(glm_Max1)$coefficients[,2]*1.96 #Standard deviation for maximal model 1
VIF(glm_Max1) # Checking VIF, all under 2, good
nrow(subset(glm_Max1$model,glm_Max1$model$stateIUCN==0)) # checking non-threatened species number in the model data
nrow(subset(glm_Max1$model,glm_Max1$model$stateIUCN==1)) # checking threatened species number in the model data
predicted1 <- predict(glm_Max1, data=glm_Max1$model, type="response") # Predicting the IUCN RL status with the model
auc(glm_Max1$model$stateIUCN, predicted1) # Model AUC

stepCriterion(glm_Max1, direction = "forward", criterion = "aic") # Forward, AIC-based model selection

# Running every model from the stepCriterion results:
data_glm_MAX1.1<-na.omit(data_glm2[,c(47,49,14,39)]) #deleting all the empty data for traits used 
nrow(subset(data_glm_MAX1.1, data_glm_MAX1.1$stateIUCN==0)) # checking non-threatened species number in the model data
nrow(subset(data_glm_MAX1.1, data_glm_MAX1.1$stateIUCN==1)) # checking threatened species number in the model data
glm3.1<-glm(stateIUCN~plantSex2+`sporophyte presence`+scale(SubstrateBreadth), data=data_glm_MAX1.1, family=binomial())
glm2.1<-glm(stateIUCN~`sporophyte presence`+scale(SubstrateBreadth), data=data_glm_MAX1.1, family=binomial())
glm1.1<-glm(stateIUCN~scale(SubstrateBreadth), data=data_glm_MAX1.1, family=binomial())
glmNULL.1<-glm(stateIUCN~1, data=data_glm_MAX1.1, family=binomial()) #NULL model

AICcmodavg::aictab(list(glmNULL.1, glm1.1,glm2.1,glm3.1), second.ord=F) # AIC weights
anova.res<-anova(glm1.1,glm3.1) #ANOVA results model for model comparaison
1-pchisq( abs(anova.res$Deviance[2]), abs(anova.res$Df[2])) #ANOVA p-value

MAM1<-glm_Max1 #Attributing the selected model to MAM1
summary(MAM1) #MAM1 results

# Calculating probabilities of being threatened for different traits:
a<-summary(glm_Max1)$coefficients[1,1]+(summary(glm_Max1)$coefficients[4,1]*3) 
probability1<-exp(a)/(exp(a)+1)
probability1

c<-summary(glm_Max1)$coefficients[1,1]+(summary(glm_Max1)$coefficients[4,1]*4)
probability2<-exp(c)/(exp(c)+1)
probability2
probability1/probability2

b<-exp(a-(summary(glm_Max1)$coefficients[3,2])*1.96)
probability_CI<-b/(b+1)
probability_CI

# Model 2: With sporophyte traits # # # # # # # # # # # # # # # # # # # # # # # 

glm_Max2<-glm(stateIUCN~plantSex2+scale(`seta_length(mm)`)+scale(`capsule_length(mm)`)+scale(SubstrateBreadth), data=data_glm2, family=binomial())
summary(glm_Max2) #model summary for maximal model 2
summary(glm_Max2)$coefficients[,2]*1.96 #standard deviations for maximal model 2
VIF(glm_Max2) #checking VIF, all good
predicted2 <- predict(glm_Max2, data=glm_Max2$model, type="response") #predicting values from maximal model 2
auc(glm_Max2$model$stateIUCN, predicted2) #AUC of maximal model 2

stepCriterion(glm_Max2, direction = "forward", criterion = "aic") #forward, AIC-based model selection for maximal model 2

data_glm_MAX2.1<-na.omit(data_glm2[,c(42,44,47,49,39)]) #cleaning traits used for maximal model 2
glm4.2<-glm(stateIUCN~plantSex2+scale(`seta_length(mm)`)+scale(`capsule_length(mm)`)+scale(SubstrateBreadth), data=data_glm_MAX2.1, family=binomial())
glm3.2<-glm(stateIUCN~plantSex2+scale(`capsule_length(mm)`)+scale(SubstrateBreadth), data=data_glm_MAX2.1, family=binomial())
glm2.2<-glm(stateIUCN~scale(`capsule_length(mm)`)+scale(SubstrateBreadth), data=data_glm_MAX2.1, family=binomial())
glm1.2<-glm(stateIUCN~scale(SubstrateBreadth), data=data_glm_MAX2.1, family=binomial())
glmNULL.2<-glm(stateIUCN~1, data=data_glm_MAX2.1, family=binomial())
nrow(data_glm_MAX2.1)
AICcmodavg::aictab(list(glmNULL.2, glm1.2,glm2.2,glm3.2,glm4.2), second.ord=F)# AIC weights
anova.res<-anova(glm4.2,glm3.2)# ANOVA for model comparaison
1-pchisq( abs(anova.res$Deviance[2]), abs(anova.res$Df[2])) #ANOVA p-values

MAM2<-glm(stateIUCN~scale(`capsule_length(mm)`)+scale(SubstrateBreadth), data=data_glm2, family=binomial()) #modeling MAM2
summary(MAM2) #MAM2 summary
summary(MAM2)$coefficients[,2]*1.96 #MAM2 standard deviations
sd(MAM2$model$`scale(SubstrateBreadth)`)
nrow(subset(MAM2$model,MAM2$model$stateIUCN==1))
nrow(subset(MAM2$model,MAM2$model$stateIUCN==0))

predicted2.2 <- predict(MAM2, data=MAM2$model, type="response")
auc(MAM2$model$stateIUCN, predicted2.2) #MAM2 AUC

# Calculating probabilities of being threatened for different traits:
a<-summary(MAM2)$coefficients[1,1]+(summary(MAM2)$coefficients[3,1]*1)
probability1<-exp(a)/(exp(a)+1)
probability1

c<-summary(MAM2)$coefficients[1,1]+(summary(MAM2)$coefficients[3,1]*2)
probability2<-exp(c)/(exp(c)+1)
probability2
probability1/probability2

b<-exp(a+(summary(MAM2)$coefficients[1,2])*1.96)
probability_CI<-b/(b+1)
probability_CI

# Bootstrap for MAM1 ####

datax1<-subset(MAM1$model, MAM1$model$stateIUCN==1) #sampling threatened species from MAM1 data
datax0<-subset(MAM1$model, MAM1$model$stateIUCN==0) #sampling non-threatened species from MAM1 data
nrow(datax1)

# Creating empty lists for storing bootstrap estimates for different coefficients
sample_coef_intercept <- NULL
sample_coef_PlantSex <- NULL
sample_coef_SporophytePresence1 <- NULL
sample_coef_SubBreadth <- NULL

# Bootstrap: 100 repetitions for 224 sample size for non-threatened species
for (i in 1:1000) {
  sample_data0<-datax0[sample(1:nrow(datax0), size=224, replace = F), ]
  sample_data0<-rbind(sample_data0, datax1)
  
  model_bootstrap <- glm(stateIUCN~plantSex2+`sporophyte presence`+`scale(SubstrateBreadth)`, data=sample_data0, family=binomial())

  sample_coef_intercept <-c(sample_coef_intercept, summary(model_bootstrap)$coefficients[1,1])
  sample_coef_PlantSex <-c(sample_coef_PlantSex, summary(model_bootstrap)$coefficients[2,1])
  sample_coef_SporophytePresence1 <-c(sample_coef_SporophytePresence1, summary(model_bootstrap)$coefficients[3,1])
  sample_coef_SubBreadth <-c(sample_coef_SubBreadth, summary(model_bootstrap)$coefficients[4,1])
}

# Cleaning bootstrap data
data_intercept_boot<-as.data.frame(sample_coef_intercept)
data_intercept_boot$Coef<-"Intercept"
colnames(data_intercept_boot)[1]<-c("Coefficients")
data_PlantSex_boot<-as.data.frame(sample_coef_PlantSex)
data_PlantSex_boot$Coef<-"Plant Sex"
colnames(data_PlantSex_boot)[1]<-c("Coefficients")
data_SpoPre_boot<-as.data.frame(sample_coef_SporophytePresence1)
data_SpoPre_boot$Coef<-"Sporophyte Presence"
colnames(data_SpoPre_boot)[1]<-c("Coefficients")
data_SubBreadth_boot<-as.data.frame(sample_coef_SubBreadth)
data_SubBreadth_boot$Coef<-"Substrate Breadth"
colnames(data_SubBreadth_boot)[1]<-c("Coefficients")

data_intercept_boot1<-c("InterceptB",mean(data_intercept_boot$Coefficients),sd(data_intercept_boot$Coefficients)*1.96)
data_plantsex_boot1<-c("Plant SexB",mean(data_PlantSex_boot$Coefficients),sd(data_PlantSex_boot$Coefficients)*1.96)
data_sporpresence_boot1<-c("Sporophyte PresenceB",mean(data_SpoPre_boot$Coefficients),sd(data_SpoPre_boot$Coefficients)*1.96)
data_subbreadth_boot1<-c("Substrate BreadthB",mean(data_SubBreadth_boot$Coefficients),sd(data_SubBreadth_boot$Coefficients)*1.96)
data_boot1<-as.data.frame(rbind(data_intercept_boot1, data_plantsex_boot1,data_sporpresence_boot1, data_subbreadth_boot1))

data_boot1$V2<-as.numeric(data_boot1$V2)
data_boot1$V3<-as.numeric(data_boot1$V3)

#Calculating mean and confidence interval values from MAM1 for the plot
data_intercept1<-c("Intercept1",summary(MAM1)$coefficients[1,1],summary(MAM1)$coefficients[1,2])
data_plantsex1<-c("Plant Sex1",summary(MAM1)$coefficients[2,1],summary(MAM1)$coefficients[2,2])
data_sporpresence1<-c("Sporophyte Presence1",summary(MAM1)$coefficients[3,1],summary(MAM1)$coefficients[3,2])
data_subbreadth1<-c("Substrate Breadth1",summary(MAM1)$coefficients[4,1],summary(MAM1)$coefficients[4,2])
data_glmX<-as.data.frame(rbind(data_intercept1, data_plantsex1,data_sporpresence1, data_subbreadth1))
data_glmX$V2<-as.numeric(data_glmX$V2)
data_glmX$V3<-as.numeric(data_glmX$V3)

# Plot used for the Figure 2
ggplot(data_boot1, mapping=aes(y=V2, x=V1)) + 
  geom_errorbar(data=data_boot1, aes(ymin= V2-V3,ymax=V2+V3,width = 0.4))+
  stat_summary(fun.y=mean, geom="point", colour="red",shape=20, size=4)+
  geom_errorbar(data=data_glmX, aes(x=V1, y=V2,ymin=V2-1.96*V3, ymax=V2+V3*1.96,width=0.4), color="blue")+
  geom_point(data=data_glmX,aes(x=V1, y=V2), colour="blue",shape=20, size=4)+
  theme_bw()

# Bootstrap for Sporophyte (required for MAM2) ####

data_glm_bootstrap3<-na.omit(data_glm2[,c(14,39)])#cleaning data for IUCN status and 'sporophyte presence'
glm_Sporophyte<-glm(stateIUCN~`sporophyte presence`, data=data_glm_bootstrap3, family=binomial()) #model for 'sporophyte presence'
summary(glm_Sporophyte)
summary(glm_Sporophyte)$coefficients[,2]*1.96

datax1.3<-subset(data_glm_bootstrap3, data_glm_bootstrap3$stateIUCN==1)
datax0.3<-subset(data_glm_bootstrap3, data_glm_bootstrap3$stateIUCN==0)
nrow(datax1.3)

sample_coef_intercept3 <- NULL
sample_coef_SporophytePresence3 <- NULL

#Bootstrap for 'sporophyte presence'
for (i in 1:1000) {
  sample_data0<-datax0.3[sample(1:nrow(datax0.3), size=240, replace = F), ]
  sample_data0<-rbind(sample_data0, datax1.3)
  
  model_bootstrap <- glm(stateIUCN~`sporophyte presence`, data=sample_data0, family=binomial())
  sample_coef_intercept3 <-c(sample_coef_intercept3, summary(model_bootstrap)$coefficients[1,1])
  sample_coef_SporophytePresence3 <-c(sample_coef_SporophytePresence3, summary(model_bootstrap)$coefficients[2,1])
}

# Cleaning bootstrap data
data_intercept3_boot<-as.data.frame(sample_coef_intercept3)
data_intercept3_boot$Coef<-"Intercept"
colnames(data_intercept3_boot)[1]<-c("Coefficients")
data_Sporophyte3_boot<-as.data.frame(sample_coef_SporophytePresence3)
data_Sporophyte3_boot$Coef<-"Sporophyte Presence"
colnames(data_Sporophyte3_boot)[1]<-c("Coefficients")

data_intercept_boot3<-c("1InterceptB",mean(data_intercept3_boot$Coefficients),sd(data_intercept3_boot$Coefficients)*1.96)
data_Sporophyte3_boot3<-c("3Sporo",mean(data_Sporophyte3_boot$Coefficients),sd(data_Sporophyte3_boot$Coefficients)*1.96)
data_boot3<-as.data.frame(rbind(data_intercept_boot3, data_Sporophyte3_boot3))

data_boot3$V2<-as.numeric(data_boot3$V2)
data_boot3$V3<-as.numeric(data_boot3$V3)

#Calculating mean and confidence interval values from 'sporophyte presence' model for the plot
data_intercept3<-c("2Intercept3",summary(glm_Sporophyte)$coefficients[1,1],summary(glm_Sporophyte)$coefficients[1,2])
data_sporo3<-c("4Sporophyte Presence1",summary(glm_Sporophyte)$coefficients[2,1],summary(glm_Sporophyte)$coefficients[2,2])

data_glmX3<-as.data.frame(rbind(data_intercept3, data_sporo3))
data_glmX3$V2<-as.numeric(data_glmX3$V2)
data_glmX3$V3<-as.numeric(data_glmX3$V3)

#Ploting 'sporophyte presence' model and bootstrap data
ggplot(data_boot3, mapping=aes(y=V2, x=V1)) + 
  geom_errorbar(data=data_boot3, aes(ymin= V2-V3,ymax=V2+V3,width = 0.4))+
  stat_summary(fun.y=mean, geom="point", colour="red",shape=20, size=4)+
  geom_errorbar(data=data_glmX3, aes(x=V1, y=V2,ymin=V2-1.96*V3, ymax=V2+V3*1.96,width=0.4), color="blue")+
  geom_point(data=data_glmX3,aes(x=V1, y=V2), colour="blue",shape=20, size=4)+
  theme_bw()

a<-mean(as.numeric(data_intercept3_boot$Coefficients))+mean(as.numeric(data_Sporophyte3_boot$Coefficients))
p_Sporo1<-exp(a)/(exp(a)+1) # Calculating threat probability: only 0.17, not 0.2, meaning that the MAM2 should be a ratio based on this
(1-p_Sporo1)*26/p_Sporo1 # Calculating required non-threatened species number to have a balanced data: 129.56 (~130) non-threatened ones required

# Bootstrap for MAM2 ####
#SAME AS FOR MAM1:
datax1.2<-subset(MAM2$model, MAM2$model$stateIUCN==1)
datax0.2<-subset(MAM2$model, MAM2$model$stateIUCN==0)
nrow(datax1.2)

sample_coef_intercept2 <- NULL
sample_coef_Capsule <- NULL
sample_coef_SubBreadth2 <- NULL

for (i in 1:1000) {
  sample_data0.2<-datax0.2[sample(1:nrow(datax0.2), size=130, replace = F), ]
  sample_data0.2<-rbind(sample_data0.2, datax1.2)
  colnames(sample_data0.2)[2]<-"capsule_length"
  
  model_bootstrap <- glm(stateIUCN~capsule_length+`scale(SubstrateBreadth)`, data=sample_data0.2, family=binomial())
  sample_coef_intercept2 <-c(sample_coef_intercept2, summary(model_bootstrap)$coefficients[1,1])
  sample_coef_Capsule <-c(sample_coef_Capsule, summary(model_bootstrap)$coefficients[2,1])
  sample_coef_SubBreadth2 <-c(sample_coef_SubBreadth2, summary(model_bootstrap)$coefficients[3,1])
}

data_intercept2_boot<-as.data.frame(sample_coef_intercept2)
data_intercept2_boot$Coef<-"Intercept"
colnames(data_intercept2_boot)[1]<-c("Coefficients")
data_Capsule_boot<-as.data.frame(sample_coef_Capsule)
data_Capsule_boot$Coef<-"xCapsule length"
colnames(data_Capsule_boot)[1]<-c("Coefficients")
data_SubBreadth2_boot<-as.data.frame(sample_coef_SubBreadth2)
data_SubBreadth2_boot$Coef<-"Substrate Breadth"
colnames(data_SubBreadth2_boot)[1]<-c("Coefficients")

data_intercept_boot2<-c("2InterceptB",mean(data_intercept2_boot$Coefficients),sd(data_intercept2_boot$Coefficients)*1.96)
data_capsule_boot2<-c("4capsuleB",mean(data_Capsule_boot$Coefficients),sd(data_Capsule_boot$Coefficients)*1.96)
data_subbreadth_boot2<-c("6Substrate Breadth",mean(data_SubBreadth2_boot$Coefficients),sd(data_SubBreadth2_boot$Coefficients)*1.96)
data_boot2<-as.data.frame(rbind(data_intercept_boot2, data_capsule_boot2,data_subbreadth_boot2))

data_boot2$V2<-as.numeric(data_boot2$V2)
data_boot2$V3<-as.numeric(data_boot2$V3)

data_intercept2<-c("1Intercept",summary(MAM2)$coefficients[1,1],summary(MAM2)$coefficients[1,2])
data_CapsuleLength2<-c("3Capsule Length2",summary(MAM2)$coefficients[2,1],summary(MAM2)$coefficients[2,2])
data_subbred2<-c("5Substrate Breadth2",summary(MAM2)$coefficients[3,1],summary(MAM2)$coefficients[3,2])

data_glmX2<-as.data.frame(rbind(data_intercept2, data_CapsuleLength2, data_subbred2))
data_glmX2$V2<-as.numeric(data_glmX2$V2)
data_glmX2$V3<-as.numeric(data_glmX2$V3)

ggplot(data_boot2, mapping=aes(y=V2, x=V1)) + 
  geom_errorbar(data=data_boot2, aes(ymin= V2-V3,ymax=V2+V3,width = 0.4))+
  stat_summary(fun.y=mean, geom="point", colour="red",shape=20, size=4)+
  geom_errorbar(data=data_glmX2, aes(x=V1, y=V2,ymin=V2-1.96*V3, ymax=V2+V3*1.96,width=0.4), color="blue")+
  geom_point(data=data_glmX2,aes(x=V1, y=V2), colour="blue",shape=20, size=4)+
  theme_bw()

# Binned residuals plots (assumption) ####
arm::binnedplot(y = MAM1$residuals, x = MAM1$fitted.values,
                xlab = "Predicted Probabilities", 
                main = "Binned Residual vs. Predicted Values", 
                col.int = FALSE) #all good

arm::binnedplot(y = MAM2$residuals, x = MAM2$fitted.values,
                xlab = "Predicted Probabilities", 
                main = "Binned Residual vs. Predicted Values", 
                col.int = FALSE) #all good





