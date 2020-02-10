################################################################################
################## SDM - Heliconius melpomene melpomene ########################
################################################################################
################################################################################

# Clear R workspace
rm(list=ls())


# LOAD LIBRARIES
library(raster)
library(dismo)
library(maps)
library(kernlab)
library(vegan)
library(rJava)
library(maptools)
library(sdm)
library(randomForest)
library(glm2)


######################
###################### PART 1 -  PREPARE ENVIRONMENTAL LAYERS AND OCCURANCE DATA
######################

#### All the layers need to be with the same extent and resoltuion.

# List new raster files with right resolution and extent
predictors <- list.files(path="/home/anandam/project/anandam/SDM/predictors/for_test/",pattern =".tif", full.names = TRUE)
#  raster the objects from the list
predictors2 <- lapply(predictors, raster)

# STACK PREDICTORS - PUT ALL THE PREDICTORS IN THE SAME FILE (AS A LIST)
predictors<-stack(predictors2)

# LOAD OCCURRENCE OF H. melpomene melpomene
setwd("/home/anandam/project/anandam/SDM/ocur/")
ocor <- read.table("mel_filtered_rosser_ananda.txt", h=T)

# JOIN THE OCCURRENCE POINTS INFORMATION WITH THE ENVIRONMENTAL VARIABLES INFORMATION (PS: Long first and lat)
### Makes a table with cell information: it is the cellgrid with values of the predictors
ocor.val<-extract(predictors, ocor, cell=T) 

# IDENTIFY DUPLICATED CELL
### To identify if you have more than one individual per cellgrid to avoid Spatial autocorrelation.
id.dup<-duplicated(ocor.val[,"cells"])

# CBIND JOINS THE COLUMNS OF OCCURRENCE POINTS AND ENVIRON. VARIABLES
ocor1<-cbind(ocor, ocor.val[,-1])
# oco.val[,- 1]: exclude the first column of ocor.val ("column number")
# ... as what is important here are the coordinates and the environmental variables not the
# ... cell number.In ocor1 I will have the coordinates info and the environmental info for each coordinate.

# FALSE means it is excluding the duplicates
ocor2<-ocor1[which(id.dup==FALSE),] 

# Excluding NA lines. I don't think I have that because I made this step while organizing the occurance table, but just in case it is better to do.
ocor3<-na.omit(ocor2)


######################
###################### PART 2 -  SELECT PREDICTORS
######################

# After decide which variables to keep. Copy just the varibles of interest in a
# diferent directory -  pick ones which are most ecologically relevant to your species
# Repeat steps before correlation or separate the predictors of interest as bellow
# or check which layers you are going to include and separate them based on the number of the correlation table.

# RENAME PREDICTORS - SEPARATE THE PREDICTORS OF INTEREST
layers <- c(1:5)
Predictors <- stack(predictors[[layers]])

# Check the order of layers in the correlation table
Predictors # check last line
bioclim_names<-c("soil PH", "soil clay content %", "soil sand content %", "soil silt content %", "NDVI")
names(Predictors)<-bioclim_names

######################
###################### PART 3 -  BACKGROUND POINTS
######################

## contrast the "presence points" in a random scenario (the species would occur anywhere)

# Extension of the map
e <- extent(-118,-34,-55,32) 

#####
# a # Call for several background points (more than what you have of occurance <261>). In this case 500.
#####
bg <- randomPoints(Predictors, 500, ext=e)
#####
# b # Subset among the 500 background points, just the number you need (same of occurance)
#####
bg2 <-bg[1:nrow(ocor3),]

# The steps a and b are not necessary in all cases. You just need to do that when there is an error in "randomPoints(Predictors, nrow(ocor3), ext=e)". The error might be because of high number of NAs in some rasterfiles
# ... which makes it difficult to choose valid points for the background.
# If not you can just do
# bg2 <- randomPoints(Predictors, nrow(ocor3), ext=e)

# Extract/select the cells chosen to be "absent/background" points
back <- extract(Predictors, bg2)
# Combine points of background coordinates and environmental information of the extracted cells
back2<-cbind(bg2, back)

## back2 and ocor3 are two similar objects with longitude and latitude information
## ... and predictors information for each coordinate. The difference is that ocor3
##... is for the real occurrence data and back2 for the background data created. 

# Create a data frame including pb (1 presence, 0 absence) and the predictor values 
# Join the information of the predictors values and the coordinates (columns 1 and 2) for the presence (p=ocor3) and background points (b=back2)
dados <- prepareData(x=Predictors, p=ocor3[,1:2], b=back2[,1:2])

######################
###################### PART 4 -  MODELS
######################

# How many times the models are going to run
## In the first run, use as a test - repeticao = 1
repeticao=1
#repeticao = 20


# RUNNING ALGORITHMS
# Profile methods: 1.Bioclim, 2.Domain (Gower distance), 3.Mahalanobis
# Regression models: 1.Generalized Linear Models, 2.Generalized Additive Models
# Machine Learning: 1.Maxent. 2.Support Vector Machines, 3.Random Forest, 4.Boosted Regression Trees

## CREATING OBJECTS TO KEEP RESULTS
#1.BIOCLIM, 2.Mahalanobis, 3.MAXENT, 4.GLM, 5.RandomForest
bioclim0k<-maha0k<-maxent0k<-GLM0k<-RF0k<-stack()
bioclim.e<-maha.e<-maxent.e<-GLM.e<-RF.e<-NULL
bioclim.t<-maha.t<-maxent.t<-GLM.t<-RF.t<-NULL

for(i in 1:repeticao){ # number of repetitions. repeticao=1 for test
  id.treino<-sample(1:nrow(ocor3),round(0.75*nrow(ocor3),0))  ### 75% of occurance points for training. Round is to avoid broken numbers
  treino<-prepareData(x=Predictors, p=ocor3[id.treino,1:2], b=back2[id.treino,1:2])  ## id.treino is to keep just the lines of id.treino. Which means 75% of the ocor3 and back2
  teste<-prepareData(x=Predictors, p=ocor3[-id.treino,1:2], b=back2[-id.treino,1:2])  ## -id.treino is to take out the the ones used for id.treino. Which means 25% of ocor3 and back2
  
  # The object id.treino takes randonmly 75% of the occurance coordinates to use for the models and tests.
  # The object treino takes the environmental variables values for the points randomly selected in id.treino for both presence and background points. This object is going to be used to model the distribution.
  # The object test is pretty much the same as the 'treino' but uses the 25% of occurance that was not used for treino and it is used to test de models. 
  # The logic here is to take part of the ocor3 and back2 to calibrate the models and test them.
  
  ## 1) Bioclim [profile algorithm]
  
  #### Adjusting the model
  bioclim.modelo<-bioclim(treino[treino[,"pb"]==1,-1]) ## [treino[,"pb"]==1 is to run just the training column that has 1 (presence). -1 is to take out the column 'pb'
  #plot(bioclim.modelo)
  #response(bioclim.modelo)
  
  # Making predictions
  bioclim0k<-stack(bioclim0k, predict(object=bioclim.modelo, x=Predictors, progress='text'))  
  #plot(bioclim0k)
  #map(add=T)
  #plot(bioclim0k>0.2) ### "binarization", the threshold is to transform just in presence/absence
  #map(add=T)
  
  ### It is not necessary to run the plots and maps. This can be done as the last step for all the models. But you can do one by one.
  
  ## ???? Why use stack(bioclim0k,...) When I use the same logic for GLM and randomForest it does not work.
  
  
  ### Evaluating the model
  bioclim.eval<-evaluate(p=teste[teste[,"pb"]==1,-1], a=teste[teste[,"pb"]==0,-1], model=bioclim.modelo)
  bioclim.e<-c(bioclim.e, bioclim.eval@auc)
  bioclim.t<-c(bioclim.t, threshold(bioclim.eval, "spec_sens"))
  # density(bioclim.eval)
  # boxplot(bioclim.eval, col = c("blue","red"))
  # threshold(bioclim.eval) ## o valor dos threshold que maximiza, sensibilidade são as taxas de presença que o modelo previu como presença, e a especificidade as ausencias que são ausencias
  
  ##  2) Mahalanobis [profile algorithm]
  
  #### Adjusting the model
  maha.modelo<-mahal(treino[treino[,"pb"]==1,-1])
  # Making predictions
  maha0k<-stack(maha0k, predict(object=maha.modelo, x=Predictors, progress='text'))
  #plot(maha0k)
  #map(add=T)
  
  ### Evaluating the model
  maha.eval<-evaluate(p=teste[teste[,"pb"]==1,-1], a=teste[teste[,"pb"]==0,-1], model=maha.modelo)
  maha.e<-c(maha.e, maha.eval@auc)
  maha.t<-c(maha.t, threshold(maha.eval, "spec_sens"))
  
  ##  3) Maxent [machine learning algorithm]
  
  #### Adjusting the model
  system("java -version") # checking the Java version
  Sys.setenv(NOAWT=TRUE)
  #### Adjusting the model
  maxent.modelo<-maxent(x=treino[,-1], p=treino[,1])
  
  # Making predictions
  maxent0k<-stack(maxent0k,predict(object=maxent.modelo, x=Predictors,progress='text' ))  
  #plot(maxent0k)
  
  ### Evaluating the model
  maxent.eval<-evaluate(p=teste[teste[,"pb"]==1,-1], a=teste[teste[,"pb"]==0,-1], model=maxent.modelo)
  maxent.e<-c(maxent.e, maxent.eval@auc)
  maxent.t<-c(maxent.t, threshold(maxent.eval, "spec_sens"))
  
  ## 4) GLM [regression algorithm]
  
  #### Adjusting the model
  glm.modelo<-glm2(pb ~ ., family = gaussian(link = "identity"), data=treino)
  ## Fits generalized linear models using the same model specification as glm in the stats package, but with a modified default fitting method that 
  ##... provides greater stability for models that may fail to converge using glm.
  
  ## pb ~ . (means presence and background data in function of y is a function of all the predictors in the data.frame provided to the function)
  ## family - ????
  
  # Making predictions
  GLM0k<-predict(Predictors, glm.modelo)  
  #plot(GLM0k)
  
  ### Evaluating the model
  GLM.eval<-evaluate(p=teste[teste[,"pb"]==1,-1], a=teste[teste[,"pb"]==0,-1], model=glm.modelo)
  GLM.e<-c(GLM.e, GLM.eval@auc)
  GLM.t<-c(GLM.t, threshold(GLM.eval, "spec_sens"))
  
  ## 5) Random Forest [machine learning algorithm] 
  #### Adjusting the model
  RF.modelo <- randomForest(pb ~ ., data=treino)
  
  # Making predictions
  RF0k<-predict(Predictors, RF.modelo)  
  #plot(RF0k)
  
  ### Evaluating the model
  RF.eval<-evaluate(p=teste[teste[,"pb"]==1,-1], a=teste[teste[,"pb"]==0,-1], model=RF.modelo)
  RF.e<-c(RF.e, GLM.eval@auc)
  RF.t<-c(RF.t, threshold(RF.eval, "spec_sens"))
  
} #close for i

# Making rasters
writeRaster(bioclim0k, "bioclim0k.bil",format = "EHdr", overwrite = T )
writeRaster(maha0k, "maha0k.bil",format = "EHdr",overwrite = T  )
writeRaster(maxent0k, "maxent0k.bil",format = "EHdr",overwrite = T  )
writeRaster(GLM0k, "GLM0k.bil",format = "EHdr",overwrite = T  )
writeraster(RF0k, "RF0k.bil", format="EHdr",overwrite = T)


# Values of AUC (evaluating model - > 0.7 is good)
write.table(data.frame(bioclim=bioclim.e, mahal=maha.e, maxent=maxent.e, glm2= GLM.e,randomForest=RF.e), "AUC.txt", sep="\t",row.names=F )
write.table(data.frame(bioclim=bioclim.t, mahal=maha.t,maxent=maxent.t, glm2=GLM.t, randomForest=RF.t)
            , "Threshold.txt", sep="\t",row.names=F )


# Save workspace
save.image(file = "/home/anandam/project/anandam/SDM/predictors/for_test/test_model_melpomene.rda")
