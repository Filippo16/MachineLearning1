#########PROJECT 1 ##########

# Loading dataset
data <- read.csv("Absenteeism_at_work.csv", sep=";", check.names=FALSE)

# 740 observations & 21 attributes

attributes <- colnames(data)

# if we consider that attributes 11 (Hit target) & 12 ( Discipline failure) are irrelevants  
#Weight (18) and height (19) useless because they are related to body mass index
datanew <- data[,-c()]

# pick up new attributes (4 removed)
attributesfinal <-colnames(datafinal) 

# #if we consider that the row 324 (325 on xls) is an outlier 
# datanew <- datanew[-325,]

#if we want to remove all rows where Absenteeism_Value > 100 of the attribute Absenteeism at work (attribute 21)
datafinal <- datanew
k=0
for (i in 1:739){
  if (datanew[i,2]==0){
    datafinal <- datafinal[-(i-k),]
    k=k+1
  }
}



# Summary statistics 

summary(datafinal)
boxplot(datafinal)
X=as.matrix(datafinal)
for (i in 1:dim(X)[2]){
  std <- sd(X[,i])
  X[,i] <- X[,i]/std
}
summary(X)

# sd <- colSds(datafinal)
cor(X)


############PCA###############

###1. Standardize: substraction of the mean from each attribute
means <- colMeans(datafinal)
# sd <- sd(datafinal)
datamean<- t(apply(datafinal,1,'-',means))

###2. Find the eigenvectors with SVD

SVD <- svd(datamean)
#We collect the singulars values which are on the diagonal 
singularvalues <- SVD$d
# Matrix of eigenvectors 
U=SVD$u
V=SVD$v
# 1st main component :
V[,1]


# EXPLICATIONS FUNCTION SVD 
# svd$d = vector containing the singular values of x sorted decreasingly.
# svd$u = matrix whose columns contain the left singular vectors of x
# svd$v = matrix whose columns contain the right singular vectors of x


#Variance explained
n=dim(dim(attributes))
Var <- singularvalues^2/sum(singularvalues^2)

# Verify that more than 90% of the variation in the data is explained 
# by the 4 first principal components
sum(Var[1:5])



# Vizualisation 1
plot(cumsum(Var), main="Data variance explained by PCs", xlab="Number of PCs included in variance sum", ylab="Proportion of variance explained")



### 3. Projection onto the main component

# NB
# %*%= multiplication for matrix

#Method 1: 
# Z = Y*V 
# Y = the zero mean data (datamean)
# dim(Y)= (733,17)
# V = the matrix containing right singular vectors (V=SVD$v)
#dim(V) = (17,17)
# Z1 <- datamean%*%t(V)





#Method 2: 
# SVD : Z = U*D 

# U = the matrix of left singular vectors (U=SVD$u)
# dim(U) = (733,17)
# S = the matrix with the singular values in the diagonal (matrix with singularvalues within)
# D=diag(singularvalues)
# dim(D) = (17,17)
# Z2 <- U%*%D



## I DONT UNDERSTAND WHY Z1!=Z2
# I don't know which one is the good one 


# Z[i,] = projections of observations 
# Z[,j] = PC(j).
# Z[i,j = projection of observation i onto the PC(j)

# 
# boxplot(as.data.frame(Z))

####################
# exercise 2.1.4
####################

Z <- U%*%diag(SVD$d)

pcx <- Z[,1]
pcy <- Z[,2]

plot(c(min(pcx), max(pcx)), c(min(pcy), max(pcy)), xlab="PC A", ylab="PC B", main="Absenteeism", type="n")
points(pcx, pcy)

print(V[,1])
print(V[,2])

