---
title: "Principal Component 3d"
author: "Manu"
date: "5/3/2020"
output:
  word_document: default
  pdf_document: default
---


It is commonly said that the more data the better when thinking in Big Data. This is totally true when talking about observations, but it is not always the case with columns. Sometimes when two variable are highly correlated, they are providing the same information to the model. Here is when dimensionality reduction techniques comes to play. In this case PCA.

```{r}
library(dplyr)
library("factoextra")
```



```{r}

```

```{r}


elec_mined <- readRDS("elec_mined.RDS")
elec_mined <- elec_mined[,-1]

elec_cl <- elec_mined %>% 
  select(CCAA, WomanPopulationPtge, Izda_Pct, Dcha_Pct, Otros_Pct, AbstentionPtge, SameComAutonPtge, ForeignersPtge, Unemploy25_40_Ptge, Age_under19_Ptge)

ccaa <- aggregate(.~CCAA,elec_cl,mean)
rownames(ccaa)<-ccaa$CCAA
ccaa<-ccaa[,-1]




```

```{r}



```

# 1. Understanding Linear Algebra 

Principal components are extracted from the covariance matrix of the data, because, what a PC explains is the aumount of variation of the dataset that can explain by itself. 

In the covariance matrix, the diagonal is the variance of the column $X_i$, this is easily explained with the covariance and variance formula:

Recall the covariance formula:

$cov(x,y) = \sum^n_1(x_i-\bar{x})(y_i-\bar{y}) / n-1$

Now the variance:

$\sigma^2=\sum^n_1(x_i+\bar{x})^2/n-1$


The data is going to be simplified to a (4x3) matrix.

```{r}
cov_matrix_math <- as.matrix(ccaa[1:4, 1:3])
cov_matrix_math
```

Considering the formula, you want to substract each observation the mean of its vairable: $x_i-\bar{x}_i$ 

$cov(x,y) = \sum^n_1(x_i-\bar{x})(y_i-\bar{y})$

```{r}
cov_matrix_math[,1] <- cov_matrix_math[,1] - mean(cov_matrix_math[,1])
cov_matrix_math[,2] <- cov_matrix_math[,2] - mean(cov_matrix_math[,2])
cov_matrix_math[,3] <- cov_matrix_math[,3] - mean(cov_matrix_math[,3])
cov_matrix_math

# If you want to try it in a bigger dataframe and not waste time writing down code:
  # A <- ccaa
  # for (i in 1:length(A)) A[,i] = A[,i]-mean(A[,i]) 
  # A
```

The next step is to multiply: $(x_i-\bar{x})(y_i-\bar{y})$. But you need a squared matrix (Same rows, same columns) to get the same variables in rows and columns. This can be achivied with the transpose of the matrix, that gives a (3x4) matrix, so you get: (3x4)*(4x3) = (3x3) matrix.


```{r}
t(cov_matrix_math)
```


Now just multiply $A^t*A$. Remember Linear Algebra in highschool? The multiplication of 2 matrix is: **row*column**, which is shown below manually. Notice that is the firt number of the row of the first matrix times the first second matrix's fist column number plus the number of the first matrix's row times plus the second number of the second matrix, so at the end you are computing a $\sum^n_i$, part of the variance and covariance formula!


To compute A[1,1] =  (0.7152665 * 0.7152665) + (-2.772373 * -2.7723732) + (1.2193154 * 1.2193154) + (0.8377911*0.8377911) = 10.39

  * Oh wait! this is equal to $\sum^n_1(x_i-\bar{x})^2$, so in the diagonal you are computing the variance!
  
To compute A[2,1] = (7.4612714 * 0.7152665) + (-6.118552 * -2.7723732) + (1.985247 * 1.2193154) + (-3.3279661 * 0.8377911) = 21.93

  * There you go! this is $\sum^n_1(x_i-\bar{x})(y_i-\bar{y})$, the covariance.

```{r}
(0.7152665 * 0.7152665) + (-2.772373 * -2.7723732) + (1.2193154 * 1.2193154) + (0.8377911*0.8377911)
(7.4612714 * 0.7152665) + (-6.118552 * -2.7723732) + (1.985247 * 1.2193154) + (-3.3279661 * 0.8377911)
```

Let's check and divide by n, (numbers of A rows-1), so that you can finished the formula: $cov(x,y) = \sum^n_1(x_i-\bar{x})(y_i-\bar{y}) / n-1$

```{r}
t(cov_matrix_math)%*%cov_matrix_math
t(cov_matrix_math)%*%cov_matrix_math/(nrow(cov_matrix_math)-1) # covariance + variance
```

No you know how to calculate the covariance using linear algebra and why the variance of $x_i$ is in the diagonal.

```{r}
cov(cov_matrix_math)
var(cov_matrix_math[,1]) # variance of first column 
```

Here applied to the whole data:

```{r}
A <- ccaa

for (i in 1:length(A)) A[,i] = A[,i]-mean(A[,i]) 

t(as.matrix(A))%*%as.matrix(A)/(nrow(A)-1)

cov(ccaa)
```

# 2. Eigenvalues & Eigenvectors.

So why all of this theory? 


   1. A PC is a "new variable" which is made with a set of correlated independent variables. So they are just lineal combinations of those variables!
   
      * $CP_1 = v_{11}*x_1 + v_{12}*x_2 + ... + a_{1m}*x_m$; m=nº of variables; v=eigenvector position.
      
  2. What are the$vij$? The ij value of an **eigenvector**, for instance, first eigenvalue, first position. The eigenvectors are the Principal Component that describe a portion of the variability of the dataset. The firsrt PC1 tries to explain the maximum variability, the PC2, ties to explain the **remaining** variability that the PC1 couldn't explain, and it is not correlated to the PC1. So the vectors are *orthogonal* (each one pointing in distinct directions).
  
  3. Eigenvalue: Each eigenvector is associated to an eigenvalue. The eigenvalues ($\lambda_i$)  display the portion of variation retained by the PC that are associated to.
  
  4. The covariance matrix is essenctial to compute all of this. R can compute eigenvalues and eigenvectos, just keep in mind that it finds the values and vectors that follow this rule:
  
  * $A * v_{(matricial product)}$ = $v*\lambda_{(escalarproduct)}$; A = CovMatrix, v=eigenvector, lambda = eigenvalue.

For example:

The Eigen function computes the $v,\lambda$ for a given matrix. 

```{r}
eigen(cov(cov_matrix_math))
```

It can also be shown that: $A * v_{(matricial product)}=v*\lambda_{(escalarproduct)}$

```{r}
c(-0.1696792 ,-0.7240665 , 0.6685332)*67.265 # PC1 eigenvector
cov(cov_matrix_math)%*%c(-0.1696792 ,-0.7240665 , 0.6685332)
```

Now compute PCA. You can tell that PC1 is explained by the relations between Izda_Pct (left_wing) and Dcha_Pct (right_wing). You can see it as "the correlation" between the variables and the PC. So that, WomanPoplation would be in the PC2. 

```{r}
pc <- prcomp(cov_matrix_math)
pc$rotation # eigenvalues (Principal Component)
pc$sdev**2 # eigen vectors (the function shows sd(lambda))
```

So now as you would do in a linear regression, apply the first PC1, to the first observation, in this case "Andalucía".

$CP_1 = v_{11}*x_1 + v_{12}*x_2 + ... + a_{1m}*x_m$
$C_1=(-0.1696792 *0.7152665)+(-0.7488229*7.461271)+(0.6406819*-6.3649520)$

```{r}
cov_matrix_math
```

Now just apply it to every observation with $x.

```{r}
-0.1696792 *0.7152665+(-0.7488229*7.461271)+(0.6406819*-6.3649520)
pc$x
```

With summary you can see how the PC1 explains the 96% of the data set. You started with 3 variables and now you only need one.

```{r}
pc %>% summary()

8.2^2/(8.2^2 + 1.59^2 + 0.49^2) # Eigen values.
```



# 3. 3d PCA + Plotly


Now the represention with the whole dataframe.

```{r}
ccaa_location <- data.frame(location = factor(c("South", "North", "North", "ExtraPeninsular", "ExtraPeninsular", "North", "Center", "Center", "North", "ExtraPeninsular", "Center", "South", "South", "Center", "ExtraPeninsular", "South", "North", "North", "North" )))
```

```{r}

```



list(color=y, opacity = 0.7)

```{r}
#ccaa <- read.csv("ccaa.csv")
ccaa2 <- data.frame(ccaa)

# PCA
pc <- prcomp(ccaa,retx = T,scale. = T)

# Eigenvectors applied to observations.
res <- pc$x*(-1) # changing the direction.
x <- res[,1]
y <- res[,2]
z <- res[,3]

# Loadings/Eigenvectors
ev <- pc$rotation*-1 # Changing the direction.

# 3D plot
library(plotly)
ply <- plot_ly() %>%
  add_trace(x=x, y=y, z=z,
            type="scatter3d", 
            mode="markers",
            color=ccaa_location$location, 
            text = rownames(ccaa)
            ) 

for (i in 1:nrow(ev)) {
   x <- c(0, ev[i,1])*4 # Creating a vector the origin is 0, and direction vij.
   y <- c(0, ev[i,2])*4 # Multiplied * 4 because of the standarizarion that us PrComp function.
   z <- c(0, ev[i,3])*4
   ply <- ply %>% add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="lines",
            line = list(width=8),
            opacity = 1, name = names(ccaa)[i]) 
}

ply <- ply%>% 
  layout(
    title = "Principal Component: 82.75%",
    scene = list(
      xaxis = list(title = "PC1 (Age under 19) 40%",
                   backgroundcolor="rgb(0, 0,0)",
                   gridcolor="rgb(255,255,255)",
                    showbackground=TRUE,
                    zerolinecolor="rgb(152, 78, 165)"
      ),
      
      yaxis = list(title = "PC2 (Political Party) 25.8%",
                   backgroundcolor="rgb(0, 0,0)",
                    gridcolor="rgb(255,255,255)",
                    showbackground=TRUE,
                    zerolinecolor="rgb(152, 78, 165)"
      ),
      zaxis = list(title = "PC3 (Left_Wing) 17%",
                  backgroundcolor="rgb(0, 0,0)",
                  gridcolor="rgb(255,255,255)",
                  showbackground=TRUE,
                  zerolinecolor="rgb(152, 78, 165)"
                   
      )
    ))
ply 
```

```{r}
saveRDS(ply,"3dplot.rds")
```



```{r}
pc %>% summary()
pc$rotation[,1:3]
```

```{r}
readRDS("3dplot.rds")

```



