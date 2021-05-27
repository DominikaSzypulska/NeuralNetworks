# Project Name
> NeuralNetworks

## Table of contents
* [General Information](#general-information)
* [Technologies](#technologies)
* [Features](#features)
* [Screenshots](#screenshots)
* [Code Examples](#code-examples)
* [Setup](#setup)
* [Status](#status)
* [Contact](#contact)

## General Information
The script teaches the neural network using the back propagation error algorithm. It was created with RStudio and R. <br>
The project was created for the needs of the classes.

## Technologies
Project is created with:
- RStudio version: 1.4.1106
- R version: 4.1.0

## Features
The script:
- Teaches the neural network 
- Graphs the error for each iteration in the learning process 
- Performs a neural network test 
- Calculates the network error
- Displays the effectiveness of the tests performed.

Additionally, the user can specify:
- How many inputs and outputs does the neural network have
- How many layers does a neural network have
- How many neurons are in each layer
- What activation function a given layer has
- Source of input data (from a text file)
- Learning factor, minimum error, maximum number of iterations

## Screenshots
Example screenshots showing the operation of the network training script:

Example diagram of the learning process:<br>
![Learning Process](/images/learning.PNG)

The effectiveness of the tests and the SSE:<br>
![SSE and effectiveness of the tests](/images/sse.PNG)

## Code Examples
The code shows the logic of splitting the input data into training data (80%) and test data (20%):
```
podzialDanych <- 0.8*nrow(dane)

X_uczenie <- X[1:podzialDanych,]
Y_uczenie <- Y[1:podzialDanych,]

X_test <- X[(podzialDanych+1):nrow(X),]
Y_test <- Y[(podzialDanych+1):nrow(Y),]
```

The code shows the logic of randomizing the initial weights on neurons:
```
 listaWag <- list()
  for (i in 1:(liczbaWarstw-1)) {
    a <- liczbaNeuronowWWarstwach[i]
    b <- liczbaNeuronowWWarstwach[i+1]
    listaWag[[i]] <- matrix(runif((a+1)*b, 0, 1), a+1, b)
  }
```

## Setup
To run the code, download the R language installer [here](https://cran.r-project.org/) and download and install the [RStudio](https://www.rstudio.com/products/rstudio/) graphical interface.
After a successful installation it is necessary to download the "ggplot2" package and the "ggplot2" library.
Download "NeuralNetworks". You need to load the input data (change the file path in the code). Sample data can be found in the project folder under the name "iris". Run the code.

## Status
Project is: *complete*.

## Contact
Created by [Dominika Szypulska](https://github.com/DominikaSzypulska).
<br>E-mail: dominikaszypulska@onet.pl -feel free to contact me!
