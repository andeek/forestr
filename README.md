# forestr

Just as a forester uses many tools to manage a forest, we may need multiple tools to manage a random forest. `forestr` is an R package (in development) that extends the random forest methodology by including multiple splitting criteria for building the trees. Additionally, the possibility for user specified splitting criteria is left open by functionalizing the splitting methods.

(To be) included splitting criteria within `forestr`:
- Gini
- Entropy
- One-Sided extreme
- One-Sided purity

# Installation

To install `forestr`, use the command `devtools::install_github("andeek/forestr")`

# Plan of Action

This is a project for STAT 503 with Di Cook in Spring 2015. Below I detail the steps that will be taken to create this package and test its use.

## Coding

#### Update 4/12/2015

The old plan (see below) has been temporarily dropped due to the incomplete port of `randomForest` from Fortran to C. With my lack of Fortran experience, finishing the port myself has proved to difficult. Instead of extending the `randomForest` library, I am building a random forest framework around the `rpart` library, which does include the ability for a user to create splitting functions.

I am currently in the process of writing the framework.

#### Old plan

I will be extending the current `randomForest` package by updating the C, Fortran, and R files within its source. The main lifting will be to create splitting functions, rather than having the splitting be native within the code. Additionally, parameters will need to be created in the top-level R code to uncover the functionality to the user.

Currently I have read through the code files in the `randomForest` package and located where the splitting is being done (with Gini). The challengs that I foresee are that I do not know Fotran, nor have I ever written an R package. There is a first time for everything though, and extending a well written package will be a much more achievable goal than starting from scratch.

## Testing

The one-sided extreme and one-sided purity methods will be the focus of testing the package for my paper. These methods were created to better handle unbalanced (2-class) classification tasks where one class is of more importance to be classified correctly (think cancer detection). As such, we will test these splitting criteria with varying levels of unbalanced data in a 2-class classification problem. 

The data used in this project come from the [UCI Machine Learning repository](http://archive.ics.uci.edu/ml/), and are produced using Monte Carlo simulations to resemble properties measured by particle detectors in an accelerator to detect the Higgs Boson particle. I have created [smaller datasets](https://github.com/andeek/forestr/tree/master/data) from this large dataset by sampling observations to create the percentage of unbalancedness I desire. The levels of unbalancedness I will test are 1%, 2%, 5%, and 10% and compare the performance of the one-sided extremes and one-sided impurity to gini and entropy performance on the same datasets.
