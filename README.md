# ukencompetition

## Introduction

Case competition project from Simon Fraser University's department gala (Fall
2015).

The task was to analyze sales and time spent playing from a Uken mobile app
game. The app was a fremium game meaning that it is free to play, but the
company makes money from in app purchases.

## Files

* analysis.R		contains code to produce predictions and graphics.		
			for predictions, we used a random forest algorithm
			and gradient boosting algorithm.

* Poster/		contains poster.pdf used in the case competition. Intended
			printing size is 4 feet wide and 3 feet high. Poster	
			can be produced by running pdflatex on poster.tex

* predictions.csv	The predictions we submitted. These can be produced 
			by running analysis.R (with some additional minor tuning 
			for the randomForest and xgboost algorithms). However,
			we haven't uploaded the data set and the code won't run
			without it.

## Dataset

I do not have permission to upload the Data to GitHub. The dataset contained
300,000 rows (250,000 training rows with known target variables and 50,000 test
rows with unknown target variables).
