# ukencompetition

## Introduction

Case competition project from Simon Fraser University's department gala (Fall
2015). The team consisted of Nathan Esau and Steve Kane. We won the following
prizes at the competition (out of 7 teams)

* Best predictions
* Best use of graphics
* Best overall

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
rows with unknown target variables). The variables are described below.

### Target variables 

* return_player:	whether the user plays the game at end of observation period
* revenue:			a measure of how much money the user spends
* engagement:		a measure of how much time the user spends playing

### Description of variables

1. obs: 					row ID
2. user_id:					user ID
3. install_date:			date user installed the app
4. platform:				install platform (ipad or iphone)
5. platform2_install_date: 	date user installed the app a second time
6. country:					country user is from 
7. gender:					male or female
8. return_player:			TARGET VARIABLE 
9. engagement:				TARGET VARIABLE
10. revenue:				TARGET VARIABLE
11. fb_connect				date the user connected the app to facebook
12. tutorial_completed:		date the user completed the tutorial
13. first_game_played:		date the user first played the game
14. first_type_1_game:		date the user played game type 1
15. first_type_2_game:		date the user played game type 2
16. first_type_3_game:		date the user played game type 3
17. first_type_4_game:		date the user played game type 4
18. first_win:				date the user first won the game
19. first_bonus:			date the user first won the bonus
20. first_purchase_a:		date the user first made a type A purchase
21. first_special_purchase:	date the user first madea special purchase
22. first_purchase_b:		date the user first made a type B purchase
23. first_purchase_c:		date the user first made a type C purchase
24. first_purchase_d:		date the user first made a type D purchase
25. first_purchase_e:		date the user first made a type E purchase
26. first_purchase_f:		date the user first made a type F purchase
27.	first_purchase_g:		date the user first made a type G purchase
28. first_purchase_h:		date the user first made a type H purchase
29. first_gift_sent:		date the user first sent a gift
30. first_gift_received:	date the user first received a gift
31. first_gift2_received:	date the user received a second gift
32. first_uken_gift_received:	date the user received a uken gift
33. first_collection:		date the user first received an artifact
34. first_prize_a:			date the user first received a prize of type A
35. first_prize_b:			date the user first received a prize of type B
36. first_prize_c:			date the user first received a prize of type C
37. stage1:					date the user completed stage 1
38. stage2:					date the user completed stage 2
39. stage3:					date the user completed stage 3
40. stage4:					date the user completed stage 4
41. stage5:					date the user completed stage 5
42. stage6:					date the user completed stage 6
43. stage7:					date the user completed stage 7
44. training0validation1:	0 if user is part of training set, 1 otherwise

### Details

If an event never occured, it is missing. There are also some missing values for
country, gender, etc. The validation set had the columns revenue, engagement and
return_player missing (the target variables), but these variables were all known
for the training set. 
