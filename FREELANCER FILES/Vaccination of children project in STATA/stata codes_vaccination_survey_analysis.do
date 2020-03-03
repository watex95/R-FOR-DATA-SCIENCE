

ttest c13==q13
/*paired sample t-test between pretest attitude and posttest attitude*/

ologit c11 c13
 /*Ordered logistic regression model between pretest likelihood of vaccination and attitude*/
 
ologit c10 c13 
/*Ordered logistic regression model between pretest vaccination status and attitude*/

ttest c11==q11
 /*Paired t-tests to test the difference in the likelihood of vaccinating the current child (pre-test and post-test)*/
 
ttest c12==q12
 /*Paired t-tests to test the difference in the likelihood of vaccinating another child (pre-test and post-test)*/
 
ologit link i.message i.c13
 /*Ordered logistic regression between parent reactions (link) depending on message and the attitude (c13)*/
 
ologit link i.message
 /*Ordered logistic regression between parent reactions (link) depending on message given*/
 
gen att_change=q13-c13 
/*generate a new variable att_change from a difference of two variables*/

ologit att_change i.message
 /*Ordered logistic regression between attitude changes (att_change) depending on message*/
 
tabulate messsage c10,chi2 lrchi2 
/*chi square test of independence between vaccine status and message*/

tabulate messsage c13,chi2 lrchi2
 /*chi square test of independence between attitude and message*/
 
olgit link i.message c5 c51 
/*Ordered logistic regression between parent reaction to message and the control variable age*/

olgit link i.message i.d1 
/*Ordered logistic regression between parent reaction to message and the control variable education*/

olgit link i.message i.d4
 /*Ordered logistic regression between parent reaction to message and the control variable financial status*/

