data  t9q1;
	infile "D:\@复旦u\5.大三第一学期@NUS\1.学习\ST3131 Regression Analysis\Tutorial\T9\t9q1.txt";
	input y x1 x2 x3;
run;

proc reg data = t9q1;
	model y =x1 x2 x3;
	test x3 = 0;
run;

proc rsquare cp b data = t9q1 ;
	model y = x1 x2 x3;
run;

proc reg data = t9q1;
	model y = x1 x2 x3/ selection = forward sle = 0.05;
run;
proc reg data = t9q1;
	model y = x1 x2 x3/ selection = backward sls = 0.05;
run;

proc reg data = t9q1;
	model y = x1 x2 x3/ selection = stepwise sle = 0.05 sls = 0.05;
run;

data t9q1_1;
	set t9q1;
	x12 = x1*x2;
	x23 = x2*x3;
	x13 = x1*x3;
run;

proc reg data =t9q1_1;
	model y = x2 x3 x12 x23 x13 / selection = forward sle = 0.05 sls = 0.05 include = 2;
run;
