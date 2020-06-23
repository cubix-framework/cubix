int not_main(void){
	int i , k;
	char j;
	goto label;
	double x = 21+32%45*43-56%2%3*4*5+ 6%7;
label:
	for(i = 0,j = 't',k = 8;1; i++ , k = k - i, i = k+2)
		printf("x = %f, one = \n",x,1);
	return -1;
}
