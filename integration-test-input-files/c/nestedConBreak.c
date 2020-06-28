int main (void){
	int x = 0;
lab1:
	for(int i = 0; i < x; i += x){
		break;
		goto lab1;
		continue;
lab3:		goto lab2;
		while(1){
			printf("hello world");
lab4:			goto lab3;
			break;
			goto lab1;
lab2:			continue;
			return 0;
			do{
				goto lab4;
				continue;
				printf("Hi");
				break;
			}while(1);
	}
}
}
