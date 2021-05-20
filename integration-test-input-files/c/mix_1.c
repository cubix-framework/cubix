int abs (int x){
	if (x >= 0)
		return x;
	else
		return -x;
}

int main (void){
	int arr[11];
	printf("Enter 10  numbers(less than 100 , greater than -100) , press enter after each number, (dont enter 999, its unholy):\n");
	for(int i = 0; i < 10; i++){
		scanf("%d",&arr[i]);
		if(arr[i] == 999){
			printf("I did warn you!\n");
			break;
		}else if ((arr[i] >= 100) || (arr[i] <= -100)){
			printf("Sorry, Number is outta range, please enter again!");
			i--;
			continue;
		}
		printf("Number accepted");
		arr[i] = abs(arr[i]);
	}
	printf("Good Bye!");
	return 0;
}
