public class Main {
    public static void main(String[] args){
        int i = 0;
lab1:
        while (i < 10) {
lab2:	
            for (int j = 0; j < 10; j++) {
		break lab1;
                if (j == 1)
                    break lab2;
		else
			continue lab1;
lab3:		do{
			System.out.println("The Heir of Slytherin");
lab4:			while(1){
				break lab2;
				int s = 0;
				continue lab1;
				return 0;
				continue lab4;
			}
		}while(1);
                System.out.println(" value of j = " + j);
            }
            continue lab1;
            i++;
        }
    }
}

