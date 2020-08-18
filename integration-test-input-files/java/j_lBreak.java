public class Main { 
    public static void main(String[] args) 
    {	int x = 43%45+87*34/23/89*12+32% (-54); 
 	int i = 0; 
    outer: 
        while (i < 10) { 
            for (int j = 0; j < 10; j++) { 
                if (j == 1) 
                    break outer; 
                System.out.println(" value of j = " + j); 
            }
	    continue;
	    i++;
	}
    }
}
