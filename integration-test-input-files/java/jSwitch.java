public class Test { 
    public static void main(String[] args) 
    { 
        int day = 5; 
        String dayString; 
  
        switch (day) { 
        case 1: 
            dayString = "Heads"; 
            break; 
        case 2: 
            dayString = "Tails"; 
        default: 
            dayString = "No, Coin will not stay up in the air, experiment is being performed on earth."; 
            break; 
        } 
        System.out.println(dayString); 
    } 
} 
