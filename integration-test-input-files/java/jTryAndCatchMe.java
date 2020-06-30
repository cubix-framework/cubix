class GFG 
{ 
    public static void main (String[] args)  
    { 
        int[] arr = new int[4]; 
          
        try
        { 
            int i = arr[4]; 
            System.out.println("Inside try block"); 
        } 
        catch(NullPointerException ex) 
        { 
            System.out.println("Exception has been caught"); 
        } 
        finally
        { 
            System.out.println("finally block executed"); 
        } 
        
        try { 
            int a[] = { 1, 2, 3, 4, 5 }; 
            System.out.println(a[5]); 
            try { 
                int x = a[2] / 0; 
            } 
            catch (ArithmeticException e2) { 
                System.out.println("division by zero is not possible"); 
            } 
        } 
        catch (ArrayIndexOutOfBoundsException e1) { 
            System.out.println("ArrayIndexOutOfBoundsException"); 
            System.out.println("Element at such index does not exists"); 
        } 
    	
        System.out.println("Outside try-catch-finally clause"); 
    } 
}
