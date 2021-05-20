def outerFunction(text):  
    text = text  
    
    def innerFunction():  
        print(text)  
    
    innerFunction()  
    
if __name__ == '__main__':  
    outerFunction('I AM LORD VOLDEMORT')  
