class person: 
    def __init__(self): 
        self.name = 'Draco Malfoy'
        self.db = self.Dob() 
          
    def display(self): 
        print('NAME = ', self.name) 
          
    # this is inner class 
    class Dob: 
        def __init__(self): 
            self.dd = 10
            self.mm = 3
            self.yy = 2000
        def display(self): 
            print('DOB = {}/{}/{}'.format(self.dd, self.mm, self.yy)) 
              
# creating person class object 
p = person() 
p.display() 
  
# create inner class object 
x = p.db 
x.display() 
