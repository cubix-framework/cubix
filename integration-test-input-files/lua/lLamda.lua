g = function (x) 
        return x*x*x end

product = function (f, n)
        return function(x) return f(x)*n end end
  
f = function(a, b)
        return function(c) return function(d) return a+b+c+d end end end


print("Hello World!")

