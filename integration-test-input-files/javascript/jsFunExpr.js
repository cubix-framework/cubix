function main(){
        var square = function(a) {
                        return a * a;
                        };
        var math = {
                'factit': function factorial(n) {
                                        console.log(n);
                                        if (n <= 1) {
                                                 return 1;
                                                 }
                                         return n * factorial(n - 1);
                                        }
                };      
        var fivePower = function(a){
                        return a * square(a) * square(a);
        };
}
