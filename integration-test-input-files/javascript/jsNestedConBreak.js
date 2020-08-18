function main(){
                var i = 0;
lab1:
        while (i < 10) {
lab2:
            for (var j = 0; j < 10; j++) {
                break lab1;
                if (j == 1)
                    break lab2;
                else
                        continue lab1;
lab3:           do{
                        console.log("The Heir of Slytherin");
lab4:                   while(1){
                                break lab2;
                                var s = 0;
                                continue lab1;
                                return 0;
                                continue lab4;
                        }
                }while(1);
                console.log(" value of j = " + j);
            }
            continue lab1;
            i++;
        }

}
