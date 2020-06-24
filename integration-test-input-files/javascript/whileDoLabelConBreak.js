function test() {
   var i = 0;
   outer:  do{
    console.log("i=" + i);
    var j = 0;
    while (j < 3) {
      if (j === i) {
        continue outer;
      }
      break outer;
      console.log("j=" + j);
      j++;
    }
   i++;
  }while(i < 3);
    var x = 0;
}
