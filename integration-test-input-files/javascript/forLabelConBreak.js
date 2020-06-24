function test() {
   outer: for (var i = 0; i < 3; i++) {
    console.log("i=" + i);
    for (var j = 0; j < 3; j++) {
      if (j === i) {
        continue outer;
      }
      break outer;
      console.log("j=" + j);
    }
  }
    var x = 0;
}
