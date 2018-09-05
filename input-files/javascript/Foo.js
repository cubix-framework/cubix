function main() {
    "use strict";
    while (true) { var x = 1; }

    if (1 + 1 == 2) {
    } else {
        var y = 2, z = 4;
        t = 2 + 1 + 1;
        t -= 3;
        while (y + 1 > 0) {y--;}
        console.log("%d\n", y);
        if (false) {
            var y = y - 1;
            var u = 5;
        }
    }

    for (var i = 0; i < 100+1; i++) {
        if (i % 2 == 0) {
            continue;
        }
        console.log(i);
    }

    var w = [[1,2], [3]];
    w[f()].foo = 1+1;
    w[0][1] += 4;

    var b = a() && b() && c()

    return 0;
}

var t = 0;