def f4():
    x = F3()
    x.f3(1, 2)
class F3:
    def f3(self, x, y):
        F2().f2(x, y)
