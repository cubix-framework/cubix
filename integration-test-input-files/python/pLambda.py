def main():
    g = lambda x: x*x*x
    product = lambda f, n: lambda x: f(x)*n
    def action(x):
	return (lambda newx: x + newx)
    f = lambda a = 2, b = 3:lambda c: a+b+c

