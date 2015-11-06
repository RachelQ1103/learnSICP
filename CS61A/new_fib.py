def new_fib(n):
    """ fib(n) = fib(n-1)+fib(n-2) = (fib(n-2)+fib(n-3))+fib(n-2)
    = 2 * fib(n-2) + fib(n-3)"""
    if n == 0:
        return 0
    elif n == 1:
        return 1
    elif n == 2:
        return 1
    else:
        return 2 * new_fib(n-2) + new_fib(n-3)
        
def count(f):
    def counted(*args):
        counted.call_count += 1
        return f(*args)
    counted.call_count = 0
    return counted