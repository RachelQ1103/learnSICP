def inverse_cascade(n):
    grow(n)
    print(n)
    shrink(n)
    
def grow(n):
    all_but_last,last = n//10,n%10
    if n<10:
        return n
    else:
        grow(all_but_last)
        print(all_but_last)
        
def shrink(n):
    all_but_last, last = n//10, n%10
    if n<10:
        return n
    else:
        print (all_but_last)
        shrink(all_but_last)