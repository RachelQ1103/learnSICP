# count-change iteration
"""Calculate different ways of exchanging certain amount of dollars into 5 kinds of coins.
int a: amount of dollars.
return: int, return the number of different ways of exchanging."""

def cciter(a):
    first = 50
    second = 25
    third = 10
    fourth = 5
    ccnum = 0
    # iterate the first-domination kinds-of-coins in range(a/n+1).
    for num_1 in range(a/first + 1):
        amount1 = a - num_1 * first
        for num_2 in range(amount1/second + 1):
            amount2 = amount1 - num_2 * second
            for num_3 in range(amount2/third + 1):
                amount3 = amount2 - num_3 * third
                for num_4 in range(amount3/fourth + 1):
                    amount4 = amount3 - num_4 * fourth
                    ccnum += 1
    return ccnum