def fib(n) {
    if (n == 0 || n == 1) {
        return 1
    }
    else {
        return fib(n-1) + fib(n-2)
    }
}

def sum = 0
def i = 1

while (true) {
    def value = fib(i++)
    if (value >= 4000000)
        break
        
    if (value % 2 == 0) {
        sum += value
    }
}

sum