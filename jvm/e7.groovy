def prime(num) { 
    if (num < 2)
        return false
        
    if (num == 2 || num == 3) 
        return true
        
    if (num % 2 == 0 || num % 3 == 0)
        return false
    
    def half = Math.floor(Math.sqrt(num))
    mkodd = half % 2 ? 0 : 1
    for (it = half + mkodd; it > 4; it -= 2) {
        if (num % it == 0) {
            return false
        }
    }
    
    return true
}

def nPrimes = 0
def n = 2
for (n; true; n++) {
    if (prime(n) && (++nPrimes == 10001))
        break
}

n