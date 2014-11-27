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

def n = 600851475143
def sq_n = Math.floor(Math.sqrt(n))
sq_n += (sq_n % 2 ? 0 : 1)

for (div in (sq_n..3).step(2)) {
    if (n % div == 0 && prime(div)) {
        println "$div is the largest prime factor"
        break
    }
}