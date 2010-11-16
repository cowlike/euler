def prime = { num -> 
    if (num <= 3) 
        return true
        
    if (num % 2 == 0) {
        println "even number"
        return false
    }
    
    def half = Math.floor(num / 2)
    mkodd = half % 2 ? 0 : 1
    for (it = half + mkodd; it > 3; it -= 2) {
        if (num % it == 0) {
            println "$it divides"
            return false
        }
    }
    
    return true
}

//println prime(15)

def n = 600851475143

for (it = 3; it < 300425737571; it += 2) {
    def div = Math.floor(n / it)
    if (n % div == 0 && prime(div)) 
        println "*** prime: $div ***"
}

