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

limit = 2000000
def primes = [2,3]

for (int n = 5; true; n++) {
    if (prime(n)) {
        if (n < limit)
            primes << n
         else
             break
     }
}

println "min:${primes.min()} max:${primes.max()} size:${primes.size()} sum:${primes.sum()}"
println "inject: " + primes.inject(0L) { long t, long v -> t += v }