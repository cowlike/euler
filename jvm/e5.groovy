boolean divisibleBy(n, from, to) {
    for (i in (from..to)) {
        if (n % i)
            return false
    }
    true
}

def from = 3
def to = 20
def i

for (i = to; !divisibleBy(i, from, to); i += to)
    ;
    
i