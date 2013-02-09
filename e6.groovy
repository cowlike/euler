def sumSquares(from, to) {
    (from..to).inject(0) { t,v -> t += (v * v); t }
}

def squareSum(from, to) {
    def sum = (from..to).sum()
    sum * sum
}

def from = 1
def to = 100
squareSum(from, to) - sumSquares(from, to)