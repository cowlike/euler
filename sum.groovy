//trying to see why initial solution to problem 10 using sum() failed

def limit = 10000000
def nums = []
for (int i = 1; i <= limit; i++)
    nums << i
println "loaded..."

println nums.sum()
println nums.inject(0L) { t,v -> t += v }
