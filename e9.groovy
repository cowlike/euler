def list = []
for (a in (1..997)) {
    for (b in (a+1..997)) {
        for (c in (b+1..997)) {
            if ((a < b && b < c) && (a + b + c == 1000) && (a*a + b*b == c*c))
                list << [a,b,c,a*b*c]
        }
    }
}
println list
println list.size()