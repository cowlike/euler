def div = {a,b -> a/b}
def flip = {foo -> { a,b -> foo(b,a) }}

println div(4,2)
println flip(div)(4,2)
println flip(div).curry(4)(2)
