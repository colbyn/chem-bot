module Alpha

using LinearAlgebra
using LinearAlgebraX


# A = [-1 0; -2 -1; 1 2; 2 0; 0 -2]
# b = [2; -3; 0; 4; 2]
# b = [2 -3 0 4 2]
a = LinearAlgebraX.Matrix([-1 0 2; -2 -1 -3; 1 2 0; 2 0 4; 0 -2 2])
println(a)


# a = [-1 0; -2 -1; 1 2; 2 0; 0 -2]

# numat = LinearAlgebraX.rrefx(a)
println(numat)

# println(b * numat)


end