using LinearAlgebra
using Distributions
using Plots

d(n) = MvNormal(zeros(n), I)

unit_sphere(x) = norm(x) <= 1
in_sphere(x, r) = unit_sphere(x/r)

unit_cube(x) = norm(x, Inf) <= 1
in_cube(x, dims) = unit_cube(x ./ (dims ./ 2))

shift(x, x0) = x - x0
shift(x, x0::Real) = x .- x0

make_temperature(region, temp_f, scale=0.1) =  x -> (region(x) ? scale : 1) * temp_f(x)

step(xn, t) = xn + t * rand(d(length(xn)))
step_torus(xn, t, L) = mod.(step(xn, t), L)


function torus(N, L)
    warmup = floor(Int, N/10)
    xs = [rand(Float64) * L]
    
    temp = make_temperature(x -> shift([x], 5) |> unit_sphere, x -> 1)
    for i in 1:N
        xn = xs[end]
        append!(xs, Vector(step_torus([xn], temp(xn), L)))
    end
    p = plot()
    stephist!(p, xs, normalize=:pdf, alpha=0.5)
    title!(p, "1d torus sim")
    savefig(p, "1dT_density.png")
    return xs
end

function ndtorus(N, L, d)
    warmup = floor(Int, N/10)
    xs = [rand(Float64, d) .* L]
    
    temp = make_temperature(x -> shift(x, 5) |> unit_cube, x -> x[1] > 5 ? 5 : 2)
    for i in 1:N
        xn = xs[end]
        push!(xs, step_torus(xn, temp(xn), L))
    end
    return xs[warmup:end]
end

xs = ndtorus(1e6, 10, 2)

# histogram2d(first.(xs), xs .|> v -> v[2], normalize=:pdf, bins=(20,20))

# shift back by -x0  before plotting
# keep on torus -- only place we get ergodicity probably. need stddev <= 1/2 * L probably
# look at stephist!(norm.(xs))
# try for higher D toruses
# try for even lower temperatures in target region
# try for imbalanced temperatures on positive orthant
# check escape times -- for i in 1:N, when in_region(xs[i]), loop j=i until xs[j] out of region. record j and see distribution

# WTS: for small temperatures, in a convex region the argmax(P(x)) should lie on the centroid w/ gaussian dist