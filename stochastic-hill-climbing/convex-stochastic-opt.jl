using LinearAlgebra
using Distributions
using MultivariateStats
using Plots

d(n) = MvNormal(zeros(n), I)

unit_sphere(x) = norm(x) <= 1
in_sphere(x, r) = unit_sphere(x/r)
in_ellipsoid(x,Rs) = unit_sphere(x ./ Rs)

in_2d_crescent(x) = (!in_sphere(x, 1)) * in_ellipsoid(x, [2, 1])

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

function ndtorus(N, L, d; constraint=unit_cube, heat=(x -> x[1] > 5 ? 4 : 2))
    warmup = floor(Int, N/10)
    xs = [rand(Float64, d) .* L]
    
    # temp = make_temperature(x -> shift(x, 5) |> constraint, heat)
    temp(x) = ((shift(x, 5) |> constraint) ? 0.5 : heat(x))
    for i in 1:N
        xn = xs[end]
        push!(xs, step_torus(xn, temp(xn), L))
    end
    return xs[warmup:end]
end



xs = ndtorus(1e7, 10, 2; constraint=unit_sphere, heat=(x -> x[1] > 5 ? 4 : 2))
xs0 = xs .|> x -> x .- 5
rs = norm.(xs0)





# histogram2d(first.(xs), xs .|> v -> v[2], normalize=:pdf, bins=(100,100))
# stephist(rs, normalize=:pdf, bins=100)

# shift back by -x0  before plotting
# keep on torus -- only place we get ergodicity probably. need stddev <= 1/2 * L probably
# look at stephist!(norm.(xs))
# try for higher D toruses
# try for even lower temperatures in target region
# try for imbalanced temperatures on positive orthant
# check escape times -- for i in 1:N, when in_region(xs[i]), loop j=i until xs[j] out of region. record j and see distribution

# WTS: for small temperatures, in a convex region the argmax(P(x)) should lie on the centroid w/ gaussian dist

# https://juliastats.org/MultivariateStats.jl/dev/pca/
# https://juliastats.org/Distributions.jl/stable/multivariate/#Distributions.MvNormal
# https://docs.juliahub.com/SpectralDistances/d1rIE/0.1.12/interpolations/
