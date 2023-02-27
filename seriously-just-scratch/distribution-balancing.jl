using Random
using Statistics
using Plots

function to_cdf(y)
    sy = sort(y)
    N = length(sy)

    return [(sy_i, i/N) for (i, sy_i) in enumerate(sy)]
end

function dcdf(cdf, x)
    idx = findfirst(y -> first(y) > x, cdf)
    if last(cdf[idx]) >= 1
        return 0
    end

    return (last(cdf[idx]) - last(cdf[idx+1]))/(first(cdf[idx]) - first(cdf[idx+1]) + 1e-7)
end

Random.seed!(8)

N = 100
y1 = sort(2 .* randn(N) .+ 2)
y2 = sort(2 .* randn(N) .- 2)


cdf1, cdf2 = to_cdf(y1), to_cdf(y2)
