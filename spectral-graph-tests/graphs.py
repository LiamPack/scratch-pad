import networkx as nx
import matplotlib.pyplot as plt
import numpy as np


def get_evals_evecs(g):
    L = nx.laplacian_matrix(g)

    e, v = np.linalg.eig(L.todense()) 
    idx = np.argsort(e)
    e = e[idx]
    v = v[:,idx]
    return e,v

def set_weight(g, i, j, w):
    g[i][j]['weight'] = w

def reset_weights(g):
    for u,v in g.edges:
        set_weight(g,u,v,0)

## first graph setup
g = nx.Graph()
[g.add_edge(i, i+1, weight=1) for i in range(19)]
nx.draw_spring(g)
plt.show()

# first spectrum check
e,v = get_evals_evecs(g)
plt.plot(v[:,1])
plt.plot(v[:,2])
plt.plot(v[:,3])
plt.show()

## increase tension on most nodes, strengthening cluster
for i in range(19):
    set_weight(g, i, i+1, 100)

# set weak-points, 2 cuts, for 3 clusters
set_weight(g, 5, 6, 1)
set_weight(g, 15, 16, 1)
nx.draw_spring(g)
plt.show()

# second spectrum check
e,v = get_evals_evecs(g)
print(e)
plt.plot(v[:,1])
plt.plot(v[:,2])
plt.show()

# show clustering in 3d
fig = plt.figure()
ax = fig.add_subplot(projection='3d')
ax.scatter(v[:,1].reshape(-1), v[:,2].reshape(-1), v[:,3].reshape(-1))
plt.show()


## go bigger....
n = 100
g = nx.Graph()
[g.add_edge(i, i+1, weight=1) for i in range(n)]
for i in range(n):
    g[i][i+1]['weight'] = 100
g[5][6]['weight'] = 1
g[15][16]['weight'] = 1
g[50][51]['weight'] = 1
g[70][71]['weight'] = 5
nx.draw_spring(g)
plt.show()

e,v = get_evals_evecs(g)
fig = plt.figure()
ax = fig.add_subplot(projection='3d')
ax.scatter(v[:,1].reshape(-1), v[:,2].reshape(-1), v[:,3].reshape(-1))
plt.show()
