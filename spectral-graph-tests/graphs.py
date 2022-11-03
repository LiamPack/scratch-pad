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
    
g = nx.Graph()
[g.add_edge(i, i+1, weight=1) for i in range(19)]

e,v = get_evals_evecs(g)
plt.plot(v[:,1])
plt.show()

g[5][6]['weight'] = 0.01
g[15][16]['weight'] = 0.01

e,v = get_evals_evecs(g)
print(e)
plt.plot(v[:,1])
plt.plot(v[:,2])
plt.plot(v[:,3])
plt.show()
