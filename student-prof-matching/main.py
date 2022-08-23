import networkx as nx

student_prof_preferences = {
    'a': ['2', '3'], 'b': ['3', '4', '2']
}

students = list(set(student_prof_preferences.keys()))
profs = list(set([p for l in student_prof_preferences.values() for p in l]))
g = nx.DiGraph(student_prof_preferences)

for s,t in g.edges():
    if s in students:
        g[s][t]['capacity'] = 1 


g.add_node('s')
[g.add_edge('s', s, capacity=1) for s in students]

g.add_node('t')
g.add_edge('2', 't', capacity=2)
g.add_edge('3', 't', capacity=1)
g.add_edge('4', 't', capacity=0)

nx.maximum_flow(g, 's', 't')
