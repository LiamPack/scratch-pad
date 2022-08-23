import networkx as nx

mentees_df = pd.read_csv('anonymous_mentees.csv')
mentees_df = mentees_df.iloc[:, 1:4] # ... tuned to header hopefully,
                                     # should be "name" "# needed
                                     # mentors", "choice1", "choice2",
                                     # "choice3"

mentors_df = pd.read_csv('anonymous_menotrs.csv')
mentors_df = mentors_df[['Name', 'number of mentees']]

g = nx.DiGraph()

[g.add_edge(row[0], 't', capacity=row[1]) for row in zip(mentors_df['Name'], mentors_df['Maximum Number of Mentees'])]
[g.add_edge('s', row[0], capacity=row[1]) for row in zip(mentees_df.iloc[:, 0], mentees_df.iloc[:, 1])]

students = list(set([t for s,t in g.edges() if s == 's']))
profs = list(set([s for s,t in g.edges() if t == 't']))

for s, p1, p2, p3 in zip(mentees_df.iloc[:, 0], mentees_df.iloc[:, 2], mentees_df.iloc[:, 3], mentees_df.iloc[:, 4]):
    g.add_edge(s, p1, capacity=1)
    g.add_edge(s, p2, capacity=1)
    g.add_edge(s, p3, capacity=1)

max_flow, edges = nx.maximum_flow(g, 's', 't')
assignments = {k:v for k,v in edges.items() if 't' not in v.keys() and 's' != k and 't' != k and k in students}
