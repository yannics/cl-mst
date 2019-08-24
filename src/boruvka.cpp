#include <iostream>

using namespace std;

class Edge {
    public:
        int source, destination, weight; 
};

class Graph {
    public:
        int numberofvertices, numberofedges;

        Edge *edge;
};

Graph *createGraph(int vertices, int edges) {
    Graph *graph = new Graph();
    graph->numberofvertices = vertices;
    graph->numberofedges = edges;

    graph->edge = new Edge[edges];

    return graph;
}

class SubGraph {
    public:
        int parent;
        int rank;
};

int find(SubGraph subgraphs[], int i) {
    if(subgraphs[i].parent != i) {
        subgraphs[i].parent = find(subgraphs, subgraphs[i].parent);
    }

    return subgraphs[i].parent;
}

void mix(SubGraph subgraphs[], int x, int y) {
    int xroot = find(subgraphs, x);
    int yroot = find(subgraphs, y);

    if(subgraphs[xroot].rank < subgraphs[yroot].rank) {
        subgraphs[xroot].parent = yroot;
    }
    else if (subgraphs[xroot].rank > subgraphs[yroot].rank) {
        subgraphs[yroot].parent = xroot;
    }
    else {
        subgraphs[yroot].parent = xroot;
        subgraphs[xroot].rank++;
    }
}

int comparisonfunction(const void *a, const void *b) {
    Edge *a1 = (Edge *)a;
    Edge *b1 = (Edge *)b;
    return a1->weight > b1->weight;
}

void boruvkaMST(Graph *graph) {
    int V = graph->numberofvertices;
    int E = graph->numberofedges;
    Edge *edge = graph->edge;

    SubGraph *subsets = new SubGraph[V];

    int *cheapest = new int[V];

    for (int v=0; v<V; ++v) {
        subsets[v].parent = v;
        subsets[v].rank = 0;
        cheapest[v] = -1;
    }

    int numTrees = V;
    int MSTweight = 0;

    while(numTrees > 1)  {
        for(int v=0; v<V; ++v) {
            cheapest[v] = -1;
        }

        for(int i=0; i<E; i++) {
            int set1 = find(subsets, edge[i].source);
            int set2 = find(subsets, edge[i].destination);

            if (set1 == set2) {
                continue;
            } else {
                if(cheapest[set1] == -1 ||
                   edge[cheapest[set1]].weight > edge[i].weight) {
                    cheapest[set1] = i;
                }

                if(cheapest[set2] == -1 ||
                   edge[cheapest[set2]].weight > edge[i].weight) {
                    cheapest[set2] = i;
                }
            }
        }

        for(int i=0; i<V; i++) {
            if(cheapest[i] != -1) {
                int set1 = find(subsets, edge[cheapest[i]].source);
                int set2 = find(subsets, edge[cheapest[i]].destination);

                if (set1 == set2) {
                    continue;
                }

                MSTweight += edge[cheapest[i]].weight;
                cout << edge[cheapest[i]].source << " " <<
		  edge[cheapest[i]].destination << " " <<
		  edge[cheapest[i]].weight << endl;
		
                mix(subsets, set1, set2);
                numTrees--;
            }
        }
    }
    
    // cout << "Weight of MST is " << MSTweight << endl;
    return;
}

