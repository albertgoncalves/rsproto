// NOTE: See `https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm`.

use std::cmp;
use std::collections::{BTreeMap, BTreeSet};

type Graph<'a, T> = BTreeMap<&'a T, BTreeSet<&'a T>>;

fn prepare<'a, T: Ord>(edges: &'a [(T, T)]) -> (Vec<&'a T>, Graph<'a, T>) {
    let mut vertices: BTreeSet<&'a T> = BTreeSet::new();
    let mut graph: Graph<'a, T> = BTreeMap::new();
    for (vertex, successor) in edges {
        vertices.insert(vertex);
        vertices.insert(successor);

        if let Some(successors) = graph.get_mut(vertex) {
            successors.insert(successor);
        } else {
            graph.insert(vertex, [successor].into());
        }

        if !graph.contains_key(successor) {
            graph.insert(successor, BTreeSet::new());
        }
    }
    (vertices.into_iter().collect(), graph)
}

fn strong_connect<'a, T: Ord>(
    vertex: &'a T,
    graph: &'a Graph<'a, T>,
    indices: &mut BTreeMap<&'a T, usize>,
    low_links: &mut BTreeMap<&'a T, usize>,
    stack: &mut Vec<&'a T>,
    components: &mut Vec<Vec<&'a T>>,
) {
    {
        let index = indices.len();
        assert!(indices.insert(vertex, index).is_none());
        assert!(low_links.insert(vertex, index).is_none());
    }
    assert!(!stack.contains(&vertex));
    stack.push(vertex);

    for successor in &graph[vertex] {
        let successor: &'a T = successor;
        if !indices.contains_key(&successor) {
            strong_connect(successor, graph, indices, low_links, stack, components);
            low_links.insert(vertex, cmp::min(low_links[vertex], low_links[successor]));
        } else if stack.contains(&successor) {
            low_links.insert(vertex, cmp::min(low_links[vertex], low_links[successor]));
        }
    }

    if indices[vertex] == low_links[vertex] {
        components.push(
            stack.split_off(
                stack
                    .iter()
                    .rposition(|other| vertex == *other)
                    .unwrap_or(0),
            ),
        );
    }
}

fn tarjan<'a, T: Ord>(vertices: &'a [&'a T], graph: &'a Graph<'a, T>) -> Vec<Vec<&'a T>> {
    let mut indices: BTreeMap<&'a T, usize> = BTreeMap::new();
    let mut low_links: BTreeMap<&'a T, usize> = BTreeMap::new();
    let mut stack: Vec<&'a T> = vec![];
    let mut components: Vec<Vec<&'a T>> = vec![];

    for vertex in vertices {
        let vertex: &'a T = vertex;
        if !indices.contains_key(vertex) {
            strong_connect(
                vertex,
                graph,
                &mut indices,
                &mut low_links,
                &mut stack,
                &mut components,
            );
        }
    }

    components
}

fn main() {
    let edges = [
        ('a', 'b'),
        ('b', 'a'),
        ('b', 'e'),
        ('f', 'g'),
        ('g', 'a'),
        ('g', 'h'),
        ('h', 'c'),
        ('h', 'f'),
    ];
    let (vertices, graph) = prepare(&edges);
    for component in tarjan(&vertices, &graph) {
        println!("{component:?}");
    }
}
