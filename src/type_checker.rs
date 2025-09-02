use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Key {
    VarInt(i32),
    VarChr(char),
    GenChr(char, Option<i32>),
}

#[derive(Clone, Eq, Ord, PartialEq, PartialOrd)]
struct Edge(Key, Key);

#[derive(Clone, PartialEq)]
enum Type {
    Key(Key),
    Atom(char),
    Call(usize, Vec<usize>),
    Func(Vec<usize>, usize),
}

struct TypesAndIndex<'a>(&'a [Type], usize);

#[derive(Debug)]
enum Error {
    Merge(usize, usize),
    Deref(Key),
    InitGeneric(usize),
}

impl Edge {
    fn new(left_key: Key, right_key: Key) -> Option<Self> {
        match left_key.cmp(&right_key) {
            Ordering::Equal => None,
            Ordering::Less => Some(Self(left_key, right_key)),
            Ordering::Greater => Some(Self(right_key, left_key)),
        }
    }
}

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::VarInt(int) => write!(f, "__{int}__"),
            Self::VarChr(chr) => write!(f, "{chr}"),
            Self::GenChr(chr, Some(int)) => write!(f, "{chr}.{int}"),
            Self::GenChr(chr, None) => write!(f, "{chr}.?"),
        }
    }
}

impl fmt::Display for TypesAndIndex<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_type(f, self.0, self.1)
    }
}

fn display_type(f: &mut fmt::Formatter<'_>, types: &[Type], index: usize) -> fmt::Result {
    match &types[index] {
        Type::Key(key) => write!(f, "{key}"),
        Type::Atom(atom) => write!(f, "#{atom}"),
        Type::Call(func, args) => {
            display_type(f, types, *func)?;
            write!(f, "(")?;
            if !args.is_empty() {
                display_type(f, types, args[0])?;
                for arg in &args[1..] {
                    write!(f, ", ")?;
                    display_type(f, types, *arg)?;
                }
            }
            write!(f, ")")
        }
        Type::Func(inputs, output) => {
            write!(f, "(\\")?;
            if !inputs.is_empty() {
                display_type(f, types, inputs[0])?;
                for input in &inputs[1..] {
                    write!(f, ", ")?;
                    display_type(f, types, *input)?;
                }
            }
            write!(f, " -> ")?;
            display_type(f, types, *output)?;
            write!(f, ")")
        }
    }
}

#[derive(Default)]
struct State {
    types: Vec<Type>,
    key_to_type: BTreeMap<Key, (usize, bool)>,
    key_to_key: BTreeSet<Edge>,
    k: i32,
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "[")?;
        for index in 0..self.types.len() {
            write!(f, "    ")?;
            display_type(f, &self.types, index)?;
            writeln!(f, ",")?;
        }
        writeln!(f, "]")?;

        writeln!(f, "{{")?;
        for (key, (index, _)) in &self.key_to_type {
            write!(f, "    {key}: ")?;
            display_type(f, &self.types, *index)?;
            writeln!(f, ",")?;
        }
        writeln!(f, "}}")?;

        writeln!(f, "{{")?;
        for Edge(left_key, right_key) in &self.key_to_key {
            writeln!(f, "    ({left_key}, {right_key}),")?;
        }
        writeln!(f, "}}")
    }
}

impl State {
    fn get_index(&self, new_type: &Type) -> Option<usize> {
        self.types.iter().position(|old_type| old_type == new_type)
    }

    fn push_index(&mut self, new_type: Type) -> usize {
        if let Some(index) = self.get_index(&new_type) {
            index
        } else {
            self.types.push(new_type);
            self.types.len() - 1
        }
    }

    const fn get_k(&mut self) -> i32 {
        let k = self.k;
        self.k += 1;
        k
    }

    fn insert(&mut self, key: Key, new_index: usize, new_frozen: bool) -> Result<(), Error> {
        if let Some((old_index, old_frozen)) = self.key_to_type.remove(&key) {
            let merged = self.merge(old_index, new_index)?;
            if old_frozen {
                self.key_to_type.insert(key, (old_index, old_frozen));
            } else {
                self.key_to_type.insert(key, (merged, new_frozen));
            }
        } else {
            self.key_to_type.insert(key, (new_index, new_frozen));
        }
        Ok(())
    }

    fn push_and_insert(&mut self, key: Key, r#type: Type, frozen: bool) -> Result<(), Error> {
        let index = self.push_index(r#type);
        self.insert(key, index, frozen)?;
        Ok(())
    }

    fn collect(&mut self, index: usize) -> Result<Key, Error> {
        match self.types[index].clone() {
            Type::Key(key) => Ok(key),
            Type::Atom(..) => {
                let key = Key::VarInt(self.get_k());
                self.insert(key.clone(), index, false)?;
                Ok(key)
            }
            Type::Call(func, args) => {
                let func = self.collect(func)?;

                let mut inputs = Vec::with_capacity(args.len());
                for arg in &args {
                    inputs.push(self.collect_and_push(*arg)?);
                }
                let output = Key::VarInt(self.get_k());

                let index = self.push_index(Type::Key(output.clone()));
                self.push_and_insert(func, Type::Func(inputs, index), false)?;

                Ok(output)
            }
            Type::Func(old_inputs, output) => {
                let mut new_inputs = Vec::with_capacity(old_inputs.len());
                for input in &old_inputs {
                    new_inputs.push(self.collect_and_push(*input)?);
                }

                let key = Type::Key(self.collect(output)?);
                let output = self.push_index(key);
                let func = Key::VarInt(self.get_k());

                self.push_and_insert(func.clone(), Type::Func(new_inputs, output), false)?;

                Ok(func)
            }
        }
    }

    fn collect_and_push(&mut self, index: usize) -> Result<usize, Error> {
        let key = Type::Key(self.collect(index)?);
        Ok(self.push_index(key))
    }

    fn init_generic(&mut self, index: usize, k: i32) -> Result<usize, Error> {
        match self.types.get(index) {
            Some(Type::Key(Key::GenChr(_, Some(_)))) => Err(Error::InitGeneric(index)),
            Some(Type::Key(Key::GenChr(chr, None))) => {
                Ok(self.push_index(Type::Key(Key::GenChr(*chr, Some(k)))))
            }
            Some(..) => Ok(index),
            None => unreachable!(),
        }
    }

    fn merge(&mut self, left_index: usize, right_index: usize) -> Result<usize, Error> {
        if left_index == right_index {
            return Ok(left_index);
        }

        match (self.types[left_index].clone(), self.types[right_index].clone()) {
            (Type::Key(Key::GenChr(_, None)), _) | (_, Type::Key(Key::GenChr(_, None))) => {
                Err(Error::Merge(left_index, right_index))
            }
            (Type::Call(left_func, left_args), Type::Call(right_func, right_args)) => {
                let func = self.merge(left_func, right_func)?;

                let n = left_args.len();
                if n != right_args.len() {
                    return Err(Error::Merge(left_index, right_index));
                }

                let mut args = Vec::with_capacity(n);
                for i in 0..n {
                    args.push(self.merge(left_args[i], right_args[i])?);
                }

                Ok(self.push_index(Type::Call(func, args)))
            }
            (Type::Func(left_inputs, left_output), Type::Func(right_inputs, right_output)) => {
                let n = left_inputs.len();
                if n != right_inputs.len() {
                    return Err(Error::Merge(left_index, right_index));
                }

                let left_k = self.get_k();
                let right_k = self.get_k();

                let mut inputs = Vec::with_capacity(n);
                for i in 0..n {
                    let left_input = self.init_generic(left_inputs[i], left_k)?;
                    let right_input = self.init_generic(right_inputs[i], right_k)?;
                    inputs.push(self.merge(left_input, right_input)?);
                }

                let left_output = self.init_generic(left_output, left_k)?;
                let right_output = self.init_generic(right_output, right_k)?;
                let output = self.merge(left_output, right_output)?;

                Ok(self.push_index(Type::Func(inputs, output)))
            }
            (Type::Key(left_key), Type::Key(right_key)) => {
                self.key_to_key
                    .insert(Edge::new(left_key, right_key).unwrap());
                Ok(left_index)
            }
            (Type::Key(key), _) => {
                self.insert(key, right_index, false)?;
                Ok(left_index)
            }
            (_, Type::Key(key)) => {
                self.insert(key, left_index, false)?;
                Ok(right_index)
            }
            (..) => Err(Error::Merge(left_index, right_index)),
        }
    }

    fn deref(&mut self, old_key: &Key) -> Result<usize, Error> {
        let mut found_types: BTreeSet<usize> = BTreeSet::new();
        let mut found_keys: BTreeSet<Key> = BTreeSet::from([old_key.clone()]);

        let mut stack: Vec<Key> = vec![old_key.clone()];
        while let Some(key) = stack.pop() {
            for Edge(left_key, right_key) in &self.key_to_key {
                if key == *left_key && found_keys.insert(right_key.clone()) {
                    stack.push(right_key.clone());
                }
                if key == *right_key && found_keys.insert(left_key.clone()) {
                    stack.push(left_key.clone());
                }

                if let Some((index, _)) = self.key_to_type.get(&key) {
                    if matches!(&self.types[*index], Type::Key(..)) {
                        unreachable!();
                    }
                    found_types.insert(*index);
                }
            }
        }

        for new_key in found_keys.into_iter().filter(|new_key| old_key != new_key) {
            self.key_to_key.insert(Edge::new(old_key.clone(), new_key).unwrap());
        }

        let mut found_types: Vec<usize> = found_types.into_iter().collect();

        let Some(mut left_index) = found_types.pop() else {
            return Err(Error::Deref(old_key.clone()));
        };
        while let Some(right_index) = found_types.pop() {
            left_index = self.merge(left_index, right_index)?;
        }
        Ok(left_index)
    }
}

#[allow(clippy::many_single_char_names)]
fn main() {
    let mut state = State::default();

    {
        let x = state.push_index(Type::Key(Key::GenChr('x', None)));
        let y = state.push_index(Type::Key(Key::GenChr('y', None)));

        state
            .push_and_insert(Key::VarChr('f'), Type::Func(vec![x, y], x), true)
            .unwrap();
    }

    let f = state.push_index(Type::Key(Key::VarChr('f')));
    let a = state.push_index(Type::Key(Key::VarChr('a')));
    let b = state.push_index(Type::Key(Key::VarChr('b')));

    {
        let call_inner = state.push_index(Type::Call(f, vec![a, b]));
        let call_outer = state.push_index(Type::Call(call_inner, vec![]));

        let _ = state.collect(call_outer).unwrap();
    }

    {
        let call_inner = state.push_index(Type::Call(f, vec![b, a]));
        let call_outer = state.push_index(Type::Call(call_inner, vec![]));

        let _ = state.collect(call_outer).unwrap();
    }

    let index = state.deref(&Key::VarChr('a')).unwrap();
    println!("{state}");
    println!("{}", TypesAndIndex(&state.types, index));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_a_eq_a() {
        let mut state = State::default();
        let key = Key::VarChr('x');
        let atom = Type::Atom('a');
        assert!(state.push_and_insert(key.clone(), atom.clone(), false).is_ok());
        assert!(state.push_and_insert(key, atom, false).is_ok());
    }

    #[test]
    fn test_a_ne_b() {
        let mut state = State::default();
        let key = Key::VarChr('x');
        assert!(state.push_and_insert(key.clone(), Type::Atom('a'), false).is_ok());
        assert!(state.push_and_insert(key, Type::Atom('b'), false).is_err());
    }
}
