# Fltrs [![Build Status]][Build Action] [![Latest Version]][crates.io] 

[Build Status]: https://github.com/lima1909/fltrs/actions/workflows/continuous_integration.yml/badge.svg
[Build Action]: https://github.com/lima1909/fltrs/actions
[Latest Version]: https://img.shields.io/crates/v/fltrs.svg
[crates.io]: https://crates.io/crates/fltrs

*Easy to define filters for querying lists.* `Fltrs` has **no** dependencies!

## Overview

Fltrs want to support creating easy, fast and expandable filters for iterable things (like Vec, Array, Map, Set, ...) in rust.
A filter is created based on an input string (query).
This has particular advantages if the filter is created at runtime, i.e. in a GUI or command line tool (CLI).

## Extensions:

It is possible, to expand the filter/query to your own needs:
- create your own [operator](https://docs.rs/fltrs/latest/fltrs/operator/index.html)
- create a converter for the filter Value (e.g.: conversion of units).

You can find examples on the [Query](https://docs.rs/fltrs/latest/fltrs/struct.Query.html) builder page.

## Examples:

```rust
use fltrs::query;

let result: Vec<_> = [3, 2, 1, 4, 5, 7, 5, 4, 3]
        .into_iter()
        .filter(query("> 1 and < 5").unwrap())
        .collect();

assert_eq!(vec![3, 2, 4, 4, 3], result);
```

```rust
use fltrs::query;

let result: Vec<_> = ["Inge", "Petra", "Paul", "Egon", "Peter"]
        .into_iter()
        .filter(query("contains 'e'").unwrap())
        .collect();

assert_eq!(vec!["Inge", "Petra", "Peter"], result);
```

### Option queries:

```rust
use fltrs::query;

let result: Vec<Option<char>> = [None, Some('a'), None, Some('b'), Some('c'), Some('a')]
        .into_iter()
        .filter(query(" != 'a' and not = none ").unwrap())
        .collect();

assert_eq!(vec![Some('b'), Some('c')], result);
```

### Nested and Not queries:

```rust
use fltrs::query;

let result: Vec<_> = [3, 2, 1, 4, 5, 7, 5, 4, 3]
        .into_iter()
        .filter(query("(= 1 or = 5) and > 1").unwrap())
        .collect();

assert_eq!(vec![5, 5], result);
```

```rust
use fltrs::query;

let result: Vec<_> = [3, 2, 1, 4, 5, 7, 5, 4, 3]
        .into_iter()
        .filter(query("not( (= 1 or = 5) and > 1)").unwrap())
        .collect();

assert_eq!(vec![3, 2, 1, 4, 7, 4, 3], result);
```

### Fltrs supported queries on structs too.

This is possible, if the struct implement the trait: [`PathResolver`].

```rust
use fltrs::{PathResolver, Filterable, query};

#[derive(PartialEq, Debug)]
struct Point {
    name: &'static str,
    x:    i32,
    y:    i32,
}

impl PathResolver for Point {
    fn path_to_index(path: &str) -> Option<usize> {
        match path {
            "name"  => Some(0),
            "x"     => Some(1),
            "y"     => Some(2),
            _ => None,
        }
    }

    fn value(&self, idx: usize) -> &dyn Filterable {
        match idx {
            0 => &self.name,
            1 => &self.x,
            _ => &self.y,
        }
    }
}

let result: Vec<Point> =
    [
      Point { name: "Point_1_3", x: 1, y: 3},
      Point { name: "Point_3_3", x: 3, y: 3},
      Point { name: "Point_2_6", x: 2, y: 6},
    ]
     .into_iter()
     .filter(query(r#"x one_of [3, 7]"#).unwrap())
     .collect();

assert_eq!(vec![Point { name: "Point_3_3", x: 3, y: 3}], result);
```
