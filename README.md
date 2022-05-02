[![Build Status]][Build Action] [![Latest Version]][crates.io] 

[Build Status]: https://github.com/lima1909/fltrs/actions/workflows/continuous_integration.yml/badge.svg
[Build Action]: https://github.com/lima1909/fltrs/actions
[Latest Version]: https://img.shields.io/crates/v/fltrs.svg
[crates.io]: https://crates.io/crates/fltrs

# Fltrs

Fltrs want to support creating easy, fast and expandable filters for iterable things (like Vec, Array, Map, Set, ...) in rust. A filter is created based on an input string (query). This has particular advantages if the filter is created at runtime, i.e. in a GUI or command line tool (CLI).


```rust
use fltrs::query;

assert_eq!(
    5,
    [3, 2, 1, 4, 5, 7, 5, 4, 3]
        .into_iter()
        .filter(query("> 1 and < 5").unwrap())
        .count()
);
```

Fltrs supported queries on structs too. This is possible, if the struct implement the trait: `fltrs::PathResolver`.

```rust
use fltrs::{PathResolver, Filterable, query};

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


assert_eq!(
    1,
    [
      Point { name: "Point_1_3", x: 1, y: 3},
      Point { name: "Point_2_3", x: 2, y: 3},
      Point { name: "Point_2_6", x: 2, y: 6},
    ]
        .into_iter()
        .filter(query(r#"name starts_with 'P' and x > 1 and y < 5"#).unwrap())
        .count()
);
```
