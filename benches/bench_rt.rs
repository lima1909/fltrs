use criterion::{criterion_group, criterion_main, Criterion};
use fltrs::{Filterable, PathResolver};

fn query(c: &mut Criterion) {
    c.bench_function("query", |b| {
        b.iter(|| {
            assert_eq!(
                24,
                get_points()
                    .into_iter()
                    .filter(fltrs::fltrs(r#"x == 42 or name == "Point""#))
                    .count()
            );
        })
    });
}

fn std_rust(c: &mut Criterion) {
    c.bench_function("std.Rust", |b| {
        b.iter(|| {
            assert_eq!(
                24,
                get_points()
                    .iter()
                    .filter(|p| p.x == 42 || p.name == String::from("Point"))
                    .count()
            );
        })
    });
}

criterion_group!(benches, query, std_rust);
criterion_main!(benches);

struct Point {
    name: String,
    x: i32,
}

impl PathResolver for Point {
    fn path_to_index(path: &str) -> Option<usize> {
        match path {
            "name" => Some(0),
            "x" => Some(1),
            _ => None,
        }
    }

    fn value(&self, idx: usize) -> &dyn Filterable {
        match idx {
            1 => &self.x,
            _ => &self.name,
        }
    }
}

fn get_points() -> Vec<Point> {
    vec![
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Bad"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Bad"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Bad"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Bad"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Bad"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Bad"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Bad"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Bad"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Bad"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Bad"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Bad"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
        Point {
            x: 43,
            name: String::from("Bad"),
        },
        Point {
            x: 43,
            name: String::from("Point"),
        },
    ]
}
