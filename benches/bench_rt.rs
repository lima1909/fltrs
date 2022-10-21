use criterion::{criterion_group, criterion_main, Criterion};
use fltrs::{path_resolver, Filterable};

fn query(c: &mut Criterion) {
    c.bench_function("query", |b| {
        b.iter(|| {
            assert_eq!(
                24,
                get_points()
                    .into_iter()
                    .filter(fltrs::query(r#"x == 42 or name == "Point""#).unwrap())
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
                    .into_iter()
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

path_resolver!(Point: name, x);

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
