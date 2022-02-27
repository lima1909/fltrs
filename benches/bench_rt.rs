use criterion::{criterion_group, criterion_main, Criterion};
use fltrs::{Filterable, PathResolver};

fn rt_exp(c: &mut Criterion) {
    use fltrs::operator::Operators;
    use fltrs::{create_path_executor, parse};

    let ps = get_points();

    let p = Point {
        name: "Foo".into(),
        x: 5,
    };

    let ops = Operators::default();
    let exp = parse(r#"x = 42 or name = "Point""#).unwrap();
    let mut exp = create_path_executor::<Point>(exp, &ops);
    exp.prepare(&p).unwrap();

    c.bench_function("rt.Exp", |b| {
        b.iter(|| {
            assert_eq!(24, ps.iter().filter(|p| exp.exec(*p)).count());
        })
    });
}

fn std_rust(c: &mut Criterion) {
    let ps = get_points();
    let pp = String::from("Point");

    c.bench_function("std.Rust", |b| {
        b.iter(|| {
            assert_eq!(
                24,
                ps.iter().filter(|p| p.x == 42 || p.name.eq(&pp)).count()
            );
        })
    });
}

criterion_group!(benches, std_rust, rt_exp);
criterion_main!(benches);

struct Point {
    name: String,
    x: i32,
}

impl PathResolver for Point {
    fn path_to_index(&self, path: &str) -> Option<usize> {
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
