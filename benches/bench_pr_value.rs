use criterion::{criterion_group, criterion_main, Criterion};
use fltrs::{
    value::{Number, Value},
    Filterable, PathResolver,
};

const MAX: usize = 100;
const NAME: &'static str = "Foo";

fn pr(c: &mut Criterion) {
    let p = Point {
        name: NAME.into(),
        x: 5,
    };
    let pr: &dyn PathResolver = &p;
    let name = Value::Text(NAME.into());

    c.bench_function("pr", |b| {
        b.iter(|| {
            criterion::black_box((0..MAX).for_each(|_| {
                assert!(pr.value(0).eq(&name));
                assert!(pr.value(1).eq(&Value::Number(Number::I32(5))));
            }));
        })
    });
}

fn array(c: &mut Criterion) {
    let p = Point {
        name: "Foo".into(),
        x: 5,
    };
    let a: [&dyn Filterable; 2] = [&p.name, &p.x];
    let name = Value::Text(NAME.into());

    c.bench_function("array", |b| {
        b.iter(|| {
            criterion::black_box((0..MAX).for_each(|_| {
                assert!(a[0].eq(&name));
                assert!(a[1].eq(&Value::Number(Number::I32(5))));
            }));
        })
    });
}

criterion_group!(benches, pr, array);
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
