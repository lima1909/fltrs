use fltrs::exec_new;
use fltrs::operator::Operators;
use fltrs::runtime::Executor;
use fltrs::{Filterable, PathResolver};

fn main() {
    let ps = get_points();

    let p = Point {
        name: "Foo".into(),
        x: 5,
    };

    let ops = Operators::default();
    let mut rt = exec_new(r#"x = 42 or name = "Point" "#, &ops);
    rt.prepare(&p).unwrap();

    assert_eq!(24, ps.iter().filter(|p| rt.exec(*p)).count());
}

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
