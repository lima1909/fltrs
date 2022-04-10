use crate::error::FltrError;
use crate::operator::Operators;
use crate::token::{Exp, Filter};
use crate::{PathResolver, Predicate, Result};

pub fn query<PR: PathResolver + 'static>(exp: Exp, ops: &Operators<PR>) -> Result<Predicate<PR>> {
    if exp.ands.is_empty() {
        return Err(FltrError("empty expression is not allowed".into()));
    }

    let mut it = exp.ands.into_iter();
    let mut query;

    let ands = it.next().expect("expect at least one Ands");
    if ands.is_or() {
        // only ONE filter
        query = from_filter(ands.filter, ops)?;
    } else {
        // only AND filters
        query = from_filter(ands.filter, ops)?;
        for a in ands.next {
            let ex = from_filter(a, ops)?;
            query = Box::new(move |pr| (query)(pr) && (ex)(pr));
        }
    }

    for ands in it {
        if ands.is_or() {
            let ex = from_filter(ands.filter, ops)?;
            query = Box::new(move |pr| (query)(pr) || (ex)(pr));
        } else {
            let mut sub_ands = from_filter(ands.filter, ops)?;
            for a in ands.next {
                let ex = from_filter(a, ops)?;
                sub_ands = Box::new(move |pr| (sub_ands)(pr) && (ex)(pr));
            }
            query = Box::new(move |pr| (query)(pr) || (sub_ands)(pr));
        }
    }

    Ok(query)
}

fn from_filter<PR: PathResolver + 'static>(
    filter: Filter,
    ops: &Operators<PR>,
) -> Result<Predicate<PR>> {
    match filter {
        Filter::Predicate(p) => {
            let path = p.path.unwrap_or_default();
            let idx = PR::path_to_index(&path).ok_or_else(|| {
                FltrError(format!(
                    "invalid path: '{}' for value: '{}'",
                    path, &p.value
                ))
            })?;
            Ok(ops
                .get(&p.op, idx, p.value)
                .ok_or_else(|| FltrError(format!("invalid operation: '{}'", &p.op)))?)
        }
        Filter::Not(exp) => Ok(Not(query(exp, ops)?).into()),
        Filter::Nested(exp) => query(exp, ops),
    }
}

struct Not<PR>(Predicate<PR>);

impl<PR: PathResolver + 'static> From<Not<PR>> for Predicate<PR> {
    fn from(n: Not<PR>) -> Self {
        Box::new(move |pr| !(n.0)(pr))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{parser::parse, Filterable};
    use test_case::test_case;

    #[test_case("= 7" => true; "eq 7")]
    #[test_case("== 7" => true; "eq eq 7")]
    #[test_case("> 6" => true; "gt 6")]
    #[test_case("< 8" => true; "lt 8")]
    #[test_case("len 1 as usize" => true; "len 1 usize")]
    #[test_case("len 1" => true; "len 1")]
    #[test_case(r#"starts_with "7""# => true; "starts_with 7")]
    #[test_case("self != 8" => true; "self ne 8")]
    #[test_case("= 8" => false; "eq 8")]
    #[test_case("> 7" => false; "gt 7")]
    #[test_case("< 7" => false; "lt 7")]
    fn query_i32(input: &str) -> bool {
        let exp = parse(input).unwrap();
        query(exp, &Operators::default()).unwrap()(&7)
    }

    #[test_case(r#"= "Jasmin""# => true; "eq Jasmin")]
    #[test_case(r#"< "jasmin""# => true; "lt jasmin")]
    #[test_case(r#"> "Ina""# => true; "lt Ina")]
    #[test_case(r#"len 6"# => true; "len 6")]
    #[test_case(r#"starts_with "J""# => true; "starts_with J")]
    fn query_string(input: &str) -> bool {
        // assert!('c' > 'C');
        let exp = parse(input).unwrap();
        query(exp, &Operators::default()).unwrap()(&"Jasmin")
    }

    #[test_case(r#"= "Jasmin""# => true; "eq Jasmin")]
    #[test_case(r#"< "jasmin""# => true; "lt jasmin")]
    #[test_case(r#"> "Ina""# => true; "lt Ina")]
    #[test_case(r#"len 6"# => true; "len 6")]
    #[test_case(r#"not(len 9)"# => true; "not len 9")]
    #[test_case(r#"starts_with "J""# => true; "starts_with J")]
    #[test_case(r#"(!= "Inge")"# => true; "nested ne Inge")]
    #[test_case(r#"!= "Inge" and != "Paul""# => true; "ne Inge and ne Paul")]
    #[test_case(r#"= "Paul" or = "Jasmin""# => true; "eq Paul or eq Jasmin")]
    #[test_case(r#"!= "Inge" and != "Paul" and != "Peter""# => true; "ne Inge and ne Paul and ne Peter")]
    #[test_case(r#"!= "Inge" and (!= "Paul" and != "Peter")"# => true; "nested ne Inge and ne Paul and ne Peter")]
    fn query_nested_not(input: &str) -> bool {
        let exp = parse(input).unwrap();
        query(exp, &Operators::default()).unwrap()(&"Jasmin")
    }

    struct Car<'a> {
        name: &'a str,
        ps: i32,
        size: i32,
    }

    impl PathResolver for Car<'_> {
        fn path_to_index(path: &str) -> Option<usize> {
            match path {
                "name" => Some(0),
                "ps" => Some(1),
                "size" => Some(2),
                _ => None,
            }
        }

        fn value(&self, idx: usize) -> &dyn Filterable {
            match idx {
                1 => &self.ps,
                2 => &self.size,
                _ => &self.name,
            }
        }
    }

    #[test_case("size = 54" => true; "size eq 54")]
    #[test_case("size len 2" => true; "len eq 2")]
    #[test_case("ps > 140" => true; "ps gt 140")]
    #[test_case(r#"name = "BMW""# => true; "name eq BMW")]
    #[test_case(r#"name < "bmw""# => true; "ne bmw")]
    #[test_case(r#"name != "Audi" "# => true; "name ne Audi")]
    #[test_case(r#"name starts_with "BM" "# => true; "name starts_with BM")]
    fn query_car(input: &str) -> bool {
        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };
        let exp = parse(input).unwrap();
        query(exp, &Operators::default()).unwrap()(&car)
    }

    #[test_case(r#"name = "BMW" and ps > 100"# => true; "name eq BMW and ps gt 100")]
    #[test_case(r#"name = "BMW" and ps > 100 and size len 2"# => true; "name eq BMW and ps gt 100 and size len 2")]
    fn query_and_car(input: &str) -> bool {
        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };
        let exp = parse(input).unwrap();
        query(exp, &Operators::default()).unwrap()(&car)
    }

    #[test_case(r#"name = "BMW" or ps > 100"# => true; "name eq BMW or ps gt 100")]
    #[test_case(r#"name = "Audi" or ps > 100"# => true; "name eq Audi or ps gt 100")]
    #[test_case(r#"name = "Audi" or size = 200 or ps = 142"# => true; "3 ors")]
    #[test_case(r#"name = "Audi" or size = 200"# => false; "name eq Audi or size eq 200")]
    fn query_or_car(input: &str) -> bool {
        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };
        let exp = parse(input).unwrap();
        query(exp, &Operators::default()).unwrap()(&car)
    }

    #[test_case(r#"name = "BMW" and ps != 100 or size = 54"# => true; "and or")]
    #[test_case(r#"name = "Audi" or ps = 142 and size = 54 or size > 100"# => true; "false || true && true || false -> true")]
    #[test_case(r#"name = "BMW" or ps = 142 and size = 158 or size > 100"# => true; "true || true && false || false -> true")]
    fn query_or_and_car(input: &str) -> bool {
        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };
        let exp = parse(input).unwrap();
        query(exp, &Operators::default()).unwrap()(&car)
    }

    #[test_case(r#"name = "BMW" "# => true; "name eq BMW")]
    #[test_case(r#"name != "Audi" "# => true; "name ne Audi")]
    #[test_case(r#"not(name = "Audi") "# => true; "not name eq Audi")]
    #[test_case(r#"not(name = "BMW") "# => false; "not name eq BMW")]
    #[test_case(r#"ps >= 142 "# => true; "ps ge 142")]
    #[test_case(r#"ps > 141 "# => true; "ps gt 141")]
    #[test_case(r#"not(size > 141)"# => true; "not size gt 141")]
    #[test_case(r#"(size <= 141)"# => true; "nested size le 141")]
    #[test_case(r#"size <= 54 and ps >= 142"# => true; "size le 54 and ps ge 142")]
    #[test_case(r#"size < 54 or ps >= 142"# => true; "size lt 54 or ps ge 142")]
    #[test_case(r#"size = 54 and ps = 142 and name = "BMW" "# => true; "size le 54 and ps ge 142 and name eq BMW")]
    #[test_case(r#"size = 54 and (ps = 142 and name = "BMW") "# => true; "nested size eq 54 and ps ge 142 and name eq BMW")]
    #[test_case(r#"size = 54 and (ps = 500 or name = "BMW") "# => true; "nested size eq 54 and ps eq 500 or name eq BMW")]
    fn query_nested_not_car(input: &str) -> bool {
        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };
        let exp = parse(input).unwrap();
        query(exp, &Operators::default()).unwrap()(&car)
    }

    #[test_case(r#"= "bmw""#, FltrError("invalid path: '' for value: 'bmw'".into()); "empty path")]
    #[test_case(r#"foo = "bmw""#, FltrError("invalid path: 'foo' for value: 'bmw'".into()); "invalid path")]
    fn query_err(input: &str, err: FltrError) {
        let exp = parse(input).unwrap();
        assert_eq!(err, query::<Car>(exp, &Operators::default()).err().unwrap());
    }
}
