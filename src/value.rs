//! The value mod contains different value types, which are the result of the parse process.
use core::fmt::{Debug, Display};
use core::str::FromStr;

#[derive(PartialEq, PartialOrd, Debug)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i32),
    Float(f64),
    Char(char),
    Text(String),
    List(Vec<Value>),
}

impl Display for Value {
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(fm, "NULL"),
            Value::Bool(v) => write!(fm, "{}", v),
            Value::Int(v) => write!(fm, "{}", v),
            Value::Float(v) => write!(fm, "{}", v),
            Value::Char(v) => write!(fm, "{}", v),
            Value::Text(v) => write!(fm, "{}", v),
            Value::List(v) => write!(fm, "{:?}", v),
        }
    }
}

#[inline]
pub(crate) fn str_to_number(s: &str) -> core::result::Result<Value, String> {
    if s.contains('.') {
        Ok(Value::Float(
            f64::from_str(s).map_err(|err| err.to_string())?,
        ))
    } else {
        Ok(Value::Int(i32::from_str(s).map_err(|err| err.to_string())?))
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! partial_eq_cmp {
        ( $val:path => $($t:ty) + ) => {
            $( partial_eq_cmp!( main $val => $t); )+
        };

        ( $val:path as $as:ty => $($t:ty) + ) => {
            $( partial_eq_cmp!( main $val as $as => $t); )+
        };

        ( main $val:path $(as $as:ty)? => $t:ty ) => {
            impl ::core::cmp::PartialEq<Value> for $t {
                #[inline]
                fn eq(&self, other: &crate::value::Value) -> bool {
                    if let $val(v) = other {
                        return  (*self $(as  $as)?).eq(v);
                    }
                    false
                }
            }

            impl ::core::cmp::PartialOrd<Value> for $t {
                #[inline]
                fn partial_cmp(&self, other: &crate::value::Value) -> Option<::core::cmp::Ordering> {
                    if let $val(v) = other {
                        return (*self $(as  $as)?).partial_cmp(v);
                    }
                    None
                }
            }
       }
}

partial_eq_cmp! { Value::Bool => bool }
partial_eq_cmp! { Value::Char => char }
partial_eq_cmp! { Value::Text => String &str}
partial_eq_cmp! { Value::Int as i32 => usize u8 u16 u32 u64 u128 isize i8 i16 i32 i64 }

impl PartialEq<Value> for f64 {
    #[inline]
    fn eq(&self, other: &Value) -> bool {
        if let Value::Float(f) = other {
            return (self - f).abs() < f64::EPSILON;
        }
        false
    }
}

impl PartialOrd<Value> for f64 {
    #[inline]
    fn partial_cmp(&self, other: &Value) -> Option<::core::cmp::Ordering> {
        if let Value::Float(f) = other {
            return self.partial_cmp(f);
        }
        None
    }
}

impl PartialEq<Value> for f32 {
    #[inline]
    fn eq(&self, other: &Value) -> bool {
        if let Value::Float(f) = other {
            return (self - (*f as f32)).abs() < f32::EPSILON;
        }
        false
    }
}

impl PartialOrd<Value> for f32 {
    #[inline]
    fn partial_cmp(&self, other: &Value) -> Option<::core::cmp::Ordering> {
        if let Value::Float(f) = other {
            return self.partial_cmp(&(*f as f32));
        }
        None
    }
}

impl<V> PartialEq<Value> for Option<V>
where
    V: PartialEq<Value>,
{
    #[inline]
    fn eq(&self, other: &Value) -> bool {
        match self {
            Some(v) => v.eq(other),
            None if &Value::Null == other => true,
            _ => false,
        }
    }
}

impl<V> PartialOrd<Value> for Option<V>
where
    V: PartialOrd<Value>,
{
    #[inline]
    fn partial_cmp(&self, other: &Value) -> Option<::core::cmp::Ordering> {
        match self {
            Some(v) => v.partial_cmp(other),
            None => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn cmp_number_integer() {
        assert!(10 == Value::Int(10));
        assert!(10 > Value::Int(9));
        assert!(10 < Value::Int(11));

        assert_eq!(10.to_string(), Value::Int(10).to_string());

        assert!(10u128 == Value::Int(10));
        assert!(10 as u8 > Value::Int(9));
        assert!(10u16 < Value::Int(11));

        assert!(10usize == Value::Int(10));
        assert!(10 as u32 > Value::Int(9));
        assert!(10u64 < Value::Int(11));
        assert!(10i8 < Value::Int(12));
    }

    #[test]
    fn cmp_number_float() {
        assert!(10.2 as f32 == Value::Float(10.2));
        assert!(10.2 as f64 == Value::Float(10.2));
        assert!(10.2 as f32 > Value::Float(9.3));
        assert!((10.2 as f32) < Value::Float(11.3));

        assert_eq!(10.2f32.to_string(), Value::Float(10.2).to_string());

        assert!(10.2 == Value::Float(10.2));
        assert!(10.2 > Value::Float(9.3));
        assert!(10.2 < Value::Float(11.3));

        assert_eq!(10.2.to_string(), Value::Float(10.2).to_string());

        assert!(10.2 == Value::Float(10.2));
        assert!(10.2 > Value::Float(9.3));
        assert!(10.2 < Value::Float(11.3));
    }

    #[test]
    fn cmp_bool() {
        assert!(true == Value::Bool(true));
        assert!(true > false);
        assert!(true > Value::Bool(false));

        assert_eq!(false.to_string(), Value::Bool(false).to_string());
    }

    #[test]
    fn cmp_string() {
        assert!(String::from("foo") == Value::Text("foo".into()));
        assert!(String::from("foo") > String::from("bar"));
        assert!(String::from("foo") > Value::Text("bar".into()));

        assert!("foo" == Value::Text("foo".into()));
        assert!("foo" > Value::Text("bar".into()));

        assert_eq!("foo".to_string(), Value::Text("foo".into()).to_string());
    }

    #[test]
    fn cmp_char() {
        assert!('X' == Value::Char('X'));
        assert!('Y' > 'X');
        assert!('Y' > Value::Char('X'));

        assert_eq!('Z'.to_string(), Value::Char('Z').to_string());
    }

    #[test]
    fn conv_str_to_number() {
        assert_eq!(0, str_to_number("0").unwrap());
        assert_eq!(0.1, str_to_number("0.1").unwrap());
    }

    #[test]
    fn conv_str_to_number_err() {
        str_to_number("foo").err().unwrap();
        str_to_number("i32").err().unwrap();
    }
}
