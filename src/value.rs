#![allow(dead_code)] // TODO: remove this

use core::fmt::Display;

pub enum Value {
    Char(char),
    String(String),
    Bool(bool),
    Number(Number),
}

impl Display for Value {
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Char(c) => write!(fm, "{}", c),
            Value::String(s) => write!(fm, "{}", s),
            Value::Bool(b) => write!(fm, "{}", b),
            Value::Number(n) => write!(fm, "{}", n),
        }
    }
}

pub enum Number {
    F32(f32),
    F64(f64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    Usize(usize),
}

impl Display for Number {
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::Number::*;

        match self {
            F32(v) => write!(fm, "{}", v),
            F64(v) => write!(fm, "{}", v),
            I8(v) => write!(fm, "{}", v),
            I16(v) => write!(fm, "{}", v),
            I32(v) => write!(fm, "{}", v),
            I64(v) => write!(fm, "{}", v),
            I128(v) => write!(fm, "{}", v),
            U8(v) => write!(fm, "{}", v),
            U16(v) => write!(fm, "{}", v),
            U32(v) => write!(fm, "{}", v),
            U64(v) => write!(fm, "{}", v),
            U128(v) => write!(fm, "{}", v),
            Usize(v) => write!(fm, "{}", v),
        }
    }
}

#[macro_export]
macro_rules! partial_eq_cmp {
    (  $($lt:ty : $p:path => $rt:ty) + ) => {
        $( partial_eq_cmp!( main $lt : $p => $rt); ) +
    };

    (  main $lt:ty : $p:path => $rt:ty ) => {
        impl ::core::cmp::PartialEq<$lt> for $rt {
            #[inline]
            fn eq(&self, other: &$lt) -> bool {
                match other {
                    $p(v) => self.eq(v),
                    _ => false,
                }
            }
        }

        impl ::core::cmp::PartialOrd<$lt> for $rt {
            #[inline]
            fn partial_cmp(&self, other: &$lt) -> Option<::core::cmp::Ordering> {
                match other {
                    $p(v) => self.partial_cmp(v),
                    _ => None,
                }
            }
        }
    };
}

partial_eq_cmp! {
    Value : Value::Char => char
    Value : Value::String => String
    Value : Value::Bool => bool
    Value : Value::Number => usize
    Value : Value::Number => u8
    Value : Value::Number => u16
    Value : Value::Number => u32
    Value : Value::Number => u64
    Value : Value::Number => u128
    Value : Value::Number => i8
    Value : Value::Number => i16
    Value : Value::Number => i32
    Value : Value::Number => i64
    Value : Value::Number => i128
    Value : Value::Number => f32
    Value : Value::Number => f64

    Number : Number::Usize => usize
    Number : Number::U8 => u8
    Number : Number::U16 => u16
    Number : Number::U32 => u32
    Number : Number::U64 => u64
    Number : Number::U128 => u128
    Number : Number::I8 => i8
    Number : Number::I16 => i16
    Number : Number::I32 => i32
    Number : Number::I64 => i64
    Number : Number::I128 => i128
    Number : Number::F32 => f32
    Number : Number::F64 => f64
}

impl ::core::cmp::PartialEq<Value> for &str {
    #[inline]
    fn eq(&self, other: &Value) -> bool {
        match other {
            Value::String(v) => self.eq(v),
            _ => false,
        }
    }
}

impl ::core::cmp::PartialOrd<Value> for &str {
    #[inline]
    fn partial_cmp(&self, other: &Value) -> Option<::core::cmp::Ordering> {
        match other {
            Value::String(v) => self.to_string().partial_cmp(v),
            _ => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn cmp_number_integer() {
        assert!(10 == Number::I32(10));
        assert!(10 > Number::I32(9));
        assert!(10 < Number::I32(11));

        assert_eq!(10.to_string(), Number::I32(10).to_string());

        assert!(10 == Value::Number(Number::I32(10)));
        assert!(10 > Value::Number(Number::I32(9)));
        assert!(10 < Value::Number(Number::I32(11)));
    }

    #[test]
    fn cmp_number_float() {
        assert!(10.2 as f32 == Number::F32(10.2));
        assert!(10.2 as f32 > Number::F32(9.3));
        assert!((10.2 as f32) < Number::F32(11.3));

        assert_eq!(10.2f32.to_string(), Number::F32(10.2).to_string());

        assert!(10.2 == Number::F64(10.2));
        assert!(10.2 > Number::F64(9.3));
        assert!(10.2 < Number::F64(11.3));

        assert_eq!(10.2.to_string(), Number::F64(10.2).to_string());

        assert!(10.2 == Value::Number(Number::F64(10.2)));
        assert!(10.2 > Value::Number(Number::F64(9.3)));
        assert!(10.2 < Value::Number(Number::F64(11.3)));
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
        assert!(String::from("foo") == Value::String("foo".into()));
        assert!(String::from("foo") > String::from("bar"));
        assert!(String::from("foo") > Value::String("bar".into()));

        assert!("foo" == Value::String("foo".into()));
        assert!("foo" > Value::String("bar".into()));

        assert_eq!("foo".to_string(), Value::String("foo".into()).to_string());
    }

    #[test]
    fn cmp_char() {
        assert!('X' == Value::Char('X'));
        assert!('Y' > 'X');
        assert!('Y' > Value::Char('X'));

        assert_eq!('Z'.to_string(), Value::Char('Z').to_string());
    }
}
