use core::fmt::{Debug, Display};
use core::str::FromStr;

#[derive(PartialEq, PartialOrd, Debug)]
pub(crate) struct Predicate {
    pub(crate) path: Option<String>,
    pub(crate) op: String,
    pub(crate) value: Value,
}

impl Display for Predicate {
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(p) = &self.path {
            write!(fm, "{} ", p)?;
        }
        write!(fm, "{} {}", &self.op, &self.value)
    }
}

#[derive(PartialEq, PartialOrd, Debug)]
pub enum Value {
    String(String),
    CopyValue(CopyValue),
}

impl Display for Value {
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::String(s) => write!(fm, "{}", s),
            Value::CopyValue(c) => write!(fm, "{}", c),
        }
    }
}

#[derive(Clone, Copy)]
pub enum RefValue<'a> {
    String(&'a dyn AsRef<str>),
    CopyValue(CopyValue),
}

impl Display for RefValue<'_> {
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RefValue::String(s) => write!(fm, "{}", s.as_ref()),
            RefValue::CopyValue(c) => write!(fm, "{}", c),
        }
    }
}

impl Debug for RefValue<'_> {
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RefValue::String(s) => write!(fm, "{}", s.as_ref()),
            RefValue::CopyValue(c) => write!(fm, "{}", c),
        }
    }
}

#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
pub enum CopyValue {
    Char(char),
    Bool(bool),
    Number(Number),
}

impl Display for CopyValue {
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::CopyValue::*;

        match self {
            Char(c) => write!(fm, "{}", c),
            Bool(b) => write!(fm, "{}", b),
            Number(n) => write!(fm, "{}", n),
        }
    }
}

#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
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

impl Number {
    #[inline]
    pub(crate) fn default(val: &str) -> core::result::Result<Self, String> {
        if val.contains('.') {
            Number::try_from_as("f64", val)
        } else {
            Number::try_from_as("i32", val)
        }
    }

    #[inline]
    pub fn try_from_as(as_type: &str, val: &str) -> core::result::Result<Self, String> {
        match as_type {
            "f32" => Ok(Number::F32(
                f32::from_str(val).map_err(|err| err.to_string())?,
            )),
            "f64" => Ok(Number::F64(
                f64::from_str(val).map_err(|err| err.to_string())?,
            )),
            "i8" => Ok(Number::I8(
                i8::from_str(val).map_err(|err| err.to_string())?,
            )),
            "i16" => Ok(Number::I16(
                i16::from_str(val).map_err(|err| err.to_string())?,
            )),
            "i32" => Ok(Number::I32(
                i32::from_str(val).map_err(|err| err.to_string())?,
            )),
            "i64" => Ok(Number::I64(
                i64::from_str(val).map_err(|err| err.to_string())?,
            )),
            "i128" => Ok(Number::I128(
                i128::from_str(val).map_err(|err| err.to_string())?,
            )),
            "u8" => Ok(Number::U8(
                u8::from_str(val).map_err(|err| err.to_string())?,
            )),
            "u16" => Ok(Number::U16(
                u16::from_str(val).map_err(|err| err.to_string())?,
            )),
            "u32" => Ok(Number::U32(
                u32::from_str(val).map_err(|err| err.to_string())?,
            )),
            "u64" => Ok(Number::U64(
                u64::from_str(val).map_err(|err| err.to_string())?,
            )),
            "u128" => Ok(Number::U128(
                u128::from_str(val).map_err(|err| err.to_string())?,
            )),
            "usize" => Ok(Number::Usize(
                usize::from_str(val).map_err(|err| err.to_string())?,
            )),

            _ => Err(String::from("is not an valid 'as type' for a number")),
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
    Value : Value::String => String

    Value : Value::CopyValue => char
    Value : Value::CopyValue => bool
    Value : Value::CopyValue => usize
    Value : Value::CopyValue => u8
    Value : Value::CopyValue => u16
    Value : Value::CopyValue => u32
    Value : Value::CopyValue => u64
    Value : Value::CopyValue => u128
    Value : Value::CopyValue => i8
    Value : Value::CopyValue => i16
    Value : Value::CopyValue => i32
    Value : Value::CopyValue => i64
    Value : Value::CopyValue => i128
    Value : Value::CopyValue => f32
    Value : Value::CopyValue => f64

    CopyValue: CopyValue::Char => char
    CopyValue: CopyValue::Bool => bool
    CopyValue : CopyValue::Number => usize
    CopyValue : CopyValue::Number => u8
    CopyValue : CopyValue::Number => u16
    CopyValue : CopyValue::Number => u32
    CopyValue : CopyValue::Number => u64
    CopyValue : CopyValue::Number => u128
    CopyValue : CopyValue::Number => i8
    CopyValue : CopyValue::Number => i16
    CopyValue : CopyValue::Number => i32
    CopyValue : CopyValue::Number => i64
    CopyValue : CopyValue::Number => i128
    CopyValue : CopyValue::Number => f32
    CopyValue : CopyValue::Number => f64

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

impl<'a> ::core::cmp::PartialEq<Value> for RefValue<'a> {
    #[inline]
    fn eq(&self, other: &Value) -> bool {
        match (other, self) {
            (Value::String(vs), RefValue::String(vrs)) => vs.eq(vrs.as_ref()),
            (Value::CopyValue(v), RefValue::CopyValue(rv)) => v.eq(rv),
            _ => false,
        }
    }
}

impl<'a> ::core::cmp::PartialOrd<Value> for RefValue<'a> {
    #[inline]
    fn partial_cmp(&self, other: &Value) -> Option<::core::cmp::Ordering> {
        match (other, self) {
            (Value::String(vs), RefValue::String(vrs)) => vrs.as_ref().partial_cmp(vs),
            (Value::CopyValue(v), RefValue::CopyValue(rv)) => rv.partial_cmp(v),
            _ => None,
        }
    }
}

macro_rules! number_into_ref_value {
    ($($p:path => $t:ty)*) => ($(
        impl<'v> ::core::convert::From<$t> for RefValue<'v> {
            #[inline]
            fn from(v: $t) -> Self {
                RefValue::CopyValue(CopyValue::Number($p(v)))
            }
        }
    )*);
}

number_into_ref_value! {
    Number::Usize => usize
    Number::U8    => u8
    Number::U16   => u16
    Number::U32   => u32
    Number::U64   => u64
    Number::U128  => u128
    // N: Number::I32 => isize
    Number::I8    => i8
    Number::I16   => i16
    Number::I32   => i32
    Number::I64   => i64
    Number::I128  => i128
    Number::F32   => f32
    Number::F64   => f64
}

impl<'a> From<&'a String> for RefValue<'a> {
    fn from(s: &'a String) -> Self {
        RefValue::String(s)
    }
}

impl<'a> From<&'a &str> for RefValue<'a> {
    fn from(s: &'a &str) -> Self {
        RefValue::String(s)
    }
}

impl<'a> From<bool> for RefValue<'a> {
    fn from(b: bool) -> Self {
        RefValue::CopyValue(CopyValue::Bool(b))
    }
}

impl<'a> From<char> for RefValue<'a> {
    fn from(c: char) -> Self {
        RefValue::CopyValue(CopyValue::Char(c))
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

        assert!(10u128 == CopyValue::Number(Number::U128(10)));
        assert!(10 as u8 > CopyValue::Number(Number::U8(9)));
        assert!(10u16 < CopyValue::Number(Number::U16(11)));

        assert!(10usize == Value::CopyValue(CopyValue::Number(Number::Usize(10))));
        assert!(10 as u32 > Value::CopyValue(CopyValue::Number(Number::U32(9))));
        assert!(10u64 < Value::CopyValue(CopyValue::Number(Number::U64(11))));

        assert!(
            RefValue::CopyValue(CopyValue::Number(Number::Usize(10)))
                == Value::CopyValue(CopyValue::Number(Number::Usize(10)))
        );
        assert!(
            RefValue::CopyValue(CopyValue::Number(Number::Usize(20)))
                > Value::CopyValue(CopyValue::Number(Number::Usize(10)))
        );
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

        assert!(10.2 == CopyValue::Number(Number::F64(10.2)));
        assert!(10.2 > CopyValue::Number(Number::F64(9.3)));
        assert!(10.2 < Value::CopyValue(CopyValue::Number(Number::F64(11.3))));

        assert!(
            RefValue::CopyValue(CopyValue::Number(Number::F64(10.2)))
                < Value::CopyValue(CopyValue::Number(Number::F64(11.3)))
        );
    }

    #[test]
    fn cmp_bool() {
        assert!(true == CopyValue::Bool(true));
        assert!(true > false);
        assert!(true > Value::CopyValue(CopyValue::Bool(false)));

        assert_eq!(false.to_string(), CopyValue::Bool(false).to_string());

        assert!(
            RefValue::CopyValue(CopyValue::Bool(true)) > Value::CopyValue(CopyValue::Bool(false))
        );
    }

    #[test]
    fn cmp_string() {
        assert!(String::from("foo") == Value::String("foo".into()));
        assert!(String::from("foo") > String::from("bar"));
        assert!(String::from("foo") > Value::String("bar".into()));

        assert!("foo" == Value::String("foo".into()));
        assert!("foo" > Value::String("bar".into()));

        assert_eq!("foo".to_string(), Value::String("foo".into()).to_string());
        assert_eq!(RefValue::String(&"foo"), Value::String("foo".into()));
    }

    #[test]
    fn cmp_char() {
        assert!('X' == CopyValue::Char('X'));
        assert!('Y' > 'X');
        assert!('Y' > Value::CopyValue(CopyValue::Char('X')));

        assert_eq!('Z'.to_string(), CopyValue::Char('Z').to_string());

        assert!(RefValue::CopyValue(CopyValue::Char('Y')) > Value::CopyValue(CopyValue::Char('X')));
    }

    #[test]
    fn number_default() {
        assert_eq!(0, Number::default("0").unwrap());
        assert_eq!(0.1, Number::default("0.1").unwrap());
    }

    #[test]
    fn number_try_from_as() {
        for as_type in [
            "f32", "f64", "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128",
            "usize",
        ] {
            Number::try_from_as(as_type, "0").unwrap();
        }
    }

    #[test]
    fn number_try_from_as_err() {
        Number::try_from_as("foo", "0").err().unwrap();
        Number::try_from_as("i32", "a").err().unwrap();
    }
}
