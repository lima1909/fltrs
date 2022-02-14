use core::fmt::Display;
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
        use self::Value::*;

        match self {
            String(s) => write!(fm, "{}", s),
            CopyValue(c) => write!(fm, "{}", c),
        }
    }
}

#[derive(PartialEq, PartialOrd, Debug)]
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

#[derive(PartialEq, PartialOrd, Debug)]
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

impl ::core::cmp::PartialEq<Value> for i32 {
    #[inline]
    fn eq(&self, other: &Value) -> bool {
        use self::{CopyValue::*, Number::*, Value::*};

        match other {
            CopyValue(Number(I32(v))) => self.eq(v),
            _ => false,
        }
    }
}

impl ::core::cmp::PartialOrd<Value> for i32 {
    #[inline]
    fn partial_cmp(&self, other: &Value) -> Option<::core::cmp::Ordering> {
        use self::{CopyValue::*, Number::*, Value::*};

        match other {
            CopyValue(Number(I32(v))) => self.partial_cmp(v),
            _ => None,
        }
    }
}

impl ::core::cmp::PartialEq<Value> for f32 {
    #[inline]
    fn eq(&self, other: &Value) -> bool {
        match other {
            Value::CopyValue(CopyValue::Number(Number::F32(v))) => self.eq(v),
            _ => false,
        }
    }
}

impl ::core::cmp::PartialOrd<Value> for f32 {
    #[inline]
    fn partial_cmp(&self, other: &Value) -> Option<::core::cmp::Ordering> {
        match other {
            Value::CopyValue(CopyValue::Number(Number::F32(v))) => self.partial_cmp(v),
            _ => None,
        }
    }
}

impl ::core::cmp::PartialEq<Value> for f64 {
    #[inline]
    fn eq(&self, other: &Value) -> bool {
        match other {
            Value::CopyValue(CopyValue::Number(Number::F64(v))) => self.eq(v),
            _ => false,
        }
    }
}

impl ::core::cmp::PartialOrd<Value> for f64 {
    #[inline]
    fn partial_cmp(&self, other: &Value) -> Option<::core::cmp::Ordering> {
        match other {
            Value::CopyValue(CopyValue::Number(Number::F64(v))) => self.partial_cmp(v),
            _ => None,
        }
    }
}

impl ::core::cmp::PartialEq<Value> for char {
    #[inline]
    fn eq(&self, other: &Value) -> bool {
        match other {
            Value::CopyValue(CopyValue::Char(v)) => self.eq(v),
            _ => false,
        }
    }
}

impl ::core::cmp::PartialOrd<Value> for char {
    #[inline]
    fn partial_cmp(&self, other: &Value) -> Option<::core::cmp::Ordering> {
        match other {
            Value::CopyValue(CopyValue::Char(v)) => self.partial_cmp(v),
            _ => None,
        }
    }
}

impl ::core::cmp::PartialEq<Value> for bool {
    #[inline]
    fn eq(&self, other: &Value) -> bool {
        match other {
            Value::CopyValue(CopyValue::Bool(v)) => self.eq(v),
            _ => false,
        }
    }
}

impl ::core::cmp::PartialOrd<Value> for bool {
    #[inline]
    fn partial_cmp(&self, other: &Value) -> Option<::core::cmp::Ordering> {
        match other {
            Value::CopyValue(CopyValue::Bool(v)) => self.partial_cmp(v),
            _ => None,
        }
    }
}

impl ::core::cmp::PartialEq<Value> for String {
    #[inline]
    fn eq(&self, other: &Value) -> bool {
        match other {
            Value::String(v) => self.eq(v),
            _ => false,
        }
    }
}

impl ::core::cmp::PartialOrd<Value> for String {
    #[inline]
    fn partial_cmp(&self, other: &Value) -> Option<::core::cmp::Ordering> {
        match other {
            Value::String(v) => self.partial_cmp(v),
            _ => None,
        }
    }
}

partial_eq_cmp! {
    // Value : Value::String => String
    // Value : Value::CopyValue => CopyValue
    // Value : Value::CopyValue::Number => i32
    // Value : Value::CopyValue(CopyValue::Number(Number::I32)) => i32

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

#[derive(Clone, Copy)]
pub enum ValueRef<'a> {
    String(&'a dyn AsRef<str>),
    I32(i32),
}

impl Display for ValueRef<'_> {
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueRef::String(s) => write!(fm, "{}", s.as_ref()),
            ValueRef::I32(i) => write!(fm, "{}", i),
        }
    }
}

impl<'a> From<&'a String> for ValueRef<'a> {
    fn from(s: &'a String) -> Self {
        ValueRef::String(s)
    }
}

impl<'a> From<&'a &str> for ValueRef<'a> {
    fn from(s: &'a &str) -> Self {
        ValueRef::String(s)
    }
}

impl<'a> From<i32> for ValueRef<'a> {
    fn from(i: i32) -> Self {
        ValueRef::I32(i)
    }
}

impl<'a> ::core::cmp::PartialEq<Value> for ValueRef<'a> {
    #[inline]
    fn eq(&self, other: &Value) -> bool {
        match (other, self) {
            (Value::String(vs), ValueRef::String(vrs)) => vs.eq(vrs.as_ref()),
            (Value::CopyValue(CopyValue::Number(Number::I32(vi))), ValueRef::I32(vri)) => {
                vi.eq(vri)
            }
            _ => false,
        }
    }
}

impl<'a> ::core::cmp::PartialOrd<Value> for ValueRef<'a> {
    #[inline]
    fn partial_cmp(&self, other: &Value) -> Option<::core::cmp::Ordering> {
        match (other, self) {
            (Value::String(vs), ValueRef::String(vrs)) => vrs.as_ref().partial_cmp(vs),
            (Value::CopyValue(CopyValue::Number(Number::I32(vi))), ValueRef::I32(vri)) => {
                vri.partial_cmp(vi)
            }
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

        assert!(10 == CopyValue::Number(Number::I32(10)));
        assert!(10 > CopyValue::Number(Number::I32(9)));
        assert!(10 < CopyValue::Number(Number::I32(11)));
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
        assert!(10.2 < CopyValue::Number(Number::F64(11.3)));
    }

    #[test]
    fn cmp_bool() {
        assert!(true == CopyValue::Bool(true));
        assert!(true > false);
        assert!(true > CopyValue::Bool(false));

        assert_eq!(false.to_string(), CopyValue::Bool(false).to_string());
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
        assert!('X' == CopyValue::Char('X'));
        assert!('Y' > 'X');
        assert!('Y' > CopyValue::Char('X'));

        assert_eq!('Z'.to_string(), CopyValue::Char('Z').to_string());
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
