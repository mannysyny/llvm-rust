use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, digit1},
    combinator::{map, opt},
    multi::{many0, many1},
    sequence::{delimited, preceded, separated_pair, terminated},
    IResult,
};

#[derive(Debug, PartialEq)]
pub enum Type<'a> {
    Void,
    Int(u32),
    Float,
    Double,
    Pointer(Box<Type<'a>>),
    Array(Box<Type<'a>>, u32),
    Struct(Vec<Type<'a>>),
    Function(Vec<Type<'a>>, Box<Type<'a>>),
    Named(&'a str),
    Metadata,
}

fn parse_int(input: &str) -> IResult<&str, u32> ;{
    map(digit1, |s: &str| s.parse().unwrap())(input)
}

fn parse_float(input: &str) -> IResult<&str, Type> {
    map(tag("float"), |_| Type::Float)(input)
}

fn parse_double(input: &str) -> IResult<&str, Type> {
    map(tag("double"), |_| Type::Double)(input)
}

fn parse_named(input: &str) -> IResult<&str, &str> {
    preceded(char('%'), take_while1(|c| c != ' ' && c != ','))(input)
}

fn parse_type_void(input: &str) -> IResult<&str, Type> {
    map(tag("void"), |_| Type::Void)(input)
}

fn parse_type_int(input: &str) -> IResult<&str, Type> {
    map(preceded(tag("i"), parse_int), Type::Int)(input)
}

fn parse_type_pointer(input: &str) -> IResult<&str, Type> {
    map(
        preceded(
            char('*'),
            delimited(
                char(' '),
                parse_type,
                opt(preceded(char(','), parse_int)),
            ),
        ),
        |t| match t {
            Type::Array(t, n) => Type::Pointer(Box::new(Type::Array(t, n))),
            t => Type::Pointer(Box::new(t)),
        },
    )(input)
}

fn parse_type_array(input: &str) -> IResult<&str, Type> {
    map(
        delimited(
            char('['),
            separated_pair(parse_type, char(' '), parse_int),
            char(']'),
        ),
        |(t, n)| Type::Array(Box::new(t), n),
    )(input)
}

fn parse_type_struct(input: &str) -> IResult<&str, Type> {
    map(
        delimited(
            char('{'),
            many0(preceded(char(' '), parse_type)),
            char('}'),
        ),
        Type::Struct,
    )(input)
}

fn parse_type_function(input: &str) -> IResult<&str, Type> {
    map(
        delimited(
            char('('),
            separated_pair(
                many0(preceded(char(' '), parse_type)),
                char(','),
                preceded(char(' '), tag("...)")),
            char(')'),
        ),
        |(args, ret)| Type::Function(args, Box::new(ret)),
    )(input)
}

fn parse_type(input: &str) -> IResult<&str, Type> {
    alt((
        parse_type_void,
        parse_type_int,
        parse_float,
        parse_double,
        parse_type_pointer,
        parse_type_array,
        parse_type_struct,
        parse_type_function,
        map(parse_named, Type::Named),
    ))(input)
}
    )(input)
}

fn parse_type_named(input: &str) -> IResult<&str, Type> {
    map(parse_named, Type::Named)(input)
}

fn parse_type_metadata(input: &str) -> IResult<&str, Type> {
    map(tag("metadata"), |_| Type::Metadata)(input)
}

fn parse_type(input: &str) -> IResult<&str, Type> {
    alt((
        parse_type_void,
        parse_type_int,
        parse_type_float,
        parse_type_double,
        parse_type_pointer,
        parse_type_array,
        parse_type_struct,
        parse_type_function,
        parse_type_named,
        parse_type_metadata,
    ))(input)
}

fn ptr(t: Type) -> Type {
    Type::Pointer(Box::new(t))
}

fn array(t: Type, n: u32) -> Type {
    Type::Array(Box::new(t), n)
}

fn arg_list(input: &str) -> IResult<&str, Vec<Type>> {
    terminated(many0(preceded(char(' '), parse_type)), opt(char(',')))(input)
}

fn trailing_type(input: &str) -> IResult<&str, Type> {
    preceded(char(' '), parse_type)(input)
}

fn test_types() {
    assert_eq!(parse_type("void"), Ok(("", Type::Void)));
    assert_eq!(parse_type("i32"), Ok(("", Type::Int(32))));
    assert_eq!(parse_type("float"), Ok(("", Type::Float)));
    assert_eq!(parse_type("double"), Ok(("", Type::Double)));
    assert_eq!(
        parse_type("i32*"),
        Ok(("", Type::Pointer(Box::new(Type::Int(32)))))
    );
    assert_eq!(
        parse_type("i32*, i32"),
        Ok((
            "",
            Type::Pointer(Box::new(Type::Int(32))),
        ))
    );
    assert_eq!(
        parse_type("[10 x i32]"),
        Ok(("", Type::Array(Box::new(Type::Int(32)), 10)))
    );
    assert_eq!(
        parse_type("{ i32, i32 }"),
        Ok((
            "",
            Type::Struct(vec![Type::Int(32), Type::Int(32)]),
        ))
    );
    assert_eq!(
        parse_type("(i32, ...) -> i32"),
        Ok((
            "",
            Type::Function(
                vec![Type::Int(32)],
                Box::new(Type::Named("i32")),
            ),
        ))
    );
    assert_eq!(parse_type("%foo"), Ok(("", Type::Named("foo"))));
    assert_eq!(parse_type("metadata"), Ok(("", Type::Metadata)));
}

fn main() {
    test_types();
}
