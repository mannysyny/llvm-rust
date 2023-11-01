use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, line_ending, space0},
    combinator::{map_res, opt},
    sequence::{delimited, preceded},
    IResult,
};

fn llvm_nl(input: &str) -> IResult<&str, &str> {
    line_ending(input)
}

fn llvm_ws(input: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c.is_whitespace())(input)
}

#[derive(Debug)]
pub enum Operand {
    Constant(String),
    Label(String),
}

#[derive(Debug)]
pub struct Instruction {
    pub name: String,
    pub operands: Vec<Operand>,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Module {
    pub functions: Vec<Function>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
        }
    }
}

fn parse_u64(input: &str) -> IResult<&str, u64> {
    map_res(
        take_while1(|c: char| c.is_digit(10)),
        |s: &str| s.parse::<u64>(),
    )(input)
}

fn type_name(input: &str) -> IResult<&str, &str> {
    preceded(char('%'), take_while1(|c: char| c.is_alphanumeric() || c == '.'))(input)
}

fn function_name(input: &str) -> IResult<&str, &str> {
    preceded(char('@'), take_while1(|c: char| c.is_alphanumeric() || c == '.'))(input)
}

fn label_name(input: &str) -> IResult<&str, &str> {
    preceded(char('%'), take_while1(|c: char| c.is_alphanumeric() || c == '.' || c == '_'))(input)
}

fn constant(input: &str) -> IResult<&str, &str> {
    alt((global_name, local_name))(input)
}

fn operand(input: &str) -> IResult<&str, &str> {
    alt((constant, label_name))(input)
}

fn instruction(input: &str) -> IResult<&str, (&str, Vec<&str>)> {
    let (input, name) = take_while1(|c: char| c.is_alphabetic())(input)?;
    let (input, operands) = space_separated(operand)(input)?;
    Ok((input, (name, operands)))
}

fn address_space(input: &str) -> IResult<&str, u64> {
    preceded(tag("addrspace("), parse_u64)(input)
}

fn local_name(input: &str) -> IResult<&str, &str> {
    preceded(char('%'), take_while1(|c: char| c.is_alphanumeric() || c == '_'))(input)
}

fn global_name(input: &str) -> IResult<&str, &str> {
    preceded(char('@'), take_while1(|c: char| c.is_alphanumeric() || c == '_'))(input)
}

fn token<'a, T>(parser: impl Fn(&'a str) -> IResult<&'a str, T>) -> impl Fn(&'a str) -> IResult<&'a str, T> {
    preceded(space0, parser)
}

fn comma_separated<'a, T>(parser: impl Fn(&'a str) -> IResult<&'a str, T>) -> impl Fn(&'a str) -> IResult<&'a str, Vec<T>> {
    let sep = delimited(space0, char(','), space0);
    let parser = preceded(space0, parser);
    separated_list0(sep, parser)
}

fn space_separated<'a, T>(parser: impl Fn(&'a str) -> IResult<&'a str, T>) -> impl Fn(&'a str) -> IResult<&'a str, Vec<T>> {
    separated_list0(space0, parser)
}

fn newline_separated<'a, T>(parser: impl Fn(&'a str) -> IResult<&'a str, T>) -> impl Fn(&'a str) -> IResult<&'a str, Vec<T>> {
    separated_list0(llvm_nl_, parser)
}

fn semicolon_separated<'a, T>(parser: impl Fn(&'a str) -> IResult<&'a str, T>) -> impl Fn(&'a str) -> IResult<&'a str, Vec<T>> {
    let sep = delimited(space0, char(';'), space0);
    let parser = preceded(space0, parser);
    separated_list0(sep, parser)
}

fn separated_list0<'a, T, U>(
    sep: impl Fn(&'a str) -> IResult<&'a str, U>,
    parser: impl Fn(&'a str) -> IResult<&'a str, T>,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<T>> {
    let separated_pair = separated_pair(parser, sep, parser);
    let parser = alt((separated_pair, parser));
    let (input, first) = parser(input)?;
    let (input, rest) = many0(preceded(sep, parser))(input)?;
    Ok((input, std::iter::once(first).chain(rest.into_iter()).collect()))
}
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    use nom::{
        branch::alt,
        bytes::complete::{tag, take_while1},
        character::complete::{char, line_ending, space0, take_while},
        combinator::{map_res, opt},
        multi::{many0, separated_list},
        sequence::{delimited, preceded, separated_pair},
        IResult,
    };

    fn llvm_nl(input: &str) -> IResult<&str, &str> {
        line_ending(input)
    }

    fn llvm_ws(input: &str) -> IResult<&str, &str> {
        take_while1(|c: char| c.is_whitespace())(input)
    }

    #[derive(Debug)]
    pub enum Operand {
        Constant(Constant),
        Label(String),
    }

    #[derive(Debug)]
    pub enum Constant {
        Int(u64),
        Float(f64),
        Array(Vec<Constant>),
        Pointer(Box<Constant>),
    }

    #[derive(Debug)]
    pub struct Instruction {
        pub name: String,
        pub operands: Vec<Operand>,
    }

    #[derive(Debug)]
    pub struct Function {
        pub name: String,
        pub instructions: Vec<Instruction>,
    }

    #[derive(Debug)]
    pub struct Module {
        pub functions: Vec<Function>,
    }

    impl Module {
        pub fn new() -> Self {
            Self {
                functions: Vec::new(),
            }
        }
    }

    fn parse_u64(input: &str) -> IResult<&str, u64> {
        map_res(
            take_while1(|c: char| c.is_digit(10)),
            |s: &str| s.parse::<u64>(),
        )(input)
    }

    fn parse_f64(input: &str) -> IResult<&str, f64> {
        map_res(
            alt((
                take_while1(|c: char| c.is_digit(10)),
                delimited(char('-'), take_while1(|c: char| c.is_digit(10)), char('.')),
                delimited(take_while1(|c: char| c.is_digit(10)), char('.'), take_while1(|c: char| c.is_digit(10))),
            )),
            |s: &str| s.parse::<f64>(),
        )(input)
    }

    fn parse_constant(input: &str) -> IResult<&str, Constant> {
        alt((parse_int, parse_float, parse_array, parse_pointer))(input)
    }

    fn parse_int(input: &str) -> IResult<&str, Constant> {
        map_res(parse_u64, |i| Ok(Constant::Int(i)))(input)
    }

    fn parse_float(input: &str) -> IResult<&str, Constant> {
        map_res(parse_f64, |f| Ok(Constant::Float(f)))(input)
    }

    fn parse_array(input: &str) -> IResult<&str, Constant> {
        let (input, _) = tag("[")(input)?;
        let (input, constants) = separated_list(tag(","), parse_constant)(input)?;
        let (input, _) = tag("]")(input)?;
        Ok((input, Constant::Array(constants)))
    }

    fn parse_pointer(input: &str) -> IResult<&str, Constant> {
        let (input, _) = tag("ptr")(input)?;
        let (input, _) = llvm_ws(input)?;
        let (input, constant) = parse_constant(input)?;
        Ok((input, Constant::Pointer(Box::new(constant))))
    }

    fn type_name(input: &str) -> IResult<&str, &str> {
        preceded(char('%'), take_while1(|c: char| c.is_alphanumeric() || c == '.'))(input)
    }

    fn function_name(input: &str) -> IResult<&str, &str> {
        preceded(char('@'), take_while1(|c: char| c.is_alphanumeric() || c == '.'))(input)
    }

    fn label_name(input: &str) -> IResult<&str, &str> {
        preceded(char('%'), take_while1(|c: char| c.is_alphanumeric() || c == '.' || c == '_'))(input)
    }

    fn global_name(input: &str) -> IResult<&str, &str> {
        preceded(char('@'), take_while1(|c: char| c.is_alphanumeric() || c == '.'))(input)
    }

    fn local_name(input: &str) -> IResult<&str, &str> {
        preceded(char('%'), take_while1(|c: char| c.is_alphanumeric() || c == '.' || c == '_'))(input)
    }

    fn constant(input: &str) -> IResult<&str, &str> {
        alt((global_name, local_name))(input)
    }

    fn operand(input: &str) -> IResult<&str, Operand> {
        alt((constant_operand, label_operand))(input)
    }

    fn constant_operand(input: &str) -> IResult<&str, Operand> {
        let (input, constant) = parse_constant(input)?;
        Ok((input, Operand::Constant(constant)))
    }

    fn label_operand(input: &str) -> IResult<&str, Operand> {
        let (input, label) = label_name(input)?;
        Ok((input, Operand::Label(label.to_owned())))
    }

    fn instruction(input: &str) -> IResult<&str, Instruction> {
        let (input, name) = take_while1(|c: char| c.is_alphabetic())(input)?;
        let (input, operands) = space_separated(operand)(input)?;
        Ok((input, Instruction { name: name.to_owned(), operands }))
    }

    fn address_space(input: &str) -> IResult<&str, u64> {
        preceded(tag("addrspace("), parse_u64)(input)
    }

    fn token<'a, T>(parser: impl Fn(&'a str) -> IResult<&'a str, T>) -> impl Fn(&'a str) -> IResult<&'a str, T> {
        preceded(space0, parser)
    }

    fn comma_separated<'a, T>(parser: impl Fn(&'a str) -> IResult<&'a str, T>) -> impl Fn(&'a str) -> IResult<&'a str, Vec<T>> {
        let sep = delimited(space0, char(','), space0);
        let parser = preceded(space0, parser);
        separated_list0(sep, parser)
    }

    fn space_separated<'a, T>(f: impl Fn(&'a str) -> IResult<&'a str, T>) -> impl Fn(&'a str) -> IResult<&'a str, Vec<T>> {
        separated_list(llvm_ws, f)
    }

    fn newline_separated<'a, T>(parser: impl Fn(&'a str) -> IResult<&'a str, T>) -> impl Fn(&'a str) -> IResult<&'a str, Vec<T>> {
        separated_list0(llvm_nl_, parser)
    }

    fn semicolon_separated<'a, T>(parser: impl Fn(&'a str) -> IResult<&'a str, T>) -> impl Fn(&'a str) -> IResult<&'a str, Vec<T>> {
        let sep = delimited(space0, char(';'), space0);
        let parser = preceded(space0, parser);
        separated_list0(sep, parser)
    }

    fn separated_list0<'a, T, U>(
        sep: impl Fn(&'a str) -> IResult<&'a str, U>,
        parser: impl Fn(&'a str) -> IResult<&'a str, T>,
    ) -> impl Fn(&'a str) -> IResult<&'a str, Vec<T>> {
        let separated_pair = separated_pair(parser, sep, parser);
        let parser = alt((separated_pair, parser));
        let (input, first) = parser(input)?;
        let (input, rest) = many0(preceded(sep, parser))(input)?;
        Ok((input, std::iter::once(first).chain(rest.into_iter()).collect()))
    }
