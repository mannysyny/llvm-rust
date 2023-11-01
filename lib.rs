use llvm_sys::prelude::*;
use llvm_sys::core::*;
use llvm_sys::LLVMIntPredicate::*;
use llvm_sys::LLVMRealPredicate::*;
use nom::IResult;
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Module {
    pub name: String,
    pub source_filename: String,
    pub data_layout: String,
    pub target_triple: String,
    pub functions: Vec<Function>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub return_type: Type,
    pub basic_blocks: Vec<BasicBlock>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Argument {
    pub name: String,
    pub argument_type: Type,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct BasicBlock {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Instruction {
    Add {
        dest: String,
        op1: String,
        op2: String,
    },
    Sub {
        dest: String,
        op1: String,
        op2: String,
    },
    Mul {
        dest: String,
        op1: String,
        op2: String,
    },
    Div {
        dest: String,
        op1: String,
        op2: String,
    },
    ICmp {
        dest: String,
        predicate: LLVMIntPredicate,
        op1: String,
        op2: String,
    },
    FCmp {
        dest: String,
        predicate: LLVMRealPredicate,
        op1: String,
        op2: String,
    },
    Ret {
        value: String,
    },
    Br {
        label: String,
    },
    CondBr {
        condition: String,
        true_label: String,
        false_label: String,
    },
    Call {
        dest: String,
        function: String,
        arguments: Vec<String>,
    },
    Load {
        dest: String,
        pointer: String,
    },
    Store {
        value: String,
        pointer: String,
    },
    Alloca {
        dest: String,
        alloca_type: Type,
    },
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Type {
    Void,
    Int(u32),
    Float,
    Double,
    Pointer(Box<Type>),
    Function {
        arguments: Vec<Type>,
        return_type: Box<Type>,
    },
}

pub fn create_module(name: &str, source_filename: &str, data_layout: &str, target_triple: &str) -> LLVMModuleRef {
    unsafe {
        let name = CString::new(name).unwrap();
        let source_filename = CString::new(source_filename).unwrap();
        let data_layout = CString::new(data_layout).unwrap();
        let target_triple = CString::new(target_triple).unwrap();
        LLVMModuleCreateWithNameInContext(name.as_ptr(), LLVMGetGlobalContext())
    }
}

pub fn add_function(module: LLVMModuleRef, name: &str, return_type: LLVMTypeRef) -> LLVMValueRef {
    unsafe {
        let name = CString::new(name).unwrap();
        let function_type = LLVMFunctionType(return_type, std::ptr::null_mut(), 0, 0);
        let function = LLVMAddFunction(module, name.as_ptr(), function_type);
        LLVMSetLinkage(function, LLVMLinkage::LLVMExternalLinkage);
        function
    }
}

pub fn add_argument(function: LLVMValueRef, name: &str, argument_type: LLVMTypeRef) -> LLVMValueRef {
    unsafe {
        let name = CString::new(name).unwrap();
        LLVMAddArgument(function, argument_type, name.as_ptr())
    }
}

pub fn add_basic_block(function: LLVMValueRef, name: &str) -> LLVMBasicBlockRef {
    unsafe {
        let name = CString::new(name).unwrap();
        LLVMAppendBasicBlockInContext(LLVMGetGlobalContext(), function, name.as_ptr())
    }
}

pub fn add_instruction(block: LLVMBasicBlockRef, instruction: LLVMValueRef) {
    unsafe {
        LLVMPositionBuilderAtEnd(LLVMGetBasicBlockParent(block), block);
        LLVMBuildInsertValue(LLVMGetInsertBlock(), instruction, 0, std::ptr::null_mut(), 0, std::ptr::null_mut());
    }
}

pub fn parse_module(input: &str) -> Result<Module, String> {
    match module(input.as_bytes()) {
        Ok((_, module)) => Ok(module),
        Err(e) => Err(format!("Error parsing module: {:?}", e)),
    }
}

named!(module<&[u8], Module>,
    do_parse!(
        tag!("; ModuleID = ") >>
        name: map_res!(take_until!("\n"), std::str::from_utf8) >>
        tag!("\nsource_filename = \"") >>
        source_filename: map_res!(take_until!("\"\n"), std::str::from_utf8) >>
        tag!("\ndata_layout = \"") >>
        data_layout: map_res!(take_until!("\"\n"), std::str::from_utf8) >>
        tag!("\ntarget triple = \"") >>
        target_triple: map_res!(take_until!("\"\n"), std::str::from_utf8) >>
        functions: many1!(function) >>
        (Module {
            name: name.to_string(),
            source_filename: source_filename.to_string(),
            data_layout: data_layout.to_string(),
            target_triple: target_triple.to_string(),
            functions,
        })
    )
);

named!(function<&[u8], Function>,
    do_parse!(
        tag!("\ndefine ") >>
        return_type: type_ >>
        tag!(" @") >>
        name: map_res!(take_until!("("), std::str::from_utf8) >>
        tag!("(") >>
        arguments: separated_list!(tag!(", "), argument) >>
        tag!(") ") >>
        tag!("{\n") >>
        basic_blocks: many1!(basic_block) >>
        tag!("}\n") >>
        (Function {
            name: name.to_string(),
            arguments,
            return_type,
            basic_blocks,
        })
    )
);

named!(argument<&[u8], Argument>,
    do_parse!(
        name: map_res!(take_until!(": "), std::str::from_utf8) >>
        tag!(": ") >>
        argument_type: type_ >>
        (Argument {
            name: name.to_string(),
            argument_type,
        })
    )
);

named!(basic_block<&[u8], BasicBlock>,
    do_parse!(
        tag!("; <label>:\n") >>
        name: map_res!(take_until!(":\n"), std::str::from_utf8) >>
        tag!(":\n") >>
        instructions: many0!(instruction) >>
        (BasicBlock {
            name: name.to_string(),
            instructions,
        })
    )
);

named!(instruction<&[u8], Instruction>,
    alt!(
        add_instruction |
        sub_instruction |
        mul_instruction |
        div_instruction |
        icmp_instruction |
        fcmp_instruction |
        ret_instruction |
        br_instruction |
        cond_br_instruction |
        call_instruction |
        load_instruction |
        store_instruction |
        alloca_instruction
    )
);

named!(add_instruction<&[u8], Instruction>,
    do_parse!(
        tag!("  %") >>
        dest: map_res!(take_until!(" = add "), std::str::from_utf8) >>
        tag!(" = add ") >>
        op1: operand >>
        tag!(", ") >>
        op2: operand >>
        tag!("\n") >>
        (Instruction::Add {
            dest: dest.to_string(),
            op1: op1.to_string(),
            op2: op2.to_string(),
        })
    )
);

named!(sub_instruction<&[u8], Instruction>,
    do_parse!(
        tag!("  %") >>
        dest: map_res!(take_until!(" = sub "), std::str::from_utf8) >>
        tag!(" = sub ") >>
        op1: operand >>
        tag!(", ") >>
        op2: operand >>
        tag!("\n") >>
        (Instruction::Sub {
            dest: dest.to_string(),
            op1: op1.to_string(),
            op2: op2.to_string(),
        })
    )
);

named!(mul_instruction<&[u8], Instruction>,
    do_parse!(
        tag!("  %") >>
        dest: map_res!(take_until!(" = mul "), std::str::from_utf8) >>
        tag!(" = mul ") >>
        op1: operand >>
        tag!(", ") >>
        op2: operand >>
        tag!("\n") >>
        (Instruction::Mul {
            dest: dest.to_string(),
            op1: op1.to_string(),
            op2: op2.to_string(),
        })
    )
);

named!(div_instruction<&[u8], Instruction>,
    do_parse!(
        tag!("  %") >>
        dest: map_res!(take_until!(" = div "), std::str::from_utf8) >>
        tag!(" = div ") >>
        op1: operand >>
        tag!(", ") >>
        op2: operand >>
        tag!("\n") >>
        (Instruction::Div {
            dest: dest.to_string(),
            op1: op1.to_string(),
            op2: op2.to_string(),
        })
    )
);

named!(icmp_instruction<&[u8], Instruction>,
    do_parse!(
        tag!("  %") >>
        dest: map_res!(take_until!(" = icmp "), std::str::from_utf8) >>
        tag!(" = icmp ") >>
        predicate: int_predicate >>
        tag!(" ") >>
        op1: operand >>
        tag!(", ") >>
        op2: operand >>
        tag!("\n") >>
        (Instruction::ICmp {
            dest: dest.to_string(),
            predicate,
            op1: op1.to_string(),
            op2: op2.to_string(),
        })
    )
);

named!(fcmp_instruction<&[u8], Instruction>,
    do_parse!(
        tag!("  %") >>
        dest: map_res!(take_until!(" = fcmp "), std::str::from_utf8) >>
        tag!(" = fcmp ") >>
        predicate: real_predicate >>
        tag!(" ") >>
        op1: operand >>
        tag!(", ") >>
        op2: operand >>
        tag!("\n") >>
        (Instruction::FCmp {
            dest: dest.to_string(),
            predicate,
            op1: op1.to_string(),
            op2: op2.to_string(),
        })
    )
);

named!(ret_instruction<&[u8], Instruction>,
    do_parse!(
        tag!("  ret ") >>
        value: operand >>
        tag!("\n") >>
        (Instruction::Ret {
            value: value.to_string(),
        })
    )
);

named!(br_instruction<&[u8], Instruction>,
    do_parse!(
        tag!("  br label %") >>
        label: map_res!(take_until!("\n"), std::str::from_utf8) >>
        tag!("\n") >>
        (Instruction::Br {
            label: label.to_string(),
        })
    )
);

named!(cond_br_instruction<&[u8], Instruction>,
    do_parse!(
        tag!("  br i1 ") >>
        condition: operand >>
        tag!(", label %") >>
        true_label: map_res!(take_until!(", "), std::str::from_utf8) >>
        tag!(", label %") >>
        false_label: map_res!(take_until!("\n"), std::str::from_utf8) >>
        tag!("\n") >>
        (Instruction::CondBr {
            condition: condition.to_string(),
            true_label: true_label.to_string(),
            false_label: false_label.to_string(),
        })
    )
);

named!(call_instruction<&[u8], Instruction>,
    do_parse!(
        tag!("  %") >>
        dest: map_res!(take_until!(" = call "), std::str::from_utf8) >>
        tag!(" = call ") >>
        function: map_res!(take_until!("("), std::str::from_utf8) >>
        tag!("(") >>
        arguments: separated_list!(tag!(", "), operand) >>
        tag!(")\n") >>
        (Instruction::Call {
            dest: dest.to_string(),
            function: function.to_string(),
            arguments: arguments.iter().map(|s| s.to_string()).collect(),
        })
    )
);

named!(load_instruction<&[u8], Instruction>,
    do_parse!(
        tag!("  %") >>
        dest: map_res!(take_until!(" = load "), std::str::from_utf8) >>
        tag!(" = load ") >>
        pointer: operand >>
        tag!("\n") >>
        (Instruction::Load {
            dest: dest.to_string(),
            pointer: pointer.to_string(),
        })
    )
);

named!(store_instruction<&[u8], Instruction>,
    do_parse!(
        tag!("  store ") >>
        value: operand >>
        tag!(", ") >>
        pointer: operand >>
        tag!("\n") >>
        (Instruction::Store {
            value: value.to_string(),
            pointer: pointer.to_string(),
        })
    )
);

named!(alloca_instruction<&[u8], Instruction>,
    do_parse!(
        tag!("  %") >>
        dest: map_res!(take_until!(" = alloca "), std::str::from_utf8) >>
        tag!(" = alloca ") >>
        alloca_type: type_ >>
        tag!("\n") >>
        (Instruction::Alloca {
            dest: dest.to_string(),
            alloca_type,
        })
    )
);

named!(operand<&[u8], &str>,
    alt!(
        map_res!(tag!("%"), std::str::from_utf8) |
        map_res!(tag!("@"), std::str::from_utf8) |
        map_res!(tag!("float"), std::str::from_utf8) |
        map_res!(tag!("double"), std::str::from_utf8) |
        map_res!(tag!("i1"), std::str::from_utf8) |
        map_res!(tag!("i8"), std::str::from_utf8) |
        map_res!(tag!("i16"), std::str::from_utf8) |
        map_res!(tag!("i32"), std::str::from_utf8) |
        map_res!(tag!("i64"), std::str::from_utf8)
    )
);

named!(type_<&[u8], Type>,
    alt!(
        map!(tag!("void"), |_| Type::Void) |
        map!(tag!("float"), |_| Type::Float) |
        map!(tag!("double"), |_| Type::Double) |
        map!(tag!("i1"), |_| Type::Int(1)) |
        map!(tag!("i8"), |_| Type::Int(8)) |
        map!(tag!("i16"), |_| Type::Int(16)) |
        map!(tag!("i32"), |_| Type::Int(32)) |
        map!(tag!("i64"), |_| Type::Int(64)) |
        map!(
            delimited!(
                tag!("("),
                separated_list!(tag!(", "), type_),
                tag!(")")
            ),
            |arguments| {
                let return_type = Box::new(arguments.last().unwrap().clone());
                let arguments = arguments[..arguments.len() - 1].to_vec();
                Type::Function {
                    arguments,
                    return_type,
                }
            }
        ) |
        map!(
            preceded!(
                tag!("*"),
                type_
            ),
            |ty| Type::Pointer(Box::new(ty))
        )
    )
);

named!(int_predicate<&[u8], LLVMIntPredicate>,
    alt!(
        map!(tag!("eq"), |_| LLVMIntEQ) |
        map!(tag!("ne"), |_| LLVMIntNE) |
        map!(tag!("ugt"), |_| LLVMIntUGT) |
        map!(tag!("uge"), |_| LLVMIntUGE) |
        map!(tag!("ult"), |_| LLVMIntULT) |
        map!(tag!("ule"), |_| LLVMIntULE) |
        map!(tag!("sgt"), |_| LLVMIntSGT) |
        map!(tag!("sge"), |_| LLVMIntSGE) |
        map!(tag!("slt"), |_| LLVMIntSLT) |
        map!(tag!("sle"), |_| LLVMIntSLE)
    )
);

named!(real_predicate<&[u8], LLVMRealPredicate>,
    alt!(
        map!(tag!("oeq"), |_| LLVMRealOEQ) |
        map!(tag!("ogt"), |_| LLVMRealOGT) |
        map!(tag!("oge"), |_| LLVMRealOGE) |
        map!(tag!("olt"), |_| LLVMRealOLT) |
        map!(tag!("ole"), |_| LLVMRealOLE) |
        map!(tag!("one"), |_| LLVMRealONE) |
        map!(tag!("ord"), |_| LLVMRealORD) |
        map!(tag!("ueq"), |_| LLVMRealUEQ) |
        map!(tag!("ugt"), |_| LLVMRealUGT) |
        map!(tag!("uge"), |_| LLVMRealUGE) |
        map!(tag!("ult"), |_| LLVMRealULT) |
        map!(tag!("ule"), |_| LLVMRealULE) |
        map!(tag!("une"), |_| LLVMRealUNE) |
        map!(tag!("uno"), |_| LLVMRealUNO)
    )
);
named!(add_instruction<&[u8], Instruction>,
    do_parse!(
        tag!("  %") >>
        dest: map_res!(take_until!(" = add "), std::str::from_utf8) >>
        tag!(" = add ") >>
        ty: type_ >>
        tag!(" ") >>
        op1: operand >>
        tag!(", ") >>
        op2: operand >>
        tag!("\n") >>
        (Instruction::Add {
            dest: dest.to_string(),
            ty,
            op1: op1.to_string(),
            op2: op2.to_string(),
        })
    )
);
