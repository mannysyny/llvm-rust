use std::str::FromStr;
use std::num::ParseIntError;

impl FromStr for DataLayout {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split('-');

        let endianness = match parts.next() {
            Some("e") => Endianness::Little,
            Some("E") => Endianness::Big,
            Some(other) => return Err(format!("Invalid endianness: {}", other)),
            None => return Err("Missing endianness".to_string()), 
        };

        let stack_alignment = match parts.next().and_then(|s| s.parse().ok()) {
            Some(n) => n,
            None => return Err("Missing or invalid stack alignment".to_string()),
        };

        let heap_alignment = match parts.next().and_then(|s| s.parse().ok()) {
            Some(n) => n,
            None => return Err("Missing or invalid heap alignment".to_string()),
        };
        
        let address_space = match parts.next().and_then(|s| s.parse().ok()) {
            Some(n) => n,
            None => return Err("Missing or invalid address space".to_string()),
        };

        let pointer_alignment = match parts.next().and_then(|s| s.parse().ok()) {
            Some(n) => n,
            None => return Err("Missing or invalid pointer alignment".to_string()),
        };

        let integer_alignment = match parts.next().and_then(|s| s.parse().ok()) {
            Some(n) => n,
            None => return Err("Missing or invalid integer alignment".to_string()),
        };

        let vector_alignment = match parts.next().and_then(|s| s.parse().ok()) {
            Some(n) => n,
            None => return Err("Missing or invalid vector alignment".to_string()),
        };

        let float_alignment = match parts.next().and_then(|s| s.parse().ok()) {
            Some(n) => n,
            None => return Err("Missing or invalid float alignment".to_string()),
        };

        let object_alignment = match parts.next().and_then(|s| s.parse().ok()) {
            Some(n) => n,
            None => return Err("Missing or invalid object alignment".to_string()),
        };

        let native_integers = match parts.next() {
            Some(s) => s.split(':').map(|s| s.parse().unwrap_or(0)).collect(),
            None => return Err("Missing native integers".to_string()),
        };

        let non_integral_address_spaces = match parts.next() {
            Some(s) => s.split(':').map(|s| s.parse().unwrap_or(0)).collect(),
            None => return Err("Missing non-integral address spaces".to_string()),
        };

        let mangling = match parts.next() {
            Some("e") => Some(Mangling::ELF),
            Some("m") => Some(Mangling::MachO),
            Some("w") => Some(Mangling::WindowsCOFF),
            Some("x") => Some(Mangling::WindowsCOFFX86),
            Some("y") => Some(Mangling::WindowsCOFFX64),
            Some(other) => return Err(format!("Invalid mangling: {}", other)),
            None => None,
        };

        Ok(Self::new(
            endianness,
            stack_alignment,
            address_space,
            pointer_alignment,
            integer_alignment,
            vector_alignment,
            float_alignment,
            object_alignment,
            native_integers,
            non_integral_address_spaces,
            mangling,
        ))
    }
}

#[derive(Debug, PartialEq)]
pub struct DataLayout {
    endianness: Endianness,
    stack_alignment: u64,
    address_space: u64,
    pointer_alignment: u64,
    integer_alignment: u64,
    vector_alignment: u64,
    float_alignment: u64,
    object_alignment: u64,
    native_integers: Vec<u64>,
    non_integral_address_spaces: Vec<u64>,
    mangling: Option<Mangling>,
}

#[derive(Debug, PartialEq)]
pub enum Endianness {
    Little,
    Big,
}

#[derive(Debug, PartialEq)]
pub enum Mangling {
    ELF,
    MachO,
    WindowsCOFF,
    WindowsCOFFX86,
    WindowsCOFFX64,
}

impl DataLayout {
    pub fn new(
        endianness: Endianness,
        stack_alignment: u64,
        address_space: u64,
        pointer_alignment: u64,
        integer_alignment: u64,
        vector_alignment: u64,
        float_alignment: u64,
        object_alignment: u64,
        native_integers: Vec<u64>,
        non_integral_address_spaces: Vec<u64>,
        mangling: Option<Mangling>,
    ) -> Self {
        Self {
            endianness,
            stack_alignment,
            address_space,
            pointer_alignment,
            integer_alignment,
            vector_alignment,
            float_alignment,
            object_alignment,
            native_integers,
            non_integral_address_spaces,
            mangling,
        }
    }

    pub fn endianness(&self) -> Endianness {
        self.endianness
    }

    pub fn stack_alignment(&self) -> u64 {
        self.stack_alignment
    }

    pub fn address_space(&self) -> u64 {
        self.address_space
    }

    pub fn pointer_alignment(&self) -> u64 {
        self.pointer_alignment
    }

    pub fn integer_alignment(&self) -> u64 {
        self.integer_alignment
    }

    pub fn vector_alignment(&self) -> u64 {
        self.vector_alignment
    }

    pub fn float_alignment(&self) -> u64 {
        self.float_alignment
    }

    pub fn object_alignment(&self) -> u64 {
        self.object_alignment
    }

    pub fn native_integers(&self) -> &[u64] {
        &self.native_integers
    }

    pub fn non_integral_address_spaces(&self) -> &[u64] {
        &self.non_integral_address_spaces
    }

    pub fn mangling(&self) -> Option<Mangling> {
        self.mangling
    }

    pub fn type_size_in_bits(&self, ty: &str) -> Option<u64> {
        pub fn type_size_in_bits(&self, ty: &str) -> Option<u64> {
            let mut parts = ty.split(':');
            let ty_name = parts.next().unwrap();
            let ty_size = parts.next().and_then(|s| s.parse().ok());

            match ty_name {
                "i1" => Some(1),
                "i8" => Some(8),
                "i16" => Some(16),
                "i32" => Some(32),
                "i64" => Some(64),
                "f16" => Some(16),
                "f32" => Some(32),
                "f64" => Some(64),
                _ => ty_size,
            }
        }
        unimplemented!()
    }
}

impl FromStr for DataLayout {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut endianness = Endianness::Little;
        let mut stack_alignment = 0;
        let mut address_space = 0;
        let mut pointer_alignment = 0;
        let mut integer_alignment = 0;
        let mut vector_alignment = 0;
        let mut float_alignment = 0;
        let mut object_alignment = 0;
        let mut native_integers = Vec::new();
        let mut non_integral_address_spaces = Vec::new();
        let mut mangling = None;

        for part in s.split('-') {
            let mut parts = part.split(':');
            match parts.next().unwrap() {
                "e" => {
                    endianness = match parts.next().unwrap() {
                        "m" => Endianness::Big,
                        "M" => Endianness::Little,
                        _ => return Err(format!("Invalid endianness: {}", part)),
                    };
                }
                "p" => {
                    stack_alignment = parts.next().unwrap().parse().unwrap();
                    address_space = parts.next().unwrap().parse().unwrap();
                }
                "i" => {
                    pointer_alignment = parts.next().unwrap().parse().unwrap();
                    integer_alignment = parts.next().unwrap().parse().unwrap();
                }
                "v" => {
                    vector_alignment = parts.next().unwrap().parse().unwrap();
                }
                "f" => {
                    float_alignment = parts.next().unwrap().parse().unwrap();
                }
                "o" => {
                    object_alignment = parts.next().unwrap().parse().unwrap();
                }
                "n" => {
                    native_integers = parts
                        .next()
                        .unwrap()
                        .split(':')
                        .map(|s| s.parse().unwrap())
                        .collect();
                }
                "S" => {
                    non_integral_address_spaces = parts
                        .next()
                        .unwrap()
                        .split(':')
                        .map(|s| s.parse().unwrap())
                        .collect();
                }
                "m" => {
                    mangling = match parts.next().unwrap() {
                        "e" => Some(Mangling::ELF),
                        "o" => Some(Mangling::MachO),
                        "w" => match parts.next().unwrap() {
                            "c" => Some(Mangling::WindowsCOFF),
                            "x" => match parts.next().unwrap() {
                                "86" => Some(Mangling::WindowsCOFFX86),
                                "64" => Some(Mangling::WindowsCOFFX64),
                                _ => return Err(format!("Invalid mangling: {}", part)),
                            },
                            _ => return Err(format!("Invalid mangling: {}", part)),
                        },
                        _ => return Err(format!("Invalid mangling: {}", part)),
                    };
                }
                _ => return Err(format!("Invalid part: {}", part)),
            }
        }

        Ok(DataLayout::new(
            endianness,
            stack_alignment,
            address_space,
            pointer_alignment,
            integer_alignment,
            vector_alignment,
            float_alignment,
            object_alignment,
            native_integers,
            non_integral_address_spaces,
            mangling,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_datalayout() {
        let expected = DataLayout::new(
            Endianness::Little,
            64,
            0,
            64,
            64,
            128,
            64,
            64,
            vec![8, 16, 32, 64],
            vec![1, 2, 3],
            Some(Mangling::ELF),
        );
        let input = "e-m:e-p:64:64-i64:64-f64:64-o64:64-n8:16:32:64-S128";
        let result = input.parse::<DataLayout>().unwrap();
        assert_eq!(result, expected);
    }
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum Endianness {
    Little,
    Big,
}

#[derive(Debug, PartialEq)]
pub enum Mangling {
    ELF,
    MachO,
    WindowsCOFF,
    WindowsCOFFX86,
    WindowsCOFFX64,
}

#[derive(Debug, PartialEq)]
pub struct DataLayout {
    pub endianness: Endianness,
    pub stack_alignment: u64,
    pub address_space: u64,
    pub pointer_alignment: u64,
    pub integer_alignment: u64,
    pub vector_alignment: u64,
    pub float_alignment: u64,
    pub object_alignment: u64,
    pub native_integers: Vec<u64>,
    pub non_integral_address_spaces: Vec<u64>,
    pub mangling: Option<Mangling>,
}

impl DataLayout {
    pub fn new(
        endianness: Endianness,
        stack_alignment: u64,
        address_space: u64,
        pointer_alignment: u64,
        integer_alignment: u64,
        vector_alignment: u64,
        float_alignment: u64,
        object_alignment: u64,
        native_integers: Vec<u64>,
        non_integral_address_spaces: Vec<u64>,
        mangling: Option<Mangling>,
    ) -> Self {
        DataLayout {
            endianness,
            stack_alignment,
            address_space,
            pointer_alignment,
            integer_alignment,
            vector_alignment,
            float_alignment,
            object_alignment,
            native_integers,
            non_integral_address_spaces,
            mangling,
        }
    }
}

impl std::str::FromStr for DataLayout {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut endianness = Endianness::Little;
        let mut stack_alignment = 0;
        let mut address_space = 0;
        let mut pointer_alignment = 0;
        let mut integer_alignment = 0;
        let mut vector_alignment = 0;
        let mut float_alignment = 0;
        let mut object_alignment = 0;
        let mut native_integers = Vec::new();
        let mut non_integral_address_spaces = Vec::new();
        let mut mangling = None;

        // User-defined parsing rules
        let mut rules: HashMap<&str, Box<dyn Fn(&mut DataLayout, &str) -> Result<(), String>>> =
            HashMap::new();
        rules.insert(
            "e",
            Box::new(|dl, part| {
                dl.endianness = match part {
                    "B" => Endianness::Big,
                    "M" => Endianness::Little,
                    _ => return Err(format!("Invalid endianness: {}", part)),
                };
                Ok(())
            }),
        );
        rules.insert(
            "p",
            Box::new(|dl, part| {
                dl.stack_alignment = part
                    .split(':')
                    .next()
                    .unwrap_or_default()
                    .parse()
                    .unwrap_or_default();
                dl.address_space = part
                    .split(':')
                    .skip(1)
                    .next()
                    .unwrap_or_default()
                    .parse()
                    .unwrap_or_default();
                Ok(())
            }),
        );
        rules.insert(
            "i",
            Box::new(|dl, part| {
                dl.pointer_alignment = part
                    .split(':')
                    .next()
                    .unwrap_or_default()
                    .parse()
                    .unwrap_or_default();
                dl.integer_alignment = part
                    .split(':')
                    .skip(1)
                    .next()
                    .unwrap_or_default()
                    .parse()
                    .unwrap_or_default();
                Ok(())
            }),
        );
        rules.insert(
            "v",
            Box::new(|dl, part| {
                dl.vector_alignment = part
                    .split(':')
                    .next()
                    .unwrap_or_default()
                    .parse()
                    .unwrap_or_default();
                Ok(())
            }),
        );
        rules.insert(
            "f",
            Box::new(|dl, part| {
                dl.float_alignment = part
                    .split(':')
                    .next()
                    .unwrap_or_default()
                    .parse()
                    .unwrap_or_default();
                Ok(())
            }),
        );
        rules.insert(
            "o",
            Box::new(|dl, part| {
                dl.object_alignment = part
                    .split(':')
                    .next()
                    .unwrap_or_default()
                    .parse()
                    .unwrap_or_default();
                Ok(())
            }),
        );
        rules.insert(
            "n",
            Box::new(|dl, part| {
                dl.native_integers = part
                    .split(':')
                    .map(|s| s.parse().unwrap_or_default())
                    .collect();
                Ok(())
            }),
        );
        rules.insert(
            "S",
            Box::new(|dl, part| {
                dl.non_integral_address_spaces = part
                    .split(':')
                    .map(|s| s.parse().unwrap_or_default())
                    .collect();
                Ok(())
            }),
        );
        rules.insert(
            "m",
            Box::new(|dl, part| {
                dl.mangling = match part.split(':').next().unwrap_or_default() {
                    "e" => Some(Mangling::ELF),
                    "o" => Some(Mangling::MachO),
                    "w" => match part.split(':').skip(1).next().unwrap_or_default() {
                        "c" => Some(Mangling::WindowsCOFF),
                        "x" => match part.split(':').skip(2).next().unwrap_or_default() {
                            "86" => Some(Mangling::WindowsCOFFX86),
                            "64" => Some(Mangling::WindowsCOFFX64),
                            _ => return Err(format!("Invalid mangling: {}", part)),
                        },
                        _ => return Err(format!("Invalid mangling: {}", part)),
                    },
                    _ => return Err(format!("Invalid mangling: {}", part)),
                };
                Ok(())
            }),
        );

        // Parsing options
        let strict_mode = true; // Enable strict parsing mode by default
        let allow_unknown_tokens = false; // Disallow unknown tokens by default

        for part in s.split('-') {
            if part.is_empty() {
                continue;
            }

            let prefix = part.chars().next().unwrap();
            let suffix = &part[1..];

            if let Some(rule) = rules.get(&prefix.to_string()) {
                if let Err(err) = rule(&mut DataLayout {
                    endianness,
                    stack_alignment,
                    address_space,
                    pointer_alignment,
                    integer_alignment,
                    vector_alignment,
                    float_alignment,
                    object_alignment,
                    native_integers,
                    non_integral_address_spaces,
                    mangling,
                }, suffix)
                {
                    return Err(err);
                }
            } else if !allow_unknown_tokens {
                return Err(format!("Unknown token: {}", part));
            }
        }

        if strict_mode {
            // Ensure all required fields are present
            if stack_alignment == 0 {
                return Err("Missing stack alignment".to_string());
            }
            if address_space == 0 {
                return Err("Missing address space".to_string());
            }
            if pointer_alignment == 0 {
                return Err("Missing pointer alignment".to_string());
            }
            if integer_alignment == 0 {
                return Err("Missing integer alignment".to_string());
            }
            if vector_alignment == 0 {
                return Err("Missing vector alignment".to_string());
            }
            if float_alignment == 0 {
                return Err("Missing float alignment".to_string());
            }
            if object_alignment == 0 {
                return Err("Missing object alignment".to_string());
            }
        }

        Ok(DataLayout::new(
            endianness,
            stack_alignment,
            address_space,
            pointer_alignment,
            integer_alignment,
            vector_alignment,
            float_alignment,
            object_alignment,
            native_integers,
            non_integral_address_spaces,
            mangling,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_datalayout() {
        let expected = DataLayout::new(
            Endianness::Little,
            64,
            0,
            64,
            64,
            128,
            64,
            64,
            vec![8, 16, 32, 64],
            vec![1, 2, 3],
            Some(Mangling::ELF),
        );
        let input = "e-m:e-p:64:64-i64:64-f64:64-o64:64-n8:16:32:64-S128";
        let result = input.parse::<DataLayout>().unwrap();
        assert_eq!(result, expected);
    }
}
