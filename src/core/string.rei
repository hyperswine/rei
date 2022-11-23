#*
    String Manipulation Utilities
*#

// NOTE: base::types::String is a primitive type. In core, it is pretty much treated as UTF-8 for the most part

use core::types::ReiType

# Dynamically Allocated UTF8 String
export(prelude) String: {
    v: Vec[ReiType::Char]
}

# Dynamically allocated String
export(prelude) String[CharT]: {
    v: Vec[CharT]
}

# Statically allocated String
export(prelude) String[CharT, Size]: {
    v: [CharT; Size]
}

export AsciiString: Vec[Byte]

// Force reference so you only define it once
export StaticString: &str

// UTF-8 or 16
export UnicodeString: Utf8String | Utf16String
