#*
    String Manipulation Utilities
*#

// NOTE: String is a primitive type. In core, it is pretty much treated as UTF-8 for the most part
export(prelude) String: {}

export AsciiString: Vec[Byte]

// Force reference so you only define it once
export StaticString: &str

// UTF-8 or 16
export UnicodeString: Utf8String | Utf16String
