#Conversions Between Types/Variables in Haskell vs. C++

##Understanding Return Types in Haskell vs. C++
I found `:t` fascinating when learning Haskell. This command explains how to use a function by showing what types it uses.  For example, `show` converts types into `String`:

    ghci> :t show
    show :: Show a => a -> String

There are many different ways to represent string-like things.  In Haskell, converting between a `String`, `ByteString`, and `Text` is important because each function asks for a different type. For example, `(++) :: [a] -> [a] -> [a]` asks for `[a]`. `[a]` is a list with an ambiguous type; it can be an `Integer` or `Char`.

    ghci> (++) 2 3

    <interactive>:2:6:
        No instance for (Num [a0]) arising from the literal `2'
        Possible fix: add an instance declaration for (Num [a0])
        In the first argument of `(++)', namely `2'
        In the expression: (++) 2 3
        In an equation for `it': it = (++) 2 3

    ghci> (++) ['a'] ['b']
    "ab"

(For C++ users, it is as if you are converting between a Linked List of `char`, ASCII `string` or `char*`, and unicode `string` respectively.)

For C++, unicode `string` may store characters that have a different encoding than ASCII. This is similar to Haskell’s `Text`.
`ByteString` in Haskell is stored like a `char*` in C++ because the characters are stored adjacently.
C++ does not have a variable type specifically for unicode, but there are types that can store unicode characters such as `wstring` and `utf8`.

---
###`ByteString` to `String` Conversions
First, we will look at the Haskell’s `ByteString` conversions and compare them to C++ `string` conversions.
In Haskell, to convert between `ByteString` and a `[Char]` or `String`, we would use the library [`Data.ByteString.Char8`](https://hackage.haskell.org/package/bytestring-0.9.2.1/docs/Data-ByteString-Char8.html).

    pack :: String -> ByteString
and to do the reverse, `ByteString` to `String`:

    unpack :: ByteString -> String


This conversion between `ByteString` and `String` in Haskell is similar to converting between `char*` and `string` in C++.

In C++, to convert from `char*` to `string`, we would use the built in string constructor that takes in a ‘char*’. Include the libraries `sstream` and `string`.:

    char* str = “hello”;
    string str2(str);

In C++, to convert from `string` to `char*`, you would use `c_str()`,

    string s = “apple”;
    char* c_arr = s.c_str();

Both C++ and Haskell take one line to convert between these two types of variables (after declaring the variable). However, Haskell’s conversion is clear and concise. C++ is not so straightforward. Next, we will talk about the conversions from `ByteString` and `Text`.

---
###`ByteString` to `Text` Conversions
In Haskell, there are several ways to convert from `ByteString` to `Text` because of the different types of unicode encodings.
For example, in the library [`Data.Text.Encoding`](http://hackage.haskell.org/package/text-1.1.1.3/docs/Data-Text-Encoding.html) there is a function

    decodeUtf8 :: ByteString -> Text
where it decodes a `ByteString` that contains `utf8` encoded text.
Similarly, there are many ways to convert between `Text` and `ByteString`.
In the same library, [`Data.Text.Encoding`](http://hackage.haskell.org/package/text-1.1.1.3/docs/Data-Text-Encoding.html) there is a function

    encodeUtf8 :: Text -> ByteString
where it encodes text that uses `utf8` encoding into a `ByteString`.

In C++, this type of conversion from unicode `string` to ASCII `string` isn’t as simple as Haskell’s.
In C++, to convert between a ASCII `string` and unicode `string`, we use `converter`. Include `string`, `locale`, and `codecvt`.:

    std::wstring_convert<std::codecvt_utf8<wchar_t>> converter;
    const std::wstring wide_string = L"This string";
    const std::string u_string = converter.to_bytes(wide_string);

The variable `u_string` will contain `This string` as an ASCII string.

To convert to or from any type of unicode character (in both Haskell and C++), the user must know the unicode encoding. Two common examples are `UTF8` and `UTF16`.

---
###Extra: C++ Conversions `wstring` vs. `utf8`
Include the libraries `codecvt` and `string`.
([Click here for more information](http://stackoverflow.com/questions/4358870/convert-wstring-to-string-encoded-in-utf-8)).

    // convert UTF-8 string to wstring
    std::wstring utf8_to_wstring (const std::string& str)
    {
        std::wstring_convert<std::codecvt_utf8<wchar_t>> myconv;
        return myconv.from_bytes(str);
    }

    // convert wstring to UTF-8 string
    std::string wstring_to_utf8 (const std::wstring& str)
    {
        std::wstring_convert<std::codecvt_utf8<wchar_t>> myconv;
        return myconv.to_bytes(str);
    }


We can use these functions to convert between `wstring` and `utf8`.
However, I must know what kind of conversion I am doing. Otherwise, I may end up with gibberish or the incorrect output like the “[Bush hid the facts](http://en.wikipedia.org/wiki/Bush_hid_the_facts)” event. "Bush hid the facts" shows how important it is to know what encoding the characters have.

##Conclusion
I think that converting in Haskell is cleaner, simpler, and straightforward. While there is a correct way to convert between types in Haskell, there isn’t a correct way to convert between variables in C++. C++ does not have a clear encoding variable for unicode, but Haskell takes care of each type of encoding type.

