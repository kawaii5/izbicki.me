#Conversions Between Character Strings in Haskell vs. C++

##Understanding Return Types in Haskell vs. C++
I found `:t` fascinating when learning Haskell. This command explains how to use a function by showing what types it uses.

```Haskell
    ghci> :t show
    show :: Show a => a -> String
```
In the above example, `:t` explains how `show` works. It's format is `Show a` and then it takes in a variable `a` and outputs a `String`. `:t` tells me what kinds of types each function takes. However, there are types which output similar values but are stored as different types such as `String`, `ByteString`, and `Text` (in Haskell).

---
Programming languages have many different ways to store characters, like a string. Haskell can store characters in `String`, `ByteString` and `Text`. Converting between a `String`, `ByteString`, and `Text` is important because functions ask for different types. For example, `writeBS :: ByteString -> Snap ()` asks for a `ByteString`. If we use any other way of storing characters, like a `String`, there will be an error message such as in the example below.

```Haskell
    ghci> writeBS "hello"

    <interactive>:3:9:
        Couldn't match expected type `Data.ByteString.Internal.ByteString'
                with actual type `[Char]'
        In the first argument of `writeBS', namely `"hello"'
        In the expression: writeBS "hello"
        In an equation for `it': it = writeBS "hello"
```

Later, I will show you how to convert a `String` into a `ByteString` so you can use this function.

For C++ users, it is as if you are converting between a `list<char>` (`String` in Haskell), ASCII `string` or `char*` (`ByteString` in Haskell), and `wstring` (`Text` in Haskell). Just as `list<char>` does not necessarily store characters adjacently in C++, the `[Char]`, or `String`, types do not necessarily store characters adjacently in Haskell. Similarly, `string`, or `char*`, stores characters adjacently in C++ just like Haskell's `ByteString`. However, there are other variable types like `wstring` that can hold other types of encoding. `wstring` can store 16-bits in each character slot, instead of the standard 8-bit. This is similar to Haskell’s `Text`, but `Text` can take any type of encoding such as ASCII or UTF16 (a type that stores unicode characters). C++ does not have a variable type specifically for unicode, but there are types that can store unicode characters such as `wstring` and `utf8`. C++ has a library by IBM called [`ICU`](http://site.icu-project.org/) which can take all types of encodings.

First, we will look at a simple conversion, between `ByteString` and `String`.

---
###`ByteString` to `String` Conversions
First, we will look at the Haskell’s `ByteString` conversions and compare them to C++ `string` conversions.
In Haskell, to convert between `ByteString` and a `[Char]` or `String`, we would use the library [`Data.ByteString.Char8`](https://hackage.haskell.org/package/bytestring-0.9.2.1/docs/Data-ByteString-Char8.html).

```Haskell
    pack :: String -> ByteString
```
and to do the reverse, `ByteString` to `String`:

```Haskell
    unpack :: ByteString -> String
```


This conversion between `ByteString` and `String` in Haskell is similar to converting between `char*` and `string` in C++.

In C++, to convert from `char*` to `string`, we would use the built in string constructor that takes in a `char*`. Include the libraries `sstream` and `string`.:

```C++
    char* str = "hello";
    string str2(str);
```

In C++, to convert from `string` to `char*`, you would use `c_str()`,

```C++
    string s = "apple";
    char* c_arr = s.c_str();
```

Both C++ and Haskell take one line to convert between these two types of variables (after declaring the variable). These are both simplistic and look easy to use. The functions are clear and concise on their usage.
Next, we will look at a more complicated conversion, between `ByteString` and `Text`.

---
###`ByteString` to `Text` Conversions
In Haskell, there are several ways to convert from `ByteString` to `Text` because of the different types of unicode encodings. There are many types of unicode because of the vast amount of characters. People in Russia use a different default encoding than the encoding we use in US (which is usually ASCII). We need different types of encoding to encode different types of characters, even if coming from different countries. If we used the ASCII encoding on the Russian language, it will output gibberish.

I'm going to take some time here to explain about Unicode encodings.
Quick Reminder: A string stores 8-bit characters.
Now, each character in a string uses the default encoding (in the US, it is ASCII) which tells the program how to encode the character. There are many different types of encoding that can be stored within data types such as `wstring`, `utf8` in C++ and `ByteString` and `Text` in Haskell. Within these types, characters can be longer, or shorter than 8-bits. `ByteString` stores characters with the same bit length, otherwise it may not be encoded correctly (this is the same with `wstring` and `utf8`). However, `Text` stores characters with varying length. `Text` knows what kind of encoding each character has when it is stored whereas `ByteString` doesn't. 

In the library [`Data.Text.Encoding`](http://hackage.haskell.org/package/text-1.1.1.3/docs/Data-Text-Encoding.html) there is a function

```Haskell
    decodeUtf8 :: ByteString -> Text
```
where it decodes a `ByteString` that contains `utf8` encoded text.
Similarly, there are many ways to convert between `Text` and `ByteString`.
In the same library, [`Data.Text.Encoding`](http://hackage.haskell.org/package/text-1.1.1.3/docs/Data-Text-Encoding.html) there is a function

```Haskell
    encodeUtf8 :: Text -> ByteString
```
where it encodes text that uses `utf8` encoding into a `ByteString`.

In C++, this type of conversion from unicode `string` to ASCII `string` isn’t as simple as Haskell’s.
In C++, to convert between a ASCII `string` and unicode `string`, we use `converter`. Include `string`, `locale`, and `codecvt`.:

```C++
    std::wstring_convert<std::codecvt_utf8<wchar_t>> converter;
    const std::wstring wide_string = L"This string";
    const std::string u_string = converter.to_bytes(wide_string);
```

The variable `u_string` will contain `This string` as an ASCII string.

To convert to or from any type of unicode character (in both Haskell and C++), the user must know the unicode encoding. Two common examples are `UTF8` and `UTF16`.

---
###Extra: C++ Conversions `wstring` vs. `utf8`
`wstring` is a `string` in which is character is 16-bits long, twice as long as a `string` character. `utf8` can store 8-bit characters; it is also "capable of encoding all possible characters in Unicode" [(See Wiki article)](http://en.wikipedia.org/wiki/UTF-8).

Include the libraries `codecvt` and `string`.
([Click here for more information](http://stackoverflow.com/questions/4358870/convert-wstring-to-string-encoded-in-utf-8)).

```C++
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
```


We can use these functions to convert between `wstring` and `utf8`.
However, I must know what kind of conversion I am doing. Otherwise, I may end up with gibberish or the incorrect output like the “[Bush hid the facts](http://en.wikipedia.org/wiki/Bush_hid_the_facts)” event. "Bush hid the facts" shows how important it is to know what encoding the characters have.

##Conclusion
I think that converting in Haskell is cleaner, simpler, and straightforward. While there is a correct way to convert between types in Haskell, there isn’t a clear, correct way to convert between variables in C++. C++ does not have a clear encoding variable for unicode, but Haskell takes care of each type of encoding type.

