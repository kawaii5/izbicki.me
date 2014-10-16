#Converting Character Encodings in Haskell vs. C++

There are many different languages, characters, and symbols that are used. Programs need to be able to correctly store and display these characters. This is done by the use of different encodings such as ASCII and unicode. While Americans usually use [ASCII](http://www.asciitable.com/) to decode their characters, there is another type of encoding called unicode which stores other types of characters. Two common examples of unicode are UTF-8 and UTF-16.

[UTF-8](http://en.wikipedia.org/wiki/UTF-8) and [UTF-16](http://en.wikipedia.org/wiki/UTF-16) are both types of character encodings that are capable of encoding all possible characters and are variable-length. [Variable-length](http://en.wikipedia.org/wiki/Variable-width_encoding) encoding is a type of character encoding where codes of different lengths are used to encode a character set to display. However, UTF-8 encoding uses 8-bit code units, whereas UTF-16 encoding uses one or two 16-bit code units.

The way a program encodes characters is very important. In 2004, a bug was discovered in some Microsoft Windows applications. When a user typed in “Bush hid the facts” in a Notepad document, saved, closed and then reopened the same file, nonsensical words “畂桳栠摩琠敨映捡獴” would be displayed in place of “Bush hid the facts”. The ASCII characters “Bush hid the facts” were encoded as UTF-16 characters. [(Read more)](http://en.wikipedia.org/wiki/Bush_hid_the_facts).

Converting between string-types is important in each coding language. If I put a UTF-16 encoded string in a function that worked with UTF-8 encoded strings, the string may be treated incorrectly or cannot be dealt with.

I will focus on two programming language to compare their differences in the way they convert from one encoding to another: Haskell and C++.

##Converting from One Encoding to Another

C++ does not have an easy way to store and convert unicode characters. However, people have found workarounds using [`string`](http://www.cplusplus.com/reference/string/string/) and [`wstring`](http://www.cplusplus.com/reference/string/wstring/?kw=wstring). `wstring` is fixed-length and uses 16-bit code units and an encoding called UCS-2, which is similar to UTF-16. 

The UCS-2 uses a fixed-length format rather than the variable length format. This means that each character is exactly 2-bytes long. Some mistaken UCS-2 as UTF-16 [(Read more)](http://en.wikipedia.org/wiki/Universal_Character_Set).

The `string` class is able to handle variable-length and multi-byte characters if encoding is used (such as UTF-8 or UTF-16).

To do these conversions in C++, you should use [`codecvt`](http://www.cplusplus.com/reference/locale/codecvt/?kw=codecvt) and `string` libraries. [(Read more)](http://stackoverflow.com/questions/4358870/convert-wstring-to-string-encoded-in-utf-8).

On the other hand, Haskell has a clean and simple way of converting and storing unicode characters. Haskell uses one library, [`Data.Text.Encoding`](http://hackage.haskell.org/package/text-1.1.1.3/docs/Data-Text-Encoding.html) to do these conversions.

This table shows the differences from converting from `ByteString` to UTF-8 `Text` in Haskell and `wstring` and UTF-8 `string` in C++. For Haskell, import [`Data.ByteString`](https://hackage.haskell.org/package/bytestring-0.9.2.1/docs/Data-ByteString.html).
<table>
<tr>
<td>Haskell</td>
<td>C++</td>
</tr>
<tr>
<td><pre>
decodeUtf8 "unicode ByteString to UTF8"
</td></pre>
<td><pre>
// convert wstring to UTF-8 string
std::wstring_convert> myconv;
myconv.to_bytes(L"This string");
</td></pre>
</tr>
</table>

Heres the table from converting from UTF-8 `Text` to `ByteString` in Haskell and UTF-8 `string` and `wstring` in C++. For Haskell, import [`Data.Text`](http://hackage.haskell.org/package/text-0.11.2.0/docs/Data-Text.html).
<table>
<tr>
<td>Haskell</td>
<td>C++</td>
</tr>
<tr>
<td><pre>
encodeUtf8 "unicode UTF8 to ByteString"
</td></pre>
<td><pre>
// convert UTF-8 string to wstring
std::wstring_convert<std::codecvt_utf8<wchar_t>> myconv;
myconv.from_bytes("This string");
</td></pre>
</tr>
</table>

##Conclusion
I think that converting in Haskell is cleaner as it has a built-in function. C++ does not have a clear encoding variable for unicode, but Haskell takes care of each type of encoding type. Instead, C++ uses a template. However, conversions can be advantageous in a template because then it can convert between string types easily. The C++ functions are based on the same format, using "`wstring_convert`..." and it is able to convert between a UCS-2 encoded string to a UTF-8 encoded string and back.
