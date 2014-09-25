#Converting Character Encodings in Haskell vs. C++

As you know, there are many different languages, characters, and symbols. Programs need to be able to store these characters and display the correct character. Programs are able to understand these different characters by their encodings. Americans usually use [ASCII](http://www.asciitable.com/) to decode their characters. There is a special type of encoding called unicode. Two common examples are UTF-8 and UTF-16.

[UTF-8](http://en.wikipedia.org/wiki/UTF-8) and [UTF-16](http://en.wikipedia.org/wiki/UTF-16) are both types of character encodings that are capable of encoding all possible characters. However, UTF-8 encoding is variable-length and uses 8-bit code units. On the other hand, UTF-16 encoding is variable-length and uses one or two 16-bit code units. [Variable-length](http://en.wikipedia.org/wiki/Variable-width_encoding) encoding is a type of character encoding where codes of different lengths are used to encode a character set to display.

The way a program encodes characters is very important. In 2004, a bug was discovered in some Microsoft Windows applications. When a user typed in “Bush hid the facts” in a Notepad document, saved, closed and then reopened the same file, nonsensical words “畂桳栠摩琠敨映捡獴” would be displayed in place of “Bush hid the facts”. The ASCII characters “Bush hid the facts” were encoded as UTF-16 characters. [(Read more)](http://en.wikipedia.org/wiki/Bush_hid_the_facts).

##Converting from One Encoding to Another

I will focus on two programming language to compare their differences in the way they convert from one encoding to another: Haskell and C++.

C++ does not have a simple, good way to store and convert unicode characters. However, people have found ways using `string` and `wstring`. C++ uses multiple libraries, `codecvt` and `string`.

On the other hand, Haskell has a clean and simple way of converting and storing unicode characters. Haskell uses one library, [`Data.Text.Encoding`](http://hackage.haskell.org/package/text-1.1.1.3/docs/Data-Text-Encoding.html).

This table shows the differences from converting from ByteString to UTF8 in Haskell and wstring and UTF8 in C++.
<table>
<tr>
<td>Haskell</td>
<td>C++</td>
</tr>
<tr>
<td><pre>
   decodeUTF8 "unicode ByteString to UTF8"
</td></pre>
<td><pre>
   // convert wstring to UTF-8 string
   std::string wstring_to_utf8 (const std::wstring& str)
   {
      std::wstring_convert<std::codecvt_utf8<wchar_t>> myconv;
      return myconv.to_bytes(str);
   }

   const std::wstring wide_string = L"This string";
   const std::string u_string = wstring_to_utf8(wide_string);
</td></pre>
</tr>
</table>

Heres the table from converting from UTF8 to ByteString in Haskell and UTF8 and wstring in C++.
<table>
<tr>
<td>Haskell</td>
<td>C++</td>
</tr>
<tr>
<td><pre>
   encodeUTF8 "unicode UTF8 to ByteString"
</td></pre>
<td><pre>
       // convert UTF-8 string to wstring
       std::wstring utf8_to_wstring (const std::string& str)
       {
          std::wstring_convert<std::codecvt_utf8<wchar_t>> myconv;
          return myconv.from_bytes(str);
       }
   
       const std::string u_string = "This string";
       const std::wstring wide_string = utf8_to_wstring(u_string);
</td></pre>
</tr>
