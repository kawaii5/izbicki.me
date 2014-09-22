#Conversions Between Character Encodings in Haskell vs. C++

As you know, there are many different languages, characters, and symbols. Programs need to be able to store these characters and display the correct character. Programs are able to understand these different characters by their encodings. Americans usually use [ASCII](http://www.asciitable.com/) to decode their characters. There is a special type of encoding called unicode. Two common examples are UTF-8 and UTF-16.

[UTF-8](http://en.wikipedia.org/wiki/UTF-8) and [UTF-16](http://en.wikipedia.org/wiki/UTF-16) are both types of character encodings that are capable of encoding all possible characters. However, UTF-8 encoding is variable-length and uses 8-bit code units. On the other hand, UTF-16 encoding is variable-length and uses one or two 16-bit code units. [Variable-length](http://en.wikipedia.org/wiki/Variable-width_encoding) encoding is a type of character encoding where codes of different lengths are used to encode a character set to display.

