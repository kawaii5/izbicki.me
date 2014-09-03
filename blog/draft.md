---
title: Reasons why C++ is easier for beginners than Haskell
author: <a href="//github.com/mikeizbicki">Paul Satrkey</a>
---

## Reasons why C++ is easier for beginners than Haskell 

So let me start off with this disclosure and bit of background: learning Haskell over these past few months has been an excruciating experience and I have only about a years worth of experience programming with C++. I do not know if learning Haskell was difficult for me because C++ is where I feel comfortable programming or if it is because Haskell has a steep learning curve. Either way, my experience with Haskell has been a whirlwind of emotion and turmoil.  In particular, the error messages from the haskell compiler `ghc` were more difficult to understand than the error messages I was used to from `g++`.

Let's see some concrete examples by comparing a few code snippets side by side. 



### Example 1 


Below are two equivalent C++ and Haskell programs. I've intentionally added some syntax errors:

```
	/* C++ Code */
	#include <iostream>

	using namespace std;

	int main () 
	{
		int in = -1;
		cout << "Please choose 1 for a message" << endl;
		cin >> in;
err->	if in == 1 
		{
			cout << "Hello, World!" << endl;
		}
 		else{
			cout << "Error, wrong choice" << endl;
		}
		return 0;
	}
```
----------------------------------------------------------------------- 
```
	{- Haskell Code -}
	main = do
		putStrLn "Please enter 1 for a message"
		num <- getLine
		if num /= "1"
			then do
				putStrLn "Error, wrong choice"
err->		
				putStrLn "Hello, World"	
```

Alright, so the first notable difference is that the Haskell code is far more compact. It takes up roughly half the space that the C++ code does yet they both output `hello world` when the correct number is entered. Great! Haskell already seems better, right? Wrong! Notice how I omitted the parentheses in the C++ code for the `if` parameters and the `else` in the Haskell code. Both omissions are simple mistakes that I've made while learning these languages. 

Now let's see the error messages:

	-- C++ Error --
	main.cpp: In function 'int main()':
	main.cpp:15:5: error: expected '(' before 'in'
	main.cpp:19:2: error: 'else' without a previous 'if'
	Compilation failed.
-----------------------------------------------------------------------
	-- Haskell Error --
	[..]main.hs:19:1:
		parse error (possibly incorrect indentation or mismatched brackets)
	Failed, modules loaded: none.

Both error messages let the programmer know where the mistake happened, but the `g++` message is far more helpful. It tells us how to fix the syntax error by adding some missing parentheses. Bam! Problem solved. 

Now let us turn to `ghc`'s output. Okay, something about a parse error use.. might have indentation errors.. and no modules loaded. Cool. Now I've never taken a compiler course, so I'm still left wondering what's wrong with my code. The error message is simply not helpful.

### Example 2
Next, we will look at what happens when you pass too many arguments to functions in both languages:

```
	/* C++ Code */
	#include <iostream>
	using namespace std;

	int main () {
		string in[256];
		cout << "Please enter a single word to get the string size back" << endl;
		cin >> in;
	
err->	cout << "The size of your string, \"" << in << "\", is " << (unsigned)strlen(in, in);
		cout << "!" << endl;
		return 0;
	}
```
-----------------------------------------------------------------------
```
	{- Haskell Code -}
	main = do
		putStrLn "Please enter a single word to get the string size back"
		num <- getLine
err->	let size = length num num
		putStrLn $ "The size of your string, \"" ++ num ++ "\", is " ++ show size ++ "!"
```
The source of the error in the above code sample is that in both languages the length functions are being passed too many arguments. 

	-- C++ Error --
	main.cpp:16:78: error: too many arguments to function 'int newLength(std::string)'
	main.cpp:6:5: note: declared here
	Compilation failed.
-----------------------------------------------------------------------
	-- Haskell Error --
	Couldn't match expected type 'String -> t0' with actual type 'Int'	
	The function 'length' is applied to two arguments,
	but its type '[Char] -> Int' has only one
	In the expression: length num num
	In an equation for 'size':size = length num num
	Failed, modules loaded: none.

As you can see, this time both messages seem to be helpful. The C++ error clearly explains how to fix the code, and I even understand the Haskell error this time. Both languages tell me that there are too many arguments. Yet C++ tells this in a single line versus Haskell's five lines. So even when Haskell is actually helpful with its error messages, it still manages to be more convoluted and not entirely transparent as to what it wants the user to do.

### Example 3
For my next example, I give you another pair of syntax errors. The Haskell error personally took me 45 minutes to fix. And that was only after I emailed Mike to basically give me the answer. I had already invested so much energy and time trying to solve this minuscule error on Google to no avail.  

```
	/* C++ Code */ 
	#include <iostream>
	
	using namespace std;
	
	int main() 
	{
		string in = "";
		cout << "Please enter a single word and get the string size back" << endl;
		cin >> in;
	
		cout << "The size of your word, \"" << in << "\", is "
err->	     << /*in.*/length() << "!" << endl;
		return 0;
	}
```
-----------------------------------------------------------------------
```
	{- Haskell Code -}
	main = do
		putStrLn "Please enter a single word and get the string size back"
  		num <- getLine
		let size = length num
err->	putStrLn $ "The size of your word, \"" ++ num ++ "\", is " 
		++ {-show-} size ++ "!"
```
And the error messages:

	-- C++ Error --
 	main.cpp: In function 'int main()':
	main.cpp:11:76: error: 'length' was not declared in this scope
	Compilation failed.
-----------------------------------------------------------------------
	-- Haskell Error --
	[..]main.hs:7:36:
	parse error on input '++'
	Failed, modules loaded: none.
	Prelude>
	
Okay, so the C++ compiler says something about `length` not being declared in the scope. Now that is odd, C++ has a built in length function so it has to be defined, right? My natural thought process from this point is to Google `C++ length`. The first hit tells you exactly how to use the function. I read through the examples and figured out that the compiler thought I was using my own function named length and was telling me that it does not exist in the scope of main. Fair enough. Now even though the compiler warning may not have been as helpful as the previous example, it gave me enough information to start a successful Google search which did lead me to the solution. 

Now to the Haskell warning. It appears I have another parse error in my code. The error says the mistake is near my use of `++`. Normally, that would be an excellent lead to the solution if I did not have four `++` in my code. As you can see, I forgot to include `show` before using the `size` function provided by Haskell. The mistake itself seems simple enough, yet from the error that haskell gave me, I was unable to find the solution on my own. 

### Example 4
For this next example, I use the same code as above just with new errors. 

```
	/* C++ Code */ 
	#include <iostream>
	
	using namespace std;
	
	int main() 
	{
err->	string in = ""
		cout << "Please enter a single word and get the string size back" << endl;
		cin >> in;
	
		cout << "The size of your word, \"" << in << "\", is "
		     << in.length() << "!" << endl;
		return 0;
	}
```
-----------------------------------------------------------------------
```
	{- Haskell Code -}
err->	main = {-do-}
		putStrLn "Please enter a single word and get the string size back"
  		num <- getLine
		let size = length num
		putStrLn $ "The size of your word, \"" ++ num ++ "\", is "
				++ show size ++ "!"
```

As you can see, in the C++ I forgot include a semicolon and in Haskell I forgot the `do` in main. As a beginner, I've personally made both of these mistakes.

	-- C++ Error --
	main.cpp:8:2: error: expected ',' or ';' before 'cout'
	Compilation failed.
-----------------------------------------------------------------------
	-- Haskell Error --
	[..]main.hs:4:13:
		parse error on input '<-'
	Failed, modules loaded: none.

C++ delivers a clear message explaining how to fix the error. Haskell however, is not so helpful. It says there's a parse error on the input operator. How should I know this is related to a missing `do`? 

###Example 5
Now, for our final comparison, let us see what happens when you call the built-in length functions of C++ and Haskell with no arguments at all.

```
	/* C++ Code */
	#include <iostream>
	#include <cstring>

	using namespace std;
	
	int main (){
		char input[256];
		cout << "Please enter a word" << endl;
		cin >> input;
	
err->	cout << "THe size of your string is: " << (unsigned)strlen();
		cout << "!" << endl;
		return 0;
	}
```
-----------------------------------------------------------------------
	{- Haskell Code -}
	main = do
		putStrLn "Please enter a word"
		num <- getLine
err->	let size = length 
		putStrLn $ "The size of your string is: " ++ show size ++ "!"
```
	
Now let us see the different error messages that are produced.

	-- C++ Error --
	main.cpp: In function 'int main()':
	main.cpp:11:61: error: too few arguments to function 'size_t_strlen(const char*)'
	Compilation failed.
-----------------------------------------------------------------------
	-- Haskell Error --
	[..]main.hs:7:36:
	No instance for (Show ([a0]->Int)) arising from a use of 'show'
	Possile fix: add an instance declaration for (Show ([a0]->Int))
	In the first argument of '(++)', namely 'show size'
	In the second argument of '(++)', namely 'show size ++ "!"'
	In the second argument of '(++)', namely
	  '"\", is " ++ show size ++ "!"'
	Failed, modules loaded:none.

Once again, it appears that the C++ compiler `g++` knew exactly what was wrong with the code and how to fix the error. It tells me that there are not enough arguments in my function call.

Wow, Hakell's error message is quite the mouthful this time. On top of it being lengthy, I'm not sure what exactly `ghc` is even wanting me to correct. This type of error message is definitely not beneficial in my debugging efforts. The error is simply too vague and technical to derive any help in order to fix my code. 

###Conclusion 
To me, Haskell seems to be an excellent language only if you are a well-versed programmer because Haskell simply is not that user-friendly. How is a programmer to write successful code if a few simple mistakes nearly cripple his progress, all because of vague and unhelpful warning messages? Haskell's compiler `ghc` simply lags behing C++'s superior `g++` in terms of useful functionality in helping fix buggy code. Throughout my experience with Haskell, I have developed a newfound sense of appreciation for the `g++` compiler and how friendly it is for beginning programmers. 


<iframe width="560" height="315" src="//www.youtube.com/embed/Fv5Cy8i14ck" frameborder="0" allowfullscreen></iframe>
