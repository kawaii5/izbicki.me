---
title: Polymorphism in Haskell vs. C++
author: <a href="//github.com/jduga002">Jonathan Dugan</a>
---

[Parametric polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism) is when you write a single piece of code for a function that will work on a variety of data types.  In C++, this is pretty confusing, but it's really easy in Haskell. Let’s take a look at an example.

Let’s say we want a function that calculates the surface area of a box.  In C++, we’d use templates so that our function works with any numeric type:

    template<typename T>
    T boxSArea(T length, T width, T height)
    {
        return length * width * height;
    }

Templates have an awkward syntax, but that isn’t too much of a hassle. C++ has much bigger problems.  What if in the course of writing your program, you accidentally pass in some strings to this function?

    int main()
    {
        Cout << boxSArea(“oops”,”no”,”strings”) << endl;
    }

We get this error when we compile with g++:

    test.cpp: In instantiation of _T cuboidSArea(T, T, T) [with T = const    char*]_:
    test.cpp:22:47:   required from here
    test.cpp:8:19: error: invalid operands of types _const char*_ and _const char*_ to binary
    _operator*_
        return length * width * height;

Compare this error to if, instead of using templates, we used doubles:

    double boxSArea(double length, double width, double height)
    {
        return length * width * height;
    }

    int main()
    {
        Cout << boxSArea(“oops”,”no”,”strings”) << endl;
    }

After compiling with g++, we get this error message:


    test.cpp: In function _int main()_:
    test.cpp:22:47: error: cannot convert _const char*_ to _double_ for argument _1_ to _double
    cuboidSArea(double, double, double)_
        cout << cuboidSArea("oops","nope","bad!") << endl;

We see that this error is shorter and easier to use, as it clearly tells us we cannot pass string literals to our function.  Plus there is no superfluous comment about our “instantiation” of `boxSArea`.
Now let’s see how to write this function in Haskell:

    cuboidSArea :: a -> a -> a -> a
    cuboidSArea length width height = length * width * height

Let’s see what happens when we try to compile this.

    test.hs:2:50:
        No instance for (Num a) arising from a use of `*'
        Possible fix:
          add (Num a) to the context of
            the type signature for cuboidSArea :: a -> a -> a -> a
        In the expression: length * width * height
        In an equation for `cuboidSArea':
            cuboidSArea length width height = length * width * height

Uh-oh!  An error message!  What went wrong?  It says that we tried to use the `*` operator without declaring our parameters as an instance of the `Num` type class.

But what is a type class?  This leads us to [ad hoc polymorphism](https://en.wikipedia.org/wiki/Ad_hoc_polymorphism), also known as function overloading.  Ad hoc polymorphism is when a function can be applied to different argument types, each with a different implementation.  For example, the STL classes stack and queue each have their own push and pop functions, which, although they have the same names, do different things:

    stack<int> s;
    queue<int> q;
    
    s.push(1); q.push(1);
    s.push(2); q.push(2);
    s.push(3); q.push(3);
    
    s.pop(); q.pop();

After the above code is executed, the stack `s` will be left with the numbers `1,2` while the queue `q` will be left with the numbers `2,3`.  The function `pop` behaves differently on stacks and queues: calling `pop` on a stack removes the item added last, while calling `pop` on a queue removes the item added first.

Haskell does not support function overloading, except through type classes.  For example, if we were to specifically declare our own `Stack` and `Queue` classes with `push` and `pop` functions:

    data Stack = Stack  [Int] deriving Show
    data Queue = Queue [Int] deriving Show

    push :: Stack -> Int -> Stack
    push (Stack xs) x = Stack (x:xs)

    pop :: Stack -> Stack
    pop (Stack []) = Stack []
    pop (Stack xs) = Stack (tail xs)

    push :: Queue -> Int -> Queue
    push (Queue xs) x = Queue (x:xs)

    pop :: Queue -> Queue
    pop (Queue []) = Queue []
    pop (Queue xs) = Queue (init xs)

It results in a compiler error:

    stack.hs:11:1:
        Duplicate type signatures for `push'
        at stack.hs:4:1-4
           stack.hs:11:1-4

    stack.hs:12:1:
        Multiple declarations of `push'
        Declared at: stack.hs:5:1
                     stack.hs:12:1

    stack.hs:14:1:
        Duplicate type signatures for `pop'
        at stack.hs:7:1-3
           stack.hs:14:1-3

    stack.hs:15:1:
        Multiple declarations of `pop'
        Declared at: stack.hs:8:1
                     stack.hs:15:1

Changing the names of our push and pop functions to, say, `stackPush`, `stackPop`, `queuePush`, and `queuePop` will let the program compile.

A more generic way, however, is to create a type class.  Let’s make a `Sequence` type class that implements our `push` and `pop` functions.

    class Sequence s where
        push :: s -> Int -> s
        pop :: s -> s

This type class declaration says that any data type that is an instance of this `Sequence` type class can use the `push` and `pop` operations, or, in other words, can add and remove an `Int`.  By making our `Stack` and `Queue` instances of the `Sequence` type class, both data types can have their own implementations of the `push` and `pop` functions!

    instance Sequence Stack where
        push (Stack xs) x = Stack (x:xs)
        pop (Stack []) = Stack []
        pop (Stack xs) = Stack (tail xs)

    instance Sequence Queue where
        push (Queue xs) x = Queue (x:xs)
        pop (Queue []) = Queue []
        pop (Queue xs) = Queue (init xs)

Replacing our function definitions with these instantiations of the Sequence type class lets our program compile.

Type classes are also an important part of using templates in function definitions.  In our function `boxSArea`, we got an error because we tried to use the `*` operation without declaring the type variable `a` as an instance of the `Num` type class.  The `Num` type class is basically for anything that acts like a number, such as `Int`, `Float`, and `Double`, and it lets you use the common operations of `+`, `-`, and `*`.  

Let’s change our function to declare that `a` is a `Num`:

    boxSArea :: (Num a) => a -> a -> a -> a
    boxSArea length width height = length * width * height

This is called adding a class constraint.  Whenever we want to declare a template function that relies on other functions, we have to add a class constraint that tells both the user and the compiler  which types of data can be put into the function.

If we were to call `boxSArea` on strings, we would get this simple error message:

    ghci> boxSArea "a" "b" "c"

    <interactive>:14:1:
        No instance for (Num [Char]) arising from a use of `cuboidSArea'
        Possible fix: add an instance declaration for (Num [Char])
        In the expression: cuboidSArea "a" "b" "c"
        In an equation for `it': it = cuboidSArea "a" "b" "c"

The compiler tells us it can’t evaluate this function because strings aren’t numbers!  If we really wanted to, we could make `String` an instance of the `Num` type class, and then this function would work!  (Of course, why you would want to do that is beyond me.)  That’s the power of parametric polymorphism combined with type classes.

So there you have it.  In C++, although we can easily implement ad hoc polymorphism through function overloading, parametric polymorphism is a tricky beast.  This is made easier in Haskell, especially with the use of type classes.  Type classes guarantee that data passed in to functions will work, and guide the user into what they can pass into a function.  Use type classes to your advantage when you next write a Haskell program!
