---
title: Getting started with GitHub, vim, and the Terminal
author: <a href="//github.com/rgosh001">Rashid Goshtasbi</a> and <a href="//github.com/kryne001">Kyler Rynear</a>
---

Learning to use `git`, `vim`, and `bash` was hard for us.
These tools are so different than the tools we used when we first learned to program.
And they're confusing!
But our professor made us use them... and eventually... after we learned the tools... we discovered that we really like them! 
So we've put together a simple video guide to help you learn and enjoy these tools too.
We did this as part of the CS100 open source software development class at UC Riverside. 

*[Click here](https://www.youtube.com/watch?v=bap-NSjgPFg&list=PLQ6W7BIxh4zsJgGwZr-cpHCB_EjyY1NFK) to watch the full playlist on YouTube.*

##Getting Started with GitHub

<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/bap-NSjgPFg?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

This video shows you step by step how to create an account on GitHub. 
Then we see how to create our first repository called `test`, and transfer it from GitHub onto our local machine using the `git clone` command.

###Creating a file, pushing to GitHub, and pulling from GitHub

<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/UrLkCZaXg9o?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

How do we create files and upload them to GitHub?
The `touch <filename>` command will create an empty file for you.
The `vim <filename>` command will open a file in an advanced text editor that [we talk about farther down the page](#vim).
The `git push` command sends these files from your local machine up to GitHub, and the `git pull` command downloads files from GitHub and saves them to your local computer.

###<a name="branch">Branches</a>
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/E8-hUsR7IXA?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

Branches let you work on files without messing up your original code. 
When you finish your changes, you can merge them into the `master` branch.
This is the best part of version control.


###<a name="tags">Tags</a>
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/WKG1u4Y_f3s?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

Most programs have different versions, for example: 1.0, 1.1, 1.2, 2.1 and 2.2.1.
The `git tag` command let's you create these versions. 
They're just like a checkpoint in a Mario game!

###<a name="fork">Forking & Pull Requests</a>
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/tTnL84EvJTM?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

Let's say you want to contribute to an open source project, but you don't have permission.
In order to contribute to someone else's repository, you must first "fork" it to create a repo that you do have push permission on.
Then you issue a pull request through the GitHub website.
This tells the owner of the original repo that you've made some changes they can incorporate.

###<a name="readme">The `README.md` file</a>
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/4UTSEKzsSvM?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

`README.md` files are how you document your projects.
The `README.md` should explain your program, give installation instructions, and list known bugs.
Basically, it explains to someone else who has absolutely no idea what your program does or how to code, but it enables the user to understand the concepts and basic directions to execute your program.
The `.md` extension at the end of the filename indicates that the file uses markdown formatting.
This is a simple way to create nice looking documentation.

## <a name=vim>Learning vim</a>

`vim` is an advanced text editor for Unix operating systems.
It's powerful, but all the commands are intimidating for first time users.
Even though it's hard to get used to at first, these videos will help you learn some of the basic commands and get comfortable with `vim`.

###Getting Started
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/szTtE60fIt8?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

It was difficult at first trying to transverse my code while using `vim`. I was so used to being able to use my mouse and simply click where I wanted to go.
There are many ways to maneuver inside of `vim`. 
Some may just use the `h,j,k,l`, `up, down, left, right arrow keys`, or the `w, e, b` keys to move. 
You can also press `gg` to go to the top of the code, `G` to go to the bottom of it, and `(any number)G` to go to the line number typed before the capital G.)

Cutting, copying, and pasting took a while to get used to when using `vim`.
Sometimes there was something I wanted in my code that was in the instructions for the assignment.
In order to paste I would use the `p` command, but I could not paste things from outside of `vim` into it.
If I had something copied outside of `vim`, then to paste it into `vim` I would right click and just click paste.
This would paste it wherever the cursor currently is.
If you right click to copy, then it will not affect what is copied by using the commands `y` to copy or the commands `d` or `x` to cut.
If those commands are used, the just clicking `p` will paste them.
There are other ways to store more than one thing while copying or cutting, but these two ways were the most helpful as I learned how to use `vim`.

Another personal favorite features of `vim`, are the `shift-a` (takes you to the end of the line and into insert mode) and the `shift-i` (takes you to the beginning of the line and into insert mode) command.
You can also press `a` to append after the cursor position, as well as `i` to insert before the current cursor position

`vim` also allows you to use the `v` or `shift-v` keys to highlight certain text or lines of code.
You can then use other `vim` commands such as the copy, paste and delete keys to perform your needed actions.

###Indentation
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/uuztdE_gixs?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

At first it felt very time consuming to indent multiple lines.
I felt this way until I found about the `V` command.
`V` lets users highlight a line and pressing up or down can highlight as many lines as they desire.
All that was left to do was to type `>` after everything I wanted to indent was highlighted and it all would indented once to the right.
Typing `<` would instead indent it to the left if I ever wanted to do that.

###Deletion

<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/x0BMbS2kWYc" frameborder="0" allowfullscreen></iframe>


There are two commands for deleting single character. `x` deletes the character that the cursor is on and moves the cursor to the right; and `X` deletes the character that the cursor is on and moves the cursor to the left.

The `d` command is a more powerful way to delete.
`d` can be used with many different things after it. 
`dd` will delete the entire line.
`d$` will delete the rest of the current line.
`de` will delete from where the cursor is up until the end of the word.

###<a name="replace">Replacing</a>

<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/d-quT7u3f_o" frameborder="0" allowfullscreen></iframe>

Lower case `r` can replace one letter while upper case `R` can replace one letter with many. 

There are three `c` commands that I regularly use for replacement: `ce` , which deletes up until the end of the word that the cursor is currently on, then allows you to insert immediately; `c$` , which deletes from where the cursor is up until the end of the line, then allows you to insert immediately; and `cc` , which deletes the whole line that the cursor is on and allows you to insert immediately at the beginning of the line.

###<a name="vimrc">Customizing your vim editor with the .vimrc file</a>
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/VhAiVux6GBg?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

Ever wondered how've we get our `vim` editor to work in the way we have it versus the default editor? 
`vim` has a file where you can setup it's defaults such as auto parentheses, auto-indent, and much more. 
By watching our video above, you can easily create new defaults for your `vim` editor that can cut time spent formating your text to spend more on coding.

##Learning The Terminal

One of the best features of Unix operating systems is the powerful terminal they provide.

###<a name="ls">The `ls` command</a>
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/xSSahh5HbUY?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

The `ls` command is one of the most used terminal commands. 

The basic `ls` command, when run, displays the contents within the current working directory.
Passing in a directory name as an argument will display the contents of that directory. 
It is also possible to pass in a path for a directory to display any directory, regardless of the directory the user is currently in.

If the `-a` flag is passed in with `ls`, all items in the current working directory prepended with a `.` are also displayed, along with the rest of the items.

Passing in the `-l` flag prints information for each item in the directory in a series of columns on a single line. 
The first column displays the read, write, and executable permissions for the main user, the group the current user is in, and any user in that order. 
The next column shows the owner of the item and the next column shows the group owner. 
The fourth column displays the size, in bytes, of the item. 
The fifth column displays the moment the item was created, and the last column displays the name of the item.

If the `-R` flag is passed in, the command will display the contents of the current directory, and then recursively enter every directory within the current directory and display the contents of that directory, then keep going into every directory until there are no more directories in the current directory it is in.

All these options are combinable for different uses. 
For example, I could use the `-l` and `-a` flags to display the information for the items prepended with a `.` , or use `-R` and `-l` together.

###<a name="cdmv">The `cd` and `mv` commands</a>
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/1s5TiFbETh4?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>
 
The `cd` and `mv` commands are crucial commands in order to actually use the terminal.
Without cd, I would forever be stuck in their home directory.
The `mv` command is necessary for moving files from one section of the hard drive.
The `cd` command by itself will change the current working directory to the home directory.
If passed a directory name that is within the current working directory, the current working directory will be changed to the name of the passed in directory.
`cd` will also take a path as an argument. When a path is passed in, the current working directory will be changed to the directory specified by the path.
When `cd` is passed with `..`, the directory will go backwards, the directory that the current directory is in.

The `mv` command will move an item within a certain directory to the directory passed in.
 
If the destination argument is not a path, the command will look for the destination in the current working directory.
The destination argument can be a path, so I can move the item to any directory in the hard drive.



###<a name="script">Recording terminal sessions via scripts</a>

<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/ZnIrku27C94?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

With the `script` command you can record the commands you run in your terminal into a file.
By just typing `script file_name_here`, you can start a script. 
Also, you don't need to worry about making a file beforehand, because when you specify the filename, it will make once for you in that name. 
Then when you're done, type `exit` and your terminal will say your script session has ended and re-state the filename in which it recorded all your commands in. 

###<a name="ssh">How To SSH (into well server)</a>
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/Letf4txWPic?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

Computer Science students have the ability to log into the school's server using the `ssh` command.
The way to do access the terminal is to type into the command terminal the following text: 
```
ssh your_NetId@bell.cs.ucr.edu
``` 
If it is your first time entering the terminal, you will be asked to trust the encryption that the server uses, then prompted to enter the password associated with your NetID. Once doing all those steps, you will be brought to your home directory on the server. To exit the server, type `exit` into the command prompt and press enter. 

A useful command that moves files to and from the remote server onto your home computer is the `scp` command. To put items from your home computer to the school's server, type into the command prompt: 
```
scp filename/absolute_path your_NetID@bell.cs.ucr.edu:absolute_path
```
To move items from the remote server onto your home computer, type into the command prompt: 
```
scp your_NetID@bell.cs.ucr.edu:absolute_path absolute_path
```

###<a name="spectacle">Spectacle App: Using the terminal and `vim` in one screen</a>
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/j1fnYZp4foI?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

One of the first things I noticed about `vim` that I initially disliked was that it took over the terminal when I used it.
Users with Windows 7 & above automatically have this ability by dragging your screen to the left or right border of your screen.
Unfortunately, OS X users don't have this built in ability. To get around this, OS X users can install the Spectacle App which will enable you to organize multiple windows on your screen with a touch of a buttom.
To get around this issue, I started using two terminals instead of just one while I was programming.
I would run `vim` using the first terminal and would run the executable in the second.
It was as simple as using `:w` to save on `vim` instead of using `:wq` to save and quit.
I could now test my code without ever having to close `vim`.

###<a name="perror">perror</a>
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/GsoVzP3sRsA?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

When programming for unix based operating systems (which is a primary component of CS100), system calls are a prominent component for code. 
The `perror` function captures the error value (if returned) from the system call and prints to stdout an error message based on the system call and the type of error. 
It takes in one c-string argument, which is a message the user can pass in. 
