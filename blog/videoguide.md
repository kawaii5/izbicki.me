---
title: Getting started with GitHub, vim, and the terminal
author: <a href="//github.com/rgosh001">Rashid Goshtasbi</a> and <a href="//github.com/kryne001">Kyler Rynear</a>
---

Learning to use `git`, `vim`, and `bash` was hard for us.
These tools are so different than the tools we used when we first learned to program.
And they're confusing!
But our professor made us use them... and eventually... after we learned the tools... we discovered that we really like them! 
So we've put together a simple video guide to help you learn and enjoy these tools too.
We did this as part of the CS100 open source software development class at UC Riverside. 

##Getting Started with GitHub

<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/RMyqz8jh3Rc?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

This video shows you step by step how to create an account on GitHub. 
Then we see how to create our first repository called `test`, and transfer it from GitHub onto our local machine using the `git clone` command.

###Creating a file, pushing to GitHub, and pulling from GitHub

<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/1Hn0UbiyMb0?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

How do we create files and upload them to GitHub?
The `touch <filename>` command will create an empty file for you.
The `vim <filename>` command will open a file in an advanced text editor that [we talk about farther down the page](#vim).
The `git push` command sends these files from your local machine up to GitHub, and the `git pull` command downloads files from GitHub and saves them to your local computer.

###Branches
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/E8-hUsR7IXA?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

We've all been in the position before. You've been coding up a program where most of it's files are ready to be published to the world while some may not.
Thanks to GitHub, you are able to create branches off of your program that will enable you to work around your bugs and eventually merge the correct fixes to your main program.
And since every repository has a branch named `master`, lets say your working on an app and theres one file in your app named count.cpp that doesn't count properly. 
Thankfully, you can create a `branch` named count and work on the count file and any other file without messing up your original code. 
This way you can mess around with any of your files till your count.cpp file works. 
After which, you can get back to your `master` branch and merge your two branches together. 
In the end, you completed your program, but safely worked out your bugs without messing up your whole program. 
In other terms, this is called version control.


###Tags
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/WKG1u4Y_f3s?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

Most programs have different versions, for example: 1.0, 1.1, 1.2, 2.1 and 2.2.1.
The `git tag` command let's you create these versions. 
They're just like a checkpoint in a Mario game!


###Forking & Pull Requests
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/tTnL84EvJTM?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

Let's say you want to contribute to an open source project.
You can `git clone` it like we've already discussed, but you can't run `git push` to add your changes.
You don't have permission.
In order to contribute to someone else's repository, you must first "fork" it to create a repo that you do have `push` permission on.
Then you issue a pull request through the GitHub website.
This tells the owner of the original repo that you've made some changes they can incorporate.
This is the basic procedure you follow any time you contribute to an open source project.

###The `README.md` file
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/4UTSEKzsSvM?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

`README.md` files are how you document your projects.
The `README.md` should explain your program, give installation instructions, and list known bugs.
Basically, it explains to someone else who has absolutely no idea what your program does or how to code, but it enables the user to understand the concepts and basic directions to execute your program.
The `.md` extension at the end of the filename indicates that the file uses markdown formatting.
This is a simple way to create nice looking documentation.

## Learning vim

`vim` is an advanced text editor for Unix operating systems.
It's powerful, but all the commands are intimidating for first time users.
At first, you'll think that using a more familiar editor would make coding more efficient.
But these videos will help you learn some of the basic commands and get comfortable with `vim`.
I'm still no master at using `vim`, but I'm already enjoying it a lot.

###Getting Started
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/szTtE60fIt8?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

It was difficult at first trying to transverse my code while using `vim`. I was so used to being able to use my mouse and simply click where I wanted to go.
I was now at the mercy of moving my cursor one line at a time to go anywhere until I learned more about the many helpful tools `vim` already had just for traveling through code.
Some are simple such as `gg` to go to the top of the code, `G` to go to the bottom of it, and `(any number)G` to go to the line number typed before the capital G.)

Some ways of moving through the text are a little more complicated but easy once a user gets used to them.
Any line of text can be searched if something specific is being searched for the text’s line is unknown.
This is done by using the command `/` and then typing what the user wants to search.
This will highlight the searched text wherever it appears in the code and place the cursor in the first highlighted area that it can find after the cursor’s original position. There are many ways to transverse these highlighted characters, but the one I found most helpful was typing `n` for it will take you to the next highlighted word no matter where you currently are.

Cutting, copying, and pasting took a while to get used to when using `vim`.
Sometimes there was something I wanted in my code that was in the instructions for the assignment.
I knew that in order to paste I would use the `p` command, but I could not paste things from outside of `vim` into it.
I solved this with one of the few times I could use my mouse in `vim`.
If I had something copied outside of `vim`, then to paste it into `vim` I would right click and just click paste.
This would paste it wherever the cursor currently is.
This can also be used to copy from `vim`.
If you right click to copy, then it will not affect what is copied by using the commands `y` to copy or the commands `d` or `x` to cut.
If those commands are used, the just clicking `p` will paste them.
There are other ways to store more than one thing while copying or cutting, but these two ways were the most helpful as I learned how to use `vim`.

There are many ways to maneuver inside of `vim`. 
Some may just use the `h,j,k,l`, `up, down, left, right arrow keys`, or the `w, e, b` keys to move. 
Luckily, `vim` has two special keys to get you editing much easier. 
The two, which are my favorite features of `vim`, are the `shift-a` (takes you to the end of the line and into insert mode) and the `shift-i` (takes you to the beginning of the line and into insert mode) command.
You can also press `a` to append after the cursor position, as well as `i` to insert before the current cursor position

###Indentation
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/uuztdE_gixs?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

At first it felt very time consuming to indent multiple lines.
I would go to the beginning of each line and after using `i` and type tab.
With so many commands in `vim`, I was surprised that there was no command to just indent a few lines.
I felt this way until I found about the `V` command.
`V` lets users highlight a line and pressing up or down can highlight as many lines as they desire.
All that was left to do was to type `>` after everything I wanted to indent was highlighted and it all would indented once to the right.
Typing `<` would instead indent it to the left if I ever wanted to do that.
This greatly improved my usage of `vim` for `V`, or `v` if users wanted to highlight characters instead of entire lines, was could be combined with many commands and not just indenting.

###Deletion
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/G2ppzuNpr98?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

Just like indenting, I thought that I could only delete one character at a time when I started using `vim`.
This would become bothersome if I wanted to fix a big part of my code that I knew needed to be deleted.
There are plenty of different ways that I learned to speed this up.
The most helpful ways were pressing `x` after highlighting what I wanted deleted, and using the `d` command.
`d` can be used with many different things after it. `dd` will delete the entire line.
`d$` will delete the rest of the current line.
`dw` will delete the rest of the current word.
These commands made writing code in `vim` go by much faster.

###Replacing
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/TPbAdONJHyA?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

Similar to using `d` to get rid of code, I learned a few ways to replace what was already typed. 
Lower case `r` can replace one letter while upper case `R` can replace one letter with many. 
Just type `r` or `R` and what you want to replace it with. 
Some commands allow the user to replace multiple characters with as many as the user wants to using the `c` command. 
A few of these that I found most helpful were cw to replace the rest of the current word, `cb` replaces the characters before the cursor on the current word, `c$` replaces the rest of the current line, and `c0` replaces the entire line before the cursor.

###Customizing your vim editor with the .vimrc file
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/VhAiVux6GBg?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

Ever wondered how've we get our `vim` editor to work in the way we have it versus the default editor? 
`vim` has a file where you can setup it's defaults such as auto parentheses, auto-indent, and much more. 
By watching our video above, you can easily create new defaults for your `vim` editor that can cut time spent formating your text to spend more on coding.

##Learning The Terminal

One of the best features of Unix operating systems is the powerful terminal they provide.

###The `ls` command
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/xSSahh5HbUY?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

The `ls` command is one of the most used terminal commands. 
I would say that there isn't a time that I don't use `ls` when using the terminal. 
Proper knowledge of the `ls` command and the useful flags for `ls` helps to streamline the programming process. 

Passing in a directory name will display the contents of that directory. 
It is also possible to pass in a path for a directory to display any directory, regardless of the directory the user is currently in.
The basic `ls` command, when run, displays the contents within the current working directory.

If the `-a` flag is passed in with `ls`, all items in the current working directory prepended with a `.` are also displayed, along with the rest of the items.
Passing in the `-l` flag prints information for each item in the directory in a series of columns on a single line. 
The first column displays the read, write, and executable permissions for the main user, the group the current user is in, and any user in that order. 
The next column shows the owner of the item and the next column shows the group owner. 
The fourth column displays the size, in bytes, of the item. 
The fifth column displays the moment the item was created, and the last column displays the name of the item.
If the `-R` flag is passed in, the command will display the contents of the current directory, and then recursively enter every directory within the current directory and display the contents of that directory, then keep going into every directory until there are no more directories in the current directory it is in.

All these options are combinable for different uses. 
For example, I could use the `-l` and `-a` flags to display the information for the items prepended with a `.` , or use `-R` and `-l` together.

###The `cd` and `mv` commands
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/1s5TiFbETh4?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>
 
The `cd` and `mv` commands are crucial commands in order to actually use the terminal.
Without cd, I would forever be stuck in their home directory.
The `mv` command is necessary for moving files from one section of the hard drive.
They're really simple commands, but without them, there'd be no way to organize contents within a hard drive.
The `cd` command by itself will change the current working directory to the home directory.
If passed a directory name that is within the current working directory, the current working directory will be changed to the name of the passed in directory.
`cd` will also take a path as an argument. When a path is passed in, the current working directory will be changed to the directory specified by the path.
When `cd` is passed with `..`, the directory will go backwards, the directory that the current directory is in.
The `mv` command will move an item within a certain directory to the directory passed in.
 
If the destination argument is not a path, the command will look for the destination in the current working directory.
The destination argument can be a path, so I can move the item to any directory in the hard drive.



###Recording terminal sessions via scripts

<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/ZnIrku27C94?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

Have you ever wondered how you could possibly record data output from a program, or how you could record a list of commands within your terminal to share it with your colleague or professors in the future? 
With the `script` command, all of this is possible. 
By just typing `script file_name_here`, you can start a script. 
Also, you don't need to worry about making a file beforehand, because when you specify the filename, it will make once for you in that name. 
Once you've gotten your script started, anything and everything you do within that terminal window, will be recorded as text within the document. 
Then when you're done, type `exit` and your terminal will say your script session has ended and re-state the filename in which it recorded all your commands in. 
You can then share or upload your script to show the any bugs or amazing new programs you've gotten to make.

###How To SSH (into well server)
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/Letf4txWPic?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

For the CS100 class, working within UCR's well server is crucial to succeed. 
The version of linux that the school's well server uses is different than other linux builds. 
Because of this, compilers will work differently in the well server than other local bash terminals, giving error messages after compilation on local bash terminals when compilation on the school servers will not, or compiling fine locally but failing to compile when compiled on the well server. 
Therefore, it is necessary to `ssh` into the well server and compile and run any code before submission. 
To `ssh` into the server, write in the command line `ssh`, followed by your net ID, and append `@bell.cs.ucr.edu`. 
You will be prompted for the password associated with the net ID (the password you use when signing into the lab computers), then placed in the home directory. 
To exit out of the server, type into the command line `exit`. 
You will exit the server and be placed back into your local bash. 
Any change saved to the server will be saved, so if you exit and re-enter the server, the changes will remain. 
If it works on well, then it will work when it is graded. It's not necessary to write the code in the school's server, but I would recommend doing all writing in the well server so that it is faster to debug and test, as opposed to writing locally, copying the file, saving it to your personal server, then compiling. I know for myself, when working on my first assignment for CS100, I wrote and compiled all my on my local machine and then submitted the final version without first checking if it compiled and ran on the school's server. Sure enough, I received a zero for inability to compile. From then on, I never wrote any code on my local terminal, doing all work on the school's server. I then checked my final version after modifying it to work on the school's server, and it didn't compile in my own bash terminal. After that, I realized there was no point in writing code on my local terminal, as it was easier to ssh into the school server and do all my work in there. It cloud-saves all your work, allowing you to access it almost anywhere. It's a really useful tool while still a student here at UCR.

###Spectacle App: Using the terminal and `vim` in one screen
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/j1fnYZp4foI?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

One of the first things I noticed about `vim` that I initially disliked was that it took over the terminal when I used it.
I thought that I would have to close `vim` each time I wanted to test if my program could run correctly.
To get around this issue, I started using two terminals instead of just one while I was programming.
I would run `vim` using the first terminal and would run the executable in the second.
It was as simple as using `:w` to save on `vim` instead of using `:wq` to save and quit.
I could now test my code without ever having to close `vim`.

####PERROR
<iframe width="697.6" height="419.65" src="//www.youtube.com/embed/GsoVzP3sRsA?rel=0&vq=hd1080" frameborder="0" allowfullscreen></iframe>

When programming for unix based operating systems (which is a primary component of CS100), system calls are a prominent component for code. 
When coding, error checking is always necessary. 
System calls make error checking very simple and very easy to implement with `perror`. 
Every unix system call returns an error value if the call fails. 
The `perror` function captures this error value (if returned) and prints to stdout an error message based on the system call and the type of error. 
It takes in one c-string argument, which is a message the user can pass in. 

When using system calls, it is absolutely necessary to implement `perror` with every system call. 
Any program is not complete without `perror`.
Without it, bugs will be exponentially harder to find and fix and will significantly stifle programming speed.
Also, if you're like me and essentially did not use any `perror` functions whatsoever, your grade will suffer.
It's really simple to implement, essentially adding only a line or two per system call, but the benefits are enormous.

