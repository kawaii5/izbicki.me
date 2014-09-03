# izbicki.me

This is the github repo for my hakyll-based website http://izbicki.me.  

If you are submitting a post, first fork the repo by pressing the fork button in the upper right corner.

Then clone the fork; then checkout the `draft` branch; then create a branch just for your post:

```
$ git clone https://github.com/yourusername/izbicki.me
$ git checkout draft
$ git checkout -b myposttitle
```

Next, create a file in the `/blog` folder for your post.  For example, if you're writing about alien kittens living on mars, you might create a file called `/blog/alien-kittens-on-mars.md`.  The file should be in the same markdown format that github uses.  It will automatically be converted to html.

To preview your work, you'll need to:

1.  Install the [haskell platform](http://www.haskell.org/platform/)
2.  Install the hakyll library using the command `cabal install hakyll`
3.  Run the preview script in the repo: `./preview.sh`

This will compile your markdown file into html and start a simple webserver hosting your file.  To see what your file looks like, visit http://localhost:8000.  You do not have to redo this command every time you update your file.  The webserver will automatically detect that the file has changed and display the new file.

If you want to include images on the page, copy the image into the `/img` folder of the repo.  The url you should use for the image is `/img/nameofimage.png`.

When finished, upload your results to github:

```
$ git add .
$ git commit -m "my post"
$ git push origin myposttitle
```

Then issue a pull request through the github interface.  When I accept your request, the webpage will be automatically updated.
