# twentyfour

Calculator for the 24 game.

## Setup

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL. 

Note: the production build is handled by a GitHub Action and automatically
deployed to GitHub Pages, so there is no need to run the production build
normally.

For repl.it usage:

If the `Run` button isn't producing an app, try running:

  `busybox restart`

To restart the entire repl.it.