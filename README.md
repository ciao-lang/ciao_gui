# Launcher of Ciao GUI applications

This bundle provides a launcher of cross platform GUI desktop
applications for Ciao based on HTML/CSS/Javascript technology.

It supports the following backends:

 - The [Electron](https://electron.atom.io/) framework (itself based
   on [Node](https://nodejs.org),
   [Chromium](http://www.chromium.org/Home), and
   [V8](https://developers.google.com/v8/))

 - Any modern available Web browser (as fallback)

## Build
    
This code depends on some third-party tools and libraries. You can use
this script to install them:
```	
$ ciao custom_run . fetch_externals
```
Then you can build the bundle with:
```	
$ ciao build
```

## Usage

Execute with `ciao-gui <Cmd>`. Use the `--server` option to start in
server mode (needs external browser).

See also the `ciao-serve` command for serving Web applications.
