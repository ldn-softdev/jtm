# jtm - HTML to JSON converter

#### A simple tool offering quick lossless HTML to JSON conversion

the tool offers following behaviors:
- HTML tags semantic unaware - convertor does not keep track or different tags meanings,
 except few tag:
  * separately parses `<script>` tag, which requires no tag value interpolation
  * separately parses `<!...>` tags, which do not not contain attributes
  * empty tags (those which do not have a closing pair: either self closed, e.g:
`<img .../>`, or specially defined like `<br>`) are not tracked, instead following parsing
logic applied:
    - if currently parsed tag's value is getting closed by another tag, it means that the tag
being parsed is an empty tag


#### Conversion rules:

 - each tag is translated into a JSON object, with a single label - name of the tag
 - all attributes go into object pairs *`label:value`*, where:
    * *`label`* is the attribute name
    * *`value`* is the attribute value
 - a value of the tag (i.e. everything between opening and closing tag) is going under the
 label `~value` (a default label, could be changed by user)
 - empty tags w/o attributes will be set to JSON `null` value


#### A following sample illustrates HTML to JSON conversion rules:

- source HTML sample:
```
<!DOCTYPE html>
<html>
   <head>
       <title>HTML example</title>
       <meta charset="utf-8">
   </head>
   <body text="green">
       <p>
           Oh Brother,<br>
           Where Art Thou?<br>
       </p>
   </body>
</html>
```
- is converted into JSON:
```
[
   {
      "!": "DOCTYPE html"
   },
   {
      "html": {
         "~value": [
            {
               "head": {
                  "~value": [
                     {
                        "title": {
                           "~value": "HTML example"
                        }
                     },
                     {
                        "meta": {
                           "charset": "utf-8"
                        }
                     }
                  ]
               }
            },
            {
               "body": {
                  "text": "green",
                  "~value": {
                     "p": {
                        "~value": [
                           "Oh Brother,",
                           {
                              "br": null
                           },
                           "Where Art Thou?",
                           {
                              "br": null
                           }
                        ]
                     }
                  }
               }
            }
         ]
      }
   }
]
```

#### Linux and MacOS precompiled binaries are available for download

For compiling c++14 (or later) is required:
  - to compile under macOS, use cli: `c++ -o jtm -Wall -std=c++14 -Ofast jtm.cpp`
  - To compile under Linux, use cli: `c++ -o jtm -Wall -std=gnu++14 -static -Ofast jtm.cpp`

or download latest precompiled binary:
- [macOS 64 bit](https://github.com/ldn-softdev/jtm/raw/master/jtm-macos-64.v1.01)
- [macOS 32 bit](https://github.com/ldn-softdev/jtm/raw/master/jtm-macos-32.v1.01)
- [linux 64 bit](https://github.com/ldn-softdev/jtm/raw/master/jtm-linux-64.v1.01)
- [linux 32 bit](https://github.com/ldn-softdev/jtm/raw/master/jtm-linux-32.v1.01)


#### Compile and install instructions:

download `jtm-master.zip`, unzip it, descend into unzipped folder, compile using
an appropriate command, move compiled file into an install location.

here's the example steps (for macOS):
  - say, `jtm-master.zip` has been downloaded to a folder and the terminal app is open in that
folder:
  - `unzip jtm-master.zip`
  - `cd jtm-master`
  - `c++ -o jtm -Wall -std=c++14 -Ofast jtm.cpp`
  - `sudo mv ./jtm /usr/local/bin/`

#### help screen:
```
bash $ jtm -h
usage: jtm [-d] [-h] [-n] [-r] [-s] [-t] [-e label] [-v label] [html_src]

HTML to JSON lossless convertor. Version 1.01, developed by Dmitry Lyssenko (ldn.softdev@gmail.com)

optional arguments:
 -d             turn on debugs (multiple calls increase verbosity)
 -h             help screen
 -n             start enlisting tag values from the first entry
 -r             force printing json in a raw format
 -s             enforce quoted solidus behavior
 -t             do not retry parsing upon facing a closing tag w/o pair
 -e label       label used for extra text in tags (i.e. non-attributes) [default: ~extra]
 -v label       label used for tag values [default: ~value]

standalone arguments:
  html_src      file to read html from [default: <stdin>]

the tool is html tag agnostic, though provides separate behaviors:
 - understand and parse tag <!...>
 - parse tag attributes
 - <script> tag value is not interpolated

bash $ 
```



