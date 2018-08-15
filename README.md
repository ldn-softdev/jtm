# jtm - HTML/XML to JSON converter

#### A simple tool offering quick lossless HTML/XML to JSON conversion

- version 2.x offers an improved JSON layout (compared to v1.x), so it's easier to parse 
a resulting JSON
- command interface switches also changed (to reflect action semantic better)

the tool offers following behaviors:
- HTML/XML tags semantic unaware - convertor does not keep track or tag meaning,
 however provides following behaviors:
  * separately parses `<!...>` tags, which do not not contain attributes
  * separately parses `<?...>` tags and their attributes
  * separately parses `<script>` tag, which requires no tag value interpolation
  * empty tags (those which do not have a closing pair: either self closed, e.g:
`<img .../>`, or specially defined like `<br>`) are not tracked, instead following parsing
logic applied:
    - if currently parsed tag's value is getting closed by another tag, it means that the tag
being parsed is an empty tag


#### Conversion rules:

 - each tag is translated into a JSON object, with a single label - name of the tag
 - all attributes go into object with the label  *`attributes`* (a default label, could
 be changed by user)
 - a value of the tag (i.e. everything between opening and closing tags) is merged into the
 tag label
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
      "html": [
         {
            "head": [
               {
                  "title": "HTML example"
               },
               {
                  "meta": {
                     "attributes": {
                        "charset": "utf-8"
                     }
                  }
               }
            ]
         },
         {
            "body": [
               {
                  "attributes": {
                     "text": "green"
                  }
               },
               {
                  "p": [
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
            ]
         }
      ]
   }
]
```

#### Linux and MacOS precompiled binaries are available for download

For compiling c++14 (or later) is required:
  - to compile under macOS, use cli: `c++ -o jtm -Wall -std=c++14 -Ofast jtm.cpp`
  - To compile under Linux, use cli: `c++ -o jtm -Wall -std=gnu++14 -static -Ofast jtm.cpp`

or download latest precompiled binary:
- [macOS 64 bit](https://github.com/ldn-softdev/jtm/raw/master/jtm-macos-64.v2.01)
- [macOS 32 bit](https://github.com/ldn-softdev/jtm/raw/master/jtm-macos-32.v2.01)
- [linux 64 bit](https://github.com/ldn-softdev/jtm/raw/master/jtm-linux-64.v2.01)
- [linux 32 bit](https://github.com/ldn-softdev/jtm/raw/master/jtm-linux-32.v2.01)


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
usage: jtm [-d] [-e] [-h] [-n] [-r] [-s] [-a label] [-t label] [html_src]

HTML/XML to JSON lossless convertor. Version 2.01, developed by Dmitry Lyssenko (ldn.softdev@gmail.com)

optional arguments:
 -d             turn on debugs (multiple calls increase verbosity)
 -e             start enlisting tag values from the first entry
 -h             help screen
 -n             do not retry parsing upon facing a closing tag w/o its pair
 -r             force printing json in a raw format
 -s             enforce quoted solidus behavior
 -a label       a label used for attribute values [default: attributes]
 -t label       a label used for trailing text inside tags [default: trailing]

standalone arguments:
  html_src      file to read html from [default: <stdin>]

the tool is html tag semantic agnostic, though provides isolated parsing for:
 - parsing of tag attributes
 - understand and parse tag <!...> w/o parsing attributes
 - understand and parse tag <?...> with parsing attributes
 - <script> tag value is not interpolated

bash $ 
```

-  Once HTML/XML is converted to JSON, use [jtc](https://github.com/ldn-softdev/jtc) 
tool to extract / manipulated JSON data


