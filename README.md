# jtm - lossless HTML/XML to JSON and back converter

#### A simple tool offering quick lossless HTML/XML to JSON conversion

- version 2.x offers an improved JSON layout (compared to v1.x), so it's easier to parse 
a resulting JSON
- command interface switches also changed (to reflect action semantic better)

the tool offers following behaviors:
- HTML/XML tags semantic unaware - convertor does not keep track or tag meaning, see conversion specification below
- converted JSON is possible to reistate back to its original format XML/HTML (thanks to lossless conversion)
- detects malformed HTML/XML (i.e. closed tags w/o corresponding openning) and automatically fixing it (optionally could be disabled)


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
- and recovered from JSON back to HTML:
```
<!DOCTYPE html>
<html>
   <head>
      <title>HTML example</title>
      <meta charset="utf-8">
   </head>
   <body text="green">
      <p>Oh Brother,<br>Where Art Thou?<br>
      </p>
   </body>
</html>
```

#### Linux and MacOS precompiled binaries are available for download

For compiling c++14 (or later) is required:
  - to compile under macOS, use cli: `c++ -o jtm -Wall -std=c++14 -Ofast jtm.cpp`
  - To compile under Linux, use cli: `c++ -o jtm -Wall -std=gnu++14 -static -Ofast jtm.cpp`

or download latest precompiled binary:
- [macOS 64 bit](https://github.com/ldn-softdev/jtm/raw/master/jtm-macos-64.v2.06)
- [macOS 32 bit](https://github.com/ldn-softdev/jtm/raw/master/jtm-macos-32.v2.06)
- [linux 64 bit](https://github.com/ldn-softdev/jtm/raw/master/jtm-linux-64.v2.06)
- [linux 32 bit](https://github.com/ldn-softdev/jtm/raw/master/jtm-linux-32.v2.06)


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
usage: jtm [-defhnrs] [-a label] [-i indent] [src_file]

HTML/XML to JSON and back lossless convertor.
Version 2.06, developed by Dmitry Lyssenko (ldn.softdev@gmail.com)

optional arguments:
 -d             turn on debugs (multiple calls increase verbosity)
 -e             enlist even single values (otherwise don't)
 -f             digitize all numerical strings
 -h             help screen
 -n             do not retry parsing upon facing a closing tag w/o its pair
 -r             force printing json in a raw format
 -s             enforce quoted solidus behavior
 -a label       a label used for attribute values [default: attributes]
 -i indent      indent for pretty printing [default: 3]

standalone arguments:
  src_file      file to read source from [default: <stdin>]

the tool is html/xml tag semantic agnostic, follows conversion specification:
  <tag> </tag>                <-> { "tag": [] }
  <tag> ... </tag>            <-> { "tag": [ <...> ] }
  <tag attributes> </tag>     <-> { "tag": [ { <attributes> } ] }
  <tag attributes> ... </tag> <-> { "tag": [ { <attributes> }, <...> ] }
  <self_closed attributes />  <-> { "self_closed/": { <attributes> } }
  <self_closed/>              <-> { "self_closed/": null }
  <empty_tag attributes>      <-> { "empty_tag": { <attributes> } }
  <empty_tag>                 <-> { "empty_tag": null }
  <!...>                      <-> { "!": <...> }
  <?tag attributes>           <-> { "?tag": { <attributes> } }
  <?tag>                      <-> { "?tag": null }
if a tag enlists a single value then optionally it could be de-listed (default
behavior), unless the value is "attributes" - then no delisting occurs

bash $ 
```

-  Once HTML/XML is converted to JSON, use [jtc](https://github.com/ldn-softdev/jtc) 
tool to extract / manipulated JSON data


Here's a trivial example how use them together.
Say, we want to remove from the original html document all specific tags (and their content respecitvely)? 
Let it be tag `<p>` in the above html sample. A simple way to do it would be like this:
```
bash $ jtm sample.html | jtc -w '<p>l+0[-1]' -p | jtm
<!DOCTYPE html>
<html>
   <head>
      <title>HTML example</title>
      <meta charset="utf-8">
   </head>
   <body text="green">
   </body>
</html>
bahs $ 
```

As easy as a pie!

 

