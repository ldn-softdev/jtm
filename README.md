# jtm

HTML to JSON converter

This is a quick and trivial HTML to JSON lossless convertor:
- HTML tags semantic unaware - convertor does not keep track or different tags meanings,
 except few tag:
  * separately parses `<script>` tag, which requires no tag value interpolation
  * separately parses `<!...>` tags, which do not not contain attributes
  * empty tags (those which do not have a closing pair: either self closed, e.g:
`<img .../>`, or specially defined like `<br>`) are not tracked, instead following parsing
logic applied:
  -- if currently parsed tag's value is getting closed by another tag, it means that the tag
being parsed is an empty tag

 Conversion rules:
 - each tag is translated into a JSON object, with a single label - name of the tag
 - all attributes go into object pairs *`label:value`*, where:
    * *`label`* is the attribute name
    * *`value`* is the attribute value
 - a value of the tag (i.e. everything between opening and closing tag) is going under the label 
`~value` (a default label, could be changed by user)
 - empty tags w/o attributes will be set to JSON `null` value
