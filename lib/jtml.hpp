/*
 * Created by Dmitry Lyssenko, last modified August 9, 2018.
 *
 * html/xml tags agnostic, lossless html/xml to json converter with a trivial user interface
 *
 * This class does not keep track of HTML list for empty tags (some are and some
 * are not), instead it relies on a parsing rule:
 *  - if tag's value is getting closed by another tag, it means that my tag (if
 *    not marked explicitly) is an empty tag (i.e. a tag w/o value)
 *
 * This parser though understands and parses properly:
 *  - attributes within tags,
 *  - parses separately tags starting with <!... (not parsing content)
 *  - parses separately tags starting with <?... (parses attributes)
 *  - value of <script> tags is not interpolated (i.e not parsed)
 *
 * Conversion rule:
 *  - each tag is translated into a JSON object, with a single label - name of the
 *    tag and array for all tag's values
 *  - all attributes go into object with the reserved label "attributes" (default
 *    label name, could be changed by user)
 *  - values of the tag (i.e. everything between open tag and closing tag) are listed
 *    in the array of the tag's object
 *  - empty tags (those w/o attributes and values) will be set to  JSON null value
 *
 * This simple code sample illustrates the above behavior:
 *
 *   string html = R"(
 *   <!DOCTYPE html>
 *   <html>
 *      <head>
 *          <title>HTML example</title>
 *          <meta charset="utf-8">
 *      </head>
 *      <body text="green">
 *          <p>
 *              Oh Brother,<br>
 *              Where Art Thou?<br>
 *          </p>
 *      </body>
 *   </html>)";
 *
 *   Jtml cv;
 *   cout << cv.jsonize(html) << endl;
 *
 *  Output:
 *  [
 *     {
 *        "!": "DOCTYPE html"
 *     },
 *     {
 *        "html": [
 *           {
 *              "head": [
 *                 {
 *                    "title": "HTML example"
 *                 },
 *                 {
 *                    "meta": {
 *                       "attributes": {
 *                          "charset": "utf-8"
 *                       }
 *                    }
 *                 }
 *              ]
 *           },
 *           {
 *              "body": [
 *                 {
 *                    "attributes": {
 *                       "text": "green"
 *                    }
 *                 },
 *                 {
 *                    "p": [
 *                       "Oh Brother,",
 *                       {
 *                          "br": null
 *                       },
 *                       "Where Art Thou?",
 *                       {
 *                          "br": null
 *                       }
 *                    ]
 *                 }
 *              ]
 *           }
 *        ]
 *     }
 *  ]
 *
 *
 *
 * Tag values interpolation logic:
 * 1. extract tag-name
 * 2. tag is self-closed?
 *    - yes: add children - attributes (if any), return resulting JSON object
 * 3. parse value, facing a new tag?
 *    - merge parsed so far content 
 *    - closing tag == my tag? return JSON object.
 *    - closing tag != my tag?  // means my tag is an empty tag
 *          yes: return [ { mytag: parsed_my_tag_attr }, parsed_value, closing_tag ]
 *          no: // it's another open tag
 *              parse new tag
 *              - returned object? // if no, then it's an array
 *                  yes: merge obj, keep parsing
 *                  otherwise: 
 *                      last value of array == mytag? // my tag being closed?
 *                      - yes: pop last value from array,
 *                             merge array with content, return JSON object
 *                      - no: //array != mytag => my tag is an empty tag
 *                            make [ {mytag: parsed_my_tag_w/o_value}, my_parsed value ],
 *                            merge my array with returned array, return Json array
 *
 */


#pragma once

#include <string.h>
#include <string>
#include <vector>
#include <set>
#include <iomanip>
#include <sstream>
#include "enums.hpp"
#include "Json.hpp"
#include "dbg.hpp"



#define DBG_WIDTH 80
#define CHR_RTRN '\n'
#define TAG_ATTRB_LBL "attributes"                              // default json label for attributes
#define TAG_TRAIL_LBL "trailing"                                // label for values w/o attribute

#define JSN_QUOTE "\b\f\n\r\t\"\\"                              // chars requiring quotation in JSON
#define JSN_QUOTED "bfnrt\"\\"
// NOTE: strict JSON behavior requires quoting solidus char '/', however it seems a common
//       behavior is to ignore it. A user will have an option to switch between behaviors,
//       defaulting to *ignore* option





class Jtml {
 public:
    #define THROWREASON \
                expect_tag_opening, \
                white_space_before_tag_name, \
                unexpected_closing_tag, \
                premature_end_of_file, \
                empty_self_closed_not_allowed, \
                unexpected_input_after_self_closing, \
                empty_assignment_in_attributes, \
                unexpected_empty_tag_with_attributes, \
                unexpected_json_type, \
                end_of_trhow
    ENUMSTR(ThrowReason, THROWREASON)

    #define ATTRPROP \
                self_closed, \
                open
    ENUM(AttrProperty, ATTRPROP)


    std::string         quote_str(std::string && src) const;
    std::string         quote_str(const std::string & str) const
                         { std::string src{str}; return quote_str(std::move(src)); }
    std::string         unquote_str(std::string && src) const;
    std::string         unquote_str(const std::string & str) const
                         { std::string src{str}; return unquote_str(std::move(src)); }

    Json &              jsonize(const std::string &html);
    Json &              json(void) { return json_; };

    Jtml &              add_ws(const std::string &ws)
                         { ws_.push_back(ws); return *this; }
    template<typename... Args>
    Jtml &              add_ws(const std::string &ws, const Args &... rest)
                         { ws_.push_back(ws); return add_ws(rest...); }
    Jtml &              reset_ws(void)
                         { ws_.clear(); add_ws(" ", "\t", "\n", "\r" ); return *this; }

    Jtml &              attr_label(const std::string &lbl) { al_ = lbl; return *this; }
    const std::string & attr_label(void) const { return al_; }
    Jtml &              trail_label(const std::string &lbl) { tl_ = lbl; return *this; }
    const std::string & trail_label(void) const { return tl_; }

    Jtml &              enumerate(bool x) { ae_ = x; return *this; }
    bool                enumerate(void) const { return ae_; }

    Jtml &              retry(bool x) { rt_ = x; return *this; }
    bool                retry(void) const { return rt_; }

    Jtml &              quoted_solidus(bool x) {
                         if(x) { jsn_quote_=JSN_QUOTE"/"; jsn_quoted_ = JSN_QUOTED"/"; }
                         else { jsn_quote_=JSN_QUOTE; jsn_quoted_ = JSN_QUOTED; }
                         return *this;
                        }

 protected:
    typedef std::string::const_iterator const_sit;

    Json                json_;                                  // parsed html will go here
    std::vector<std::string>
                        ws_{" ", "\t", "\n", "\r"};             // white space definitions
    std::string         al_{TAG_ATTRB_LBL};                     // attribute label
    std::string         tl_{TAG_TRAIL_LBL};                     // label for extras in tag
    bool                ae_{false};                             // always arrange as arrays flag
    std::set<const_sit> itl_;                                   // ignored tag locations
    bool                rt_{true};                              // retry parsing
  std::set<std::string> npt_{"script"};                         // do not interpolate tags

    Json                jsonize_(const_sit & si) const;
    Json                parseTag_(const_sit & si) const;
    const_sit &         findClosingTag_(const std::string & tag, const_sit & si) const;
    AttrProperty        parseAttributes_(Json &, std::string &&attr) const;
    std::string         parseAttributeValue_(const_sit & si) const;
    void                mergeContent_(Json &, Json && ) const;
    std::string         extractTag_(const_sit & si, const_sit & end_it) const;
    void                parseContent_(Json &j, const_sit & si) const;
    Jnode::Jtype        interpolateTag_(Json &j, const_sit &si, const std::string &mytag) const;
    const_sit &         parseExclamationTag(const_sit& si) const;

 private:
    const char *        jsn_quote_{JSN_QUOTE};                  // JSN_QUOTE pointer
    const char *        jsn_quoted_{JSN_QUOTED};                // JSN_QUOTED pointer

    const_sit &         findAnyOf_(const char *dlm, const_sit &si, bool throw_exp = true) const ;
    const_sit &         skipWhiteSpace_(const_sit & si) const;
    const_sit &         skipUntilWhiteSpace_(const_sit & si) const;
    int                 matchWhiteSpace_(const char *str) const;
    int                 matchWhiteSpace_(char chr) const
                         { char cs[2] {chr, '\0'}; return matchWhiteSpace_(cs); };
    std::string &       trimTrailingWhiteSpace_(std::string &) const;
    void                printParsingPoint_(const_sit & si) const;
    void                printRawJson_(const char * prompt, Json &j) const;
    Jnode &             enlist_(Jnode &) const;

    EXCEPTIONS(ThrowReason)                                     // see "enums.hpp"

 public:
    DEBUGGABLE(json_)
};

STRINGIFY(Jtml::ThrowReason, THROWREASON)
#undef THROWREASON
#undef ATTRPROP




std::string Jtml::quote_str(std::string && src) const {
 // quote source string as per JSON quotation
 std::stringstream ss;
 size_t start=0;
 for(size_t end{ src.find_first_of(jsn_quote_, start) };
     end != std::string::npos;
     end = src.find_first_of(jsn_quote_, start)) {
  char qc{ src[end] };
  src[end] = '\0';
  ss << src.data() + start;
  switch(qc) {                                                  // JSN_QUOTE: "\b\f\n\r\t\"\\"
   case '\b': ss << "\\b"; break;
   case '\f': ss << "\\f"; break;
   case '\n': ss << "\\n"; break;
   case '\r': ss << "\\r"; break;
   case '\t': ss << "\\t"; break;
   default: ss << '\\' << qc; break;
  }
  start = end + 1;
 }
 ss << src.c_str() + start;
 return ss.str();
}



std::string Jtml::unquote_str(std::string && src) const {
 // unquote source string as per JSON quotation
 std::stringstream ss;
 size_t start=0;
 for(size_t end{ src.find('\\', start) };
     end != std::string::npos;
     end = src.find('\\', start)) {
  char qc = src[end++];
  if(end >= src.size()) break;                                  // i.e. line ending "...\"
  if(strchr(jsn_quoted_, qc) == nullptr) continue;              // i.e other (non-Json) char quoted
  src[end-1] = '\0';
  ss << src.data() + start;
  switch(qc) {                                                  // JSN_QUOTED: bfnt"\/
   case 'b': ss << '\b'; break;
   case 'f': ss << '\f'; break;
   case 'n': ss << '\n'; break;
   case 'r': ss << '\r'; break;
   case 't': ss << '\t'; break;
   default: ss << qc; break;
  }
  start = end;
 }
 ss << src.c_str() + start;
 return ss.str();
}



Json & Jtml::jsonize(const std::string &html) {
 // parse input html string into json
 json_ = ARY{};
 const_sit si{ html.cbegin() };                                 // input string iterator

 while(*si != '\0') {
  json_.push_back( jsonize_(si) );                              // actually jsonize here
  if(json_.back().is_array()) {                                 // all good jsons must be objects
   std::cerr << "error: malformed document: tag </" << json_.back().back().str()
             << "> at " << std::distance(html.cbegin(), si) << " has no opening"
             << (retry()? ", re-parse ignoring the tag": "" ) << std::endl;
   if(retry()) {
    itl_.insert(si);                                            // add location to ignored
    si = html.cbegin();                                         // reinstate string iterator
    json_.clear();                                              // clean up json
    continue;                                                   // and start over
   }
  }
  while(json_.has_children() and json_.back().is_null())        // in case jsonize_() returned NUL{}
   json_.pop_back();
 }

 return json_;
}



Json Jtml::jsonize_(const_sit & si) const {
 // wrapper for parseTag - where actual jsonization happens
 // expected *si start/end: |<|... / ...>|.|..
 DBG(0) DOUT() << "begin parsing tag block" << std::endl;

 if(*skipWhiteSpace_(si) == '\0')                               // if no input, must return a valid
  return NUL{};                                                 // JSON, null serves as placeholder
 if(*si != '<') throw EXP(expect_tag_opening);
 return parseTag_(si);
}



Json Jtml::parseTag_(const_sit & si) const {
 // 1. extract tag
 // 2. tag is self-closed? - add children, return json object
 // 3. else: parse tag content (value)
 // expected *si start/end: |<|... / ...>|.|.."
 DBG(2) printParsingPoint_(si);                                 // *si: |<|...

 const_sit end_of_tag;                                          // *end_of_tag: <tag| |attr=...>
 std::string tagname{ extractTag_(si, end_of_tag) };            // *si: ...|>|
 DBG(1) DOUT() << "extracted tag: <" << tagname << ">" << std::endl;

 if(tagname == "!") {                                           // html header
  DBG(1) DOUT() << "<!> content: '" << std::string{ end_of_tag, si } << "'" << std::endl;
  return OBJ { LBL { "!", STR{ quote_str(std::string{ end_of_tag, si++ }) } } };
 }
 if(tagname.front() == '?') {                                   // xml header
  DBG(1) DOUT() << "XML prolog content: '" << std::string{ end_of_tag, si } << "'" << std::endl;
  Json j{ OBJ{ LBL { tagname, NUL{} } } };
  parseAttributes_(j, std::string{end_of_tag, si++});
  return j;
 }
 if(tagname.front() == '/')                                     // closing tag, unexpected
  throw EXP(unexpected_closing_tag);

 Json j{ OBJ{ LBL{ tagname, NUL{} } } };                        // parsed tag will go here
 if(end_of_tag != si) {                                         // there could be attributes, parse
  if(parseAttributes_(j, std::string{end_of_tag, si}) == self_closed)
   { ++si; return j; }
  // else (not self-closed, i.e. normal tag) - parse content
 }
 else                                                           // no attributes
  if(tagname.back() == '/')                                     // self-closed, w/o attr.: <br/>
    { ++si; return j; }
 if(npt_.count(tagname) == 1) {                                 // no interpolation (e.g. <script>)
  end_of_tag = ++si;                                            // *end_of_tag: ...>|.|...
  mergeContent_(j, STR{ quote_str(std::string{ end_of_tag, findClosingTag_(tagname, si)}) } );
  --si;                                                         // adjustment after findClosingTag_
 }

 // not self-closed, process tag content
 parseContent_(j, ++si);
 return j;
}



Jtml::const_sit & Jtml::findClosingTag_(const std::string & tag, const_sit & si) const {
 // find a closing pair for tag w/o interpolation
 // expected *si start/end: <tag>|.|.. / ...|<|/tag>
 const_sit sit;
 while(true) {
  sit = findAnyOf_("<", si);                                    // *si: |<|...
  if(*++sit == '/')                                             // a closing one?
   if( strncmp(&*skipWhiteSpace_(++sit), tag.c_str(), tag.size()) == 0) {   // is it my closing tag?
    sit += tag.size();
    if(*skipWhiteSpace_(sit) == '>')                            // it's a valid closure
     return si;                                                 // *si: |<|/tag>
   }
  ++si;
 }
}



Jtml::AttrProperty Jtml::parseAttributes_(Json &j, std::string && attr) const {
 // return 'close' if attribute string ends with '/', otherwise - return 'open'
 DBG(2) DOUT() << "extracting attributes from: '" << attr << "'" << std::endl;
 AttrProperty aprop{ open };                                    // attribute property state
 const_sit si = attr.cbegin();
 Json att{ OBJ{ LBL{ attr_label(), OBJ{} } } };                 // store attributes here
 auto & a = att.front();

 for(skipWhiteSpace_(si); *si != '\0'; skipWhiteSpace_(si)) {
  if(*si == '/') {
   aprop = self_closed;
   ++si;
   DBG(2) DOUT() << "tag is self-closed" << std::endl;
   continue;
  }
  if(aprop == self_closed)                                      // i.e. there should be no input
   throw EXP(unexpected_input_after_self_closing);              // after '/', like in: <".../blah">

  auto start_it = si;                                           // extract attribute name
  std::string attribute{start_it, findAnyOf_("=", si, false)};
  if(trimTrailingWhiteSpace_(attribute).empty())                // e.g. " = some_value"
   throw EXP(empty_assignment_in_attributes);

  if(*si == '\0')                                               // a trailing value w/o attributes
   { a[trail_label()] = STR{ quote_str(std::move(attribute)) }; break; }

  std::string value{ parseAttributeValue_(++si) };
  DBG(2) DOUT() << "extracted attribute '" << attribute << "' = '" << value << "'" << std::endl;
  a[attribute] = STR{ quote_str(std::move(value)) };
 }
 j.front() = std::move(enumerate()? enlist_(att): att.root());
 return aprop;
}



std::string Jtml::parseAttributeValue_(const_sit & si) const {
 // parse and return attribute value
 // expected *si start/end: =| |"...."| |, or =| |'....'| |, or =|v|alue| |
 auto start_it = skipWhiteSpace_(si);
 if(*si == '\'' or *si == '"') {
  std::string quote{1, *si};
  std::string return_str{ ++start_it, findAnyOf_(quote.c_str(), ++si) };
  ++si;
  return return_str;
 }
 return std::string{ start_it, skipUntilWhiteSpace_(si) };
}



void Jtml::parseContent_(Json &j, const_sit & si) const {
 // parse value between open/close tag pair
 // expected *si start/end: <some_tag>|.|.. / ...<[/]some_tag>|.|..
 const_sit start_it, end_it;

 while(true) {
  start_it = skipWhiteSpace_(si);
  std::string content{ start_it, findAnyOf_("<", si) };         // *si: |<|...
  DBG(1) DOUT() << "extracted content: '" << content << "'" << std::endl;
  mergeContent_(j, STR{ quote_str(trimTrailingWhiteSpace_(content)) });

  start_it = si;                                                // preserve beginning of a tag '<'
  std::string tagname{ extractTag_(si, end_it) };               // *si: ...|>|
  ++si;                                                         // *si: ...>|.|..
  if(retry() and itl_.count(si) == 1) {                         // ignore found tag
   DBG(1) DOUT() << "found a tag at ignored position </" << tagname << ">, skipping" << std::endl;
   continue;
  }
  std::string mytag{ j.begin()->label() };
  DBG(1) DOUT() << "found next tag: <" << tagname << "> (my tag: <" << mytag << ">)" << std::endl;

  if(tagname.front() == '/') {                                  // a closing tag found:
   std::string ctag{tagname, 1};
   if(mytag != ctag) enlist_(j).push_back( STR{ctag} );         // not my closing tag
   return;
  }

  si = start_it;                                                // it's another tag, interpolate it
  DBG(1) DOUT() << "parsing nested tag <" << tagname << ">" << std::endl;
  if(interpolateTag_(j, si, mytag) == Jnode::Array) return;
 }
}



Jnode::Jtype Jtml::interpolateTag_(Json &j, const_sit &si, const std::string &mytag) const {
 // return Object or Array after interpolation is done
 // expected *si start/end: |<|sub_tag>... / ...<[/]some_tag>|.|..
 Json subj{ parseTag_(si) };
 if(subj.is_null()) return Jnode::Object;                       // should never be the case though
 if(subj.is_object())                                           // parsed normally - closed tag
   { mergeContent_(j, std::move(subj)); return Jnode::Object; }

 if(not subj.is_array()) throw EXP(unexpected_json_type);      // now expecting only array type

 std::string return_tag{subj.back().str()};
 subj.pop_back();                                               // yank returned tag from array
 DBG(2)
  DOUT() << "nested closing tag: </" << return_tag << "> (my tag: <" << mytag << ">)" << std::endl;
 if(return_tag == mytag)                                        // my closing tag -> merge array
  { mergeContent_(j, std::move(subj)); return Jnode::Array; }   // indicate no further processing

 // return_tag != mytag: j must be converted into an array
 Json jcnt{ NUL{} };                                            // to store content here
 if(not j[mytag].is_null() and not j[mytag].is_array())         // i.e. content is a single value
  enlist_(j[mytag]);
 if(j[mytag].is_array()) {                                      // value / attribute are present
  std::swap(jcnt.root(), j[mytag]);                             // move value out of json
  if(jcnt.front().is_object() and jcnt.front().count(attr_label()) == 1) {  // value has attributes
   j[mytag] = std::move( enumerate()? enlist_(jcnt.front()): jcnt.front() );
   if(jcnt.pop_front().empty()) jcnt = NUL{};
  }
 }
 enlist_(j);
 if(jcnt.is_array())                                            // i.e. if value was actually moved
  for(auto & jcr: jcnt)
   j.push_back( std::move(jcr) );
 for(auto &jnr: subj) j.push_back(std::move(jnr));              // merge subj
 j.push_back( STR{return_tag} );                                // add return tag
 return Jnode::Array;
}




void Jtml::mergeContent_(Json &json, Json && jn) const {
 // merge second arg (jn) into first arg (json) value
 if(jn.is_string())
  if(jn.str().empty()) return;                                  // nothing to add, (empty string)

 auto & j = json.front();                                       // value of the tag
 DBG(4) printRawJson_("content being added: ", jn);
 DBG(4) printRawJson_("json before merging: ", json);
 while(true) {
  if(j.is_null())                                               // first time content being added
   { j = std::move( enumerate()? enlist_(jn): jn.root()); break; }
  if(not j.is_array())                                          // content being added second time
   enlist_(j);
  if(not jn.is_array())                                         // content is not array type
   j.push_back(std::move(jn));
  else                                                          // otherwise - merge into array
   for(auto &jnr: jn)
    j.push_back(std::move(jnr));
  break;
 }

 DBG(4) printRawJson_("resulting json: ", json);
}




std::string Jtml::extractTag_(const_sit & si, const_sit& end_it) const {
 // extract tag-name, set end_it pointing at the end of the tag-name
 // expected *si start/end: |<|... / ...|>|
 // expected *end_it at the end: <tag_name| |...>, or <!|-|- ...-->
 if(*si != '<') throw EXP(expect_tag_opening);
 auto start_it = ++si;
 if(*si == '!') {                                               // extract <! tag>
  end_it = ++si;                                                // *si: <!|.|...
  parseExclamationTag(si);                                      // *si: ...|>|
  return std::string{"!"};
 }
 if(start_it != skipWhiteSpace_(si)) throw EXP(white_space_before_tag_name);
 findAnyOf_("> \r\n\t", si);
 end_it = si;
 if(*si != '>')
  findAnyOf_(">", ++si);
 return std::string{ start_it, end_it };
}



Jtml::const_sit & Jtml::parseExclamationTag(const_sit& si) const {
 // expected *si start/end: <|-|-... / --|>|, or <|.|.. / ...|>|
 if(*si == '-' and *(si+1) == '-') {                            // valid comment, find "-->" end
  do {
   do findAnyOf_("-", ++si); while(*(si+1) != '-') ;            // i.e. do while "--" not found
   ++si;
  } while(*(si+1) != '>');                                      // while "-->" not found
  return ++si;
 }
 return findAnyOf_(">", si);
}



Jtml::const_sit & Jtml::findAnyOf_(const char *dlm, const_sit &si, bool throw_exp) const {
 // iterate si until any char in dlm is found
 while(*si != '\0') {
  for(const char * d = dlm; *d != '\0'; ++d)
   if(*d == *si) return si;
  ++si;
 }
 if(throw_exp)
  throw EXP(premature_end_of_file);
 return si;
}



Jtml::const_sit & Jtml::skipWhiteSpace_(const_sit & si) const {
 // skip all white spaces in ws_
 while(*si != '\0') {
  int skip_idx{ matchWhiteSpace_(&*si) };
  if(skip_idx < 0) break;                                       // no white space found
  si += ws_[skip_idx].size();
 }
 return si;
}



Jtml::const_sit & Jtml::skipUntilWhiteSpace_(const_sit & si) const {
 // skip until any white spaces in ws_
 while(*si != '\0') {
  if(matchWhiteSpace_(&*si) >=0 ) break;
  ++si;
 }
 return si;
}



int Jtml::matchWhiteSpace_(const char *str) const {
 // return -1 if no leading lexemes, otherwise index of the leading lexeme
 int i{0};
 for(auto &l: ws_)
  if(strncmp(str, l.c_str(), l.size()) == 0) return i;
  else ++i;
 return -1;
}



std::string & Jtml::trimTrailingWhiteSpace_(std::string &str) const {
 // trim all trailing white spaces
 while(true) {
  if(str.size() >= 2 and matchWhiteSpace_(str.c_str() + str.size() -2) >= 0)
   { str.pop_back(); str.pop_back(); continue; }
  if(matchWhiteSpace_(& str.back()) >= 0)
   { str.pop_back(); continue; }
  break;
 }
 return str;
}



void Jtml::printParsingPoint_(const_sit & si) const {
 // debug output current parsing point
 static const char* pfx{"parsing point: "};

 bool truncate{ std::strlen(&*si) > (DBG_WIDTH-sizeof(pfx)) };
 std::string str{ si, si + (truncate? DBG_WIDTH - sizeof(pfx) - 3: strlen(&*si)) };
 for(auto &c: str) if(c == CHR_RTRN) c = '|';
 DOUT() << pfx << str << (truncate? "...":"") << std::endl;
}



void Jtml::printRawJson_(const char * prompt, Json & j) const {
 // debug-print json in raw format, preserving json format state after printing
 bool js{ j.is_pretty() };
 DOUT() << prompt << j.raw() << std::endl;
 j.pretty(js);
}



Jnode& Jtml::enlist_(Jnode &j) const {
 // enclose referred json node into an ARRAY type, e.g.: "blah" -> [ "blah" ]
 Jnode tmp{ std::move(j) };
 (j = ARY{}).push_back( std::move(tmp) );
 return j;
}






#undef DBG_WIDTH
#undef CHR_RTRN
#undef TAG_ATTRB_LBL
#undef TAG_TRAIL_LBL
#undef JSN_QUOTE
#undef JSN_QUOTED










