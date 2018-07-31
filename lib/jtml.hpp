/*
 * Created by Dmitry Lyssenko, last modified July 26, 2018.
 *
 * html tags agnostic, lossless html to json converter with a trivial user interface
 *
 * This class does not keep track of HTML list for empty tags (some are and some
 * are not), instead it relies on a parsing rule:
 *  - if tag's value is getting closed by another tag, it means that my tag (if
 *    not marked explicitly) is an empty tag
 *
 * This parser though understands and parses properly:
 *  - attributes within tags,
 *  - parses separately tags starting with <!...
 *  - value of <script> tags is not interpolated (i.e not parsed)
 *
 * Conversion rule:
 *  - each tag is translated into a JSON object, with a single label - name of the
 *    tag
 *  - all attributes go into object pairs <label>:<value> where <label> is the attribute's
 *    name and <value> is its value
 *  - value of the tag (i.e. everything between open tag and closing tag) is going
 *    under the label "~value" (default name, could be changed by user)
 *  - empty tags (those w/o attributes and values) will be set to  JSON null value
 *
 * This code sample illustrates the above behavior:
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
 *   cout << cv.jsonize(html).json() << endl;
 *
 *  Output:
 *  [
 *     {
 *        "!": "DOCTYPE html"
 *     },
 *     {
 *        "html": {
 *           "~value": [
 *              {
 *                 "head": {
 *                    "~value": [
 *                       {
 *                          "title": {
 *                             "~value": "HTML example"
 *                          }
 *                       },
 *                       {
 *                          "meta": {
 *                             "charset": "utf-8"
 *                          }
 *                       }
 *                    ]
 *                 }
 *              },
 *              {
 *                 "body": {
 *                    "text": "green",
 *                    "~value": {
 *                       "p": {
 *                          "~value": [
 *                             "Oh Brother,",
 *                             {
 *                                "br": null
 *                             },
 *                             "Where Art Thou?",
 *                             {
 *                                "br": null
 *                             }
 *                          ]
 *                       }
 *                    }
 *                 }
 *              }
 *           ]
 *        }
 *     }
 *  ]
 *
 *
 *
 * Tag values interpolation logic:
 * 1. extract tag-name (name, end of tag, end of name)
 * 2. tag is self-closed?
 *    - yes: add children - attributes (if any), return resulting JSON object.
 * 3. parse value, facing a new tag?
 *    - yes: parsed so far value empty? ignore: otherwise merge with label "~value"
 *    - closing tag == my tag? return JSON object.
 *    - closing tag != my tag?  // means my tag is an empty tag
 *      yes: return [ { mytag: parsed_my_tag_attr }, parsed_value, closing_tag ]
 *      no, another open tag: parse_tag()
 *    - returned object? yes: add obj to "~value", keep parsing
 *      otherwise: last value of array == mytag?
 *                 - yes: pop last value from array,
 *                        merge array with content, return JSON object
 *                 - no:  //array != mytag: my tag is an empty tag
 *                   make [ {mytag: parsed_my_tag_w/o_value}, my_parsed value ],
 *                   merge my array with returned array, return Json array
 */


#pragma once

#include <string.h>
#include <string>
#include <vector>
#include <set>
#include <iomanip>
#include <sstream>
#include <iomanip>
#include "enums.hpp"
#include "Json.hpp"
#include "dbg.hpp"



#define DBG_WIDTH 80
#define CHR_RTRN '\n'
#define TAG_VALUE_LBL "~value"                                  // default json label for tag value
#define TAG_EXTRA_LBL "~extra"                                  // label for values w/o attribute

#define JSN_QUOTE "\b\f\n\r\t\"\\"                             // chars requiring quotation in JSON
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
                premature_end_of_html, \
                empty_self_closed_not_allowed, \
                unexpected_input_after_self_closing, \
                empty_assignment_in_attributes, \
                unexpected_empty_tag_with_attributes, \
                unexpected_json_type, \
                end_of_trhow
    ENUMSTR(ThrowReason, THROWREASON)


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

    Jtml &              value_label(const std::string &lbl) { vl_ = lbl; return *this; }
    const std::string & value_label(void) const { return vl_; }
    Jtml &              extra_label(const std::string &lbl) { el_ = lbl; return *this; }
    const std::string & extra_label(void) const { return el_; }

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
                        ws_{" ", "\t", "\n", "\r"};             // white space lexemes
    std::string         el_{TAG_EXTRA_LBL};                     // label for extras in tag
    std::string         vl_{TAG_VALUE_LBL};                     // tag value label
    bool                ae_{false};                             // always arrange as arrays flag
    std::set<const_sit> itl_;                                   // ignored tag locations
    bool                rt_{true};                              // retry parsing
  std::set<std::string> npt_{"script"};                         // no parse tags

    Json                jsonize_(const_sit & si) const;
    Json                parseTag_(const_sit & si) const;
    const_sit &         findClosingTag_(const std::string & tag, const_sit & si) const;
    bool                parseAttributes_(Json &, std::string &&attr) const;
    std::string         parseAttributeValue_(const_sit & si) const;
    void                mergeContent_(Json &, Json && ) const;
    std::string         extractTag_(const_sit & si, const_sit & end_it) const;
    void                parseContent_(Json &j, const_sit & si) const;
    bool                interpolateTag_(Json &j, const_sit &si, const std::string &mytag) const;
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

    EXCEPTIONS(ThrowReason)                                     // see "enums.hpp"

 public:
    DEBUGGABLE(json_)
};

STRINGIFY(Jtml::ThrowReason, THROWREASON)
#undef THROWREASON





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
   std::cerr << "error: broken html: tag </" << json_.back().back().str() << "> at "
             << std::distance(html.cbegin(), si) << " has no opening"
             << (retry()? ", re-parse ignoring the tag": "" ) << std::endl;
   if(retry()) {
    itl_.insert(si);                                            // add location to ignored
    si = html.cbegin();                                         // reinstate string iterator
    json_.clear();                                              // clean up json
    continue;                                                   // and start over
   }
  }
  while(json_.has_children() and json_.back().is_null())        // in case jsonize_() returned NUL{}
   json_.erase(json_.children()-1);
 }

 return json_;
}



Json Jtml::jsonize_(const_sit & si) const {
 // expected *si start/end: |<|... / ...>|.|..
 DBG(0) DOUT() << "begin parsing tag block" << std::endl;

 if(*skipWhiteSpace_(si) == '\0')                               // if no input, must return a valid
  return NUL{};                                                 // JSON, null serves as placeholder
 if(*si != '<') throw EXP(expect_tag_opening);
 return parseTag_(si);
}



Json Jtml::parseTag_(const_sit & si) const {
 // expected *si start/end: |<|... / ...>|.|.."
 // 1. extract tag
 // 2. tag is self-closed? - add children, return json object
 // 3. else: parse tag content (value)
 const_sit end_of_tag;                                          // *end_of_tag: <tag| |attr=...>

 DBG(2) printParsingPoint_(si);                                 // *si: |<|...
 std::string tagname{ extractTag_(si, end_of_tag) };            // *si: ...|>|
 DBG(1) DOUT() << "extracted tag: <" << tagname << ">" << std::endl;

 if(tagname == "!") {
  DBG(1) DOUT() << "<!> content: '" << std::string{ end_of_tag, si } << "'" << std::endl;
  return OBJ { LBL { "!", STR{ quote_str(std::string{ end_of_tag, si++ }) } } };
 }
 if(tagname.front() == '/')                                     // closing tag, unexpected
  throw EXP(unexpected_closing_tag);

 Json j{ OBJ{ LBL{ tagname, NUL{} } } };                        // parsed tag will go here
 if(end_of_tag != si) {                                         // there could be attributes, parse
  if(parseAttributes_(j, std::string{end_of_tag, si}))          // if tag is self-closed
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
 // expected *si start/end: <tag>|.|.. / ...|<|/tag>
 // find a closing pair for tag w/o interpolation
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



bool Jtml::parseAttributes_(Json &j, std::string && attr) const {
 // return true if attribute string ends with '/' - i.e. self-closed, false otherwise
 DBG(2) DOUT() << "extracting attributes from: '" << attr << "'" << std::endl;
 bool self_closed{ false };
 const_sit si = attr.cbegin();

 for(skipWhiteSpace_(si); *si != '\0'; skipWhiteSpace_(si)) {
  if(*si == '/') {
   DBG(2) DOUT() << "tag is self-closed" << std::endl;
   self_closed = true;
   ++si;
   continue;
  }
  if(self_closed)                                               // i.e. there should be no input
   throw EXP(unexpected_input_after_self_closing);              // after '/', like in: <".../blah">

  auto start_it = si;                                           // extract attribute name
  std::string attribute{start_it, findAnyOf_("=", si, false)};
  if(trimTrailingWhiteSpace_(attribute).empty())                // e.g. " = some_value"
   throw EXP(empty_assignment_in_attributes);

  if(*si == '\0') {                                             // some value w/o attributes
   mergeContent_(j, OBJ{ LBL{ extra_label(), STR{quote_str(attribute)} } } );
   return false;
  }

  std::string value{ parseAttributeValue_(++si) };
  DBG(2) DOUT() << "extracted attribute '" << attribute << "' = '" << value << "'" << std::endl;
  if(j[0].is_null()) j[0] = OBJ{};                              // add attribute to json
  j[0][attribute] = STR{ quote_str(std::move(value)) };
 }

 return self_closed;
}



std::string Jtml::parseAttributeValue_(const_sit & si) const {
 // expected *si start/end: =| |"...."| |, or =| |'....'| |, or =|v|alue| |
 // parse and return attribute value
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
  if(retry() and itl_.count(si) == 1) {
   DBG(1) DOUT() << "found a tag at ignored position </" << tagname << ">, skipping" << std::endl;
   continue;
  }
  std::string mytag{ j.begin()->label() };
  DBG(1) DOUT() << "found next tag: <" << tagname << "> (my tag: <" << mytag << ">)" << std::endl;

  if(tagname.front() == '/') {                                  // a closing tag found:
   std::string ctag{tagname, 1};
   if(mytag == ctag) return;                                    // it's my closing tag
   j = ARY{ OBJ{ LBL{ mytag, j[mytag] } } };                    // not my closing, return array
   if(not content.empty()) j.push_back( STR{content} );         // with a non-empty content
   j.push_back( STR{ctag} );                                    // and closing tag itself
   return;
  }

  si = start_it;                                                // it's another tag, interpolate it
  DBG(2) DOUT() << "parsing nested tag <" << tagname << ">" << std::endl;
  if(interpolateTag_(j, si, mytag) == false) return;
 }
}



bool Jtml::interpolateTag_(Json &j, const_sit &si, const std::string &mytag) const {
 // expected *si start/end: |<|sub_tag>... / ...<[/]some_tag>|.|..
 // return true if parsed tag is object, false if it's array
 Json subj{ parseTag_(si) };
 if(subj.is_null()) return true;                                // should never be the case though
 if(subj.is_object())                                           // parsed normally closed tag
   { mergeContent_(j, std::move(subj)); return true; }          // returned json object

 if(not subj.is_array()) throw EXP(unexpected_json_type);      // now expecting only array type

 std::string return_tag{subj.back().str()};
 subj.erase(subj.children()-1);                                 // yank returned tag from array
 DBG(2)
  DOUT() << "nested closing tag: </" << return_tag << "> (my tag: <" << mytag << ">)" << std::endl;
 if(return_tag == mytag)                                        // my closing tag -> merge array
  { mergeContent_(j, std::move(subj)); return false; }

 Json jcnt{ NUL{} };                                            // no my closing tag
 if(j[mytag].is_object() and j[mytag].count(value_label()) > 0) {   // i.e. ~value is present in j
  jcnt = std::move(j[mytag][value_label()]);                    // move label "~value" out of j
  if(j[mytag].erase(value_label()).empty()) j[mytag] = NUL{};
 }
 j = ARY{ OBJ{ LBL{ mytag, j[mytag] } } };
 if(not jcnt.is_null()) {                                       // i.e. if "~value" was moved here
  if(enumerate() and jcnt.is_array() and jcnt.size() == 2 and jcnt.front().is_string())
   jcnt = jcnt.front();
  j.push_back(jcnt);
 }
 for(auto &jnr: subj) j.push_back(std::move(jnr));              // merge subj
 j.push_back(STR{return_tag});                                  // add return tag
 return false;
}




void Jtml::mergeContent_(Json &json, Json && jn) const {
 if(jn.is_string())
  if(jn.str().empty()) return;                                  // nothing to add, (empty string)

 auto & j = json[0];                                            // json is guaranteed to have one
 DBG(4) printRawJson_("content being added: ", jn);
 DBG(4) printRawJson_("json before merging: ", json);
 while(true) {
  if(j.is_null())                                               // first time content being added
   j = enumerate()? OBJ{ LBL{ value_label(), ARY{}} } : OBJ{};
  if(j.count(value_label()) == 0)                               // adding given label first time
   { j[value_label()] = std::move(jn); break; }
  if(not j[value_label()].is_array())                           // adding content a second time
   j[value_label()] = ARY{ j[value_label()] };                  // extend to array
  if(not jn.is_array())                                         // content is not array type
   j[value_label()].push_back(std::move(jn));
  else                                                          // otherwise - merge into array
   for(auto &jnr: jn)
    j[value_label()].push_back(std::move(jnr));
  break;
 }

 DBG(4) printRawJson_("resulting json: ", json);
}




std::string Jtml::extractTag_(const_sit & si, const_sit& end_it) const {
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
  throw EXP(premature_end_of_html);
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
 bool js{ j.is_pretty() };
 DOUT() << prompt << j.raw() << std::endl;
 j.pretty(js);
}







#undef DBG_WIDTH
#undef CHR_RTRN
#undef TAG_VALUE_LBL
#undef TAG_EXTRA_LBL
#undef JSN_QUOTE
#undef JSN_QUOTED










