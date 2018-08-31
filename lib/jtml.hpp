/*
 * Created by Dmitry Lyssenko, last modified August 26, 2018.
 *
 * html/xml tags agnostic, lossless html/xml to JSON and back converter with a
 * trivial user interface
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
 *          no: // it's another open tag, they my tag is an empty one
 *              parse new tag, and proses the return
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
 * // Conversion spec:
 * //
 * // 1. Conversion rules:
 * //    <a_tag> </a_tag> -> { "a_tag": [ ] }
 * //    <a_tag> ... </a_tag> -> { "a_tag": [ <...> ] }
 * //    <a_tag attributes> </a_tag> -> { "tag": [ { <attributes> } ] }
 * //    <a_tag attributes> ... </a_tag> -> { "a_tag": [ { <attributes> }, <...> ] }
 * //    - i.e. for open/closed pair of tags value is always enclosed into ARY{}
 * //      if tag enlists a single value then optionally it could be de-listed, unless
 * //      the value is attributes (otherwise it'll be indistinguishable from an empty tag)
 * //
 * //    <self_closed attributes /> -> { "self_closed/": { <attributes> } }
 * //    <self_closed/> -> { "self_closed/": null }
 * //    - self-closed tags have attributes value only not enlisted
 * //    - even though not strictly allowed, self-closed w/o attributes converted
 * //      to self-closed with null-value
 * //
 * //    <empty_tag attributes> -> { "empty_tag": { <attributes> } }
 * //    <empty_tag> -> { "empty_tag": null }
 * //    - empty tags (like <br>) have JSON null value
 * //
 * //    - specially processed:
 * //    <!...> -> { "!": "..." }
 * //    <?tag attributes> -> { "?tag": { <attributes> } }
 * //    <?tag> -> { "?tag": null }
 * //
 * //
 * // 2. above rules let convert unambiguously HTML/XML to lossless JSON and
 * //    revert from converted json back to its original format
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
#define CHR_NLIN '\n'
#define CHR_RTRN '\r'
#define TAG_ATTRB_LBL "attributes"                              // default json label for attributes

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
                unexpected_input_after_self_closing, \
                empty_assignment_in_attributes, \
                quote_characters_in_attribute_label, \
                unexpected_empty_tag, \
                unexpected_json_type, \
                attribute_label_not_matching, \
                input_json_is_not_convertible, \
                end_of_throw
    ENUMSTR(ThrowReason, THROWREASON)

    #define ATTRPROP \
                self_closed, \
                open
    ENUM(AttrProperty, ATTRPROP)

    #define FINDOPT \
                throw_exp, \
                dont_throw
    ENUM(Findopt, FINDOPT)

    #define DOCTYPE \
                undefined, \
                html, \
                xml
    ENUMSTR(Doctype, DOCTYPE)


    // User interface
    std::string         quote_str(std::string && src) const;
    std::string         quote_str(const std::string & str) const
                         { std::string src{str}; return quote_str(std::move(src)); }
    std::string         unquote_str(std::string && src) const;
    std::string         unquote_str(const std::string & str) const
                         { std::string src{str}; return unquote_str(std::move(src)); }

    Json &              jsonize(const std::string &src);
    Json &              json(void) { return json_; };
    const std::string & reinstate(const Json &json);
    const std::string & reinstated(void) const { return rstr_; };

    Jtml &              add_ws(const std::string &ws)
                         { ws_.push_back(ws); return *this; }
    template<typename... Args>
    Jtml &              add_ws(const std::string &ws, const Args &... rest)
                         { ws_.push_back(ws); return add_ws(rest...); }
    Jtml &              reset_ws(void)
                         { ws_.clear(); add_ws(" ", "\t", "\n", "\r" ); return *this; }

    Jtml &              attr_label(const std::string &lbl) { al_ = lbl; return *this; }
    const std::string & attr_label(void) const { return al_; }

    Jtml &              enumerate(bool x) { ev_ = x; return *this; }
    bool                enumerate(void) const { return ev_; }
    Jtml &              digitize(bool x) { dj_ = x; return *this; }
    bool                digitize(void) const { return dj_; }
    Jtml &              retry(bool x) { rt_ = x; return *this; }
    bool                retry(void) const { return rt_; }
    bool                is_solidus_quoted(void) const { return jsn_quoted_[0] == '/'; }
    Jtml &              quote_solidus(bool x) {
                         if(x) { jsn_quote_="/" JSN_QUOTE; jsn_quoted_ = "/" JSN_QUOTED; }
                         else { jsn_quote_=JSN_QUOTE; jsn_quoted_ = JSN_QUOTED; }
                         return *this;
                        }
    uint8_t             tab(void) const { return tab_; }
    Jtml &              tab(uint8_t n) { tab_ = n; return *this; }

 protected:
    typedef std::string::const_iterator const_sit;

    Json                json_;                                  // parsed html will go here
    std::string         rstr_;                                  // restored string
    std::vector<std::string>
                        ws_{" ", "\t", "\n", "\r"};             // white space definitions
    std::string         al_{TAG_ATTRB_LBL};                     // attribute label
    bool                ev_{false};                             // always enumerate values as arrays
    bool                dj_{false};                             // digitize numeric values

    std::set<const_sit> itl_;                                   // ignored tag locations
    bool                rt_{true};                              // retry parsing
    Doctype             dt_{undefined};
  std::set<std::string> npt_{"script", "style"};                // do not interpolate tags

    // methods converting XML/HTML
    bool                noParsing_(const std::string &tag) const
                         { return dt_ == html and npt_.count(tag) == 1; }
    bool                ignored_(const const_sit &it) const
                         { return itl_.count(it) == 1; }
    Jnode               jsonize_(const_sit & si);
    void                postProcess_(Jnode &j) const;
    Jnode               parseTag_(const_sit & si);
    Jnode               htmlProlog_(const_sit & si, const_sit & et);
    Jnode               xmlProlog_(const std::string &tag, const_sit & si, const_sit & et);
    const_sit           findClosingTag_(const std::string & tag, const_sit & si) const;
    AttrProperty        parseAttributes_(Jnode &, std::string &&attr) const;
    std::string         parseAttributeValue_(const_sit & si) const;
    void                mergeContent_(Jnode &, Jnode && ) const;
    std::string         extractTag_(const_sit & si, const_sit & end_it) const;
    void                parseContent_(Jnode &j, const_sit & si);
    void                convertEmptyTag_(Jnode &j) const;
    Jnode::Jtype        interpolateTag_(Jnode &j, const_sit &si, const std::string &mytag);
    const_sit &         parseExclamationTag(const_sit& si) const;

    // methods restoring original format
    void                restoreArray_(const Jnode &jn, size_t il=0);
    void                restoreObject_(const Jnode &jn, size_t il);
    void                restoreHtmlTag_(const Jnode &jf, size_t il);
    void                restoreXmlTag_(const Jnode &jf, size_t il, const std::string &tag);
    void                restoreSelfClosed_(const Jnode &jf, size_t il, const std::string &tag);
    void                restoreNullTag_(size_t il, const std::string &tag);
    void                restoreSingleObject_(const Jnode &jf, size_t il, const std::string &tag);
    void                restoreSingleValue_(const Jnode &jf, size_t il, const std::string &tag);
    void                restoreTagPair_(const Jnode &jf, size_t il, const std::string &tag);
    std::string         restoreAttributes_(const Jnode &attr) const;

 private:
    const char *        jsn_quote_{JSN_QUOTE};                  // JSN_QUOTE pointer
    const char *        jsn_quoted_{JSN_QUOTED};                // JSN_QUOTED pointer
    uint8_t             tab_{3};                                // tab size (for indention)

    const_sit &         findAnyOf_(const char *dlm, const_sit &si,
                                   Findopt behavior = throw_exp) const;
    const_sit &         skipWhiteSpace_(const_sit & si) const;
    const_sit &         skipUntilWhiteSpace_(const_sit & si) const;
    int                 matchWhiteSpace_(const char *str) const;
    int                 matchWhiteSpace_(char chr) const
                         { char cs[2] {chr, '\0'}; return matchWhiteSpace_(cs); };
    std::string &       trimTrailingWhiteSpace_(std::string &) const;
    std::string &       trimTrailingWhiteSpace_(std::string && str) const
                         { return trimTrailingWhiteSpace_(str); }
    void                dbgoutParsingPoint_(const_sit & si) const;
    void                dbgoutRawJson_(const char * prompt, const Jnode &j) const;
    Jnode &             enlist_(Jnode &) const;
    std::string         tolower(std::string s)
                         { std::transform(s.begin(), s.end(), s.begin(), ::tolower); return s; }
    std::string         ind_(size_t x) const {                  // generate indent
                         if(rstr_.back() != '\n') return std::string{};
                         return std::string(tab_*x, ' ');
                        };

    EXCEPTIONS(ThrowReason)                                     // see "enums.hpp"

 public:
    DEBUGGABLE(json_)
};

STRINGIFY(Jtml::ThrowReason, THROWREASON)
STRINGIFY(Jtml::Doctype, DOCTYPE)
#undef THROWREASON
#undef FINDOPT
#undef ATTRPROP
#undef DOCTYPE



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
  char qc = src[++end];
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
  start = ++end;
 }
 ss << src.c_str() + start;
 return ss.str();
}



Json & Jtml::jsonize(const std::string &src) {
 // parse input source string into json
 json_.quote_solidus( is_solidus_quoted() );
 json_ = ARY{};                                                 // top collection is always array
 const_sit si{ src.cbegin() };                                  // input string iterator

 while(*si != '\0') {
  json_.push_back( jsonize_(si) );                              // actually jsonize here
  if(json_.back().is_array()) {                                 // all good jsons must be objects!
   std::cerr << "error: malformed document: tag </" << json_.back().back().str()
             << "> at " << std::distance(src.cbegin(), si) << " has no opening"
             << (retry()? ", re-parse ignoring the tag": "" ) << std::endl;
   if(retry()) {
    itl_.insert(si);                                            // add location to ignored
    si = src.cbegin();                                          // reinstate string iterator
    json_.clear();                                              // clean up json
    continue;                                                   // and start over
   }
  }
  while(json_.has_children() and json_.back().is_null())        // in case jsonize_() returned NUL{}
   json_.pop_back();
 }

 if(not enumerate() or digitize())                              // post process resulting JSON
  postProcess_(json_);

 return json_;
}



const std::string & Jtml::reinstate(const Json &json) {
 // this is a wrapper for actual restoreArray_()
 DBG(0) DOUT() << "begin restoring from JSON" << std::endl;

 if(not json.is_array())                                        // must be some other JSON
  throw EXP(input_json_is_not_convertible);

 rstr_.clear();
 restoreArray_(json);

 rstr_.pop_back();                                              // pop trailing '\n'
 return rstr_;
}



void Jtml::postProcess_(Jnode &j) const {
 // 1. de-list single values (unless the single value is { "attributes": ... }
 // 2. also digitize numeric values
 if(j.is_iterable())
  for(auto &r: j) {
   if(not enumerate())
    if(r.is_array() and r.children() == 1) {                    // child is a single value array
     if(not r.front().is_object() or                            // if isn's an object, or it is but
        r.front().front_label() != attr_label())                // label is not "attributes"
      r = std::move(r.front());                                 // then "optimize" it (delist)
    }
   postProcess_(r.value());                                     // process futher
  }
 else                                                           // j is atomic
  if(j.is_string() and digitize()) {                            // then try digitizing
   auto si = j.str().begin();
   if(Json::json_number_definition(si) == Jnode::Number and *si == '\0')
    j.type() = Jnode::Number;                                   // for Json it's enough
  }
}



Jnode Jtml::jsonize_(const_sit & si) {
 // wrapper for parseTag_() - where actual jsonization happens
 // expected *si start / end: |<|... / ...>|.|..
 DBG(0) DOUT() << "begin parsing tag block" << std::endl;

 if(*skipWhiteSpace_(si) == '\0')                               // if no input, must return a valid
  return NUL{};                                                 // JSON, null serves as placeholder
 if(*si != '<') throw EXP(expect_tag_opening);
 return parseTag_(si);
}



Jnode Jtml::parseTag_(const_sit & si) {
 // 1. extract tag (expect opening tags only here)
 // 2. tag is self-closed? - add children, return json object
 // 3. else: parse tag content (value)
 // expected *si start/end: |<|... / ...>|.|.."
 DBG(2) dbgoutParsingPoint_(si);                                // *si: |<|...

 const_sit end_of_tag;                                          // *end_of_tag: <tag| |attr=...>
 std::string tagname{ extractTag_(si, end_of_tag) };            // *si: ...|>|
 DBG(1) DOUT() << "extracted tag: <" << tagname << ">" << std::endl;
 if(tagname.empty())
  throw EXP(unexpected_empty_tag);
 std::string quoted_tag = quote_str(tagname);

 if(tagname == "!") return htmlProlog_(si, end_of_tag);
 if(tagname.front() == '?') return xmlProlog_(tagname, si, end_of_tag);

 if(tagname.front() == '/')                                     // closing tag, unexpected
  throw EXP(unexpected_closing_tag);

 if(tagname.back() == '/')                                      // empty self-closing tag like <br/>
  { tagname.erase(tagname.size()-1); --end_of_tag; }            // let intall null-attribute

 Jnode j;                                                       // prepare dst JSON,
 if(end_of_tag != si) {                                         // there could be attributes, parse
  Jnode atr;
  if(parseAttributes_(atr, std::string{end_of_tag, si}) == self_closed)
   { j[quote_str(tagname + '/')] = std::move(atr); ++si; return j; }
  j[quoted_tag] = ARY{};                                        // else: (not self-closed) -
  if(not atr.is_null())                                         // parse content then, but first,
   j.front().push_back( std::move(atr) );                       // add parsed attributes
 }

 if(noParsing_(tagname)) {                                      // no interpolation (e.g. <script>)
  if(j.empty())
   j[tagname] = ARY{};                                          // parsed tag will go here
  end_of_tag = ++si;                                            // *end_of_tag: ...>|.|...
  mergeContent_(j.front(),
                STR{ quote_str(std::string{ end_of_tag, findClosingTag_(tagname, si) }) });
  return j;
 }

 // not self-closed, process tag content
 if(j.empty())                                                  // if no attributes were yet added
  j[quoted_tag] = ARY{};                                        // then add tag
 parseContent_(j, ++si);
 return j;
}



Jnode Jtml::htmlProlog_(const_sit & si, const_sit & end_of_tag) {
 DBG(1) DOUT() << "<!> content: '" << std::string{ end_of_tag, si } << "'" << std::endl;
 Jnode hp;
 hp["!"] = STR{ quote_str(std::string{ end_of_tag, si++ }) };
 if(dt_ == undefined and tolower(hp["!"]).find("doctype") == 0) {
  dt_ = html;
  DBG(1) DOUT() << "document defined as: " << ENUMS(Doctype, dt_) << std::endl;
 }
 return hp;
}



Jnode Jtml::xmlProlog_(const std::string &tagname, const_sit &si, const_sit &end_of_tag) {
 DBG(1) DOUT() << "XML prolog content: '" << std::string{ end_of_tag, si } << "'" << std::endl;
 Jnode xp;
 parseAttributes_(xp[tagname], std::string{end_of_tag, si++});
 if(dt_ == undefined and tolower(tagname) == "?xml") {
  dt_ = xml;
  DBG(1) DOUT() << "document defined as: " << ENUMS(Doctype, dt_) << std::endl;
 }
 return xp;
}



Jtml::const_sit Jtml::findClosingTag_(const std::string & tag, const_sit & si) const {
 // find a closing pair for tag w/o interpolation
 // expected *si start/end: <tag>|.|.. / ...</tag>|.|..
 // returned sit: ...|<|/tag>...
 const_sit sit;
 --si;

 for(bool tag_found{false}; not tag_found;) {
  tag_found = false;
  sit = findAnyOf_("<", ++si);                                  // *sit = *si: |<|...
  if(*++sit == '/')                                             // a closing tag?
   if(strncmp(&*skipWhiteSpace_(++sit), tag.c_str(), tag.size()) == 0) {// is it my closing tag?
    sit += tag.size();
    tag_found = *skipWhiteSpace_(sit) == '>';
   }
 }

 swap(++sit, si);
 return sit;                                                    // *sit: |<|/tag>; *si: tag>|.|...
}



Jtml::AttrProperty Jtml::parseAttributes_(Jnode &j, std::string && attr) const {
 // attr string is all chars between tag and '>', exclusive
 // return 'close' if attribute string ends with '/', otherwise - return 'open'
 DBG(2) DOUT() << "extracting attributes from: '" << attr << "'" << std::endl;
 AttrProperty aprop{ open };                                    // attribute property state
 const_sit si = attr.cbegin();

 j[attr_label()] = OBJ{};
 auto & a = j.front();

 for(skipWhiteSpace_(si); *si != '\0'; skipWhiteSpace_(si)) {
  if(*si == '/') {
   DBG(2) DOUT() << "tag is self-closed" << std::endl;
   aprop = self_closed;
   ++si;
   continue;
  }
  if(aprop == self_closed)                                      // i.e. there should be no input
   throw EXP(unexpected_input_after_self_closing);              // after '/', like in: <".../blah">
  if(*si == '=')                                                // i.e. '   = value'
   throw EXP(empty_assignment_in_attributes);

  auto begin_it{ si };                                          // to extract attribute name
  std::string attribute{begin_it, findAnyOf_("=/ \r\n\t", si, dont_throw)};
  DBG(2) DOUT() << "found attribute: '" << attribute << "'" << std::endl;

  while(attribute.find_first_of("'\"") != std::string::npos)    // remove any quotations from label
   attribute.erase(attribute.find_first_of("'\""));             // no worries, it will optimized
  if(attribute.empty())
   continue;

  skipWhiteSpace_(si);

  if(*si != '=') {                                              // a trailing value w/o attributes
   DBG(2) DOUT() << "empty attribute: '" << attribute << "'" << std::endl;
   a[attribute] = NUL{};
   continue;
  }

  a[attribute] = STR{ quote_str(parseAttributeValue_(++si)) };  // it's attribute with value then
  DBG(2)
   DOUT() << "extracted attribute: '" << attribute
          << "' = '" << a[attribute] << "'" << std::endl;
 }

 DBG(2) dbgoutRawJson_("all attributes: ", a);
 if(a.empty()) j = NUL{};                                       // if no attributes
 return aprop;
}



std::string Jtml::parseAttributeValue_(const_sit & si) const {
 // parse and return attribute value
 // expected *si start/end: = |"|...."| |, or = |'|....'| |, or =|v|alue| |
 auto start_it = skipWhiteSpace_(si);
 if(*si == '\'' or *si == '"') {
  std::string quote{1, *si};
  std::string rstr{ ++start_it, findAnyOf_(quote.c_str(), ++si, dont_throw) };
  ++si;
  if(std::all_of(rstr.begin(), rstr.end(), [](char c){ return c=='"' or c=='\''; }))
   return {};                                                   // made of `"`s and `'`s only
  return rstr;
 }

 std::string rstr{ start_it, skipUntilWhiteSpace_(si) };
 if(rstr.back() == '/')
  { rstr.pop_back(); --si; }
 return rstr;
}



void Jtml::parseContent_(Jnode &j, const_sit & si) {
 // parse value between open/close tag pair
 // expected *si start / end: <some_tag>|.|.. / ...<[/]some_tag>|.|..
 const_sit start_it, end_it;
 std::string mytag{ unquote_str(j.front_label()) };
 auto extractTag = [&start_it, &end_it, &si, this](void)
                    { start_it=si; std::string s{extractTag_(si, end_it)}; ++si; return s; };
 do {
  start_it = skipWhiteSpace_(si);
  std::string content{ start_it, findAnyOf_("<", si) };         // *si: |<|...
  DBG(1) DOUT() << "extracted content: '" << content << "'" << std::endl;
  mergeContent_(j.front(), STR{ quote_str(trimTrailingWhiteSpace_(content)) });

  std::string tagname{ extractTag() };
  if(retry() and ignored_(si)) {                                // ignore found tag
   DBG(1) DOUT() << "found a tag <" << tagname << "> at ignored position, skipping" << std::endl;
   start_it = si;                                               // fetch content, tag again
   if(not content.empty())                                      // if content was actually added
    j.front().pop_back();                                       // only then remove old content
   content += quote_str(trimTrailingWhiteSpace_( {start_it, findAnyOf_("<", si)} ));
   mergeContent_(j.front(), STR{ std::move(content) });
   tagname = extractTag();
  }
  DBG(1) DOUT() << "found next tag: <" << tagname << "> (my tag: <" << mytag << ">)" << std::endl;

  if(tagname.front() == '/') {                                  // it's a closing tag
   std::string ctag{tagname, 1};                                // closing tag name, skipping '/'
   if(mytag != ctag) {                                          // my tag must become empty then
    convertEmptyTag_(j);
    j.push_back( STR{std::move(ctag)} );
   }
   return;
  }

  si = start_it;                                                // it's another tag, interpolate it
  DBG(1) DOUT() << "parsing nested tag <" << tagname << ">" << std::endl;
 } while(interpolateTag_(j, si, mytag) != Jnode::Array);
}



void Jtml::convertEmptyTag_(Jnode &j) const {
 // enlist json j, extract its content and add to resulting list (mind "attributes")
 Jnode content{ NUL{} };
 swap(content, j.front());
 enlist_(j);

 for(auto it = content.begin(); it != content.end(); content.erase(it))
  if(it->is_object() and it->front_label() == attr_label())     // i.e.: if [ { "attributes": ...} ]
   j.front().front() = std::move(*it);                          // then put attributes back into j
  else
   j.push_back(std::move(*it));
}



Jnode::Jtype Jtml::interpolateTag_(Jnode &j, const_sit &si, const std::string &mytag) {
 // return Object or Array after interpolation is done
 // expected *si start/end: |<|sub_tag>... / ...<[/]some_tag>|.|..
 // Json j is expected to come always as an OBJ type
 Jnode subj{ parseTag_(si) };
 if(subj.is_null()) return Jnode::Object;                       // should never be the case though
 if(subj.is_object())                                           // parsed normally - closed tag
   { mergeContent_(j.front(), std::move(subj)); return Jnode::Object; }

 if(not subj.is_array()) throw EXP(unexpected_json_type);       // now expecting only array type

 std::string return_tag{subj.back().str()};                     // last item or array: a return tag
 subj.pop_back();                                               // yank returned tag from array
 DBG(2)
  DOUT() << "nested closing tag: </" << return_tag
         << "> (my tag: <" << mytag << ">)" << std::endl;
 if(return_tag == mytag)                                        // my closing tag -> merge array
  { mergeContent_(j.front(), std::move(subj)); return Jnode::Array; }   // no further processing

 // return_tag != mytag: j must be converted into an array (meaning: j must become an empty tag)
 convertEmptyTag_(j);
 for(auto &jnr: subj)                                           // merge interpolated subj
  j.push_back(std::move(jnr));
 j.push_back( STR{return_tag} );                                // add the return tag
 return Jnode::Array;
}



void Jtml::mergeContent_(Jnode &jvalue, Jnode && jn) const {
 // merge second arg (jn) into first arg (json) value
 if(jn.is_string())
  if(jn.str().empty()) return;                                  // nothing to merge, (empty string)

 DBG(4) dbgoutRawJson_("content being added: ", jn);
 DBG(5) dbgoutRawJson_("value before merging: ", jvalue);
 if(not jn.is_array())                                          // content is not array type
  jvalue.push_back( std::move(jn) );
 else                                                           // otherwise - merge into array
  for(auto &jnr: jn)
   jvalue.push_back( std::move(jnr) );                          // jvalue always MUST be an array
 DBG(5) dbgoutRawJson_("resulting value: ", jvalue);
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
 findAnyOf_("> \r\n\t", si);                                    // not really consistent with ws_,
 end_it = si;                                                   // will fix it later, once required 
 if(*si != '>')
  findAnyOf_(">", ++si);
 return std::string{ start_it, end_it };
}



Jtml::const_sit & Jtml::parseExclamationTag(const_sit& si) const {
 // expected *si start/end: <! |-|-... / --|>|, or <! |.|.. / ...|>|
 if(*si == '-' and *(si+1) == '-') {                            // valid comment, find "-->" end
  do {
   do findAnyOf_("-", ++si); while(*(si+1) != '-');             // i.e. do while "--" not found
   ++si;
  } while(*(si+1) != '>');                                      // while "-->" not found
  return ++si;
 }
 return findAnyOf_(">", si);
}



Jtml::const_sit & Jtml::findAnyOf_(const char *dlm, const_sit &si,
                                   Findopt behavior) const {
 // iterate si until any char in dlm is found
 while(*si != '\0') {
  for(const char * d = dlm; *d != '\0'; ++d)
   if(*d == *si) return si;
  ++si;
 }
 if(behavior == throw_exp)
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
 // trim all trailing white spaces and return copy
 while(matchWhiteSpace_(& str.back()) >= 0)
  str.pop_back();
 return str;
}



void Jtml::dbgoutParsingPoint_(const_sit & si) const {
 // debug output current parsing point
 static const char* pfx{"parsing point: "};

 bool truncate{ std::strlen(&*si) > (DBG_WIDTH-sizeof(pfx)) };
 std::string str{ si, si + (truncate? DBG_WIDTH - sizeof(pfx) - 3: strlen(&*si)) };
 for(auto &c: str) if(c == CHR_NLIN or c == CHR_RTRN) c = '|';
 DOUT() << pfx << str << (truncate? "...":"") << std::endl;
}



void Jtml::dbgoutRawJson_(const char * prompt, const Jnode & j) const {
 // debug-print json in raw format, preserving json format state after printing
 bool js{ json_.is_pretty() };
 json_.raw();
 DOUT() << prompt << j << std::endl;
 json_.pretty(js);
}



Jnode& Jtml::enlist_(Jnode &j) const {
 // enclose referred json node into an ARRAY type, e.g.: "blah" -> [ "blah" ]
 Jnode tmp{ std::move(j) };
 (j = ARY{}).push_back( std::move(tmp) );
 return j;
}



//
// methods implementing reverse conversion: from JSON to original XML/HTML format
//
void Jtml::restoreArray_(const Jnode &jn, size_t il) {
 // restore elements from ARY{}
 for(auto &jr: jn)
  switch(jr.type()) {
   case Jnode::Object:
    if(jr.is_object() and jr.front_label() == attr_label())     // it's an attribute
     break;                                                     // skip attributes object
    restoreObject_(jr, il);
    break;
   case Jnode::String:
   case Jnode::Number:
    if(rstr_.back() == '\n') rstr_.pop_back();
    rstr_ += unquote_str(jr.val());
    break;
   default:
    throw EXP(unexpected_json_type);                            // though never expected
  }
}



void Jtml::restoreObject_(const Jnode &jn, size_t il) {
 // process all object kinds
 const Jnode & jf = jn.front();
 const std::string & tag = unquote_str(jn.front_label());
 DBG(1) DOUT() << "restoring tag <" << tag << ">:" << std::endl;
 if(tag.empty())
  throw EXP(unexpected_empty_tag);

 if(tag == "!")                                                 // <!...> tag
  return restoreHtmlTag_(jf, il);

 if(tag.front() == '?')                                         // <?xlm...> tag
  return restoreXmlTag_(jf, il, tag);

 if(tag.back() == '/')                                          // self-closed tag: .../>
  return restoreSelfClosed_(jf, il, {tag, 0, tag.size()- 1 });

 if(jf.is_null())                                               // empty tag w/o attr
  return restoreNullTag_(il, tag);

 if(jf.is_object())                                             // empty w. attr, or normal w/o
  return restoreSingleObject_(jf, il, tag);

 if(jf.is_string() or jf.is_number())                           // optimized single value tag
  return restoreSingleValue_(jf, il, tag);

 // here jf.could be only array: null, str, num, obj has been processed, bool is not expected
 restoreTagPair_(jf, il, tag);
}



void Jtml::restoreHtmlTag_(const Jnode &jf, size_t il) {
 // restore tag  <!...>
 DBG(2) DOUT() << "- html prolog tag" << std::endl;
 rstr_ += ind_(il) + "<!" + unquote_str(jf.str()) + ">\n";
 if(dt_ == undefined and tolower(jf.str()).find("doctype") == 0) {
  dt_ = html;
  DBG(1) DOUT() << "document source is: " << ENUMS(Doctype, dt_) << std::endl;
 }
}


void Jtml::restoreXmlTag_(const Jnode &jf, size_t il, const std::string &tag) {
 // restore tag  <?...>
 DBG(2) DOUT() << "- xml prolog tag" << std::endl;
 if(dt_ == undefined and tolower(tag) == "?xml") {
  dt_ = xml;
  DBG(1) DOUT() << "document source is: " << ENUMS(Doctype, dt_) << std::endl;
 }
 rstr_ += ind_(il) + "<" + tag + restoreAttributes_(jf) + ">\n";
}



void Jtml::restoreSelfClosed_(const Jnode &jf, size_t il, const std::string &tag) {
 // restore tag  <..../>
 DBG(2) DOUT() << "- self-closed tag" << std::endl;
 rstr_ += ind_(il) + "<" + tag + restoreAttributes_(jf) + "/>\n";
}



void Jtml::restoreNullTag_(size_t il, const std::string &tag) {
 // restore tag  empty tag with no attr. e.g.: <br>
 DBG(2) DOUT() << "- empty tag w/o attributes" << std::endl;
 rstr_ +=  ind_(il) + "<" + tag + ">\n";
}



void Jtml::restoreSingleObject_(const Jnode &jf, size_t il, const std::string &tag) {
 if(jf.front_label() == attr_label()) {                        // empty tag with attr.
  DBG(2) DOUT() << "- empty tag with attributes" << std::endl;
  rstr_ +=  ind_(il) + "<" + tag + restoreAttributes_(jf) + ">\n";
  return;
 }
 DBG(2) DOUT() << "- tag pair w/o attributes catering another tag" << std::endl;
 rstr_ += ind_(il) + "<" + tag + ">\n";
 restoreObject_(jf, il+1);
 rstr_ +=  ind_(il) + "</" + tag + ">\n";
}



void Jtml::restoreSingleValue_(const Jnode &jf, size_t il, const std::string &tag) {
 DBG(2) DOUT() << "- tag pair catering single atomic value" << std::endl;
 rstr_ += ind_(il) + "<" + tag + ">" + unquote_str(jf.val()) + "</" + tag + ">\n";
}



void Jtml::restoreTagPair_(const Jnode &jf, size_t il, const std::string &tag) {
 if(jf.empty()) {                                               // empty normal: <tag></tag>
  DBG(2) DOUT() << "- empty tag pair" << std::endl;
  rstr_ += ind_(il) + "<" + tag + ">\n" + ind_(il) + "</" + tag + ">\n";
  return;
 }

 std::string atr;
 if(jf.front().is_object() and jf.front().front_label() == attr_label()) // attributes present
  atr = restoreAttributes_(jf.front());
 DBG(2) DOUT() << "- tag pair catering multiple values" << std::endl;
 rstr_ += ind_(il) + "<" + tag + atr + (noParsing_(tag)? ">": ">\n");
 restoreArray_(jf, il+1);
 rstr_ += (noParsing_(tag)? "": ind_(il)) + "</" + tag + ">\n";
}



std::string Jtml::restoreAttributes_(const Jnode &attr) const {
 // restore attributes string from attr = { "attributes": { ... } }
 DBG(1) dbgoutRawJson_("restoring attributes: ", attr);

 if(attr.is_null()) return "";                                  // no attributes
 if(not attr.is_object())                                       // expect to be OBJ{} only
  throw EXP(unexpected_json_type);
 if(attr.front_label() != attr_label())                         // JSON was produced with -a '...'
  throw EXP(attribute_label_not_matching);

 std::string atr;
 for(auto &a: attr[attr_label()]) {
  atr += " " + a.label();
  if(not a.is_null())
   atr += "=\"" + (unquote_str(a.val())) + "\"";
 }
 return atr;
}





#undef DBG_WIDTH
#undef CHR_NLIN
#undef CHR_RTRN
#undef TAG_ATTRB_LBL
#undef JSN_QUOTE
#undef JSN_QUOTED









