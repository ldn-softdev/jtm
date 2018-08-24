#include <iostream>
#include <fstream>
#include <sstream>
#include "lib/jtml.hpp"
#include "lib/getoptions.hpp"
#include "lib/dbg.hpp"

using namespace std;

#define VERSION "2.05"


#define OPT_RDT -
#define OPT_ALB a
#define OPT_DBG d
#define OPT_ENM e
#define OPT_DGT f
#define OPT_IND i
#define OPT_RTR n
#define OPT_RAW r
#define OPT_SLD s
#define OPT_TLB t


// facilitate option materialization
#define STR(X) XSTR(X)
#define XSTR(X) #X
#define CHR(X) XCHR(X)
#define XCHR(X) *#X


#define RETURN_CODES \
        RC_OK, \
        RC_EMPTY, \
        RC_END
ENUM(ReturnCodes, RETURN_CODES)

#define OFF_GETOPT RC_END                                       // offset for Getopt exceptions
#define OFF_JTML (OFF_GETOPT + Getopt::end_of_throw)            // offset for Jtml exceptions
#define OFF_REGEX (OFF_JTML + Jtml::end_of_throw)               // offset for Regex exceptions


struct CommonResource {
    Getopt              opt;
    string              src_str;
    Jtml                conv;
    DEBUGGABLE()
};

#define __REFX__(A) auto & A = __common_resource__.A;
#define REVEAL(X, ARGS...) \
        auto & __common_resource__ = X; \
        MACRO_TO_ARGS(__REFX__, ARGS)
// usage: REVEAL(cr, opt, DBG())


// forward declarations
void try_reversing(CommonResource &);
string read_source(CommonResource &);



int main(int argc, char *argv[]) {

 CommonResource r;
 REVEAL(r, opt, src_str, conv, DBG())

 opt.prolog("\nHTML/XML to JSON and back lossless convertor. Version " VERSION \
            ", developed by Dmitry Lyssenko (ldn.softdev@gmail.com)\n");
 opt[CHR(OPT_ALB)].desc("a label used for attribute values")
                  .bind(conv.attr_label().c_str()).name("label");
 opt[CHR(OPT_DBG)].desc("turn on debugs (multiple calls increase verbosity)");
 opt[CHR(OPT_ENM)].desc("enlist even single values (otherwise don't)");
 opt[CHR(OPT_DGT)].desc("digitize all numerical strings");
 opt[CHR(OPT_IND)].desc("indent for pretty printing").bind("3").name("indent");
 opt[CHR(OPT_RTR)].desc("do not retry parsing upon facing a closing tag w/o its pair");
 opt[CHR(OPT_RAW)].desc("force printing json in a raw format");
 opt[CHR(OPT_SLD)].desc("enforce quoted solidus behavior");
 opt[CHR(OPT_TLB)].desc("a label used for trailing text (empty attributes) inside tags")
                  .bind(conv.trail_label().c_str()).name("label");
 opt[0].desc("file to read source from").name("src_file").bind("<stdin>");
 opt.epilog("\nthe tool is html/xml tag semantic agnostic, follows conversion specification:\n\
  <tag> </tag>                <-> { \"tag\": [] }\n\
  <tag> ... </tag>            <-> { \"tag\": [ <...> ] }\n\
  <tag attributes> </tag>     <-> { \"tag\": [ { <attributes> } ] }\n\
  <tag attributes> ... </tag> <-> { \"tag\": [ { <attributes> }, <...> ] }\n\
  <self_closed attributes />  <-> { \"self_closed/\": { <attributes> } }\n\
  <self_closed/>              <-> { \"self_closed/\": null }\n\
  <empty_tag attributes>      <-> { \"empty_tag\": { <attributes> } }\n\
  <empty_tag>                 <-> { \"empty_tag\": null }\n\
  <!...>                      <-> { \"!\": <...> }\n\
  <?tag attributes>           <-> { \"?tag\": { <attributes> } }\n\
  <?tag>                      <-> { \"?tag\": null }\n\
if a tag enlists a single value then optionally it could be de-listed (default\n\
behavior), unless the value is \"attributes\" - then no delisting occurs\n");

 // parse options
 try { opt.parse(argc,argv); }
 catch (stdException & e) { opt.usage(); return e.code() + OFF_GETOPT; }
 conv.attr_label(opt[CHR(OPT_ALB)].c_str())
     .trail_label(opt[CHR(OPT_TLB)].c_str())
     .enumerate(opt[CHR(OPT_ENM)])
     .digitize(opt[CHR(OPT_DGT)])
     .retry(not opt[CHR(OPT_RTR)])
     .tab(opt[CHR(OPT_IND)])
     .quoted_solidus(opt[CHR(OPT_SLD)]);

 DBG().level(opt[CHR(OPT_DBG)])
      .use_ostream(cerr)
      .severity(conv);


 try {
  src_str = read_source(r);

  // see if source is JSON first
  try_reversing(r);

  // it's not json, them must be html/xml
  conv.jsonize(src_str);
  if(conv.json() == ARY{}) return RC_EMPTY;
  cout << conv.json().tab(opt[CHR(OPT_IND)]).raw(opt[CHR(OPT_RAW)]) << endl;
 }
 catch( stdException & e ) {
  DBG(0) DOUT() << "exception raised by " << e.where() << endl;
  cerr << opt.prog_name() << " exception: " << e.what() << endl;
  return e.code() + OFF_JTML;
 }
 catch (std::regex_error & e) {
  cerr << "regexp exception: " << e.what() << endl;
  return e.code() + OFF_REGEX;
 }

 return RC_OK;
}





void try_reversing(CommonResource &r) {
 // try reinstate original XML/HTML from JSON
 REVEAL(r, conv, src_str, DBG())
 DBG(0) DOUT() << "attempt parsing as Json..." << endl;

 try {
  Json j;
  cout << conv.reinstate(j.parse(src_str)) << endl;
  exit(RC_OK);
 }
 catch(stdException & e) {
  DBG(0) DOUT() << "exception raised by " << e.where() << endl;
  if(e.code() != Jnode::expected_json_value) throw e;
  DBG(0) DOUT() << "source does not appear to be Json, will parse HTML/XML" << endl;
 }
}





string read_source(CommonResource &r) {
 // read and src_str string
 REVEAL(r, opt, DBG())

 bool redirect{ opt[CHR(OPT_RDT)].hits() != 0 or opt[0].hits() == 0 };
 DBG(0)
  DOUT() << "reading source from: " << (redirect? "<stdin>": opt[0].c_str()) << endl;

 return string{istream_iterator<char>(redirect?
                                      cin>>noskipws:
                                      ifstream{opt[0].c_str(), ifstream::in}>>noskipws),
               istream_iterator<char>{}};
}












