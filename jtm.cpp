#include <iostream>
#include <fstream>
#include <sstream>
#include "lib/jtml.hpp"
#include "lib/getoptions.hpp"
#include "lib/dbg.hpp"

using namespace std;

#define VERSION "1.03"


#define OPT_RDT -
#define OPT_DBG d
#define OPT_ELB e
#define OPT_ENM n
#define OPT_RAW r
#define OPT_SLD s
#define OPT_RTR t
#define OPT_VLB v


// facilitate option materialization
#define STR(X) XSTR(X)
#define XSTR(X) #X
#define CHR(X) XCHR(X)
#define XCHR(X) *#X


#define RETURN_CODES \
        RC_OK, \
        RC_END
ENUM(ReturnCodes, RETURN_CODES)

#define OFF_GETOPT RC_END                                       // offset for Getopt exceptions
#define OFF_JTML (OFF_GETOPT + Getopt::end_of_trhow)            // offset for Jtml exceptions
#define OFF_REGEX (OFF_JTML + Jtml::end_of_trhow)               // offset for Regex exceptions


struct CommonResource {
    Getopt              opt;
    string              html;
    Jtml                conv;
    DEBUGGABLE()
};

#define __REFX__(A) auto & A = __common_resource__.A;
#define REVEAL(X, ARGS...) \
        auto & __common_resource__ = X; \
        MACRO_TO_ARGS(__REFX__, ARGS)
// usage: REVEAL(cr, opt, DBG())


// forward declarations
string read_html(CommonResource &);



int main(int argc, char *argv[]) {

 CommonResource r;
 REVEAL(r, opt, html, conv, DBG())

 opt.prolog("\nHTML?XML to JSON lossless convertor. Version " VERSION \
            ", developed by Dmitry Lyssenko (ldn.softdev@gmail.com)\n");
 opt[CHR(OPT_DBG)].desc("turn on debugs (multiple calls increase verbosity)");
 opt[CHR(OPT_ELB)].desc("label used for extra text in tags (i.e. non-attributes)")
                  .bind(conv.extra_label().c_str()).name("label");
 opt[CHR(OPT_ENM)].desc("start enlisting tag values from the first entry");
 opt[CHR(OPT_RAW)].desc("force printing json in a raw format");
 opt[CHR(OPT_SLD)].desc("enforce quoted solidus behavior");
 opt[CHR(OPT_RTR)].desc("do not retry parsing upon facing a closing tag w/o pair");
 opt[CHR(OPT_VLB)].desc("label used for tag values")
                  .bind(conv.value_label().c_str()).name("label");
 opt[0].desc("file to read html from").name("html_src").bind("<stdin>");
 opt.epilog("\nthe tool is html tag semantic agnostic, though provides separate behaviors:\n\
 - parse tag attributes\n\
 - understand and parse tag <!...>\n\
 - understand and parse tag <?...>\n\
 - <script> tag value is not interpolated\n");

 // parse options
 try { opt.parse(argc,argv); }
 catch (stdException & e) { opt.usage(); return e.code() + OFF_GETOPT; }
 conv.value_label(opt[CHR(OPT_VLB)].c_str())
     .extra_label(opt[CHR(OPT_ELB)].c_str())
     .enumerate(opt[CHR(OPT_ENM)])
     .retry(not opt[CHR(OPT_RTR)])
     .quoted_solidus(opt[CHR(OPT_SLD)]);

 DBG().level(opt[CHR(OPT_DBG)])
      .use_ostream(cerr)
      .severity(conv);

 html = read_html(r);

 try{
  cout << conv.jsonize(html).raw(opt[CHR(OPT_RAW)]) << endl;
 }
 catch( stdException & e ) {
  cerr << opt.prog_name() << " exception: " << e.what() << endl;
  return e.code() + OFF_JTML;
 }
 catch (std::regex_error & e) {
  cerr << "regexp exception: " << e.what() << endl;
  return e.code() + OFF_REGEX;
 }

 return RC_OK;
}





string read_html(CommonResource &r) {
 // read and html string
 REVEAL(r, opt, DBG())

 bool redirect{ opt[CHR(OPT_RDT)].hits() != 0 or opt[0].hits() == 0 };
 DBG(0)
  DOUT() << "reading html from: " << (redirect? "<stdin>": opt[0].c_str()) <<endl;

 return string{istream_iterator<char>(redirect?
                                      cin>>noskipws: 
                                      ifstream{opt[0].c_str(), ifstream::in}>>noskipws),
               istream_iterator<char>{}};
}








