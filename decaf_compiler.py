import sys
import decaf_codegen
import ply.lex as lex
import ply.yacc as yacc

# Names         : Henry Lam    &    Sharon Guan
# Netid         : henlam            shgguan
# Student ID    : 113350850         114430436

def just_scan():
    fn = sys.argv[1] if len(sys.argv) > 1 else ""
    if fn == "":
        print("Missing file name for source program.")
        print("USAGE: python3 decaf_compiler.py <decaf_source_file_name>")
        sys.exit()
    import decaf_lexer
    lexer = lex.lex(module = decaf_lexer, debug = 0)

    fh = open(fn, 'r')
    source = fh.read()
    lexer.input(source)
    next_token = lexer.token()
    while next_token != None:
        # print(next_token)
        next_token = lexer.token()
# end def just_scan()


def main():
    fn = sys.argv[1] if len(sys.argv) > 1 else ""
    if fn == "":
        print("Missing file name for source program.")
        print("USAGE: python3 decaf_compiler.py <decaf_source_file_name>")
        sys.exit()
    
    import decaf_lexer
    import decaf_parser
    lexer = lex.lex(module = decaf_lexer, debug = 0)
    parser = yacc.yacc(module = decaf_parser, debug = 0)

    fh = open(fn, 'r')
    source = fh.read()
    fh.close()
    result = parser.parse(source, lexer = lexer, debug = 0)
    #print(str(result))

    codeGen = decaf_codegen.main()
    fileName = sys.argv[1]
    fileName = fileName[0:fileName.index('.')]+".txt"
    
    textFile = open(fileName, "w")
    textFile.write(codeGen)
    textFile.close()

    # Parsing Successful
   
    print("YES")
    #print()

if __name__ == "__main__":
    just_scan()
    main()
