import ply.yacc as yacc
from decaf_lexer import *
from decaf_ast import *
from decaf_typecheck import *

# Get the token map from the lexer.  This is required.
from decaf_lexer import tokens

# Names         : Henry Lam    &    Sharon Guan
# Netid         : henlam            shgguan
# Student ID    : 113350850         114430436

precedence = (
    ('right', 'EQUALS'),
    ('left', 'OR'),
    ('left', 'AND'),
    ('nonassoc', 'NEQUALS', 'EEQUALS'),
    ('nonassoc', 'LESS', 'GEQUALS', 'LEQUALS', 'GREATER'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'NOT', 'UMINUS', 'UPLUS'),
)

# INPUT:    node - node, 
#           parent - parent of node
#           field - field the node belongs to in the parent. Either an ATTRIBUTE or BLOCK STMT INDEX
# OUTPUT:   NOTHING
def bfs(node, parent, field, blockID, index = 0):
    # Fail Cases
    if (not isinstance(node, list) and
        not isinstance(node, VariableExpr) and
        not isinstance(node, BlockStatement) and
        not isinstance(node, StatementRecord) and
        not isinstance(node, ExpressionRecord)):
        return
    
    # Success Case ->   1. Replace VariableExpr with ClassReference. Update Variable Record
    #                   2. Replace VariableExpr letter id with number id. Update Variable Record                
    if isinstance(node, VariableExpr):
        # 1. Replace VariableExpr with ClassReference
        lineNum = node.lineNum
        if node.id in classTable:
            classRef = ClassReferenceExpr(node.id, lineNum)    
            if isinstance(parent, BlockStatement) or isinstance(parent, list):
                parent[index] = classRef
            else:
                setattr(parent, field, classRef)
        # 2. Replace VariableExpr letter id with number id
        else:
            var = traverseUp(node, "block", blockID)
            if not var:
                raise Exception("Variable " + node.id + " not declared" + node.lineNum)
            if not var.type.name: 
                var.type.name = "user"
            node.id = var.id
        return
    
    # Continue Search
    # List to handle parameters ...
    if isinstance(node, list):
        for i, param in enumerate(node):
            bfs(param, node, None, blockID, i)
    # Blocks reference values by indices
    if isinstance(node, BlockStatement):
        for i, stmt in enumerate(node.stmts):
            bfs(stmt, node, "stmts", node.id, i)

    # Statements and Expressions reference valeus by attributes
    elif isinstance(node, StatementRecord) or isinstance(node, ExpressionRecord):
        for var in vars(node):  # Iterates all attribute names
            child = getattr(node, var)
            bfs(child, node, var, blockID) 

# INPUT:    varExpr - variable
#           type - type of block
#           id - id of block
# OUTPUT:   variable record
def traverseUp(varExpr, type, id):
    name = varExpr.id

    # Get out of blocks
    while type == "block":
        cur = blockTable[id]
        type = cur.parent["type"]
        id = cur.parent["id"]

    parentID = cur.parent["id"]
    if type == "method":
        parent = methodTable[parentID]
    if type == "constructor":
        parent = constrTable[parentID]

    if name in parent.variableTable:
        var = parent.variableTable[name]
        return var

    return None

# Program Declaration
def p_program(p):
    '''program  : many_class_decl'''
    p[0] = Program(p[1])

    # Set Class References
    for cls in p[0].classes:
        for constr in cls.constructors:
            bfs(constr.body, constr, "body", constr.body.id, 0)
            typeAssignExprBFS(constr.body, cls, constr.body.id, 0)
            typeCheckStmtBFS(constr.body, None, constr.body.id, 0)
        for method in cls.methods:
            bfs(method.body, method, "body", method.body.id, 0)
            typeAssignExprBFS(method.body, cls, method.body.id, 0) 
            typeCheckStmtBFS(method.body, method, method.body.id, 0) 

def p_many_class_decl(p):
    '''many_class_decl : class_decl many_class_decl
                        | empty'''
    if not p[1]:
        p[0] = []
    else:
        p[2].append(p[1])
        p[0] = p[2] 
    
# Class Declaration
def p_class_decl(p):
    '''class_decl   : CLASS ID EXTENDS ID LCURLY class_body_decl_list RCURLY
                    | CLASS ID LCURLY class_body_decl_list RCURLY'''
    
    # Init index of class_body_decl_list
    cbdl = 6
    if len(p) == 8:
        superName = p[4]
    else:
        superName = ""
        cbdl -= 2

    # Errors
    if p[2] in classTable:
        raise Exception("Duplicate class declaration!")
    
    fields = p[cbdl]["fields"]
    names = set()
    for field in fields:
        if field.name in names:
            raise Exception("Duplicate field names!")
        names.add(field.name)
    
    # Success
    p[0] = ClassRecord(
        name         = p[2],
        superName    = superName,
        constructors = p[cbdl]["constructors"],
        methods      = p[cbdl]["methods"],
        fields       = p[cbdl]["fields"])
    classTable[p[2]] = p[0]

    # Add Child (itself)
    subTypes[p[2]].add(p[2])
    subTypes[p[2]].add("null")

    # Add Parent
    subTypes[superName].add(p[2])
    subTypes[superName].add("null")
    subTypes[superName].add(superName)

def p_class_body_decl_list(p):
    '''class_body_decl_list : class_body_decl many_class_body_decl'''
    p[0] = {"constructors" : [],
            "methods": [],
            "fields": []}
    p[2].append(p[1])

    # Organize bodies by category
    for body in p[2]:
        # Somehow fix methodcall expr here
        if isinstance(body, ConstructorRecord):
            p[0]["constructors"].append(body)
        if isinstance(body, MethodRecord):
            p[0]["methods"].append(body)
        if isinstance(body, list):  # List of Fields
            p[0]["fields"] += body

def p_many_class_body_decl(p):
    '''many_class_body_decl : class_body_decl many_class_body_decl
                            | empty'''
    if not p[1]:
        p[0] = []
    else:
        p[2].append(p[1])
        p[0] = p[2]

def p_class_body_decl(p):
    '''class_body_decl  : field_decl
                        | method_decl
                        | constructor_decl'''
    
    # Add Method/Constructor Formals and Locals
    if isinstance(p[1], MethodRecord) or isinstance(p[1], ConstructorRecord):
        record = p[1]
        type = "method" if isinstance(p[1], MethodRecord) else "constructor"
        # Inserting Formals
        for index, formal in enumerate(record.parameters):
            if(formal.name in record.variableTable):
                raise Exception("Error with variable names duplicated")
            
            newVar = VariableRecord(formal.name, len(varTable) + 1, "formal", formal.type)
            record.variableTable[formal.name] = newVar
            record.parameters[index] = newVar
            varTable[len(varTable) + 1] = newVar

        # Inserting Locals
        locals = [stmt for stmt in record.body.stmts if isinstance(stmt, DeclareVariable)] # List of DeclareVariable
        for declarVar in locals:
            for name in declarVar.names:
                if(name in record.variableTable):
                    raise Exception("Error with variable names duplicated")
                newVar = VariableRecord(name, len(varTable) + 1, "local", declarVar.type)
                record.variableTable[name] = newVar
                varTable[len(varTable) + 1] = newVar
        
        # Link Parents for Blocks
        record.body.parent = {"type": type,
                              "id": record.id}
        
    p[0] = p[1]

# Fields
def p_field_decl(p):
    '''field_decl : modifier var_decl'''
    p[0] = []
    declareVar = p[2]
    for name in declareVar.names:
        p[0].append(FieldRecord(
            name            = name,
            id              = len(fieldTable) + 1,
            containingClass = "",   # Declared in ast.ClassRecord
            visibility      = p[1]["visibility"],
            applicability   = p[1]["applicability"],
            type            = declareVar.type))
    fieldTable[len(fieldTable)+1] = p[0]

def p_modifier(p):
    '''modifier : PUBLIC STATIC
                | PRIVATE STATIC
                | STATIC
                | PUBLIC
                | PRIVATE
                | empty'''
    # Default object
    p[0] = {"visibility" : "private", "applicability" : "instance"}
    if len(p) == 3:
        p[0]["applicability"] = p[2]
    if p[1] in ["public", "private"]:
        p[0]["visibility"] = p[1]
    elif p[1] == "static":
        p[0]["applicability"] = "static"

def p_var_decl(p):
    '''var_decl : type variables SEMICOLON'''

    p[0] = DeclareVariable(p[1], p[2])

def p_type(p):
    '''type : INT
            | FLOAT
            | BOOLEAN
            | VOID
            | NULL
            | ID'''
    if p[1] not in ["int", "float", "boolean", "void", "null"]:
        p[0] = TypeRecord("user", p[1])
    else:
        p[0] = TypeRecord(p[1])

def p_variables(p):
    '''variables : variable comma_var'''
    p[2].append(p[1])
    p[0] = p[2]

def p_comma_var(p):
    '''comma_var    : COMMA variable comma_var
                    | empty'''
    if not p[1]:
        p[0] = []
    else:
        p[3].append(p[2])
        p[0] = p[3]

def p_variable(p):
    '''variable : ID'''
    p[0] = p[1]

# Methods and Constructors
def p_method_decl(p):
    '''method_decl  : modifier type ID LPAREN formals RPAREN block
                    | modifier VOID ID LPAREN formals RPAREN block'''
    
    # Fail Cases
    formalNames = []
    for formal in p[5]:
        formalNames.append(formal.name)
    
    for var in p[7].variableTable:
        if var in formalNames:
            raise Exception("Method formals already has this variable name!")
        
    retType = TypeRecord("void") if p[2] == "void" else p[2]
    p[0] = MethodRecord(
        name            = p[3],
        id              = len(methodTable) + 1,
        containingClass = "",   # Declared in ast.ClassRecord
        visibility      = p[1]["visibility"],
        applicability   = p[1]["applicability"],
        parameters      = p[5][::-1],
        returnType      = retType,
        variableTable   = {},
        body            = p[7])
    
    methodTable[len(methodTable)+1] = p[0]

    
def p_constructor_decl(p):
    '''constructor_decl : modifier ID LPAREN formals RPAREN block'''
    p[0] = ConstructorRecord(
        id            = len(constrTable) + 1,
        visibility    = p[1]["visibility"],
        parameters    = p[4][::-1],
        variableTable = {},
        body          = p[6]
    )

    constrTable[len(constrTable)+1] = p[0]

def p_formals(p):
    '''formals : formal_param comma_formal
               | empty'''
    if not p[1]:
        p[0] = []
    else:
        p[2].append(p[1])
        p[0] = p[2]

def p_comma_formal(p):
    '''comma_formal : COMMA formal_param comma_formal
                    | empty'''
    if not p[1]:
        p[0] = []
    else:
        p[3].append(p[2])
        p[0] = p[3]

def p_formal_param(p):
    '''formal_param : type variable'''
    p[0] = FormalParam(p[1], p[2])

# Statements
def p_block(p):
    '''block : LCURLY many_stmts RCURLY'''

    # Fail case - Declarations not at the top of the block
    declaring = True
    for stmt in p[2][::-1]:
        if isinstance(stmt, DeclareVariable):
            if not declaring:
                raise Exception("Declarations must be at the top of the block")
        else:
            declaring = False

    # Make Variable Table
    variableTable = {}
    locals = [stmt for stmt in p[2] if isinstance(stmt, DeclareVariable)][::-1] # List of DeclareVariable
    for declarVar in locals:
        for name in declarVar.names:
            if(name in variableTable):
                raise Exception("Error with variable names duplicated" + getLineNum(p))
            newVar = VariableRecord(name, len(varTable) + 1, "local", declarVar.type)
            variableTable[name] = newVar

    # Make Block, blockTable Insert
    id = len(blockTable) + 1
    block = BlockStatement(id , {"type": None, "id": -1}, variableTable, p[2], getLineNum(p))
    blockTable[id] = block

    # Create Parent - Child Link
    for stmt in block.stmts:
        # block
        if isinstance(stmt, BlockStatement):
            stmt.parent = {"type": "block", "id": id}   
        # If
        if isinstance(stmt, IfStatement):
            if isinstance(stmt.thenStmt, BlockStatement):
                stmt.thenStmt.parent = {"type": "block", "id": id} 
            if isinstance(stmt.elseStmt, BlockStatement):
                stmt.elseStmt.parent = {"type": "block", "id": id} 
        # While
        if isinstance(stmt, WhileStatement):
            if isinstance(stmt.loopBody, BlockStatement):
                stmt.loopBody.parent = {"type": "block", "id": id} 
        # For
        if isinstance(stmt, ForStatement):
            if isinstance(stmt.loopBody, BlockStatement):
                stmt.loopBody.parent = {"type": "block", "id": id}
                
    # Create Variable Table
    p[0] = block

def p_many_stmts(p):
    '''many_stmts : stmt many_stmts
                  | empty'''
    if not p[1]:
        p[0] = []
    else:
        p[2].append(p[1])
        p[0] = p[2]

def p_stmt(p):  # Listed all cases for the for loop. Starting from most matches -> least
    '''stmt : IF LPAREN expr RPAREN stmt
            | IF LPAREN expr RPAREN stmt ELSE stmt
            | WHILE LPAREN expr RPAREN stmt
            | FOR LPAREN for_cond1 SEMICOLON for_cond2 SEMICOLON for_cond3 RPAREN stmt
            | RETURN return_val SEMICOLON
            | stmt_expr SEMICOLON 
            | BREAK SEMICOLON
            | CONTINUE SEMICOLON
            | block
            | var_decl
            | SEMICOLON'''
    if p[1] == "if":
        elseExpr = SkipStatement(getLineNum(p)) if (len(p) == 6) else p[7]
        p[0] = IfStatement(p[3], p[5], elseExpr, getLineNum(p))
    elif p[1] == "while":
        p[0] = WhileStatement(p[3], p[5], getLineNum(p))
    elif p[1] == "for":
        p[0] = ForStatement(p[3], p[5], p[7], p[9], getLineNum(p))
    elif p[1] == "return":
        p[0] = ReturnStatement(p[2], getLineNum(p))
    elif p[1] == "stmt_expr":
        p[0] = ExprStatement(p[1], getLineNum(p,2))
    elif p[1] == "break":
        p[0] = BreakStatement(getLineNum(p))
    elif p[1] == "continue":
        p[0] = ContinueStatement(getLineNum(p))
    elif p[1] == "block":
        p[0] = p[1]
    elif p[1] == "var_decl":
        p[0] = p[1]
    elif p[1] == ";":
        p[0] = SkipStatement(getLineNum(p))
    else:
        p[0] = p[1]

def p_for_cond1(p):
    '''for_cond1 : stmt_expr
                 | empty'''
    if p[1]:
        p[0] = ExprStatement(p[1], getLineNum(p))

def p_for_cond2(p):
    '''for_cond2 : expr
                 | empty'''
    if p[1]:
        p[0] = p[1]
        
def p_for_cond3(p):
    '''for_cond3 : stmt_expr
                 | empty'''
    if p[1]:
        p[0] = ExprStatement(p[1], getLineNum(p))

def p_return_val(p):
    '''return_val : expr
                  | empty'''
    p[0] = p[1]

# Expressions
def p_literal(p):
    '''literal  : INT_CONST
                | FLOAT_CONST
                | STRING_CONST
                | NULL
                | TRUE
                | FALSE'''
    
    lineNum = getLineNum(p)
    if p[1] == "null":
        p[0] = NullConstantExpr(lineNum)
    elif p[1] == "true":
        p[0] = TrueConstantExpr(lineNum)
    elif p[1] == "false":
        p[0] = FalseConstantExpr(lineNum)
    elif isinstance(p[1], int):
        p[0] = IntegerConstantExpr(int(p[1]), lineNum)
    elif isinstance(p[1], float):
        p[0] = FloatConstantExpr(float(p[1]), lineNum)
    elif isinstance(p[1], str):
        p[0] = StringConstantExpr(str(p[1]), lineNum)
    
    

def p_primary(p):
    '''primary  : literal
                | THIS
                | SUPER
                | LPAREN expr RPAREN
                | NEW ID LPAREN arguments RPAREN
                | lhs
                | method_invocation'''
    if len(p) == 2:
        if p[1] == "this":
            p[0] = ThisExpr(getLineNum(p))
        elif p[1] == "super":
            p[0] = SuperExpr(getLineNum(p))
        else:
            # literal , lhs, method_invocation
            p[0] = p[1]
    # new object expr
    elif p[1] == "new":
        p[0] = NewObjectExpr(p[2],p[4], getLineNum(p))
    else:
        
        # Im not sure about this
        p[0] = p[2]
    
def p_arguments(p):
    '''arguments : expr many_comma_expr
                 | empty''' 
    if not p[1]:
        p[0] = []
    else:
        p[2].append(p[1])
        p[0] = p[2]
    
def p_many_comma_expr(p):
    '''many_comma_expr : COMMA expr many_comma_expr
                       | empty'''
    if not p[1]:
        p[0] = []
    else:
        p[3].append(p[2])
        p[0] = p[3]

def p_lhs(p):
    '''lhs  : field_access'''
    p[0] = p[1]

def p_field_access(p): 
    '''field_access : primary PERIOD ID
                    | ID'''
    if len(p) == 4:
        p[0] = FieldAccessExpr(p[1],p[3], getLineNum(p,2))
    else:
        p[0] = VariableExpr(p[1])

def p_method_inovation(p):
    '''method_invocation : field_access LPAREN arguments RPAREN'''

    fieldAcc = p[1]
    #p[4]
    p[0] = MethodCallExpr(fieldAcc.base, fieldAcc.fieldName, p[3], getLineNum(p,2))

def p_expr_simple(p):
    '''expr : primary
            | assign'''
    p[0] = p[1]

def p_expr_bin_op(p):
    '''expr : expr PLUS expr
            | expr MINUS expr
            | expr TIMES expr
            | expr DIVIDE expr
            | expr AND expr
            | expr OR expr
            | expr EEQUALS expr
            | expr NEQUALS expr
            | expr LESS expr
            | expr GREATER expr
            | expr LEQUALS expr
            | expr GEQUALS expr
            '''
    p[0] = BinaryExpr(p[1], p[3], p[2], getLineNum(p,2))

def p_expr_unary_op(p):
    '''expr : PLUS expr %prec UPLUS
            | MINUS expr %prec UMINUS
            | NOT expr'''
    p[0] = UnaryExpr(p[1], p[2], getLineNum(p))
    
def p_assign_equals(p):
    '''assign : lhs EQUALS expr'''
    p[0] = AssignExpr(p[1], p[3], getLineNum(p,2))

def p_post_inc(p):
    '''assign : lhs PLUSPLUS'''
    p[0] = AutoExpr(p[1],"inc", "post", getLineNum(p,2))

def p_pre_inc(p):
    '''assign : PLUSPLUS lhs'''
    p[0] = AutoExpr(p[2],"inc", "pre", getLineNum(p))

def p_post_dec(p):
    '''assign : lhs MINUSMINUS'''
    p[0] = AutoExpr(p[1],"dec", "post", getLineNum(p,2))

def p_pre_dec(p):
    '''assign : MINUSMINUS lhs'''
    p[0] = AutoExpr(p[2],"dec", "pre", getLineNum(p))

def p_stmt_expr(p):
    '''stmt_expr    : assign
                    | method_invocation'''
    p[0] = ExprStatement(p[1], getLineNum(p))

# Helper Productions
def p_empty(p):
    '''empty :'''
    pass

def p_error(p):
    if p:
        if not hasattr(p.lexer, "lineStart"):
            column = 0   
        else: 
            column = p.lexpos - p.lexer.lineStart
        print("Syntax error at token", p.type, 
              "at line", p.lineno,
              "at position", column)
        # Just discard the token and tell the parser it's okay.
        # parser.errok()
        print(p)
    else:
        print("Syntax error at EOF")
    exit()

def getLineNum(p, i=1):
    lineNum = " at line " + str(p.lineno(i))
    return lineNum

# Build the parser
parser = yacc.yacc()


