ELEMENTARY_TYPES = ["int", "float", "string", "boolean", "void", "error", "null"]

# Tables
classTable = {} # uses name as key
constrTable = {} # uses ID as key
methodTable = {} # uses ID as key
blockTable = {} # uses ID as key
fieldTable = {} # uses ID as key
varTable = {} # uses ID as key

# AST Nodes
class Node():
    def __init__(self):
        self.parent = None

    def parentCount(self):
        count = 0
        current = self.parent
        while current is not None:
            count += 1
            current = current.parent
        return count

    def __str__(self) -> str:
        pass

# # Declarations
# 2. Program
class Program(Node):
    def __init__(self, classes = []):
        super().__init__()
        self.classes = classes[::-1]

    def eval(self):
        results = []
        for node in self.classes:
            results.append(node.eval(self.names))
        
        return results
        
    def __str__(self):
        res = ""
        for node in self.classes:
            res += "\n" + str(node)
        return res

# Class Record:
# • Class name: Name of the class
# • Super class name: Name of the super class (if any) of the current class.
# • Class constructors: The set of constructors defined in the class.
# • Methods: The set of methods defined in the class.
# • Fields: The set of fields defined in the class. 

class ClassRecord:
    def __init__(self, name = "", superName="", constructors=[], methods=[], fields=[]):
    # Do we need line number and column number?
    # Each class record should have its own scope
        self.name = name
        self.superName = superName
        self.constructors = constructors[::-1]
        self.methods = methods[::-1]
        self.fields = fields[::-1]

        # Error Checking
        if len(self.constructors) > 1:
            raise Exception("Class " + self.name + " has multiple constructors")
        # Add Children
        for method in self.methods:
            method.containingClass = self.name
        for field in self.fields:
            field.containingClass = self.name

    def __str__(self) -> str:
        # call str methods of children (constructors/methods/fields)
        res = "--------------------------------------------------------------------------"
        res += "\n" + "Class Name: " + self.name
        res += "\n" + "Superclass Name: " + self.superName
        res += "\n" + "Fields:"
        for field in self.fields:
            res += "\n" + str(field)
        res += "\n" + "Constructors:"
        for constr in self.constructors:
            res += "\n" + str(constr)
        res += "\n" + "Methods:"
        for method in self.methods:
            res += "\n" + str(method)
        return res
        

# Constructor Record:
# • Constructor id: Note that the name of a constructor in Decaf is identical to its containing class. So, we
# will identify each constructor by a unique integer id. This id should be unique across the entire program:
# no two constructors, irrespective of their containing class, should have the same id.
# • Constructor visibility: which encodes the visibility (public/private) of the constructor.
# • Constructor parameters: sequence of formal parameters of the constructor. Each parameter is a variable
# (a reference to a variable in the variable table, described later).
# • Variable table: A table of variables containing information on all the formal parameters and local
# variables of the constructor (described later).
# • Constructor body: is a statement (an instance of a statement record, described later)

class ConstructorRecord:
    def __init__(self,id=1, visibility="private", parameters=[], variableTable={}, body=[]):
        super().__init__()
        self.id = id
        self.visibility = visibility
        self.parameters = parameters
        self.variableTable = variableTable
        self.body = body

    def __str__(self) -> str:
        res = "CONSTRUCTOR: " + str(self.id) + ", " + self.visibility
        formalIDs =  []
        for value in self.variableTable.values():
            if value.variableKind == "formal":
                formalIDs.append(str(value.id))
        res += '\n' + "Constructor Parameters: " + ", ".join(formalIDs)
        res += '\n' + "Variable Table: "
        for value in self.variableTable.values():
        #if value.variableKind == "local":
            res += '\n' + "VARIABLE: " + str(value)
        res += '\n' + "Constructor Body: "
        res += str(self.body)
        return res

# Method Record:
# • Method name: name of this method.
# • Method id: A unique integer id for this method. This id should be unique across the entire program: no
# two methods, irrespective of their containing class, should have the same id.
# • Containing class: the name of the class where this method is defined.
# • Method visibility: which encodes the visibility (public/private) of the method.
# • Method applicability: which describes whether this is a class method (i.e. static) or an instance
# method (non-static).
# • Method parameters: sequence of formal parameters of the method. Each parameter is a variable (a
# reference to a variable in the variable table, described later).
# • Return type: an instance of a type record (described later) that describes the declared return type of this
# method. This information is omitted for void methods (i.e. those that return no value).
# • Variable table: A table of variables containing information on all the formal parameters and local
# variables of the method (described later).
# • Method body: is a statement (an instance of a statement record, described later). 
class MethodRecord:
    def __init__(self, name="", id=1, containingClass="", visibility="private", applicability="", parameters=[], returnType=None, variableTable={}, body = []):
        super().__init__()
        self.name = name
        self.id = id
        self.containingClass = containingClass
        self.visibility = visibility
        self.applicability = applicability
        self.parameters = parameters
        self.returnType = returnType
        self.variableTable = variableTable
        self.body = body
        
    def __str__(self) -> str:
        csl = [self.id, self.name, self.containingClass, self.visibility, self.applicability, self.returnType]
        res = "METHOD: " + ", ".join(str(attr) for attr in csl if attr) #Join attributes if non-empty
        formalIDs =  []
        for value in self.variableTable.values():
            if value.variableKind == "formal":
                formalIDs.append(str(value.id))
        res += '\n' + "Method Parameters: " + ", ".join(formalIDs)
        res += '\n' + "Variable Table:"
        for value in self.variableTable.values():
            res += '\n' + "VARIABLE: " + str(value)
        res += '\n' + "Method Body: "
        res += str(self.body)
        return res

# Fields
# Field Record:
# • Field name: name of this field.
# • Field id: A unique integer id for this field. This id should be unique across the entire program: no two
# fields, irrespective of their containing class, should have the same id.
# • Containing class: the name of the class where this field is defined.
# • Field visibility: which encodes the visibility (public/private) of the field.
# • Field applicability: which describes whether this is a class field (i.e. static) or an instance field (nonstatic).
# • Type: an instance of a type record (described later) that describes the declared type of this field. 
class FieldRecord:
    def __init__(self, name = "", id=1, containingClass="", visibility="", applicability="", type=""):
    # Do we need line number and column number?
        self.name = name
        self.id = id
        self.containingClass = containingClass
        self.visibility = visibility
        self.applicability = applicability
        self.type = type
    
    def __str__(self) -> str:
        csl = [self.id, self.name, self.containingClass, self.visibility, self.applicability, self.type]
        res = "FIELD " + ", ".join(str(attr) for attr in csl if attr)
        return res 

class DeclareVariable:
    def __init__(self,type=None, names=""):
        self.type = type
        self.names = names[::-1]
    
    def __str__(self) -> str:
        return ""

class FormalParam:
    def __init__(self,type, name):
        self.type = type
        self.name = name
    
    def __str__(self) -> str:
        return ""
    
# Variable Record:
# • Variable name: Name of the variable
# • Variable id: A integer that forms an unique id for this variable. No two distinct variables in a constructor
# or method should have the same id. Note that scopes determine when two variables are distinct; this issue is described in detail later in this document. Note that the uniqueness is only within the constructor/method; two distinct variables in two different methods may have the same id.
# • Variable kind: (formal/local) indicating whether this variable is a formal parameter or a local variable. 
# • Type: an instance of a type record (described later) that describes the declared type of this variable.
class VariableRecord:
    
    def __init__(self,name="", id = -1,variableKind="", type=""):
    #Do we need line number and column number
        self.name=name
        self.id= id
        self.variableKind=variableKind
        self.type = type

    
    def __str__(self) -> str:
        attrs = [self.id, self.name, self.variableKind, self.type]
        attrs = [str(attr) for attr in attrs]
        res = ", ".join(attrs)
        return res 

# Type Record: Contents
# • name: the name of the type
# • className: the name of class referenced
class TypeRecord:
    def __init__(self, name, className=""):
        self.name = name
        self.className = className
    
    def __str__(self) -> str:
        if self.name in ELEMENTARY_TYPES:
            return self.name
        else:
            return f"{self.name}({self.className})"
        # if self.name == "user":
        #     return f"user({self.name})"
        # if self.name == "class-literal":
        #     return f"class-literal({self.name})"

# 3. Statements
# 1. If-stmt: has three pieces of information:
# • the condition of the "if", which is an expression (described later),
# • the "then" part, which is another statement, and
# • the "else" part, which is another statement.
# 2. While-stmt: has two pieces of information:
# • the loop-condition of the "while", which is an expression (described later),
# • the loop body, which is another statement.
# 3. For-stmt: has four pieces of information:
# • the initializer expression, which is an expression (described later),
# • the loop condition, which is another expression (described later),
# • the update expression, which is another expression (described later),
# • the loop body, which is another statement.
# 4. Return-stmt: has one optional piece of information:
# • the return value, specified by an expression (described later).
# 5. Expr-stmt: has one piece of information:
# • the expression that comprises of this statement.
# 6. Block-stmt: has one piece of information:
# • a sequence of statements that comprise of this block.
# 7. Break-stmt: representing break.
# 8. Continue-stmt: representing continue.
# 9. Skip-stmt: representing an empty statement (as in an empty else part of an "if" statement). 
# Each statement record additionally contains the line number range in the input corresponding to it. (This
# information may be used in later assignments for signaling additional errors/warnings).

class StatementRecord:
    def __init__(self, stmtKind="", lineNum=None):
        self.stmtKind = stmtKind
        self.lineNum = lineNum
        self.typeCorrect = True
    
    def __str__(self) -> str:
        res = "\n" + self.stmtKind
        return res

class IfStatement(StatementRecord):
    def __init__(self, ifExpr, thenStmt, elseStmt, lineNum) -> None:
        super().__init__("If", lineNum)
        self.ifExpr = ifExpr
        self.thenStmt = thenStmt
        self.elseStmt = elseStmt

    def __str__(self) -> str:
        res = super().__str__()

        res+= '('+str(self.ifExpr)
        if(self.thenStmt is not None):
            res+= ", Then( "+str(self.thenStmt)+' )'
        if(self.elseStmt is not None):
            res+= ", Else( "+str(self.elseStmt)+' )'
        res+=')'

        # attrs = [self.ifExpr, self.thenStmt, self.elseStmt]
        # attrs = [str(attr) for attr in attrs]
        # res += '(' + ", ".join(attrs) + ')'
        return res 

class WhileStatement(StatementRecord):
    def __init__(self, whileExpr, loopBody, lineNum) -> None:
        super().__init__("While", lineNum)
        self.whileExpr = whileExpr
        self.loopBody = loopBody

    def __str__(self) -> str:
        res = super().__str__()
        attrs = [self.whileExpr, self.loopBody]
        attrs = [str(attr) for attr in attrs]
        res += '(' + ", ".join(attrs) + ')'
        return res 
    
class ForStatement(StatementRecord):
    def __init__(self, initExpr, loopCond, updateExpr, loopBody, lineNum) -> None:
        super().__init__("For", lineNum)
        self.initExpr = initExpr
        self.loopCond = loopCond
        self.updateExpr = updateExpr
        self.loopBody = loopBody

    def __str__(self) -> str:
        res = super().__str__()
        attrs = [self.initExpr, self.loopCond, self.updateExpr, self.loopBody]
        attrs = [str(attr) for attr in attrs]
        res += '(' + ", ".join(attrs) + ')'
        return res 
    
class ReturnStatement(StatementRecord):
    def __init__(self, returnExpr, lineNum) -> None:
        super().__init__("Return", lineNum)
        self.returnExpr = returnExpr

    def __str__(self) -> str:
        res = super().__str__()
        res += '(' + str(self.returnExpr) + ')'
        return res 
    
class ExprStatement(StatementRecord):
    def __init__(self, expr, lineNum) -> None:
        super().__init__("Expr", lineNum)
        self.expr = expr

    def __str__(self) -> str:
        res = super().__str__()
        res += '(' + str(self.expr) + ')'
        return res 
    
class BlockStatement(StatementRecord):
    def __init__(self, id, parent, variableTable, stmts, lineNum) -> None:
        super().__init__("Block", lineNum)
        self.id = id
        self.parent = parent
        self.variableTable = variableTable
        self.stmts = stmts[::-1]

    def __str__(self) -> str:
        res = super().__str__()
        stmts = ", ".join([str(stmt) for stmt in self.stmts if str(stmt)])
        #res += f" Id: {self.id} Parent type: {self.parent['type']} Parent id: {self.parent['id']}"
        res += '([' + stmts + '\n])'
        return res 

class BreakStatement(StatementRecord):
    def __init__(self, lineNum) -> None:
        super().__init__("Break", lineNum)

    def __str__(self) -> str:
        res = super().__str__()
        res += '(' + ')'
        return res 

class ContinueStatement(StatementRecord):
    def __init__(self, lineNum) -> None:
        super().__init__("Continue", lineNum)

    def __str__(self) -> str:
        res = super().__str__()
        res += '(' + ')'
        return res 
    
class SkipStatement(StatementRecord):
    def __init__(self, lineNum) -> None:
        super().__init__("Skip", lineNum)

    def __str__(self) -> str:
        res = super().__str__()
        res += '(' + ')'
        return res 

# Each expression record additionally contains the line number range in the input corresponding to it. (This
# information may be used in later assignments for signaling additional errors/warnings).

#Expressions 
# When do i call this?
class ExpressionRecord:
    
    def __init__(self, expressionKind="", lineNum=None):
        self.expressionKind = expressionKind
        
        self.lineNum = lineNum
    
        self.type = None
  

    def __str__(self) -> str:
            res = self.expressionKind
            return res

# 1. Constant-expression: which, in turn, is one of the following six forms:
class ConstantExpr:
    def __init__(self, constantKind=None, lineNum=None):
        self.constantKind = constantKind
        
        self.lineNum = lineNum

        self.type = None
            
    def __str__(self, info) -> str:
        res = "Constant" + '(' + self.constantKind + '(' + info + ')' + ')'
        return res 
    
# • Integer-constant: with the integer value as its information.
class IntegerConstantExpr(ConstantExpr):
    def __init__(self, info=None, lineNum=None):
        super().__init__("Integer-constant", lineNum)
        self.info = info
        self.type = TypeRecord("int")
        
        self.lineNum = lineNum
            
    def __str__(self) -> str:
        res = super().__str__(str(self.info))
        return res 

# • Float-constant: with the floating-point value as its information.
class FloatConstantExpr(ConstantExpr):
    def __init__(self, info=None, lineNum=None):
        super().__init__("Float-constant", lineNum)
        self.info = info
        self.type = TypeRecord("float")
       
        self.lineNum = lineNum
        
    def __str__(self) -> str:
        res = super().__str__(str(self.info))
        return res 
    
# • String-constant: with the string value as its information.
class StringConstantExpr(ConstantExpr):
    def __init__(self, info=None, lineNum=None):
        super().__init__("String-constant", lineNum)
        self.info = info
        self.type = TypeRecord("string")
        
        self.lineNum = lineNum
            
    def __str__(self) -> str:
        res = super().__str__(str(self.info))
        return res 
    
# • Null, True, and False: with no additional information.
class NullConstantExpr(ConstantExpr):
    def __init__(self, lineNum=None):
        super().__init__("Null", lineNum)
        self.info = "Null"
        self.type = TypeRecord("null")
        
        self.lineNum = lineNum
        
    def __str__(self) -> str:
        res = "Constant(Null)"
        return res 
    
class TrueConstantExpr(ConstantExpr):
    def __init__(self, lineNum=None):
        super().__init__("True", lineNum)
        self.info = "True"
        self.type = TypeRecord("boolean")
        
        self.lineNum = lineNum
        
    def __str__(self) -> str:
        res = "Constant(True)"
        return res 

class FalseConstantExpr(ConstantExpr):
    def __init__(self, lineNum=None):
        super().__init__("False", lineNum)
        self.info = "False"
        self.type = TypeRecord("boolean")

        self.lineNum = lineNum
            
    def __str__(self) -> str:
        res = "Constant(False)"
        return res 
    
# 2. Var-expression: has one piece of information: the id of the referenced variable. Note that if there are
# multiple variables of the same name defined in nested blocks, the scope rules (defined later) specify
# which if those variables will be the referenced one. 
class VariableExpr(ExpressionRecord):
    def __init__(self, id, lineNum=None):
        super().__init__("Variable", lineNum)
        self.id = id
        
        self.lineNum = lineNum
 
    def __str__(self) -> str:
        res = super().__str__()
        res += '(' + str(self.id) + ')'
        return res 
    
 

# 3. Unary-expression: has two pieces of information: the operand (an expression) and the unary operator.
# Unary operators are either uminus or neg. Note that the unary operator "+" in the concrete syntax has
# no effect. So an expression of the form "+25" will be represented as though it was simply "25".
class UnaryExpr(ExpressionRecord):
    def __init__(self, operator = "",operand = "",lineNum=None):
        super().__init__("Unary", lineNum)
        self.operator = self.operatorToWord(operator)
        self.operand = operand
        
        self.lineNum = lineNum
    
    def operatorToWord(self, operator):
        if operator == "+":
            return "PLUS"
        if operator == "-":
            return "MINUS"
        if operator == "!":
            return "NOT"
        
    def __str__(self) -> str:
        res = super().__str__()
        res += '(' 
        res+= str(self.operator)
        res += ", "
        res+= str(self.operand)
        res+= ')'
        return res 



# 4. Binary-expression: has three pieces of information: the two operands (both expressions themselves) and
# the binary operator. Binary operators are one of add, sub, mul, div, and, or, eq, neq, lt, leq, gt,
# and geq.
class BinaryExpr(ExpressionRecord):
    def __init__(self, operand1 = "",operand2 = "" ,operator = "",lineNum=None):
        super().__init__("Binary", lineNum)
        self.operand1 = operand1
        self.operand2 = operand2
        self.operator = self.operatorToWord(operator)
        
        self.lineNum = lineNum
    
    def operatorToWord(self, operator):
        if operator == '+':
            return "add"
        if operator == '-':
            return "sub"
        if operator == '*':
            return "mul"
        if operator == '/':
            return "div"
        if operator == "&&":
            return "and"
        if operator == "||":
            return "or"
        if operator == "==":
            return "eq"
        if operator == "!=":
            return "neq"
        if operator == '<':
            return "lt"
        if operator == '<=':
            return "leq"
        if operator == '>':
            return "gt"
        if operator == '>=':
            return "geq"
        
    def __str__(self) -> str:
        res = super().__str__()
    
        res += '(' 
        res+= str(self.operator)
        res += ", "
        res+= str(self.operand1)
        res+= ", "
        res+= str(self.operand2)
        res+= ')'
        return res 

# 5. Assign-expression: has two pieces of information: the left- and right- hand sides of the assignment (both
# expressions).
class AssignExpr(ExpressionRecord):
    def __init__(self, lhs="", rhs="",lineNum=None):
        super().__init__("Assign", lineNum)
        self.lhs = lhs
        self.rhs = rhs

        self.lineNum = lineNum
        
    def __str__(self) -> str:
        res = super().__str__()
    
        res += '(' 
        res += str(self.lhs)
        res += ", "
        res += str(self.rhs)


        # INCLUDE THIS PART FOR STEP 4
        res += ", "
        res += str(self.lhs.type)
        res += ", "
        res += str(self.rhs.type)


        res+= ')'
        return res 



# 6. Auto-expression: has three pieces of information to represent auto-increment and auto-decrement
# expressions (e.g. "x++"). The three pieces of information are the operand (e.g. "x") which is an
# expression, whether the operation is an auto-increment (e.g. x++) or auto-decrement (e.g x--), and
# whether the operation is post (e.g. x++) or pre (e.g. ++x).
class AutoExpr(ExpressionRecord):
    def __init__(self, operand = "",operation = "" ,order = "",lineNum=None):
        #might not need super() because its not inheriting everything
        super().__init__("Auto", lineNum)
        self.operand = operand
        self.operation = operation
        self.order = order
        
        self.lineNum = lineNum
        
    def __str__(self) -> str:
        res = super().__str__()
    
        res += '(' 
        res+= str(self.operand)
        res += ", "
        res+= self.operation
        res+= ", "
        res+= self.order
        res+= ')'
        return res 
    

#7. Field-access-expression: has two pieces of information to represent p.x: the base (e.g. p), which is an
# expression, and the field name (e.g. x), which is a string.

class FieldAccessExpr(ExpressionRecord):
    def __init__(self,base="",fieldName = "", lineNum=None):
        super().__init__("Field-access", lineNum)
        self.base = base
        self.fieldName = fieldName
        self.fieldID = -1

        self.lineNum = lineNum

    def __str__(self) -> str:
        res = super().__str__()

        res += '('
        
        if not(self.base is None):
            res+= str(self.base)
            res+= ', '

        res+= self.fieldName
        res+= ', '
        res+= str(self.fieldID)
        res+= ')'

        return res 
    

# 8. Method-call-expression: has three pieces of information to represent p.f(x, y): the base (e.g. p),
# which is an expression, the method name (e.g. x), which is a string, and a sequence of expressions
# representing the arguments to the method call (e.g. x, y). Note that the argument sequence may be
# empty.
class MethodCallExpr(ExpressionRecord):
    def __init__(self,base="",methodName = "", arguments= None,lineNum=None):
        super().__init__("Method-call", lineNum)
        
        self.base = base
        self.methodName = methodName
        self.methodID = -1
        
        if arguments is None:
            self.arguments = []
        else:
            self.arguments = arguments
        
        self.lineNum = lineNum

    def __str__(self) -> str:
        res = super().__str__()
        args = [str(arg) for arg in self.arguments]
        args = '[' + ", ".join(args) + ']'
        csl = [str(attr) for attr in [self.base, self.methodName, args, self.methodID]]
        res += '(' + ", ".join(csl) + ')'

        return res 
        



# 9. New-object-expression: has two pieces of information for creating a new object as in new a(i, x):
# the base class name (e.g. a), and the sequence of expressions representing the arguments to the
# constructor (e.g. i, x). Note that the argument sequence may be empty.
class NewObjectExpr(ExpressionRecord):
    def __init__(self,base="",arguments=None, lineNum=None):
        super().__init__("New-object", lineNum)
        self.base = base
        self.arguments = arguments
        self.constrID = -1

        if arguments is None:
            self.arguments = []
        else:
            self.arguments = arguments
            
        self.lineNum = lineNum
    
    def __str__(self) -> str:
        res = super().__str__()
        res += '(' 
        
        if not(self.base is None):
            if isinstance(self.base,str):
                res += str(self.base) 
                res+= ','
        
        res+= ' ['
        if isinstance(self.arguments, list) and len(self.arguments) > 0:
            res+= ", ".join(str(argument) for argument in self.arguments)

        res+= "]"
        res+= ", "
        res+= str(self.constrID)
        res+= ')'
        return res 
    
# 10. This-expression: to denote this.
class ThisExpr(ExpressionRecord):
    def __init__(self,lineNum = None):
        super().__init__("This", lineNum)
        
        self.lineNum = lineNum
    
    def __str__(self) -> str:
        res = "This"
        return res

# 11. Super-expression: to denote super.
class SuperExpr(ExpressionRecord):
    def __init__(self,lineNum = None):
        super().__init__("Super", lineNum)
        
        self.lineNum = lineNum
    
    def __str__(self) -> str:
        res = "Super"
        return res

# 12. Class-reference-expression: with the referred class name as its information. This expression is used to
# denote the value of literal class names.
class ClassReferenceExpr(ExpressionRecord):
    # Do with setattr(object, attr) and getattr(object, attr, value)
    # 1 - Make tree traversal
    # 2 - Look for nodes that are VariableExpr
    # 3 - Check if variableExpr name is a className
    def __init__(self,className="", lineNum=None):
        super().__init__("Class-reference", lineNum)
        self.className =className
        self.lineNum = lineNum
    
    def __str__(self) -> str:
        res = super().__str__()
        res += '(' + self.className + ')'
        return res


#In and Out Classes
scanInt = MethodRecord("scan_int", 1, "In", "public", "static", [], TypeRecord("int"), {}, BlockStatement(id , -1, [], [], -1))
scanFloat = MethodRecord("scan_float", 2, "In", "public", "static", [], TypeRecord("float"), {}, BlockStatement(id , -1, [], [], -1))
methodTable[1] = scanInt
methodTable[2] = scanFloat
classTable["In"] = ClassRecord("In", 
                               None, 
                               [], 
                               [scanInt, scanFloat],
                               []
                                )

printInt = MethodRecord("print", 3, "Out", "public", "static", [VariableRecord("i", 1, "formal", TypeRecord("int"))], TypeRecord("int"), {}, BlockStatement(id , -1, [], [], -1))
printFloat = MethodRecord("print", 4, "Out", "public", "static", [VariableRecord("f", 2, "formal", TypeRecord("float"))], TypeRecord("float"), {}, BlockStatement(id , -1, [], [], -1))
printBool = MethodRecord("print", 5, "Out", "public", "static", [VariableRecord("b", 3, "formal", TypeRecord("boolean"))], TypeRecord("boolean"), {}, BlockStatement(id , -1, [], [], -1))
printString = MethodRecord("print", 6, "Out", "public", "static", [VariableRecord("s", 4, "formal", TypeRecord("string"))], TypeRecord("string"), {}, BlockStatement(id , -1, [], [], -1))
methodTable[3] = printInt
methodTable[4] = printFloat
methodTable[5] = printBool
methodTable[6] = printString
classTable["Out"] = ClassRecord("Out", 
                                None, 
                                [], 
                                [printInt, printFloat, printBool, printString],
                                []
                                )

        