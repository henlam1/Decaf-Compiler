from collections import defaultdict
from decaf_ast import *

# Names         : Henry Lam    &    Sharon Guan
# Netid         : henlam            shgguan
# Student ID    : 113350850         114430436

# Subtype Dictionary
subTypes = defaultdict(set)
subTypes["int"] = set(["int"])
subTypes["float"] = set(["float"])
subTypes["float"].add("int")
subTypes["string"] = set(["string"])
subTypes["boolean"] = set(["boolean"])
subTypes["void"] = set(["void"])
subTypes["error"] = set(["error"])
subTypes["null"] = set(["null"])


# INPUT:    node
# OUTPUT:   NONE
def typeAssignExprBFS(node, curClass, blockID, index = 0):
    # Fail Cases
    if (not isinstance(node, list) and 
        not isinstance(node, ConstantExpr) and
        not isinstance(node, ExpressionRecord) and
        not isinstance(node, StatementRecord)):
        return

    # Continue Search
    # List of params
    if isinstance(node, list):
        for i, stmt in enumerate(node):
            typeAssignExprBFS(stmt, curClass, blockID, i)
    # Blocks reference values by indices
    if isinstance(node, BlockStatement):
        for i, stmt in enumerate(node.stmts):
            typeAssignExprBFS(stmt, curClass, node.id, i)

    # Statements reference valeus by attributes
    elif isinstance(node, StatementRecord):
        for var in vars(node):  # Iterates all attribute names
            child = getattr(node, var)
            typeAssignExprBFS(child, curClass, blockID) 

    # Success Cases
    elif isinstance(node, ExpressionRecord) or isinstance(node, ConstantExpr):
        for var in vars(node):  # Iterates all attribute names
            child = getattr(node, var)
            typeAssignExprBFS(child, curClass, blockID)
        typeCheckExpr(node, curClass)

# INPUT:    node - node, 
#           parent - parent of node
#           field - field the node belongs to in the parent. Either an ATTRIBUTE or BLOCK STMT INDEX
# OUTPUT:   NOTHING

def typeCheckStmtBFS(node, curMethod, blockID, index = 0):
    # Fail Cases
    if (not isinstance(node, StatementRecord)):
        return
    
   
    # Blocks reference values by indices
    if isinstance(node, BlockStatement):
        for i, stmt in enumerate(node.stmts):
            typeCheckStmtBFS(stmt, curMethod, node.id, i)

    elif isinstance(node, StatementRecord):
        for var in vars(node):  # Iterates all attribute names
            child = getattr(node, var)
            typeCheckStmtBFS(child, curMethod, blockID) 
    
    typeCheckStmt(node, curMethod)

#Check each statement
def typeCheckStmt(stmt, curMethod = None):
    # print(stmt)
    if isinstance(stmt, IfStatement):
        # print("If")
        typeCheckIf(stmt)

    elif isinstance(stmt, WhileStatement):
        # print("While")
        typeCheckWhile(stmt)

    elif isinstance(stmt, ForStatement):
        # print("For")
        typeCheckFor(stmt)

    elif isinstance(stmt, ReturnStatement):
        # print("Return")
        typeCheckReturn(stmt, curMethod)

    # Handled in BFS
    elif isinstance(stmt, ExprStatement):
        # print("Expr")
        pass

    # Handled in BFS
    elif isinstance(stmt, BlockStatement):
        # print("Block")
        pass

    elif isinstance(stmt, BreakStatement):
        # print("Break")
        pass
    
    elif isinstance(stmt, ContinueStatement):
        # print("Continue")
        pass
    
    elif isinstance(stmt, SkipStatement):
        # print("Skip")
        pass

    else:
        raise Exception("Invalid statement" + stmt.lineNum)
    return

# Check If
def typeCheckIf(stmt):
    if (stmt.ifExpr.type.name != "boolean"):
        raise Exception("If expr not boolean" + stmt.lineNum)

# Check While
def typeCheckWhile(stmt):
    if (stmt.whileExpr.type.name != "boolean"):
        raise Exception("While expr not boolean" + stmt.lineNum)

# Check For
def typeCheckFor(stmt):
    if (stmt.loopCond.type.name != "boolean"):
        raise Exception("For loop cond not boolean" + stmt.lineNum)

# Check Return
def typeCheckReturn(stmt, curMethod):
    # Get Method's Return Type
    retType = curMethod.returnType

    # If no returnExpr, assert method return type = void
    if (not stmt.returnExpr and retType.name != "void" or
        stmt.returnExpr and retType.name == "void"):
        raise Exception("Return expr void mismatch" + stmt.lineNum)
    
    # If returnExpr, assert method returned type = return type
    else:
        # Confirm both type name and type className
        stmtReturnType = stmt.returnExpr.type
        # Regular basic types int bool string etc
        if retType.name in ELEMENTARY_TYPES:
            if not typeCheckSubType(stmtReturnType.name, retType.name):
                raise Exception("Returned type " + str(stmtReturnType) + 
                                " isn't a subtype of method" + curMethod.name + 
                                " declared return type " + str(retType) + 
                                stmt.lineNum)
        # User(A) or Class-literal(A)
        else:
            if (stmtReturnType.name != retType.name or
                stmtReturnType.className != retType.className or
                not typeCheckSubType(stmtReturnType.className, retType.className)):
                raise Exception("Returned type " + str(stmtReturnType) + 
                                " isn't a subtype of method" + curMethod.name + 
                                " declared return type " + str(retType) + 
                                stmt.lineNum)

#Check each expression
def typeCheckExpr(expr, curClass = None):
    if isinstance(expr, ConstantExpr):
        # print("constant")
        typeCheckConstant(expr)
    
    elif isinstance(expr, VariableExpr):
        # print("var")
        typeCheckVar(expr)
    
    elif isinstance(expr, UnaryExpr):
        # print("unary")
        typeCheckUnary(expr)
    
    elif isinstance(expr, BinaryExpr):
        # print("binary")
        typeCheckBinary(expr)

    elif isinstance(expr, AssignExpr):
        # print("assign")
        typeCheckAssign(expr)

    elif isinstance(expr, AutoExpr):
        # print("auto")
        typeCheckAuto(expr)
    
    elif isinstance(expr, FieldAccessExpr):
        # print("fieldAcess")
        typeCheckFieldAccess(expr, curClass)
    
    elif isinstance(expr, MethodCallExpr):
        # print("methodCall")
        typeCheckMethodCall(expr,curClass)

    elif isinstance(expr, NewObjectExpr):
        # print("newObject")
        typeCheckNewObject(expr,curClass)
    
    elif isinstance(expr, ThisExpr):
        # print("this")
        typeCheckThis(expr, curClass)
    
    elif isinstance(expr, SuperExpr):
        # print("super")
        typeCheckSuper(expr, curClass)

    elif isinstance(expr, ClassReferenceExpr):
        # print("classRefExpr")
        typeCheckClassRef(expr)

    else:
        raise Exception("Invalid expr" + expr.lineNum)
    # print(expr)

    return 

def typeCheckConstant(expr):
    if expr.constantKind == "Integer-constant":
        expr.type = TypeRecord("int")      
    elif expr.constantKind == "Float-constant":
        expr.type = TypeRecord("float")
    elif expr.constantKind == "String-constant":
        expr.type = TypeRecord("string")
    elif(expr.constantKind == "True" or expr.constantKind == "False"):
        expr.type = TypeRecord("boolean")
    elif expr.constantKind == "Null":
        expr.type = TypeRecord("null")
    else:
        raise Exception("Invalid constant type" + expr.lineNum)
    
def typeCheckVar(expr):
    varRecord = varTable[expr.id]
    expr.type = varRecord.type

def typeCheckUnary(expr):
    #uminus e: has the same type as e if e's type is int or float; has type error otherwise
    if expr.operator == "MINUS" or expr.operator == "PLUS":
        if (expr.operand.type.name in ["int", "float"]):
            expr.type = expr.operand.type   
        else:
            raise Exception("Invalid UMINUS/UPLUS operand" + expr.lineNum)
    
    #neg e: has type boolean if e's type is boolean; has type error otherwise.
    elif expr.operator == "NOT":
        if expr.operand.type.name == "boolean":
            expr.type = expr.operand.type
        else:
            raise Exception("Invalid NOT operand" + expr.lineNum)
    else:
        expr.type = expr.operand.type       
       
def typeCheckBinary(expr):
    
    #is operand a string?
    operand1 = expr.operand1
    operand2 = expr.operand2
    op1Name = operand1.type.name if operand1.type.className == "" else operand1.type.className
    op2Name = operand2.type.name if operand2.type.className == "" else operand2.type.className

    arithmeticOperations = ["add", "sub", "mul", "div"]
    floatType = ["int", "float"]
    booleanOperations = ["and", "or"]
    arithmeticComparisons = ["lt", "leq", "gt", "geq"]
    equalityComparisons = ["eq", "neq"]
    
    # Arithmetic operations: add, sub, mul, div: have type int if both operands have type int; otherwise, have type float if both operands have type int or float; otherwise have type error.
    if expr.operator in arithmeticOperations:

        if (op1Name == "int" and op2Name == "int"):
            expr.type = TypeRecord("int")

        elif(op1Name in floatType and op2Name in floatType):
            expr.type = TypeRecord("float")
        else:
            raise Exception("Invalid arithmetic operand types of " + 
                            op1Name + ", " + op2Name + expr.lineNum)

    #Boolean operations and, or: have type boolean if both operands have type boolean; otherwise have type error.
    elif expr.operator in booleanOperations:
        if(op1Name == "boolean" and op2Name == "boolean"):
            expr.type = TypeRecord("boolean")
        else:
            raise Exception("Invalid boolean operand types of " + 
                            op1Name + ", " + op2Name + expr.lineNum)
    
    #Arithmetic comparisons: lt, leq, gt, geq: have type boolean if both operands have type int or float; otherwise have type error.
    elif expr.operator in arithmeticComparisons:
        if(op1Name in floatType and op2Name in floatType):
            expr.type = TypeRecord("boolean")
        else:
            raise Exception("Invalid comparison operand types of " + 
                            op1Name + ", " + op2Name + expr.lineNum)
    
    #Equality comparisons eq, neq: have type boolean if the type of one operand is a subtype of another; otherwise have type error.
    elif expr.operator in equalityComparisons:
        if typeCheckSubType(op1Name, op2Name) or typeCheckSubType(op2Name, op1Name):
            expr.type = TypeRecord("boolean")
        else:
            raise Exception("Invalid equality operand type on " + 
                            op1Name + ", " + op2Name + expr.lineNum)
    
    else:
        raise Exception("Invalid operator type of " + expr.type.name + expr.lineNum)

def typeCheckAssign(expr):
    #e1 and e2 are type correct. rhs is subtype of lhs

    lhsType, rhsType = expr.lhs.type.name, expr.rhs.type.name

    if lhsType not in ELEMENTARY_TYPES:
        lhsType = expr.lhs.type.className
    if rhsType not in ELEMENTARY_TYPES:
        rhsType = expr.rhs.type.className

    if (typeCheckSubType(rhsType, lhsType)):
        expr.type = expr.rhs.type
    else:
        raise Exception("Invalid lhs/rhs type. Trying to assign " + 
                        rhsType + " to " + lhsType + expr.lineNum)

def typeCheckAuto(expr):
    #: has the same type as its argument expression e, if e's type is int or float; has type error otherwise.
    if (expr.operand.type.name in ["int", "float"]):
        expr.type = TypeRecord(expr.operand.type.name)
    else:
        raise Exception("Invalid auto operand type" + expr.lineNum)

def getBaseType(base):
    # What can base be? How in depth do we need to be to handle base
    # This, Super
    # VariableExpr, ClassReferenceExpr, FieldAccessExpr, MethodCallExpr, NewObjExpr
    return {base.type}

def typeCheckArgs(params, args):
    if len(params) != len(args):
        return False
    
    for i, arg in enumerate(args):
        # Gather types of params and args
        if params[i].type.className == "":
            paramType = params[i].type.name
            argType = arg.type.name 
        else:
            paramType = params[i].type.className
            argType = arg.type.className
        
        # If types don't match
        if not typeCheckSubType(argType, paramType):
            return False
    
    return True

def superSearchField(fieldName: str, cls: ClassRecord):
    while True:
        for field in cls.fields:
            if field.name == fieldName:
                return field
            
        if not cls.superName: 
            return None
        cls = classTable[cls.superName]

def superSearchMethod(methodName: str, args: list, cls: ClassRecord):
    while True:
        for method in cls.methods:
            # Skip wrong names
            if method.name != methodName:
                continue

            correct = typeCheckArgs(method.parameters, args)
            if correct:
                return method
            
        if not cls.superName: 
            return None
        cls = classTable[cls.superName]

def superSearchConstr(args: list, cls: ClassRecord):
    for constr in cls.constructors:
        correct = typeCheckArgs(constr.parameters, args)
        if correct:
            return constr
    return None
            

            
def typeCheckFieldAccess(expr, curClass):
    #Let p.x be the field access expression, and let z be the field obtained by name resolution. 
    base = expr.base
    field = fieldNameResolution(expr, curClass)

    #p's type is user(A), and z is a non-static field.
    #(this does not work because Class-reference was made as a type record) need to check the string instead
    if isinstance(base, TypeRecord) and field.applicability != "instance": 
        raise Exception("base " + base.type.name + 
                        " is type user but field " + field.name + 
                        " is not non static" + expr.lineNum)
    
    #p's type is class-literal(A) and z is a static field.
    elif isinstance(base, ClassReferenceExpr) and field.applicability != "static":
        raise Exception("base is type class literal but field is not static" + expr.lineNum)
    
    # Set id of resolved field (expr.fieldID)
    expr.fieldID = field.id
    expr.type = field.type

# INPUT: Field Expr, Current Class
# OUTPUT: Field Record
def fieldNameResolution(expr, curClass):
    # 1. check what is base's type e.g Instance Fields(this, super) or Class Fields(user(A), class-literal(A))
    # 2. check for field in the base
    #     a. Instance Fields - Use current class/super of current
    #     b. Class Fields - Use class name
    # 3. found? return
    # 4. not found?
    #     a. Instance Fields - Return Error
    #     b. Class Fields - Look at their class/super class
    #           i.
    base = expr.base
    fieldName = expr.fieldName

    if isinstance(base, ThisExpr) or isinstance(base, SuperExpr) or isinstance(base, VariableExpr) or isinstance(base, ClassReferenceExpr):
        pass
    else:
        raise Exception("Invalid base type of " + str(type(base)) + expr.lineNum)

    # Instance Fields
    if isinstance(base, ThisExpr):
        # Starting point is curClass
        className = curClass.name

    elif isinstance(base, SuperExpr):
        # Starting point is super class
        className = curClass.superName
    
    # Class Fields
    elif isinstance(base, VariableExpr):
        base = base.type
        className = base.className
    
    elif isinstance(base, ClassReferenceExpr):
        className = base.className

    # No Class Found
    if className not in classTable:
        raise Exception("Class name " + className + " not defined" + expr.lineNum)
    
    # Go up superclass chain
    foundField = superSearchField(fieldName, classTable[className])
    if not foundField: raise Exception("Field not found in " + className + expr.lineNum)

    return foundField
        
def typeCheckMethodCall(expr,curClass):

    # Set id of resolved method (expr.methodID)
    #expr.type = expr.base.type

    base = expr.base
    method = methodCallNameResolution(expr,curClass)

    #p's type is user(A), and z is a non-static field.
    if isinstance(base, TypeRecord) and method.applicability != "instance":
        raise Exception("base is type user but field is not non static" + expr.lineNum)
    #p's type is class-literal(A) and z is a static field.
    if isinstance(base, ClassReferenceExpr) and method.applicability != "static":
    #if("class-literal(" in str(base.type) and method.applicability != "static"):
        raise Exception("base is type class literal but field is not static" + expr.lineNum)
    
    # Set id of resolved field (expr.methodID)
    expr.methodID = method.id
    expr.type = method.returnType

def methodCallNameResolution(expr, curClass):
    base = expr.base
    methodName = expr.methodName
    args = expr.arguments
    # print(expr)
    # print(type(base))
    # What can base be? How in depth do we need to be to handle base
    # This, Super
    # VariableExpr, ClassReferenceExpr, FieldAccessExpr, MethodCallExpr, NewObjExpr

    if isinstance(base, ThisExpr) or isinstance(base, SuperExpr) or isinstance(base, VariableExpr) or isinstance(base, ClassReferenceExpr) or isinstance(base, FieldAccessExpr):
        pass
    else:
        raise Exception("Invalid base type " + str(type(base)) + expr.lineNum)

   # Instance Fields
    if isinstance(base, ThisExpr):
        # Starting point is curClass
        className = curClass.name

    elif isinstance(base, SuperExpr):
        # Starting point is super class
        className = curClass.superName
    
    # Class Fields
    elif isinstance(base, VariableExpr):
        base = base.type
        className = base.className
    
    elif isinstance(base, ClassReferenceExpr):
        className = base.className

    elif isinstance(base, FieldAccessExpr):
        className = base.type.className
    
    # No Class Found
    if className not in classTable:
        raise Exception("Class name " + className + " not defined")

    foundMethod = superSearchMethod(methodName, args, classTable[className])
    if foundMethod is None: 
        raise Exception("Method " + methodName + 
                        " not found. Check the name/arguments" + expr.lineNum)
    
    return foundMethod      

def typeCheckNewObject(expr,curClass):

    base = expr.base
    object =  newObjectNameResolution(expr,curClass)

    #The type of this expression is user(A) if name resolution succeeds (error, otherwise).
    
    # Set id of resolved constructor (expr.constrID)
    expr.constrID = object.id
    expr.type = TypeRecord("user", base)

def newObjectNameResolution(expr, curClass):
    className = expr.base
    args = expr.arguments

    if className not in classTable:
        raise Exception("Class name " + className + " not defined" + expr.lineNum)

    foundConstr = superSearchConstr(args, classTable[className])
    if foundConstr is None: 
        raise Exception("Method " + className + 
                        " not found. Check the name/arguments" + expr.lineNum)
    
    return foundConstr      

def typeCheckThis(expr, curClass):
    expr.type = TypeRecord("user", curClass.name)

def typeCheckSuper(expr, curClass):
    if not curClass.superName:
        raise Exception("Missing super class " + curClass.superName + expr.lineNum)
    expr.type = TypeRecord("user", curClass.superName)

def typeCheckClassRef(expr):
    if not expr.className in classTable:
        raise Exception("Missing class declaration, " + expr.className + expr.lineNum)
    expr.type = TypeRecord("class-literal", expr.className)

# Check if t1 is a subtype of t2
def typeCheckSubType(t1, t2):
    return t1 in subTypes[t2]