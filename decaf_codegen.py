from decaf_ast import *
from decaf_absmc import *

ARITHOPS = ["add", "sub", "mul", "div"]
BOOLOPS = ["and", "or"]
ARITHCOMPS = ["lt", "leq", "gt", "geq"]
EQUALITYCOMPS = ["eq", "neq"]

postIncrements = []

isStatic = 0
prevLabel = ""
nextLabel = ""
absmc = ABSMC()

def main():
    #absmc.allocStaticFields()

    for c in classTable:
        #iterate through classes and generate code
        codeGenClass(classTable[c])
    return absmc.codeStr

def codeGenClass(curClass):
    #iterate through all constructors
    #The first instruction in this sequence will be labeled as C_%d, where %d is the unique id of the constructor
    instanceCount = 0
    staticCount = 0
    for f in curClass.fields:
        print(f)
        if f.applicability == "static":
            staticCount += 1
        else:
            instanceCount += 1

    for c in curClass.constructors:
        absmc.addLabel("C_%d" %(c.id))
        codeGenConstructor(c)
        absmc.addNewLine()

    #iterate through all methods
    #This label will be of the form "M_%s_%d", where the %s argument will be the name of the method, and %d will be the unique id of the method. 
    
    #check if the method is main?
    for m in curClass.methods:   
        absmc.addLabel("M_%s_%d" %(m.name, m.id))
        codeGenMethod(m)
        absmc.addNewLine()



def codeGenConstructor(constructor):
    #This constructor will be compiled to an initializer function that takes a reference to a newly created object as a0
    #The value of formal parameter i as a1

    # absmc.resetRegisterCount()

    absmc.argRegisterCount = len(constructor.parameters)
    absmc.tempregistercount = len(constructor.variableTable)

    codeGenBody(constructor.body)

    #constructor does not return?
    return

def codeGenMethod(method):
    # absmc.resetRegisterCount()
    
    #For static methods, the value of the first parameter is in a0, second is in a1, and so on
    #For instance methods, the implicit object (accessed using "this") is in a0, the first parameter value in a1, the second in a2, and so on 
    absmc.argRegisterCount = len(method.parameters)
    #Temporary registers are used to hold all local variables as well as all intermediate values computed while evaluating an expression.
    absmc.tempRegisterCount = len(method.variableTable)

    global isStatic
    if method.applicability == "static":
        isStatic = 1
    
    codeGenBody(method.body)
    isStatic = 0
    
    return

def codeGenBody(body):
    for stmt in body.stmts:
        codeGenStatement(stmt)

def codeGenStatement(stmt):
    if isinstance(stmt, IfStatement):
        codeGenIfStmt(stmt)
    elif isinstance(stmt, WhileStatement):
        codeGenWhileStmt(stmt)
    elif isinstance(stmt, ForStatement):
        codeGenForStmt(stmt)
    elif isinstance(stmt, ReturnStatement):
        codeGenReturnStmt(stmt)
    elif isinstance(stmt, BlockStatement):
        
        codeGenBlockStmt(stmt)
    elif isinstance(stmt, ExprStatement):
        codeGenExprStmt(stmt)
    elif isinstance(stmt, BreakStatement):
        codeGenBreakStmt(stmt)
    elif isinstance(stmt, ContinueStatement):
        codeGenContinueStmt(stmt)
    elif isinstance(stmt, SkipStatement):
        pass
    return

def codeGenIfStmt(stmt):
    print("In If Stmt")
    ifExpr = stmt.ifExpr
    thenStmt = stmt.thenStmt
    elseStmt = stmt.elseStmt
    r1 = codeGenExpr(ifExpr)

    #if condition is true jump to label
    label1 = absmc.incLabelCounter()
    absmc.bz(r1, label1)
    codeGenStatement(thenStmt)

    label2 = absmc.incLabelCounter()
    absmc.jmp(label2)
    absmc.addLabel(label1)
    codeGenStatement(elseStmt)
  
    absmc.addLabel(label2)    
    return

def codeGenWhileStmt(stmt):
    whileExpr = stmt.whileExpr
    body = stmt.loopBody

    label1 = absmc.incLabelCounter()
    global prevLabel
    prevLabel = label1

    label2 = absmc.incLabelCounter()
    global nextLabel
    nextLabel = label2

    absmc.addLabel(label1)
    r1 = codeGenExpr(whileExpr)
    absmc.bz(r1, nextLabel)

    codeGenStatement(body)
    absmc.jmp(label1)

    absmc.addLabel(label2)

    return

def codeGenForStmt(stmt):
    initExpr = stmt.initExpr
    loopCond = stmt.loopCond
    updateExpr = stmt.updateExpr
    loopBody = stmt.loopBody

    r1 = codeGenExpr(initExpr)
    label1 = absmc.incLabelCounter()
    label2 = absmc.incLabelCounter()

    global nextLabel
    nextLabel = label2

    r2 = codeGenExpr(loopCond)
    label3 = absmc.incLabelCounter()

    absmc.bz(r2, label2)

    global prevLabel
    prevLabel = label3

    codeGenStatement(loopBody)
    absmc.addLabel(label3)

    codeGenExpr(updateExpr)
    absmc.jmp(label1)
    absmc.addLabel(label2)
 




    return


def codeGenReturnStmt(stmt):
    returnExpr = stmt.returnExpr

    r1 = codeGenExpr(returnExpr)
    if returnExpr is not None:
        absmc.move("a0", r1)
    absmc.ret()
    return

def codeGenBlockStmt(stmt):
    print("in code gen block")
    stmts = stmt.stmts
    for s in stmts:
        print(s)
        print("----")
        codeGenStatement(s)
    return

def codeGenExprStmt(stmt):
    codeGenExpr(stmt.expr)
    return

def codeGenBreakStmt(stmt):
    global prevLabel
    absmc.jmp(prevLabel)
    return

def codeGenContinueStmt(stmt):
    global nextLabel
    absmc.jmp(nextLabel)
    return

def codeGenExpr(expr):
    print("@codeGenExpr")
    
    if isinstance(expr, ConstantExpr):
        print("print ConstantExpr")
        return codeGenConstant(expr)
    
    elif isinstance(expr, VariableExpr):
        print("print variable expr")
        return codeGenVariable(expr)
    
    elif isinstance(expr, AssignExpr):
        print("print AssignExpr")
        return codeGenAssign(expr)
    
    elif isinstance(expr, UnaryExpr):
        print("print unary expr")
        return codeGenUnary(expr)
    
    elif isinstance(expr, BinaryExpr):
        print("print binary expr")
        return codeGenBinary(expr)
    
    elif isinstance(expr, AutoExpr):
        print("print auto expr")
        return codeGenAuto(expr)
    
    elif isinstance(expr, FieldAccessExpr):
        print("print field access expr")
        return codeGenFieldAccess(expr)
    
    elif isinstance(expr, MethodCallExpr):
        print("print method call expr")
        return codeGenMethodCall(expr)
    
    elif isinstance(expr, NewObjectExpr):
        print("print new obect expr")
        return codeGenNewObject(expr)
    
    elif isinstance(expr, ThisExpr):
        print("print This expr")
    
    elif isinstance(expr,SuperExpr):
        print("print super expr")
    
    elif isinstance(expr, ClassReferenceExpr):
        print("print class ref expr")

    return

def codeGenConstant(expr):
    # Get Register
    reg = absmc.tempRegs.getRegister(expr.info)
    if not reg: 
        reg = absmc.tempRegs.newRegister()
        absmc.tempRegs.setRegister(reg, expr) # Store constant

        if isinstance(expr, FloatConstantExpr): absmc.move_immed_f(reg, expr.info)
        if isinstance(expr, IntegerConstantExpr): absmc.move_immed_i(reg, expr.info)
        if isinstance(expr, TrueConstantExpr): absmc.move_immed_i(reg, 1)
        if isinstance(expr, FalseConstantExpr): absmc.move_immed_i(reg, 0)

    return reg

def codeGenVariable(expr):
    # Get Register
    varRecord = varTable[expr.id]
    reg = absmc.tempRegs.getRegister(varRecord)
    if not reg: 
        reg = absmc.tempRegs.newRegister()
        absmc.tempRegs.setRegister(reg, varRecord) # Store variable

    return reg

def codeGenAssign(expr):
    lhs = expr.lhs
    rhs = expr.rhs
    r1 = codeGenExpr(lhs)
    r2 = codeGenExpr(rhs)

    # print(r1, r2)
    # print(absmc.tempRegs.registers[r1])
    # print(absmc.tempRegs.registers[r2])

    absmc.move(r1, r2)  # DO I KEEP THIS OR NOT. LET ASSIGN HANDLE MOVE VS USE RESREG

    return r1

def codeGenUnary(expr):
    operator = expr.operator
    operand = expr.operand
    type = expr.type.name

    r1 = absmc.tempRegs.newRegister()
    r2 = codeGenExpr(operand)

    if type == "int":
        if operator == "MINUS":
            absmc.move_immed_i(r1, 0)
            absmc.isub(r1, r1, r2)  
        if operator == "PLUS":
            absmc.move_immed_i(r1, 0)
            absmc.iadd(r1, r1, r2)  
    
    if type == "float":
        if operator == "MINUS":
            absmc.move_immed_f(r1, 0.0)
            absmc.fsub(r1, r1, r2)  
        if operator == "PLUS":
            absmc.move_immed_f(r1, 0.0)
            absmc.fadd(r1, r1, r2)  

    if type == "boolean":
        if operator == "NOT":
            absmc.move_immed_i(r1, -1)
            absmc.imul(r1, r1, r2)

    return r1

def codeGenBinary(expr):
    #is operand a string?
    operand1 = expr.operand1
    operand2 = expr.operand2
    type = expr.type.name

    if expr.operator in ARITHOPS:
        return codeGenArithOps(operand1, operand2, expr.operator, type)
    
    if expr.operator in BOOLOPS:
        return codeGenBoolOps(operand1, operand2, expr.operator)

    if expr.operator in ARITHCOMPS:
        return codeGenArithComps(operand1, operand2, expr.operator, type)
    
    if expr.operator in EQUALITYCOMPS:
        return codeGenEqualityComps(operand1, operand2, expr.operator, type)

def codeGenArithOps(operand1, operand2, operator, type):
    print("code gen arith ops", operand1, operand2)
    # Get Registers
    r1 = absmc.tempRegs.newRegister()
    r2 = codeGenExpr(operand1)
    r3 = codeGenExpr(operand2)

    # Call Instructions
    if operand1.type.name == "int":
        if operator == "add": absmc.iadd(r1, r2, r3)
        if operator == "sub": absmc.isub(r1, r2, r3)
        if operator == "div": absmc.idiv(r1, r2, r3)
        if operator == "mul": absmc.imul(r1, r2, r3)

    else:
        if operator == "add": absmc.fadd(r1, r2, r3)
        if operator == "sub": absmc.fsub(r1, r2, r3)
        if operator == "div": absmc.fdiv(r1, r2, r3)
        if operator == "mul": absmc.fmul(r1, r2, r3)
    
    return r1

def codeGenBoolOps(operand1, operand2, operator):
    # Get Regisers
    r1 = absmc.tempRegs.newRegister()
    r2 = codeGenExpr(operand1)
    r3 = codeGenExpr(operand2)

    # Call Instructions
    if operator == "and": absmc.imul(r1, r2, r3)
    if operator == "or":
        # Sum up both operands. Check if sum > 0
        r4 = absmc.tempRegs.newRegister()
        absmc.move_immed_i(r4, 0)
        absmc.iadd(r1, r2, r3)
        absmc.igt(r1, r1, r4)

    return r1

def codeGenArithComps(operand1, operand2, operator, type):
    print("code gen arith comps", operand1, operand2)
    # Get Registers
    r1 = absmc.tempRegs.newRegister()
    
    r2 = codeGenExpr(operand1)
    r3 = codeGenExpr(operand2)

    # Call Instructions
    if operand1.type.name == "int":
        if operator == "gt": absmc.igt(r1, r2, r3)
        if operator == "geq": absmc.igeq(r1, r2, r3)
        if operator == "lt": absmc.ilt(r1, r2, r3)
        if operator == "leq": absmc.ileq(r1, r2, r3)

    if type == "float":
        if operator == "gt": absmc.fgt(r1, r2, r3)
        if operator == "geq": absmc.fgeq(r1, r2, r3)
        if operator == "lt": absmc.flt(r1, r2, r3)
        if operator == "leq": absmc.fleq(r1, r2, r3)
    
  
    return r1

def codeGenEqualityComps(operand1, operand2, operator, type):
    # Get Registers
    r1 = absmc.tempRegs.newRegister()
    r2 = codeGenExpr(operand1)
    r3 = codeGenExpr(operand2)
    L1 = absmc.incLabelCounter() # Represents True Branch
    L2 = absmc.incLabelCounter() # Represents Conditional End

    # Find type of operands
    if (operand1.type == "float" or operand2.type == "float"):
        subType = "float"
    else:
        subType = "int"

    # Call Instructions
    if subType == "int":
        absmc.isub(r1, r2, r3)
        if operator == "eq":
            absmc.bz(r1, L1)    
            absmc.move_immed_i(r1, 0)   # False Branch
            absmc.jmp(L2)       # Jump to End
            absmc.addLabel(L1)  # True Branch
            absmc.move_immed_i(r1, 1)
        
        if operator == "neq":
            absmc.bnz(r1, L1)
            absmc.move_immed_i(r1, 0)   # False Branch
            absmc.jmp(L2)       # Jump to End
            absmc.addLabel(L1)  # True Branch
            absmc.move_immed_i(r1, 1)
    if subType == "float":
        absmc.fsub(r1, r2, r3)
        if operator == "eq":
            absmc.bz(r1, L1)    
            absmc.move_immed_i(r1, 0)   # False Branch
            absmc.jmp(L2)       # Jump to End
            absmc.addLabel(L1)  # True Branch
            absmc.move_immed_i(r1, 1)
        
        if operator == "neq":
            absmc.bnz(r1, L1)    
            absmc.move_immed_i(r1, 0)   # False Branch
            absmc.jmp(L2)       # Jump to End
            absmc.addLabel(L1)  # True Branch
            absmc.move_immed_i(r1, 1)

    absmc.addLabel(L2)   # End of Conditional

    return r1

def codeGenAuto(expr):
    operand = expr.operand
    operator = expr.operation
    order = expr.order

    reg = codeGenExpr(operand)
    temp = absmc.tempRegs.getRegister(1)
    if not temp:
        temp = absmc.tempRegs.newRegister()
        absmc.move_immed_i(temp, 1)

    if order == "pre":
        if operator == "inc": absmc.iadd(reg, reg, temp)
        if operator == "dec": absmc.isub(reg, reg, temp)
    
    if order == "post":
        # WE WANT TO INCREMENT LATER AT THE END OF SOME BLOCK
        if operator == "inc": absmc.iadd(reg, reg, temp)
        if operator == "dec": absmc.isub(reg, reg, temp)
    
    return reg

def codeGenFieldAccess(expr):
    pass

def codeGenMethodCall(expr):
    pass

def codeGenNewObject(expr):
    base = expr.base
    classRecord = classTable[base]
    constrID = expr.constrID
    args = expr.arguments

    instanceCount = 0
    staticCount = 0

    # Count the instance fields
    for field in classRecord.fields:
        if field.applicability == "instance":
            instanceCount += 1
        else:
            staticCount += 1

    # Allocate space on heap
    r1 = absmc.tempRegs.newRegister()
    absmc.move_immed_i(r1, instanceCount)
    absmc.halloc(r1, r1)

    # Put arguments into arg registers
    tempRegs = []
    for arg in args:
        r = absmc.tempRegs.newRegister()
        if arg.type == "int":
            absmc.move_immed_i(arg.info)
        if arg.type == "float":
            absmc.move_immed_f(arg.info)
        
        tempRegs.append(r)
    
    argRegs = []
    for i, arg in enumerate(args):
        r = absmc.argRegs.newRegister()
        absmc.move(r, tempRegs[i])
        argRegs.append(r)

    absmc.callConstructor(constrID)

    # Restore Registers used
    for arg in argRegs:
        absmc.restore(arg)
    
    for temp in tempRegs:
        absmc.restore(temp)

    return