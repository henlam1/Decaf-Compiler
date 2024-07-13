from decaf_ast import *

class RegisterSet:
    # INPUT : prefix of register name
    # OUTPUT; None
    def __init__(self, prefix: str) -> None:
        self.registers = {}
        self.prefix = prefix

    # INPUT: None
    # OUTPUT: Key of available register
    def newRegister(self) -> str:
        register = f"{self.prefix}{len(self.registers)}"
        self.registers[register] = None

        return register
    
    # INPUT: Register key, Value to be set
    # OUTPUT: None
    def setRegister(self, register: str, value: any) -> None:
        self.registers[register] = value
    
    # INPUT: Value
    # OUTPUT: Register
    def getRegister(self, value: any) -> str:
        # Search Registers
        for k, v in self.registers.items():
            # print("Key: ", k, "value: ", v)
            # print("looking for ", value)
            if v == value:
                return k
        
        # # Create new register
        # reg = self.newRegister()
        # self.setRegister(reg, value)
        # return reg
        return None
    
    # INPUT: Key of register
    # OUTPUT: Value stored in the register
    def getValue(self, key: str) -> any:
        if key not in self.registers:
            raise Exception("Register key " + key + " not defined")
        
        return self.registers[key]

class Stack:
    # INPUT: None
    # OUTPUT: None
    def __init__(self) -> None:
        self.stack = []
    
    # INPUT: Either return address/register value
    # OUTPUT: None
    def push(self, val: str) -> None:
        self.stack.append(val)
    
    # INPUT: None
    # OUTPUT: Either return address/register value
    def pop(self) -> str:
        if not self.stack:
            raise Exception("Can't pop from an empty stack")
        
        return self.stack.pop()


class ABSMC:
    def __init__(self):
        self.codeStr = "" #returns a string to write to file
        self.static_data = 0 #total number of static fields to allocate
        
        # Registers
        self.argRegs = RegisterSet("a") # a for argument
        self.tempRegs = RegisterSet("t")    # t for temp
        self.SAP = 0
        self.labelCounter = -1

        # Heap
        self.heap = {}  # unsure for now

        # Control Stack
        self.ctrlStack = Stack()

        # Data Stack
        self.dataStack = Stack()

        self.labelCounter = 0

    
    # def resetRegisterCount(self):
    #     self.argRegisterCount = 0
    #     self.tempRegisterCount = 0
        
    def initialize(self):
        self.allocStaticFields()
        #self.initializeInstanceFields
    
    def allocStaticFields(self):
        return
    
    def addLabel(self, label):
        self.codeStr += label + ":"
        self.codeStr += '\n'
    
    def addNewLine(self):
        self.codeStr+= '\n'
    
    def addIndent(self, num=1):
        indent = '\t' * num
        self.codeStr += indent
    
    def incLabelCounter(self):
        self.labelCounter +=1
        return "L_"+str(self.labelCounter)
    
    def move_immed_i(self,reg, constant):
        #move_immed_i r, i : Set register r to integer constant i
        self.codeStr +="move_immed_i %s, %d" % (reg,constant)
        self.codeStr += "\n"
    
    def move_immed_f(self,reg, constant):
        #move_immed_f r, f : Set register r to floating point constant f
        self.codeStr +="move_immed_f %s, %f" % (reg,constant)
        self.codeStr += "\n"
    
    def move(self, r1,r2):
        #move r1, r2 : Set register r1 to the value of register r2
        self.codeStr += "move %s, %s" % (r1,r2)
        self.codeStr += "\n"
    
    def iadd(self,r1,r2,r3):
        #iadd r1, r2, r3 : Set register r1 to the sum r2 + r3
        self.codeStr += "iadd %s, %s, %s" % (r1,r2,r3)
        self.codeStr += "\n"
    
    def isub(self, r1,r2,r3):
        #isub r1, r2, r3 : Set register r1 to the difference r2 – r3
        self.codeStr += "isub %s, %s, %s" % (r1,r2,r3)
        self.codeStr += "\n"
    
    def imul(self, r1,r2,r3):
        #imul r1, r2, r3 : Set register r1 to the product r2 * r3
        self.codeStr += "imul %s, %s, %s" % (r1,r2,r3)
        self.codeStr += "\n"
    
    def idiv(self,r1,r2,r3):
        #idiv r1, r2, r3 : Set register r1 to the quotient r2 / r3
        self.codeStr += "idiv %s, %s, %s" % (r1, r2,r3)
        self.codeStr += "\n"
    
    def imod(self,r1,r2,r3):
        #igt r1, r2, r3 : Set register r1 to the quotient r2 % r3
        self.codeStr += "imod %s, %s, %s" % (r1, r2,r3)
        self.codeStr += "\n"
    
    def igt(self,r1,r2,r3):
        #igt r1, r2, r3 : Set register r1 to 1 if r2 > r3; 0 otherwise
        self.codeStr += "igt %s, %s, %s" % (r1, r2,r3)
        self.codeStr += "\n"
    
    def igeq(self,r1,r2,r3):
        # igeq r1, r2, r3 : Set register r1 to 1 if r2 >= r3; 0 otherwise
        self.codeStr += "igeq %s, %s, %s" % (r1, r2,r3)
        self.codeStr += "\n"
    
    def ilt(self,r1,r2,r3):
        # ilt r1, r2, r3 : Set register r1 to 1 if r2 < r3; 0 otherwise
        self.codeStr += "ilt %s, %s, %s" % (r1, r2,r3)
        self.codeStr += "\n"

    def ileq(self,r1,r2,r3):
        # ileq r1, r2, r3 : Set register r1 to 1 if r2 < r3; 0 otherwise
        self.codeStr += "ileq %s, %s, %s" % (r1, r2,r3)
        self.codeStr += "\n"
    
    def fadd(self, r1, r2, r3):
        #fadd r1, r2, r3 : Set register r1 to the sum r2 + r3
        self.codeStr += "fadd %s, %s, %s" % (r1,r2,r3)
        self.codeStr += "\n"
    
    def fsub(self, r1, r2, r3):
        # fsub r1, r2, r3 : Set register r1 to the difference r2 – r3
        self.codeStr += "fsub %s, %s, %s" % (r1,r2,r3)
        self.codeStr += "\n"

    def fmul(self, r1, r2, r3):
        #fmul r1, r2, r3 : Set register r1 to the product r2 * r3
        self.codeStr += "fmul %s, %s, %s" % (r1,r2,r3)
        self.codeStr += "\n"

    def fdiv(self, r1, r2, r3):
        #fdiv r1, r2, r3 : Set register r1 to the quotient r2 / r3
        self.codeStr += "fdiv %s, %s, %s" % (r1,r2,r3)
        self.codeStr += "\n"
    
    def fgt(self, r1, r2, r3):
        #fgt r1, r2, r3 : Set register r1 to 1 if r2 > r3; 0 otherwise
        self.codeStr += "fgt %s, %s, %s" % (r1,r2,r3)
        self.codeStr += "\n"
    
    def fgeq(self, r1, r2, r3):
        #fgeq r1, r2, r3 : Set register r1 to 1 if r2 >= r3; 0 otherwise
        self.codeStr += "fgeq %s, %s, %s" % (r1,r2,r3)
        self.codeStr += "\n"
    
    def flt(self, r1, r2, r3):
        #fgeq r1, r2, r3 : Set register r1 to 1 if r2 >= r3; 0 otherwise
        self.codeStr += "flt %s, %s, %s" % (r1,r2,r3)
        self.codeStr += "\n"
    
    def fleq(self, r1, r2, r3):
        #fgeq r1, r2, r3 : Set register r1 to 1 if r2 >= r3; 0 otherwise
        self.codeStr += "fleq %s, %s, %s" % (r1,r2,r3)
        self.codeStr += "\n"
    
    def ftoi(self,r1,r2):
        #ftoi r1, r2 : Set register r1 to an integer value by truncating (taking the floor of) the floating point value in r2
        self.codeStr += "ftoi %s, %s" % (r1,r2)
    
    def itof(self,r1,r2):
        #itof r1, r2 : Set register r1 to a floating point value corresponding to the integer value in r2.
        self.codeStr += "itof %s, %s" % (r1,r2)


    def bz(self, reg, label):
        #Branch to the label L if the value in r1 is zero
        self.codeStr+= "bz %s, %s" % (reg, label)
        self.codeStr+= '\n'
    
    def bnz(self, reg, label):
        #bnz r1, L : Branch to the label L if the value in r1 is not zero
        self.codeStr +="bnz %s, %s" % (reg, label)
        self.codeStr += "\n"

    def jmp(self, label):
        #jmp L : Branch to the label L unconditionally
        self.codeStr += "jmp %s" % (label)
        self.codeStr += "\n"
    
    def hload(self, r1,r2,r3):
        #hload r1, r2, r3 : A heap cell is addressed by the pair r2, r3 : base address in r2 and offset in r3. Register r1 is set to the value in this cell.
        self.codeStr += "hload %s, %s, %s" % (r1,r2,r3)
        self.codeStr += "\n"
    
    def hstore(self, r1,r2,r3):
        #hstore r1, r2, r3 : A heap cell is addressed by the pair r1, r2 : base address in r1 and offset in r2. This cell is set to the value in register r3.
        self.codeStr += "hstore %s, %s, %s" % (r1,r2,r3)
        self.codeStr += "\n"
    
    def halloc(self, r1,r2):
        #halloc r1, r2 : This instruction assumes that r2 contains an integer value; that value specifies the number of cells to be created on the heap. Register r1 is set to the base address (i.e., the smallest address) of this group of cells.
        self.codeStr += "halloc %s, %s" % (r1, r2)
        self.codeStr += "\n"
    
    def callConstructor(self, id):
        self.codeStr += "call C_%d" % (id)
        self.codeStr += "\n"

    def callMethod(self, name, id):
        self.codeStr += "call M_%s_%d" &(name,id)
        self.codeStr += "\n"
    
    def ret(self):
        #ret : The top of the Control Stack has the return address; the Control Stack is popped and the control transfers to the return address.
        self.codeStr += "ret"
        self.codeStr += "\n"

    def save(self, reg):
        #save r : Value of register r is pushed on to the Data Stack.
        self.codeStr += "save %s" % (reg)
        self.codeStr += "\n"
    
    def restore(self, reg):
        #restore r : The Data Stack is popped and register r is set to the popped value.
        self.codeStr += "restore %s" % (reg)
        self.codeStr += "\n"