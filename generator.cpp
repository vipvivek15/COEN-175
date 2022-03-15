/*
 * File:	generator.cpp
 *
 * Description:	This file contains the public and member function
 *		definitions for the code generator for Simple C.
 *
 *		Extra functionality:
 *		- putting all the global declarations at the end
 */

# include <cassert>
# include <iostream>
# include <map>
# include <iterator>
# include "generator.h"
# include "Label.h"
# include "machine.h"
# include "string.h"
# include "Tree.h"

using namespace std;

static int offset;
static string funcname;
static ostream &operator <<(ostream &ostr, Expression *expr);

static Register *eax = new Register("%eax", "%al");
static Register *ecx = new Register("%ecx", "%cl");
static Register *edx = new Register("%edx", "%dl");

static vector<Register *> registers = {eax, ecx, edx};

static map<string,Label> labels;
/* These will be replaced with functions in the next phase.  They are here
   as placeholders so that Call::generate() is finished. */

void assign(Expression* expr, Register* reg);
void load(Expression* expr, Register* reg);
Register* getreg();
static void findBaseAndOffset(Expression *expr, Expression*& base, int &base_offset);
/*
 * Function:	align (private)
 *
 * Description:	Return the number of bytes necessary to align the given
 *		offset on the stack.
 */

static int align(int offset)
{
    if (offset % STACK_ALIGNMENT == 0)
	return 0;

    return STACK_ALIGNMENT - (abs(offset) % STACK_ALIGNMENT);
}


/*
 * Function:	operator << (private)
 *
 * Description:	Convenience function for writing the operand of an
 *		expression using the output stream operator.
 */

static ostream &operator <<(ostream &ostr, Expression *expr)
{
    if (expr->_register != nullptr)
	return ostr << expr->_register;

    expr->operand(ostr);
    return ostr;
}

static void findBaseAndOffset(Expression *expr, Expression*& base, int &base_offset)
{
	int field;
	base = expr;
	base_offset = 0;
	while(base->isField(base,field)) base_offset+=field;
}	
/*
 * Function:	Expression::operand
 *
 * Description:	Write an expression as an operand to the specified stream.
 */

void Expression::operand(ostream &ostr) const
{
    assert(_offset != 0);
    ostr << _offset << "(%ebp)";
}


/*
 * Function:	Identifier::operand
 *
 * Description:	Write an identifier as an operand to the specified stream.
 */

void Identifier::operand(ostream &ostr) const
{
    if (_symbol->_offset == 0)
	ostr << global_prefix << _symbol->name();
    else
	ostr << _symbol->_offset << "(%ebp)";
}


/*
 * Function:	Number::operand
 *
 * Description:	Write a number as an operand to the specified stream.
 */

void Number::operand(ostream &ostr) const
{
    ostr << "$" << _value;
}

void String::operand(ostream& ostr) const
{
	Label label;
	if(labels.find(_value) == labels.end()) labels.insert({_value,label});
	else label = labels.find(_value)->second;
	ostr << ".L" << label.number();
}	
/*
 * Function:	Call::generate
 *
 * Description:	Generate code for a function call expression.
 *
 * 		On a 32-bit Linux platform, the stack needs to be aligned
 * 		on a 4-byte boundary.  (Online documentation seems to
 * 		suggest that 16 bytes are required, but 4 bytes seems to
 * 		work and is much easier.)  Since all arguments are 4-bytes
 *		wide, the stack will always be aligned.
 *
 *		On a 32-bit OS X platform, the stack needs to aligned on a
 *		16-byte boundary.  So, if the stack will not be aligned
 *		after pushing the arguments, we first adjust the stack
 *		pointer.  However, this trick only works if none of the
 *		arguments are themselves function calls.
 *
 *		To handle nested function calls, we need to generate code
 *		for the nested calls first, which requires us to save their
 *		results and then push them on the stack later.  For
 *		efficiency, we only first generate code for the nested
 *		calls, but generate code for ordinary arguments in place.
 */

void Call::generate()
{
    unsigned numBytes;


    /* Generate code for any nested function calls first. */

    numBytes = 0;

    for (int i = _args.size() - 1; i >= 0; i --) {
	numBytes += _args[i]->type().size();

	if (STACK_ALIGNMENT != SIZEOF_REG && _args[i]->_hasCall)
	    _args[i]->generate();
    }


    /* Align the stack if necessary. */

    if (align(numBytes) != 0) {
	cout << "\tsubl\t$" << align(numBytes) << ", %esp" << endl;
	numBytes += align(numBytes);
    }


    /* Generate code for any remaining arguments and push them on the stack. */

    for (int i = _args.size() - 1; i >= 0; i --) {
	if (STACK_ALIGNMENT == SIZEOF_REG || !_args[i]->_hasCall)
	    _args[i]->generate();

	cout << "\tpushl\t" << _args[i] << endl;
	assign(_args[i], nullptr);
    }


    /* Call the function and then reclaim the stack space. */

    load(nullptr, eax);
    load(nullptr, ecx);
    load(nullptr, edx);

    if (_expr->type().isCallback()) {
	_expr->generate();

	if (_expr->_register == nullptr)
	    load(_expr, getreg());

	cout << "\tcall\t*" << _expr << endl;
	assign(_expr, nullptr);

    } else
	cout << "\tcall\t" << _expr << endl;

    if (numBytes > 0)
	cout << "\taddl\t$" << numBytes << ", %esp" << endl;

    assign(this, eax);
}


/*
 * Function:	Block::generate
 *
 * Description:	Generate code for this block, which simply means we
 *		generate code for each statement within the block.
 */

void Block::generate()
{
    for (auto stmt : _stmts) {
	stmt->generate();

	for (auto reg : registers)
	    assert(reg->_node == nullptr);
    }
}


/*
 * Function:	Simple::generate
 *
 * Description:	Generate code for a simple (expression) statement, which
 *		means simply generating code for the expression.
 */

void Simple::generate()
{
    _expr->generate();
    assign(_expr, nullptr);
}


/*
 * Function:	Procedure::generate
 *
 * Description:	Generate code for this function, which entails allocating
 *		space for local variables, then emitting our prologue, the
 *		body of the function, and the epilogue.
 */

void Procedure::generate()
{
    int param_offset;


    /* Assign offsets to the parameters and local variables. */

    param_offset = 2 * SIZEOF_REG;
    offset = param_offset;
    allocate(offset);


    /* Generate our prologue. */

    funcname = _id->name();
    cout << global_prefix << funcname << ":" << endl;
    cout << "\tpushl\t%ebp" << endl;
    cout << "\tmovl\t%esp, %ebp" << endl;
    cout << "\tsubl\t$" << funcname << ".size, %esp" << endl;


    /* Generate the body of this function. */

    _body->generate();


    /* Generate our epilogue. */

    cout << endl << global_prefix << funcname << ".exit:" << endl;
    cout << "\tmovl\t%ebp, %esp" << endl;
    cout << "\tpopl\t%ebp" << endl;
    cout << "\tret" << endl << endl;

    offset -= align(offset - param_offset);
    cout << "\t.set\t" << funcname << ".size, " << -offset << endl;
    cout << "\t.globl\t" << global_prefix << funcname << endl << endl;
}


/*
 * Function:	generateGlobals
 *
 * Description:	Generate code for any global variable declarations.
 */

void generateGlobals(Scope *scope)
{
    const Symbols &symbols = scope->symbols();

    for (auto symbol : symbols)
    {
	if (!symbol->type().isFunction()) {
	    cout << "\t.comm\t" << global_prefix << symbol->name() << ", ";
	    cout << symbol->type().size() << endl;
	}
    }
    cout << "\t.data" << endl;
    for(auto it = labels.begin();it!=labels.end();it++)
    	cout << it->second << ":\t.asciz\t\"" << escapeString(it->first) << "\"" << endl;
}


/*
 * Function:	Assignment::generate
 *
 * Description:	Generate code for an assignment statement.
 *
 *		NOT FINISHED: Only works if the right-hand side is an
 *		integer literal and the left-hand side is an integer
 *		scalar.
 */


void assign(Expression* expr, Register* reg)
{
	if(expr!=nullptr)
	{
		if(expr->_register!=nullptr) expr->_register->_node = nullptr;
		expr->_register = reg;
	}
	if(reg!=nullptr)
	{
		if(reg->_node!=nullptr) reg->_node->_register = nullptr;
		reg->_node = expr;
	}
}

void load(Expression* expr, Register* reg)
{
	if(reg->_node!=expr)
	{
		if(reg->_node!= nullptr)
		{
			unsigned n = reg->_node->type().size();
			offset -= n;
			reg->_node->_offset = offset;
			cout << (n == 1 ? "\tmovb\t" : "\tmovl\t");
			cout << reg << ", " << offset << "(%ebp)" << endl;
		}
		if(expr!= nullptr)
		{
			unsigned n = expr->type().size();
			cout << ((n == 1) ? "\tmovb\t" : "\tmovl\t");
			cout << expr << ", " << reg->name(n) << endl;
		}
		assign(expr,reg);
	}	
}

Register* getreg()
{
	for(auto& reg: registers)
		if(reg->_node==nullptr) return reg;
	load(nullptr,registers[0]);
	return registers[0];
}
static void structures(Expression* result, Expression* expr, Expression* left, Expression* right, const Type& type, const string& opcode, const bool& isAssignment)
{
    Expression *base;
    int fieldoffset;
    if(opcode == "Field" && !isAssignment) findBaseAndOffset(result,base,fieldoffset);	
    else if(opcode == "Assignment" && isAssignment) findBaseAndOffset(left,base,fieldoffset);
    else if(opcode == "Address" && !isAssignment) findBaseAndOffset(expr,base,fieldoffset);	
    Expression *pointer;
    unsigned num;
    if(right!=nullptr && opcode == "Assignment" && isAssignment) right->generate();
    if(base->isDereference(pointer))
    {
		if(opcode == "Field" && !isAssignment)
		{
			pointer->generate();
			if (pointer->_register == nullptr) load(pointer, getreg());
			(type.size()==1) ? cout << "\tmovb\t" : cout << "\tmovl\t";
			if(fieldoffset!=0) cout << fieldoffset;
			cout  << "(" << pointer << ")" << ", "; 
			assign(result,pointer->_register);
			cout << result << endl;
			assign(nullptr,pointer->_register);
			assign(pointer,nullptr);
		}
		else if(opcode == "Assignment" && isAssignment)
		{
			pointer->generate();
			if (pointer->_register == nullptr) load(pointer, getreg());
			if(right->_register == nullptr && !right->isNumber(num)) load(right,getreg());
			if (left->type().size() != 1) cout << "\tmovl\t" << right << ", ";
			else if (right->_register!=nullptr) cout << "\tmovb\t" << right->_register->byte() << ", ";
                        else cout << "\tmovb\t" << right << ", ";
			if(fieldoffset!=0) cout << fieldoffset;
			cout << "(" << pointer << ")" << endl;
			assign(nullptr,pointer->_register);
			assign(pointer, nullptr);
			assign(nullptr,right->_register);
			assign(nullptr,left->_register);
			assign(right, nullptr);
			assign(left,nullptr);
		}
		else if(opcode == "Address" && !isAssignment)
		{
			pointer->generate();
			if (pointer->_register == nullptr) load(pointer, getreg());
			assign(result,pointer->_register);
			cout << "\taddl\t" << "$" << fieldoffset << ", " << result << endl;
			assign(nullptr,pointer->_register);
			assign(pointer,nullptr);
		}
      }
      else
      {
		if(opcode == "Field" && !isAssignment)
		{
			assign(result,getreg());
			(type.size()==1) ? cout << "\tmovb\t" : cout << "\tmovl\t";
			if(fieldoffset!=0) cout << fieldoffset << "+";
			cout << base << ", " << result << endl;
		}
		else if(opcode == "Assignment" && isAssignment)
		{
			if(right->_register == nullptr && !right->isNumber(num)) load(right,getreg());
			if(base->type().size()!=1) cout << "\tmovl\t" << right << ", ";
			else cout << "\tmovb\t" << right << ", ";
			if(fieldoffset!=0) cout << fieldoffset << "+";
			cout << base << endl;
			assign(nullptr,base->_register);
			assign(base,nullptr);
			assign(nullptr,right->_register);
			assign(nullptr,left->_register);
			assign(right, nullptr);
			assign(left,nullptr);
		}
		else if(opcode == "Address" && !isAssignment)
		{
			assign(result, getreg());
			(fieldoffset == 0) ?  cout << "\tleal\t" << base << ", " << result << endl : cout << "\tleal\t" << fieldoffset << "+" << base << ", " << result << endl;	
		}
      }
}
		
void Field::generate()
{
    structures(this,nullptr,nullptr,nullptr,_type,"Field",false);
}

void Assignment::generate()
{
    structures(nullptr,nullptr,_left,_right,_right->type(),"Assignment",true);
}

void Address::generate()
{
    structures(this,_expr,nullptr,nullptr,_type,"Address",false); 
}


static void unary(Expression* result, Expression* expr, const Type& type, const string& unary_opcode, const bool& isCast) 
{
	expr->generate();
	if(expr->_register == nullptr) load(expr,getreg());
	/*cast*/
	if(unary_opcode == "Cast" && isCast)
	{
		if(expr->type().size()==1 && type.size()==4) cout << "\tmovsbl\t" << expr << ", " << expr->_register->name() << endl;		
		assign(result,expr->_register);
        	assign(nullptr,expr->_register);
        	assign(expr,nullptr);
	}
	/*dereference*/
	else if(unary_opcode == "Dereference" && !isCast)
	{
		if (expr->type().size() == 1) cout << "\tmovb\t" << "(" << expr << "), " << expr << endl;
    		else cout << "\tmovl\t" << "(" << expr << "), " << expr << endl;
		assign(result,expr->_register);
		assign(nullptr,expr->_register);
		assign(expr,nullptr);
	}
	/*negation*/
	else if(unary_opcode == "Negate" && !isCast)  
	{
		cout << "\tnegl\t" << expr << endl;
		assign(result,expr->_register);
        	assign(nullptr,expr->_register);
        	assign(expr,nullptr);

	}
	/*not*/
	else if(unary_opcode == "Not" && !isCast)
	{
		cout << "\tcmpl\t" << "$0, " << expr->_register << endl;
    		cout << "\tsete\t" << expr->_register->byte() << endl;
    		cout << "\tmovzbl\t" << expr->_register->byte() << ", " << expr << endl;
		assign(result,expr->_register);
        	assign(nullptr,expr->_register);
        	assign(expr,nullptr);
	
	}
}	

void Dereference::generate() 
{
    unary(this,_expr,_type,"Dereference",false);
}

void Cast::generate() 
{
    unary(this,_expr,_type,"Cast",true);
}

void Not::generate() 
{
    unary(this,_expr,_type,"Not",false);
}

void Negate::generate() 
{
    unary(this,_expr,_type,"Negate",false);
}
static void divide(Expression* result, Expression* left, Expression* right, Register* reg)
{
    left->generate();
    right->generate();

    load(left, eax);
    unsigned num;
    if(right->isNumber(num)) load(right,ecx);
    load(nullptr, edx);

    cout << "\tcltd\t" << endl;
    cout << "\tidivl\t" << right << endl;

    assign(nullptr, eax);
    assign(nullptr, ecx);
    assign(nullptr, edx);
    assign(result, reg);
}	
void Divide::generate() 
{
	divide(this,_left,_right,eax);
}
void Remainder::generate() 
{
	divide(this,_left,_right,edx);
}
void compute(Expression* result, Expression* left, Expression* right, const string& opcode)
{
	left->generate();
	right->generate();
	if(left->_register == nullptr) load(left,getreg());
	cout << "\t" << opcode << "\t" << right << ", " << left << endl;
	assign(nullptr,right->_register);
	assign(right,nullptr);
	assign(result,left->_register);
}
void Add::generate()
{
	compute(this,_left,_right,"addl");
}
void Subtract::generate()
{
	compute(this,_left,_right,"subl");
}
void Multiply::generate()
{
	compute(this,_left,_right,"imull");
}

void compare(Expression* result, Expression* left, Expression* right, const string& opcode)
{
	left->generate();
	right->generate();
	if(left->_register == nullptr) load(left,getreg());
	cout << "\tcmpl\t" << right << ", " << left << endl;
	cout << "\t" << opcode << "\t" << left->_register->byte() << endl;
        cout << "\tmovzbl\t" << left->_register->byte() << ", " << left << endl;
	assign(result,left->_register);
        assign(nullptr,left->_register);
        assign(left,nullptr);
        assign(nullptr,right->_register);
        assign(right,nullptr);
}

void LessThan::generate() 
{
    compare(this, _left, _right, "setl");
}

void GreaterThan::generate() 
{
    compare(this, _left, _right, "setg");
}

void LessOrEqual::generate() 
{
    compare(this, _left, _right, "setle");
}

void GreaterOrEqual::generate() 
{
    compare(this, _left, _right, "setge");
}

void Equal::generate() 
{
    compare(this, _left, _right, "sete");
}

void NotEqual::generate() 
{
    compare(this, _left, _right, "setne");
}

static void logical(Expression* result, Expression* left, Expression* right, const bool& isSkip)
{
    Label skip, short_circuit;
    left->test(short_circuit, isSkip);
    right->test(short_circuit,isSkip);
    assign(result,getreg());
    if(isSkip)
    {
    	cout << "\tmovl\t$0, " << result << endl;
    	cout << "\tjmp\t" << skip << endl;
    	cout << short_circuit << ":" << endl; 	
    	cout << "\tmovl\t$1, " << result << endl;
    	cout << skip << ":" << endl;
    }
    else
    {
	cout << "\tmovl\t$1, " << result << endl;
	cout << "\tjmp\t" << skip << endl;	
	cout << short_circuit << ":" << endl;
	cout << "\tmovl\t$0, " << result << endl;
	cout << skip << ":" << endl;
    }
}
	
void LogicalAnd::generate() 
{
	logical(this,_left,_right,false);	
}

void LogicalOr::generate() 
{
	logical(this,_left,_right,true);
}

void Expression::test(const Label &label, const bool &ifTrue) 
{
    this->generate();
    if (_register == nullptr) load(this, getreg());
    cout << "\tcmpl\t$0, " << this << endl;
    cout << (ifTrue ? "\tjne\t" : "\tje\t") << label << endl;
    assign(this, nullptr);
}

void Return::generate() 
{
    _expr->generate();
    load(_expr, eax);
    cout << "\tjmp\t" << funcname << ".exit" << endl;
    assign(nullptr,_expr->_register);
    assign(_expr,nullptr);
    assign(nullptr,eax);
}
static void loops(Expression* expr, Statement *init, Statement* stmt, Statement* incr, const bool& isFor)
{
	Label loop, exit;
	if(isFor && init!=nullptr) init->generate();
	cout << loop << ":" << endl;
	expr->test(exit,false);
	stmt->generate();
	if(isFor && incr!=nullptr) incr->generate();
	cout << "\tjmp\t" << loop << endl;
	cout << exit << ":" << endl;
}				
void While::generate() 
{
    loops(_expr,nullptr,_stmt,nullptr,false);
}

void For::generate() 
{
    loops(_expr,_init,_stmt,_incr,true);
}

void If::generate() 
{
	Label _elseLabel,_endLabel;
	if(_elseStmt != nullptr)
	{
		_expr->test(_elseLabel,false);
		_thenStmt->generate();	
		cout << "\tjmp\t" << _endLabel << endl;
		cout << _elseLabel << ":" << endl;
		_elseStmt->generate();
		cout << _endLabel << ":" << endl;
	}
	else
	{
		_expr->test(_endLabel,false);
		_thenStmt->generate();
		cout << _endLabel << ":" << endl;
	}
}
