/* File: parser.y
 * --------------
 * Bison input file to generate the parser for the compiler.
 *
 * pp2: your job is to write a parser that will construct the parse tree
 *      and if no parse errors were found, print it.  The parser should
 *      accept the language as described in specification, and as augmented
 *      in the pp2 handout.
 */

%{

/* Just like lex, the text within this first region delimited by %{ and %}
 * is assumed to be C/C++ code and will be copied verbatim to the y.tab.c
 * file ahead of the definitions of the yyparse() function. Add other header
 * file inclusions or C++ variable declarations/prototypes that are needed
 * by your code here.
 */
#include "scanner.h" // for yylex
#include "parser.h"
#include "errors.h"

void yyerror(const char *msg); // standard error-handling routine

%}

/* The section before the first %% is the Definitions section of the yacc
 * input file. Here is where you declare tokens and types, add precedence
 * and associativity options, and so on.
 */

/* yylval
 * ------
 * Here we define the type of the yylval global variable that is used by
 * the scanner to store attibute information about the token just scanned
 * and thus communicate that information to the parser.
 *
 * pp2: You will need to add new fields to this union as you add different
 *      attributes to your non-terminal symbols.
 */
%code requires {
	struct FullyType {
		Type* type_specifier;
		TypeQualifier* type_qualifier;
	};
}

%union {
    int integerConstant;
    bool boolConstant;
    float floatConstant;
    char identifier[MaxIdentLen+1]; // +1 for terminating null
    Decl *decl;
    List<Decl*> *declList;
    VarDecl* vardecl;
    FnDecl *fnDecl;
    Type* type;
    TypeQualifier* tpQualifier;
    Expr* expr;
    Call* callexpr;
    Identifier* ident;
    Operator* op;
    Stmt* stmt;
    IfStmt* Ifstmt;
    ForStmt* forstmt;
    StmtBlock* stmtblock;
    List<Case*>* caseList;
    Case* caseStmt;
    Default* defaultStmt;
    struct FullyType fullyType;   
}


/* Tokens
 * ------
 * Here we tell yacc about all the token types that we are using.
 * Bison will assign unique numbers to these and export the #define
 * in the generated y.tab.h header file.
 */
%token   T_Void T_Bool T_Int T_Float
%token   T_LessEqual T_GreaterEqual T_EQ T_NE T_LeftAngle T_RightAngle
%token   T_And T_Or
%token   T_Equal T_MulAssign T_DivAssign T_AddAssign T_SubAssign
%token   T_While T_For T_If T_Else T_Return T_Break
%token   T_Const T_Uniform T_Layout T_Continue T_Do
%token   T_Inc T_Dec T_Switch T_Case T_Default
%token   T_In T_Out T_InOut
%token   T_Mat2 T_Mat3 T_Mat4 T_Vec2 T_Vec3 T_Vec4
%token   T_Ivec2 T_Ivec3 T_Ivec4 T_Bvec2 T_Bvec3 T_Bvec4
%token   T_Uint T_Uvec2 T_Uvec3 T_Uvec4 T_Struct
%token   T_Semicolon T_Dot T_Colon T_Question T_Comma
%token   T_Dash T_Plus T_Star T_Slash
%token   T_LeftParen T_RightParen T_LeftBracket T_RightBracket T_LeftBrace T_RightBrace

%token   <identifier> T_Identifier
%token   <integerConstant> T_IntConstant
%token   <floatConstant> T_FloatConstant
%token   <boolConstant> T_BoolConstant

/* Non-terminal types
 * ------------------
 * In order for yacc to assign/access the correct field of $$, $1, we
 * must to declare which field is appropriate for the non-terminal.
 * As an example, this first type declaration establishes that the DeclList
 * non-terminal uses the field named "declList" in the yylval union. This
 * means that when we are setting $$ for a reduction for DeclList ore reading
 * $n which corresponds to a DeclList nonterminal we are accessing the field
 * of the union named "declList" which is of type List<Decl*>.
 * pp2: You'll need to add many of these of your own.
 */
%type <declList>    Translation_Unit
%type <caseList>    Case_List
%type <decl>        External_Declaration
%type <decl>	    Decl
%type <fnDecl>	    Function_Definition
%type <stmt>	    Compound_Statement_No_New_Scope
%type <stmt>	    Compound_Statement_With_Scope
%type <stmt>	    Statement_With_Scope
%type <stmt>	    Statement_No_New_Scope
%type <stmt>	    Simple_Statement
%type <stmt>	    Selection_Statement
%type <stmt>	    Switch_Statement
%type <stmt>	    Iteration_Statement
%type <stmt> 	    Jump_Statement
%type <stmtblock>   Statement_List
%type <stmtblock>   Switch_Statement_List
%type <Ifstmt>	    Selection_Rest_Statement
%type <defaultStmt> Default_Statement
%type <caseStmt>    Case_Statement
%type <forstmt>	    For_Rest_Statement
%type <stmt>	    Statement
%type <fnDecl> 	    Function_Prototype
%type <fnDecl>	    Function_Declarator
%type <fnDecl>	    Function_Header
%type <fnDecl>	    Function_Header_With_Params
%type <vardecl>     Declaration_Statement
%type <vardecl>     Parameter_Declaration
%type <vardecl>     Parameter_Declarator
%type <vardecl>	    Init_Declarator_List
%type <vardecl>	    Single_Declaration
%type <tpQualifier> Type_Qualifier
%type <tpQualifier> Single_Type_Qualifier
%type <tpQualifier> Storage_Qualifier
%type <type>	    Parameter_Type_Specifier
%type <fullyType>   Fully_Specified_Type
%type <type>	    Type_Specifier
%type <type>	    Array_Specifier
%type <type>	    Type_Specifier_Nonarray
%type <ident>	    Variable_Identifier
%type <ident>	    Function_Identifier
%type <expr>	    Expr_Statement
%type <expr>	    For_Init_Statement
%type <expr>	    Case_Label
%type <expr>	    Condition
%type <expr>	    Conditionopt
%type <expr>	    Initializer
%type <expr>	    Constant_Expr
%type <expr>	    Assignment_Expr 
%type <expr>	    Conditional_Expr
%type <expr>	    Logical_Or_Expr
%type <expr>	    Logical_Xor_Expr
%type <expr>	    Logical_And_Expr
%type <expr>	    Inclusive_Or_Expr
%type <expr>	    Exclusive_Or_Expr
%type <expr>	    And_Expr
%type <expr>	    Equality_Expr
%type <expr>	    Relational_Expr
%type <expr>	    Shift_Expr
%type <expr>	    Additive_Expr
%type <expr>	    Multiplicative_Expr
%type <expr>	    Unary_Expr
%type <expr>	    Postfix_Expr
%type <expr>	    Primary_Expr
%type <expr>	    Expr
%type <expr>	    Integer_Expr
%type <callexpr>    Function_Call
%type <callexpr>    Function_Call_Or_Method
%type <callexpr>    Function_Call_Generic
%type <callexpr>    Function_Call_Header_With_Params
%type <callexpr>    Function_Call_Header_No_Params
%type <callexpr>    Function_Call_Header
%type <op>	    Assignment_Operator
%type <op>	    Unary_Operator

%nonassoc "then"
%nonassoc T_Else
%%
/* Rules
 * -----
 * All productions and actions should be placed between the start and stop
 * %% markers which delimit the Rules section.

 */
Program   :    Translation_Unit     {
                                      @1;
                                      /* pp2: The @1 is needed to convince
                                       * yacc to set up yylloc. You can remove
                                       * it once you have other uses of @n*/
                                      Program *program = new Program($1);
                                      // if no errors, advance to next phase
                                      if (ReportError::NumErrors() == 0)
                                          program->Print(0);
                                    }
          ;

Translation_Unit  :   Translation_Unit External_Declaration        { ($$=$1)->Append($2); }
          	  |   External_Declaration                 { ($$ = new List<Decl*>)->Append($1); }
          	  ;

External_Declaration :	Function_Definition { $$ = $1; }
		     |  Decl { $$ = $1; }
		     ;

Function_Definition  :  Function_Prototype Compound_Statement_No_New_Scope { $$=$1; if ($2) $$->SetFunctionBody($2); }
		     ;

Compound_Statement_No_New_Scope :	T_LeftBrace T_RightBrace { $$ = new StmtBlock(new List<VarDecl*>(), new List<Stmt*>()); }
				|   	T_LeftBrace Statement_List T_RightBrace { $$ = $2; }
				;

Statement_List	:	Statement { ($$ = new StmtBlock(new List<VarDecl*>(), new List<Stmt*>()))->AppendStmt($1); }
		|	Statement_List Statement { ($$=$1)->AppendStmt($2); }
		|	Declaration_Statement { ($$ = new StmtBlock(new List<VarDecl*>(), new List<Stmt*>()))->AppendVarDecl($1); }
		|	Statement_List Declaration_Statement {	($$ = $1)->AppendVarDecl($2); }
		;

Statement	:	Compound_Statement_With_Scope { $$ = $1; }
		|   	Simple_Statement { $$ = $1; }
		;

Compound_Statement_With_Scope:	T_LeftBrace T_RightBrace { $$ = new StmtBlock(new List<VarDecl*>(), new List<Stmt*>()); }
			     |	T_LeftBrace Statement_List T_RightBrace { $$ = $2; }
			     ;

Simple_Statement:	Expr_Statement { $$ = $1; }
		|	Selection_Statement { $$ = $1; }
		|	Switch_Statement { $$ = $1; }
		|	Iteration_Statement { $$ = $1; }
		|	Jump_Statement { $$ = $1; }
		;

Expr_Statement	:	T_Semicolon { $$ = new EmptyExpr(); }
		|	Expr T_Semicolon { $$ = $1; }
		;
	
Declaration_Statement:	Init_Declarator_List T_Semicolon {  $$ = $1; }
		     ;

Selection_Statement  : T_If T_LeftParen Expr T_RightParen Selection_Rest_Statement { $5->SetTestExpr($3); $$ = $5; }
		     ;

Selection_Rest_Statement : Statement_With_Scope T_Else Statement_With_Scope { $$ = new IfStmt(new EmptyExpr(), $1, $3); }
			 | Statement_With_Scope %prec "then" { $$ = new IfStmt(new EmptyExpr(), $1, NULL); }
			 ;

Switch_Statement	:	T_Switch T_LeftParen Expr T_RightParen T_LeftBrace Case_List T_RightBrace { $$ = new SwitchStmt($3, $6, NULL);}
			|	T_Switch T_LeftParen Expr T_RightParen T_LeftBrace Case_List Default_Statement T_RightBrace { $$ = new SwitchStmt($3,$6,$7);}

Case_List		:	Case_Statement { ($$ = new List<Case*>())->Append($1); }
			|	Case_List Case_Statement { ($$ = $1)->Append($2); }

Case_Statement		:	Case_Label Switch_Statement_List { $$ = new Case($1,$2->getStmtList()); }
			;

Default_Statement	:	T_Default T_Colon Switch_Statement_List { $$ = new Default($3->getStmtList()); }
			;

Case_Label		:	T_Case Expr T_Colon { $$ = $2; }
			;

Switch_Statement_List	:	Statement_List { $$ = $1; }
			;

Iteration_Statement	:	T_While T_LeftParen Condition T_RightParen Statement_No_New_Scope	{ $$ = new WhileStmt($3,$5);}
			|	T_Do Statement_With_Scope T_While T_LeftParen Expr T_RightParen T_Semicolon { $$ = new DoWhileStmt($2,$5); }
			|	T_For T_LeftParen For_Init_Statement For_Rest_Statement T_RightParen Statement_No_New_Scope { $4->SetInit($3); $4->SetBody($6); $$ = $4;}
			;

For_Init_Statement	:	Expr_Statement { $$ = $1; }
			;

For_Rest_Statement	:	Conditionopt T_Semicolon { $$ = new ForStmt(new EmptyExpr(), $1, NULL, new EmptyStmt()); }
			|	Conditionopt T_Semicolon Expr { $$ = new ForStmt(new EmptyExpr(), $1, $3, new EmptyStmt());}
			;

Conditionopt		:	Condition { $$ = $1; }
			;

Statement_With_Scope	:	Compound_Statement_No_New_Scope { $$ = $1; }
			|	Simple_Statement { $$ = $1; }
			;

Jump_Statement		:	T_Break T_Semicolon { $$ = new BreakStmt(@1); }
			|	T_Return T_Semicolon { $$ = new ReturnStmt(@1,NULL);}
			|	T_Return Expr T_Semicolon { $$ = new ReturnStmt(@1,$2);}
			;
		
Statement_No_New_Scope	:	Compound_Statement_No_New_Scope { $$ = $1; }
			|	Simple_Statement { $$ = $1; }
			;

Condition		:	Expr	{ $$ = $1; }
			|	Fully_Specified_Type T_Identifier T_EQ Initializer { $$ = new AssignExpr(new VarExpr(@2,new Identifier(@2,$2)), new Operator(@3,"=="), $4); }
			;
	 
Decl      :     Function_Prototype T_Semicolon { $$=$1; }
	  |	Init_Declarator_List T_Semicolon { $$=$1; }
          ;

Function_Prototype	:	Function_Declarator T_RightParen { $$=$1; }
			;

Function_Declarator	:	Function_Header	{ $$=$1; }
			|	Function_Header_With_Params {$$ = $1;}
			;

Function_Header_With_Params : Function_Header Parameter_Declaration { $$ = $1; $$->AppendParameterDeclaration($2);}
			    | Function_Header_With_Params T_Comma Parameter_Declaration { $$ = $1; $$->AppendParameterDeclaration($3); }
			    ;

Parameter_Declaration	:	Parameter_Declarator 	 { $$ = $1; }
			|	Parameter_Type_Specifier { $$ = new VarDecl(new Identifier(@1,""), $1); }
			;

Init_Declarator_List	:	Single_Declaration	{ $$ = $1; }
			;

Single_Declaration      :       Fully_Specified_Type T_Identifier { $$ = new VarDecl(new Identifier(@2,$2),$1.type_specifier); 
								    if ( $1.type_qualifier != NULL ) $$->SetTypeQualifier($1.type_qualifier);
								  }
			|	Fully_Specified_Type T_Identifier Array_Specifier { $$ = new VarDecl(new Identifier(@2,$2), new ArrayType(@1,$1.type_specifier));
if ($1.type_qualifier != NULL) $$->SetTypeQualifier($1.type_qualifier); }
			|	Fully_Specified_Type T_Identifier T_Equal Initializer { $$ = new VarDecl(new Identifier(@2,$2),$1.type_specifier,$4); 
    if ( $1.type_qualifier != NULL ) $$->SetTypeQualifier($1.type_qualifier);
			}
                        ;

Initializer		:	Assignment_Expr { $$ = $1; }
			;

Assignment_Expr		:	Conditional_Expr { $$ = $1; }
			|	Unary_Expr Assignment_Operator Assignment_Expr { $$= new AssignExpr($1,$2,$3); }
			;

Assignment_Operator	:	T_Equal { $$ = new Operator(@1,"="); }
			|	T_MulAssign { $$ = new Operator(@1,"*="); }
			|	T_DivAssign { $$ = new Operator(@1,"/="); }
			|	T_AddAssign { $$ = new Operator(@1,"+="); }
			|	T_SubAssign { $$ = new Operator(@1,"-="); }
			;	

Conditional_Expr	:	Logical_Or_Expr { $$ = $1; }
			|	Logical_Or_Expr T_Question Expr T_Colon Assignment_Expr { $$ = new SelectionExpr($1,$3,$5);}
			;


Logical_Or_Expr		:	Logical_Xor_Expr { $$ = $1; }
			|	Logical_Or_Expr T_Or Logical_Xor_Expr { $$ = new LogicalExpr($1, new Operator(@2,"||"), $3);}
			;

Logical_Xor_Expr	:	Logical_And_Expr { $$ = $1; }
			;

Logical_And_Expr	:	Inclusive_Or_Expr { $$ = $1; }
			|	Logical_And_Expr T_And Inclusive_Or_Expr { $$ = new LogicalExpr($1, new Operator(@2,"&&"),$3);}
			;

Inclusive_Or_Expr	:	Exclusive_Or_Expr { $$ = $1; }
			;

Exclusive_Or_Expr	:	And_Expr { $$ = $1; }
			;

And_Expr		:	Equality_Expr	{ $$ = $1; }
			;

Equality_Expr		:	Relational_Expr { $$ = $1; }
			|	Equality_Expr T_EQ Relational_Expr { $$ = new EqualityExpr($1, new Operator(@2, "=="), $3); }
			|	Equality_Expr T_NE Relational_Expr {$$ = new EqualityExpr($1, new Operator(@2, "!="), $3); }
			;

Relational_Expr		:	Shift_Expr	{ $$ = $1; }
			|	Relational_Expr T_LeftAngle Shift_Expr { $$ = new RelationalExpr($1, new Operator(@2,"<"), $3); } 
			|	Relational_Expr T_RightAngle Shift_Expr { $$ = new RelationalExpr($1, new Operator(@2,">"), $3); }
			|	Relational_Expr T_LessEqual Shift_Expr { $$ = new RelationalExpr($1, new Operator(@2, "<="), $3); }
			|	Relational_Expr T_GreaterEqual Shift_Expr { $$ = new RelationalExpr($1, new Operator(@2,">="), $3); }
			;

Shift_Expr		:	Additive_Expr { $$ = $1; }
			;

Additive_Expr		:	Multiplicative_Expr { $$ = $1; }
			|	Additive_Expr T_Plus Multiplicative_Expr { $$ = new ArithmeticExpr($1, new Operator(@2,"+"), $3); }
			|	Additive_Expr T_Dash Multiplicative_Expr { $$ = new ArithmeticExpr($1, new Operator(@2,"-"), $3); }
			;


Multiplicative_Expr	:	Unary_Expr { $$ = $1; }
			|	Multiplicative_Expr T_Star Unary_Expr { $$ = new ArithmeticExpr($1, new Operator(@2,"*"), $3); }
			|	Multiplicative_Expr T_Slash Unary_Expr { $$ = new ArithmeticExpr($1, new Operator(@2,"/"), $3); }
			;

Unary_Expr		:	Postfix_Expr { $$ = $1; }
			|	T_Inc Unary_Expr { $$ = new ArithmeticExpr(new Operator(@1,"++"), $2); }
			|	T_Dec Unary_Expr { $$ = new ArithmeticExpr(new Operator(@1,"--"), $2); }
			|	Unary_Operator Unary_Expr { $$ = new ArithmeticExpr($1, $2); }
			;

Unary_Operator		:	T_Plus { $$ = new Operator(@1,"+"); }
			|	T_Dash { $$ = new Operator(@1,"-"); }
			;


Postfix_Expr		:	Primary_Expr { $$ = $1; }
			| 	Postfix_Expr T_LeftBracket Integer_Expr T_RightBracket { $$ = new ArrayAccess(@1,$1,$3);  }
			|	Function_Call { $$ = $1; }
			|	Postfix_Expr T_Dot T_Identifier { $$ = new FieldAccess($1, new Identifier(@3,$3));}
			|	Postfix_Expr T_Inc { $$ = new PostfixExpr($1, new Operator(@2,"++"));}
			|	Postfix_Expr T_Dec { $$ = new PostfixExpr($1, new Operator(@2,"--"));}
			;

Integer_Expr		:	Expr { $$ = $1; }

Constant_Expr		:	Conditional_Expr { $$ = $1; }


Function_Call		:	Function_Call_Or_Method { $$ = $1; }
			;

Function_Call_Or_Method :	Function_Call_Generic { $$ = $1; }
			;

Function_Call_Generic	:	Function_Call_Header_With_Params T_RightParen { $$ = $1; }
			|	Function_Call_Header_No_Params T_RightParen { $$ = $1; }
			;

Function_Call_Header_With_Params: Function_Call_Header Assignment_Expr { ($$=$1)->AppendArgs($2);}
				| Function_Call_Header_With_Params T_Comma Assignment_Expr { ($$=$1)->AppendArgs($3);}

Function_Call_Header_No_Params: Function_Call_Header T_Void { $$ = $1; }
			      | Function_Call_Header { $$ = $1; }

Function_Call_Header	:	Function_Identifier T_LeftParen {  $$ = new Call(@1,NULL,$1,new List<Expr*>());}
			;

Function_Identifier	:	Type_Specifier	{ $$ = new Identifier(@1,$1->getTypeName()); }
			|	Variable_Identifier {  $$ = $1; }
			;

Primary_Expr		:	Variable_Identifier { $$ = new VarExpr(@1,$1);}
			|	T_IntConstant	{ $$ = new IntConstant(@1,$1);}
			|	T_FloatConstant { $$ = new FloatConstant(@1,$1);}
			|	T_BoolConstant  { $$ = new BoolConstant(@1,$1);}
			|	T_LeftParen Expr T_RightParen	{ $$ = $2; }
			;

Variable_Identifier	:	T_Identifier { $$ = new Identifier(@1,$1); }
			;

Expr			:	Assignment_Expr { $$ = $1; }
			;	

	
Parameter_Type_Specifier :	Type_Specifier { $$ = $1; }
			 ;	

Parameter_Declarator	:	Type_Specifier T_Identifier { $$= new VarDecl(new Identifier(@2,$2), $1); }
			|	Type_Specifier T_Identifier Array_Specifier { $$ = new VarDecl(new Identifier(@2,$2), new ArrayType(@1,$1)); }
			;

Array_Specifier		:	T_LeftBracket Constant_Expr T_RightBracket { $$ = '\0'; }
			;	


Function_Header		:	Fully_Specified_Type T_Identifier T_LeftParen {
				$$ = new FnDecl(new Identifier(@2,$2), $1.type_specifier,new List<VarDecl*>()); 
				if ($1.type_qualifier != NULL){
					$$->SetTypeQualifier($1.type_qualifier);
				}
			}
			;

Fully_Specified_Type	:	Type_Specifier	{ $$.type_specifier = $1; }
			|	Type_Qualifier Type_Specifier { $$.type_qualifier = $1; $$.type_specifier = $2;}
			;

Type_Qualifier		:	Single_Type_Qualifier { $$=$1; }
			;

Single_Type_Qualifier	:	Storage_Qualifier { $$ = $1; }
			;

Storage_Qualifier	:	T_Const { $$ = TypeQualifier::constTypeQualifier;}
			|	T_In	{ $$ = TypeQualifier::inTypeQualifier; }
			|	T_Out	{ $$ = TypeQualifier::outTypeQualifier;}
			|	T_Uniform { $$ = TypeQualifier::uniformTypeQualifier;}
			;

Type_Specifier		:	Type_Specifier_Nonarray { $$=$1; }
			|	Type_Specifier_Nonarray Array_Specifier { $$= new ArrayType(@1,$1); }
			;


Type_Specifier_Nonarray :	T_Void  { $$ = Type::voidType; }
			|	T_Float { $$ = Type::floatType; }
			|	T_Int	{ $$ = Type::intType;}
			|	T_Uint	{ $$ = Type::uintType; }
			|	T_Bool	{ $$ = Type::boolType; }
			|    	T_Vec2 	{ $$ = Type::vec2Type; }
               		|    	T_Vec3 	{ $$ = Type::vec3Type; }
               		|    	T_Vec4 	{ $$ = Type::vec4Type; }
			|	T_Ivec2 { $$ = Type::ivec2Type; }
			|	T_Ivec3 { $$ = Type::ivec3Type; }
			|	T_Ivec4 { $$ = Type::ivec4Type; }
			|	T_Bvec2 { $$ = Type::bvec2Type; }
			|	T_Bvec3 { $$ = Type::bvec3Type; }
			|	T_Bvec4 { $$ = Type::bvec4Type; }
			|	T_Uvec2 { $$ = Type::uvec2Type; }
			|	T_Uvec3 { $$ = Type::uvec3Type; }
			|	T_Uvec4	{ $$ = Type::uvec4Type; }
               		|    	T_Mat2 	{ $$ = Type::mat2Type; }
               		|    	T_Mat3 	{ $$ = Type::mat3Type; }
               		|    	T_Mat4 	{ $$ = Type::mat4Type; }
               		;
%%

/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated y.tab.c file.
 * This section is where you put definitions of helper functions.
 */

/* Function: InitParser
 * --------------------
 * This function will be called before any calls to yyparse().  It is designed
 * to give you an opportunity to do anything that must be done to initialize
 * the parser (set global variables, configure starting state, etc.). One
 * thing it already does for you is assign the value of the global variable
 * yydebug that controls whether yacc prints debugging information about
 * parser actions (shift/reduce) and contents of state stack during parser.
 * If set to false, no information is printed. Setting it to true will give
 * you a running trail that might be helpful when debugging your parser.
 * Please be sure the variable is set to false when submitting your final
 * version.
 */
void InitParser()
{
   PrintDebug("parser", "Initializing parser");
   yydebug = false;
}
