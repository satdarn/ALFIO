# Goals, 
- Lexically analysis 
- Parse into AST 
- Generate asm 

# Language specs 
- Addition
- Subtraction 
- 64 bit Integer variables 

# Grammar

```
prgm: assign | expr | print_stmt
assign: var_ident "=" expr
expr: term "+" expr | term "-" expr | term 
term: integer_literal | var_ident 
print_stmt: "!print:" expr "!"
```

#example program

```
x = 3 + 4 
y = 0
!print:x+1!
!print:y! 
```

# Lexical analysis

- stream of character: (maybe borrow input stream from glyph)

States: 
Program:
    
    consume_char until{
        if isIdent() && nextchar == '=' -> Program.addChild(Assign)
        else if isIdent -> Program.addChild(Expression)
        else if isInteger -> Program.addChild(Expression)
        else if isPrint -> Print)
    }

    Assign: Returns Variable Node 
        HandleIdent 
        consume '='
        Create variable node and set its head to head
    






