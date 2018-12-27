/*
 * MIT License
 *
 * Copyright (c) 2018 Konrad Kleczkowski
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

grammar Imperator;

@parser::header {
    import java.util.*;
    import java.lang.*;
    import java.util.*;

    import scala.*;
    import scala.math.*;
    import static scala.collection.JavaConverters.*;

    import com.github.repaj.kompilator.*;
    import com.github.repaj.kompilator.parser.ast.*;
}

@parser::members {
    private SymbolTable table = new SymbolTable();

    public SymbolTable getSymbolTable() {
        return table;
    }
}

compilationUnit
returns [Block node]
@after {
    StdOut.validate();
}
    : program EOF { $node = $program.node; }
    ;

program
returns [Block node]
    : 'DECLARE' decls 'IN' stmts 'END' { $node = $stmts.node; }
    ;

decls
    : (decl ';')*
    ;

decl
    : Id '(' startIdx=num ':' stopIdx=num ')' { table.newArray(new Location($ctx), $Id.text, $startIdx.bigInt, $stopIdx.bigInt); }
    | Id { table.newVariable(new Location($ctx), $Id.text, false); }
    ;

stmts
returns [Block node]
locals [List<Stmt> stmtList]
@init {
    $stmtList = new ArrayList<>();
}
    : (stmt { $stmtList.add($stmt.node); })+
                                { $node = new Block(asScalaBuffer($stmtList)); }
    ;

stmt
returns [Stmt node]
    : ifStmt                    { $node = $ifStmt.node; }
    | whileStmt                 { $node = $whileStmt.node; }
    | doWhileStmt               { $node = $doWhileStmt.node; }
    | forStmt                   { $node = $forStmt.node; }
    | assignStmt                { $node = $assignStmt.node; }
    | readStmt                  { $node = $readStmt.node; }
    | writeStmt                 { $node = $writeStmt.node; }
    ;

ifStmt
returns [If node]
locals [Block elseBlock]
@init {
    $elseBlock = new Block(asScalaBuffer(new ArrayList<>()));
}
    : 'IF' condition
      'THEN' ifTrue=stmts
      ('ELSE' ifFalse=stmts     { $elseBlock = $ifFalse.node; })?
      'ENDIF'                   { $node = new If($condition.node, $ifTrue.node, $elseBlock); }
    ;

whileStmt
returns [While node]
    : 'WHILE' condition
      'DO' body=stmts
      'ENDWHILE'                { $node = new While($condition.node, $body.node); }
    ;

doWhileStmt
returns [DoWhile node]
    : 'DO' body=stmts
      'WHILE' condition
      'ENDDO'                   { $node = new DoWhile($condition.node, $body.node); }
    ;

forStmt
returns [For node]
locals [boolean downTo, SymbolTable.VariableEntry entry]
@init {
    table = table.enterContext();
}
@after {
    table = table.exitContext();
}
    : 'FOR' itName=Id           { table.newVariable(new Location($ctx), $itName.text, true);
                                  $entry = table.lookup($itName.text).asVariable();
                                  $entry.asVariable().initialized_\$eq(true); }
      'FROM' lo=operand ('TO' { $downTo = false; } | 'DOWNTO' { $downTo = true; }) hi=operand
      'DO' body=stmts
      'ENDFOR'                  { $node = new For($entry, $lo.node, $downTo, $hi.node, $body.node); }
    ;

assignStmt
returns [Assign node]
    : lhs=ref[false] ':=' rhs=expr ';'
      {
        $node = new Assign($lhs.node, $rhs.node);
        if (($lhs.node.entry().isArray() && $lhs.node instanceof VariableRef)
            || ($lhs.node.entry().isVariable() && $lhs.node instanceof ArrayRef)) {
            StdOut.error(new Location($ctx), "type mismatch");
        } else if ($lhs.node instanceof VariableRef && $lhs.node.entry().asVariable().iterator()) {
            StdOut.error(new Location($ctx), "assignment to the for-loop iterator");
        }
        if ($lhs.node instanceof VariableRef && $lhs.node.entry().isVariable()) {
            $lhs.node.entry().asVariable().initialized_\$eq(true);
        }
      }
    ;

readStmt
returns [Read node]
    : 'READ' ref[false] ';'
      {
        $node = new Read($ref.node);
        if ($ref.node instanceof VariableRef && $ref.node.entry().isVariable()) {
            $ref.node.entry().asVariable().initialized_\$eq(true);
        }
      }
    ;

writeStmt
returns [Write node]
    : 'WRITE' operand ';'       { $node = new Write($operand.node); }
    ;

expr
returns [Expr node]
    : operand                   { $node = $operand.node; }
    | l=operand '+' r=operand   { $node = new Add($l.node, $r.node); }
    | l=operand '-' r=operand   { $node = new Sub($l.node, $r.node); }
    | l=operand '*' r=operand   { $node = new Mul($l.node, $r.node); }
    | l=operand '/' r=operand   { $node = new Div($l.node, $r.node); }
    | l=operand '%' r=operand   { $node = new Rem($l.node, $r.node); }
    ;

condition
returns [Cond node]
    : l=operand '=' r=operand   { $node = new Eq($l.node, $r.node); }
    | l=operand '!=' r=operand  { $node = new Ne($l.node, $r.node); }
    | l=operand '<' r=operand   { $node = new Lt($l.node, $r.node); }
    | l=operand '>' r=operand   { $node = new Gt($l.node, $r.node); }
    | l=operand '<=' r=operand  { $node = new Le($l.node, $r.node); }
    | l=operand '>=' r=operand  { $node = new Ge($l.node, $r.node); }
    ;

operand
returns [Value node]
    : ref[true]                       { $node = $ref.node; }
    | constant                  { $node = $constant.node; }
    ;

ref [boolean rhs]
returns [Ref node]
    : arrayRef[rhs]                  { $node = $arrayRef.node; }
    | variableRef[rhs]               { $node = $variableRef.node; }
    ;

variableRef [boolean rhs]
returns [VariableRef node]
    : variableSymb                      { $node = new VariableRef($variableSymb.entry);
                                          if (rhs && $variableSymb.entry instanceof SymbolTable.VariableEntry
                                            && !$variableSymb.entry.asVariable().initialized()) {
                                            StdOut.error(new Location($ctx), $variableSymb.entry + " is not initialized but used");
                                         } }
    ;

arrayRef [boolean rhs]
returns [ArrayRef node]
    : arraySymb '(' constant ')'        { $node = new ArrayRef($arraySymb.entry, $constant.node); }
    | arraySymb '(' variableRef[false] ')'     { $node = new ArrayRef($arraySymb.entry, $variableRef.node); }
    ;

variableSymb
returns [SymbolTable.Entry entry]
    : Id { $entry = table.getVariable(new Location($ctx), $Id.text); }
    ;

arraySymb
returns [SymbolTable.Entry entry]
    : Id { $entry = table.getArray(new Location($ctx), $Id.text); }
    ;

constant
returns [Constant node]
    : num { $node = new Constant($num.bigInt); }
    ;

num
returns [BigInt bigInt]
    : Num
      {
        $bigInt = BigInt.apply($Num.text);
        if (!$bigInt.isValidLong()) {
            StdOut.error(new Location($ctx), $bigInt + " is not valid 64-bit integer");
        }
      }
    ;

Id
    : [a-z_]+
    ;

Num
    : '0'|[1-9][0-9]*
    ;

Ws
    : [ \n\r\t]+ -> skip
    ;

Comment
    : '[' .*? ']' -> skip
    ;