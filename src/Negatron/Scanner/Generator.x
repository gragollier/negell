{
module Negatron.Scanner.Generator where
import Negatron.Ast
}

%wrapper "basic"

$alpha = [a-zA-Z]
$digit = 0-9
$newline = [\n]


tokens :-
 $white+  ;
 "/*" ( $newline | [^\*] | \*+ ($newline | [^\/]) )* "*/" ;
 "//" [^$newline]* $newline ;
 \(       { const LPAREN   }
 \)       { const RPAREN   }
 \{       { const LBRACE   }
 \}       { const RBRACE   }
 \;       { const LSemi    }
 \,       { const LComma   }
 \-       { const LSub     }
 \*       { const LMul     }
 \/       { const LDiv     }
 \=       { const LAssign  }
 \=\=     { const LEqual   }
 \!\=     { const LNeq     }
 \<       { const LLess    }
 \<\=     { const LLeq     }
 \>       { const LGreater }
 \>\=     { const LGeq     }
 \&\&     { const LAnd     }
 \|\|     { const LOr      }
 \!       { const LNot     }
 "if"     { const LIf      }
 "else"   { const LElse    }
 "while"  { const LWhile   }
 "return" { const LRet     }
 "int"    { const $ LType IntType   }
 "bool"   { const $ LType BoolType  }
 "void"   { const $ LType VoidType  }
 "true"   { const $ LBool True    }
 "false"  { const $ LBool False   }
 "NULL"   { const LNull }
 "output" { const LOutput }
 "input"  { const LInput }

 $digit+  { LInt . read }
 $alpha [$alpha $digit \_]* { LId }
 \" [^\"]* \"  { LStrLit . read -- this doesn't handle quote escaping }

{
data Lexeme = LInt Int
            | LStrLit String
            | LId String
            | LType Type
            | LBool Bool
            | LNull
            | LRet
            | LAssign
            | LComma
            | LSemi
            | LPAREN
            | RPAREN
            | LBRACE
            | RBRACE
            | LBRACK
            | RBRACK
            | LFor
            | LWhile
            | LIf
            | LElse
            | LSub
            | LMul
            | LDiv
            | LEqual
            | LNeq
            | LLess
            | LLeq
            | LGreater
            | LGeq
            | LAnd
            | LOr
            | LOutput
            | LInput
            | LNot
}