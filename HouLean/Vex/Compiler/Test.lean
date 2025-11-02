import HouLean.Vex.Compiler.Grammar



#check `(vexExpr| pos = point(0, "P", @ptnum))
#check `(vexExpr| pos += point(0, "P", @ptnum))
#check `(vexStmt| int a = 1;)
#check `(vexStmtLike| int a = 1;)
#check `(vexSnippet| 
  float foo() {
    @P = 10;
    return 0;
  }
  int a = 1;
  vector p = 0;)


-- As a statement
#check `(vexStmt| int a = 1;)
#check `(vexStmtLike| int a = 1;)




-- As an expression (this would be an identifier, not a declaration)
#check `(vexExpr| a)

-- Or an assignment expression
#check `(vexExpr| a = 1)


