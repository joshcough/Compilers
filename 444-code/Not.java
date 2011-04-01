public class Not extends UnaryExpression{
  Expression exp;
  public void setExpression(Expression e) { exp = e; }

  public String toString() { return "Not Expression"; }

 void typeCheck() throws TypeCheckException {
    exp.typeCheck(); 
    type = Cls.BOOL;
    ensureBool( exp, "!" );
  } 

  void genCode(){
    exp.genCode();
    System.out.println( "iconst_1" );
    System.out.println( "ixor" );
  }

  void genLoad(){
    genCode();
  }

}
