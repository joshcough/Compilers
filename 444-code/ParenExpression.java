
public class ParenExpression extends Expression{
  
  Expression exp;
  
  public void setExpression(Expression e) { exp = e; }

  public String toString() { return "Paren Expression"; }

  void typeCheck() throws TypeCheckException{
    exp.typeCheck();
    type = exp.getType();
  }

  void genCode(){
    exp.genCode();
  }

  void genLoad(){
    genCode();
  }
  
}
