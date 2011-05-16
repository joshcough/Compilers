
public class WhileStat extends Statement {
  Expression cond;
  Statements body;
  
  void setCond( Expression c ){ cond = c; }
  void setBody( Statements s ){ body = s; }
  Expression getCond(){ return cond; }
  Statements getBody(){ return body; }
  
  public String toString(){
    String toString="WhileStatement while( " + cond.toString() + 
      " ){\n " + body.toString() +"\n}";
    return toString;
  }
  
  void typeCheck() throws TypeCheckException { 
    cond.typeCheck();
    ensureBool(cond,"<While Stat>");
    body.typeCheck();
  }

  void genCode(){
    String label1 = Cls.getLabel();
    String label2 = Cls.getLabel();
    cond.genCode();
    System.out.println( label1 + ":" );
    System.out.println( "ifeq " + label2 );
    body.genCode();
    System.out.println("goto " + label1 );
    System.out.println( label2 + ":" );
  }

  void genLoad(){
    genCode();
  }
}




