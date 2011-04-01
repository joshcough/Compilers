public class ElseIfStat extends Statement{

  Expression cond;
  Statements truePart;
  
  void setCond( Expression e ){ cond = e; }
  void setTruePart( Statements s ){ truePart = s; }
  
  public String toString(){ 
    String toString = "Else If Statement( " + cond.toString() + "){\n";
    if( truePart!=null ) toString+=truePart.toString();
    toString += "\n}";
    return toString;
  }   
   
  void typeCheck() throws TypeCheckException{
    cond.typeCheck();
    ensureBool(cond,"<ElseIf Stat>");
    truePart.typeCheck();
  }

  void genCode( String label2 ){
    String label1 = Cls.getLabel();
    cond.genCode();
    System.out.println( "ifeq " + label1 );
    truePart.genCode();
    System.out.println( "goto " + label2 );
    System.out.println( label1 + ":" );
  }
  
}


