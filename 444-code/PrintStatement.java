/* Generated by Together */

public class PrintStatement extends Statement{

  Expression exp;
  
  void setExp( Expression e ){ exp=e; }
  Expression getExp(){ return exp; }
  
  void typeCheck() throws TypeCheckException {
    exp.typeCheck();
    ensureInt( exp, "<PRINT STATEMENT>" );
  }

  void genCode(){
    System.out.println( "getstatic java/lang/System/out Ljava/io/PrintStream;" );
    exp.genCode();
    System.out.println( "invokestatic java/lang/String/valueOf(I)Ljava/lang/String;" );
    System.out.println( "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V" );
  }


}

