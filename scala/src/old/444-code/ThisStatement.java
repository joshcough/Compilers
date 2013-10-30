public class ThisStatement extends Expression{

  Cls scope;
  Cls type;
  
  public ThisStatement(Cls c) {
    scope = c; type=scope;
  }
  
  public void setType(Cls c){ type = c; }
  public void setScope( Cls c ){ scope = c; }
  
  public Cls getType(){ return type; }
  public Cls getScope(){ return scope;}

  void genCode(){
    System.out.println("aload 0");
  }

  void genLoad(){
    genCode();
  }

}
