public class IDStatement extends Expression{

  String name;
  Cls type;
  Cls clsScope;
  Method methodScope;
  Var var;
  

  public IDStatement(String n, Cls c, Method m) {
    name = n;
    clsScope = c;
    methodScope = m;
  }

  public void setVar(Var v){ var = v; }
  public void setType(Cls c){ type = c; }
  public void setMethodScope( Method c ){ methodScope = c; }
  public void setClsScope( Cls c ){ clsScope = c; } 
  public void setName(String s){ name = s; }
  public Var getVar(){ return var; }
  public Cls getType(){ return type; }
  public Method getMethodScope(){ return methodScope; }  
  public Cls getClsScope(){ return clsScope; } 
  public String getName(){ return name; }
  boolean isFinal(){ if( var == null ) return false; return var.isFinal(); }
  Expression getExpression(){ return var==null?null:var.getExpression(); }
  void setExpression( Expression e ){
    if( var != null ) var.setExpression( e );
  }

  public String toString() {
    String idStatString = ("Identifier: "+ this.name +
                           " \n\tExists in Class: "+clsScope.getName() + 
                           " \n\tExists in Method: "+methodScope.getName()+"\n");
    return idStatString;
  }

  void resolve(){
    Var v=null;
    if( methodScope != null ) v = methodScope.resolveVar( name );
    if( v == null ) v = clsScope.resolveVar( name );
    if( v == null ){
      System.out.println( "Cannot resolve symbol: " + name + "\n");
      CompilerSystem.addTypeCheckError();
      System.exit( 42 );
    }
    else var = v; 
  }

  void typeCheck() throws TypeCheckException {
    type=(var==null)?null:var.getType();
  }

  
  void genStore(){
    var.genStore();
  }

  void genLoad(){
    var.genLoad();
  }

  void genCode(){
    genLoad();
  }

}
