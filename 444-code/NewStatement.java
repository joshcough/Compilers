public class NewStatement extends Expression {

  Expression arrayExp;
  boolean isArray = false;

  public NewStatement(Cls t) { 
    type = t;
  }  

  public NewStatement( Cls t, Expression e ) { 
    arrayExp = e; 
    isArray = true;
    type = (Cls)Cls.typeToArrayMap.get( t );
    if( type == null ){
      System.out.println( "Cannot have an array of type " + t.getName() );
      CompilerSystem.addTypeCheckError(); 
      type = Cls.ANY;
    }
  } 
  
  public void setType(Cls c){ type = c; }
  public Cls getType(){ return type; }
  
  public String toString() { return "New Statement: " + type.getName(); }

  void typeCheck() throws TypeCheckException {
    if( Cls.isPrimitive( type ) || type == Cls.VOID ){
      printPrimitiveNewError( type );
      throw new TypeCheckException();
    }
    if( arrayExp != null ){
      arrayExp.typeCheck();
      if( arrayExp.getType() != Cls.INT ){
        printIncompatibleTypesError( arrayExp.getType(), Cls.INT );
        throw new TypeCheckException();
      }
    }
  }

  void printPrimitiveNewError( Cls c ){
    System.out.println( "New Statement cannot be applied to primitive: " + c.getName() );
  }

  void genCode(){
    if( isArray ){
      arrayExp.genCode();
      Cls c = (Cls)Cls.arrayToTypeMap.get( type );
      if( ! Cls.isPrimitive( c ) ) System.out.print( "a" );
      System.out.println( "newarray " + c.getName() );
    }
    else{
      System.out.println( "new " + type.getName() );
      System.out.println( "dup" );
      System.out.println( "invokenonvirtual " + 
                          type.getName() 
                          + "/<init>" + "()V");
    }
  }

  void genLoad(){ genCode(); }

}
