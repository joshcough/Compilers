/* Generated by Together */
import java.util.Hashtable;
import java.util.Iterator;

public class Cls {


  static Cls INT = new Cls( "int" );
  static Cls VOID = new Cls("void");
  static Cls ANY = new Cls("any");
  static Cls BOOL = new Cls("boolean");
  static Cls CHAR = new Cls("char");
  static Cls INTARRAY = new Cls( "intarray" );
  static Cls BOOLARRAY = new Cls( "boolarray" );
  static Cls CHARARRAY = new Cls( "chararray" );
  
  static Hashtable arrayToTypeMap = getArrayToTypeMap();
  static Hashtable typeToArrayMap = getTypeToArrayMap();

  static int label = 1;

  Fields fields;
  Methods methods;
  Cls superclass;
  StaticFields staticFields;
  String name;
  int pointer;


  Cls(){ fields = new Fields(); methods = new Methods(); }
  Cls( String name ){ 
    fields = new Fields(); 
    methods = new Methods();
    this.name = name;
  }

  public String toString(){
    String toString="Cls ";
    toString += name;
    if( superclass != null )
        toString += " extends " + superclass.getName();
    toString+="\n\n" + fields.toString()+"\n";
    toString+=methods.toString();
    return toString;
  }

  String genSignature(){
    if(this == Cls.INT) 
      return("I");
    else if(this == Cls.VOID)
      return("V");
    else if(this == Cls.BOOL) 
      return("Z");
    else if(this == Cls.CHAR)
      return("C");
    else{
      if( isArray( this ) )return("[L"+getName());
      else return("L"+getName());
    }
  }

  void genCode(){
    if( this==INT || this==VOID || this==ANY || this==BOOL || this==CHAR ) return;
    System.out.println();
    printHeader();
    System.out.println();
    printFields();
    System.out.println();
    printConstructor();
    methods.genCode();
    System.out.println();
    printStatic();
    System.out.println();
  }

  void printHeader(){
    System.out.println( ".class public " + getName() ); 
    System.out.println( ".super " + getSuperClass().getName() );
  }

  void printFields(){
    fields.genCode();
  }
  
  void printMethods(){
    methods.genCode();
  }
  void printConstructor(){
    System.out.println( "; constructor" );
    System.out.println( ".method public <init>()V" ); 
    System.out.println(".limit stack 99");
    System.out.println(".limit locals 99");
    System.out.println( "aload_0" );
    System.out.println( "invokenonvirtual java/lang/Object/<init>()V" );
    for( Iterator it = fields.keySet().iterator(); it.hasNext(); ){
      Field f = (Field)fields.get ( it.next() );
      if( ! f.isStatic() ){
         if( f.getExpression() != null ){ 
          f.getExpression().genLoad();
          f.genStore();
        }
      }
    }
    System.out.println( "return\n.end method" );
  } 

  void printStatic(){
    System.out.println ( "; class init" );
    System.out.println( ".method public <clinit>()V" );
    System.out.println(".limit stack 99");
    System.out.println(".limit locals 99");
    for( Iterator it = fields.keySet().iterator(); it.hasNext(); ){
      Field f = (Field)fields.get ( it.next() );
      if( f.isStatic() ){ 
        if( f.getExpression() != null ){ 
          f.getExpression().genLoad();
          f.genStore();
        }
      }
    }
    System.out.println( "return\n.end method" );
  }
    

  Fields getFields() { return fields; }

  void addField( Field f ){ 
    if( fields.get( f.getName() ) ==null ){ 
        fields.put( f.getName(),f );
        f.setPointer( pointer++ );
    }
    else{
      System.out.println("Field: " + f.getName() + 
                         " already declared in Class: " + 
                         f.getScope().getName() + "\n");
      CompilerSystem.addTypeCheckError();

    }
  }


  void addMethod( Method m ){ methods.put( m.getName(),m ); m.setPointer( pointer++ ); }
  void setName( String s ){ name = s; }
  String getName(){ return name; }
  void setSuperClass( Cls c ){ superclass = c; }
  Cls getSuperClass(){ return superclass; }   
  
  void setPointer( int i ){ pointer = i; }
  int getPointer(){ return pointer; }   

  Var resolveVar( String name ){

      //System.out.println("Trying to resolve: " + name + " in class " + getName() );

    Field f = (Field)fields.get( name );
    if( f != null ) return f;
    if( superclass == null ) return null;
    else return superclass.resolveVar( name );
  }

  Method resolveMethod( String name ){

      //System.out.println("Trying to resolve: " + name + "() in class " + getName() );

    Method m = (Method)methods.get( name );
    if( m != null ) return m;
    if( superclass == null ) return null;
    else return superclass.resolveMethod( name );
  }
  void typeCheck() throws TypeCheckException {
    try{
      fields.typeCheck();
    }catch( TypeCheckException tce ){ CompilerSystem.addTypeCheckError(); }
    try{
      methods.typeCheck();
    }catch( TypeCheckException tce ){ CompilerSystem.addTypeCheckError(); }
  }
      
  boolean isSuperClass( Cls c ){
    Cls current=c;
    if( c == null ) return false;
    while( current.getSuperClass() != null ){
      if( current.getSuperClass() == this ) return true;
    }
    return false;
  }
  
  static boolean isPrimitive( Cls c ){
    return (c == INT) || (c == BOOL) || (c == CHAR);
  }

  static boolean isLiteral( Cls c ){
    return isPrimitive( c );
  }

  static boolean isNull( Cls c ){
    return c.getName().equals("null");
  }

  static String getLabel(){
    return "Label" + label++;
  }

  static Hashtable getArrayToTypeMap(){
    Hashtable h = new Hashtable();
    h.put( Cls.INTARRAY , Cls.INT );
    h.put( Cls.BOOLARRAY , Cls.BOOL );
    h.put( Cls.CHARARRAY , Cls.CHAR );
    return h;
  }

 
  static Hashtable getTypeToArrayMap(){
    Hashtable h = new Hashtable();
    h.put( Cls.INT , Cls.INTARRAY );
    h.put( Cls.BOOL , Cls.BOOLARRAY ); 
    h.put( Cls.CHAR , Cls.CHARARRAY );
    return h;
  }

  static boolean isArray(Cls c) {
    return (Cls)arrayToTypeMap.get( c ) != null;
  }
}