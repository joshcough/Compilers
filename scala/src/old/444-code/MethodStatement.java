import java.util.Iterator;

public class MethodStatement extends Expression{
  
  IDStatement idStat;
  String methodId;
  Cls scope;
  ExpressionList el;
  Method method;
  
  public MethodStatement(String methId, IDStatement id, Cls s, ExpressionList els ) { 
    methodId = methId;
    idStat = id;
    scope = s;
    el = els;
    method = null;
    resolve();
  }
  

  public void setM( Method m ){ method = m; }
  public void setScope( Cls c ){ scope = c; }
  public void setMethod( String m) { methodId = m;}
  public void setIDStatement(IDStatement id) { idStat = id;}
  
    
  public Method getM(){ return method; }
  public String getMethod() {return methodId;}
  public IDStatement getIDStatement() {return idStat;}
  public Cls getScope(){ return scope; }
  
  public String toString() {
    String methStatString = "Method\n";
    if(idStat != null) { 
        methStatString += "\tOwner: " + idStat.getName() + "\n"; 
    }
    methStatString += ("\tName: "+ methodId +
                      " \n\tExists in: "+scope.getName()+"\n");
    return methStatString;
  }
   
  void resolve(){
      if( idStat != null ){
          idStat.resolve();
          try{
              idStat.typeCheck();
          } catch( TypeCheckException tce ){}
          if( idStat.getType() == null ){}
          else{
              Method m = idStat.getType().resolveMethod( methodId );
              if(m==null) System.out.println( "Cannot resolve symbol: " + methodId + "\n");
              else method = m;
          }
      }
      else{
          Method m = scope.resolveMethod( methodId );
          if( m==null)  System.out.println( "Cannot resolve symbol: " + methodId );
          else method = m;
      } 
  }

    public void typeCheck() throws TypeCheckException { 
      if( method == null ){ type = Cls.ANY; throw new TypeCheckException(); }
      el.typeCheck();
      type = method.getType();
      Params params = method.getParams();

      if( el.size() != params.size() ){
          printInvalidMethodCall( params, el );
          throw new TypeCheckException();
      }

      Iterator elit = el.iterator();
      for( Iterator pait = params.iterator(); pait.hasNext(); ){
        Expression e = (Expression)elit.next();
        Param p = (Param)pait.next();
        if( e.getType() != p.getType() ){
          printInvalidMethodCall( params, el );
          throw new TypeCheckException();
        }
      }

    }

    void printInvalidMethodCall( Params ps, ExpressionList el ){
        System.out.print( "Call: " + method.getName() + "( " );
        printExpressionTypes( el );
        System.out.print( " ) cannot be applied to Method: ");
        System.out.print( method.getName() + "( " );
        printParamTypes( ps );
        System.out.println(" )\n");
    }

    void printParamTypes( Params ps ){
      for( Iterator it = ps.iterator(); it.hasNext(); ){
        System.out.print( ((Param)it.next()).getType().getName() );
        if( it.hasNext() ) System.out.print(", ");
      }
    }
 
   void printExpressionTypes( ExpressionList el ){
      for( Iterator it = el.iterator(); it.hasNext(); ){
        System.out.print( ((Expression)it.next()).getType().getName());
        if( it.hasNext() ) System.out.print(", ");
      }
    }

 
  void genCode(){
    System.out.println(";=============== method call =================");
    if( idStat != null ) idStat.genLoad();
    else System.out.println( "aload 0" );
    el.genLoad();
    genSignature();
    System.out.println(";=============== method  end =================");
  }
  
  void genLoad(){ genCode(); }
  
  void genSignature(){
    Cls c = (idStat != null)?idStat.getType():scope;
    System.out.print( "invokevirtual " );
    System.out.print( c.getName() + "/" + method.getName() + "(" );
    el.genSignature();
    System.out.print( ")" );
    String s = getType().genSignature();
    System.out.print( s );
    if( ! s.equals( "I" ) &&  ! s.equals( "C" ) &&  ! s.equals( "Z" ) && ! s.equals( "V" ) )
      System.out.print(";");
    System.out.println();
  }
 
}
