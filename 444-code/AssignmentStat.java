
public class AssignmentStat extends Statement {

  Expression lhs;
  Expression rhs;
  
  AssignmentStat( Expression left, Expression right ){ lhs=left; rhs=right; }
  
  void setLeft( Expression e ){ lhs = e; }
  void setRight( Expression e ){ rhs=e; }
  Expression getLeft(){ return lhs; }
  Expression getRight(){ return rhs; }
  
  public String toString(){
    String toString = "Assignment: " + 
      lhs.toString() + " = " +
      rhs.toString();
    return toString;                     
  }
  
//   void typeCheck() throws TypeCheckException {
//     boolean array = false;
//     ArrayExpression ae = null;
//     if( lhs instanceof ArrayExpression ){
//       ae = (ArrayExpression)lhs;
//       ((ArrayExpression)lhs).typeCheck();
//       lhs = ae.getIDStatement();
//       array = true;
//     }
//     if( ! (lhs instanceof IDStatement) ){
//       illegalLhsError( lhs );
//       throw new TypeCheckException();
//     }
//     lhs.typeCheck();
//     if( ((IDStatement)lhs).isFinal() ){
//       if( ((IDStatement)lhs).getExpression() != null ){
//         assigningFinalError( (IDStatement)lhs );  
//         throw new TypeCheckException();
//       }
//       else{ ((IDStatement)lhs).setExpression( rhs ); }
//     }
//     rhs.typeCheck();
//     Cls leftc = null; 
//     if( array ) 
//       leftc = (Cls)Cls.arrayToTypeMap.get( lhs.getType() );
//     else
//       leftc = lhs.getType();
//     Cls rightc = rhs.getType();
//     ensureCompatibleTypes( leftc, rightc );
//     if( array ) lhs = ae;
//   }

  void typeCheck() throws TypeCheckException {
    if( lhs instanceof ArrayExpression ){
      arrayExpTypeCheck();
      return;
    }
    if( ! (lhs instanceof IDStatement) ){
      illegalLhsError( lhs );
      throw new TypeCheckException();
    }
    else idStatTypeCheck();
  }

  void arrayExpTypeCheck()throws TypeCheckException{
    lhs.typeCheck();
    IDStatement ids = ((ArrayExpression)lhs).getIDStatement();
    if( ids.isFinal() ){
      if( ids.getExpression() != null ){
        assigningFinalError( ids );  
        throw new TypeCheckException();
      }
      else{ ids.setExpression( rhs ); }
    }
    rhs.typeCheck();
    Cls leftc = lhs.getType();
    Cls rightc = rhs.getType();
    ensureCompatibleTypes( leftc, rightc );
  }

  void idStatTypeCheck()throws TypeCheckException{
    lhs.typeCheck();
    if( ((IDStatement)lhs).isFinal() ){
      if( ((IDStatement)lhs).getExpression() != null ){
        assigningFinalError( (IDStatement)lhs );  
        throw new TypeCheckException();
      }
      else{ ((IDStatement)lhs).setExpression( rhs ); }
    }
    rhs.typeCheck();
    Cls leftc = lhs.getType();
    Cls rightc = rhs.getType();
    ensureCompatibleTypes( leftc, rightc );
  }


  void assigningFinalError( IDStatement ids ){
    System.out.println( "cannot assign a value to final variable " + 
                        ids.getVar().getName() + "<" +
                        ids.getVar().getType().getName() +
                        ">\n"
                        );
  }
  
  void illegalLhsError( Expression e ) throws TypeCheckException {
    System.out.println( "Illegal lhs in an assignment statement.\n" );
  }
    
  void incompatibleTypesError( Cls l, Cls r ){
    System.out.println("incompatible types");
    System.out.println("found    : " + r.getName() );
    System.out.println("required : " + l.getName() );
    System.out.println("\n"); 
  }

  void genCode(){
    rhs.genCode();
    if( lhs instanceof IDStatement )
      ((IDStatement)lhs).genStore();
    else lhs.genStore();
  }

}

