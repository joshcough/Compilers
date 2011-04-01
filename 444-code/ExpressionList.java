import java.util.*;

class ExpressionList extends ArrayList{
  
  void typeCheck() throws TypeCheckException {
    for(Iterator it = iterator(); it.hasNext();){
      ((Expression)it.next()).typeCheck(); 
    }
  }

    void print(){
      for(Iterator it = iterator(); it.hasNext();){
         System.out.println(((Expression)it.next()).toString()); 
      }
    }

  void genLoad(){
     for(Iterator it = iterator(); it.hasNext();){
         ((Expression)it.next()).genLoad(); 
      }
  }

  void genSignature(){
    for(Iterator it = iterator(); it.hasNext();){
      Cls c = ((Expression)it.next()).getType();
      System.out.print( c.genSignature() );
      if( ! Cls.isPrimitive( c ) ) System.out.print( ";" );
    }
  }

}
