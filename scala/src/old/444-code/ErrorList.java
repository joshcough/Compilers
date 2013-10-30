/**
 *   File Name: ErrorList.java
 *   Author: Joshua D. Cough
 *   Description: 
 */

/**
 *   Revision History
 *   -----------------------------------------------------
 *   Date               Author            Changes
 *   -----------------------------------------------------
 *   Fri Mar  9, 2001	Joshua D. Cough	  Created
 */


/** package **/


/** imports **/
import java.util.*;

/**
 *
 */
class ErrorList extends ArrayList{

  int max = 100;

  /**
   *
   */
  static public void main(String args[]){

  }

  /**
   *
   */
  ErrorList(){
  }

  public void addException( Exception e ){
    if( size() <= 100 )
      add( e );
  }

  public void print(){
    if( size() > 0 ){
      for( Iterator it = this.iterator(); it.hasNext(); )
        System.out.println( ((Exception)it.next()).toString() + "\n" );
      System.out.println( "\n" + size() + " error(s)");
    }
  }
}
