/**
 *   File Name: ErrorParser.java
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


/**
 *
 */
class ErrorParser{

  int line;

  /**
   *
   */
  static public void main(String args[]){
    ErrorParser ep = new ErrorParser( new Exception
      ("Encountered \"end\" at line 18, column 1."));
    System.out.println( ep.line );
  }

  /**
   *
   */
  ErrorParser( Exception e ){
    line = getLine( e.toString() );
  }

  int getLine( String s ){
    String num = s.substring( s.indexOf( "line" ) + 5, 
                              s.indexOf( "," ));
    return Integer.parseInt( num );
  }

}
