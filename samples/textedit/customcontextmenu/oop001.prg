/****************************************************************************
** $HQL_BEGIN_LICENSE$
**
** Copyright (C) 2020 by Luigi Ferraris
**
** This file is part of HQL project
**
** HQL is free software: you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation, either version 3 of the License, or
** (at your option) any later version.
**
** HQL is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with HQL.  If not, see <http://www.gnu.org/licenses/>.
**
** $HQL_END_LICENSE$
****************************************************************************/
#include "hqlinclude.ch"

/*!

 \brief starting procedure

*/
INIT PROCEDURE ThisInit()

   hb_CdpSelect( hb_CdpOS() )    // to align HVM to os codePage
   hb_SetTermCP( hb_CdpTerm() )  //where <cTermCP> is OS encoding and <cHostCP> is HVM encoding. When <cHostCP> is not given then _SET_CODEPAGE is used
   SET( _SET_OSCODEPAGE, hb_CdpOS() )
   SET( _SET_DBCODEPAGE, "ITWIN" )        // I choose Italian

   SET( _SET_EPOCH, 2000 )
   SET CENTURY ON
   SET( _SET_EXCLUSIVE, .F. )
   SET( _SET_DELETED, .F. )

RETURN

/*!

 \brief ending procedure

*/
EXIT PROCEDURE ThisExit()

   DBCOMMITALL()
   DBCLOSEALL()

RETURN

/*

   standard main procedure

*/
PROCEDURE Main()

   hqlErrorSys()  /*hbqt_errorsys()*/

   hqlSetStyle( "Fusion" )

   hqlOnAboutToQuit( { || UDFOnAboutToQuit() } )

   hqlStart()

   UDFshowMainWindow()

RETURN

STATIC PROCEDURE UDFOnAboutToQuit()
   hql_Trace( PADR("Quitting QApplication", 25) + hb_TtoS(hb_DateTime()) )
RETURN

/*!

 \brief show mainwindow

*/
STATIC PROCEDURE UDFshowMainWindow()
   LOCAL oWnd, this, oSize
   LOCAL oMlayout
   LOCAL cText := "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus pretium magna eu lorem aliquet, et posuere eros dapibus. Etiam volutpat felis elit. Nullam quis enim ligula. Morbi laoreet dapibus tincidunt. Etiam enim ex, facilisis luctus facilisis at, porttitor vitae orci. Mauris interdum scelerisque suscipit. Donec leo dolor, vehicula sit amet ipsum eget, consectetur volutpat tellus." + ;
                  hb_Eol() + ;
                  "Vivamus sit amet pulvinar erat, eu lacinia urna. Pellentesque cursus eleifend nunc, in luctus erat efficitur sed. Etiam a ullamcorper urna. Nullam sagittis, eros ut interdum eleifend, tortor nunc posuere est, sed eleifend velit massa quis mi. Sed at nisl vel enim tincidunt tincidunt. Nunc non lorem sed dui consectetur consequat. Morbi congue suscipit augue at sollicitudin. Maecenas nulla turpis, aliquet at porta sit amet, pharetra eu augue."

   WITH OBJECT oWnd := hqlMainWindow(/*name*/)
      this := :hqlThis()
      :setWindowTitle( "HQLTEXTEDIT tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget(/*name*/, this) )
      :centralWidget():setLayout( QVBoxLayout() )

      WITH OBJECT hqlMenuBar(/*name*/)
         WITH OBJECT :hqlAddMenu(/*name*/)
            :hqlCaption( "&File" ) //==>:setTitle( "&File" )
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Quit" )  //==>:setText( "&Quit" )
               :setIcon( QIcon( ":/hqlres/quit" ) )
               :setShortcut( QKeySequence( "Alt+Q" ) )
               :hqlOnTriggered( { || oWnd:hqlRelease() } )
            END WITH
            :addSeparator()
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Close all windows" )
               :hqlOnTriggered( { || hqlQapplication:closeAllWindows() } )
               :setIcon( QIcon( ":/hqlres/exit" ) )
            END WITH
         END WITH
      END WITH
   END WITH

   WITH OBJECT oWnd:centralWidget()
      oMlayout := :layout()

      WITH OBJECT hqlTextEdit(/*name*/)
         :hqlAddMeToLayout( oMlayout )
         :hqlSetCustomMenuEnabled( .T. )
         :setText( cText )
         :hqlOnTextChanged( { |oself| UDFOnTextChanged(oSelf) } )
      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN
STATIC PROCEDURE UDFOnTextChanged(oself)
   LOCAL nCursorPos

   nCursorPos := oself:textCursor:position()
   hql_Trace( PADR("OnTextChanged: ",25) + "curPos: " + hb_NtoS(nCursorPos) + " " +  oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
   hql_Trace( oself:toHtml() )
RETURN
