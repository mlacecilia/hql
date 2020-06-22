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
   LOCAL oWnd, oSize
   LOCAL oMlayout

   WITH OBJECT oWnd := hqlMainWindow(/*name*/)
      :hqlCaption( "HQLTREEWIDGET tester" ) // ==>:setWindowTitle( "tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget( /*name*/ ) )
      :centralWidget():setLayout( hqlVBoxLayout() )

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

         WITH OBJECT :hqlAddMenu( /*name*/ )
            :hqlCaption( "&Tools" )
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&Explore dir" )
               :hqlOnTriggered( { || udfExploreDir( oWnd:tree1() ) } )
            END WITH
         END WITH
      END WITH

      WITH OBJECT :centralWidget()
         oMlayout := :layout()

         WITH OBJECT hqlTreeWidget( "tree1" )
            :hqlAddMeToLayout( oMlayout )
            :setColumnCount( 2 )
            WITH OBJECT :headerItem()
               :setText( 0, "Name" )
               :setText( 1, "Bytes" )
            END WITH
         END WITH

      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE udfExploreDir( oTreeWdg )
   LOCAL cPath
   LOCAL cFile
   LOCAL cExt
   LOCAL aList
   LOCAL aInfo

   hb_fNameSplit( hb_ProgName(), @cPath, @cFile, @cExt )
   aList := hb_DirScan( cPath, "*.*", "A" )
//   aList := ASORT( aList,,, {|e,f| LOWER( e[1] ) < LOWER( f[1] ) } )

   FOR EACH aInfo IN aList

      udfAddChild( oTreeWdg, oTreeWdg:invisibleRootItem(), aInfo )

   NEXT


RETURN

STATIC PROCEDURE udfAddChild( oTreeWidget, oItem, aInfo, nToken )

   LOCAL aTokens
   LOCAL nParts
   LOCAL nChild

   hb_Default( @nToken, 1 )

   nParts := LEN( aTokens := hb_Atokens( aInfo[ 1 ], hb_ps() ) )

   IF nToken == nParts

      IF udfExists( oItem, aTokens[ nToken ] ) == -1
         WITH OBJECT oTreeWidget:hqlNewChild( oItem )
            :setText( 0, aTokens[ nToken ] )
            :setText( 1, hb_NtoC( aInfo[ 2 ] ) )
         END WITH
      ENDIF

   ELSE

      IF ( nChild := udfExists( oItem, aTokens[ nToken ] ) ) == -1
         WITH OBJECT oItem := oTreeWidget:hqlNewChild( oItem )
            :setText( 0, aTokens[ nToken ] )
            :setIcon( 0, QIcon( ":/hqlres/folder" ) )
         END WITH
      ELSE
         oItem := oItem:child( nChild )
      ENDIF

      udfAddChild( oTreeWidget, oItem, aInfo, nToken+1 )

   ENDIF

RETURN

STATIC FUNCTION udfExists( oItem, cText )

   LOCAL nChild

   FOR nChild := 0 TO oItem:childCount()-1
      IF oItem:child( nChild ):text( 0 ) == cText
         RETURN nChild
      ENDIF
   NEXT

RETURN -1
