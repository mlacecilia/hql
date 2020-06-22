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
      :setWindowTitle( "HQLTREEWIDGET tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget(/*name*/) )
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
               :setText( "&Get current row" )
               :hqlOnTriggered( { || hql_Trace( "row =" + hb_NtoC( oWnd:tree1:hqlValue() ) ) } )
            END WITH
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&Set current row" )
               :hqlOnTriggered( { || oWnd:tree1:hqlValue( 2, /*ncolumn*/ ) } )
            END WITH
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "set &Row and column" )
               :hqlOnTriggered( { || oWnd:tree1:hqlValue( 3, 2 ) } )
            END WITH
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&Explore struc." )
               :hqlOnTriggered( { || UDFExplore( oWnd:tree1() ) } )
            END WITH
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&Clear" )
               :hqlOnTriggered( { || oWnd:tree1:clear() } )
            END WITH
         END WITH

         WITH OBJECT :hqlAddMenu( /*name*/ )
            :setTitle( "&TopLevel" )
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&Append" )
               :hqlOnTriggered( { || UDFAddTopLevel( oWnd:tree1() ) } )
            END WITH
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&Insert" )
               :hqlOnTriggered( { || UDFInsertTopLevel( oWnd:tree1() ) } )
            END WITH
         END WITH

         WITH OBJECT :hqlAddMenu( /*name*/ )
            :setTitle( "&Childs" )
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&Append" )
               :hqlOnTriggered( { || UDFAddChild( oWnd:tree1() ) } )
            END WITH
         END WITH

      END WITH
   END WITH

   WITH OBJECT oWnd:centralWidget()
      oMlayout := :layout()

      WITH OBJECT hqlTreeWidget("tree1")
         :hqlAddMeToLayout( oMlayout )
         :header:setSectionsClickable( .T. )
         :setColumnCount( 2 )
         :setColumnWidth( 0, 200 )
         :setColumnWidth( 1, 100 )
         WITH OBJECT :headerItem()
            :setText( 0, "Column -A-" )
            :setIcon( 0, QIcon( ":ballb" ) )
            :setText( 1, "Column -B-" )
            :setIcon( 1, QIcon( ":ballr" ) )
         END WITH
      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE UDFExplore( oTreeWdg )
   UDFExploreRicursive( oTreeWdg:invisibleRootItem() )
RETURN

STATIC PROCEDURE UDFExploreRicursive( oItem, nLevel, nTab )
   LOCAL nChild

   nLevel := hb_DefaultValue( nLevel, 0 )
   nTab := hb_DefaultValue( nTab, 0 )

   hql_Trace( SPACE(nTab) + "level #" + hb_NtoC( nLevel ) + " text =" + oItem:text(0) )

   FOR nChild := 0 TO oItem:childCount()-1
      UDFExploreRicursive( oItem:child( nChild ), nLevel+1, nTab+3 )
   NEXT nChild

RETURN

STATIC PROCEDURE UDFAddTopLevel( oTreeWdg )
   LOCAL oItem
   LOCAL nTopLevel := oTreeWdg:topLevelItemCount()

   WITH OBJECT oItem := QTreeWidgetItem()
      :setText( 0 , "topLevel #" + hb_NtoS(nTopLevel) )
      :setText( 1 , "other text" )
      :setIcon( 1, QIcon( ":bally" ) )
   END WITH

   oTreeWdg:addTopLevelItem( oItem )

RETURN

STATIC PROCEDURE UDFInsertTopLevel( oTreeWdg )
   LOCAL oItem
   LOCAL nTopLevel := oTreeWdg:topLevelItemCount()

   IF 2 > nTopLevel
      UDFAddTopLevel( oTreeWdg )
   ELSE
      WITH OBJECT oItem := QTreeWidgetItem()
         :setText( 0 , "topLevel #" + hb_NtoS(nTopLevel) )
         :setText( 1 , "other text" )
         :setIcon( 1, QIcon( ":bally" ) )
      END WITH
      oTreeWdg:insertTopLevelItem( 2, oItem )
   ENDIF

RETURN

STATIC PROCEDURE udfAddChild( oTreeWdg )

   IF hb_IsObject( oTreeWdg:currentItem() )

      WITH OBJECT oTreeWdg:hqlNewChild( oTreeWdg:currentItem() )
         :setText( 0 , "this_is_a_child" )
      END WITH

   ELSE

      WITH OBJECT oTreeWdg:hqlNewChild( /*invisiblerootitem_used*/ )
         :setText( 0 , "this_is_a_child" )
      END WITH


   ENDIF

RETURN
