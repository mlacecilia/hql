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
   LOCAL oFont

   hqlErrorSys()  /*hbqt_errorsys()*/

   hqlAutoNameEnabled( .T. )

   hqlSetStyle( "Fusion" )

   hqlOnAboutToQuit( { || UDFOnAboutToQuit() } )

   hqlStart()

   oFont := hqlQapplication:font()
   oFont:setPointSize( 14 )
   hqlQapplication:setFont( oFont )

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

   WITH OBJECT oWnd := hqlMainWindow(/*name*/)
      this := :hqlThis()
      :setWindowTitle( "HQLATMODEL, HQLTABLEVIEW tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget(/*name*/, this) )
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

         WITH OBJECT :hqlAddMenu(/*name*/)
            :setTitle( "&Tools" )
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "add &Column" )
               :hqlOnTriggered( { || UDFaddColumn( oWnd:tview() ) } )
            END WITH
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "add &Row" )
               :hqlOnTriggered( { || UDFaddRow( oWnd:tview() ) } )
            END WITH
            :addSeparator()
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "c&lear" )
               :hqlOnTriggered( { || oWnd:tview:model:clear() } )
            END WITH
            :addSeparator()
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "get info" )
               :hqlOnTriggered( { || UDshowInfo( oWnd:tview() ) } )
            END WITH
         END WITH

      END WITH
   END WITH

   WITH OBJECT oWnd:centralWidget()
      oMlayout := :layout()

      WITH OBJECT myBrowse("tview")
         :hqlAddMeToLayout( oMlayout )
      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   hql_exploreChildren( oWnd, .T. )

   oWnd:hqlActivate()

RETURN


STATIC PROCEDURE UDFaddRow( oTview )

   LOCAL oIndex
   LOCAL nRow
   LOCAL nCol
   LOCAL cAutomticText

   oTview:model:addRow()   // create required space for a new row
   nRow := oTview:model:rowCount()-1
   FOR nCol := 0 TO oTview:model:columnCount() - 1
      oIndex := oTview:model:index( nRow, nCol )
      IF oIndex:isValid()
         cAutomticText := "cell_" + hb_NtoS( nRow+1 ) + "_" + hb_NtoS( nCol+1 )
         oTview:model:setData( oIndex, QVariant( cAutomticText ), Qt_DisplayRole )
      ENDIF
   NEXT

RETURN

STATIC PROCEDURE UDFaddColumn( oTview )

   LOCAL cAutomticText
   LOCAL oBrush

   oTview:model:addColumn()
   cAutomticText := "col_" + hb_NtoS( oTview:model:columnCount() )
   oTview:model:setHeaderData( oTview:model:columnCount()-1, Qt_Horizontal, QVariant( cAutomticText ), Qt_DisplayRole )
   oTview:model:setHeaderData( oTview:model:columnCount()-1, Qt_Horizontal, QVariant( Qt_AlignCenter ), Qt_TextAlignmentRole )

   oBrush := QBrush( QColor( Qt_cyan ) )
   oTview:model:setHeaderData( oTview:model:columnCount()-1, Qt_Horizontal, oBrush, Qt_BackgroundRole )

RETURN


STATIC PROCEDURE UDshowInfo( oTview )

   LOCAL nRow
   LOCAL nColumn
   LOCAL oIndex

   hql_Trace( " horizontalHeader sections =" + hb_NtoS( oTview:horizontalHeader:count() ) + ;
                      " verticalHeader sections =" + hb_NtoS( oTview:verticalHeader:count() ) + ;
                      " model rowCount =" + hb_NtoS( oTview:model:rowCount() ) + ;
                      " model columnCount =" + hb_NtoS( oTview:model:columnCount() ) )



   hql_Trace( "headerData =" + oTview:model:headerData( Qt_DisplayRole, Qt_Horizontal, 0 ) )

   FOR nRow := 0 TO oTview:model:rowCount() - 1

      FOR nColumn := 0 TO oTview:model:columnCount() - 1

         oIndex := oTview:model:index( nRow, nColumn, QModelIndex() )

         hql_Trace( " row=" + hb_NtoS( nRow+1 ) + ;
                            " col=" + hb_NtoS( nColumn+1 ) + ;
                            " text=" + oTview:model:data( oIndex, Qt_DisplayRole ) )
      NEXT nColumn

   NEXT nRow

RETURN
