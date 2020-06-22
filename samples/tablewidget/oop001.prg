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
      :setWindowTitle( "HQLTABLEWIDGET tester" )
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


         WITH OBJECT :hqlAddMenu(/*name*/)
            :hqlCaption( "&Row" )
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Count" )
               :hqlOnTriggered( { || hql_MsgStop( "Current is: " + hb_NtoS(oWnd:mytable:rowCount()) ) } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "c&Urrent" )
               :hqlOnTriggered( { || hql_MsgStop( "Current is: " + hb_NtoS(oWnd:mytable:hqlCurrentRow()) ) } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Set current" )
               :hqlOnTriggered( { || oWnd:mytable:hqlCurrentRow(1) } )
            END WITH
            :addSeparator()
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "add &Empty" )
               :hqlOnTriggered( { || UDFsetEmptyRow( oWnd:mytable() ) } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&1 Append" )
               :hqlOnTriggered( { || UDFappendRow_1( oWnd:mytable() ) } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&2 Append" )
               :hqlOnTriggered( { || UDFappendRow_2( oWnd:mytable() ) } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&3 Append" )
               :hqlOnTriggered( { || UDFappendRow_3( oWnd:mytable() ) } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Insert" )
               :hqlOnTriggered( { || UDFinsertRow( oWnd:mytable() ) } )
            END WITH
            :addSeparator()
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Get 1" )
               :hqlOnTriggered( { || UDFgetRow_1( oWnd:mytable() ) } )
            END WITH
         END WITH

         WITH OBJECT :hqlAddMenu(/*name*/)
            :hqlCaption( "&Column" )
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Count" )
               :hqlOnTriggered( { || hql_MsgStop( "Current is: " + hb_NtoS(oWnd:mytable:columnCount()) ) } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "c&Urrent" )
               :hqlOnTriggered( { || hql_MsgStop( "Current is: " + hb_NtoS(oWnd:mytable:hqlCurrentColumn()) ) } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Set current" )
               :hqlOnTriggered( { || oWnd:mytable:hqlCurrentColumn(0) } )
            END WITH
         END WITH

         WITH OBJECT :hqlAddMenu(/*name*/)
            :hqlCaption( "&Tools" )
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Clear" )
               :hqlOnTriggered( { || oWnd:mytable:hqlClear() } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Select cell" )
               :hqlOnTriggered( { || oWnd:mytable:hqlSelectCell( 0, 2 ) } )  // row,column
            END WITH
         END WITH

      END WITH
   END WITH

   WITH OBJECT oWnd:centralWidget()
      oMlayout := :layout()

      WITH OBJECT hqlTableWidget("mytable")
         :hqlAddMeToLayout( oMlayout )
         /* :hqlAlignMeToLayout( oMlayout, Qt_AlignCenter ) */
         :hqlSortable( .T. )
         // :hqlOnSectionClicked(...) the same as :hqlOnHsectionClicked(
         :hqlOnHsectionClicked( { |nint,oself| UDFonHsectionClicked(nint,oself) } )
         :hqlOnVsectionClicked( { |nint,oself| UDFonVsectionClicked(nint,oself) } )
         :hqlOnCellActivated( { |nrow,ncol,oself| UDFonCellActivated(nrow,ncol,oself) } )
         :hqlOnCellClicked( { |nrow,ncol,oself| UDFonCellClicked(nrow,ncol,oself) } )
         :hqlOnCellDoubleClicked( { |nrow,ncol,oself| UDFonCellDoubleClicked(nrow,ncol,oself) } )

         WITH OBJECT :hqlAddColumn()
            WITH OBJECT :header()
               :setText( "ID" )     //hqlTran("thisapp", "ID") )
               :setIcon( QIcon( ":/hqlres/dummy" ) )
               :setBackground( QBrush( QColor( 128, 255, 128 ) ) )
               :setForeground( QBrush( QColor( 128, 0, 0 ) ) )
            END WITH
            WITH OBJECT :cell()
               :setTextAlignment( Qt_AlignCenter )
            END WITH
         END WITH

         WITH OBJECT :hqlAddColumn()
            :setWidth( 0 ) // hidden column
            WITH OBJECT :header()
               :setText( "hidden" )     //hqlTran("thisapp", "ID") )
            END WITH
            WITH OBJECT :cell()
               :setTextAlignment( Qt_AlignCenter )
            END WITH
         END WITH

         WITH OBJECT :hqlAddColumn( 200 )
            //:setWidth( 200 )
            WITH OBJECT :header()
               :setText( "Name" )     //hqlTran("thisapp", "ID") )
            END WITH
            WITH OBJECT :cell()
               :setTextAlignment( hb_BitOr(Qt_AlignVCenter, Qt_AlignLeft) )
               :setBackground( QBrush( QColor( 128, 255, 128 ) ) )
               :setForeground( QBrush( QColor( 128, 0, 0 ) ) )
            END WITH
         END WITH

      END WITH /*hqlTableWidget*/

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE UDFsetEmptyRow( otable )
   LOCAL oRow := oTable:hqlEmptyRow()
   oTable:hqlInsertRow( 0, oRow )
RETURN

STATIC PROCEDURE UDFappendRow_1( otable )
   LOCAL oRow := oTable:hqlEmptyRow()
   oRow[1]:setText( "uno" )
   oRow[2]:setText( "hydden_uno" )
   oRow[3]:setText( "uno_name" )
   oTable:hqlInsertRow( otable:rowCount(), oRow )
RETURN

STATIC PROCEDURE UDFappendRow_2( otable )
   LOCAL oRow := oTable:hqlEmptyRow()
   oRow[1]:setText( "due" )
   oRow[2]:setText( "hydden_due" )
   oRow[3]:setText( "due_name" )
   oTable:hqlAddRow( oRow )
RETURN

STATIC PROCEDURE UDFappendRow_3( otable )
   LOCAL oRow
   oRow := oTable:hqlAddRow()
   oRow[1]:setText( "tre" )
   oRow[2]:setText( "hydden_tre" )
   oRow[3]:setText( "tre_name" )
RETURN

STATIC PROCEDURE UDFinsertRow( otable )
   LOCAL oRow := oTable:hqlEmptyRow()
   oRow[1]:setText( "zero" )
   oRow[2]:setText( "hydden_zero" )
   oRow[3]:setText( "zero_name" )
   oTable:hqlInsertRow( 0, oRow )
RETURN

STATIC PROCEDURE UDFgetRow_1( otable )
   LOCAL oRow := oTable:hqlGetRow( 1 )
   LOCAL nC
   hql_Trace( "size: " + hb_NtoS(LEN(oRow)) )
   FOR nC := 1 TO otable:columnCount()
      hql_Trace( "col: " + hb_NtoS(nC-1) + " text: " + oRow[nC]:text() )
   NEXT
RETURN

STATIC PROCEDURE UDFonHsectionClicked(nint,oSelf)
   hql_Trace( PADR("onHsectionClicked: ",25) + "nint: " + hb_NtoS(nint) + " " + oself:className() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFonVsectionClicked(nint,oSelf)
   hql_Trace( PADR("onVsectionClicked: ",25) + "nint: " + hb_NtoS(nint) + " " + oself:className() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFonCellActivated(nrow,ncol,oself)
   hql_Trace( PADR("onCellActivated: ",25) + "nrow: " + hb_NtoS(nrow) + " ncol: " + hb_NtoS(ncol) + " " + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFonCellClicked(nrow,ncol,oself)
   hql_Trace( PADR("onCellClicked: ",25) + "nrow: " + hb_NtoS(nrow) + " ncol: " + hb_NtoS(ncol) + " " + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFonCellDoubleClicked(nrow,ncol,oself)
   hql_Trace( PADR("onCellDoubleClicked: ",25) + "nrow: " + hb_NtoS(nrow) + " ncol: " + hb_NtoS(ncol) + " " + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN
