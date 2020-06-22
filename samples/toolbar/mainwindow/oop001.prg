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

   WITH OBJECT oWnd := hqlMainWindow(/*name*/)
      :setWindowTitle( "HQLTOOLBAR tester" )
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
      END WITH

      WITH OBJECT hqlToolBar( "mytbar" )
         :setIconSize( QSize( 32, 32 ) )
         :setContextMenuPolicy( Qt_PreventContextMenu ) // to prevent rightClick to close
         :setAllowedAreas( hb_BitOr( Qt_RightToolBarArea, Qt_BottomToolBarArea ) ) // by default :setAllowedAreas( Qt_AllToolBarAreas )
         :hqlPlace( Qt_BottomToolBarArea )   // required to place toolBar in the right place where you want. By default always on top!
         :hqlOnActionTriggered( { |oAction, oSelf| UDFOnActionTriggered( oAction, oSelf ) } )

         WITH OBJECT :hqlAddToolButton( /*name OR toolButton*/ )
            :hqlOnClicked( { |oSelf| UDFClicked( oSelf ) } )
            :setIcon( QIcon( ":/hqlres/txtbold" ) )
         END WITH

         :addSeparator()

         WITH OBJECT :hqlAddToolButton( /*name OR toolButton*/ )
            :hqlOnClicked( { |oSelf| UDFClicked( oSelf ) } )
            :setIcon( QIcon( ":/hqlres/txtlalign" ) )
         END WITH

         :addSeparator()

         WITH OBJECT :hqlAddWidget( hqlListWidget( /*name*/ ) )
            :setMaximumSize( QSize(150, 32) )
            :hqlAddRow( "row 1" )
            :hqlAddRow( "row 2" )
            :hqlAddRow( "row 3" )
            :hqlAddRow( "row 4" )
            :hqlOnItemActivated( { |oItem,oself| UDFonActivated(oItem,oSelf) } )
         END WITH

      END WITH //hqlToolBar

      WITH OBJECT hqlToolBar(/*name*/ )
         :setIconSize( QSize( 32, 32 ) )
         :setContextMenuPolicy( Qt_PreventContextMenu ) // to prevent rightClick to close
         :setAllowedAreas( hb_BitOr( Qt_LeftToolBarArea, Qt_BottomToolBarArea ) ) // by default :setAllowedAreas( Qt_AllToolBarAreas )
         :hqlPlace( Qt_LeftToolBarArea )   // required to place toolBar in the right place where you want. By default always on top!
         WITH OBJECT :hqlAddButtonMenu( /*cButtonName*/, /*cMenuName*/ )
            :setText( "&File" )
            WITH OBJECT :menu()
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "&Quit" )
                  :setIcon( QIcon( ":/hqlres/quit" ) )
                  :setShortcut( QKeySequence( "Alt+Q" ) )
                  :hqlOnTriggered( { || oWnd:hqlRelease() } )
               END WITH
            END WITH
         END WITH
      END WITH //hqlToolBar

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE UDFclicked( oSelf )

   hql_Trace( PADR("clicked: ",25) + ;
                      oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFonActivated(oItem,oSelf)

   hql_Trace( PADR("itemActivated: ",25) + ;
                      oitem:text() + " " + ;
                      oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFOnActionTriggered( oAction, oself)
   hql_Trace( PADR("onClicked: ",25) + "action: " + oAction:text + " " + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN
