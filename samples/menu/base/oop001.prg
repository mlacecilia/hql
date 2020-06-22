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
   LOCAL oWidget

   WITH OBJECT oWnd := hqlMainWindow(/*name*/)
      :setWindowTitle( "HQLMENUBAR, HQLMENU, HQLACTION tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget() )
      :centralWidget():setLayout( hqlVBoxLayout() )

      WITH OBJECT hqlMenuBar( "mmenu" )

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
            :hqlCaption( "&Dialog" ) //==>:setTitle( "&File" )
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Test" )
               :hqlOnTriggered( { || UDFdialog(oWnd) } )
            END WITH
         END WITH

         WITH OBJECT :hqlAddMenu( /*name*/ )
            :setTitle( "&Misc" )
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&Checkable" )
               :setShortcut( QKeySequence( "Alt+L" ) )
               :setCheckAble( .T. )
               :hqlOnTriggered( { |boolean| UDFonOff(boolean) } )
            END WITH

            :addSeparator()

            WITH OBJECT :hqlAddAction( "id01" )
               :setText( "&Available" )
               :setEnabled( .F. )
               :hqlOnTriggered( { || hql_MsgStop( "enabled" ) } )
            END WITH

         END WITH

         WITH OBJECT :hqlAddMenu( /*name*/ )
            :setTitle( "&Widgets" )
            WITH OBJECT oWidget := hqlListWidget( /*name*/ )
               :hqlAddRow( "row 1" )
               :hqlAddRow( "row 2" )
               :hqlAddRow( "row 3" )
               :hqlAddRow( "row 4" )
               :hqlOnItemClicked( { |oitem| UDFitem( oItem ) } )
            END WITH
            :hqlAddWidget( oWidget )
         END WITH

         WITH OBJECT :hqlAddMenu( /*name*/ )
            :setTitle( "&Submenu" )
            WITH OBJECT :hqlAddMenu( /*name*/ )
               :setTitle( "#1 menu" )
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :setText( "#2 visible or not" )
                  :hqlOnTriggered( { || IIF( oWnd:mmenu:id02:hqlIsVisible(), oWnd:mmenu:id02:hqlSetVisible( .F. ), oWnd:mmenu:id02:hqlSetVisible( .T. ) ) } )
               END WITH
            END WITH
            WITH OBJECT :hqlAddMenu( "id02" )
               :setTitle( "#2 menu" )
               //:hqlSetVisible( boolean )
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

STATIC PROCEDURE UDFonOff( lBoolean )
   hql_Trace( "boolean=" + hb_ValToExp( lBoolean ) )
   UDFavailable( lBoolean )
RETURN

STATIC PROCEDURE UDFavailable( lBoolean )
   hqlActiveWindow:mmenu:id01:setEnabled( lBoolean )
RETURN

STATIC PROCEDURE UDFitem( oItem )
   hql_Trace( "item.text =" + oItem:text() )
RETURN

STATIC PROCEDURE UDFdialog(oParent)
   LOCAL oWnd
   LOCAL oVlayout

   WITH OBJECT oWnd := hqlChildDialog( /*name*/, oParent )
      :hqlCaption( "HQLMENUBAR, HQLMENU, HQLACTION tester" )
      :resize( QSize(400, 300) )
   END WITH

   oVlayout := hqlVBoxLayout(/*name*/)
   oWnd:setLayout( oVlayout )

   WITH OBJECT oWnd

      WITH OBJECT hqlMenuBar("mainmenu")
         :hqlAddMeToLayout( oVlayout )
         WITH OBJECT :hqlAddMenu(/*name*/)
            :hqlCaption( "&File" )
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "e&Xit" )
               :hqlOnTriggered( { || oWnd:hqlRelease() } )
               :setIcon( QIcon( ":/hqlres/exit" ) )
            END WITH
            :addSeparator()
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Dialog" )
               :hqlOnTriggered( { || UDFdialog(oWnd) } )
            END WITH
         END WITH
      END WITH

      WITH OBJECT hqlPushButton(/*name*/)
         :hqlCaption( "close all windows" )
         :hqlAddMeToLayout( oVlayout )
         :hqlOnClicked( { || hqlQApplication:closeAllWindows() } )
      END WITH

   END WITH

   oWnd:hqlActivate()  // => oWnd:show(), oWnd:raise(),  oWnd:activateWindow()

RETURN
