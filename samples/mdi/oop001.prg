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
      :setWindowTitle( "HQLMDIAREA, HQLMDISUBWINDOW tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlMdiArea(/*name*/) )

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
            :hqlCaption( "&Tools" )
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&addSubWindow" )
               :setIcon( QIcon( ":/hqlres/recadd" ) )
               :setShortcut( QKeySequence( "Alt+D" ) )
               :hqlOnTriggered( { || UDFAddSubWindow( oWnd:centralWidget() ) } )
            END WITH
         END WITH

      END WITH
   END WITH

   WITH OBJECT oWnd:centralWidget()
      //   this works too      :hqlOnSubWindowActivated( { |oSub,oMdiArea| udfChgSubWin(oSub,oMdiArea
      :hqlOnSubWindowActivated( { |oSub| UDFonSubWindowActivated(oSub) } )
   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE UDFonSubWindowActivated(oSub)
   hql_Trace( PADR("onSubWindowActivated: ",25) + "title: " + oSub:windowTitle() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFAddSubWindow(oMdiArea)

   LOCAL oSubWin
   LOCAL oLabel

   STATIC nSubWin := 0

   ++nSubWin

   WITH OBJECT oSubWin := hqlMdiSubWindow( /*name*/ )
      :hqlCaption("hqlMdiSubWindow tester") //==>:setWindowTitle("HQLMDISUBWINDOW tester")
      :hqlOnAboutToActivate( { |oself| UDFonAboutToActivated(oself) } )
      :hqlOnWindowStateChanged( { |nold,nnew,oself| UDFonWindowStateChanged(nold,nnew,oself) } )
      :setAttribute( Qt_WA_DeleteOnClose )
   END WITH

   oLabel := hqlLabel( /*name*/ )
   oLabel:setText( "MdiSubWin #" + hb_NtoC( nSubWin ) )

   oSubWin:setWidget( oLabel )

   oMdiArea:addSubWindow( oSubWin )

   oSubWin:show()
RETURN

STATIC PROCEDURE UDFonAboutToActivated(oself)
   hql_Trace( PADR("onAboutToActivated: ",25) + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFonWindowStateChanged(nold,nnew,oself)
   hql_Trace( PADR("onWindowStateChanged: ",25) + "nold: " + hb_NtoS(nOld) + " nnew: " + hb_NtoS(nnew) + " " + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN
