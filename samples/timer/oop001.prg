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

   oFont := hqlQapplication:font()
   oFont:setPointSize( 14 )
   hqlQapplication:setFont( oFont )

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

   WITH OBJECT oWnd := hqlMainWindow( /*name*/ )
      :hqlCaption( "HQLTIMER tester" ) // ==>:setWindowTitle( "tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget( /*name*/ ) )
      :centralWidget():setLayout( hqlVBoxLayout() )
      :hqlOnActivate( { || oWnd:mytimer:start() } )

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

      WITH OBJECT :centralWidget()
         oMlayout := :layout()

         WITH OBJECT hqlLabel( "alabel" )
            :hqlAddMeToLayout( oMlayout )

            WITH OBJECT hqlTimer( /*name*/ )
               :setInterval( 1000 )
               :hqlOnTimeout( { || udfUpdateLabel_A() } )
               :start()
            END WITH

         END WITH

         WITH OBJECT hqlLabel( "blabel" )
            :hqlAddMeToLayout( oMlayout )

            WITH OBJECT hqlTimer( "mytimer" )
               :setInterval( 2000 )
               :hqlOnTimeout( { || udfUpdateLabel_B() } )
            END WITH

         END WITH

         oMlayout:addStretch()   // pushUp

      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   hql_exploreChildren( oWnd, .T. )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE udfUpdateLabel_A()
   LOCAL oDateTime := hqlDateTime():hqlCurrentDateTime()
   HqlActiveWindow:alabel:setText( oDateTime:toLocaleString() )
RETURN

STATIC PROCEDURE udfUpdateLabel_B()
   HqlActiveWindow:blabel:setText( "random int " + hb_NtoC( hb_Rand32() ) )
RETURN
