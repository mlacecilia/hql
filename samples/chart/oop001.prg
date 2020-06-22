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

   hqlSetStyle( "Fusion" )

   /* hqlQApplication:setWindowIcon( QIcon( ":/hqlres/HQL96" ) ) */
   /* hqlQApplication:setOrganizationName( "L3W" ) */
   /* hqlQApplication:setOrganizationDomain( "l3w.it" ) */
   /* hqlQApplication:setApplicationName( "hqltest" ) */

   oFont := hqlQApplication:font()
   oFont:setPointSize(12)
   hqlQApplication:setFont(oFont)

   hqlOnAboutToQuit( { || UDFOnAboutToQuit() } )

   UDFshowMainWindow()

RETURN

STATIC PROCEDURE UDFOnAboutToQuit()
   hql_Trace( PADR("Quitting QApplication", 25) + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFshowMainWindow()
   LOCAL oWnd, oSize
   LOCAL oMlayout

   WITH OBJECT oWnd := hqlMainWindow(/*name*/)
      :hqlCaption( "HQLCHART tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget(/*name*/) )

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
            :hqlCaption( "&Draw" )
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Lines" )
               :setShortcut( QKeySequence( "Alt+L" ) )
               :hqlOnTriggered( { || UDFdrawerLines( oWnd:mychart() ) } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Points" )
               :setShortcut( QKeySequence( "Alt+P" ) )
               :hqlOnTriggered( { || UDFdrawerPoints( oWnd:mychart() ) } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "p&Ies" )
               :setShortcut( QKeySequence( "Alt+I" ) )
               :hqlOnTriggered( { || UDFdrawerPies( oWnd:mychart() ) } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Bars (vert.)" )
               :setShortcut( QKeySequence( "Alt+B" ) )
               :hqlOnTriggered( { || UDFdrawerBars( oWnd:mychart() ) } )
            END WITH
            :addSeparator()
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Save png" )
               :setShortcut( QKeySequence( "Alt+S" ) )
               :hqlOnTriggered( { || oWnd:mychart():hqlSavePng(/*file*/) } )
            END WITH
         END WITH

      END WITH
   END WITH

   WITH OBJECT oWnd:centralWidget()
      oMlayout := hqlVBoxLayout()
      oMlayout:setContentsMargins( 0, 0, 0, 0 )
      :setLayout( oMlayout )

      WITH OBJECT hqlChartWidget("mychart")
         :hqlAddMeToLayout( oMlayout )
      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()  // => oWnd:show(), oWnd:raise(),  oWnd:activateWindow()

RETURN
