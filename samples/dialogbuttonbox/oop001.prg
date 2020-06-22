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
      :setWindowTitle( "HQLDIALOGBOX tester" )
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
   END WITH

   WITH OBJECT oWnd:centralWidget()
      oMlayout := :layout()
      WITH OBJECT hqlDialogButtonBox(/*name*/)
         :hqlAddMeToLayout( oMlayout )
         :hqlAlignMeToLayout( oMlayout, Qt_AlignCenter )
         :hqlOnAccepted( { || UDFaccepted() } )
         :hqlOnRejected( { || UDFrejected(oWnd) } )
         :hqlOnHelpRequested( { || UDFhelpRequested() } )

         :addButton( QDialogButtonBox_Ok )
         WITH OBJECT :button( QDialogButtonBox_Ok )   // QDialogButtonBox::ButtonRole and QDialogButtonBox::StandardButton
            :setText( "Login" )
         END WITH

         :addButton( QDialogButtonBox_Cancel )
         WITH OBJECT :button( QDialogButtonBox_Cancel )
            :setText( "Abort" )
         END WITH

         :addButton( QDialogButtonBox_Help )
         WITH OBJECT :button( QDialogButtonBox_Help )
            :setText( "Help" )
         END WITH

         WITH OBJECT :hqlNewPushButton( /*name*/, /*nRole*/ )  // nRole by default is QDialogButtonBox_ActionRole
            :setText( "Action_1" )
            :hqlOnClicked( { || hql_Trace( "Action_1" ) } )
         END WITH

         WITH OBJECT :hqlNewPushButton( /*name*/, QDialogButtonBox_ResetRole )
            :setText( "Reset_1" )
            :hqlOnClicked( { || hql_Trace( "Reset_1" ) } )
         END WITH
      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE UDFaccepted()
   hql_Trace( "accepted" )
RETURN
STATIC PROCEDURE UDFrejected( oDlg )
   hql_Trace( "Rejected" )
   oDlg:close()
RETURN
STATIC PROCEDURE UDFhelpRequested()
   hql_Trace( "helpRequested" )
RETURN
