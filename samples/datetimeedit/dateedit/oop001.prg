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
   LOCAL oWnd, this, oSize
   LOCAL oMlayout

   WITH OBJECT oWnd := hqlMainWindow(/*name*/)
      this := :hqlThis()
      :setWindowTitle( "HQLDATEEDIT tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget(/*name*/, this) )
      :centralWidget():setLayout( HqlVBoxLayout() )

      WITH OBJECT hqlMenuBar(/*name*/)
         WITH OBJECT :hqlAddMenu(/*name*/)
            :hqlCaption( "&File" )
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "e&Xit" )
               :hqlOnTriggered( { || oWnd:hqlRelease() } )
               :setIcon( QIcon( ":/hqlres/exit" ) )
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
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "set &Today" )
               :hqlOnTriggered( { ||oWnd:thedate:hqlSetCurrent() } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "set &Hb_date" )
               :hqlOnTriggered( { || oWnd:thedate:hqlValue(hb_Date(1964,1,31)) } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "set &Qdate" )
               :hqlOnTriggered( { || oWnd:thedate:hqlValue(QDate(1964,1,31)) } )
            END WITH
         END WITH
      END WITH
   END WITH

   WITH OBJECT oWnd:centralWidget()
      oMlayout := :layout()

      WITH OBJECT hqlDateEdit("thedate")
         :hqlAddMeToLayout( oMlayout )
         :hqlAlignMeToLayout( oMlayout, Qt_AlignCenter )
         :hqlOnEditingFinished( { |oself| UDFonEditingFinished(oself) } )
         :hqlOnDateChanged( { |odate| UDFonDateChanged(odate) } )
      END WITH

      WITH OBJECT hqlLineEdit(/*name*/)
         :hqlAddMeToLayout( oMlayout )
         :hqlAlignMeToLayout( oMlayout, Qt_AlignCenter )
         :setPlaceholderText( "only to gain focus" )
      END WITH

      oMlayout:addStretch()   //pushUp

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE UDFonEditingFinished(oself)
   hql_Trace( PADR("onEditingFinished: ",25) + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFonDateChanged(odate)
   hql_Trace( PADR("onDateChanged: ",25) + hb_NtoS(odate:year())+"_"+hb_NtoS(odate:month())+"_"+hb_NtoS(odate:day()) + " " + hb_TtoS(hb_DateTime()) )
RETURN
