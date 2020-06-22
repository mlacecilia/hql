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
      :setWindowTitle( "HQLLISTWIDGET tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget(/*name*/, this) )
      :centralWidget():setLayout( hqlVBoxLayout() )
      :hqlOnActivate( { || UDFreload( oWnd ) } )

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
            :hqlCaption( "&List" )
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Current row" )
               :hqlOnTriggered( { || hql_MsgStop( "value"+hb_NtoS(oWnd:listwdg1:hqlValue()), "combo current row", /*detail*/, /*info*/ ) } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Set 9" )
               :hqlOnTriggered( { || oWnd:listwdg1:hqlValue(9) } )
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Current item" )
               :hqlOnTriggered( { || UDFtraceItem(oWnd:listwdg1()) } )
            END WITH
         END WITH
      END WITH
   END WITH

   WITH OBJECT oWnd:centralWidget()
      oMlayout := :layout()
      WITH OBJECT hqlListWidget("listwdg1")
         :hqlAddMeToLayout( oMlayout )
         :hqlAlignMeToLayout( oMlayout, Qt_AlignCenter )
         :hqlOnItemActivated( { |item,oself| UDFonItemActivated(item,oSelf) } )
         :hqlOnItemClicked( { |item,oself| UDFonItemClicked(item,oSelf) } )
         :hqlOnCurrentRowChanged( { |nint,oself| UDFonCurrentRowChanged(nint, oSelf) } )
      END WITH
   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE UDFreload( oWnd )
   LOCAL aRows := {}
   LOCAL nData := 100

   oWnd:listwdg1:clear()

   AADD( aRows, { "Austria",        ++nData, ":/hqlres/cf_at" } )
   AADD( aRows, { "Belgium",        ++nData, ":/hqlres/cf_be" } )
   AADD( aRows, { "Switzerland",    ++nData, ":/hqlres/cf_ch" } )
   AADD( aRows, { "Czech republic", ++nData, ":/hqlres/cf_cz" } )
   AADD( aRows, { "Germany",        ++nData, ":/hqlres/cf_de" } )
   AADD( aRows, { "Denmark",        ++nData, ":/hqlres/cf_dk" } )
   AADD( aRows, { "Spain",          ++nData, ":/hqlres/cf_es" } )
   AADD( aRows, { "France",         ++nData, ":/hqlres/cf_fr" } )
   AADD( aRows, { "Great Britain",  ++nData, ":/hqlres/cf_gb" } )
   AADD( aRows, { "Greece",         ++nData, ":/hqlres/cf_gr" } )
   AADD( aRows, { "Ireland",        ++nData, ":/hqlres/cf_ie" } )
   AADD( aRows, { "Iceland",        ++nData, ":/hqlres/cf_is" } )
   AADD( aRows, { "Italy",          ++nData, ":/hqlres/cf_it" } )

   oWnd:listwdg1:hqlAddRows( aRows )

   // OR
   //oWnd:listwdg1:hqlAddRow( text, data, icon )
   // OR
   //oWnd:listwdg1:hqlAddRow( {text, data, icon} )

RETURN

STATIC PROCEDURE UDFonItemClicked(item,oSelf)
   hql_Trace( PADR("onItemClicked: ",25) + ;
                      "text: " + item:text() + " " + ;
                      "hasIcon: " + hb_ValToExp( !item:icon:isNull() ) + " " + ;
                      "data: " + hb_NtoS( item:data(Qt_UserRole):toInt() ) + " " + ;
                      oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFonItemActivated(item, oself)
   hql_Trace( PADR("onItemActivated: ",25) + ;
                      "text: " + item:text() + " " + ;
                      "hasIcon: " + hb_ValToExp( !item:icon:isNull() ) + " " + ;
                      "data: " + hb_NtoS( item:data(Qt_UserRole):toInt() ) + " " + ;
                      oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFonCurrentRowChanged(nInt, oself)
   hql_Trace( PADR("onCurrentRowChanged: ",25) + "nInt: " + hb_NtoS(nInt) + " " + oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFtraceItem(oList)
   hql_Trace( PADR("currentData: ",25) + ;
                      "text: " + oList:hqlCurrentData(Qt_DisplayRole):toString() + " " , ;
                      "data: " + hb_NtoS( oList:hqlCurrentData():toInt() ) )
RETURN
