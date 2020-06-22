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
      :setWindowTitle( "HQLCOMBOBOX tester" )
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
            :hqlCaption( "&Tools" )
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Clear" )
               :hqlOnTriggered( { || oWnd:mycombo:clear() } )
            END WITH
            WITH OBJECT :hqlAddMenu(/*name*/)
               :hqlCaption( "cu&Rrent row" )
               WITH OBJECT :hqlAddAction(/*name*/)
                  :hqlCaption( "&Get" )
                  :hqlOnTriggered( { || hql_MsgStop( "value is: " + hb_NtoS(oWnd:mycombo:hqlValue()) ) } )
               END WITH
               WITH OBJECT :hqlAddAction(/*name*/)
                  :hqlCaption( "&Set (3)" )
                  :hqlOnTriggered( { || oWnd:mycombo:hqlValue(3) } )
               END WITH
            END WITH
            WITH OBJECT :hqlAddMenu(/*name*/)
               :hqlCaption( "&Add row[s]" )
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :hqlCaption( "&Single row" )
                  :hqlOnTriggered( { || UDFAddRow( oWnd:mycombo() ) } )
               END WITH
               WITH OBJECT :hqlAddAction( /*name*/ )
                  :hqlCaption( "&Many rows" )
                  :hqlOnTriggered( { || UDFAddManyRow( oWnd:mycombo() ) } )
               END WITH
            END WITH
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Extract" )
               :hqlOnTriggered( { || UDFextract( oWnd:mycombo() ) } )
            END WITH
         END WITH

      END WITH
   END WITH

   WITH OBJECT oWnd:centralWidget()
      oMlayout := :layout()
      WITH OBJECT hqlComboBox( "mycombo" )
         :hqlAddMeToLayout( oMlayout )
         :hqlAlignMeToLayout( oMlayout, Qt_AlignCenter )
         :setMinimumWidth( 150 ) /* to give a preferred width */
         :hqlOnCurrentIndexChanged( { |nint, oself| UDFindexChanged(nint,oself) } )
         :hqlOnActivated( { |nint, oself| UDFactivated(nint,oself) } )  // on Windows works when you press enter on highlighted item
      END WITH
   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE UDFAddRow( oObject )
   STATIC nData := 100

   /* :hqlAddRow( text, data, icon )
      :hqlAddRow( { text, data, icon } ) */
   oObject:hqlAddRow( { ":/hqlres/cf_it", ++nData, ":/hqlres/cf_it" } )

RETURN

STATIC PROCEDURE UDFAddManyRow( oObject )
   LOCAL aRows := {}
   STATIC nData := 200

   /* :hqlAddRows( { {text, data, icon}, ... } ) */
   AADD( aRows, { ":/hqlres/cf_olimpic", ++nData, ":/hqlres/cf_olimpic" } )
   AADD( aRows, { ":/hqlres/cf_at", ++nData, ":/hqlres/cf_at" } )
   AADD( aRows, { ":/hqlres/cf_be", ++nData, ":/hqlres/cf_be" } )
   AADD( aRows, { ":/hqlres/cf_ch", ++nData, ":/hqlres/cf_ch" } )
   AADD( aRows, { ":/hqlres/cf_cz", ++nData, ":/hqlres/cf_cz" } )
   AADD( aRows, { ":/hqlres/cf_de", ++nData, ":/hqlres/cf_de" } )
   AADD( aRows, { ":/hqlres/cf_dk", ++nData, ":/hqlres/cf_dk" } )
   AADD( aRows, { ":/hqlres/cf_es", ++nData, ":/hqlres/cf_es" } )
   AADD( aRows, { ":/hqlres/cf_eu", ++nData, ":/hqlres/cf_eu" } )
   AADD( aRows, { ":/hqlres/cf_fr", ++nData, ":/hqlres/cf_fr" } )
   AADD( aRows, { ":/hqlres/cf_gb", ++nData, ":/hqlres/cf_gb" } )
   AADD( aRows, { ":/hqlres/cf_gr", ++nData, ":/hqlres/cf_gr" } )
   AADD( aRows, { ":/hqlres/cf_ie", ++nData, ":/hqlres/cf_ie" } )
   AADD( aRows, { ":/hqlres/cf_is", ++nData, ":/hqlres/cf_is" } )
   AADD( aRows, { ":/hqlres/cf_it", ++nData, ":/hqlres/cf_it" } )
   AADD( aRows, { ":/hqlres/cf_li", ++nData, ":/hqlres/cf_li" } )
   AADD( aRows, { ":/hqlres/cf_lu", ++nData, ":/hqlres/cf_lu" } )
   AADD( aRows, { ":/hqlres/cf_nl", ++nData, ":/hqlres/cf_nl" } )
   AADD( aRows, { ":/hqlres/cf_no", ++nData, ":/hqlres/cf_no" } )
   AADD( aRows, { ":/hqlres/cf_pl", ++nData, ":/hqlres/cf_pl" } )
   AADD( aRows, { ":/hqlres/cf_pt", ++nData, ":/hqlres/cf_pt" } )
   AADD( aRows, { ":/hqlres/cf_se", ++nData, ":/hqlres/cf_se" } )
   AADD( aRows, { ":/hqlres/cf_sk", ++nData, ":/hqlres/cf_sk" } )
   AADD( aRows, { ":/hqlres/cf_us", ++nData, ":/hqlres/cf_us" } )

   oObject:hqlAddRows( aRows )

RETURN

STATIC PROCEDURE UDFindexChanged( nint, oSelf )

   hql_Trace( PADR("indexChanged: ",25) + ;
                      "nint: " + hb_NtoS(nint) + " " + ;
                      oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFactivated( nint, oSelf )

   hql_Trace( PADR("activated: ",25) + ;
                      "nint: " + hb_NtoS(nint) + " " + ;
                      oself:objectName() + " " + hb_TtoS(hb_DateTime()) )
RETURN

STATIC PROCEDURE UDFextract( oObject )
   LOCAL nRow

   FOR nRow := 0 TO (oObject:count() - 1)

      hql_Trace( "index #" + hb_NtoS( nRow ) + ;
                         " text=" + oObject:itemText( nRow ) + ;
                         " hasIcon=" + hb_ValToExp( IIF( oObject:itemIcon( nRow ):isNull(), .F., .T. ) ) + ;
                         " itemData (type)=" + hb_NtoS( oObject:itemData( nRow ):type() ) )
   NEXT nRow

RETURN
