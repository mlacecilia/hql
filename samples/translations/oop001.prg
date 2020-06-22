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
   LOCAL nAt

   hqlErrorSys()  /*hbqt_errorsys()*/

   hqlSetStyle( "Fusion" )

   hqlOnAboutToQuit( { || UDFOnAboutToQuit() } )

   /* HOW TO DEFINE TRANSLATIONS loading */

   // default Qr translations: we need only give file prefix "qt_". We assume files are located in a path defined by qt.conf file
   hqlTranslations:addItem( "qt_" )

   // EMBEDDED translations
   //    before we need to register as resource
   hql_Trace( hb_ValToExp( hqlRegisterResData( hbqtres_embres() ) ) )
   //    than wee need to give file prefix and path for QTranslator
   hqlTranslations:addItem( "emb_", ":/testemb/resources/" )

   // EXTERNAL translations .qm files. In this scenario, a path will be used so ext_it.qm must be (manually) copied in the same program directory
   hqlTranslations:addItem( "ext_", hb_DirBase() )

   /*
      install all translators
         hqlTranslations:install()
      remove all translators
         hqlTranslations:remove()
      delete an item
         hqlTranslations:deleteItem( "emb_" )
      how many
         hqlTranslations:size()
   */

   /*
      HOW TO change language
      hqlSwitchLang( QLocale_French, QLocale_France )
      hqlSwitchLang( QLocale_C, QLocale_AnyCountry )     // The "C" locale is identical in behavior to English/UnitedStates
   */

   hqlStart()

   /* info about translations */
   nAt := 1
   DO WHILE ( nAt <= hqlTranslations:size() )
      hql_Trace( " prefix=" + hqlTranslations:at(nAt):prefix() + ;
                         " path=" + hqlTranslations:at(nAt):path() + ;
                         " isInstalled=" + hb_ValToExp( hqlTranslations:at(nAt):installed() ) )
      ++nAt
   ENDDO

   udfLocaleInfo( hqlQLocale )

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

   WITH OBJECT oWnd := hqlMainWindow( /*name*/ )
      :hqlCaption( "HQL translations tester" ) // ==>:setWindowTitle( "tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget( /*name*/ ) )
      :hqlOnLanguageChange( { || udfTranslate( oWnd ) } )

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

         WITH OBJECT :hqlAddMenu( /*name*/ )
            :hqlCaption( "&Language" )
            WITH OBJECT :hqlAddAction( /*name*/ )
               :hqlCaption( "&Italian" )
               :hqlOnTriggered( { || hqlSwitchLang( QLocale_Italian, QLocale_Italy ) } )
            END WITH
            WITH OBJECT :hqlAddAction( /*name*/ )
               :hqlCaption( "&French" )
               :hqlOnTriggered( { || hqlSwitchLang( QLocale_French, QLocale_France ) } )
            END WITH
            WITH OBJECT :hqlAddAction( /*name*/ )
               :hqlCaption( "&English" )
               :hqlOnTriggered( { || hqlSwitchLang( QLocale_English, QLocale_UnitedKingdom ) } )
            END WITH
         END WITH
      END WITH

      WITH OBJECT :centralWidget()

         WITH OBJECT hqlTextEdit( "editwdg" )
            :setGeometry( 10, 10, 780, 580 )
         END WITH

      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   udfTranslate( oWnd )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE udfTranslate( oForm )

   LOCAL oTedit := oForm:editwdg()
   LOCAL cEol := e"\n"
   LOCAL cHtmlEol := "<br>"

   oTedit:clear()

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertHtml( e"<b>Qt translations - start</b>" + cHtmlEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertPlainText( hqlTran( "QFileDialog", "My Computer", /*cDisambiguation*/, /*nPlural*/ ) + cEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertPlainText( hqlTran( "QFileDialog", "File", /*cDisambiguation*/, /*nPlural*/ ) + cEol )   // non tradotto IT

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertPlainText( hqlTran( "QFileDialog", "Show", /*cDisambiguation*/, /*nPlural*/ ) + cEol )   // non tradotto IT

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertPlainText( hqlTran( "QFileDialog", "Unknown", /*cDisambiguation*/, /*nPlural*/ ) + cEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertPlainText( hqlTran( "QFileDialog", "Find Directory", /*cDisambiguation*/, /*nPlural*/ ) + cEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertPlainText( hqlTran( "QFileDialog", "New Folder", /*cDisambiguation*/, /*nPlural*/ ) + cEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertPlainText( hqlTran( "QFileDialog", "&New Folder", /*cDisambiguation*/, /*nPlural*/ ) + cEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertPlainText( hqlTran( "QFileDialog", "&Choose", /*cDisambiguation*/, /*nPlural*/ ) + cEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertPlainText( hqlTran( "QFileDialog", "Remove", /*cDisambiguation*/, /*nPlural*/ ) + cEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertPlainText( hqlTran( "QFileDialog", "File &name:", /*cDisambiguation*/, /*nPlural*/ ) + cEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertPlainText( hqlTran( "QFileDialog", "Create New Folder", /*cDisambiguation*/, /*nPlural*/ ) + cEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertPlainText( hqlTran( "QApplication", "Activate", /*cDisambiguation*/, /*nPlural*/ ) + cEol )   // non tradotto IT

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertHtml( e"<b>Qt translations - end</b>" + cHtmlEol )

   // PROGRAM external messages
   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertHtml( cHtmlEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertHtml( e"<b>PROGRAM external translations - start</b>" + cHtmlEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertPlainText( hqlTran( "extTr", "&About %1", /*cDisambiguation*/, /*nPlural*/ ) + cEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertPlainText( hql_Sprintf(hqlTran( "extTr", "&About %1", /*cDisambiguation*/, /*nPlural*/ ), "<filename>" ) + cEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertHtml( e"<b>PROGRAM external translations - end</b>" + cHtmlEol )

   // PROGRAM embedded messages
   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertHtml( cHtmlEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertHtml( e"<b>PROGRAM embedded translations - start</b>" + cHtmlEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertPlainText( hqlTran( "embTr", "Embedded message %1", /*cDisambiguation*/, /*nPlural*/ ) + cEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertPlainText( hql_Sprintf( hqlTran( "embTr", "Embedded message %1", /*cDisambiguation*/, /*nPlural*/ ), "<filename>" ) + cEol )

   oTedit:moveCursor( QTextCursor_End )
   oTedit:insertHtml( e"<b>PROGRAM embedded translations - end</b>" + cHtmlEol )

   oTedit:moveCursor( QTextCursor_End )
RETURN

STATIC PROCEDURE udfLocaleInfo( oLocale )

   LOCAL nP

   hql_Trace( "language =" + hb_NtoC( oLocale:language() ) + ;
                      " country =" + hb_NtoC( oLocale:country() ) + ;
                      " bcp47Name =" + oLocale:bcp47Name() )

   hql_Trace( "decimalpoint (U)=" + hb_NumToHex( oLocale:decimalpoint:unicode() ) + ;
                      " exponential (U)=" + hb_NumToHex( oLocale:exponential():unicode() ) + ;
                      " measurementSystem =" + hb_NtoC( oLocale:measurementSystem() ) + ;
                      " groupSeparator (U)=" + hb_NumToHex( oLocale:groupSeparator():unicode() ) + ;
                      " negativeSign (U)=" + hb_NumToHex( oLocale:negativeSign():unicode() ) + ;
                      " positiveSign (U)=" + hb_NumToHex( oLocale:positiveSign():unicode() ) + ;
                      " percent (U)=" + hb_NumToHex( oLocale:percent():unicode() ) )

   hql_Trace( "currencySymbol.IsoCode =" + oLocale:currencySymbol( QLocale_CurrencyIsoCode ) + ;
                      " currencySymbol.Simbol =" + oLocale:currencySymbol( QLocale_CurrencySymbol ) + ;
                      " currencySymbol.DisplayName =" + oLocale:currencySymbol( QLocale_CurrencyDisplayName ) )

   hql_Trace( "dateFormat.LongFormat =" + oLocale:dateFormat( QLocale_LongFormat ) + ;
                      " dateFormat.ShortFormat =" + oLocale:dateFormat( QLocale_ShortFormat ) + ;
                      " dateFormat.NarrowFormat =" + oLocale:dateFormat( QLocale_NarrowFormat ) )

   hql_Trace( "dateTimeFormat.LongFormat =" + oLocale:dateTimeFormat( QLocale_LongFormat ) + ;
                      " dateTimeFormat.ShortFormat =" + oLocale:dateTimeFormat( QLocale_ShortFormat ) + ;
                      " dateTimeFormat.NarrowFormat =" + oLocale:dateTimeFormat( QLocale_NarrowFormat ) )

   FOR nP := 1 TO 7   // 1=monday
      hql_Trace( "day #" + hb_NtoC(nP) + ;
                         " dayName.LongFormat =" + oLocale:dayName( nP, QLocale_LongFormat ) + ;
                         " dayName.ShortFormat =" + oLocale:dayName( nP, QLocale_ShortFormat ) + ;
                         " dayName.NarrowFormat =" + oLocale:dayName( nP, QLocale_NarrowFormat ) )
   NEXT nP

   FOR nP := 1 TO 12   // 1=monday
      hql_Trace( "day #" + hb_NtoC(nP) + ;
                         " monthName.LongFormat =" + oLocale:monthName( nP, QLocale_LongFormat ) + ;
                         " monthName.ShortFormat =" + oLocale:monthName( nP, QLocale_ShortFormat ) + ;
                         " monthName.NarrowFormat =" + oLocale:monthName( nP, QLocale_NarrowFormat ) )
   NEXT nP

RETURN
