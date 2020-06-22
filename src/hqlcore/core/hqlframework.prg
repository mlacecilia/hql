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
#include "hbclass.ch"
#include "hqlhbqt.ch"

/*!

 \brief singleton quitter. Required because singleton object seems not able to cleanup internal objects

*/
INIT PROCEDURE __hqlFramework_starter()
   __hql_frameWork()
   IF ( !hbqt_IsActiveApplication() )
      QUIT
   ENDIF
RETURN

/*!

 \brief singleton quitter. Required because singleton object seems not able to cleanup internal objects

*/
EXIT PROCEDURE __hqlFramework_quitter()
   LOCAL lCheck := __hql_FrameWork():checkObjectsAlive()
   __hql_frameWork():quit()
   IF ( lCheck .AND. __hbqt_itemsInGlobalList() > 0 )
      __hbqt_dump_itemsInGlobalList()
   ENDIF
RETURN

/*!

 \brief SINGLETON function returns a new hql_frameWork object instance
 \param(IN)
 \return hql_frameWork

*/
FUNCTION __hql_frameWork()
   STATIC s_Object
   STATIC s_Once
   hb_ThreadOnce( @s_Once, {|| s_Object := hql_frameWork():new() } )
RETURN s_Object

/*!

 \brief define hql_frameWork class

*/
CLASS hql_frameWork STATIC

   EXPORTED:
   METHOD init
   METHOD addApplicationFont
   METHOD autoNameEnabled
   METHOD checkObjectsAlive
   METHOD getAutoName
   METHOD getMainWindow
   METHOD isMainWindowDefined
   METHOD loadQss
   METHOD onAboutToQuit
   METHOD postEvent
   METHOD processEvents
   ACCESS QtApplication                   INLINE ::oQApplication
   ACCESS QtDesktop
   ACCESS QtLibraryInfo
   ACCESS QtLocale
   ACCESS QtResource
   METHOD quit
   METHOD registerResData
   METHOD registerResFile
   METHOD sendEvent
   METHOD sendKey
   METHOD setStyle
   METHOD start
   METHOD switchLang
   METHOD translate
   METHOD translations                    INLINE ::oTranslDb

   PROTECTED:
   VAR bAboutToQuit                       INIT NIL
   VAR lAutoNameEnabled                   INIT .F.
   VAR lCheckObjectsAlive                 INIT .T.
   VAR oQApplication                      INIT NIL
   VAR lStarted                           INIT .F.
   VAR oTranslDb                          INIT NIL
   CLASSVAR s_nHqlAutoName                INIT 0      // counter for automated name assignment
   METHOD __quitProcedure
   SIGNAL __hql_QAboutToQuit

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance
 \param(IN) ...
 \return self

*/
METHOD init() CLASS hql_frameWork
   LOCAL cPath, cFile, cExt, cFunction

   ::oQApplication := QApplication()

   // by default set locale system
   QLocale():setDefault( QLocale():system() )

   // register hql internal resources
   ::registerResData( hbqtres_hqlresources() )

   // trick to auto register hbqtres_programName() - resource function - if exists
   hb_FnameSplit( hb_ProgName(), @cPath, @cFile, @cExt )
   cFunction := "hbqtres_" + cFile
   IF ( hb_IsFunction(cFunction) ) // hb_IsFunction don't want () at end
      cFunction += "()"
      ::registerResData( &cFunction )
   ENDIF

   ::oTranslDb := hql_translDb():new() // translations manager

RETURN self

/*!

 \brief add application font for given source. Returns fontID: returns -1 if the font could not be loaded, found or any error
   Source can be
      1) external file e.g. <thePath><theFile>
      2) embedded file e.g. :/thePath/theFile
         WARNINGS
            before you must use registerResData( ...) else not loaded
         WARNINGS from Qt:
            do not use "alias"
            Currently only TrueType fonts and TrueType font collections are supported.
             Adding application fonts on Unix/X11 platforms without fontconfig is currently not supported.
 \param(IN) string
 \return numedric

*/
METHOD addApplicationFont( arg1 ) CLASS hql_frameWork
   LOCAL nFontId := -1
   LOCAL cFile, oFile
   LOCAL oFontDb

   cFile := hb_DefaultValue(arg1, "")

   IF ( !EMPTY(cFile) )
      oFile := QFile()
      oFile:setFileName( cFile )
      IF ( oFile:exists() )
         IF ( oFile:open( QIODevice_ReadOnly ) )
            oFontDb := QFontDatabase()
            nFontId := oFontDb:addApplicationFontFromData( oFile:readAll() )
            oFile:close()
         ENDIF
      ENDIF
   ENDIF

   //IF ( nFontId == -1 )
   //   not installed
   //ELSE
   //   oList :=  := oFontDb:applicationFontFamilies( nFontId )  Returns a list of font families for the given application font identified by id.
   //   FOR nI := 0 TO ( oList:size()-1 )
   //      oList:at(nI)
   //   NEXT
   //ENDIF
RETURN nFontId

/*!

 \brief sets/gets automated name assignment enabled
 \param(IN) [bool]
 \return bool

*/
METHOD autoNameEnabled( arg1 ) CLASS hql_frameWork
   IF ( hb_IsLogical(arg1) )
      ::lAutoNameEnabled := arg1
   ENDIF
RETURN ::lAutoNameEnabled

/*!

 \brief set/get check object[s] alive
 \param(IN) bool
 \return bool

*/
METHOD checkObjectsAlive( arg1 ) CLASS hql_frameWork
   IF ( hb_IsLogical(arg1) )
      ::lCheckObjectsAlive := arg1
   ENDIF
RETURN ::lCheckObjectsAlive

/*!

 \brief returns automated name; MUST be UPPER
 \param(IN)
 \return string

*/
METHOD getAutoName() CLASS hql_frameWork
RETURN ( "HQL_" + hb_NtoS(++::s_nHqlAutoName) )

/*!

 \brief returns hqlMainWindow if exists
 \param(IN)
 \return hqlMainWindow | NIL

*/
METHOD getMainWindow() CLASS hql_frameWork
   LOCAL oObject := NIL
   LOCAL oList := ::oQApplication:topLevelWidgets()
   LOCAL nAt

   FOR nAt := 0 TO (oList:count() - 1)
      IF (oList:at( nAt ):isDerivedFrom("hql_mainWindow") )
         oObject := oList:at( nAt )
         EXIT
      ENDIF
   NEXT nAt

RETURN oObject

/*!

 \brief returns true if exists hqlMainWindow else false
 \param(IN) none
 \return boolean

*/
METHOD isMainWindowDefined() CLASS hql_frameWork
RETURN ( hb_IsObject(::getMainWindow()) )

/*!

 \brief helper to load & install style sheet (file) at QApplication level; returns false if failed else true
 \param(IN) string
      ":/qss/coffee.qss" embedded resource style sheet file; remember must be registered before use
      "<osPath>coffee.qss" OS local file
 \return boolean

*/
METHOD loadQss( cFullFileName ) CLASS hql_frameWork
   LOCAL oFile
   LOCAL oTextStream
   LOCAL cStyleSheet
   LOCAL lLoaded := .F.

   cFullFileName := hb_DefaultValue( cFullFileName, "" )

   IF ( EMPTY( cFullFileName ) )
      RETURN lLoaded
   ENDIF

   oFile := QFile()
   oFile:setFileName( cFullFileName )

   IF ( oFile:exists() )
      IF ( oFile:open( hb_BitOr( QIODevice_ReadOnly, QIODevice_Text ) ) )
         oTextStream := QTextStream( oFile )
         // the new style sheet will be appended to current
         cStyleSheet := ::QtApplication:styleSheet() + hb_Eol()
         // add End Of Line
         cStyleSheet += oTextStream:readAll()
         // set style sheet
         ::QtApplication:setStyleSheet( cStyleSheet )
         lLoaded := .T.
      ENDIF
   ENDIF

RETURN lLoaded

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD onAboutToQuit( arg1 ) CLASS hql_frameWork
   IF ( hb_IsEvalItem(arg1) )
      ::bAboutToQuit := arg1
      ::oQApplication:connect( "aboutToQuit()", { || ::__hql_QAboutToQuit() } )
   ENDIF
RETURN self

/*!

 \brief send event
 see http://doc.qt.nokia.com/4.7-snapshot/qcoreapplication.html#sendEvent

*/
METHOD postEvent( oReceiver, oEvent, nPriority ) CLASS hql_frameWork
   LOCAL lSent := .F.

   nPriority := hb_DefaultValue(nPriority, Qt_NormalEventPriority)

   IF ( hb_IsObject(oReceiver) .AND. hb_IsObject(oEvent) )
      ::oQApplication:postEvent( oReceiver, oEvent, nPriority )
      ::processEvents()
      lSent := .T.
   ENDIF

RETURN lSent

/*!

 \brief

*/
METHOD processEvents( nFlags, nMsecs ) CLASS hql_frameWork

   nFlags := hb_DefaultValue(nFlags, QEventLoop_AllEvents)
   nMsecs := hb_DefaultValue(nMsecs, 1000)

   IF ( PCOUNT() <= 1 )
      ::oQApplication:processEvents( nFlags )
   ELSE
      ::oQApplication:processEvents( nFlags, nMsecs )
   ENDIF

RETURN Self

/*!

 \brief returns QDesktop instance
 \param(IN)
 \return (object) QDesktop

*/
METHOD QtDesktop() CLASS hql_frameWork
RETURN ::oQApplication:deskTop()

/*!

 \brief returns QLibraryInfo instance
 \param(IN)
 \return (object) QLibraryInfo

*/
METHOD QtLibraryInfo() CLASS hql_frameWork
RETURN QLibraryInfo()

/*!

 \brief returns QLocale instance
 \param(IN)
 \return (object) QLocale

*/
METHOD QtLocale() CLASS hql_frameWork
RETURN QLocale()  // without arguments return current. See hql_framework:init()

/*!

 \brief returns QResource instance
 \param(IN)
 \return (object) QResource

*/
METHOD QtResource() CLASS hql_frameWork
RETURN QResource()

/*!

 \brief initialize object instance
 \param(IN) ...
 \return self

*/
METHOD quit( nError ) CLASS hql_frameWork

   nError := hb_DefaultValue(nError, 0)

   IF ( hb_IsObject(::oQApplication) )
      ::__quitProcedure()
      ::oQApplication:exit(nError)
   ENDIF

   ::oQApplication := NIL

RETURN nError

/*!

 \brief register resource rccData; returns true if is successfully registered
 \param(IN) (string) rccData [, (string) mapRoot ]
 \return (boolean)

 \howto create embedded resource: add <inputFile>.qrc at the end of .hbp
   Harbour creates a function that returns rccData (more or less like rcc -name option)
   the function name is composed as: "hbqtres_" + <inputFile> where <inputFile> is take from <inputFile.qrc>
   eg hbqtres_inputFile()
 \howto register embedded data: ::registerResData( hbqtres_inputFile() )
   alternative
   LOCAL cFunction := "hbqtres_inputFile()"
   ::registerResData( &(cFunction) )

*/
METHOD registerResData( ... ) CLASS hql_frameWork
RETURN ::QtResource():registerResource_1( ... )

/*!

 \brief register resource rccFile; returns true if is successfully registered
 \param(IN) (string) rccFile [, (string) mapRoot ]
 \return (boolean)

 \howto create rcc file: <pathQt>/rcc -binary -o <outputPath><outputFile>.rcc <inputPath><inputFile>.qrc
 \howto register file: ::registerResFile( "<path><fileName>.rcc"

*/
METHOD registerResFile( ... ) CLASS hql_frameWork
RETURN ::QtResource():registerResource( ... )

/*!

 \brief send event
 see http://doc.qt.nokia.com/4.7-snapshot/qcoreapplication.html#sendEvent

*/
METHOD sendEvent( oReceiver, oEvent ) CLASS hql_frameWork
   LOCAL lSent := .F.
   IF ( hb_IsObject( oReceiver ) .AND. hb_IsObject( oEvent ) )
      ::QtApplication:sendEvent( oReceiver, oEvent )
      ::processEvents()
      lSent := .T.
   ENDIF
RETURN lSent

/*!

 \brief send key

*/
METHOD sendKey( oObject, nKey, nModifier ) CLASS hql_frameWork
  LOCAL oQtKey
  LOCAL lSent := .F.

  nKey      := IIF( hb_IsNumeric( nKey )     , nKey,      Qt_Key_Tab )    // by default use Tab key
  nModifier := IIF( hb_IsNumeric( nModifier ), nModifier, Qt_NoModifier ) // by default it dosn't use modifier (Shit, Ctrl, Alt )

   IF hb_IsObject( oObject )
      // see http://doc.qt.nokia.com/4.7-snapshot/qkeyevent.html
      oQtKey := QKeyEvent( QEvent_KeyPress, nKey, nModifier, 0, 0 )
      lSent := ::sendEvent( oObject, oQtKey )
   ENDIF

RETURN lSent

/*!

 \brief helper to set application style
 \param(IN) string name
 \return Self

*/
METHOD setStyle( cString ) CLASS hql_frameWork

   cString := hb_DefaultValue( cString, "" )

   // 1) Qt tries to follow the platform style. If Windows doesn't allow header (eg QTableWidget) colours to be modified, they won't be.
   //    You could run your application with a different style (using -style stylename switch, i.e. -style plastique) on Windows
   //    and it'll probably work then.
   // 2) When switching application styles, the color palette is set back to the initial colors or the system defaults.
   //    This is necessary since certain styles have to adapt the color palette to be fully style-guide compliant.
   IF ( !EMPTY( cString ) )
      ::QtApplication:setStyle( QStyleFactory():create( cString ) )
   ENDIF

RETURN Self

/*!

 \brief
 \param(IN)
 \return Self

*/
METHOD start() CLASS hql_frameWork
   IF ( !::lStarted )
      ::lStarted := .T.
      ::oTranslDb:install()
   ENDIF
RETURN self

/*!

 \brief Change current language and country
 \param(IN) numeric, numeric
 \return Self

*/
METHOD switchLang( nLanguage, nCountry ) CLASS hql_frameWork
   LOCAL oNew
//   LOCAL oCurrent := QLocale()   // without arguments return current. See hql_framework:init()

   nLanguage := hb_DefaultValue( nLanguage, QLocale_AnyLanguage ) // to avoid wrong numeric argument at Harbour level
   nCountry := hb_DefaultValue( nCountry, QLocale_AnyCountry )

   /* from Qt docs about QLocale( nLanguage, nCountry )
      If the language/country pair is found in the database, it is used.
      If the language is found but the country is not, or if the country is AnyCountry, the language is used with the most appropriate available country (for example, Germany for German),
      If neither the language nor the country are found, QLocale defaults to the default locale (see setDefault()).
   */

   oNew := QLocale( nLanguage, nCountry )

//   IF ( oNew:language() == oCurrent:language() .AND. oNew:country() == oCurrent:country() )
      // do nothing because are the same
//   ELSE
      // something different so, change Qt default Locale
      QLocale():setDefault( oNew )
      // removes and install translators via ...
      ::oTranslDb:install()
//   ENDIF

RETURN Self

/*!

 \brief translate text ALERT based on QT > 040900
   ( const char *context, const char *sourceText, const char *disambiguation = Q_NULLPTR, int n = -1 )

   hbQt call related Qt function matching exactly type of each arguments and number of arguments; to avoid problem
   hql manage arguments before call so:
   string cContext and string cSourceText are MANDATORY (minimum required) else empty string

*/
METHOD translate( cContext, cSourceText, ... ) CLASS hql_frameWork
   LOCAL cTranslated := ""
   LOCAL nArgs

   cContext := hb_DefaultValue(cContext, "")
   cSourceText := hb_DefaultValue(cSourceText, "")

   IF ( !EMPTY( cContext ) .AND. !EMPTY( cSourceText ) )

      nArgs := 2 + IIF( hb_IsString( hb_Pvalue(3) ), 1, 0 )
      IF (nArgs == 3)
         nArgs += IIF( hb_IsNumeric( hb_Pvalue(4) ), 1, 0 )
      ENDIF

      SWITCH nArgs
      CASE 4
         cTranslated := ::QtApplication:translate( cContext, cSourceText, hb_Pvalue(3), hb_Pvalue(4) )
         EXIT

      CASE 3
         cTranslated := ::QtApplication:translate( cContext, cSourceText, hb_Pvalue(3) )
         EXIT

      OTHERWISE
         cTranslated := ::QtApplication:translate( cContext, cSourceText )
         EXIT
      END SWITCH

   ENDIF

RETURN cTranslated

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] quit procedure
 \param(IN) none
 \return NIL

*/
METHOD __quitProcedure() CLASS hql_frameWork
   LOCAL oList := ::QtApplication:topLevelWidgets()
   LOCAL nAt

   FOR nAt := 0 TO (oList:count() - 1)
      oList:at( nAt ):close()
      //oList:at( nAt ):disconnect()
      //IF ( hql_IsHqlAbsDerived( oList:at( nAt ) ) )
      //   oList:at( nAt ):hqlCleaner()
      //ENDIF
      hql_DeleteWidget( oList:at( nAt ) )
   NEXT

   ::oTranslDb:remove() // remove translators

RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QAboutToQuit() CLASS hql_frameWork
   IF ( hb_IsEvalItem(::bAboutToQuit) )
      EVAL( ::bAboutToQuit, Self )
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
