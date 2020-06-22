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

 \brief define hql_abs0020 class
   inherited by:
      hql_abs0200,
      hql_childWindow,
      hql_mainWindow,
      hql_modalWindow

*/
CLASS hql_abs0020 INHERIT hql_abs0001

   EXPORTED:
   METHOD hqlActivate
   METHOD hqlActivated                    INLINE ::lHqlActivated
   METHOD hqlCaption                      SETGET
   METHOD hqlCentered                     SETGET
   METHOD hqlClearFkey
   METHOD hqlOnActivate
   METHOD hqlOnClose
   METHOD hqlOnDeactivate
   METHOD hqlOnEachActivation
   METHOD hqlOnLanguageChange
   METHOD hqlOnMaximize
   METHOD hqlOnMinimize
   METHOD hqlOnResize
   METHOD hqlOnShow
   METHOD hqlRelease
   METHOD hqlSetFkey
   METHOD hqlSetSplashScreen

   PROTECTED:
   VAR lHqlActivated                      INIT .F.
   VAR bHqlClose                          INIT NIL
   VAR bHqlDeactivate                     INIT NIL
   VAR bHqlEachActivation                 INIT NIL
   VAR lHqlFirstActivation                INIT .T.
   VAR bHqlFirstActivation                INIT NIL
   VAR oHqlEventLoop                      INIT NIL          // local event loop
   VAR aHqlFunctionKey                    INIT {}           // store functions key block
   VAR bHqlLanguageChange                 INIT NIL
   VAR bHqlMaximize                       INIT NIL
   VAR bHqlMinimize                       INIT NIL
   VAR nHqlReleaseCode                    INIT 0
   VAR bHqlResize                         INIT NIL
   VAR bHqlShow                           INIT NIL
   VAR oHqlSplashScreen                   INIT NIL
   VAR nHqlWinPos                         INIT HQL_PARENTCENTER
   METHOD __hqlConnect
   METHOD __hqlGetFkeyPos
   METHOD __hqlHasEventLoop               INLINE ( hb_IsObject(::oHqlEventLoop) )
   METHOD __hqlWinPositioning
   SIGNAL __hql_QClose                    // connected by default
   SIGNAL __hql_QKeyPress                 // connected by default
   SIGNAL __hql_QLanguageChange
   SIGNAL __hql_QResize
   SIGNAL __hql_QShow                     // connected by default
   SIGNAL __hql_QWindowActivate           // connected by default
   SIGNAL __hql_QWindowDeactivate
   SIGNAL __hql_QWindowStateChange        // connected by default

   HIDDEN:

ENDCLASS

/*!

 \brief Activate (show/rise) form
 \param(IN)
 \return numeric

*/
METHOD hqlActivate() CLASS hql_abs0020

   IF ( !::lHqlActivated )
      ::__hqlWinPositioning()
   ENDIF

   ::show()

   IF ( !::lHqlActivated .AND. hb_IsObject( ::oHqlSplashScreen ) )
      ::oHqlSplashScreen:finish( Self )
   ENDIF

   ::raise()

   ::activateWindow()

   ::lHqlActivated := .T.

   HqlFw:processEvents( /*nFlags*/, /*nMsecs*/ )

RETURN ::nHqlReleaseCode

/*!

 \brief set/get generic text
 \param(IN) [string]
 \return string

*/
METHOD hqlCaption( arg1 ) CLASS hql_abs0020
   IF ( hb_IsString(arg1) )
      ::setWindowTitle( arg1 )
   ENDIF
RETURN ::windowTitle()

/*!

 \brief set/get centered property
 \param(IN) numeric
 \return numeric

*/
METHOD hqlCentered( arg1 ) CLASS hql_abs0020
   IF ( PCOUNT() == 1 .AND. hb_IsNumeric(arg1) )
      SWITCH arg1
      CASE HQL_NOCENTER
      CASE HQL_PARENTCENTER
      CASE HQL_DESKTOPCENTER
         ::nHqlWinPos := arg1
         EXIT
      ENDSWITCH
   ENDIF
RETURN ::nHqlWinPos

/*!

 \brief clear function key from the list. Without arguments delete all
 \param(IN) numeric, numeric
 \return self

*/
METHOD hqlClearFkey( nKey, nModifiers ) CLASS hql_abs0020
   LOCAL nX

   IF ( PCOUNT() == 0 )
      ::aHqlFunctionKey := {}
   ELSEIF ( hb_IsNumeric(nKey) .AND. hb_IsNumeric(nModifiers) .AND. ( nX := ::__hqlGetFkeyPos( nKey, nModifiers ) ) > 0 )
      hb_Adel( ::aHqlFunctionKey, nX, .T. )
   ENDIF

RETURN Self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnActivate( arg1 ) CLASS hql_abs0020
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlFirstActivation := arg1
      // connected by default
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnClose( arg1 ) CLASS hql_abs0020
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlClose := arg1
      // connected by default
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnDeactivate( arg1 ) CLASS hql_abs0020
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlDeactivate := arg1
      IF ( hb_IsEvalItem(::bHqlDeactivate) )
         ::connect( QEvent_WindowDeactivate, { |oEvent| ::__hql_QWindowDeactivate(oEvent) } )
      ELSE
         ::disconnect( QEvent_WindowDeactivate )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnEachActivation( arg1 ) CLASS hql_abs0020
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlEachActivation := arg1
      // connected by default
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnLanguageChange( arg1 ) CLASS hql_abs0020
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlLanguageChange := arg1
      IF ( hb_IsEvalItem(::bHqlLanguageChange) )
         ::connect( QEvent_LanguageChange, { |oEvent| ::__hql_QLanguageChange(oEvent) } )
      ELSE
         ::disconnect( QEvent_LanguageChange )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnMaximize( arg1 ) CLASS hql_abs0020
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlMaximize := arg1
      // related to __hql_QWindowStateChange
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnMinimize( arg1 ) CLASS hql_abs0020
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlMinimize := arg1
      // related to __hql_QWindowStateChange
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnResize( arg1 ) CLASS hql_abs0020
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlResize := arg1
      IF ( hb_IsEvalItem(::bHqlResize) )
         ::connect( QEvent_Resize , { |oEvent| ::__hql_QResize(oEvent) } )
      ELSE
         ::disconnect( QEvent_Resize )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnShow( arg1 ) CLASS hql_abs0020
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlShow := arg1
      // connected by default
   ENDIF
RETURN self

/*!

 \brief Release form
 \param(IN) numeric
 \return self

*/
METHOD hqlRelease( nExit ) CLASS hql_abs0020
   ::nHqlReleaseCode := hb_DefaultValue(nExit, 0)
   ::close()
RETURN self

/*!

 \brief set a block to be executed when a key (defined as nkey, nmodifiers) is pressed
 \param(IN) numeric, numeric, block
 \return self

*/
METHOD hqlSetFkey( nKey, nModifiers, bAction ) CLASS hql_abs0020
   LOCAL nX

   IF ( hb_IsNumeric(nKey) .AND. hb_IsNumeric(nModifiers) )
      nX := ::__hqlGetFkeyPos( nKey, nModifiers )
      IF ( nX > 0 .AND. bAction == NIL )
         hb_Adel( ::aHqlFunctionKey, nX, .T. )
      ELSEIF ( hb_IsEvalItem(bAction) )
         IF ( nX > 0 )
            ::aHqlFunctionKey[nX,3] := bAction
         ELSE
            AADD( ::aHqlFunctionKey, { nKey, nModifiers, bAction } )
         ENDIF
      ENDIF
   ENDIF

RETURN Self

/*!

 \brief set splashScreen object reference. In this way ::oHqlSplashScreen:finish( me ) can be used (see hqlActivate
 \param[in] splashScreen
 \return self

*/
METHOD hqlSetSplashScreen( arg1 ) CLASS hql_abs0020
   IF ( arg1 == NIL )
      ::oHqlSplashScreen := NIL
   ELSEIF ( hql_IsDerived(arg1, "QSplashScreen") )
      ::oHqlSplashScreen := arg1
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTD] Connect signal
 \param(IN)
 \return bool

*/
METHOD __hqlConnect() CLASS hql_abs0020
   ::hql_abs0001:__hqlConnect()
   ::connect( QEvent_Close, { |oEvent| ::__hql_QClose(oEvent) } )
   ::connect( QEvent_KeyPress , { |oEvent| ::__hql_QKeyPress(oEvent) } )
   ::connect( QEvent_Show, { |oEvent| ::__hql_QShow(oEvent) } )
   ::connect( QEvent_WindowActivate, { |oEvent| ::__hql_QWindowActivate(oEvent) } )
   ::connect( QEvent_WindowStateChange, { |oEvent| ::__hql_QWindowStateChange(oEvent) } )
RETURN .T.

/*!

 \brief [PROTECTED] returns fkey position for given nkey and nmodifiers
 \param(IN) numeric, numeric
 \return numeric

*/
METHOD __hqlGetFkeyPos( nKey, nModifiers ) CLASS hql_abs0020
RETURN ( ASCAN( ::aHqlFunctionKey,{|x| x[1] == nKey .AND. x[2] == nModifiers} ) )

/*!

 \brief [PROTECTED] helper to center
 \param[in] none
 \return NIL

*/
METHOD __hqlWinPositioning() CLASS hql_abs0020
   IF ( ::nHqlWinPos > HQL_NOCENTER) // if HQL_NOCENTER user left OS to do something
      IF ( ::nHqlWinPos == HQL_PARENTCENTER .AND. ::hqlHasParent() )  // if parent centered and it has a vali parent
         ::setGeometry( ::style:alignedRect( Qt_LeftToRight, Qt_AlignCenter, ::size(), ::parent:geometry() ) )
      ELSE  // always take desktop
         ::setGeometry( ::style:alignedRect( Qt_LeftToRight, Qt_AlignCenter, ::size(), HqlFw:QtDesktop:availableGeometry() ) )
      ENDIF
   ENDIF
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
METHOD __hql_QClose( oEvent ) CLASS hql_abs0020
   oEvent:accept()
   IF ( hb_IsEvalItem(::bHqlClose) )
      EVAL( ::bHqlClose, Self )  // Self always as last
   ENDIF
   // hack when a local loop event used
   IF ( ::__hqlHasEventLoop() )
      ::oHqlEventLoop:exit( ::nHqlReleaseCode )  // :quit() unsupported by HbQt
   ENDIF

   ::__hqlSignalEverybody( self )
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QKeyPress( oEvent ) CLASS hql_abs0020

   LOCAL nKey := oEvent:key()
   LOCAL nModifiers := oEvent:modifiers()
   LOCAL nX
//   LOCAL oHelpSystem

   // handling help option if enabled
//   IF LEN( ::aHelpOption ) == 3 .AND. nKey == ::aHelpOption[1] .AND. nModifiers ==  ::aHelpOption[2] .AND. ::lHqlWakeUpSignal
//      oEvent:accept()
//      oHelpSystem := HqlHelp( /*cName*/, Self )
//      oHelpSystem:HqlContext( ::aHelpOption[3] )
//      oHelpSystem:HqlGetHelp()
//      RETURN .F.
//   ENDIF

   IF ( ( nX := ::__hqlGetFkeyPos( nKey, nModifiers ) ) > 0 .AND. hb_IsEvalItem( ::aHqlFunctionKey[nX,3] ) )
      oEvent:accept()
      EVAL( ::aHqlFunctionKey[nX,3], Self )  // Self always as last
      RETURN .T.
   ENDIF

RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QLanguageChange( oEvent ) CLASS hql_abs0020
   oEvent:accept()
   IF ( hb_IsEvalItem( ::bHqlLanguageChange ) )
      EVAL( ::bHqlLanguageChange, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QResize( oEvent ) CLASS hql_abs0020
   oEvent:accept()
   IF hb_IsEvalItem( ::bHqlResize )
      EVAL( ::bHqlResize, oEvent:size(), oEvent:oldSize(), Self )  // Self always as last
//      2012when QResize event is "enabled" at the same time of Maximixed/Minimzed signals it is double emitted if return .F.
//      RETURN .T.
  ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QShow( oEvent ) CLASS hql_abs0020
   oEvent:accept()
   IF ( hb_IsEvalItem(::bHqlShow) )
      EVAL( ::bHqlShow, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QWindowActivate( oEvent ) CLASS hql_abs0020
   oEvent:accept()

   IF ( ::lHqlFirstActivation )
      ::lHqlFirstActivation := .F.
      IF ( hb_IsEvalItem(::bHqlFirstActivation) )
         EVAL( ::bHqlFirstActivation, Self )  // Self always as last
      ENDIF
   ENDIF

   IF ( hb_IsEvalItem(::bHqlEachActivation) )
      EVAL( ::bHqlEachActivation, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QWindowDeactivate( oEvent ) CLASS hql_abs0020
   oEvent:accept()
   IF ( hb_IsEvalItem( ::bHqlDeactivate ) )
      EVAL( ::bHqlDeactivate, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QWindowStateChange( oEvent ) CLASS hql_abs0020
   oEvent:accept()

   IF ( hb_BitAnd( ::windowState(), Qt_WindowMinimized ) == Qt_WindowMinimized .AND. hb_IsEvalItem( ::bHqlMinimize ) )
      EVAL( ::bHqlMinimize, Self )
      RETURN .T.
   ENDIF

   IF ( hb_bitAnd( ::windowState(), Qt_WindowMaximized ) == Qt_WindowMaximized .AND. hb_IsEvalItem( ::bHqlMaximize ) )
      EVAL( ::bHqlMaximize, Self )
      RETURN .T.
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
