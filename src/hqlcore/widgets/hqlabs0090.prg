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

#define HQL_NAVIGATENEXT                  0x00  // default
#define HQL_NAVIGATEPREV                  0x01
#define HQL_NAVIGATESELF                  0x02

/*!

 \brief define hql_abs0090 class
   inherited by:
      hql_lineEdit,

*/
CLASS hql_abs0090 INHERIT hql_abs0001

   EXPORTED:
   METHOD hqlCaseLetter                   SETGET
   METHOD hqlClearFkey
   METHOD hqlCursorToInit
   METHOD hqlInputMask                    SETGET
   METHOD hqlMousePostValid                                 // WARNING: it seems not required; if true, enables postValidate when field is left using mouse
   METHOD hqlOnEditingFinished
   METHOD hqlOnFixup
   METHOD hqlOnReturnPressed
   METHOD hqlOnTabPressed
   METHOD hqlOnTextChanged
   METHOD hqlOnTextEdited
   METHOD hqlOnValidator
   METHOD hqlReturnAsTab
   METHOD hqlSetFkey
   METHOD hqlSetFocus
   METHOD hqlUpDownAsTab                  SETGET
   METHOD hqlValid                        SETGET            // post validation
   METHOD hqlValidator                    SETGET
   METHOD hqlWhen                         SETGET            // pre validation

   PROTECTED:
   VAR nHqlCaseLetter                     INIT HQL_UPPERLOWERCASE
   VAR lHqlCursorInit                     INIT .F.
   VAR bHqlEditingFinished                INIT NIL
   VAR lHqlFixupCalled                    INIT .F.
   VAR lHqlFocusinPreValidFailed          INIT .F.          // internal flag when prevalidate fails
   VAR aHqlFunctionKey                    INIT {}           // store functions key block
   VAR bHqlKeyTabPress                    INIT NIL
   VAR lHqlMousePostValid                 INIT .F.          // WARNING: it seems not required; if true, enables postValidate when field is left using mouse
   VAR bHqlOnFixUp                        INIT NIL
   VAR bHqlOnValidator                    INIT NIL
   VAR lHqlReturnAsTab                    INIT .F.
   VAR bHqlReturnPressed                  INIT NIL
   VAR bHqlTextChanged                    INIT NIL
   VAR bHqlTextEdited                     INIT NIL
   VAR lHqlUpDownAsTab                    INIT .T.          // UP and DOWN keys act like TAB when .t.
   VAR bHqlValid                          INIT NIL          // post validation
   VAR bHqlWhen                           INIT NIL          // pre validation
   METHOD __hqlConnect
   METHOD __hqlGetFkeyPos
   METHOD __hqlPostValidate
   METHOD __hqlPreValidate
   METHOD __hqlNavigate
   METHOD __hqlValueSet
   SIGNAL __hql_KeyTabPressed
   SIGNAL __hql_OnFixUp
   SIGNAL __hql_OnValidator
   SIGNAL __hql_QEditingFinished
   SIGNAL __hql_QFocusIn                                    // connected by default
   SIGNAL __hql_QFocusOut                                   // connected by default
   SIGNAL __hql_QKeypress                                   // connected by default
   SIGNAL __hql_QReturnPressed
   SIGNAL __hql_QTextChanged
   SIGNAL __hql_QTextEdited

   HIDDEN:

ENDCLASS

/*!

 \brief set/get case letter
 \param(IN) [numeric]
 \return numeric

*/
METHOD hqlCaseLetter( arg1 ) CLASS hql_abs0090
   IF ( hb_IsNumeric(arg1) )
      SWITCH arg1
      CASE HQL_UPPERLOWERCASE
      CASE HQL_ALL_UPPERCASE
      CASE HQL_ALL_LOWERCASE
         ::nHqlCaseLetter := arg1
         EXIT
      ENDSWITCH
   ENDIF
RETURN ::nHqlCaseLetter

/*!

 \brief delete function key from the list. Without arguments delete all
 \param(IN) numeric, numeric
 \return self

*/
METHOD hqlClearFkey( nKey, nModifiers ) CLASS hql_abs0090
   LOCAL nX

   IF ( PCOUNT() == 0 )
      ::aHqlFunctionKey := {}
   ELSEIF ( hb_IsNumeric(nKey) .AND. hb_IsNumeric(nModifiers) .AND. ( nX := ::__hqlGetFkeyPos( nKey, nModifiers ) ) > 0 )
      hb_Adel( ::aHqlFunctionKey, nX, .T. )
   ENDIF

RETURN Self

/*!

 \brief used to move cursor at init; if boolean value given set cursor to init (INTERNAL) flag
      a) normally only move cursor to the start position
      b) with argument .t.: always cursor moved to the start when hqlValue used to setting up a new value
 \param(IN) [bool]
 \return bool

*/
METHOD hqlCursorToInit( arg1 ) CLASS hql_abs0090
   IF ( hb_IsLogical(arg1) )
      ::lHqlCursorInit := arg1
   ENDIF
   ::setCursorPosition( 0 )
RETURN ::lHqlCursorInit

/*!

 \brief set/get inputmask
 \param(IN) [string]
 \return string

*/
METHOD hqlInputMask( arg1 ) CLASS hql_abs0090
   IF ( hb_IsString(arg1) )
      ::setInputMask( arg1 )
   ENDIF
RETURN ::inputMask()

/*!

 \brief set/get lHqlMousePostValid flag; if true, enables postValidate when field is left using mouse
 \param(IN) [bool]
 \return bool

*/
METHOD hqlMousePostValid( arg1 ) CLASS hql_abs0090
   IF ( hb_IsLogical(arg1) )
      ::lHqlMousePostValid := arg1
   ENDIF
RETURN ::lHqlMousePostValid

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnEditingFinished( arg1 ) CLASS hql_abs0090
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlEditingFinished := arg1
      IF ( hb_IsEvalItem(::bHqlEditingFinished) )
         ::connect( "editingFinished()", { || ::__hql_QEditingFinished() } )
      ELSE
         ::disconnect( "editingFinished()" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnFixup( arg1 ) CLASS hql_abs0090
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlOnFixUp := arg1
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnReturnPressed( arg1 ) CLASS hql_abs0090
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlReturnPressed := arg1
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnTabPressed( arg1 ) CLASS hql_abs0090
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlKeyTabPress := arg1
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnTextChanged( arg1 ) CLASS hql_abs0090
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlTextChanged := arg1
      IF ( hb_IsEvalItem(::bHqlTextChanged) )
         ::connect( "textChanged(QString)", { |string| ::__hql_QTextChanged(string) } )
      ELSE
         ::disconnect( "textChanged(QString)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnTextEdited( arg1 ) CLASS hql_abs0090
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlTextEdited := arg1
      IF ( hb_IsEvalItem(::bHqlTextEdited) )
         ::connect( "textEdited(QString)", { |string| ::__hql_QTextEdited(string) } )
      ELSE
         ::disconnect( "textEdited(QString)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnValidator( arg1 ) CLASS hql_abs0090
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlOnValidator := arg1
   ENDIF
RETURN self

/*!

 \brief sets/return if return pressed follow the same behaviour when tab is pressed (next field)
 \param(IN) [boolean]
 \return bool

*/
METHOD hqlReturnAsTab( arg1 ) CLASS hql_abs0090
   IF ( hb_IsLogical(arg1) )
      ::lHqlReturnAsTab := arg1
   ENDIF
RETURN ::lHqlReturnAsTab

/*!

 \brief set a block to be executed when a key (defined as nkey, nmodifiers) is pressed or remove if baction is NIL
 \param(IN) numeric, numeric, block
 \return self

*/
METHOD hqlSetFkey( nKey, nModifiers, bAction ) CLASS hql_abs0090
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

 \brief helper to set focus by default Qt_OtherFocusReason used
 \param(IN) numeric reason
 \return Self

*/
METHOD hqlSetFocus( nReason ) CLASS hql_abs0090
   nReason := hb_DefaultValue( nReason, Qt_OtherFocusReason)
   ::setFocus( nReason )
   ::selectAll()
RETURN Self

/*!

 \brief sets/return if UP or DOWN arrow key follow the same behaviour when tab is pressed
 \param(IN) [boolean]
 \return boolean

*/
METHOD hqlUpDownAsTab( arg1 ) CLASS hql_abs0090
   IF ( hb_IsLogical(arg1) )
      ::lHqlUpDownAsTab := arg1
   ENDIF
RETURN ::lHqlUpDownAsTab

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlValid( arg1 ) CLASS hql_abs0090
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlValid := arg1
   ENDIF
RETURN self

/*!

 \brief set/get validator
 \param(IN) [validator]
 \return validator

*/
METHOD hqlValidator( arg1 ) CLASS hql_abs0090
   IF ( PCOUNT() == 1 .AND. hql_IsDerived(arg1, "hbqvalidator") )
      ::setValidator( arg1 )
   ENDIF
RETURN ::validator()

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlWhen( arg1 ) CLASS hql_abs0090
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlWhen := arg1
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] connect signal/event
 \param(IN) none
 \return bool

*/
METHOD __hqlConnect() CLASS hql_abs0090
   ::hql_abs0001:__hqlConnect()
   ::connect( QEvent_FocusIn , {|oEvent| ::__hql_QFocusIn(oEvent)  } )
   ::connect( QEvent_FocusOut, {|oEvent| ::__hql_QFocusOut(oEvent) } )
   ::connect( QEvent_KeyPress, {|oEvent| ::__hql_QKeypress(oEvent) } )
RETURN .T.

/*!

 \brief [PROTECTED] returns fkey position for given nkey and nmodifiers
 \param(IN) numeric, numeric
 \return numeric

*/
METHOD __hqlGetFkeyPos( nKey, nModifiers ) CLASS hql_abs0090
RETURN ( ASCAN( ::aHqlFunctionKey,{|x| x[1] == nKey .AND. x[2] == nModifiers} ) )

/*!

 \brief [PROTECTED] helper to navigate forward or backward
 \param(IN) numeric direction
 \return nil

*/
METHOD __hqlNavigate( nDirection ) CLASS hql_abs0090

   nDirection := hb_DefaultValue(nDirection, HQL_NAVIGATENEXT)

   SWITCH nDirection

   CASE HQL_NAVIGATENEXT
      //HqlFw:sendEvent( Self, QKeyEvent( QEvent_KeyPress, Qt_Key_Tab, Qt_NoModifier ) )
      HqlFw:postEvent( Self, QKeyEvent( QEvent_KeyPress, Qt_Key_Tab, Qt_NoModifier ), Qt_HighEventPriority )
      EXIT

   CASE HQL_NAVIGATEPREV
      //HqlFw:sendEvent( Self, QKeyEvent( QEvent_KeyPress, Qt_Key_Backtab, Qt_NoModifier ) )
      HqlFw:postEvent( Self, QKeyEvent( QEvent_KeyPress, Qt_Key_Backtab, Qt_NoModifier ), Qt_HighEventPriority )
      EXIT

   CASE HQL_NAVIGATESELF
      ::hqlSetFocus( /*nreason*/ )
      EXIT

   ENDSWITCH

RETURN NIL

/*!

 \brief [PROTECTED] perform post validate
 \param(IN) none
 \return boolean

*/
METHOD __hqlPostValidate() CLASS hql_abs0090
   LOCAL lReturned := .T.

   IF ( hb_IsEvalItem(::bHqlValid) )
      lReturned := EVAL( ::bHqlValid, Self )
      lReturned := IIF( hb_IsLogical( lReturned ), lReturned, .T. )
   ENDIF

RETURN lReturned

/*!

 \brief [PROTECTED] perform pre validate
 \param(IN) none
 \return boolean

*/
METHOD __hqlPreValidate() CLASS hql_abs0090
   LOCAL lReturned := .T.

   IF ( hb_IsEvalItem(::bHqlWhen) )
      lReturned := EVAL( ::bHqlWhen, Self )
      lReturned := IIF( hb_IsLogical( lReturned ), lReturned, .T. )
   ENDIF

RETURN lReturned

/*!

 \brief [PROTECTED] set text
 \param(IN) string
 \return NIL

*/
METHOD __hqlValueSet( arg1 ) CLASS hql_abs0090
   IF ( hb_IsString( arg1 ) )
      IF ( ::nHqlCaseLetter == HQL_ALL_UPPERCASE )
         arg1 := UPPER( arg1 )
      ELSEIF ( ::nHqlCaseLetter == HQL_ALL_LOWERCASE )
         arg1 := LOWER( arg1 )
      ENDIF

      ::setText( arg1 )

      IF ( ::lHqlCursorInit .OR. LEN(ALLTRIM(arg1)) == 0 )
         ::hqlCursorToInit()
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
SIGNAL __hql_KeyTabPressed() CLASS hql_abs0090
   IF ( hb_IsEvalItem( ::bHqlKeyTabPress ) )
      EVAL( ::bHqlKeyTabPress, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] handle validator fixUp
   Qt states and related Hb: Acceptable => true, Invalid => false, Intermediate => NIL
   fIxUp (seems) fired ON when Intermediate state is set ON by validator.
 \param(IN) string
 \return string

*/
SIGNAL __hql_OnFixUp( cText ) CLASS hql_abs0090
   LOCAL temp
   ::lHqlFixupCalled := .T.
   IF ( hb_IsEvalItem( ::bHqlOnFixUp ) )
      temp := EVAL( ::bHqlOnFixUp, cText )
      cText := hb_DefaultValue(temp, cText)  // to be ensure always string returned
   ENDIF
RETURN cText

/*!

 \brief [PROTECTED] handle validator
   Qt states and related Hb: Acceptable => true, Invalid => false, Intermediate => NIL
   qtContrib/hbqt/qtgui/hbqt_hbqvalidator.cpp/validate(...) manage these returned values
      - { <string>, <numeric>, <state> } where <state> can be: bool or NIL
      - <string>
      - <bool>
      ELSE Intermediate state is set
   So, keep in mind that this and bHqlOnValidator block they must return these values else Interediate state is set.
   States meaning:
      - true (Acceptable), the text is accepted, no actions done; WARNING: it seems not fired on when mouse moved on windows (e.g. [X]) buttons, toolbar
      - false (Invalid), char is not inserted
      - NIL (Intermediate), fixUp is call and then __hql_OnValidator again executed (I understand this)
   Validator seems executed when: TAB or RETURN pressed and when mouse moved on a window children.
 \param(IN) string, numeric
 \return bool | array | NIL

*/
SIGNAL __hql_OnValidator( cText, nPos ) CLASS hql_abs0090
   LOCAL xReturn

   // when fixup is call, to avoid multiple execution; I need to investigate if is good or not
//   IF ( ::lHqlFixupCalled )
      ::lHqlFixupCalled := .F.
//      RETURN .T.
//   ENDIF

   IF ( ::nHqlCaseLetter == HQL_ALL_UPPERCASE )
      cText := UPPER( cText )
   ELSEIF ( ::nHqlCaseLetter == HQL_ALL_LOWERCASE )
      cText := LOWER( cText )
   ENDIF

   IF ( hb_IsEvalItem( ::bHqlOnValidator ) )
      xReturn := EVAL( ::bHqlOnValidator, cText, nPos )
      RETURN xReturn
   ENDIF

RETURN { cText, nPos, .T. }   // OR .T. this is the default to be returned

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QEditingFinished() CLASS hql_abs0090
   IF ( hb_IsEvalItem( ::bHqlEditingFinished ) )
      EVAL( ::bHqlEditingFinished, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QFocusIn( oEvent ) CLASS hql_abs0090

   IF ( !::__hqlPreValidate() )
      ::lHqlFocusinPreValidFailed := .T.
      oEvent:accept()
      IF ( oEvent:reason() == Qt_BacktabFocusReason .OR. oEvent:reason() == Qt_MouseFocusReason )
         ::__hqlNavigate( HQL_NAVIGATEPREV )
      ELSE
         ::__hqlNavigate( HQL_NAVIGATENEXT )
      ENDIF
      RETURN .T.
   ENDIF

   ::lHqlFocusinPreValidFailed := .F.

RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QFocusOut( oEvent ) CLASS hql_abs0090

   //it seems unuseful: IF oEvent:reason() == Qt_MouseFocusReason .AND. ::hqlMousePostValid() .AND. ! ::__hqlPostValidate()
   // because ONLY when a widget with inputkey receive focus, next code is fired on and focus not moved
   // else (eg button) can be pressed always
   IF ( oEvent:reason() == Qt_MouseFocusReason .AND. !::__hqlPostValidate() )
      oEvent:accept()
      ::__hqlNavigate( HQL_NAVIGATESELF )
      RETURN .T.
   ENDIF

RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QKeypress( oEvent ) CLASS hql_abs0090
   LOCAL nKey := oEvent:key()
   LOCAL nModifiers := oEvent:modifiers()
   LOCAL nX

   // handle functions key
   IF ( ( nX := ::__hqlGetFkeyPos( nKey, nModifiers ) ) > 0 .AND. hb_IsEvalItem( ::aHqlFunctionKey[nX,3] ) )
      oEvent:accept()
      EVAL( ::aHqlFunctionKey[nX,3], Self )  // Self always as last
      RETURN .T.
   ENDIF

   // handling
   SWITCH nKey

   CASE Qt_Key_Return
   CASE Qt_Key_Enter
      IF ( ::hqlReturnAsTab() )
         oEvent:accept()
         ::__hqlNavigate( HQL_NAVIGATENEXT )
         RETURN .T.
      ENDIF

      // if postvalidate fails, set focus on current widget
      IF ( !::__hqlPostValidate() )
         oEvent:accept()
         ::__hqlNavigate( HQL_NAVIGATESELF )
         RETURN .T.
      ENDIF

      // handle returnPressed as usual Qt way: IOW don't stop Harbour event handler
      ::__hql_QReturnPressed()
      EXIT

   CASE Qt_Key_Up
      IF ( ::hqlUpDownAsTab() )  // if enabled sen TAB
         oEvent:accept()
         ::__hqlNavigate( HQL_NAVIGATEPREV )
         RETURN .T.
      ENDIF
      EXIT

   CASE Qt_Key_Down
      IF ( ::hqlUpDownAsTab() )   // if enabled send BACKTAB
         oEvent:accept()
         ::__hqlNavigate( HQL_NAVIGATENEXT )
         RETURN .T.
      ENDIF
      EXIT

   CASE Qt_Key_Tab
   CASE Qt_Key_Backtab
      IF ::lHqlFocusinPreValidFailed  // focusIN prevalidate failed but it send a Qt_Key_Tab OR Qt_Key_Backtab
         ::lHqlFocusinPreValidFailed := .F.
         EXIT
      ENDIF

      // if postvalidate fails, set focus on current widget
      IF ( !::__hqlPostValidate() )
         oEvent:accept()
         ::__hqlNavigate( HQL_NAVIGATESELF )
         RETURN .T.
      ENDIF

      // handle ontab
      ::__hql_KeyTabPressed()
      EXIT

   ENDSWITCH

RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QReturnPressed() CLASS hql_abs0090
   IF ( hb_IsEvalItem(::bHqlReturnPressed) )
      EVAL( ::bHqlReturnPressed, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QtextChanged( string ) CLASS hql_abs0090
   IF ( hb_IsEvalItem(::bHqlTextChanged) )
      EVAL( ::bHqlTextChanged, string, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QtextEdited( string ) CLASS hql_abs0090
   IF ( hb_IsEvalItem(::bHqlTextEdited) )
      EVAL( ::bHqlTextEdited, string, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
