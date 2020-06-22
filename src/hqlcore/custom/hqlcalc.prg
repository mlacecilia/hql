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

#define DIGIT_LIMIT  16

/*!

 \brief Returns a new hql_calculator object instance
 \param(IN) ...
 \return object

*/
FUNCTION hqlCalc( ... )
RETURN hql_calculator():new( ... )

/*!

 \brief define hql_calculator class

*/
CLASS hql_calculator INHERIT hql_childDialog

   EXPORTED:
   METHOD init

   PROTECTED:
   VAR nDecimalSeparator                  INIT 0
   VAR sumInMemory                        INIT 0
   VAR sumSoFar                           INIT 0
   VAR factorSoFar                        INIT 0
   VAR waitingForOperand                  INIT .T.
   VAR pendingAdditiveOperator            INIT 0
   VAR pendingMultiplicativeOperator      INIT 0

   VAR pDisplay                           INIT NIL
   METHOD __memClear
   METHOD __memRead
   METHOD __memSet
   METHOD __memAdd

   METHOD __digitClicked
   METHOD __multiplicativeOperatorClicked
   METHOD __unaryOperatorClicked
   METHOD __changeSignClicked
   METHOD __pointClicked
   METHOD __additiveOperatorClicked
   METHOD __equalClicked

   METHOD __clearAll
   METHOD __clear
   METHOD __backspaceClicked

   METHOD __abortOperation
   METHOD __calculate

   SIGNAL __hql_QKeyPress                 // connected by default

   HIDDEN:
   METHOD __h_addDisplay
   METHOD __h_addKeypad
   METHOD __h_addSpecials
   METHOD __h_makeButton
   METHOD __h_makeSpecialButton
   METHOD __h_strToVal
   METHOD __h_valToStr

ENDCLASS

/*!

 \brief Initialize a new object instance
 \param(IN) string, ...
 \return self

*/
METHOD init( ... ) CLASS hql_calculator

   ::hql_childDialog:init( ... )

   ::setWindowTitle( "hqlCalc" )
   ::setLayout( hqlVBoxLayout() )
//   ::layout():setContentsMargins( 0, 0, 0, 0 )

   ::nDecimalSeparator := QLocale():decimalPoint():unicode() // 44 => ,

   ::__h_addDisplay( ::layout() )
   ::__h_addSpecials( ::layout() )
   ::__h_addKeypad( ::layout() )

   ::adjustSize()

RETURN self

// ==================== PROTECTED section ====================

METHOD __calculate( rightOperand, pendingOperator ) CLASS hql_calculator
   SWITCH pendingOperator
   CASE Qt_Key_Plus
      ::sumSoFar += rightOperand
      EXIT
   CASE Qt_Key_Minus
      ::sumSoFar -= rightOperand
      EXIT
   CASE Qt_Key_Asterisk
      ::factorSoFar *= rightOperand
      EXIT
   CASE Qt_Key_Slash
      IF ( rightOperand == 0 )
         RETURN .F.
      ENDIF
      ::factorSoFar /= rightOperand
      EXIT
   ENDSWITCH
RETURN .T.

METHOD __memClear() CLASS hql_calculator
    ::sumInMemory := 0
RETURN NIL

METHOD __memRead() CLASS hql_calculator
   LOCAL oDisplay := hql_ObjectFromId( ::pDisplay )
   oDisplay:setText( ::__h_valToStr( ::sumInMemory ) )
   ::waitingForOperand := .T.
RETURN NIL

METHOD __memSet() CLASS hql_calculator
   LOCAL oDisplay := hql_ObjectFromId( ::pDisplay )
   ::__equalClicked()
   ::sumInMemory := ::__h_strToVal( oDisplay:text() )
RETURN NIL

METHOD __memAdd() CLASS hql_calculator
   LOCAL oDisplay := hql_ObjectFromId( ::pDisplay )
   ::__equalClicked()
   ::sumInMemory += ::__h_strToVal( oDisplay:text() )
RETURN NIL

METHOD __digitClicked( nKey ) CLASS hql_calculator
   LOCAL oDisplay := hql_ObjectFromId( ::pDisplay )
   LOCAL digitValue

   IF ( oDisplay:text() == "0" .AND. nKey == Qt_Key_0 )
      RETURN NIL
   ENDIF

   IF ( ::waitingForOperand )
      oDisplay:clear()
      ::waitingForOperand := .F.
   ENDIF

   SWITCH nKey
   CASE Qt_Key_0
      digitValue := "0"
      EXIT
   CASE Qt_Key_1
      digitValue := "1"
      EXIT
   CASE Qt_Key_2
      digitValue := "2"
      EXIT
   CASE Qt_Key_3
      digitValue := "3"
      EXIT
   CASE Qt_Key_4
      digitValue := "4"
      EXIT
   CASE Qt_Key_5
      digitValue := "5"
      EXIT
   CASE Qt_Key_6
      digitValue := "6"
      EXIT
   CASE Qt_Key_7
      digitValue := "7"
      EXIT
   CASE Qt_Key_8
      digitValue := "8"
      EXIT
   CASE Qt_Key_9
      digitValue := "9"
      EXIT
   OTHERWISE
      digitValue := "0"
   ENDSWITCH

   oDisplay:setText( oDisplay:text() + digitValue )
RETURN NIL

METHOD __multiplicativeOperatorClicked( nKey ) CLASS hql_calculator
   LOCAL oDisplay := hql_ObjectFromId( ::pDisplay )
   LOCAL operand := ::__h_strToVal( oDisplay:text() )

   IF ( ::pendingMultiplicativeOperator != 0 )
      IF ( !::__calculate( operand, ::pendingMultiplicativeOperator ) )
         ::__abortOperation()
         RETURN NIL
      ENDIF
      oDisplay:setText( ::__h_valToStr( ::factorSoFar ) )
   ELSE
      ::factorSoFar := operand
   ENDIF

   ::pendingMultiplicativeOperator := nKey
   ::waitingForOperand := .T.
RETURN NIL

METHOD __unaryOperatorClicked( nKey ) CLASS hql_calculator
   LOCAL oDisplay := hql_ObjectFromId( ::pDisplay )
   LOCAL operand := ::__h_strToVal( oDisplay:text() )
   LOCAL result := 0

   SWITCH nKey
   CASE Qt_Key_S     // Sqrt
      IF ( operand < 0 )
         ::__abortOperation()
         RETURN NIL
      ENDIF
      result := SQRT( operand )
      EXIT

   CASE Qt_Key_P     // Percent (was Power)
      //result := operand ^ 2
      IF ( operand != 0 )
         result := operand * 0.01
      ENDIF
      EXIT

   CASE Qt_Key_R     // Recipr
      IF ( operand == 0 )
         ::__abortOperation()
         RETURN NIL
      ENDIF
      result := 1 / operand
      EXIT

   ENDSWITCH

   oDisplay:setText( ::__h_valToStr( result ) )
   ::waitingForOperand := .T.
RETURN NIL

METHOD __changeSignClicked() CLASS hql_calculator
   LOCAL oDisplay := hql_ObjectFromId( ::pDisplay )
   LOCAL nValue := ::__h_strToVal( oDisplay:text() )
   nValue *= -1
   oDisplay:setText( ::__h_valToStr(nValue) )
RETURN NIL

METHOD __pointClicked() CLASS hql_calculator
   LOCAL oDisplay := hql_ObjectFromId( ::pDisplay )
   IF ( ::waitingForOperand )
      oDisplay:setText( "0" )
   ENDIF

   // any decimal separator already exists: do nothing
   IF ( hb_Ati(",", oDisplay:text()) > 0 .OR. hb_Ati(".", oDisplay:text()) > 0 )
   ELSE
      oDisplay:setText( oDisplay:text() + hb_Uchar(::nDecimalSeparator) )
   ENDIF
   ::waitingForOperand := .F.
RETURN NIL

METHOD __additiveOperatorClicked( nKey ) CLASS hql_calculator
   LOCAL oDisplay := hql_ObjectFromId( ::pDisplay )
   LOCAL operand := ::__h_strToVal( oDisplay:text() )

   IF ( ::pendingMultiplicativeOperator != 0 )
      IF ( !::__calculate( operand, ::pendingMultiplicativeOperator ) )
         ::__abortOperation()
         RETURN NIL
      ENDIF
      oDisplay:setText( ::__h_valToStr( ::factorSoFar ) )
      operand := ::factorSoFar
      ::factorSoFar := 0
      ::pendingMultiplicativeOperator := 0
   ENDIF

   IF ( ::pendingAdditiveOperator != 0 )
      IF ( !::__calculate( operand, ::pendingMultiplicativeOperator ) )
         ::__abortOperation()
         RETURN NIL
      ENDIF
      oDisplay:setText( ::__h_valToStr( ::sumSoFar ) )
   ELSE
      ::sumSoFar := operand
   ENDIF

   ::pendingAdditiveOperator := nKey
   ::waitingForOperand := .T.
RETURN NIL

METHOD __equalClicked() CLASS hql_calculator
   LOCAL oDisplay := hql_ObjectFromId( ::pDisplay )
   LOCAL operand := ::__h_strToVal( oDisplay:text() )

   IF ( ::pendingMultiplicativeOperator != 0 )
      IF ( !::__calculate( operand, ::pendingMultiplicativeOperator ) )
         ::__abortOperation()
         RETURN NIL
      ENDIF
      operand := ::factorSoFar
      ::factorSoFar := 0
      ::pendingMultiplicativeOperator := 0
   ENDIF

   IF ( ::pendingAdditiveOperator != 0 )
      IF ( !::__calculate( operand, ::pendingAdditiveOperator ) )
         ::__abortOperation()
         RETURN NIL
      ENDIF
      ::pendingAdditiveOperator := 0
   ELSE
      ::sumSoFar := operand
   ENDIF

   oDisplay:setText( ::__h_valToStr( ::sumSoFar ) )
   ::sumSoFar := 0
   ::waitingForOperand := .T.
RETURN NIL

METHOD __backspaceClicked() CLASS hql_calculator
   LOCAL oDisplay := hql_ObjectFromId( ::pDisplay )
   LOCAL cString
   IF ( ::waitingForOperand )
      RETURN NIL
   ENDIF

   cString := hb_StrShrink(oDisplay:text(), 1)
   IF ( EMPTY(cString) )
      cString := "0"
      ::waitingForOperand := .T.
   ENDIF
   oDisplay:setText( cString )
RETURN NIL

METHOD __clearAll() CLASS hql_calculator
   LOCAL oDisplay := hql_ObjectFromId( ::pDisplay )
   oDisplay:setText( "0" )
   ::sumSoFar := 0
   ::factorSoFar := 0
   ::pendingAdditiveOperator := 0
   ::pendingMultiplicativeOperator := 0
   ::waitingForOperand := .T.
RETURN NIL

METHOD __clear() CLASS hql_calculator
   LOCAL oDisplay
   IF ( !::waitingForOperand )
      oDisplay := hql_ObjectFromId( ::pDisplay )
      oDisplay:setText( "0" )
      ::waitingForOperand := .T.
   ENDIF
RETURN NIL

METHOD __abortOperation() CLASS hql_calculator
   LOCAL oDisplay := hql_ObjectFromId( ::pDisplay )
   ::__clearAll()
   oDisplay:setText( "####" )
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QKeyPress( oEvent ) CLASS hql_calculator
   LOCAL nKey := oEvent:key()
   LOCAL nModifiers := oEvent:modifiers()
   LOCAL lStop := .F.

   SWITCH nKey
   // digits
   CASE Qt_Key_0
   CASE Qt_Key_1
   CASE Qt_Key_2
   CASE Qt_Key_3
   CASE Qt_Key_4
   CASE Qt_Key_5
   CASE Qt_Key_6
   CASE Qt_Key_7
   CASE Qt_Key_8
   CASE Qt_Key_9
      oEvent:accept()
      ::__digitClicked( nKey )
      lStop := .T.
      EXIT

   //Separator
   CASE Qt_Key_Period
   CASE Qt_Key_Comma
      IF ( (::nDecimalSeparator == 44 .AND. nKey == Qt_Key_Comma) .OR. (::nDecimalSeparator != 44 .AND. nKey == Qt_Key_Period) )
         oEvent:accept()
         ::__pointClicked()
         lStop := .T.
      ENDIF
      EXIT

   CASE Qt_Key_Slash
   CASE Qt_Key_Asterisk
      oEvent:accept()
      ::__multiplicativeOperatorClicked( nKey ) // Multiplicative Operators / *
      lStop := .T.
      EXIT

   CASE Qt_Key_Minus
      oEvent:accept()
      ::__additiveOperatorClicked( nKey ) // Additive Operators -
      lStop := .T.
      EXIT

   CASE Qt_Key_Plus
      IF ( nModifiers == Qt_AltModifier )  // Memory add
         oEvent:accept()
         ::__memAdd()
      ELSE
         oEvent:accept()
         ::__additiveOperatorClicked( nKey ) // Additive Operators +
      ENDIF
      lStop := .T.
      EXIT

   CASE Qt_Key_S
      IF ( nModifiers == Qt_AltModifier )  // Memory set
         oEvent:accept()
         ::__memSet()
      ELSE
         oEvent:accept()
         ::__unaryOperatorClicked( nKey ) // Sqrt
      ENDIF
      lStop := .T.
      EXIT

   CASE Qt_Key_P     // Percent (was Power)
      oEvent:accept()
      ::__unaryOperatorClicked( nKey ) // Unary Operators
      lStop := .T.
      EXIT

   CASE Qt_Key_R
      IF ( nModifiers == Qt_AltModifier )  // Memory read
         oEvent:accept()
         ::__memRead()
      ELSE
         oEvent:accept()
         ::__unaryOperatorClicked( nKey ) // Unary Operators  1/x
      ENDIF
      lStop := .T.
      EXIT

   //Change sign
   CASE Qt_Key_I
      oEvent:accept()
      ::__changeSignClicked()
      lStop := .T.
      EXIT

   //Equal, return
   CASE Qt_Key_Equal
   CASE Qt_Key_Enter
   CASE Qt_Key_Return
      oEvent:accept()
      ::__equalClicked()
      lStop := .T.
      EXIT

   CASE Qt_Key_A
      IF ( nModifiers == Qt_ShiftModifier )  // ClearAll
         oEvent:accept()
         ::__clearAll()
         lStop := .T.
      ENDIF
      EXIT

   CASE Qt_Key_C
      IF ( nModifiers == Qt_ShiftModifier )  // Clear
         oEvent:accept()
         ::__clear()
         lStop := .T.
      ELSEIF ( nModifiers == Qt_AltModifier )  // Memory clear
         oEvent:accept()
         ::__memClear()
         lStop := .T.
      ENDIF
      EXIT

   CASE Qt_Key_Backspace
   CASE Qt_Key_Delete
      oEvent:accept()
      ::__backspaceClicked()
      lStop := .T.

   ENDSWITCH

RETURN lStop

// ==================== HIDDEN section ====================

/*!

 \brief [HIDDEN] add display to main layout
 \param(IN) layout
 \return NIL

*/
METHOD __h_addDisplay( oMlayout ) CLASS hql_calculator
   LOCAL oLayout := QHBoxLayout()
   LOCAL oDisplay

   oMlayout:addLayout( oLayout )

   oDisplay := hqlLineEdit(/*name*/, self )
   ::pDisplay := oDisplay:hqlObjectId()
   oDisplay:setReadOnly( .T. )
   oDisplay:setMaxLength( DIGIT_LIMIT )
   oDisplay:setAlignment( hb_BitOr( Qt_AlignVCenter,Qt_AlignRight ) )
   oDisplay:setText( "0" )

   oLayout:addWidget( oDisplay )

RETURN NIL

/*!

 \brief [HIDDEN] add keypad to main layout
 \param(IN) layout
 \return NIL

   MC  |  7  |  8  |  9  |  /  |  Sqrt                 AltC  |  7  |  8  |  9  |  /  |  S
   MR  |  4  |  5  |  6  |  *  |  x2                   AltR  |  4  |  5  |  6  |  *  |  P
   MS  |  1  |  2  |  3  |  -  |  1/x                  AltS  |  1  |  2  |  3  |  -  |  R
   M+  |  +- |  0  |  .  |  +  |  =                    Alt+  |  I  |  0  |  .  |  +  |  =

*/
METHOD __h_addKeypad( oMlayout ) CLASS hql_calculator
   LOCAL oLayout, oBtn

   // 1 row
   oLayout := QHBoxLayout()
   oMlayout:addLayout( oLayout )
   oLayout:addStretch()
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "MC" )
   oBtn:hqlOnClicked( { || ::__memClear() } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "7" )
   oBtn:hqlOnClicked( { || ::__digitClicked( Qt_Key_7 ) } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "8" )
   oBtn:hqlOnClicked( { || ::__digitClicked( Qt_Key_8 ) } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "9" )
   oBtn:hqlOnClicked( { || ::__digitClicked( Qt_Key_9 ) } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "/" )  // hb_UChar( 0x00F7 )
   oBtn:hqlOnClicked( { || ::__multiplicativeOperatorClicked( Qt_Key_Slash ) } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "Sqrt" )
   oBtn:hqlOnClicked( { || ::__unaryOperatorClicked( Qt_Key_S ) } )
   oLayout:addStretch()

   // 2 row
   oLayout := QHBoxLayout()
   oMlayout:addLayout( oLayout )
   oLayout:addStretch()
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "MR" )
   oBtn:hqlOnClicked( { || ::__memRead() } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "4" )
   oBtn:hqlOnClicked( { || ::__digitClicked( Qt_Key_4 ) } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "5" )
   oBtn:hqlOnClicked( { || ::__digitClicked( Qt_Key_5 ) } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "6" )
   oBtn:hqlOnClicked( { || ::__digitClicked( Qt_Key_6 ) } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "X" )
   oBtn:hqlOnClicked( { || ::__multiplicativeOperatorClicked( Qt_Key_Asterisk ) } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "%" )  // was oBtn:setText( "x2" )
   oBtn:hqlOnClicked( { || ::__unaryOperatorClicked( Qt_Key_P ) } )
   oLayout:addStretch()

   // 3 row
   oLayout := QHBoxLayout()
   oMlayout:addLayout( oLayout )
   oLayout:addStretch()
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "MS" )
   oBtn:hqlOnClicked( { || ::__memSet() } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "1" )
   oBtn:hqlOnClicked( { || ::__digitClicked( Qt_Key_1 ) } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "2" )
   oBtn:hqlOnClicked( { || ::__digitClicked( Qt_Key_2 ) } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "3" )
   oBtn:hqlOnClicked( { || ::__digitClicked( Qt_Key_3 ) } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "-" )
   oBtn:hqlOnClicked( { || ::__additiveOperatorClicked( Qt_Key_Minus ) } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "1/x" )
   oBtn:hqlOnClicked( { || ::__unaryOperatorClicked( Qt_Key_R ) } )
   oLayout:addStretch()

   // 4 row
   oLayout := QHBoxLayout()
   oMlayout:addLayout( oLayout )
   oLayout:addStretch()
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "M+" )
   oBtn:hqlOnClicked( { || ::__memAdd() } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "+-" )
   oBtn:hqlOnClicked( { || ::__changeSignClicked() } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "0" )
   oBtn:hqlOnClicked( { || ::__digitClicked( Qt_Key_0 ) } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( hb_UChar( ::nDecimalSeparator ) )
   oBtn:hqlOnClicked( { || ::__pointClicked() } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "+" )
   oBtn:hqlOnClicked( { || ::__additiveOperatorClicked( Qt_Key_Plus ) } )
   oBtn := ::__h_makeButton( oLayout )
   oBtn:setText( "=" )
   oBtn:hqlOnClicked( { || ::__equalClicked() } )
   oLayout:addStretch()

RETURN NIL

/*!

 \brief [HIDDEN] add specials button
 \param(IN) layout
 \return NIL

   ClearAll  |  Clear  |  Del                 ShiftA  |  ShiftC  |  ShiftD

*/
METHOD __h_addSpecials( oMlayout ) CLASS hql_calculator
   LOCAL oLayout, oBtn

   oLayout := QHBoxLayout()
   oMlayout:addLayout( oLayout )
   oBtn := ::__h_makeSpecialButton( oLayout )
   oBtn:setText( "ClearAll" )
   oBtn:hqlOnClicked( { || ::__clearAll() } )
   oBtn := ::__h_makeSpecialButton( oLayout )
   oBtn:setText( "Clear" )
   oBtn:hqlOnClicked( { || ::__clear() } )
   oBtn := ::__h_makeSpecialButton( oLayout )
   oBtn:setText( "Backspace" )
   oBtn:hqlOnClicked( { || ::__backspaceClicked() } )

RETURN NIL

/*!

 \brief [HIDDEN] helper to make button
 \param(IN) layout
 \return button

*/
METHOD __h_makeButton( oMlayout ) CLASS hql_calculator
   LOCAL oBtn
   oBtn := hqlPushButton(/*name*/, self )
   oBtn:setMinimumSize( 65, 40 )
   oBtn:setMaximumSize( 65, 40 )
   oBtn:setAutoDefault( .F. )    // This property's default is true for buttons that have a QDialog parent.
   oMlayout:addWidget( oBtn )
RETURN oBtn

/*!

 \brief [HIDDEN] helper to make "specials" button
 \param(IN) layout
 \return button

*/
METHOD __h_makeSpecialButton( oMlayout ) CLASS hql_calculator
   LOCAL oBtn
   oBtn := hqlPushButton(/*name*/, self )
   oBtn:setSizePolicy( QSizePolicy_Expanding, QSizePolicy_Preferred )
   oBtn:setMinimumHeight( 40 )
   oBtn:setAutoDefault( .F. )    // This property's default is true for buttons that have a QDialog parent.
   oMlayout:addWidget( oBtn )
RETURN oBtn

METHOD __h_strToVal( cString ) CLASS hql_calculator
   cString := hb_DefaultValue(cString, "")
   cString := STRTRAN(cString, ",", ".", 1, /*nCount*/)
RETURN ( VAL(cString) )

METHOD __h_valToStr( nNumber, nLimit ) CLASS hql_calculator
   LOCAL cString, aTokens
   nNumber := hb_DefaultValue(nNumber, 0)
   nLimit := hb_DefaultValue(nLimit, DIGIT_LIMIT)

   aTokens := hb_Atokens(hb_NtoC(nNumber), ".")

   cString := aTokens[1]
   IF ( LEN(aTokens) == 2 )
      // remove trailing "0"
      WHILE ( RIGHT(aTokens[2], 1) == "0" )
         aTokens[2] := hb_StrShrink(aTokens[2], 1)
      END
      cString += "." + aTokens[2]
   ENDIF

   cString := SUBSTR(cString, 1, nLimit)

   IF ( RIGHT(cString, 1) == "." )
      cString := hb_StrShrink(cString, 1)
   ENDIF

   IF ( EMPTY(cString) )
      cString := "0"
   ENDIF

   IF ( ::nDecimalSeparator == 44 )
      cString := STRTRAN(cString, ".", ",", 1, /*nCount*/)
   ENDIF

RETURN cString
