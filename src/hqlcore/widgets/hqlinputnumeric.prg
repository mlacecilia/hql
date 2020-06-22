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

 \brief Returns a new hql_inputNumeric object instance

*/
FUNCTION hqlInputNumeric( ... )
RETURN hql_inputNumeric():new( ... )

/*!

 \brief define hql_inputNumeric class

*/
CLASS hql_inputNumeric INHERIT hb_QLineEdit, hql_abs0090

   EXPORTED:
   METHOD init
   METHOD hqlAlwaysSigned                 SETGET
   METHOD hqlDecimals                     SETGET
   METHOD hqlDecimalSeparator             INLINE ( hb_Uchar(::nHqlDecUcode) )
   METHOD hqlFormat                       SETGET
   METHOD hqlGroupSeparator               INLINE ( hb_Uchar(::nHqlGrpUcode) )
   METHOD hqlInputMask                    SETGET            // override to handle
   METHOD hqlIntegers                     SETGET
   METHOD hqlThousands                    SETGET

   PROTECTED:
   VAR lHqlAlwaysSigned                   INIT .F.
   VAR nHqlDecimals                       INIT -1     // -1 no limits
   VAR nHqlIntegers                       INIT -1     // -1 no limits
   VAR lHqlThousands                      INIT .F.    // no thousands separated
   VAR nHqlFormat                         INIT 0      // 0=native style, 1=USA
   VAR nHqlDecUcode                       INIT 0      // decimal separator Ucode
   VAR nHqlGrpUcode                       INIT 0      // group separator Ucode
   METHOD __hqlBuildMask
   METHOD __hqlFormatToEdit
   METHOD __hqlFormatToShow
   METHOD __hqlGetInputMask
   METHOD __hqlSetAlignment
   METHOD __hqlSetInputMask
   METHOD __hqlSetSeparators
   METHOD __hqlValueGet                               // override
   METHOD __hqlValueSet                               // override
   SIGNAL __hql_OnFixUp                               // override
   SIGNAL __hql_OnValidator                           // override
   SIGNAL __hql_QFocusIn                              // override
   SIGNAL __hql_QFocusOut                             // override

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_inputNumeric

   ::QLineEdit:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   // install validator
   ::setValidator( HBQValidator( {|cText,nPos| ::__hql_OnValidator( cText, nPos ) }, {|cText| ::__hql_OnFixUp( cText ) } ) )
   ::__hqlSetSeparators(0)  // by default native style
   ::__hqlSetAlignment( Qt_AlignRight )
   ::__hqlValueSet( 0 )

   ::__hqlConnect()

RETURN self

/*!

 \brief set/get always signed flag
 \param(IN) bool
 \return self

*/
METHOD hqlAlwaysSigned( arg1 ) CLASS hql_inputNumeric
   LOCAL nValue
   IF ( hb_IsLogical(arg1) .AND. arg1 != ::lHqlAlwaysSigned )
      nValue := ::__hqlValueGet()
      ::lHqlAlwaysSigned := arg1
      ::__hqlValueSet( nValue )
   ENDIF
RETURN ::lHqlAlwaysSigned

/*!

 \brief set/get decimals number; -1 no limits
 \param(IN) [OPTIONAL] numeric
 \return numeric

*/
METHOD hqlDecimals( arg1 ) CLASS hql_inputNumeric
   LOCAL nValue
   IF ( hb_IsNumeric(arg1) .AND. arg1 != ::nHqlDecimals .AND. arg1 >= -1 ) // -1 no limits
      nValue := ::__hqlValueGet()
      ::nHqlDecimals := arg1
      ::__hqlValueSet( nValue )
   ENDIF
RETURN ::nHqlDecimals

/*!

 \brief set/get always signed flag
 \param(IN) bool
 \return self

*/
METHOD hqlFormat( arg1 ) CLASS hql_inputNumeric
   LOCAL nValue
   IF ( hb_IsNumeric(arg1) .AND. arg1 >= 0 .AND. arg1 <= 1 .AND. arg1 != ::nHqlFormat )
      nValue := ::__hqlValueGet()
      ::__hqlSetSeparators(arg1)
      ::__hqlValueSet( nValue )
   ENDIF
RETURN ::nHqlFormat

/*!

 \brief set/get inputmask
 \param(IN) [OPTIONAL] string
 \return string

*/
METHOD hqlInputMask( arg1 ) CLASS hql_inputNumeric
   LOCAL nValue
   IF ( hb_IsString(arg1) )
      nValue := ::__hqlValueGet()
      ::__hqlSetInputMask(arg1)
      ::__hqlValueSet( nValue )
   ENDIF
RETURN ::__hqlGetInputMask()

/*!

 \brief sets/returns integers number; -1 no limits
 \param(IN) [OPTIONAL] numeric
 \return numeric

*/
METHOD hqlIntegers( arg1 ) CLASS hql_inputNumeric
   LOCAL nValue
   IF ( hb_IsNumeric(arg1) .AND. arg1 != ::nHqlIntegers .AND. arg1 >= -1 ) // -1 no limits
      nValue := ::__hqlValueGet()
      ::nHqlIntegers := IIF( arg1 == 0, 1, arg1 ) // 0 (zero) not admitted
      ::__hqlValueSet( nValue )
   ENDIF
RETURN ::nHqlIntegers

/*!

 \brief sets/returns thousands separator status
 \param(IN) [OPTIONAL] boolean
 \return boolean

*/
METHOD hqlThousands( arg1 ) CLASS hql_inputNumeric
   LOCAL nValue
   IF ( hb_IsLogical(arg1) .AND. arg1 != ::lHqlThousands )
      nValue := ::__hqlValueGet()
      ::lHqlThousands := arg1
      ::__hqlValueSet( nValue )
   ENDIF
RETURN ::lHqlThousands

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] build Cl..pper mask
 \param(IN) bool, numeric, numeric
 \return string

*/
METHOD __hqlBuildMask( lThousands, nIntegers, nDecimals ) CLASS hql_inputNumeric
   LOCAL cMask
   LOCAL nI

   // about intgeres
   IF ( lThousands )
      IF ( nIntegers < 0 )          // see noLimits
         nIntegers := 17
      ELSEIF ( nIntegers == 0 )    // at least one digit
         nIntegers := 1
      ENDIF
      cMask := ""
      FOR nI := 1 TO INT(nIntegers / 3)
         cMask += ",999"
      NEXT nI
      nI := ( nIntegers % 3 )
      IF ( nI > 0 )
         cMask := REPLICATE("9", nI) + cMask
      ENDIF
      IF ( AT(",", cMask) == 1 )
         cMask := SUBSTR(cMask, 2)
      ENDIF
   ELSE
      IF ( nIntegers < 0 )          // see noLimits
         cMask := REPLICATE("9", 17)
      ELSEIF ( nIntegers == 0 )    // at least one digit
         cMask := REPLICATE("9", 1)
      ELSE
         cMask := REPLICATE("9", nIntegers)
      ENDIF
   ENDIF

   // about decimals
   IF ( nDecimals == -1 )  // see noLimits
      cMask += "." + REPLICATE("9", 17)
   ELSEIF ( nDecimals > 0 )
      cMask += "." + REPLICATE("9", nDecimals)
   ENDIF

   IF ( ::nHqlDecUcode == 44 )   // "," instead "."
      cMask := "@E " + cMask
   ENDIF
RETURN cMask

/*!

 \brief [PROTECTED] convert current ::text() for editing
 \param(IN) none
 \return string

*/
METHOD __hqlFormatToEdit() CLASS hql_inputNumeric
   LOCAL cBuffer := ::text()
   LOCAL cSep := ::hqlGroupSeparator()

   // remove group separator
   cBuffer := STRTRAN(cBuffer, cSep, "")
   // remove "+" if exists (see hqlAlwaysSigned)
   cBuffer := STRTRAN(cBuffer, "+", "")
   ::setText( cBuffer )
   ::selectAll()
RETURN .T.

/*!

 \brief [PROTECTED] convert numeric value to string for showing
 \param(IN) numeric value
 \return string

*/
METHOD __hqlFormatToShow( nValue ) CLASS hql_inputNumeric
   LOCAL cString
   LOCAL aSections      // { <integerStringPart>, <decimalStringPart> }
   LOCAL nIntegers
   LOCAL nDecimals
   LOCAL cMask
   LOCAL nI                     // extension to always show sign

   nValue := hb_DefaultValue(nValue, 0)
   cString := hb_NtoS( nValue )  // is numeric so Cl..pper/USA style. Do not use hb_NtoC() too many decimals
   // split into integer and decimal parts
   aSections := hb_Atokens(cString, ".")
   // always two sections
   IF LEN(aSections) == 1 ; AADD(aSections, "") ; ENDIF
   // at least 1 digit for integer
   nIntegers := IIF( LEN(aSections[1]) > 0, LEN(aSections[1]), 1 )
   // if there isn't integers limit it assumes current number of integers digit
   nIntegers := IIF( ::nHqlIntegers <> -1, ::nHqlIntegers, nIntegers )
   // decimals
   nDecimals := LEN( aSections[2] )
   // if there isn't decimals limit it assumes current number of decimals digit
   nDecimals := IIF( ::nHqlDecimals <> -1, ::nHqlDecimals, nDecimals )

   // create a mask (CLIPPER) based upon integers, decimals and thousands separator
   cMask := ::__hqlBuildMask( ::lHqlThousands, nIntegers, nDecimals )

   // now use CL..pper transform
   cString := TRANSFORM( nValue, cMask )

   // extension to add "+"
   IF ( ::lHqlAlwaysSigned .AND. nValue > 0 .AND. (nI := RAT(SPACE(1),cString)) > 0 ) // extension to always show sign
      cString := STUFF(cString, nI, 1, "+")                                        // extension to always show sign
   ENDIF

RETURN ALLTRIM(cString)

/*!

 \brief [PROTECTED] returns Cl..per mask based upon current settings
 \param(IN) none
 \return string

*/
METHOD __hqlGetInputMask() CLASS hql_inputNumeric
RETURN ( ::__hqlBuildMask( ::lHqlThousands, ::nHqlIntegers, ::nHqlDecimals ) )

/*!

 \brief [PROTECTED] to set alignment left or right
 \param(IN) numeric
 \return NIL

*/
METHOD __hqlSetAlignment( nAlignment ) CLASS hql_inputNumeric
   LOCAL nValign
   LOCAL nDir

   nAlignment := hb_DefaultValue(nAlignment, Qt_AlignLeft)
   IF ( nAlignment == Qt_AlignLeft .OR. nAlignment == Qt_AlignRight )
      nValign := hb_BitAnd( ::alignment(), Qt_AlignVertical_Mask )
      nDir := hb_BitAnd( hb_BitAnd( ::alignment(), Qt_AlignHorizontal_Mask ), Qt_AlignAbsolute )
      ::setAlignment( hb_BitOr( nDir, nValign, nAlignment ) )
   ENDIF
RETURN NIL

/*!

 \brief [PROTECTED] handle input mask and set internal values. REMEMBER mask follows Cl..per but only for "9.," chars
 \param(IN) string
 \return NIL

*/
METHOD __hqlSetInputMask( cString ) CLASS hql_inputNumeric
   LOCAL cChar
   LOCAL cMask
   LOCAL aSections      // { <integerStringPart>, <decimalStringPart> }

   cString := ALLTRIM(hb_DefaultValue(cString, ""))
   ::nHqlDecimals := -1  // -1 no limits
   ::nHqlIntegers := -1  // -1 no limits
   ::lHqlThousands := .F.

   IF ( LEN(cString) > 0 )
      cMask := ""
      // validate chars
      FOR EACH cChar IN cString
         IF ( cChar $ "9.," )
            cMask += cChar
         ENDIF
      NEXT

      // group separator requested
      ::lHqlThousands := IIF( AT(",", cMask) > 0, .T., .F. )
      // I need to remove group separator to find out integers and decimals length
      cMask := STRTRAN(cMask, ",", "")
      // split string to find integers and decimals strings
      aSections := hb_Atokens(cMask, ".")
      // always two sections
      IF LEN(aSections) == 1 ; AADD(aSections, "") ; ENDIF
      // a least 1 digit for integer
      ::nHqlIntegers := IIF ( LEN(aSections[1]) < 1, 1, LEN(aSections[1]) )
      // decimals
      ::nHqlDecimals := LEN(aSections[2])
   ENDIF

RETURN NIL

/*!

 \brief [PROTECTED] set separators
 \param(IN) numeric
 \return NIL

*/
METHOD __hqlSetSeparators(arg1) CLASS hql_inputNumeric
   arg1 := hb_DefaultValue(arg1, 0)
   arg1 := IIF( arg1 >= 0 .AND. arg1 <= 1, INT(arg1), 0)
   SWITCH arg1 // 0=native style, 1=USA
   CASE 0
      ::nHqlFormat := 0
      ::nHqlDecUcode := HqlFw:QtLocale():decimalPoint():unicode()
      ::nHqlGrpUcode := HqlFw:QtLocale():groupSeparator():unicode()
      EXIT
   CASE 1
      ::nHqlFormat := 1
      ::nHqlDecUcode := 46 // 0x2E
      ::nHqlGrpUcode := 44 // 0x2C
      EXIT
   ENDSWITCH
RETURN NIL

/*!

 \brief [PROTECTED] default getting value
 \param(IN) none
 \return numeric

*/
METHOD __hqlValueGet() CLASS hql_inputNumeric
   LOCAL cBuffer := ::text()
   LOCAL cGrpSep := ::hqlGroupSeparator()
   LOCAL cDecSep := ::hqlDecimalSeparator()

   cBuffer := STRTRAN( cBuffer, cGrpSep, "" )  // remove group separator
   cBuffer := STRTRAN( cBuffer, "+", "" )   // remove "+"
   IF( ::nHqlDecUcode == 44 ) // I need to swap "," into "." for decimals separator
      cBuffer := STRTRAN(cBuffer, cDecSep, "." )
   ENDIF
RETURN VAL(ALLTRIM(cBuffer))

/*!

 \brief [PROTECTED] default setting value
 \param(IN) numeric
 \return NIL

*/
METHOD __hqlValueSet( arg1 ) CLASS hql_inputNumeric
   LOCAL cBuffer

   IF ( hb_IsNumeric(arg1) )
      cBuffer := ::__hqlFormatToShow(arg1)
      ::setText( cBuffer )
      IF ::lHqlCursorInit
         ::setCursorPosition( 0 )
      ENDIF
   ENDIF
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] handle validator fixUp: READ ACREFULLY src/hqlcore/widgets/hqlabstr0009.prg
 \param(IN) string
 \return string

*/
SIGNAL __hql_OnFixUp( cText ) CLASS hql_inputNumeric

   ::lHqlFixupCalled := .T.

   IF ( LEN(ALLTRIM(cText)) == 0 )
      cText := "0"
   ENDIF

   IF ( hb_IsEvalItem(::bHqlOnFixUp) )
      cText := EVAL( ::bHqlOnFixUp, cText )
   ENDIF

RETURN cText

/*!

 \brief [PROTECTED] handle validator validate: READ ACREFULLY src/hqlcore/widgets/hqlabstr0009.prg
 \param(IN) string, numeric
 \return boolean | array

*/
SIGNAL __hql_OnValidator( cText, nPos ) CLASS hql_inputNumeric
   LOCAL xReturn
   LOCAL nP := nPos
   LOCAL lChanged := .F.
   LOCAL nX
   LOCAL nAt
   LOCAL aSections
   LOCAL cDecSep := ::hqlDecimalSeparator()
   LOCAL cGrpSep := ::hqlGroupSeparator()
   LOCAL cAllowedChars := "0123456789-+"+cDecSep+cGrpSep

   IF ( ::lHqlFixupCalled )
      ::lHqlFixupCalled := .F.
      RETURN .T.
   ENDIF

   IF ( ::nHqlCaseLetter == HQL_ALL_UPPERCASE )
      cText := UPPER( cText )
   ELSEIF ( ::nHqlCaseLetter == HQL_ALL_LOWERCASE )
      cText := LOWER( cText )
   ENDIF

   //----> specified for numeric - START

   // if start with decimal separator, "0" must be prefixed
   IF AT( cDecSep, cText ) == 1
      lChanged := .T.
      cText := "0" + cText
      nP := nPos + 1
   ENDIF

   // if start with sign and next char is decimal separator, "0" must be inserted
   IF ( ( AT( "+", cText ) > 0 .OR. AT( "-", cText ) > 0 ) .AND. AT( cDecSep, cText ) == 2 )
      lChanged := .T.
      cText := SUBSTR( cText, 1, 1 ) + "0" + SUBSTR( cText, 2 )
      nP := nPos + 1
   ENDIF

   // wrong character
   FOR nX := 1 TO LEN( cText )
      IF ( !( SUBSTR( cText, nX, 1 ) $ cAllowedChars ) )
         RETURN .F.
      ENDIF
   NEXT nX

   // avoid group separator
   IF ( AT( cGrpSep, cText ) > 0 )
      RETURN .F.
   ENDIF

   // check double decimal separator
   nAt := AT( cDecSep, cText )
   IF ( nAt > 0 .AND. AT( cDecSep, SUBSTR( cText, nAt+1 ) ) > 0 )
      RETURN .F.
   ENDIF

   // check decimal separator, but nDecimals is 0 (not allowed)
   IF ( AT( cDecSep, cText ) > 0 .AND. ::nHqlDecimals == 0 )
      RETURN .F.
   ENDIF

   // 1) check double "+" and 2) check "+-"
   nAt := AT( "+", cText )
   IF ( nAt > 0 .AND. ( AT("+", SUBSTR(cText, nAt+1)) > 0 .OR. AT("-", cText) > 0 ) )
      RETURN .F.
   ENDIF

   // 1) check double "-" and 2) check "-+"
   nAt := AT( "-", cText )
   IF ( nAt > 0 .AND. ( AT("-", SUBSTR(cText, nAt+1)) > 0 .OR. AT("+", cText) > 0 ) )
      RETURN .F.
   ENDIF

   // sign must be at first pos
   nAt := AT( "-", cText )
   nAt := IIF( nAt == 0, AT( "+", cText ), nAt )
   IF ( nAt > 0 .AND. nAt != 1 )
      RETURN .F.
   ENDIF

   // check amount of integeres and decimals
   aSections := hb_Atokens( cText, cDecSep )
   IF LEN( aSections ) == 1 ; AADD( aSections, "" ) ; ENDIF
   IF ( ::nHqlIntegers != -1 .AND. LEN(aSections[1]) > ::nHqlIntegers )
      RETURN .F.
   ENDIF
   IF ( ::nHqlDecimals != -1 .AND. LEN(aSections[2]) > ::nHqlDecimals )
      RETURN .F.
   ENDIF

   IF lChanged
      nPos := nP
   ENDIF

   //----> specified for numeric - END

   IF ( hb_IsEvalItem( ::bHqlOnValidator ) )
      xReturn := EVAL( ::bHqlOnValidator, cText, nPos )
      RETURN xReturn
   ENDIF

RETURN { cText, nPos, .T. }   // OR .T. this is the default to be returned

/*!

 \brief [PROTECTED] handle event/signal
 \param(IN) ... based on event/signal
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QFocusIn( ... ) CLASS hql_inputNumeric

   IF ::hql_abs0090:__hql_QFocusIn( ... )   // .T. means stop error handler IOW there is an error in this scenario
      RETURN .T.
   ENDIF

   ::__hqlFormatToEdit()

   ::__hqlSetAlignment( Qt_AlignLeft )      // while editing, left aligned

RETURN .F.

/*!

 \brief [PROTECTED] handle event/signal
 \param(IN) ... based on event/signal
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QFocusOut( ... ) CLASS hql_inputNumeric
   LOCAL nValue

   //beforeWAS ::__hqlValueSet( ::__hqlValueGet() )
   nValue := ::__hqlValueGet()
   ::setText( ::__hqlFormatToShow(nValue) )

   ::__hqlSetAlignment( Qt_AlignRight )      // while editing, left aligned

RETURN ::hql_abs0090:__hql_QFocusOut( ... )

// ==================== HIDDEN section ====================
