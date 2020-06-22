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

 \brief Returns a new hql_inputDate object instance

*/
FUNCTION hqlInputDate( ... )
RETURN hql_inputDate():new( ... )

/*!

 \brief define hql_inputDate class

*/
CLASS hql_inputDate INHERIT hb_QLineEdit, hql_abs0090

   EXPORTED:
   METHOD init
   METHOD hqlInputMask                    SETGET            // override to handle
   METHOD date                                              // to mimic dateEdit
   METHOD setDate                                           // to mimic dateEdit

   PROTECTED:
   VAR cHqlEditMask                       INIT ""           //real mask for editing
   VAR cHqlInputMask                      INIT ""           // input mask
   METHOD __hqlIsValidDate
   METHOD __hqlPostValidate                                 // override
   METHOD __hqlSetEditMask
   METHOD __hqlSetInputMask
   METHOD __hqlValueGet                                     // override
   METHOD __hqlValueSet                                     // override

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_inputDate

   ::QLineEdit:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   // install validator
   ::setValidator( HBQValidator( {|cText,nPos| ::__hql_OnValidator( cText, nPos ) }, {|cText| ::__hql_OnFixUp( cText ) } ) )
   ::cHqlInputMask := hqlFw:QtLocale:dateFormat( QLocale_ShortFormat )
   ::__hqlSetEditMask()

   ::__hqlConnect()

RETURN self

/*!

 \brief set/get inputmask
 \param(IN) [OPTIONAL] string
 \return string

*/
METHOD hqlInputMask( arg1 ) CLASS hql_inputDate
   LOCAL dDate
   IF ( hb_IsString(arg1) )
      dDate := ::__hqlValueGet()
      ::__hqlSetInputMask(arg1)
      ::__hqlSetEditMask()
      ::__hqlValueSet( dDate )
   ENDIF
RETURN ::cHqlInputMask

/*!

 \brief returns current value as QDate
 \param(IN)
 \return QDate

*/
METHOD date() CLASS hql_inputDate
RETURN ( QDate():fromString(::text(), ::cHqlInputmask) )

/*!

 \brief set current value as QDate
 \param(IN) QDate
 \return self

*/
METHOD setDate( arg1 ) CLASS hql_inputDate
   IF ( hql_IsDerived(arg1, "Qdate") )
      ::setText( arg1:toString(::cHqlInputmask) )
      ::setCursorPosition( 0 )
   ENDIF
RETURN self

// ==================== PROTECTED section ====================
/*!

 \brief [PROTECTED]

*/
METHOD __hqlIsValidDate() CLASS hql_inputDate
   LOCAL cChr
   LOCAL lNullDate
   LOCAL lValidDate

   // check if is a null date: CLIPPER accept null date as space
   lNullDate := .T.
   FOR EACH cChr IN ::text()
      IF ( hb_AsciiIsDigit(cChr) .AND. !( cChr == CHR(48)) )
         lNullDate := .F.
         EXIT
      ENDIF
   NEXT

   // null date acceptable by CL..pper
   IF lNullDate
      lValidDate := .T.
   ELSE
      // now check if it is a valid date; it repeats for every digit
      lValidDate := .T.
      FOR EACH cChr IN ::text()
         IF ( hb_AsciiIsDigit(cChr) .AND. ::__hqlValueGet() == 0d00000000 )
            lValidDate := .F.
         ENDIF
      NEXT
   ENDIF

RETURN lValidDate

/*!

 \brief [PROTECTED] perform post validate
 \param(IN)
 \return boolean

*/
METHOD __hqlPostValidate() CLASS hql_inputDate
   IF ( ::__hqlIsValidDate() )
      RETURN ::hql_abs0090:__hqlPostValidate()
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] set real Qt mask for editing
 \param(IN)
 \return NIL

*/
METHOD __hqlSetEditMask() CLASS hql_inputDate

   // build a QT inputMask for input numeric values. Following CL..pper date can be empty, for this reason "0" used
   ::cHqlEditMask := STRTRAN(::cHqlInputMask, "M", "0")
   ::cHqlEditMask := STRTRAN(::cHqlEditMask, "d", "0")
   ::cHqlEditMask := STRTRAN(::cHqlEditMask, "y", "0")

   ::setInputMask( ::cHqlEditMask )
RETURN NIL

/*!

 \brief [PROTECTED] handle input mask and set internal values
 \param(IN) string
 \return NIL

*/
METHOD __hqlSetInputMask( cString ) CLASS hql_inputDate
   LOCAL cAllowedChars := "ymd/.- :"
   LOCAL cChar
   LOCAL lValidMask := .T.
   LOCAL nI
   cString := LOWER(hb_DefaultValue(cString, ""))
   FOR EACH cChar IN cString
      IF ( !(cChar $ cAllowedChars) )
         lValidMask := .F.
         EXIT
      ENDIF
   NEXT
   IF ( lValidMask ) // check ripetude chars
      IF ( (nI := hb_At("yyyy", cString)) > 0 .AND. hb_At("y", cString, nI+4) > 0 )
         lValidMask := .F.
      ELSEIF ( hb_At("yyyy", cString) == 0 .AND. (nI := hb_At("yy", cString)) > 0 .AND. hb_At("y", cString, nI+2) > 0 )
         lValidMask := .F.
      ELSEIF ( (nI := hb_At("mm", cString)) > 0 .AND. hb_At("m", cString, nI+2) > 0 )
         lValidMask := .F.
      ELSEIF ( (nI := hb_At("dd", cString)) > 0 .AND. hb_At("d", cString, nI+2) > 0 )
         lValidMask := .F.
      ENDIF
   ENDIF
   IF ( lValidMask ) // ["yyyy" | "yy"] and "mm" and "dd" required to be a good mask
      IF !( (hb_At("yyyy", cString) > 0 .OR. hb_At("yy", cString) > 0) .AND. hb_At("mm", cString) > 0 .AND. hb_At("dd", cString) > 0 )
         lValidMask := .F.
      ENDIF
   ENDIF

   IF ( lValidMask )
      // QT inputMask need d, M and y as defined inputmask characters
      ::cHqlInputMask := STRTRAN(cString, "m", "M" )
   ENDIF
RETURN NIL

/*!

 \brief [PROTECTED] default getting value
 \param(IN)
 \return hb_Date

*/
METHOD __hqlValueGet() CLASS hql_inputDate
   LOCAL oDate := QDate():fromString(::text(), ::cHqlInputmask)
   LOCAL dDate := hb_Date( oDate:year(),  oDate:month(), oDate:day() )
RETURN dDate

/*!

 \brief [PROTECTED] default setting value
 \param(IN) ..
 \return NIL

*/
METHOD __hqlValueSet( arg1 ) CLASS hql_inputDate
   LOCAL oDate
   IF ( hql_IsDerived(arg1, "QDate") )
      ::setText( arg1:toString(::cHqlInputmask) )
      IF ::lHqlCursorInit
         ::setCursorPosition( 0 )
      ENDIF
   ELSEIF ( hb_IsDate(arg1) .OR. hb_IsDateTime(arg1) )
      oDate := QDate( YEAR(arg1), MONTH(arg1), DAY(arg1) )
      IF ( oDate:isValid() )
         ::setText( oDate:toString(::cHqlInputmask) )
         IF ::lHqlCursorInit
            ::setCursorPosition( 0 )
         ENDIF
      ELSE
         ::setText( STRTRAN( ::cHqlEditMask, "0", SPACE(1) ) )
         ::setCursorPosition( 0 )
      ENDIF
   ENDIF
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
