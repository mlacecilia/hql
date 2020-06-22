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

 \brief define hql_abs0070 class
   inherited by:
      hql_calendarWidget,
      hql_dateEdit,
      hql_dateTimeEdit,
      hql_timeEdit

*/
CLASS hql_abs0070 INHERIT hql_abs0001

   EXPORTED:
   METHOD hqlOnEditingFinished
   METHOD toLocaleString

   PROTECTED:
   VAR bHqlEditingFinished                INIT NIL
   METHOD __hqlHbToQtDate
   METHOD __hqlHbToQtDateTime
   METHOD __hqlHbToQtTime
   METHOD __hqlQtToHbDate
   METHOD __hqlQtToHbDateTime
   METHOD __hqlQtToHbTime
   METHOD __hqlToLocaleString
   SIGNAL __hql_QEditingFinished

   HIDDEN:

ENDCLASS

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnEditingFinished( arg1 ) CLASS hql_abs0070
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlEditingFinished := arg1
      IF ( hb_IsEvalItem(::bHqlEditingFinished) )
         ::connect( "editingFinished()", { |nInt| ::__hql_QEditingFinished(nInt) } )
      ELSE
         ::disconnect( "editingFinished()" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief convert current value to locale string using formats: QLocale_ShortFormat(default), QLocale_LongFormat, QLocale_NarrowFormat
 \param(IN) numeric
 \return string

*/
METHOD toLocaleString( arg1 ) CLASS hql_abs0070
   LOCAL nFormat
   arg1 := hb_DefaultValue(arg1, QLocale_ShortFormat)
   SWITCH arg1
   CASE QLocale_LongFormat
   CASE QLocale_ShortFormat
   CASE QLocale_NarrowFormat
      nFormat := arg1
      EXIT
   OTHERWISE
      nFormat := QLocale_ShortFormat
      EXIT
   ENDSWITCH
RETURN ( ::__hqlToLocaleString(nFormat) )

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] helper to convert Harbour type "D" to QDate
 \param(IN) hb_Date|hb_DateTime
 \return QDate

*/
METHOD __hqlHbToQtDate( arg1 ) CLASS hql_abs0070
   IF ( hb_IsDate(arg1) .OR. hb_IsDateTime(arg1) )
      RETURN QDate( YEAR(arg1), MONTH(arg1), DAY(arg1) )
   ENDIF
RETURN QDate()

/*!

 \brief [PROTECTED] helper to convert Harbour type "T" to QDateTime
 \param(IN) ...
 \return QDateTime

*/
METHOD __hqlHbToQtDateTime( arg1 ) CLASS hql_abs0070
   LOCAL oQdateTime
   LOCAL nSec
   LOCAL nMsec
   IF ( hb_IsTimeStamp(arg1) )
      nSec := INT(hb_Sec(arg1))
      nMsec := ROUND((hb_Sec(arg1) - nSec) * 1000, 0)
      oQdateTime := QDateTime( QDate(YEAR(arg1), MONTH(arg1), DAY(arg1)), QTime(hb_Hour(arg1), hb_Minute(arg1), nSec, nMsec) )
   ELSE
      oQdateTime := QDateTime()
   ENDIF
RETURN oQdateTime

/*!

 \brief [PROTECTED] helper to convert Harbour dateTime to Qtime; WARNING Cl..pper don't has a real time type
 \param(IN) ...
 \return Qtime

*/
METHOD __hqlHbToQtTime( arg1 ) CLASS hql_abs0070
   LOCAL nSec
   LOCAL nMsec
   LOCAL oQtime
   IF ( hb_IsDateTime(arg1) )
      nSec := INT(hb_Sec(arg1))
      nMsec := ROUND((hb_Sec(arg1) - nSec) * 1000, 0)
      oQtime := QTime(hb_Hour(arg1), hb_Minute(arg1), nSec, nMsec)
   ELSEIF ( hb_IsString(arg1) .AND. LEN(arg1) == LEN(HQL_TIMEQTMASK) )
      oQtime := QTime():fromString(arg1, HQL_TIMEQTMASK)
   ELSEIF ( hb_IsString(arg1) .AND. LEN(arg1) == LEN(HQL_TITOSQTMASK) )
      oQtime := QTime():fromString(arg1, HQL_TITOSQTMASK)
   ELSE
      oQtime := QTime()
   ENDIF
RETURN oQtime

/*!

 \brief [PROTECTED] helper to convert QDate to Harbour type "D"
 \param(IN) ...
 \return hb_Date

*/
METHOD __hqlQtToHbDate( arg1 ) CLASS hql_abs0070
   LOCAL dDate
   IF ( hql_IsDerived(arg1, "QDate") .AND. arg1:isValid() )
      dDate := hb_Date(arg1:year(), arg1:month(), arg1:day() )
   ELSE
      dDate := 0d00000000
   ENDIF
RETURN dDate

/*!

 \brief [PROTECTED] helper to convert QDateTime to Harbour type "T"
 \param(IN) ...
 \return hb_DateTime

*/
METHOD __hqlQtToHbDateTime( arg1 ) CLASS hql_abs0070
   LOCAL tDateTime
   IF ( hql_IsDerived(arg1, "QDateTime") .AND. arg1:isValid() )
      tDateTime := hb_DateTime( arg1:date:year(), ;
                                arg1:date:month(), ;
                                arg1:date:day(), ;
                                arg1:time:hour(), ;
                                arg1:time:minute(), ;
                                arg1:time:second(), ;
                                arg1:time:msec() )
   ELSE
      tDateTime := { ^ 0 / 0 / 0 0:0:0 }  // instead of hb_DateTime( 0, 0, 0, 0, 0, 0, 0 )
   ENDIF
RETURN tDateTime

/*!

 \brief [PROTECTED] helper to convert QTime to Harbour hb_DateTime; WARNING Cl..pper don't has a real time type
 \param(IN) ...
 \return hb_DateTime

*/
METHOD __hqlQtToHbTime( arg1 ) CLASS hql_abs0070
   LOCAL tDateTime
   IF ( hql_IsDerived(arg1, "QTime") .AND. arg1:isValid() )
      tDateTime := hb_DateTime( NIL, ;
                                NIL, ;
                                NIL, ;
                                arg1:hour(), ;
                                arg1:minute(), ;
                                arg1:second(), ;
                                arg1:msec() )
   ELSE
      tDateTime := { ^ 0 / 0 / 0 0:0:0 }  // instead of hb_DateTime( 0, 0, 0, 0, 0, 0, 0 )
   ENDIF
RETURN tDateTime

/*!

 \brief [PROTECTED] helper to convert into locale string using QLocale_ShortFormat, QLocale_LongFormat, QLocale_NarrowFormat
 \param(IN) numeric
 \return string

*/
METHOD __hqlToLocaleString( /*nFormat*/ ) CLASS hql_abs0070
//RETURN ( ::oQrealObject:toString( QLocale():datetimeFormat( nFormat ) ) )
RETURN ""

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QEditingFinished() CLASS hql_abs0070
   IF ( hb_IsEvalItem( ::bHqlEditingFinished ) )
      EVAL( ::bHqlEditingFinished, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
