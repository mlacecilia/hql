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

 \brief Returns a new hql_dateTimeEdit object instance

*/
FUNCTION hqlDateTimeEdit( ... )
RETURN hql_dateTimeEdit():new( ... )

/*!

 \brief define hql_dateTimeEdit class

*/
CLASS hql_dateTimeEdit INHERIT hb_QDateTimeEdit, hql_abs0070

   EXPORTED:
   METHOD init
   METHOD hqlOnDateChanged
   METHOD hqlOnDateTimeChanged
   METHOD hqlOnTimeChanged
   METHOD hqlSetCurrent
   METHOD hqlStoT
   METHOD hqlTtoS

   PROTECTED:
   VAR bHqlDateChanged                    INIT NIL
   VAR bHqlDateTimeChanged                INIT NIL
   VAR bHqlTimeChanged                    INIT NIL
   METHOD __hqlToLocaleString
   METHOD __hqlValueGet
   METHOD __hqlValueSet
   SIGNAL __hql_QDateChanged
   SIGNAL __hql_QDateTimeChanged
   SIGNAL __hql_QTimeChanged

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_dateTimeEdit

   ::QDateTimeEdit:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnDateChanged( arg1 ) CLASS hql_dateTimeEdit
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlDateChanged := arg1
      IF ( hb_IsEvalItem(::bHqlDateChanged) )
         ::connect( "dateChanged(QDate)", { |oDate| ::__hql_QDateChanged(oDate) } )
      ELSE
         ::disconnect( "dateChanged(QDate)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnDateTimeChanged( arg1 ) CLASS hql_dateTimeEdit
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlDateTimeChanged := arg1
      IF ( hb_IsEvalItem(::bHqlDateChanged) )
         ::connect( "dateTimeChanged(QDateTime)", { |oDateTime| ::__hql_QDateTimeChanged(oDateTime) } )
      ELSE
         ::disconnect( "dateTimeChanged(QDateTime)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnTimeChanged( arg1 ) CLASS hql_dateTimeEdit
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlTimeChanged := arg1
      IF ( hb_IsEvalItem(::bHqlDateChanged) )
         ::connect( "timeChanged(QTime)", { |oTime| ::__hql_QTimeChanged(oTime) } )
      ELSE
         ::disconnect( "timeChanged(QTime)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief sets OS date as current
 \param(IN)
 \return Self

*/
METHOD hqlSetCurrent() CLASS hql_dateTimeEdit
   ::setDateTime( QDateTime():currentDateTime() )
RETURN self

/*!

 \brief set current datetime using string
 \param(IN) string
 \return Self

*/
METHOD hqlStoT( arg1 ) CLASS hql_dateTimeEdit
   arg1 := hb_DefaultValue( arg1, "" )
   ::setDateTime( QDateTime():fromString( arg1, HQL_TTOSQTMASK ) )
RETURN self

/*!

 \brief returns date formatted as DTOS clipper
 \param[in] none
 \return string

*/
METHOD hqlTtoS() CLASS hql_dateTimeEdit
RETURN ::dateTime:toString( HQL_TTOSQTMASK )

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] helper to convert into locale string using QLocale_ShortFormat, QLocale_LongFormat, QLocale_NarrowFormat
 \param(IN) numeric
 \return string

*/
METHOD __hqlToLocaleString( nFormat ) CLASS hql_dateTimeEdit
   LOCAL cString := ::toString( QLocale():dateTimeFormat( nFormat ) )
RETURN cString

/*!

 \brief [PROTECTED] get
 \param(IN)
 \return hb_Date

*/
METHOD __hqlValueGet() CLASS hql_dateTimeEdit
RETURN ::__hqlQtToHbDateTime( ::dateTime() )

/*!

 \brief [PROTECTED] set current as: hb_date | QDate | year, month, day, hour, minute, second, [msec]
 \param(IN) ...
 \return NIL

*/
METHOD __hqlValueSet( arg1 ) CLASS hql_dateTimeEdit
   IF ( hb_IsTimeStamp(arg1) )
      ::setDateTime( ::__hqlHbToQtDateTime(arg1) )
   ELSEIF ( hql_IsDerived(arg1, "QDateTime") )
      ::setDateTime( arg1 )
   ELSE
      ::setDate( QDateTime() )   // wrong value: reset
   ENDIF

RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QDateChanged( oDate ) CLASS hql_dateTimeEdit
   IF ( hb_IsEvalItem( ::bHqlDateChanged ) )
      EVAL( ::bHqlDateChanged, oDate, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QDateTimeChanged( oDateTime ) CLASS hql_dateTimeEdit
   IF ( hb_IsEvalItem( ::bHqlDateTimeChanged ) )
      EVAL( ::bHqlDateTimeChanged, oDateTime, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QTimeChanged( oTime ) CLASS hql_dateTimeEdit
   IF ( hb_IsEvalItem( ::bHqlTimeChanged ) )
      EVAL( ::bHqlTimeChanged, oTime, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
