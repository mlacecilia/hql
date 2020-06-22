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

 \brief Returns a new hql_dateEdit object instance

*/
FUNCTION hqlDateEdit( ... )
RETURN hql_dateEdit():new( ... )

/*!

 \brief define hql_dateEdit class

*/
CLASS hql_dateEdit INHERIT hb_QDateEdit, hql_abs0070

   EXPORTED:
   METHOD init
   METHOD hqlDtoS
   METHOD hqlOnDateChanged
   METHOD hqlSetCurrent
   METHOD hqlStoD

   PROTECTED:
   VAR bHqlDateChanged                    INIT NIL
   METHOD __hqlToLocaleString
   METHOD __hqlValueGet
   METHOD __hqlValueSet
   SIGNAL __hql_QDateChanged

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_dateEdit

   ::QDateEdit:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief returns date formatted as DTOS clipper
 \param(IN)
 \return string

*/
METHOD hqlDtoS() CLASS hql_dateEdit
RETURN ::date:toString( HQL_DTOSQTMASK )

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnDateChanged( arg1 ) CLASS hql_dateEdit
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

 \brief sets OS date as current
 \param(IN)
 \return Self

*/
METHOD hqlSetCurrent() CLASS hql_dateEdit
   ::setDate( QDate():currentDate() )
RETURN self

/*!

 \brief set current date using string
 \param(IN) string
 \return Self

*/
METHOD hqlStoD( arg1 ) CLASS hql_dateEdit
   arg1 := hb_DefaultValue( arg1, "" )
   ::setDate( QDate():fromString( arg1, HQL_DTOSQTMASK ) )
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] helper to convert into locale string using QLocale_ShortFormat, QLocale_LongFormat, QLocale_NarrowFormat
 \param(IN) numeric
 \return string

*/
METHOD __hqlToLocaleString( nFormat ) CLASS hql_dateEdit
   LOCAL cString := ::toString( QLocale():dateFormat( nFormat ) )
RETURN cString

/*!

 \brief [PROTECTED] get
 \param(IN)
 \return hb_Date

*/
METHOD __hqlValueGet() CLASS hql_dateEdit
RETURN ::__hqlQtToHbDate( ::date() )

/*!

 \brief [PROTECTED] set current as: hb_date | QDate
 \param(IN) ...
 \return NIL

*/
METHOD __hqlValueSet( arg1 ) CLASS hql_dateEdit
   IF ( hb_IsDate(arg1) .OR. hb_IsDateTime(arg1) )
      ::setDate( ::__hqlHbToQtDate(arg1) )
   ELSEIF ( hql_IsDerived(arg1, "QDate") )
      ::setDate( arg1 )
   ELSE
      ::setDate( QDate() )   // wrong value: reset
   ENDIF

RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QDateChanged( oDate ) CLASS hql_dateEdit
   IF ( hb_IsEvalItem( ::bHqlDateChanged ) )
      EVAL( ::bHqlDateChanged, oDate, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
