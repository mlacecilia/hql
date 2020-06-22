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

 \brief Returns a new hql_timeEdit object instance

*/
FUNCTION hqlTimeEdit( ... )
RETURN hql_timeEdit():new( ... )

/*!

 \brief define hql_timeEdit class

*/
CLASS hql_timeEdit INHERIT hb_QTimeEdit, hql_abs0070

   EXPORTED:
   METHOD init
   METHOD hqlOnTimeChanged
   METHOD hqlSetCurrent
   METHOD hqlStot
   METHOD hqlTtos

   PROTECTED:
   VAR bHqlTimeChanged                    INIT NIL
   METHOD __hqlToLocaleString
   METHOD __hqlValueGet
   METHOD __hqlValueSet
   SIGNAL __hql_QTimeChanged

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_timeEdit

   ::QTimeEdit:init( ... )

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
METHOD hqlOnTimeChanged( arg1 ) CLASS hql_timeEdit
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlTimeChanged := arg1
      IF ( hb_IsEvalItem(::bHqlTimeChanged) )
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
METHOD hqlSetCurrent() CLASS hql_timeEdit
   ::setTime( QTime():currentTime() )
RETURN self

/*!

 \brief set current date using string
 \param(IN) string
 \return Self

*/
METHOD hqlStot( arg1 ) CLASS hql_timeEdit
   arg1 := hb_DefaultValue( arg1, "" )
   ::setTime( QTime():fromString( arg1, HQL_TITOSQTMASK ) )
RETURN self

/*!

 \brief returns current time as Harbour string
 \param(IN)
 \return string

*/
METHOD hqlTtos() CLASS hql_timeEdit
RETURN ::time:toString( HQL_TITOSQTMASK )

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] helper to convert into locale string using QLocale_ShortFormat, QLocale_LongFormat, QLocale_NarrowFormat
 \param(IN) numeric
 \return string

*/
METHOD __hqlToLocaleString( nFormat ) CLASS hql_timeEdit
   LOCAL cString := ::toString( QLocale():timeFormat( nFormat ) )
RETURN cString

/*!

 \brief [PROTECTED] get
 \param(IN)
 \return hb_Date

*/
METHOD __hqlValueGet() CLASS hql_timeEdit
RETURN ::__hqlQtToHbTime( ::time() )

/*!

 \brief [PROTECTED] set current as: hb_date | QDate | year, month, day
 \param(IN) ...
 \return NIL

*/
METHOD __hqlValueSet( arg1 ) CLASS hql_timeEdit
   IF ( hb_IsDateTime(arg1) .OR. hb_IsString(arg1) )
      ::setTime( ::__hqlHbToQtTime(arg1) )  // handle both hb_DateTime and string because Cl..pper don't has a real time type
   ELSEIF ( hql_IsDerived(arg1, "QTime") )
      ::setTime( arg1 )
   ELSE
      ::setTime( QTime() )   // wrong value: reset
   ENDIF
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QTimeChanged( oTime ) CLASS hql_timeEdit
   IF ( hb_IsEvalItem( ::bHqlTimeChanged ) )
      EVAL( ::bHqlTimeChanged, oTime, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
