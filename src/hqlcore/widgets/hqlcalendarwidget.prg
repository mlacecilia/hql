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

 \brief Returns a new hql_calendarWidget object instance

*/
FUNCTION hqlCalendarWidget( ... )
RETURN hql_calendarWidget():new( ... )

/*!

 \brief define hql_calendarWidget class

*/
CLASS hql_calendarWidget INHERIT hb_QCalendarWidget, hql_abs0070

   EXPORTED:
   METHOD init
   METHOD hqlOnClicked
   METHOD hqlOnSelectionChanged
   METHOD hqlSetCurrent

   PROTECTED:
   VAR bHqlClicked                        INIT NIL
   VAR bHqlSelectionChanged               INIT NIL
   METHOD __hqlValueGet
   METHOD __hqlValueSet
   SIGNAL __hql_QClicked
   SIGNAL __hql_QSelectionChanged

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_calendarWidget

   ::QCalendarWidget:init( ... )

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
METHOD hqlOnClicked( arg1 ) CLASS hql_calendarWidget
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlClicked := arg1
      IF ( hb_IsEvalItem(::bHqlClicked) )
         ::connect( "clicked(QDate)", { |oDate| ::__hql_QClicked(oDate) } )
      ELSE
         ::disconnect( "clicked(QDate)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnSelectionChanged( arg1 ) CLASS hql_calendarWidget
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlSelectionChanged := arg1
      IF ( hb_IsEvalItem(::bHqlSelectionChanged) )
         ::connect( "selectionChanged()", { || ::__hql_QSelectionChanged() } )
      ELSE
         ::disconnect( "selectionChanged()" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief sets OS date as current
 \param(IN)
 \return Self

*/
METHOD hqlSetCurrent() CLASS hql_calendarWidget
   ::setSelectedDate( QDate():currentDate() )
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] get
 \param(IN)
 \return hb_Date

*/
METHOD __hqlValueGet() CLASS hql_calendarWidget
RETURN ::__hqlQtToHbDate( ::selectedDate() )

/*!

 \brief [PROTECTED] set current as: hb_date | QDate
 \param(IN) ...
 \return NIL

*/
METHOD __hqlValueSet( arg1 ) CLASS hql_calendarWidget
   IF ( hb_IsDate(arg1) )
      ::setSelectedDate( ::__hqlHbToQtDate( arg1 ) )
   ELSEIF ( hql_IsDerived(arg1, "QDate") )
      ::setSelectedDate(arg1)
   ELSE
      ::setSelectedDate( QDate() )   // wrong value: reset
   ENDIF
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QClicked( oDate ) CLASS hql_calendarWidget
   IF ( hb_IsEvalItem( ::bHqlClicked ) )
      EVAL( ::bHqlClicked, oDate, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QSelectionChanged() CLASS hql_calendarWidget
   IF ( hb_IsEvalItem( ::bHqlSelectionChanged ) )
      EVAL( ::bHqlSelectionChanged, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
