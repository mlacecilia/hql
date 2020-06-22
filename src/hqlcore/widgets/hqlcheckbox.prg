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

 \brief Returns a new hql_checkBox object instance

*/
FUNCTION hqlCheckBox( ... )
RETURN hql_checkBox():new( ... )

/*!

 \brief define hql_checkBox class

*/
CLASS hql_checkBox INHERIT hb_QCheckBox, hql_abs0040

   EXPORTED:
   METHOD init
   METHOD hqlOnStateChanged

   PROTECTED:
   VAR bHqlStateChanged                  INIT NIL
   METHOD __hqlValueGet
   METHOD __hqlValueSet
   SIGNAL __hql_QStateChanged

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_checkBox

   ::QCheckBox:init( ... )

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
METHOD hqlOnStateChanged( arg1 ) CLASS hql_checkBox
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlStateChanged := arg1
      IF ( hb_IsEvalItem(::bHqlStateChanged) )
         ::connect( "stateChanged(int)", { |nInt| ::__hql_QStateChanged(nInt) } )
      ELSE
         ::disconnect( "stateChanged(int)" )
      ENDIF
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] get
 \param[in] none
 \return numeric

*/
METHOD __hqlValueGet() CLASS hql_checkBox
RETURN ::checkState()

/*!

 \brief [PROTECTED] set
 \param[in] numeric
 \return NIL

*/
METHOD __hqlValueSet(arg1) CLASS hql_checkBox
   IF ( hb_IsNumeric(arg1) )
      ::setCheckState( arg1 )
   ENDIF
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] handle event/signal
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QStateChanged(nInt) CLASS hql_checkBox
   IF ( hb_IsEvalItem( ::bHqlStateChanged ) )
      EVAL( ::bHqlStateChanged, nInt, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
