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

 \brief define hql_abs0080 class
   inherited by:
      hql_dial,
      hql_progressBar,
      hql_slider,
      hql_spinBox

*/
CLASS hql_abs0080 INHERIT hql_abs0001

   EXPORTED:
   METHOD hqlOnValueChanged

   PROTECTED:
   VAR bHqlValueChanged                   INIT NIL
   METHOD __hqlValueGet
   METHOD __hqlValueSet
   SIGNAL __hql_QValueChanged

   HIDDEN:

ENDCLASS

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnValueChanged( arg1 ) CLASS hql_abs0080
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlValueChanged := arg1
      IF ( hb_IsEvalItem(::bHqlValueChanged) )
         ::connect( "valueChanged(int)", { |nInt| ::__hql_QValueChanged(nInt) } )
      ELSE
         ::disconnect( "valueChanged(int)" )
      ENDIF
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] get
 \param[in] none
 \return numeric

*/
METHOD __hqlValueGet() CLASS hql_abs0080
RETURN ::value()

/*!

 \brief [PROTECTED] set
 \param[in] bool
 \return NIL

*/
METHOD __hqlValueSet( arg1 ) CLASS hql_abs0080
   IF ( hb_IsNumeric(arg1) )
      ::setValue( arg1 )
   ENDIF
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QValueChanged(nInt) CLASS hql_abs0080
   IF ( hb_IsEvalItem( ::bHqlValueChanged ) )
      EVAL( ::bHqlValueChanged, nInt, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
