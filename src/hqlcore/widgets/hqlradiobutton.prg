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

 \brief Returns a new hql_radioButton object instance

*/
FUNCTION hqlRadioButton( ... )
RETURN hql_radioButton():new( ... )

/*!

 \brief define hql_radioButton class

*/
CLASS hql_radioButton INHERIT hb_QRadioButton, hql_abs0040

   EXPORTED:
   METHOD init

   PROTECTED:
   METHOD __hqlValueGet
   METHOD __hqlValueSet

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_radioButton

   ::QRadioButton:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] get
 \param[in] none
 \return numeric

*/
METHOD __hqlValueGet() CLASS hql_radioButton
RETURN ::isChecked()

/*!

 \brief [PROTECTED] set
 \param[in] numeric
 \return NIL

*/
METHOD __hqlValueSet(arg1) CLASS hql_radioButton
   IF ( hb_IsLogical(arg1) )
      ::setChecked( arg1 )
   ENDIF
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
