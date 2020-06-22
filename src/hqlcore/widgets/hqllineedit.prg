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

 \brief Returns a new hql_lineEdit object instance

*/
FUNCTION hqlLineEdit( ... )
RETURN hql_lineEdit():new( ... )

/*!

 \brief define hql_lineEdit class

*/
CLASS hql_lineEdit INHERIT hb_QLineEdit, hql_abs0090

   EXPORTED:
   METHOD init

   PROTECTED:

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_lineEdit

   ::QLineEdit:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   // install validator
   ::setValidator( HBQValidator( {|cText,nPos| ::__hql_OnValidator( cText, nPos ) }, {|cText| ::__hql_OnFixUp( cText ) } ) )

   ::__hqlConnect()

RETURN self

// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
