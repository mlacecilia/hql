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

 \brief Returns a new hql_childDialog object instance

*/
FUNCTION hqlChildDialog( ... )
RETURN hql_childDialog():new( ... )

/*!

 \brief define hql_childDialog class

*/
CLASS hql_childDialog INHERIT hb_QDialog, hql_abs0200

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
METHOD init( cName, ... ) CLASS hql_childDialog

   ::QDialog:init( ... )
   ::setAttribute( Qt_WA_DeleteOnClose )
   ::setModal( .F. ) //MOST IMPORTANT

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF
   IF ( !::__hqlHasParent() )
      hqlThrow( hqlErrorNew( 7007, PROCNAME() ) )
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
