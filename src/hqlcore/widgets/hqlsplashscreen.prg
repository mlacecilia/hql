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

 \brief Returns a new hql_splashScreen object instance

*/
FUNCTION hqlSplashScreen( ... )
RETURN hql_splashScreen():new( ... )

/*!

 \brief define hql_splashScreen class

*/
CLASS hql_splashScreen INHERIT hb_QSplashScreen, hql_abs0001

   EXPORTED:
   METHOD init
   METHOD hqlActivate
   METHOD hqlShowMessage

   PROTECTED:

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_splashScreen
   LOCAL nFlags

   ::QSplashScreen:init( ... )
   ::setAttribute( Qt_WA_DeleteOnClose )
   // reading Qt docs: ... It may be useful to add the ... if you want to keep it above all the other windows on the desktop
   nFlags := hb_BitOr( ::windowFlags(), Qt_WindowStaysOnTopHint )
   ::setWindowFlags( nFlags )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief window activator
 \param[in] none
 \return Self

*/
METHOD hqlActivate() CLASS hql_splashScreen
   ::show()
   HqlFw:processEvents()
RETURN Self

/*!

 \brief

*/
METHOD hqlShowMessage( ... ) CLASS hql_splashScreen
   ::showMessage( ... )
   HqlFw:processEvents()
RETURN Self

// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
