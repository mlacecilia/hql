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

 \brief Returns a new hql_password object instance
 \param(IN) ...
 \return object

*/
FUNCTION hqlPassword( ... )
RETURN hql_password():new( ... )

/*!

 \brief define hql_password class

*/
CLASS hql_password INHERIT hql_lineEdit

   EXPORTED:
   METHOD init

   PROTECTED:
   VAR nHqlEchoMode                       INIT QLineEdit_Password
   METHOD __hqlCustomize
   METHOD __hqlChangeIcon

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( ... ) CLASS hql_password

   ::hql_lineEdit:init( ... )

   ::__hqlCustomize()

RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED]
 \param(IN)
 \return NIL

*/
METHOD __hqlCustomize() CLASS hql_password
   LOCAL oAction

   ::setEchoMode( ::nHqlEchoMode )

   oAction := hqlAction(/*name*/, self )
   oAction:setIcon( QIcon( ":/hqlres/eyeclose" ) )
   oAction:hqlOnTriggered( { |bool,oself| ::__hqlChangeIcon(bool,oself) } )

   ::addAction( oAction, QLineEdit_TrailingPosition )

RETURN NIL

/*!

 \brief [PROTECTED]
 \param(IN)
 \return NIL

*/
METHOD __hqlChangeIcon(bool,oaction) CLASS hql_password

   IF ( ::nHqlEchoMode == QLineEdit_Password )
      ::nHqlEchoMode := QLineEdit_PasswordEchoOnEdit
      ::setEchoMode( ::nHqlEchoMode )
      oaction:setIcon( QIcon( ":/hqlres/eyeopen" ) )
   ELSE
      ::nHqlEchoMode := QLineEdit_Password
      ::setEchoMode( ::nHqlEchoMode )
      oaction:setIcon( QIcon( ":/hqlres/eyeclose" ) )
   ENDIF

   HB_SYMBOL_UNUSED(bool)
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
