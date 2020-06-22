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

 \brief Returns a new hql_dialogButtonBox object instance

*/
FUNCTION hqlDialogButtonBox( ... )
RETURN hql_dialogButtonBox():new( ... )

/*!

 \brief define hql_dialogButtonBox class

*/
CLASS hql_dialogButtonBox INHERIT hb_QDialogButtonBox, hql_abs0001

   EXPORTED:
   METHOD init
   METHOD hqlNewPushButton
   METHOD hqlOnAccepted
   METHOD hqlOnHelpRequested
   METHOD hqlOnRejected

   PROTECTED:
   VAR bHqlAccepted                       INIT NIL
   VAR bHqlHelpRequested                  INIT NIL
   VAR bHqlRejected                       INIT NIL
   SIGNAL __hql_QAccepted
   SIGNAL __hql_QHelpRequested
   SIGNAL __hql_QRejected

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_dialogButtonBox

   ::QDialogButtonBox:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief helper to add a new pushButton with given name and Qt button role
 \param(IN) string, numeric name
 \return Self

*/
METHOD hqlNewPushButton( cName, nRole ) CLASS hql_dialogButtonBox
   LOCAL oButton

   cName := hb_DefaultValue( cName, "" )
   nRole := hb_DefaultValue( nRole, QDialogButtonBox_ActionRole )

   oButton := hqlPushButton( cName, Self )   // self as parent

   ::addButton( oButton, nRole )

RETURN oButton

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnAccepted( arg1 ) CLASS hql_dialogButtonBox
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlAccepted := arg1
      IF ( hb_IsEvalItem(::bHqlAccepted) )
         ::connect( "accepted()" , { || ::__hql_QAccepted() } )
      ELSE
         ::disconnect( "accepted()" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnHelpRequested( arg1 ) CLASS hql_dialogButtonBox
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlHelpRequested := arg1
      IF ( hb_IsEvalItem(::bHqlHelpRequested) )
         ::connect( "helpRequested()" , { || ::__hql_QHelpRequested() } )
      ELSE
         ::disconnect( "helpRequested()" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnRejected( arg1 ) CLASS hql_dialogButtonBox
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlRejected := arg1
      IF ( hb_IsEvalItem(::bHqlRejected) )
         ::connect( "rejected()" , { || ::__hql_QRejected() } )
      ELSE
         ::disconnect( "rejected()" )
      ENDIF
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QAccepted() CLASS hql_dialogButtonBox
   IF ( hb_IsEvalItem( ::bHqlAccepted ) )
      EVAL( ::bHqlAccepted, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QHelpRequested() CLASS hql_dialogButtonBox
   IF ( hb_IsEvalItem( ::bHqlHelpRequested ) )
      EVAL( ::bHqlHelpRequested, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QRejected() CLASS hql_dialogButtonBox
   IF ( hb_IsEvalItem( ::bHqlRejected ) )
      EVAL( ::bHqlRejected, Self )   // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
