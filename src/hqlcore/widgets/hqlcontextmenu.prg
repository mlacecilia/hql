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

 \brief Returns a new hql_contextMenu object instance

*/
FUNCTION hqlContextMenu( ... )
RETURN hql_contextMenu():new( ... )

/*!

 \brief define hql_contextMenu class

*/
CLASS hql_contextMenu INHERIT hb_QMenu, hql_abs0050

   EXPORTED:
   METHOD init
   METHOD hqlOnBeforeShow

   PROTECTED:
   VAR bHqlOnBeforeShow                   INIT NIL
   METHOD __hqlConnect
   METHOD __hql_showMenu
   METHOD __hql_OnBeforeShow

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, oParent ) CLASS hql_contextMenu

   IF ( !hql_IsQobjectDerived( oParent ) )
      oParent := ::__hqlNestedWithGetParent()
   ENDIF

   ::QMenu:init( oParent )

   IF ( !::__hqlHasParent() )
      HqlThrow( hqlErrorNew( 7007, PROCNAME() ) )
   ELSE
      ::parent:setContextMenuPolicy( Qt_CustomContextMenu )
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnBeforeShow( arg1 ) CLASS hql_contextMenu
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlOnBeforeShow := arg1
   ENDIF
RETURN ::bHqlOnBeforeShow

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] returns true if connected else false
 \param(IN) none
 \return boolean

*/
METHOD __hqlConnect() CLASS hql_contextMenu
   ::hql_abs0050:__hqlConnect()
   IF ( ::hqlHasParent() ) // signal connected to parent (if it has)
      ::parent:connect( "customContextMenuRequested(QPoint)", { |oPoint| ::__hql_showMenu( Self:parent:mapToGlobal(oPoint) ) } )
   ENDIF
RETURN .T.

/*!

 \brief [PROTECTED] signal/event handler
 \param(IN) ...
 \return bool

*/
METHOD __hql_OnBeforeShow( oQpoint ) CLASS hql_contextMenu
   LOCAL lReturn
   IF ( hb_IsEvalItem(::bHqlOnBeforeShow) )
      lReturn := EVAL( ::bHqlOnBeforeShow, oQPoint, Self )  // Self always as last
      lReturn := IIF( hb_IsLogical( lReturn ), lReturn, .T. )
   ELSE
      lReturn := .T.
   ENDIF
RETURN lReturn

/*!

 \brief [PROTECTED] handle event/signal
 \param(IN) ... based on event/signal
 \return object | NIL

*/
METHOD __hql_showMenu( oQpoint ) CLASS hql_contextMenu
   IF ( !::__hql_OnBeforeShow( oQpoint ) )
      RETURN .F.
   ENDIF
RETURN ::exec( oQpoint )

// ==================== SLOTS/EVENTS section ====================

// ==================== HIDDEN section ====================
