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

 \brief define hql_abs0050 class
   inherited by:
      hql_contextMenu,
      hql_menu,
      hql_menuBar

*/
CLASS hql_abs0050 INHERIT hql_abs0001

   EXPORTED:
   METHOD hqlAddAction
   METHOD hqlAddMenu
   METHOD hqlAddWidget
   METHOD hqlCaption                      SETGET
   METHOD hqlOnTriggered

   PROTECTED:
   VAR bHqlTriggered                      INIT NIL
   SIGNAL __hql_QTriggered

   HIDDEN:

ENDCLASS

/*!

 \brief helper to add action
 \param(IN) QAction | string
 \return object

*/
METHOD hqlAddAction( arg1 ) CLASS hql_abs0050
   LOCAL oObject

   IF ( hb_IsObject(arg1) .AND. arg1:isDerivedFrom("QAction") )
      oObject := arg1
   ELSE
      arg1 := hb_DefaultValue(arg1, "")
      oObject := hqlAction( arg1, Self )
   ENDIF

   ::addAction( oObject )

RETURN oObject

/*!

 \brief add menu
 \param(IN) QMenu | string
 \return object

*/
METHOD hqlAddMenu( arg1 ) CLASS hql_abs0050
   LOCAL oObject

   IF ( hb_IsObject(arg1) .AND. arg1:isDerivedFrom("QMenu") )
      oObject := arg1
   ELSE
      arg1 := hb_DefaultValue(arg1, "")
      oObject := hqlMenu( arg1, Self )
   ENDIF

   ::addMenu( oObject )

RETURN oObject

/*!

 \brief helper to add widget
 \param(IN) QWidget derived
 \return object

*/
METHOD hqlAddWidget( arg1 ) CLASS hql_abs0050
   LOCAL oWidgetAction

   IF ( arg1:isDerivedFrom("QWidget") )
      oWidgetAction := QWidgetAction( Self )
      oWidgetAction:setDefaultWidget( arg1 )
      ::addAction( oWidgetAction )
   ENDIF

RETURN arg1

/*!

 \brief set/get title
 \param(IN) [OPTIONAL] string
 \return string

*/
METHOD hqlCaption( arg1 ) CLASS hql_abs0050
   IF ( hb_IsString(arg1) )
      ::setTitle( arg1 )
   ENDIF
RETURN ::title()

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnTriggered( arg1 ) CLASS hql_abs0050
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlTriggered := arg1
      IF ( hb_IsEvalItem(::bHqlResize) )
         ::connect( "triggered(QAction*)", { |qAction| ::__hql_QTriggered(qAction) } )
      ELSE
         ::disconnect( "triggered(QAction*)" )
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
SIGNAL __hql_QTriggered(qAction) CLASS hql_abs0050
   IF ( hb_IsEvalItem(::bHqlTriggered) )
      EVAL( ::bHqlTriggered, qAction, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
