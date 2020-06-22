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

 \brief define hql_abs0040 class
   inherited by:
      hql_checkBox,
      hql_commandLinkButton,
      hql_pushButton,
      hql_radioButton,
      hql_toolButton

*/
CLASS hql_abs0040 INHERIT hql_abs0001

   EXPORTED:
   METHOD hqlOnClicked
   METHOD hqlOnPressed
   METHOD hqlOnToggled

   PROTECTED:
   VAR bHqlClicked                        INIT NIL
   VAR bHqlPressed                        INIT NIL
   VAR bHqlToggled                        INIT NIL
   SIGNAL __hql_QClicked
   SIGNAL __hql_QPressed
   SIGNAL __hql_QToggled

   HIDDEN:

ENDCLASS

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnClicked( arg1 ) CLASS hql_abs0040
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlClicked := arg1
      IF ( hb_IsEvalItem(::bHqlClicked) )
         //If the button is checkable, checked is true if the button is checked, or false if the button is unchecked.
         ::connect( "clicked(bool)", { |lBool| ::__hql_QClicked(lBool) } )
      ELSE
         ::disconnect( "clicked(bool)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnPressed( arg1 ) CLASS hql_abs0040
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlPressed := arg1
      IF ( hb_IsEvalItem(::bHqlPressed) )
         ::connect( "pressed()" , { || ::__hql_QPressed() } )
      ELSE
         ::disconnect( "pressed()" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnToggled( arg1 ) CLASS hql_abs0040
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlToggled := arg1
      IF ( hb_IsEvalItem(::bHqlToggled) )
         ::connect( "toggled(bool)" , { |lBool| ::__hql_QToggled(lBool) } )
      ELSE
         ::disconnect( "toggled(bool)" )
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
SIGNAL __hql_QClicked(lBool) CLASS hql_abs0040
   IF ( hb_IsEvalItem( ::bHqlClicked ) )
      IF ( ::isCheckable() )
         EVAL( ::bHqlClicked, lBool, Self )  // Self always as last
      ELSE
         EVAL( ::bHqlClicked, Self )  // Self always as last
      ENDIF
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QPressed() CLASS hql_abs0040
   IF ( hb_IsEvalItem( ::bHqlPressed ) )
      EVAL( ::bHqlPressed, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QToggled(lBool) CLASS hql_abs0040
   IF ( hb_IsEvalItem( ::bHqlToggled ) )
      EVAL( ::bHqlToggled, lBool, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
