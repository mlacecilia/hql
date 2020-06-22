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

 \brief define hql_abs0100 class
   inherited by:
      hql_plainTextEdit,
      hql_textEdit,

*/
CLASS hql_abs0100 INHERIT hql_abs0001

   EXPORTED:
   METHOD hqlHasSelection
   METHOD hqlOnTextChanged

   PROTECTED:
   VAR bHqlTextChanged                    INIT NIL
   METHOD __hqlValueGet
   METHOD __hqlValueSet
   SIGNAL __hql_QTextChanged

   HIDDEN:

ENDCLASS

/*!

 \brief returns true | false if it has selection
 \param[in] none
 \return boolean

*/
METHOD hqlHasSelection() CLASS hql_abs0100
RETURN ::textCursor():hasSelection()

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnTextChanged( arg1 ) CLASS hql_abs0100
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlTextChanged := arg1
      IF ( hb_IsEvalItem(::bHqlTextChanged) )
         ::connect( "textChanged()", { || ::__hql_QTextChanged() } )
      ELSE
         ::disconnect( "textChanged()" )
      ENDIF
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] get text
 \param[in] none
 \return text

*/
METHOD __hqlValueGet() CLASS hql_abs0100
RETURN ::toPlainText()

/*!

 \brief [PROTECTED] set text
 \param[in] string
 \return NIL

*/
METHOD __hqlValueSet( arg1 ) CLASS hql_abs0100
   IF ( hb_IsString(arg1) )
      ::setPlainText( arg1 )
   ENDIF
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QTextChanged() CLASS hql_abs0100
   IF ( hb_IsEvalItem( ::bHqlTextChanged ) )
      EVAL( ::bHqlTextChanged, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
