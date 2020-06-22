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

 \brief Returns a new hql_textBrowser object instance

*/
FUNCTION hqlTextBrowser( ... )
RETURN hql_textBrowser():new( ... )

/*!

 \brief define hql_textBrowser class

*/
CLASS hql_textBrowser INHERIT hb_QTextBrowser, hql_abs0001

   EXPORTED:
   METHOD init
   METHOD hqlOnBackwardAvailable
   METHOD hqlOnForwardAvailable
   METHOD hqlOnSourceChanged

   PROTECTED:
   VAR bHqlSourceChanged                  INIT NIL
   VAR bHqlBackwardAvailable              INIT NIL
   VAR bHqlForwardAvailable               INIT NIL
   METHOD __hqlValueGet
   METHOD __hqlValueSet
   SIGNAL __hql_QBackwardAvailable
   SIGNAL __hql_QForwardAvailable
   SIGNAL __hql_QSourceChanged

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_textBrowser

   ::QTextBrowser:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnBackwardAvailable( arg1 ) CLASS hql_textBrowser
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlBackwardAvailable := arg1
      IF ( hb_IsEvalItem(::bHqlBackwardAvailable) )
         ::connect( "backwardAvailable(bool)", { |lBool| ::__hql_QBackwardAvailable(lBool) } )
      ELSE
         ::disconnect( "backwardAvailable(bool)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnForwardAvailable( arg1 ) CLASS hql_textBrowser
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlForwardAvailable := arg1
      IF ( hb_IsEvalItem(::bHqlForwardAvailable) )
         ::connect( "forwardAvailable(bool)", { |lBool| ::__hql_QForwardAvailable(lBool) } )
      ELSE
         ::disconnect( "forwardAvailable(bool)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnSourceChanged( arg1 ) CLASS hql_textBrowser
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlSourceChanged := arg1
      IF ( hb_IsEvalItem(::bHqlSourceChanged) )
         ::connect( "sourceChanged(const QUrl)", { |oUrl| ::__hql_QSourceChanged(oUrl) } )
      ELSE
         ::disconnect( "sourceChanged(const QUrl)" )
      ENDIF
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] get text
 \param[in] none
 \return numeric

*/
METHOD __hqlValueGet() CLASS hql_textBrowser
RETURN ::toPlainText()

/*!

 \brief [PROTECTED] set text
 \param[in] string
 \return NIL

*/
METHOD __hqlValueSet( arg1 ) CLASS hql_textBrowser
   IF (hb_IsString(arg1) )
      ::setPlainText( hb_Pvalue(1) )
   ENDIF
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] handle event/signal
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QBackwardAvailable( lBool ) CLASS hql_textBrowser
   IF hb_IsEvalItem( ::bHqlBackwardAvailable )
      EVAL( ::bHqlBackwardAvailable, lBool, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] handle event/signal
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QForwardAvailable( lBool ) CLASS hql_textBrowser
   IF hb_IsEvalItem( ::bHqlForwardAvailable )
      EVAL( ::bHqlForwardAvailable, lBool, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] handle event/signal
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QSourceChanged( ourl ) CLASS hql_textBrowser
   IF hb_IsEvalItem( ::bHqlSourceChanged )
      EVAL( ::bHqlSourceChanged, ourl, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
