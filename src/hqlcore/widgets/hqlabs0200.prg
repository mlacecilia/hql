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

 \brief define hql_abs0200 class
   inherited by:
      hql_childDialog,
      hql_modalDialog

*/
CLASS hql_abs0200 INHERIT hql_abs0020

   EXPORTED:
   METHOD hqlAccept
   METHOD hqlOnFinished
   METHOD hqlRelease
   METHOD hqlReject

   PROTECTED:
   VAR bHqlFinished                       INIT NIL
   METHOD __hqlConnect
   SIGNAL __hql_QFinished                                   // connected by default

   HIDDEN:

ENDCLASS

/*!

 \brief forces QDialog_Accepted;
   WARNING it uses :done() so it means "Closes the dialog and sets its result code to r.
                                        If this dialog is shown with exec(), done() causes the local event loop to finish, and exec() to return r."
 \param[in] none
 \return Self

*/
METHOD hqlAccept() CLASS hql_abs0200
   ::done( QDialog_Accepted )
RETURN Self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnFinished( arg1 ) CLASS hql_abs0200
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlFinished := arg1
      // connected by default
   ENDIF
RETURN self

/*!

 \brief forces QDialog_Rejected;
   WARNING it uses :done() so it means "Closes the dialog and sets its result code to r.
                                        If this dialog is shown with exec(), done() causes the local event loop to finish, and exec() to return r."
 \param[in] none
 \return Self

*/
METHOD hqlReject() CLASS hql_abs0200
   ::done( QDialog_Rejected )
RETURN Self

/*!

 \brief Release form
 \param(IN) numeric
 \return self

*/
METHOD hqlRelease( nExit ) CLASS hql_abs0200
   ::nHqlReleaseCode := hb_DefaultValue(nExit, 0)
   ::done( ::nHqlReleaseCode )
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTD] Connect signal
 \param(IN)
 \return bool

*/
METHOD __hqlConnect() CLASS hql_abs0200
   ::hql_abs0020:__hqlConnect()
   ::connect( "finished(int)", { |nInt| ::__hql_QFinished(nInt) } )
RETURN .T.

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
METHOD __hql_QFinished( nInt ) CLASS hql_abs0200
   ::nHqlReleaseCode := nInt

   IF hb_IsEvalItem( ::bHqlFinished )
      EVAL( ::bHqlFinished, nInt, Self )  // Self always as last
   ENDIF

   ::__hqlSignalEverybody( self )
RETURN .F.
