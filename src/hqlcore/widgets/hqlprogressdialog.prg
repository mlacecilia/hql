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

 \brief Returns a new hql_progressDialog object instance

*/
FUNCTION hqlProgressDialog( ... )
RETURN hql_progressDialog():new( ... )

/*!

 \brief define hql_progressDialog class
   ALERT: the progressDialog seems not to be modal using :exe() on the other hand it's not very clear its usage reading Qt doc.
   For this reason and opportunity hql handle progressDialog always with parent required.

   In this way we can use progressDialog on two ways
   a)  showing progressBar for a defined interval eg
      :setMinimum( 0 )
      :setMaximum( 100 )
      :hqlActivate()
      FOR nAt := 1 TO 100
         :setValue( nAt )
      NEXT
      hqlRelease()   mandatory to well destroy object

   b)  showing progressBar for undefined interval (infinite loop) eg
      :setMinimumDuration( 0 )
      :setRange( 0, 0 )
      :setValue( 0 )
      :hqlActivate()
      DO WHILE
         .....
      NEXT
      hqlRelease()   mandatory to well destroy object

*/
CLASS hql_progressDialog INHERIT hb_QProgressDialog, hql_abs0030

   EXPORTED:
   METHOD init
   METHOD hqlActivate
   METHOD hqlHideCancel
   METHOD hqlOnCanceled

   PROTECTED:
   VAR bHqlCanceled                       INIT NIL
   SIGNAL __hql_QCanceled

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_progressDialog

   ::QProgressDialog:init( ... )
   ::setAttribute( Qt_WA_DeleteOnClose )
   ::setWindowIcon( QIcon( ":/hqlres/HQL96" ) )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF
   IF ( !::__hqlHasParent() )
      hqlThrow( hqlErrorNew( 7007, PROCNAME() ) )
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief Activate (show/rise) form
 \param(IN)
 \return numeric

*/
METHOD hqlActivate() CLASS hql_progressDialog

   /*IF ( !::lHqlActivated )
      ::__hqlWinPositioning()
   ENDIF*/

   ::show()

   /*IF ( !::lHqlActivated .AND. hb_IsObject( ::oHqlSplashScreen ) )
      ::oHqlSplashScreen:finish( Self )
   ENDIF*/

   ::raise()

   ::activateWindow()

   IF ( ::lHqlActivated )
      HqlFw:processEvents( /*nFlags*/, /*nMsecs*/ )
   ELSE
      ::lHqlActivated := .T.
      HqlFw:processEvents( /*nFlags*/, /*nMsecs*/ )
   ENDIF

RETURN ::nHqlReleaseCode

/*!

 \brief a little trick to hide cancelButton. Pay attention: to show again you must build again!
 \param[in] none
 \return Self

*/
METHOD hqlHideCancel() CLASS hql_progressDialog
   LOCAL oQtButton := QPushButton( Self )
  ::setCancelButton( oQtButton )
   oQtButton:hide()
RETURN Self

/*!

 \brief set signal/event block
 \param(IN) block
 \return Self

*/
METHOD hqlOnCanceled( arg1 ) CLASS hql_progressDialog
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlCanceled := arg1
      IF ( hb_IsEvalItem(::bHqlCanceled) )
         ::connect( "canceled()" , { || ::__hql_QCanceled() } )
      ELSE
         ::disconnect( "canceled()" )
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
METHOD __hql_QCanceled() CLASS hql_progressDialog
   IF ( hb_IsEvalItem(::bHqlCanceled) )
      EVAL( ::bHqlCanceled, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
