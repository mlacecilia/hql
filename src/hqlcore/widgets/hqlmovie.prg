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

 \brief Returns a new hql_movie object instance

*/
FUNCTION hqlMovie( ... )
RETURN hql_movie():new( ... )

/*!

 \brief define hql_movie class

*/
CLASS hql_movie INHERIT hb_QMovie, hql_abs0001

   EXPORTED:
   METHOD init
   METHOD hqlOnFrameChanged
   METHOD hqlOnSateChanged

   PROTECTED:
   VAR bHqlFrameChanged                   INIT NIL
   VAR bHqlStateChanged                     INIT NIL
   SIGNAL __hql_QFrameChanged
   SIGNAL __hql_QStateChanged

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_movie

   ::QMovie:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief set/get block
 \param[in] block | NIL
 \return block | NIL

*/
METHOD hqlOnFrameChanged( arg1 ) CLASS hql_movie
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlFrameChanged := arg1
      IF ( hb_IsEvalItem(::bHqlFrameChanged) )
         ::connect( "frameChanged(int)", { |nInt| ::__hql_QFrameChanged(nInt) } )
      ELSE
         ::disconnect( "frameChanged(int)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief set/get block
 \param[in] block | NIL
 \return block | NIL

*/
METHOD hqlOnSateChanged( arg1 ) CLASS hql_movie
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlStateChanged := arg1
      IF ( hb_IsEvalItem(::bHqlStateChanged) )
         ::connect( "stateChanged(QMovie::MovieState)", { |nInt| ::__hql_QStateChanged(nInt) } )
      ELSE
         ::disconnect( "stateChanged(QMovie::MovieState)" )
      ENDIF
   ENDIF
RETURN self

// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] handle event/signal
 \param(IN) ...
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QFrameChanged( nInt ) CLASS hql_movie
   IF ( hb_IsEvalItem( ::bHqlFrameChanged ) )
      EVAL( ::bHqlFrameChanged, nInt, Self )  // Self always as last
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] handle event/signal
 \param(IN) ...
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QStateChanged( nInt ) CLASS hql_movie
   IF ( hb_IsEvalItem( ::bHqlStateChanged ) )
      EVAL( ::bHqlStateChanged, nInt, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
