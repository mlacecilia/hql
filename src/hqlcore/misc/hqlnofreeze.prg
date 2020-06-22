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

 \brief [public_function] to wait ... msecs; by default 3000
 \param[in] [OPTIONAL] numeric msecs
 \param[in] [OPTIONAL] block to be executed while wait loop executed; hb_datetime argument given
   eg hqlWait( 2000, { |dateTime| doSomething( dateTime ) } )
 \return NIL

*/
PROCEDURE hqlWait( arg1, arg2 )
   LOCAL nWaitMsec
   LOCAL tEnd
   LOCAL bBlock

   nWaitMsec := IIF( hb_IsNumeric(arg1) .AND. arg1 >= 0, arg1, 3000 )
   bBlock := IIF( hb_IsEvalItem(arg2), arg2, { || NIL } )

   tEnd := hb_DateTime() + hb_MsecToT( ABS( nWaitMsec ) )   // ABS to always get positive values
   DO WHILE ( hb_DateTime() <= tEnd )
      EVAL( bBlock, hb_DateTime() )
   ENDDO

RETURN

/*!

 \brief [public_function] creates hql_nofreeze object
 \param[in] Qt arguments
 \return hql_nofreeze object

*/
FUNCTION hqlNoFreeze( ... )
RETURN hql_nofreeze():new( ... )

/*!

 \brief

*/
CLASS hql_nofreeze

   EXPORTED:
   METHOD init
   METHOD check
   METHOD msec
   METHOD msecElapsed
   METHOD processEventsEnabled
   METHOD processEventsFlag
   METHOD restart
   METHOD setMsec
   METHOD startAt

   PROTECTED:
   VAR tCheck                             INIT NIL
   VAR nMsecsToCheck                      INIT 0      // more msecs, faster application but application can be freezed;
                                                      // fewer msecs, lower application but application doesn't freeze
   VAR lProcessEnabled                    INIT .T.
   VAR nProcessFlag                       INIT 0      // see init
   VAR tStart                             INIT NIL
   METHOD __timElapsed

ENDCLASS

/*!

 \brief init

*/
METHOD init( arg1, arg2, arg3 ) CLASS hql_nofreeze

   ::nProcessFlag := QEventLoop_AllEvents //see http://doc.qt.io/qt-5/qeventloop.html#ProcessEventsFlag-enum

   ::setMsec( arg1 )
   ::processEventsEnabled( arg2 )
   ::processEventsFlag( arg3 )

RETURN Self

/*!

 \brief check if time elapsed; if true and process events enabled call it. Returns true if elapsed else false
 \param[in] [OPTIONAL] boolean to force
 \return boolean

*/
METHOD check( lForced ) CLASS hql_nofreeze

   LOCAL lReturn := .F.
   LOCAL nElapsed

   hb_Default( @lForced, .F. )

   IF ( ::tCheck == NIL )
      ::restart()
   ENDIF

   nElapsed := ::__timElapsed( ::tCheck, hb_DateTime() )

   IF ( lForced .OR. ( nElapsed >= ::nMsecsToCheck ) )
      ::tCheck := hb_DateTime()
      lReturn := .T.
   ENDIF

   IF ( lReturn .AND. ::lProcessEnabled )
      HqlFw:processEvents( ::nProcessFlag )
   ENDIF

RETURN lReturn

/*!

 \brief returns how many msecs check
 \param[in] none
 \return numeric

*/
METHOD msec() CLASS hql_nofreeze
RETURN ::nMsecsToCheck

/*!

 \brief returns time elapsed as msecs
 \param[in] [OPTIONAL] two time stamp value
 \param[in] [OPTIONAL] one time stamp
 \return numeric

*/
METHOD msecElapsed( ... ) CLASS hql_nofreeze

   IF PCOUNT() == 2 .AND. hb_IsTimeStamp( hb_Pvalue(1) ) .AND. hb_IsTimeStamp( hb_Pvalue(2) )
      RETURN ::__timElapsed( hb_Pvalue(1), hb_Pvalue(2) )
   ENDIF

   IF PCOUNT() == 1 .AND. hb_IsTimeStamp( hb_Pvalue(1) )
      RETURN ::__timElapsed( ::tStart, hb_Pvalue(1) )
   ENDIF

RETURN ::__timElapsed( ::tStart, hb_DateTime() )

/*!

 \brief set/get if processEvents call must be performed
 \param[in] [OPTIONAL] boolean value
 \return boolean

*/
METHOD processEventsEnabled( arg1 ) CLASS hql_nofreeze
   IF ( PCOUNT() == 1 .AND. hb_IsLogical(arg1) )
      ::lProcessEnabled := arg1
   ENDIF
RETURN ::lProcessEnabled

/*!

 \brief set/get if processEvents flag
 \param[in] [OPTIONAL] numeric  //see http://doc.qt.io/qt-5/qeventloop.html#ProcessEventsFlag-enum
 \return numeric

*/
METHOD processEventsFlag( arg1 ) CLASS hql_nofreeze
   IF ( PCOUNT() == 1 .AND. hb_IsNumeric(arg1) )
      ::nProcessFlag := arg1
   ENDIF
RETURN ::nProcessFlag

/*!

 \brief restart
 \param[in] none
 \return Self

*/
METHOD restart() CLASS hql_nofreeze
   ::tCheck := ::tStart := hb_DateTime()
RETURN Self

/*!

 \brief set how many msecs
 \param[in] none
 \return Self

*/
METHOD setMsec( arg1 ) CLASS hql_nofreeze
   IF ( arg1 == NIL )
      ::nMsecsToCheck := 3000 // default
   ELSEIF ( hb_IsNumeric(arg1) .AND. arg1 >= 0 )
      ::nMsecsToCheck := arg1
   ENDIF
RETURN Self

/*!

 \brief returns hb_dateTime start time
 \param[in] none
 \return hb_dateTime

*/
METHOD startAt() CLASS hql_nofreeze
RETURN::tStart

// ==================== PROTECTED section ====================

/*!

 \brief [INTERNAL] help to calculate elapsed msecs
 \param[in] time stamp minor, time stamp major
 \return numeric

*/
METHOD __timElapsed( tLeft, tRight ) CLASS hql_nofreeze
RETURN ROUND( ( hb_TtoN( tRight ) - hb_TtoN( tLeft ) ) * 86400000, 0 )
