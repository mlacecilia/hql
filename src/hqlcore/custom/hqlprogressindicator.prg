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

 \brief Returns a new hql_progressIndicator object instance

*/
FUNCTION hqlProgressIndicator( ... )
RETURN hql_progressIndicator():new( ... )

/*!

 \brief define hql_progressIndicator class

*/
CLASS hql_progressIndicator INHERIT hql_widget

   EXPORTED:
   METHOD init
   METHOD animationDelay                  INLINE ::nHqlDelay
   METHOD isAnimated                      INLINE ( ::oHqlTimer:isActive() )
   METHOD isDisplayedWhenStopped          INLINE ::lHqlDisplayedWhenStopped
   METHOD setAnimationDelay
   METHOD setColor
   METHOD setDisplayedWhenStopped
   METHOD startAnimation
   METHOD stopAnimation

   PROTECTED:
   METHOD __hqlCleaner
   METHOD __hqlConnect
   SIGNAL __hql_QPaint

   HIDDEN:
   VAR nHqlAngle                          INIT 0
   VAR oHqlTimer                          INIT NIL
   VAR nHqlDelay                          INIT 40
   VAR lHqlDisplayedWhenStopped           INIT .F.
   VAR oHqlColor                          INIT NIL
   METHOD __timerEvent


ENDCLASS

/*!

 \brief Initialize a new object instance
 \param(IN) string, ...
 \return self

*/
METHOD init( ... ) CLASS hql_progressIndicator

   ::hql_widget:init( ... )

   IF ( !::__hqlHasParent() )
      hqlThrow( hqlErrorNew( 7007, PROCNAME() ) )
   ENDIF

   ::oHqlTimer := hqlTimer(/*name*/, self)
   ::oHqlTimer:hqlOnTimeout( { || ::__timerEvent() } )

   ::oHqlColor := QColor()

   ::setSizePolicy(QSizePolicy_Fixed, QSizePolicy_Fixed)
   ::setFocusPolicy(Qt_NoFocus)

RETURN self

/*!

 \brief Set animation delay
 \param(IN) numeric
 \return self

*/
METHOD setAnimationDelay( arg1 ) CLASS hql_progressIndicator
   LOCAL lWasActive

   IF ( hb_IsNumeric(arg1) )
      IF ( (lWasActive := ::oHqlTimer:isActive()) )
         ::oHqlTimer:stop()
      ENDIF
      ::nHqlDelay := arg1
      ::oHqlTimer:setInterval( ::nHqlDelay )
      IF ( lWasActive )
         ::oHqlTimer:start()
      ENDIF
   ENDIF
RETURN self

/*!

 \brief Set color
 \param(IN) bool
 \return self

*/
METHOD setColor( arg1 ) CLASS hql_progressIndicator
   IF ( hql_IsDerived(arg1, "QColor") )
      ::oHqlColor := arg1
      ::update()
   ENDIF
RETURN self

/*!

 \brief Set display when stopped flag
 \param(IN) bool
 \return self

*/
METHOD setDisplayedWhenStopped( arg1 ) CLASS hql_progressIndicator
   IF ( hb_IsLogical(arg1) )
      ::lHqlDisplayedWhenStopped := arg1
      ::update()
   ENDIF
RETURN self

/*!

 \brief start animation
 \param(IN)
 \return self

*/
METHOD startAnimation() CLASS hql_progressIndicator

   ::nHqlAngle := 0

   IF ( ::oHqlTimer:isActive() )
      ::oHqlTimer:stop()
   ENDIF

   ::oHqlTimer:start( ::nHqlDelay )

RETURN self

/*!

 \brief start animation
 \param(IN)
 \return self

*/
METHOD stopAnimation() CLASS hql_progressIndicator

   IF ( ::oHqlTimer:isActive() )
      ::oHqlTimer:stop()
      ::update()
   ENDIF

RETURN self

// ==================== PROTECTED section ====================

METHOD __hqlCleaner() CLASS hql_progressIndicator
   ::oHqlColor := NIL
   ::hql_widget:__hqlCleaner()
RETURN NIL

/*!

 \brief [PROTECTED] connect signal/event
 \param(IN) none
 \return bool

*/
METHOD __hqlConnect() CLASS hql_progressIndicator
   ::hql_widget:__hqlConnect()
   ::connect( QEvent_Paint,  { |oEvent,oPainter| ::__hql_QPaint(oEvent,oPainter) } )
RETURN .T.

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [INTERNAL] handle event or signal
 \param[in] ...
 \return ...

*/
SIGNAL __hql_QPaint(oEvent,oPainter) CLASS hql_progressIndicator
   LOCAL nWidth
   LOCAL nOuterRadius
   LOCAL nInnerRadius
   LOCAL nCapsuleHeight
   LOCAL nCapsuleWidth
   LOCAL nCapsuleRadius
   LOCAL nI
   LOCAL oColor
   LOCAL oRect

   IF ( !::lHqlDisplayedWhenStopped .AND. !::isAnimated() )
      RETURN .T.  //  I M P O R T A N T
   ENDIF

   nWidth := MIN( ::width(), ::height() )

   oPainter:save()

   oRect := oEvent:rect()

   oPainter:setRenderHint( QPainter_Antialiasing )

   nOuterRadius := (nWidth-1)*0.5
   nInnerRadius := (nWidth-1)*0.5*0.38

   nCapsuleHeight := nOuterRadius - nInnerRadius
   nCapsuleWidth := IIF(nWidth > 32, (nCapsuleHeight * 0.23), (nCapsuleHeight * 0.35) )
   nCapsuleRadius := nCapsuleWidth / 2

   FOR nI := 0 TO 11

      oColor := QColor( ::oHqlColor )
      oColor:setAlphaF( 1.0 - (nI/12.0) )

      oPainter:setPen( Qt_NoPen )
      oPainter:setBrush( QBrush( oColor ) )

      oPainter:save()
      oPainter:translate( oRect:center() )
      oPainter:rotate( ::nHqlAngle - nI * 30.0 )
      oPainter:drawRoundedRect( -nCapsuleWidth*0.5, -(nInnerRadius+nCapsuleHeight), nCapsuleWidth, nCapsuleHeight, nCapsuleRadius, nCapsuleRadius )
      oPainter:restore()

   NEXT nI

   oPainter:restore()

RETURN .T.  //  I M P O R T A N T

// ==================== HIDDEN section ====================

/*!

 \brief
 \param(IN)
 \return nil

*/
METHOD __timerEvent() CLASS hql_progressIndicator
   ::nHqlAngle = (::nHqlAngle + 30) % 360
   ::update()
RETURN NIL
