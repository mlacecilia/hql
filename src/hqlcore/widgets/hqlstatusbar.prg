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

 \brief Returns a new hql_statusBar object instance

*/
FUNCTION hqlStatusBar( ... )
RETURN hql_statusBar():new( ... )

/*!

 \brief define hql_statusBar class

*/
CLASS hql_statusBar INHERIT hb_QStatusBar, hql_abs0001

   EXPORTED:
   METHOD init
   METHOD hqlInsertPermanentWidget
   METHOD hqlInsertWidget
   METHOD hqlOnMessageChanged
   METHOD hqlStatusDateTime

   PROTECTED:
   VAR bHqlMessageChanged                 INIT NIL
   VAR oHqlStatusDateTime                 INIT NIL
   SIGNAL __hql_QMessageChanged

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, ... ) CLASS hql_statusBar

   ::QStatusBar:init( ... )

   IF ( !::__hqlHasParent() )
      ::__hqlNestedWithSetParent()
   ENDIF
   IF ( hql_IsDerived(::parent(), "QMainWindow") )
      ::parent:setStatusBar( Self )
   ENDIF

   ::__hqlAssignObjectName( cName )
   ::__hqlConnect()

RETURN self

/*!

 \brief Inserts the given widget at the given index permanently to this status bar,
   reparenting the widget if it isn't already a child of this QStatusBar object.
   If index is out of range, the widget is appended. WARNING using C style (0 based)

*/
METHOD hqlInsertPermanentWidget( nIndex, oWidget, nStretch ) CLASS hql_statusBar

   nStretch := hb_DefaultValue( nStretch, 0 )

   IF ( hql_IsDerived(oWidget, "QWidget") )
      nIndex := IIF( hb_IsNumeric( nIndex ), nIndex, -1 )
      ::insertPermanentWidget( nIndex, oWidget, nStretch )
   ENDIF

RETURN oWidget

/*!

 \brief Inserts the given widget at the given index to this status bar,
   reparenting the widget if it isn't already a child of this QStatusBar object.
   If index is out of range, the widget is appended. WARNING using C style (0 based)

*/
METHOD hqlInsertWidget( nIndex, oWidget, nStretch ) CLASS hql_statusBar

   nStretch := hb_DefaultValue( nStretch, 0 )

   IF ( hql_IsDerived(oWidget, "QWidget") )
      nIndex := IIF( hb_IsNumeric( nIndex ), nIndex, -1 )
      ::insertWidget( nIndex, oWidget, nStretch )
   ENDIF

RETURN oWidget

/*!

 \brief set/get block
 \param[in] block | NIL
 \return block | NIL

*/
METHOD hqlOnMessageChanged( arg1 ) CLASS hql_statusBar
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlMessageChanged := arg1
      IF ( hb_IsEvalItem(::bHqlMessageChanged) )
         ::connect( "messageChanged(QString)" , { |cString| ::__hql_QMessageChanged(cString) } )
      ELSE
         ::disconnect( "messageChanged(QString)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief

*/
METHOD hqlStatusDateTime( cName ) CLASS hql_statusBar

   IF ( !hb_IsObject( ::oHqlStatusDateTime ) )
      ::oHqlStatusDateTime := hql_statusDateTime():new( cName, Self )
      ::insertPermanentWidget( -1, ::oHqlStatusDateTime, 0 )
   ENDIF

RETURN ::oHqlStatusDateTime

// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] handle event/signal
 \param[in] ... based on event/signal
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QMessageChanged( cString ) CLASS hql_statusBar

   IF hb_IsEvalItem( ::bHqlMessageChanged )
      EVAL( ::bHqlMessageChanged, cString, Self )  // Self always as last
   ENDIF

RETURN .F.

// ==================== HIDDEN section ====================

////////////////////////////////////////  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

/*!

 \brief hql_statusDateTime class definition

*/
CLASS hql_statusDateTime INHERIT hql_label STATIC

   EXPORTED:
   METHOD init
   METHOD hqlFormat                       INLINE ::nHqlFormat
   METHOD hqlSetFormat
   METHOD hqlSetType
   METHOD hqlType                         INLINE ::nHqlType

   PROTECTED:
   DATA nHqlFormat                        INIT QLocale_ShortFormat
   DATA nHqlType                          INIT HQL_STATUSDTTIME
   METHOD __hqlShowTime

ENDCLASS

/*!

 \brief initialize object instance
 \param[in] string name OR NIL (MANDATORY)
 \param[in] other Qt arguments
 \return SELF

*/
METHOD init( ... ) CLASS hql_statusDateTime

   ::hql_label:init( ... )

   WITH OBJECT hqlTimer( /*name*/, Self )
      :hqlOnTimeout( { || Self:__hqlShowTime() } )
      :start( 1000 )
   END WITH

   ::__hqlShowTime()

RETURN Self

/*!

 \brief
 \param[in]
 \return SELF

*/
METHOD hqlSetFormat( arg1 ) CLASS hql_statusDateTime
   IF ( hb_IsNumeric(arg1) )
      SWITCH arg1
      CASE QLocale_LongFormat
      CASE QLocale_ShortFormat
      CASE QLocale_NarrowFormat
         ::nHqlFormat := arg1
         ::__hqlShowTime()
         EXIT
      ENDSWITCH
   ENDIF
RETURN Self

/*!

 \brief
 \param[in]
 \return SELF

*/
METHOD hqlSetType( arg1 ) CLASS hql_statusDateTime
   IF ( hb_IsNumeric(arg1) )
      SWITCH arg1
      CASE HQL_STATUSDTTIME
      CASE HQL_STATUSDTDATE
      CASE HQL_STATUSDTALL
         ::nHqlType := arg1
         ::__hqlShowTime()
      EXIT
      ENDSWITCH
   ENDIF
RETURN Self

// ==================== PROTECTED section ====================

/*!

 \brief [INTERNAL] show current time
 \param[in] none
 \return NIL

*/
METHOD __hqlShowTime() CLASS hql_statusDateTime

   LOCAL oObject
   LOCAL cText

   SWITCH ::nHqlType
   CASE HQL_STATUSDTTIME
      oObject := QTime():currentTime()
      cText := oObject:toString(QLocale():timeFormat(::nHqlFormat))
      EXIT
   CASE HQL_STATUSDTDATE
      oObject := QDate():currentDate()
      cText := oObject:toString(QLocale():dateFormat(::nHqlFormat))
      EXIT
   OTHERWISE
      oObject := QDateTime():currentDateTime()
      cText := oObject:toString(QLocale():dateTimeFormat(::nHqlFormat))
      EXIT
   ENDSWITCH

   ::setText( cText )

RETURN NIL
