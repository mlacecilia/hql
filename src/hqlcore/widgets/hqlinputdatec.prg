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

 \brief Returns a new hql_inputDateC object instance

*/
FUNCTION hqlInputDateC( ... )
RETURN hql_inputDateC():new( ... )

/*!

 \brief define hql_inputDateC class

*/
CLASS hql_inputDateC INHERIT hql_inputDate

   EXPORTED:
   METHOD init
   METHOD hqlEnablePopUp                  SETGET

   PROTECTED:
   VAR oHqlButton                         INIT NIL
   VAR oHqlCalendar                       INIT NIL
   METHOD __hqlCleaner
   METHOD __hqlButtonInit
   METHOD __hqlCalendarInit
   METHOD __hqlCalendarPosition
   METHOD __hqlCalendarShow
   METHOD __hqlConnect                                      // override
   METHOD __hqlSetPadding
   SIGNAL __hql_button_QClicked
   SIGNAL __hql_calendar_QClicked
   SIGNAL __hql_QResize

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( ... ) CLASS hql_inputDateC
   LOCAL nFrameWidth
   LOCAL oMinSize

   ::hql_inputDate:init( ... )

   // build toolbutton
   ::__hqlButtonInit()

   // build calendarWidget
   ::__hqlCalendarInit()

   // get current frame width to set padding
   nFrameWidth := ::style():pixelMetric( QStyle_PM_DefaultFrameWidth )
   ::__hqlSetPadding( ::oHqlButton:sizeHint(), nFrameWidth )

   // get current minimu sizeHint about hql_InputDate
   oMinSize := ::minimumSizeHint()
   // set hql_InputDate MinimumSize comparing its size and minimum toolbutton required
   ::setMinimumSize( MAX( oMinSize:width(), ::oHqlButton:sizeHint:width() + nFrameWidth * 2 ), ;
                     MAX( oMinSize:height(), ::oHqlButton:sizeHint:height() + nFrameWidth * 2 ) )

RETURN self

/*!

 \brief to enable/disable calendar popup and return its state
 \param[in] boolean
 \return boolean

*/
METHOD hqlEnablePopUp( arg1 ) CLASS hql_inputDateC
   IF ( hb_IsLogical(arg1) )
      ::oHqlButton:setEnabled(arg1)
   ENDIF
RETURN ::oHqlButton:isEnabled()

// ==================== PROTECTED section ====================

/*
 \brief [PROTECTED] Object cleaner
 \param(IN)
 \return NIL

*/
METHOD __hqlCleaner() CLASS hql_inputDateC
   ::oHqlCalendar := NIL
   ::oHqlButton   := NIL
   ::hql_inputDate:__hqlCleaner()
RETURN NIL

/*!

 \brief [PROTECTED] build button
 \param[in] none
 \return NIL

*/
METHOD __hqlButtonInit() CLASS hql_inputDateC

   WITH OBJECT ::oHqlButton := hqlToolButton( "HQL_INPUTDATEC_TOOLBUTTON", Self )
      :setStyleSheet( "QToolButton { border: none; padding: 1px; border-left: 1px inset gray; }" )
      :setCursor( QCursor( Qt_ArrowCursor ) )
      :setArrowType( Qt_DownArrow )
      :setSizePolicy( QSizePolicy_Minimum, QSizePolicy_MinimumExpanding )
      :hqlOnClicked( { || ::__hql_button_Qclicked() } )
   END WITH

   ::oHqlButton:setFocusProxy( Self )

RETURN NIL

/*!

 \brief  [PROTECTED]

*/
METHOD __hqlCalendarInit() CLASS hql_inputDateC

   WITH OBJECT ::oHqlCalendar := hqlCalendarWidget( "HQL_INPUTDATEC_CALENDAR", Self )
      :setGridVisible( .T. )
      :setWindowFlags( Qt_Popup )
      :hqlOnClicked( { |qtDate| ::__hql_calendar_Qclicked(qtDate) } )
   END WITH

RETURN NIL

/*!

 \brief [PROTECTED] how to position calendar. copied from qdatetimeedit.cpp

 original code for C++ users
void QDateTimeEditPrivate::positionCalendarPopup()
{
Q_Q(QDateTimeEdit);
QPoint pos = (q->layoutDirection() == Qt::RightToLeft) ? q->rect().bottomRight() : q->rect().bottomLeft();
QPoint pos2 = (q->layoutDirection() == Qt::RightToLeft) ? q->rect().topRight() : q->rect().topLeft();
pos = q->mapToGlobal(pos);
pos2 = q->mapToGlobal(pos2);
QSize size = monthCalendar->sizeHint();
QRect screen = QApplication::desktop()->availableGeometry(pos);
//handle popup falling "off screen"
if (q->layoutDirection() == Qt::RightToLeft) {
pos.setX(pos.x()-size.width());
pos2.setX(pos2.x()-size.width());
if (pos.x() < screen.left())
pos.setX(qMax(pos.x(), screen.left()));
else if (pos.x()+size.width() > screen.right())
pos.setX(qMax(pos.x()-size.width(), screen.right()-size.width()));
} else {
if (pos.x()+size.width() > screen.right())
pos.setX(screen.right()-size.width());
pos.setX(qMax(pos.x(), screen.left()));
}
if (pos.y() + size.height() > screen.bottom())
pos.setY(pos2.y() - size.height());
else if (pos.y() < screen.top())
pos.setY(screen.top());
if (pos.y() < screen.top())
pos.setY(screen.top());
if (pos.y()+size.height() > screen.bottom())
pos.setY(screen.bottom()-size.height());
monthCalendar->move(pos);
}

*/
METHOD __hqlCalendarPosition() CLASS hql_inputDateC

   LOCAL oQpointPos
   LOCAL oQpointPos2
   LOCAL oQsize
   LOCAL oQrectScreen

   IF ::layoutDirection() == Qt_RightToLeft
      oQpointPos := ::rect:bottomRight()
      oQpointPos2 := ::rect:topRight()
   ELSE
      oQpointPos := ::rect:bottomLeft()
      oQpointPos2 := ::rect:topLeft()
   ENDIF

   oQpointPos := ::mapToGlobal( oQpointPos )
   oQpointPos2 := ::mapToGlobal( oQpointPos2 )
   oQsize := ::oHqlCalendar:sizeHint()
   oQrectScreen := HqlFw:QtDeskTop:availableGeometry()

   //handle popup falling "off screen"
   IF ::layoutDirection() == Qt_RightToLeft

      oQpointPos:setX( oQpointPos:x() - oQsize:width() )
      oQpointPos2:setX( oQpointPos2:x() - oQsize:width() )
      IF oQpointPos:x() < oQrectScreen:left()
         oQpointPos:setX( MAX( oQpointPos:x(), oQrectScreen:left() ) )
      ELSEIF oQpointPos:x() + oQsize:width() > oQrectScreen:right()
         oQpointPos:setX( MAX( oQpointPos:x() - oQsize:width(), oQrectScreen:right()- oQsize:width() ) )
      ENDIF

   ELSE

      IF oQpointPos:x() + oQsize:width() > oQrectScreen:right()
         oQpointPos:setX( oQrectScreen:right() - oQsize:width() )
         oQpointPos:setX( MAX( oQpointPos:x(), oQrectScreen:left() ) )
      ENDIF

   ENDIF

   IF oQpointPos:y() + oQsize:height() > oQrectScreen:bottom()
      oQpointPos:setY( oQpointPos2:y() - oQrectScreen:height() )
   ELSEIF oQpointPos:y() < oQrectScreen:top()
      oQpointPos:setY( oQrectScreen:top() )
   ENDIF

   IF oQpointPos:y() < oQrectScreen:top()
      oQpointPos:setY( oQrectScreen:top() )
   ENDIF

   IF oQpointPos:y() + oQsize:height() > oQrectScreen:bottom()
      oQpointPos:setY( oQrectScreen:bottom() - oQsize:height() )
   ENDIF

   ::oHqlCalendar:move( oQpointPos )

RETURN NIL

/*!

 \brief  [PROTECTED]

*/
METHOD __hqlCalendarShow() CLASS hql_inputDateC

   LOCAL oDate

   ::__hqlCalendarPosition()

   oDate := ::date() // it returns QDate object

   IF ! oDate:isValid()
      oDate := QDate():currentDate()
   ENDIF

   ::oHqlCalendar:show()
   ::oHqlCalendar:setSelectedDate( oDate ) // it doesn't accept derived object :-((
   ::oHqlCalendar:showSelectedDate()

RETURN NIL

/*!

 \brief [PROTECTED] default connected event

*/
METHOD __hqlConnect() CLASS hql_inputDateC
   ::hql_inputDate:__hqlConnect()
   ::connect( QEvent_Resize, {|oEvent| ::__hql_QResize(oEvent) } )
RETURN .T.

/*!

 \brief [PROTECTED] create a padding-right value

*/
METHOD __hqlSetPadding( oButtSize, nFrameWidth ) CLASS hql_inputDateC

   LOCAL nPadding

   nPadding := oButtSize:width() + nFrameWidth
   ::setStyleSheet( "QLineEdit { padding-right: " + hb_NtoC(nPadding) + "px; } " )

RETURN Self

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] handle event/signal
 \param[in] ... based on event/signal
 \return object | NIL

*/
SIGNAL __hql_button_Qclicked() CLASS hql_inputDateC

   ::__hqlCalendarShow()

RETURN NIL

/*!

 \brief [PROTECTED] handle event/signal
 \param[in] ... based on event/signal
 \return object | NIL

*/
SIGNAL __hql_calendar_Qclicked( oQtDate ) CLASS hql_inputDateC

   // assign to hql_inputDate
   ::__hqlValueSet( oQtDate )   // it accepts hb_date and QDate as argument

   // hide calendar
   ::oHqlCalendar:hide()

RETURN NIL

/*!

 \brief [PROTECTED] handle event/signal
 \param[in] ... based on event/signal
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QResize( oEvent ) CLASS hql_inputDateC

   LOCAL nFrameWidth
   LOCAL nButtonWidth
   LOCAL oButtSize
   LOCAL nX
   LOCAL nY

   // get frame width
   nFrameWidth := ::style():pixelMetric( QStyle_PM_DefaultFrameWidth )

   // to resize button, I need to know its width; this is a "fixed" value. Resizing is related only to the height
   nButtonWidth := ::oHqlButton:sizeHint:width()
   ::oHqlButton:resize( nButtonWidth, ::height() ) //- (nFrameWidth*2) )
   oButtSize := ::oHqlButton:size()

   ::__hqlSetPadding( oButtSize, nFrameWidth )

   // move button to the most right position related with hql-inputDate
   nX := ::rect:right() - oButtSize:width() //- nFrameWidth
   nY := ( ::rect:bottom() + nFrameWidth - oButtSize:height() ) / 2
   ::oHqlButton:move( nX, nY )

   oEvent:accept()

RETURN .F.

// ==================== HIDDEN section ====================
