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

 \brief Returns a new hql_chartWidget object instance

*/
FUNCTION hqlChartWidget( ... )
RETURN hql_chartWidget():new( ... )

/*!

 \brief define hql_chartWidget class

*/
CLASS hql_chartWidget INHERIT hql_widget

   EXPORTED:
   METHOD init
   METHOD hqlSavePng
   METHOD hqlSetChartData
   METHOD hqlSetChartType

   PROTECTED:
   VAR oDrawer                            INIT NIL
   METHOD __hqlCleaner

   METHOD __hqlCustomize
   METHOD __hqlDrawFrame

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( ... ) CLASS hql_chartWidget

   ::hql_widget:init( ... )

   ::oDrawer := hqlChartDrawer( "hql_chartDrawer", self )

   ::__hqlCustomize()

RETURN self

/*!

 \brief

*/
METHOD hqlSavePng( arg1, arg2 ) CLASS hql_chartWidget
   LOCAL cFileName
   LOCAL oQimage, oQPainter, oQSize

   IF ( hb_IsString(arg1) )
      cFileName := arg1
   ELSE
      /* cFileName := hb_FnameMerge( hb_DirBase(), hb_TtoC( hb_DateTime(), "YYYYMMDD", "HHMMSSFFF" ), "png" ) */
      cFileName := hb_FnameMerge( hb_DirBase(), hb_TtoS( hb_DateTime() ), "png" )
   ENDIF

   IF ( hb_IsObject(arg2) .AND. ( arg2:className() == UPPER( "Qsize" ) .OR. arg2:className() == UPPER( "QsizeF" ) ) )
      oQSize := QsizeF( arg2:width(), arg2:height() )
   ELSE
      oQSize := QsizeF( self:drawarea():width(), self:drawarea():height() )
   ENDIF

   oQimage := QImage( oQSize:width(), oQSize:height(), QImage_Format_ARGB32 )

   oQPainter := QPainter()
   oQPainter:begin( oQimage )
   ::oDrawer:hqlDraw( QRect( 0, 0, oQSize:width(), oQSize:height() ), oQPainter )
   oQPainter:end()

   oQimage:save( cFileName )

RETURN Self

/*!

 \brief add data to be drawn
 \param(IN) hqlChartData
 \return self

*/
METHOD hqlSetChartData( ... ) CLASS hql_chartWidget
   ::oDrawer:hqlSetChartData( ... )
RETURN self

/*!

 \brief set chart type
 \param[in] numeric
 \return Self

*/
METHOD hqlSetChartType( ... ) CLASS hql_chartWidget
   ::oDrawer:hqlSetChartType( ... )
RETURN Self

// ==================== PROTECTED section ====================


/*

 \brief [PROTECTED] Object cleaner
 \param(IN)
 \return NIL

*/
METHOD __hqlCleaner() CLASS hql_chartWidget
   ::oDrawer := NIL
   ::hql_widget:__hqlCleaner()
RETURN NIL

/*!

 \brief [PROTECTED]
 \param(IN)
 \return NIL

*/
METHOD __hqlCustomize() CLASS hql_chartWidget
   LOCAL oVlayout

   oVlayout := hqlVBoxLayout()
   oVlayout:setContentsMargins( 0, 0, 0, 0 )

   ::setLayout( oVlayout )

   WITH OBJECT hqlFrame("drawarea", self)
      :hqlAddMeToLayout( oVlayout )
      :setSizePolicy( QSizePolicy_Expanding, QSizePolicy_Expanding )
      :connect( QEvent_Paint,  { |oEvent,oPainter| ::__hqlDrawFrame(oEvent,oPainter) } )
   END WITH

RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [INTERNAL] handle event or signal
 \param[in] ...
 \return ...

*/
SIGNAL __hqlDrawFrame(oEvent, oPainter) CLASS hql_chartWidget

   oPainter:save()

   ::oDrawer:hqlDraw( oEvent:rect(), oPainter )

   oPainter:restore()

RETURN .T.  //  I M P O R T A N T

// ==================== HIDDEN section ====================
