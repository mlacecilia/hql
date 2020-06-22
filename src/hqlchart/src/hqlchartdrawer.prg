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
#include "hqlchart.ch"

/*!

 \brief Returns a new hql_chartDrawer object instance

*/
FUNCTION hqlChartDrawer( ... )
RETURN hql_chartDrawer():new( ... )

/*!

 \brief define hql_chartDrawer class

   based on http://edspi31415.blogspot.com/2012/09/cartesian-coordinates-to-pixel-screen.html
      where
      A = screen.width
      B = screen.height
      Xmax, Xmin, Ymax, Ymin = data limits
      x = Cartesian coordinate
      y = Cartesian coordinate

      xp = (x - Xmin) * A / (Xmax - Xmin)
      yp = (y - Ymax) * -B / (Ymax - Ymin)

tutorial Qt paint da guardare se utile http://zetcode.com/gui/qt5/painting/

   importante per bar chart https://stackoverflow.com/questions/46229259/scaling-a-chart-to-a-fixed-screen-height-formula?rq=1


   differente calcolo di Y (sembrerebbe) vedi (y-Ymax) contro (y-MinY)
https://stackoverflow.com/questions/35087173/calculating-screen-coordinates-from-cartesian-input-values

private static void CalcScoreToScreen(double screenWidth, double screenHeight, int scoreX, int scoreY, double minX, double maxX, double minY, double maxY)
{
    var screenX = (scoreX - minX)/(maxX-minX)*screenWidth;
    var screenY = screenHeight - (scoreY - minY)/(maxY-minY)*screenHeight;

    Console.WriteLine("Screen X: {0} - Score: {1}", screenX, scoreX);
    Console.WriteLine("Screen Y: {0} - Score: {1}", screenY, scoreY);
}

private static void CalcScreenToScore(double screenWidth, double screenHeight, double screenX, double screenY, double minX, double maxX, double minY, double maxY)
{
    int scoreX = Convert.ToInt32(minX + (maxX-minX)*screenX/screenWidth );
    int scoreY = Convert.ToInt32(minY + (maxY-minY)*(screenHeight - screenY)/screenHeight );

    Console.WriteLine("Score X: {0} - Screen X: {1}", scoreX, screenX);
    Console.WriteLine("Score Y: {0} - Screen Y: {1}", scoreY, screenY);
}

*/
CLASS hql_chartDrawer INHERIT hql_qobject

   EXPORTED:
   METHOD init
   METHOD hqlDraw
   METHOD hqlSetChartData
   METHOD hqlSetChartType

   PROTECTED:
   VAR oCanvas                            INIT NIL    // Qt
   VAR oMainMargins                       INIT NIL    // Qt
   VAR oPainter                           INIT NIL    // Qt
   VAR oChartData                         INIT NIL
   VAR nChartType                         INIT HQL_CHART_LINE
   VAR oXaxis                             INIT NIL
   VAR oYaxis                             INIT NIL
   METHOD __hqlCleaner

   METHOD __hqlCustomize
   METHOD __hqlDraw
   METHOD __hqlDraw_axes
   METHOD __hqlDraw_bars
   METHOD __hqlDraw_lines
   METHOD __hqlDraw_pies
   METHOD __hqlDraw_points
   METHOD __hqlDraw_test
   METHOD __hqlFillBackGround
   METHOD __hqlGetNewArea
   METHOD __hqlSetAxes
   METHOD __hqlSetAxes_barVert
   METHOD __hqlSetMainMargins

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( ... ) CLASS hql_chartDrawer
   ::hql_qobject:init( ... )
   ::oChartData := hqlChartData()
   ::oXaxis := hqlChartAxis( /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ )
   ::oYaxis := hqlChartAxis( /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ )
RETURN self

/*!

 \brief draw
 \param(IN) QRect, QPainter
 \return self

*/
METHOD hqlDraw( oRect, oPainter ) CLASS hql_chartDrawer

   IF ( !oRect:isEmpty() )

      ::oCanvas := oRect
      ::oPainter := oPainter

      ::oPainter:save()
      ::oPainter:setRenderHint( QPainter_Antialiasing, .T. )
      ::__hqlFillBackGround()
      IF ( !::oChartData:isEmpty() )
         ::__hqlDraw()
      ENDIF
      ::oPainter:restore()
   ENDIF

   ::oCanvas := NIL
   ::oPainter := NIL
RETURN self

/*!

 \brief add data to be drawn
 \param(IN) hqlChartData
 \return self

*/
METHOD hqlSetChartData( oData ) CLASS hql_chartDrawer
   IF ( hb_IsObject( oData ) .AND. oData:isDerivedFrom("hql_chartData") )
      ::oChartData := oData
   ENDIF
RETURN self

/*!

 \brief set chart type
 \param[in] numeric
 \return Self

*/
METHOD hqlSetChartType( arg1 ) CLASS hql_chartDrawer
   IF ( hb_IsNumeric(arg1) )
      SWITCH arg1
      CASE HQL_CHART_LINE
      CASE HQL_CHART_POINT
      CASE HQL_CHART_BAR
      CASE HQL_CHART_PIE
      CASE HQL_CHART_TEST
         ::nChartType := arg1
         EXIT
      ENDSWITCH
   ENDIF
RETURN Self

// ==================== PROTECTED section ====================

/*

 \brief [PROTECTED] Object cleaner
 \param(IN)
 \return NIL

*/
METHOD __hqlCleaner() CLASS hql_chartDrawer
   ::oCanvas := NIL
   ::oPainter := NIL
   ::oMainMargins := NIL
   ::hql_qobject:__hqlCleaner()
RETURN NIL

/*!

 \brief [PROTECTED] customization
 \param(IN)
 \return NIL

*/
METHOD __hqlCustomize() CLASS hql_chartDrawer
RETURN NIL

/*!

 \brief [PROTECTED] draw
 \param(IN)
 \return NIL

*/
METHOD __hqlDraw() CLASS hql_chartDrawer

   ::__hqlSetMainMargins()

   SWITCH ::nChartType
   CASE HQL_CHART_LINE
      ::__hqlDraw_lines()
      EXIT

   CASE HQL_CHART_POINT
      ::__hqlDraw_points()
      EXIT

   CASE HQL_CHART_BAR
      ::__hqlDraw_bars()
      EXIT

   CASE HQL_CHART_PIE
      ::__hqlDraw_pies()
      EXIT

   CASE HQL_CHART_TEST
      ::__hqlDraw_test()
      EXIT
   ENDSWITCH

RETURN NIL

/*!

 \brief [PROTECTED] draw axes
 \param(IN)
 \return NIL

*/
METHOD __hqlDraw_axes( oRect ) CLASS hql_chartDrawer
   LOCAL oPen, nTickCount, nTickSpace, nTick
   LOCAL nX1, nY1, nX2, nY2

   ::oPainter:save()
   oPen := ::oPainter:pen()
   oPen:setWidth( 1 )
   oPen:setColor( QColor( "#000000" ) )
   oPen:setStyle( Qt_SolidLine )
   ::oPainter:setPen( oPen )

   ::oPainter:drawLine( ::oXaxis:x1(), ::oXaxis:y1(), ::oXaxis:x2(), ::oXaxis:y2() )
   ::oPainter:drawLine( ::oYaxis:x1(), ::oYaxis:y1(), ::oYaxis:x2(), ::oYaxis:y2() )

   /* draw ticks X axis */
   nTickSpace := ::oXaxis:niceTickSpace()
   nTickCount := ::oXaxis:niceRange() / nTickSpace  /* remember is -1 */
   FOR nTick := 0 TO nTickCount
      nX1 := nX2 := ::oXaxis:x1() + ( ( nTick * nTickSpace ) * oRect:width() / ::oXaxis:niceRange() )
      nY1 := ::oXaxis:y1() - 3
      nY2 := ::oXaxis:y1() + 3
      ::oPainter:drawLine( nX1, nY1, nX2, nY2 )
   NEXT nTick

   /* draw ticks Y axis */
   nTickSpace := ::oYaxis:niceTickSpace()
   nTickCount := ::oYaxis:niceRange() / nTickSpace  /* remember is -1 */
   FOR nTick := 0 TO nTickCount
      nY1 := nY2 := ::oYaxis:y1() + ( ( nTick * nTickSpace ) * oRect:height() / ::oYaxis:niceRange() )
      nX1 := ::oYaxis:x1() - 3
      nX2 := ::oYaxis:x1() + 3
      ::oPainter:drawLine( nX1, nY1, nX2, nY2 )
   NEXT nTick

   ::oPainter:restore()

RETURN NIL

/*!

 \brief [PROTECTED] draw bars
 \param(IN)
 \return NIL

   base
      For everything conforming to a xxx pixel size, first find the maximum of the user inputs.
      xxx/Max = ScaleFactor. Multiply this scale factor times the height of each to find their scaled height.

   for Horizontal Histogram
      vertical spacing of item w = d / n; where: d is the height of widget and n is the number of items
      horizontal scale S = l / Vmax; where: l is the width of widget and Vmax is the max. value

   for Vertical Histogram
      horizontal spacing of item w = l / n; where: l is the width of widget and n is the number of items
      vertical scale S = d / Vmax where: d is height of widget and Vmax is the max. value

*/
METHOD __hqlDraw_bars() CLASS hql_chartDrawer
   LOCAL oDrawArea, nPieceCount, lUniqueColor
   LOCAL nBarWidth, nBarHeight, nVmax, nScale, nXstart
   LOCAL nSerie, oSerie, nPiece, oPiece
   LOCAL nX, nY, oBrush

   /* serie[s] need the same pieceCount() */
   nPieceCount := ::oChartData:at(1):pieceCount()
   FOR nSerie := 1 TO ::oChartData:serieCount()
      IF ( nPieceCount != ::oChartData:at(nSerie):pieceCount() )
         RETURN NIL
      ENDIF
   NEXT

   oDrawArea := ::__hqlGetNewArea( ::oCanvas, ::oMainMargins )

   ::__hqlSetAxes_barVert( oDrawArea )
   ::__hqlDraw_axes( oDrawArea )

   lUniqueColor := IIF ( ::oChartData:serieCount() > 1, .T., .F. )

   nBarWidth := oDrawArea:width() / ::oChartData:pieceCount()
   IF ( ::oChartData:minHeight() > 0 ) /* all positive values */
      nVmax := ::oYaxis:niceMax()
   ELSEIF ( ::oChartData:maxHeight() < 0 ) /* all negative values */
      nVmax := ABS(::oYaxis:niceMin())
   ELSE
      nVmax := ::oYaxis:niceRange()
   ENDIF
   nScale := oDrawArea:height() / nVmax

   nXstart := 0
   FOR nPiece := 1 TO nPieceCount
      FOR nSerie := 1 TO ::oChartData:serieCount()
         oSerie := ::oChartData:at(nSerie)
         oPiece := oSerie:at(nPiece)
         nBarHeight := oPiece:height() * nScale
         IF ( lUniqueColor )
            oBrush := QBrush( QColor( oSerie:color() ) )
         ELSE
            oBrush := QBrush( QColor( oPiece:color() ) )
         ENDIF
         nX := nXstart + ::oXaxis:x1()
         nY := IIF ( nBarHeight >= 0, (::oXaxis:y1() - nBarHeight), ::oXaxis:y1() )
         /* paint rectangle */
         ::oPainter:save()
         ::oPainter:setPen( Qt_NoPen )
         IF ( nBarHeight >= 0 )
            ::oPainter:fillRect( nX, nY, nBarWidth, nBarHeight, oBrush )
         ELSE
            ::oPainter:fillRect( nX, nY, nBarWidth, ABS(nBarHeight), oBrush )
         ENDIF
         ::oPainter:restore()
         nXstart += nBarWidth
      NEXT nSerie
   NEXT nPiece

RETURN NIL

/*!

 \brief [PROTECTED] draw lines
 \param(IN)
 \return NIL

*/
METHOD __hqlDraw_lines() CLASS hql_chartDrawer
   LOCAL oDrawArea
   LOCAL nSerie, oSerie, nPiece, oPiece
   LOCAL nX, nY, oP1, oP2, oPen, oBrush

   oDrawArea := ::__hqlGetNewArea( ::oCanvas, ::oMainMargins )

   ::__hqlSetAxes( oDrawArea )
   ::__hqlDraw_axes( oDrawArea )

   FOR nSerie := 1 TO ::oChartData:serieCount()
      IF ( ::oChartData:at(nSerie):pieceCount() > 0 )
         /* first point for current serie */
         oSerie := ::oChartData:at(nSerie)
         oPiece := oSerie:at(1)
         /* xp = (x - Xmin) * A / (Xmax - Xmin) */
         nX := ::oXaxis:x1() + ( (oPiece:width() - ::oXaxis:niceMin()) * oDrawArea:width() / ::oXaxis:niceRange() )
         /* yp = (y - Ymax) * -B / (Ymax - Ymin) */
         nY := ::oYaxis:y1() + ( (oPiece:height() - ::oYaxis:niceMax()) * (oDrawArea:height() * -1) / ::oYaxis:niceRange() )
         oP1 := QPointF( nX, nY )
         /* paint point square */
         oPen := ::oPainter:pen()
         oPen:setWidth( 1 )
         oPen:setColor( QColor( oPiece:color() ) )
         oBrush := QBrush( QColor( oPiece:color() ) )
         ::oPainter:save()
         ::oPainter:setPen( oPen )
         ::oPainter:setBrush( oBrush )
         ::oPainter:drawEllipse( oP1:x()-2, oP1:y()-2, 5, 5 )
         ::oPainter:restore()

         FOR nPiece := 2 TO oSerie:size()
            oPiece := oSerie:at(nPiece)
            /* xp = (x - Xmin) * A / (Xmax - Xmin) */
            nX := ::oXaxis:x1() + ( (oPiece:width() - ::oXaxis:niceMin()) * oDrawArea:width() / ::oXaxis:niceRange() )
            /* yp = (y - Ymax) * -B / (Ymax - Ymin) */
            nY := ::oYaxis:y1() + ( (oPiece:height() - ::oYaxis:niceMax()) * (oDrawArea:height() * -1) / ::oYaxis:niceRange() )
            oP2 := QPointF( nX, nY )
            /* paint point square */
            oPen := ::oPainter:pen()
            oPen:setWidth( 1 )
            oPen:setColor( QColor( oPiece:color() ) )
            oBrush := QBrush( QColor( oPiece:color() ) )
            ::oPainter:save()
            ::oPainter:setPen( oPen )
            ::oPainter:setBrush( oBrush )
            ::oPainter:drawEllipse( oP2:x()-2, oP2:y()-2, 5, 5 )
            ::oPainter:restore()

            /* paint line between p1 and p2 */
            oPen := ::oPainter:pen()
            oPen:setWidth( 1 )
            oPen:setColor( QColor( oSerie:color() ) )
            oBrush := QBrush( QColor( oSerie:color() ) )
            ::oPainter:save()
            ::oPainter:setPen( oPen )
            ::oPainter:setBrush( oBrush )
            ::oPainter:drawLine( oP1, oP2 )
            ::oPainter:restore()

            /* set start point to the last calculated */
            oP1:setX( oP2:x() )
            oP1:setY( oP2:y() )

         NEXT nPiece
      ENDIF /* serie is not empty */
   NEXT nSerie

RETURN NIL

/*!

 \brief [PROTECTED] draw pies
 \param(IN)
 \return NIL

*/
METHOD __hqlDraw_pies() CLASS hql_chartDrawer
   LOCAL oDrawArea
   LOCAL nSize, oPieArea
   LOCAL nSerie, nPiece
   LOCAL nTotalArea, nStartAngle, nEndAngle, nAngle, nPercent
   LOCAL oBrush

   IF ( ::oChartData:maxHeight() < 0 .OR. ::oChartData:minHeight() < 0 )   /* can't be draw pie with negative values */
      RETURN NIL
   ENDIF

   oDrawArea := ::__hqlGetNewArea( ::oCanvas, ::oMainMargins )

   /* ::__hqlSetAxes( oDrawArea )
      ::__hqlDraw_axes( oDrawArea ) */

   nSize := MIN( oDrawArea:width(), oDrawArea:height() )
   oPieArea := QRect( 0, 0, nSize, nSize )
   oPieArea:moveCenter( oDrawArea:center() )

   FOR nSerie := 1 TO ::oChartData:serieCount()

      IF ( ::oChartData:at(nSerie):pieceCount() > 0 )
         /* find total serie area using abs value */
         nTotalArea := 0
         FOR nPiece := 1 TO ::oChartData:at(nSerie):pieceCount()
            nTotalArea += ::oChartData:at(nSerie):at(nPiece):absHeight()
         NEXT nPiece

         nStartAngle := 180
         FOR nPiece := 1 TO ::oChartData:at(nSerie):pieceCount()
            nPercent := ::oChartData:at(nSerie):at(nPiece):absHeight() / nTotalArea
            nAngle := nPercent * 360
            nEndAngle := nStartAngle + nAngle

            oBrush := QBrush( QColor( ::oChartData:at(nSerie):at(nPiece):color() ) )
            ::oPainter:save()
            ::oPainter:setPen( Qt_NoPen )
            ::oPainter:setBrush( oBrush )
            ::oPainter:drawPie( oPieArea:left(), oPieArea:top(), oPieArea:width(), oPieArea:height(), (nStartAngle*16), (nAngle*16) )
            ::oPainter:restore()
            nStartAngle := nEndAngle
         NEXT nPiece

         nSize := MIN( oDrawArea:width(), oDrawArea:height() ) * ( nSerie / ::oChartData:serieCount() )
         oPieArea := QRect( 0, 0, nSize, nSize )
         oPieArea:moveCenter( oDrawArea:center() )
      ENDIF

   NEXT nSerie

RETURN NIL

/*!

 \brief [PROTECTED] draw points
 \param(IN)
 \return NIL

*/
METHOD __hqlDraw_points() CLASS hql_chartDrawer
   LOCAL oDrawArea
   LOCAL nSerie, oSerie, nPiece, oPiece
   LOCAL nX, nY, oP1, oPen, oBrush

   oDrawArea := ::__hqlGetNewArea( ::oCanvas, ::oMainMargins )

   ::__hqlSetAxes( oDrawArea )
   ::__hqlDraw_axes( oDrawArea )

   FOR nSerie := 1 TO ::oChartData:serieCount()
      IF ( ::oChartData:at(nSerie):pieceCount() > 0 )
         oSerie := ::oChartData:at(nSerie)
         FOR nPiece := 1 TO oSerie:size()
            oPiece := oSerie:at(nPiece)
            /* xp = (x - Xmin) * A / (Xmax - Xmin) */
            nX := ::oXaxis:x1() + ( (oPiece:width() - ::oXaxis:niceMin()) * oDrawArea:width() / ::oXaxis:niceRange() )
            /* yp = (y - Ymax) * -B / (Ymax - Ymin) */
            nY := ::oYaxis:y1() + ( (oPiece:height() - ::oYaxis:niceMax()) * (oDrawArea:height() * -1) / ::oYaxis:niceRange() )
            oP1 := QPointF( nX, nY )
            /* paint point square */
            oPen := ::oPainter:pen()
            oPen:setWidth( 1 )
            IF ( ::oChartData:serieCount() > 1 )
               oPen:setColor( QColor( oSerie:color() ) )
               oBrush := QBrush( QColor( oSerie:color() ) )
            ELSE
               oPen:setColor( QColor( oPiece:color() ) )
               oBrush := QBrush( QColor( oPiece:color() ) )
            ENDIF
            ::oPainter:save()
            ::oPainter:setPen( oPen )
            ::oPainter:setBrush( oBrush )
            ::oPainter:drawEllipse( oP1:x()-2, oP1:y()-2, 5, 5 )
            ::oPainter:restore()
         NEXT nPiece
      ENDIF
   NEXT nSerie

RETURN NIL

/*!

 \brief [PROTECTED] draw box
 \param(IN)
 \return NIL

*/
METHOD __hqlDraw_test() CLASS hql_chartDrawer
RETURN NIL

/*!

 \brief [PROTECTED] fill background
 \param(IN)
 \return NIL

*/
METHOD __hqlFillBackGround() CLASS hql_chartDrawer
   ::oPainter:save()
   ::oPainter:setPen( Qt_NoPen )
   ::oPainter:fillRect( ::oCanvas, QColor( "#FFFFFF" ) )
   ::oPainter:restore()
RETURN NIL

/*!

 \brief [PROTECTED] returns rectangle reduced by margins
 \param(IN) QRect, QMargins
 \return QRect

*/
METHOD __hqlGetNewArea( oRect, oMargins ) CLASS hql_chartDrawer
RETURN ( oRect:marginsRemoved( oMargins ) )

/*!

 \brief [PROTECTED] set axes
 \param(IN) QRect
 \return NIL

*/
METHOD __hqlSetAxes( oRect ) CLASS hql_chartDrawer
   LOCAL oScale
   LOCAL nX1, nY1, nX2, nY2, nZero

   /* X axis settings */
   oScale := hqlChartVbaScale( /*nMinimum*/, /*nMaximum*/ )
   oScale:setLimits( ::oChartData:minWidth(), ::oChartData:maxWidth() )
//UDFdbgNiceScale( oScale, "vbaScale X axis", 0 )
   WITH OBJECT ::oXaxis := hqlChartAxis( /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ )
      :setLimits( ::oChartData:minWidth(), ::oChartData:maxWidth() )
      :setNice( oScale:niceMin(), oScale:niceMax(), oScale:majorTickSpace() )
   END WITH

   /* Y axis settings */
   oScale := hqlChartVbaScale( /*nMinimum*/, /*nMaximum*/ )
   oScale:setLimits( ::oChartData:minHeight(), ::oChartData:maxHeight() )
//UDFdbgNiceScale( oScale, "vbaScale Y axis", 0 )
   WITH OBJECT ::oYaxis := hqlChartAxis( /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ )
      :setLimits( ::oChartData:minHeight(), ::oChartData:maxHeight() )
      :setNice( oScale:niceMin(), oScale:niceMax(), oScale:majorTickSpace() )
   END WITH

   /* X axis settings */
   IF ( ::oYaxis:niceMin() >= 0 )   /* all positive values */
      nY1 := nY2 := oRect:bottom()
   ELSEIF ( ::oYaxis:niceMax() <= 0 )   /* all negative values */
      nY2 := nY1 := oRect:top()
   ELSE
      /* yp = (y - Ymax) * -B / (Ymax - Ymin) */
      nZero := 0
      nY1 := nY2 := oRect:top() + ( (nZero - ::oYaxis:niceMax()) * (oRect:height() * -1) / ::oYaxis:niceRange() )
   ENDIF
   nX1 := oRect:left()
   nX2 := oRect:right()
   ::oXaxis:setCoordinates( nX1, nY1, nX2, nY2 )
//UDFdbgAxis( ::oXaxis, "X axis", 0 )

   /* Y axis settings */
   IF ( ::oXaxis:niceMin() >= 0 )   /* all positive values */
      nX2 := nX1 := oRect:left()
   ELSEIF ( ::oXaxis:niceMax() <= 0 )   /* all negative values */
      nX2 := nX1 := oRect:right()
   ELSE
      /* xp = (x - Xmin) * A / (Xmax - Xmin) */
      nZero := 0
      nX1 := nX2 := oRect:left() + ( (nZero - ::oXaxis:niceMin()) * oRect:width() / ::oXaxis:niceRange() )
   ENDIF
   nY1 :=  oRect:top()
   nY2 :=  oRect:bottom()
   ::oYaxis:setCoordinates( nX1, nY1, nX2, nY2 )
//UDFdbgAxis( ::oYaxis, "Y axis", 0 )

RETURN NIL

/*!

 \brief [PROTECTED] set axes for vertical bars
 \param(IN) QRect
 \return NIL

*/
METHOD __hqlSetAxes_barVert( oRect ) CLASS hql_chartDrawer
   LOCAL oScale
   LOCAL nX1, nY1, nX2, nY2, nZero

   /* X axis settings */
   WITH OBJECT ::oXaxis := hqlChartAxis( /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ )
      :setLimits( 1, ::oChartData:pieceCount() )
      :setNice( 0, oRect:width(), (oRect:width() / ::oChartData:pieceCount()) )
   END WITH

   /* Y axis settings */
   oScale := hqlChartVbaScale( /*nMinimum*/, /*nMaximum*/ )
   oScale:setLimits( ::oChartData:minHeight(), ::oChartData:maxHeight() )
//UDFdbgNiceScale( oScale, "vbaScale Y axis", 0 )
   WITH OBJECT ::oYaxis := hqlChartAxis( /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ )
      :setLimits( ::oChartData:minHeight(), ::oChartData:maxHeight() )
      :setNice( oScale:niceMin(), oScale:niceMax(), oScale:majorTickSpace() )
   END WITH

   /* X axis settings */
   IF ( ::oYaxis:niceMin() >= 0 )   /* all positive values */
      nY1 := nY2 := oRect:bottom()
   ELSEIF ( ::oYaxis:niceMax() <= 0 )   /* all negative values */
      nY2 := nY1 := oRect:top()
   ELSE
      /* yp = (y - Ymax) * -B / (Ymax - Ymin) */
      nZero := 0
      nY1 := nY2 := oRect:top() + ( (nZero - ::oYaxis:niceMax()) * (oRect:height() * -1) / ::oYaxis:niceRange() )
   ENDIF
   nX1 := oRect:left()
   nX2 := oRect:right()
   ::oXaxis:setCoordinates( nX1, nY1, nX2, nY2 )
//UDFdbgAxis( ::oXaxis, "X axis", 0 )

   /* Y axis settings */
   IF ( ::oXaxis:niceMin() >= 0 )   /* all positive values */
      nX2 := nX1 := oRect:left()
   ELSEIF ( ::oXaxis:niceMax() <= 0 )   /* all negative values */
      nX2 := nX1 := oRect:right()
   ELSE
      /* xp = (x - Xmin) * A / (Xmax - Xmin) */
      nZero := 0
      nX1 := nX2 := oRect:left() + ( (nZero - ::oXaxis:niceMin()) * oRect:width() / ::oXaxis:niceRange() )
   ENDIF
   nY1 :=  oRect:top()
   nY2 :=  oRect:bottom()
   ::oYaxis:setCoordinates( nX1, nY1, nX2, nY2 )
//UDFdbgAxis( ::oYaxis, "Y axis", 0 )

RETURN NIL

/*!

 \brief [PROTECTED] set main margins
 \param(IN)
 \return NIL

*/
METHOD __hqlSetMainMargins() CLASS hql_chartDrawer
   LOCAL oFm := ::oPainter:fontMetrics( ::oPainter:font() )
   ::oMainMargins := QMargins( oFm:height(), oFm:height(), oFm:height(), oFm:height() )
RETURN NIL
