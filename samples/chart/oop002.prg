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
#include "hqlchart.ch"

PROCEDURE UDFdrawerLines( oWidget )
   LOCAL oData := hqlChartData( /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ )

   oData:addSerie( UDFmaschi() )
   oData:addSerie( UDFfemmine() )

   oWidget:hqlSetChartData( oData )

   oWidget:hqlSetChartType( HQL_CHART_LINE )

   //oWidget:hqlSetChartType( /*HQL_CHART_LINE = default*/ )
   //oWidget:hqlSetChartType( HQL_CHART_POINT )
   //oWidget:hqlSetChartType( HQL_CHART_BAR )
   //oWidget:hqlSetChartType( HQL_CHART_PIE )
   //oWidget:hqlSetChartType( HQL_CHART_TEST )

   oWidget:update()

RETURN

PROCEDURE UDFdrawerPoints( oWidget )
   LOCAL oData := hqlChartData( /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ )

   oData:addSerie( UDFmaschi() )
   oData:addSerie( UDFfemmine() )

   oWidget:hqlSetChartData( oData )

   oWidget:hqlSetChartType( HQL_CHART_POINT )

   oWidget:update()

RETURN

PROCEDURE UDFdrawerPies( oWidget )
   LOCAL oData := hqlChartData( /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ )

   /* WARNING only positive values can be painted */
   oData:addSerie( UDFbasePos() )
   oData:addSerie( UDFbasePos_B() )

   oWidget:hqlSetChartData( oData )

   oWidget:hqlSetChartType( HQL_CHART_PIE )

   oWidget:update()

RETURN

PROCEDURE UDFdrawerBars( oWidget )
   LOCAL oData := hqlChartData( /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ )

   oData:addSerie( UDFbasePos() )
   oData:addSerie( UDFbaseNeg() )
   oData:addSerie( UDFbaseMix() )

   oWidget:hqlSetChartData( oData )

   oWidget:hqlSetChartType( HQL_CHART_LINE )

   //oWidget:hqlSetChartType( /*HQL_CHART_LINE = default*/ )
   //oWidget:hqlSetChartType( HQL_CHART_POINT )
   //oWidget:hqlSetChartType( HQL_CHART_BAR )
   //oWidget:hqlSetChartType( HQL_CHART_PIE )
   //oWidget:hqlSetChartType( HQL_CHART_TEST )

   oWidget:update()

RETURN

/* --- series --- */

FUNCTION UDFmaschi()
   LOCAL oSerie

   oSerie := hqlChartSerie( "maschi", "#0000FF", /*cText*/, /*cTooltip*/ )
   oSerie:addPiece( hqlChartPiece( 24669, 1961, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 24829, 1962, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 25001, 1963, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 25187, 1964, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 25408, 1965, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 25608, 1966, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 25803, 1967, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 25975, 1968, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 26122, 1969, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 26260, 1970, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )

RETURN oSerie

FUNCTION UDFfemmine()
   LOCAL oSerie

   oSerie := hqlChartSerie( "femmine", "#FF0000", /*cText*/, /*cTooltip*/ )
   oSerie:addPiece( hqlChartPiece( 25705, 1961, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 25870, 1962, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 26060, 1963, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 26257, 1964, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 26499, 1965, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 26710, 1966, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 26917, 1967, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 27106, 1968, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 27268, 1969, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( 27425, 1970, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )

RETURN oSerie

FUNCTION UDFbasePos()
   LOCAL oSerie

   oSerie := hqlChartSerie( /*cName*/, "#00FF00", /*cText*/, /*cTooltip*/ )
   oSerie:addPiece( hqlChartPiece(   5, 100, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece(  10, 200, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece(  20, 300, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece(  40, 400, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )

RETURN oSerie

FUNCTION UDFbasePos_B()
   LOCAL oSerie

   oSerie := hqlChartSerie( /*cName*/, "#00FFFF", /*cText*/, /*cTooltip*/ )
   oSerie:addPiece( hqlChartPiece(  10, 100, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece(  25, 200, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece(  35, 300, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece(  45, 400, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )

RETURN oSerie

FUNCTION UDFbaseNeg()
   LOCAL oSerie

   oSerie := hqlChartSerie( /*cName*/, "#FF0000", /*cText*/, /*cTooltip*/ )
   oSerie:addPiece( hqlChartPiece(  -5, 100, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( -10, 200, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( -20, 300, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( -40, 400, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )

RETURN oSerie

FUNCTION UDFbaseMix()
   LOCAL oSerie

   oSerie := hqlChartSerie( /*cName*/, "#0000FF", /*cText*/, /*cTooltip*/ )
   oSerie:addPiece( hqlChartPiece(   7, 100, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( -15, 200, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece(  25, 300, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece( -30, 400, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )
   oSerie:addPiece( hqlChartPiece(  35, 500, /*cName*/, /*cColor*/, /*cText*/, /*cTooltip*/ ) )

RETURN oSerie

/* --- debug --- */

PROCEDURE UDFdbgAxis( oAxis, ctext, ntab )
   ctext := hb_DefaultValue(ctext, "")
   ntab := hb_DefaultValue(ntab, 0)
   hql_Trace( SPACE(ntab) + ctext + " " + ;
                     "minimum: " + hb_NtoS(oAxis:minimum()) + " " + ;
                     "maximum: " + hb_NtoS(oAxis:maximum()) + " " + ;
                     "range: " + hb_NtoS(oAxis:range()) + " " + ;
                     "niceMin: " + hb_NtoS(oAxis:niceMin()) + " " + ;
                     "niceMax: " + hb_NtoS(oAxis:niceMax()) + " " + ;
                     "niceRange: " + hb_NtoS(oAxis:niceRange()) + " " + ;
                     "niceTickSpace: " + hb_NtoS(oAxis:niceTickSpace()) + " " + ;
                     "x1: " + hb_NtoS(oAxis:x1()) + " " + ;
                     "y1: " + hb_NtoS(oAxis:y1()) + " " + ;
                     "x2: " + hb_NtoS(oAxis:x2()) + " " + ;
                     "y2: " + hb_NtoS(oAxis:y2()) )
RETURN

PROCEDURE UDFdbgNiceScale( oScale, ctext, ntab )
   ctext := hb_DefaultValue(ctext, "")
   ntab := hb_DefaultValue(ntab, 0)
   hql_Trace( SPACE(ntab) + ctext + " " + ;
                     "minimum: " + hb_NtoS(oScale:minimum()) + " " + ;
                     "maximum: " + hb_NtoS(oScale:maximum()) + " " + ;
                     "range: " + hb_NtoS(oScale:range()) + " " + ;
                     "niceMin: " + hb_NtoS(oScale:niceMin()) + " " + ;
                     "niceMax: " + hb_NtoS(oScale:niceMax()) + " " + ;
                     "niceRange: " + hb_NtoS(oScale:niceRange()) + " " + ;
                     "majorTickSpace: " + hb_NtoS(oScale:majorTickSpace()) + " " + ;
                     "minorTickSpace: " + hb_NtoS(oScale:minorTickSpace()) )
RETURN
