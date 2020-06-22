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
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicdp.h"
#include "hbapierr.h"
#include "hbmather.h"
#include "error.ch"
#include "hqlmath.ch"

/*
 * Computes the sine
 *    hql_Sin( <nArc> ) -> nSine
 * Argument[s]: nArc Designates a radian value for which the sine is determined.
 * Returns: nSine the sine value specified in nArc
 */
HB_FUNC( HQL_SIN )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = sin( dArg );
      if( hb_mathGetError( &hb_exc, "SIN", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
            hb_retndlen( HUGE_VAL, -1, -1 );
      }
      else
         hb_retnd( dResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Computes the cosine
 *    hql_Cos( <nArc> ) -> nCosine
 * Argument[s]: nArc Designates a radian value for which the cosine is determined.
 * Returns: nCosine the cosine value specified in nArc
 */
HB_FUNC( HQL_COS )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = cos( dArg );
      if( hb_mathGetError( &hb_exc, "COS", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
            hb_retndlen( HUGE_VAL, -1, -1 );
      }
      else
         hb_retnd( dResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Tangent of the argument
 *    hql_Tan( nRadiant ) -> nTangent
 * Argument[s]: nRadiant an angle size given in radiants
 * Returns: nTangent the tangent of nRadiant
 */
HB_FUNC( HQL_TAN )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = tan( dArg );
      if( hb_mathGetError( &hb_exc, "TAN", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
            hb_retndlen( HUGE_VAL, -1, -1 );
      }
      else
         hb_retnd( dResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Cotangent of the argument
 *    hql_Cot( nRadiant ) -> nCotangent
 * Argument[s]: nRadiant an angle size given in radiants
 * Returns: nCotangent the cotangent of nRadiant
 */
HB_FUNC( HQL_COT )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = tan( dArg );
      if( hb_mathGetError( &hb_exc, "TAN", dArg, 0.0, dResult ) )
         dResult = hb_exc.handled ? hb_exc.retval : 0.0;

      dResult = dResult ? 1 / dResult : HUGE_VAL;
      hb_retnd( dResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Arcus sine of the argument
 *    hql_Asin( nSine ) -> nRadiant
 * Argument[s]: nSine the sine of an angle
 * Returns nRadiant the angle whose sine is nSine
 */
HB_FUNC( HQL_ASIN )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = asin( dArg );
      if( hb_mathGetError( &hb_exc, "ASIN", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
            hb_retndlen( HUGE_VAL, -1, -1 );
      }
      else
         hb_retnd( dResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Arcus cosine of the argument
 *    hql_Acos( nCosine ) -> nRadiant
 * Argument[s]: nCosine the cosine of an angle
 * Returns nRadiant the angle whose cosine is nCosine
 */
HB_FUNC( HQL_ACOS )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = acos( dArg );
      if( hb_mathGetError( &hb_exc, "ACOS", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
            hb_retndlen( HUGE_VAL, -1, -1 );
      }
      else
         hb_retnd( dResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Arcus tangent of the argument
 *    hql_Atan( nTangent ) -> nRadiant
 * Argument[s]: nTangent the tangent of an angle
 * Returns: nRadiant the angle whose tangent is nTangent
 */
HB_FUNC( HQL_ATAN )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = atan( dArg );
      if( hb_mathGetError( &hb_exc, "ATAN", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
         {
            /* atan normally don't error, but it's save to return Pi()/2
               or -Pi()/2, respectively, as these
               are the boundary result values */
            if( dArg < 0.0 )
               hb_retnd( -HQLMPI / 2.0 );
            else
               hb_retnd( HQLMPI / 2.0 );
         }
      }
      else
         hb_retnd( dResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Arcus tangent a sine and a cosine argument
 *    hql_Atn2( nSine, nCosine ) -> nRadiant
 * Argument[s]: nSine the sine of an angle, nCosine the cosine of an angle
 * Returns: nRadiant the angle whose tangent is nSine/nCosine
 */
HB_FUNC( HQL_ATN2 )
{
   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dY = hb_parnd( 1 );
      double dX = hb_parnd( 2 );
      double dResult;

      hb_mathResetError( &hb_exc );
      dResult = atan2( dY, dX );
      if( hb_mathGetError( &hb_exc, "ATAN2", dY, dX, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
         {
            /* DOMAIN error: both arguments to atan2 have been 0 */
            /* CTIII behaves very strange here: atn2 (0.0, 0.0) == -PI
               atn2 (0.0, -0.0) == 0.0
               atn2 (-0.0, 0.0) == -PI
               atn2 (-0.0, -0.0) == -2*PI */
            if( dX >= 0.0 )
               hb_retnd( -HQLMPI );
            else if( dY < 0.0 )
               hb_retnd( -2.0 * HQLMPI );
            else
               hb_retnd( 0.0 );
         }
      }
      else
         hb_retnd( dResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Hyperbolic Sine of the argument
 *    hql_Sinh( nArea ) -> nHyperbolicSine
 * Argument[s]: nArea the size of the area
 * Returns: nHyperbolicSine the hyperbolic sine of nArea
 */
HB_FUNC( HQL_SINH )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = sinh( dArg );
      if( hb_mathGetError( &hb_exc, "SINH", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
         {
            /* OVERFLOW error: we have no CTIII behaviour to follow,
               so return +INF or -INF, respectively */
            if( dArg < 0.0 )
               hb_retndlen( -HUGE_VAL, -1, -1 );
            else
               hb_retndlen( HUGE_VAL, -1, -1 );
         }
      }
      else
         hb_retnd( dResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Hyperbolic Cosine of the argument
 *    hql_Cosh( nArea ) -> nHyperbolicCosine
 * Argument[s]: nArea the size of the area
 * Returns: nHyperbolicSine the hyperbolic cosine of nArea
 */
HB_FUNC( HQL_COSH )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = cosh( dArg );
      if( hb_mathGetError( &hb_exc, "COSH", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
            /* OVERFLOW error: we have no CTIII behaviour to follow,
               so return +INF */
            hb_retndlen( HUGE_VAL, -1, -1 );
      }
      else
         hb_retnd( dResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Hyperbolic Tangent of the argument
 *    hql_Tanh( nArea ) -> nHyperbolicTangent
 * Argument[s]: nArea the size of the area
 * Returns: nHyperbolicTangent the hyperbolic tangent of nArea
 */
HB_FUNC( HQL_TANH )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = tanh( dArg );
      if( hb_mathGetError( &hb_exc, "TANH", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
         {
            /* normally, Tanh() doesn't give errors, but let's return -1 or +1,
               respectively, as these are the boundary result values */
            if( dArg < 0.0 )
               hb_retnd( -1.0 );
            else
               hb_retnd( 1.0 );
         }
      }
      else
         hb_retnd( dResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Convert radiant to degree
 *    hql_RtoD( nRadiant ) -> nDegree
 * Argument[s]: nRadiant the size of an angle in radiant
 * Returns: nDegree the size of that angle in degree
 */
HB_FUNC( HQL_RTOD )
{
   if( HB_ISNUM( 1 ) )
   {
      double dInput = hb_parnd( 1 );
      double dResult = ( 180.0 / HQLMPI ) * dInput;

      hb_retnd( dResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Convert degree to radiant
 *    hql_DtoR( nDegree ) -> nRadiant
 * Argument[s]: nDegree the size of that angle in degree
 * Returns: nRadiant the size of an angle in radiant
 */
HB_FUNC( HQL_DTOR )
{
   if( HB_ISNUM( 1 ) )
   {
      double dInput = hb_parnd( 1 );
      double dResult = ( HQLMPI / 180.0 ) * dInput;

      hb_retnd( dResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Returns the largest floating point number available in the system
 *    hql_Infinity( [<lNegative>] ) -> nLargestNumber
 * Argument[s]: [lNegative] .T., if the function return the maximum negative floating point value available else (default) positive
 * Returns: nLargestNumber the largest floating point number available in the system
 */
HB_FUNC( HQL_INFINITY )
{
   if( hb_parl( 1 ) )
      hb_retndlen( -HUGE_VAL, -1, -1 );
   else
      hb_retndlen( HUGE_VAL, -1, -1 );
}

/*
 * Rounds down a number to the next integer
 *    hql_Floor( <nNumber> ) -> nDownRoundedNumber
 * Argument[s]: nNumber number to round down
 * Returns: nDownRoundedNumber the rounded number
 */
HB_FUNC( HQL_FLOOR )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = floor( dArg );
      if( hb_mathGetError( &hb_exc, "FLOOR", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
            hb_retnlen( 0, 0, 0 );
      }
      else
         hb_retnlen( dResult, 0, 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Rounds up a number to the next integer
 *    hql_Ceiling( <nNumber> ) -> nUpRoundedNumber
 * Argument[s]: nNumber number to round up
 * Returns: nUpRoundedNumber the rounded number
 */
HB_FUNC( HQL_CEILING )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = ceil( dArg );
      if( hb_mathGetError( &hb_exc, "CEIL", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
            hb_retnlen( 0, 0, 0 );
      }
      else
         hb_retnlen( dResult, 0, 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Sign of a number
 *    hql_Sign( <nNumber> ) -> nSign
 * Argument[s]: nNumber a number
 * Returns: nSign sign of nNumber
 */
HB_FUNC( HQL_SIGN )
{
   if( HB_ISNUM( 1 ) )
   {
      double dInput = hb_parnd( 1 );
      int iResult;

      if( dInput == 0.00 )
         iResult = 0;
      else if( dInput > 0.00 )
         iResult = 1;
      else
         iResult = -1;

      hb_retni( iResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Decadic logarithm of a number
 *    hql_Log10( <nNumber> ) -> nLogarithm
 * Argument[s]: nNumber number to logarithm
 * Returns: nLogarithm decadic logarithm of nNumber
 */
HB_FUNC( HQL_LOG10 )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = log10( dArg );
      if( hb_mathGetError( &hb_exc, "LOG10", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
         {
            /* math exception is up to the Harbour function, so do this as Clipper compatible as possible */
            switch( hb_exc.type )
            {
               case HB_MATH_ERR_SING:               /* argument to log was 0.0 */
               case HB_MATH_ERR_DOMAIN:             /* argument to log was < 0.0 */
                  hb_retndlen( -HUGE_VAL, -1, -1 ); /* return -infinity */
                  break;

               default:
                  hb_retnd( 0.0 );
                  break;
            }
         }
      }
      else
         hb_retnd( dResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Calculates faculty
 *    hql_Fact( <nNumber> ) -> nFaculty
 * Argument[s]: nNumber number between 0 and 21
 * Returns: nFaculty the faculty of nNumber
 */
HB_FUNC( HQL_FACT )
{
   if( HB_ISNUM( 1 ) )
   {
      int iInput = hb_parni( 1 );

      if( iInput >= 0 && iInput < 22 )
      {
         double dResult = 1.0;
         int i;

         for( i = 1; i <= iInput; i++ )
            dResult *= ( double ) i;
         hb_retnd( dResult );
      }
      else
         hb_retnd( -1.0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Determines the mantissa of a floating point number (base2)
 *    hql_Mantissa(<nFloatingPointNumber>) -> nMantissa
 * Argument[s]: nFloatingPointNumber Designates any decimal number.
 * Returns: nMantissa the mantissa of the nFloatingPointNumber number.
 */
HB_FUNC( HQL_MANTISSA )
{
   double dValue;

   dValue = hb_parnd( 1 );

   if( dValue == 0.0 )
   {
      hb_retnd( 0.0 );
      return;
   }

   if( fabs( dValue ) < 1.0 )
   {
      while( fabs( dValue ) < 1.0 )
         dValue *= 2.0;
   }
   else if( fabs( dValue ) >= 2.0 )
   {
      while( fabs( dValue ) >= 2.0 )
         dValue /= 2.0;
   }
   hb_retnd( dValue );

}

/*
 * Determines the exponent of a floating point number (base 2)
 *    hql_Exponent(<nFloatingPointNumber>) -> nExponent
 * Argument[s]: nFloatingPointNumber Designates any decimal number.
 * Returns: the exponent of the nFloatingPointNumber number in base 2.
 */
HB_FUNC( HQL_EXPONENT )
{
   int iExponent = 0;
   double dValue;

   dValue = hb_parnd( 1 );

   if( dValue == 0.0 )
   {
      hb_retni( 0 );
      return;
   }

   if( fabs( dValue ) < 1.0 )
   {
      while( fabs( dValue ) < 1.0 )
      {
         dValue *= 2.0;
         iExponent--;
      }
   }
   else if( fabs( dValue ) >= 2.0 )
   {
      while( fabs( dValue ) >= 2.0 )
      {
         dValue /= 2.0;
         iExponent++;
      }
   }
   hb_retni( iExponent );
}
