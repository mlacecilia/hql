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
#ifndef __QMOVIE_CH
#define __QMOVIE_CH

   // enum QMovie::CacheMode
#define QMovie_CacheNone         0  // No frames are cached (the default).
#define QMovie_CacheAll          1  //All frames are cached.

   // enum QMovie::MovieState
#define QMovie_NotRunning        0  // The movie is not running. This is QMovie's initial state, and the state it enters after stop() has been called or the movie is finished.
#define QMovie_Paused            1  // The movie is paused, and QMovie stops emitting updated() or resized(). This state is entered after calling pause() or setPaused(true). The current frame number it kept, and the movie will continue with the next frame when unpause() or setPaused(false) is called.
#define QMovie_Running           2  // The movie is running.

#endif /* __QMOVIE_CH */
