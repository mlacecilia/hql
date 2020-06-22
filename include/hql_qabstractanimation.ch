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
#ifndef __QABSTRACTANIMATION_CH
   #define __QABSTRACTANIMATION_CH

// enum QAbstractAnimation::DeletionPolicy
#define QAbstractAnimation_KeepWhenStopped         0     // The animation will not be deleted when stopped.
#define QAbstractAnimation_DeleteWhenStopped       1     // The animation will be automatically deleted when stopped.

// enum QAbstractAnimation::Direction
#define QAbstractAnimation_Forward                 0     // The current time of the animation increases with time (i.e., moves from 0 and towards the end / duration).
#define QAbstractAnimation_Backward                1     // The current time of the animation decreases with time (i.e., moves from the end / duration and towards 0).

// enum QAbstractAnimation::State
#define QAbstractAnimation_Stopped                 0     // The animation is not running.
                                                         // This is the initial state of QAbstractAnimation, and the state QAbstractAnimation reenters when finished.
                                                         // The current time remain unchanged until either setCurrentTime() is called, or the animation is started by calling start().

#define QAbstractAnimation_Paused                  1     // The animation is paused (i.e., temporarily suspended).
                                                         // Calling resume() will resume animation activity.

#define QAbstractAnimation_Running                 2     // The animation is running.
                                                         // While control is in the event loop, QAbstractAnimation will update its current time at regular intervals, calling updateCurrentTime() when appropriate.

#endif /* __QABSTRACTANIMATION_CH */
