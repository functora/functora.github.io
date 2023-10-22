/* Copyright 2015-2021 Jack Humbert
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#pragma once

#define RGBLIGHT_ANIMATIONS

#ifdef AUDIO_ENABLE
#    define AUDIO_INIT_DELAY
#    define AUDIO_ENABLE_TONE_MULTIPLEXING
#    define AUDIO_TONE_MULTIPLEXING_RATE_DEFAULT 10
#    define STARTUP_SONG SONG(PLANCK_SOUND)
#    define PITCH_STANDARD_A 110.0f
#    define AUDIO_CLICKY
#    define MUSIC_MASK (keycode != KC_NO)
#endif

#define UNICODE_SELECTED_MODES UNICODE_MODE_LINUX
#define UNICODE_KEY_LNX LCTL(LSFT(KC_U))
