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

#include QMK_KEYBOARD_H
#include "muse.h"

enum planck_layers {
  _QWERTY,
  _LOWER,
  _RAISE,
  _ADJUST,
  _MEDIA,
  _FUN
};

enum planck_keycodes {
  QWERTY = SAFE_RANGE,
  LOWER,
  RAISE,
  ADJUST,
  MEDIA,
  FUN,
  EM_SRG,
  EM_LEN,
  EM_FLP,
  EM_FIN,
  EM_FIX
};

enum unicode_names {
    // Small Spanish letters
    AC_SA,
    AC_SE,
    AC_SI,
    AC_SO,
    AC_SU,
    TI_SN,
    DI_SU,
    // Capital Spanish letters
    AC_CA,
    AC_CE,
    AC_CI,
    AC_CO,
    AC_CU,
    TI_CN,
    DI_CU,
    // Ohter Spanish symbols
    REV_Q,
    REV_E
};

const uint32_t PROGMEM unicode_map[] = {
    // Small Spanish letters
    [AC_SA] = 0x00E1, // á
    [AC_SE] = 0x00E9, // é
    [AC_SI] = 0x00ED, // í
    [AC_SO] = 0x00F3, // ó
    [AC_SU] = 0x00FA, // ú
    [TI_SN] = 0x00F1, // ñ
    [DI_SU] = 0x00FC, // ü
    // Capital Spanish letters
    [AC_CA] = 0x00C1, // Á
    [AC_CE] = 0x00C9, // É
    [AC_CI] = 0x00CD, // Í
    [AC_CO] = 0x00D3, // Ó
    [AC_CU] = 0x00DA, // Ú
    [TI_CN] = 0x00D1, // Ñ
    [DI_CU] = 0x00DC, // Ü
    // Ohter Spanish symbols
    [REV_Q] = 0x00BF, // ¿
    [REV_E] = 0x00A1  // ¡
};

#define AC_A XP(AC_SA, AC_CA)
#define AC_E XP(AC_SE, AC_CE)
#define AC_I XP(AC_SI, AC_CI)
#define AC_O XP(AC_SO, AC_CO)
#define AC_U XP(AC_SU, AC_CU)
#define TI_N XP(TI_SN, TI_CN)
#define DI_U XP(DI_SU, DI_CU)
#define ES_Q XP(REV_Q, REV_Q)
#define ES_E XP(REV_E, REV_E)

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

/* Qwerty
 * ,-----------------------------------------------------------------------------------.
 * | Esc  |   Q  |   W  |   E  |   R  |   T  |   Y  |   U  |   I  |   O  |   P  | Bksp |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * | Lower|   A  |   S  |   D  |   F  |   G  |   H  |   J  |   K  |   L  |   ;  |  '   |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * | Shift|   Z  |   X  |   C  |   V  |   B  |   N  |   M  |   ,  |   .  |   /  | Enter|
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * | Ctrl | GUI  | Lower| Alt  |Space |Space |Space |Space |AltGr |Media | Fun  | Ctrl |
 * `-----------------------------------------------------------------------------------'
 */

[_QWERTY] = LAYOUT_planck_grid(
    KC_ESC,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_BSPC,
    LOWER,   KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT,
    KC_LSFT, KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,    KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_ENT,
    KC_LCTL, KC_LGUI, LOWER,   KC_LALT, KC_SPC,  KC_SPC,  KC_SPC,  KC_SPC,  KC_ALGR, MEDIA,   FUN,     KC_RCTL
),

/* Lower */

[_LOWER] = LAYOUT_planck_grid(
    RAISE,   KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    _______,
    ADJUST,  _______, _______, _______, _______, _______, KC_LEFT, KC_DOWN, KC_UP,   KC_RGHT, KC_LBRC, KC_RBRC,
    _______, _______, _______, _______, _______, _______, KC_DEL,  KC_INS,  _______, _______, KC_MINS, KC_EQL,
    _______, _______, ADJUST,  _______, KC_TAB,  KC_TAB,  KC_TAB,  KC_TAB,  KC_BSLS, KC_GRV,  _______, _______
),

/* Raise */

[_RAISE] = LAYOUT_planck_grid(
    _______,S(KC_1),S(KC_2),S(KC_3),S(KC_4),  S(KC_5),  S(KC_6),  S(KC_7),  S(KC_8),   S(KC_9),  S(KC_0),   _______,
    _______,S(KC_A),S(KC_S),S(KC_D),S(KC_F),  S(KC_G),  KC_HOME,  KC_PGDN,  KC_PGUP,   KC_END,   S(KC_LBRC),S(KC_RBRC),
    _______,S(KC_Z),S(KC_X),S(KC_C),S(KC_V),  S(KC_B),  S(KC_N),  S(KC_M),  S(KC_COMM),S(KC_DOT),S(KC_MINS),S(KC_EQL),
    _______,_______,_______,_______,S(KC_TAB),S(KC_TAB),S(KC_TAB),S(KC_TAB),S(KC_BSLS),S(KC_GRV),_______,   _______
),

/* Adjust (Lower + Raise)
 *
 * F-keys, mouse controls, emoji.
 *
 */

[_ADJUST] = LAYOUT_planck_grid(
  _______, KC_F1  , KC_F2,   KC_F3,   KC_F4,   KC_F5,   KC_BTN4, KC_BTN1, KC_BTN2, KC_BTN3, EM_FIN,  EM_LEN,
  _______, KC_F6,   KC_F7,   KC_F8,   KC_F9,   KC_F10,  KC_MS_L, KC_MS_D, KC_MS_U, KC_MS_R, EM_FIX,  EM_FLP,
  _______, KC_F11,  KC_F12,  KC_PSCR, KC_SCRL, KC_PAUS, KC_WH_L, KC_WH_D, KC_WH_U, KC_WH_R, _______, _______,
  _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______
),

/* Media
 *
 * Media controls, settings, modes.
 *
 */

[_MEDIA] = LAYOUT_planck_grid(
    _______, KC_BRID, KC_BRIU, _______, QK_BOOT, _______, KC_MPRV, KC_MPLY, KC_MNXT, KC_MUTE, KC_VOLD, KC_VOLU,
    _______, KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   KC_F6,   AU_TOGG, MU_NEXT, MU_TOGG, AU_PREV, AU_NEXT,
    _______, KC_F7,   KC_F8,   KC_F9,   KC_F10,  KC_F11,  KC_F12,  _______, CK_RST,  CK_TOGG, CK_DOWN, CK_UP,
    _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______
),

/* Fun
 *
 * RGB, music and emoji.
 *
 */

[_FUN] = LAYOUT_planck_grid(
    _______, RGB_TOG,  RGB_VAD, RGB_VAI, RGB_M_P, RGB_M_SW, RGB_M_X, _______, _______, _______, _______, _______,
    _______, RGB_MOD,  RGB_SAD, RGB_SAI, RGB_M_B, RGB_M_SN, RGB_M_G, _______, _______, _______, _______, _______,
    _______, RGB_M_SW, RGB_HUD, RGB_HUI, RGB_M_R, RGB_M_K,  RGB_M_T, _______, _______, _______, _______, _______,
    _______, _______,  _______, _______, _______, _______,  _______, _______, _______, _______, _______, _______
)

/* Español
 * ,-----------------------------------------------------------------------------------.
 * |      |   ¡  |      |   é  |      |      |   ü  |   ú  |   í  |   ó  |      |      |
 * |------+------+------+------+------+-------------+------+------+------+------+------|
 * |      |   á  |      |      |      |      |      |      |      |      |      |      |
 * |------+------+------+------+------+------|------+------+------+------+------+------|
 * |      |      |      |      |      |      |   ñ  |      |      |      |   ¿  |      |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * |      |      |      |      |      |      |      |      |      |      |      |      |
 * `-----------------------------------------------------------------------------------'
 */

// [_ESP] = LAYOUT_planck_grid(
//     _______, ES_E,    _______, AC_E,    _______, _______, DI_U,    AC_U,    AC_I,    AC_O,    _______, _______,
//     _______, AC_A,    _______, _______, _______, _______, _______, _______, _______, _______, _______, _______,
//     _______, _______, _______, _______, _______, _______, TI_N,    _______, _______, _______, ES_Q,    _______,
//     _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______
// ),

};

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  switch (keycode) {
    case QWERTY:
      if (record->event.pressed) {
        set_single_persistent_default_layer(_QWERTY);
      }
      return false;
      break;
    case LOWER:
      if (record->event.pressed) {
        layer_on(_LOWER);
        // update_tri_layer(_LOWER, _RAISE, _ADJUST);
      } else {
        layer_off(_LOWER);
        // update_tri_layer(_LOWER, _RAISE, _ADJUST);
      }
      return false;
      break;
    case RAISE:
      if (record->event.pressed) {
        layer_on(_RAISE);
        // update_tri_layer(_LOWER, _RAISE, _ADJUST);
      } else {
        layer_off(_RAISE);
        // update_tri_layer(_LOWER, _RAISE, _ADJUST);
      }
      return false;
      break;
    case ADJUST:
      if (record->event.pressed) {
        layer_on(_ADJUST);
      } else {
        layer_off(_ADJUST);
      }
      return false;
      break;
    case MEDIA:
      if (record->event.pressed) {
        layer_on(_MEDIA);
      } else {
        layer_off(_MEDIA);
      }
      return false;
      break;
    case FUN:
      if (record->event.pressed) {
        layer_on(_FUN);
      } else {
        layer_off(_FUN);
      }
      return false;
      break;
    case EM_SRG:
      if (record->event.pressed) {
        send_unicode_string("¯\\_(ツ)_/¯");
      }
      break;
    case EM_LEN:
      if (record->event.pressed) {
        send_unicode_string("( ͡° ͜ʖ ͡°)");
      }
      break;
    case EM_FLP:
      if (record->event.pressed) {
        send_unicode_string("(╯°□°）╯︵ ┻━┻");
      }
      break;
    case EM_FIN:
      if (record->event.pressed) {
        send_unicode_string("°◡°");
      }
      break;
    case EM_FIX:
      if (record->event.pressed) {
        send_unicode_string("┬─┬ ノ( ゜-゜ノ)");
      }
      break;
  }
  return true;
}
