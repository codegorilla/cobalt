package org.cobalt

enum State {

  // Binary integer states
  case BIN_START
  case BIN_100
  case BIN_200
  case BIN_300
  case BIN_400
  case BIN_500
  case BIN_600
  case BIN_700
  case BIN_800
  case BIN_ERROR

  // Hexadecimal number states
  case HEX_START
  case HEX_10
  case HEX_20
  case HEX_30
  case HEX_100
  case HEX_200
  case HEX_210
  case HEX_220
  case HEX_230
  case HEX_300
  case HEX_400
  case HEX_500
  case HEX_600
  case HEX_700
  case HEX_800
  case HEX_810
  case HEX_820
  case HEX_ERROR

  // Octal integer states
  case OCT_START
  case OCT_100
  case OCT_200
  case OCT_300
  case OCT_400
  case OCT_500
  case OCT_600
  case OCT_700
  case OCT_800
  case OCT_ERROR
}
