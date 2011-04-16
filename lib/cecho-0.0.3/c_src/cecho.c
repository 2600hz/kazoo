//==============================================================================
// Copyright 2010 Erlang Solutions Ltd.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//==============================================================================

// Includes
#include <stdlib.h>
#include <string.h>
#include "cecho.h"
#include "cecho_commands.h"
#include "erl_driver.h"
#include "erl_interface.h"
#include "ei.h"
#include "ncurses.h"
#include "assert.h"

// State structure
typedef struct {
  WINDOW *win[_MAXWINDOWS+1];
  ei_x_buff eixb;
  char *args;
  int argslen;
  int index;
  int version;
  ErlDrvPort drv_port;
} state;

void init_state(state *st, char *args, int argslen);
void ok(state *st);
void error_tuple(state *st, int code);
void boolean(state *st, int code);

void tuple(ei_x_buff *eixb, int size);
void atom(ei_x_buff *eixb, const char *str, int size);
void integer(ei_x_buff *eixb, int integer);
void string(ei_x_buff *eixb, const char *str);
void encode_ok_reply(state *st, int code);
int findfreewindowslot(state *st);
void loop_getch(void *arg);

void do_endwin(state *st);
void do_initscr(state *st);
void do_refresh(state *st);
void do_cbreak(state *st);
void do_nocbreak(state *st);
void do_echo(state *st);
void do_noecho(state *st);
void do_addch(state *st);
void do_addstr(state *st);
void do_move(state *st);
void do_getyx(state *st);
void do_getmaxyx(state *st);
void do_curs_set(state *st);
void do_werase(state *st);
void do_has_colors(state *st);
void do_start_color(state *st);
void do_init_pair(state *st);
void do_curs_set(state *st);
void do_init_pair(state *st);
void do_wattron(state *st);
void do_wattroff(state *st);
void do_nl(state *st);
void do_nonl(state *st);
void do_scrollok(state *st);
void do_mvaddch(state *st);
void do_mvaddstr(state *st);
void do_newwin(state *st);
void do_delwin(state *st);
void do_wmove(state *st);
void do_waddstr(state *st);
void do_waddch(state *st);
void do_mvwaddstr(state *st);
void do_mvwaddch(state *st);
void do_wrefresh(state *st);
void do_whline(state *st);
void do_wvline(state *st);
void do_wborder(state *st);
void do_box(state *st);
void do_keypad(state *st);

// =============================================================================
// Erlang Callbacks
// =============================================================================
static ErlDrvData start(ErlDrvPort port, char *command) {
  state *drvstate = (state *)driver_alloc(sizeof(state));
  drvstate->drv_port = port;
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  int i;
  for (i = 0; i < _MAXWINDOWS; i++)
    drvstate->win[i] = NULL;
  return (ErlDrvData)drvstate;
}

static void stop(ErlDrvData drvstate) {
  state *st = (state *)drvstate;
  driver_select(st->drv_port, (ErlDrvEvent)fileno(stdin), DO_READ, 0);
  driver_free(drvstate);
}

static void do_getch(ErlDrvData drvstate, ErlDrvEvent event) {
  state *st = (state *)drvstate;
  ei_x_buff eixb;
  int keycode;
  ei_x_new_with_version(&eixb);
  keycode = getch();
  integer(&eixb, keycode);
  driver_output(st->drv_port, eixb.buff, eixb.index);
}

static int control(ErlDrvData drvstate, unsigned int command, char *args,
		int argslen, char **rbuf, int rbuflen) {
  state *st = (state *)drvstate;
  init_state(st, args, argslen);

  switch (command) {
  case ENDWIN: do_endwin(st); break;
  case INITSCR: do_initscr(st); break;
  case REFRESH: do_refresh(st); break;
  case CBREAK: do_cbreak(st); break;
  case NOCBREAK: do_nocbreak(st); break;
  case ECHO: do_echo(st); break;
  case NOECHO: do_noecho(st); break;
  case ADDCH: do_addch(st); break;
  case ADDSTR: do_addstr(st); break;
  case MOVE: do_move(st); break;
  case GETYX: do_getyx(st); break;
  case GETMAXYX: do_getmaxyx(st); break;
  case CURS_SET: do_curs_set(st); break;
  case WERASE: do_werase(st); break;
  case HAS_COLORS: do_has_colors(st); break;
  case START_COLOR: do_start_color(st); break;
  case INIT_PAIR: do_init_pair(st); break;
  case WATTRON: do_wattron(st); break;
  case WATTROFF: do_wattroff(st); break;
  case NL: do_nl(st); break;
  case NONL: do_nonl(st); break;
  case SCROLLOK: do_scrollok(st); break;
  case MVADDCH: do_mvaddch(st); break;
  case MVADDSTR: do_mvaddstr(st); break;
  case NEWWIN: do_newwin(st); break;
  case DELWIN: do_delwin(st); break;
  case WMOVE: do_wmove(st); break;
  case WADDSTR: do_waddstr(st); break;
  case WADDCH: do_waddch(st); break;
  case MVWADDSTR: do_mvwaddstr(st); break;
  case MVWADDCH: do_mvwaddch(st); break;
  case WREFRESH: do_wrefresh(st); break;
  case WHLINE: do_whline(st); break;
  case WVLINE: do_wvline(st); break;
  case WBORDER: do_wborder(st); break;
  case BOX: do_box(st); break;
  case KEYPAD: do_keypad(st); break;
  default: break;
  }

  int rlen = st->eixb.index;
  ErlDrvBinary *response = driver_alloc_binary(rlen);
  memcpy(response->orig_bytes, st->eixb.buff, rlen);
  ei_x_free(&(st->eixb));
  *rbuf = (char *)response;
  return rlen;
}

// =============================================================================
// NCurses function wrappers
// ===========================================================================
void do_endwin(state *st) {
  encode_ok_reply(st, endwin());
}

void do_initscr(state *st) {
  st->win[0] = (WINDOW *)initscr();
  driver_select(st->drv_port, (ErlDrvEvent)fileno(stdin), DO_READ, 1);
  if (st->win[0] == NULL) {
    encode_ok_reply(st, -1);
  } else {
    encode_ok_reply(st, 0);
  }
}

void do_refresh(state *st) {
  encode_ok_reply(st, refresh());
}

void do_cbreak(state *st) {
  encode_ok_reply(st, cbreak());
}
void do_nocbreak(state *st) {
  encode_ok_reply(st, nocbreak());
}

void do_echo(state *st) {
  encode_ok_reply(st, echo());
}

void do_noecho(state *st) {
  encode_ok_reply(st, noecho());
}

void do_addch(state *st) {
  long ch;
  ei_decode_long(st->args, &(st->index), &ch);
  encode_ok_reply(st, addch((chtype)ch));
}

void do_addstr(state *st) {
  int arity;
  long strlen;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &strlen);
  char str[strlen];
  ei_decode_string(st->args, &(st->index), str);
  encode_ok_reply(st, addnstr(str, strlen));
}

void do_move(state *st) {
  int arity;
  long y, x;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &y);
  ei_decode_long(st->args, &(st->index), &x);
  encode_ok_reply(st, move((int)y, (int)x));
}

void do_getyx(state *st) {
  long slot;
  int x, y;
  ei_decode_long(st->args, &(st->index), &slot);
  getyx(st->win[slot], y, x);
  tuple(&(st->eixb), 2);
  integer(&(st->eixb), y);
  integer(&(st->eixb), x);
}

void do_getmaxyx(state *st) {
  long slot;
  int x, y;
  ei_decode_long(st->args, &(st->index), &slot);
  getmaxyx(st->win[slot], y, x);
  tuple(&(st->eixb), 2);
  integer(&(st->eixb), y);
  integer(&(st->eixb), x);
}

void do_curs_set(state *st) {
  long flag;
  ei_decode_long(st->args, &(st->index), &flag);
  curs_set((int)flag);
  ok(st);
}

void do_werase(state *st) {
  long slot;
  ei_decode_long(st->args, &(st->index), &slot);
  encode_ok_reply(st, werase(st->win[slot]));
}

void do_has_colors(state *st) {
  boolean(st, has_colors());
}

void do_start_color(state *st) {
  encode_ok_reply(st, start_color());
}

void do_init_pair(state *st) {
  int arity;
  long pairnum, fcolor, bcolor;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &pairnum);
  ei_decode_long(st->args, &(st->index), &fcolor);
  ei_decode_long(st->args, &(st->index), &bcolor);
  encode_ok_reply(st, init_pair((int)pairnum, (int)fcolor, (int)bcolor));
}

void do_wattron(state *st) {
  int arity;
  long slot, attrs;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &attrs);
  encode_ok_reply(st, wattron(st->win[slot], (int)attrs));
}

void do_wattroff(state *st) {
  int arity;
  long slot, attrs;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &attrs);
  encode_ok_reply(st, wattroff(st->win[slot], (int)attrs));
}

void do_nl(state *st) {
  encode_ok_reply(st, nl());
}

void do_nonl(state *st) {
  encode_ok_reply(st, nonl());
}

void do_scrollok(state *st) {
  int arity;
  int bf;
  long slot;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_boolean(st->args, &(st->index), &bf);
  encode_ok_reply(st, scrollok(st->win[slot], bf));
}

void do_mvaddch(state *st) {
  int arity;
  long y, x, ch;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &y);
  ei_decode_long(st->args, &(st->index), &x);
  ei_decode_long(st->args, &(st->index), &ch);
  encode_ok_reply(st, mvaddch((int)y, (int)x, (chtype)ch));
}

void do_mvaddstr(state *st) {
  int arity;
  long strlen, y, x;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &y);
  ei_decode_long(st->args, &(st->index), &x);
  ei_decode_long(st->args, &(st->index), &strlen);
  char str[strlen];
  ei_decode_string(st->args, &(st->index), str);
  encode_ok_reply(st, mvaddnstr((int)y, (int)x, str, (int)strlen));
}

void do_newwin(state *st) {
  int slot = findfreewindowslot(st);
  if (slot > 0) {
    int arity;
    long height, width, starty, startx;
    ei_decode_tuple_header(st->args, &(st->index), &arity);
    ei_decode_long(st->args, &(st->index), &height);
    ei_decode_long(st->args, &(st->index), &width);
    ei_decode_long(st->args, &(st->index), &starty);
    ei_decode_long(st->args, &(st->index), &startx);
    st->win[slot] = newwin(height, width, starty, startx);
    integer(&(st->eixb), slot);
  } else {
    integer(&(st->eixb), -1);
  }
}

void do_delwin(state *st) {
  long slot;
  ei_decode_long(st->args, &(st->index), &slot);
  if (slot == 0) {
    boolean(st, FALSE);
  } else if (st->win[slot] == NULL) {
    boolean(st, FALSE);
  } else if (st->win[slot] != NULL) {
    delwin(st->win[slot]);
    st->win[slot] = NULL;
    boolean(st, TRUE);
  }
}

void do_wmove(state *st) {
  int arity;
  long slot, y, x;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &y);
  ei_decode_long(st->args, &(st->index), &x);
  encode_ok_reply(st, wmove(st->win[slot], (int)y, (int)x));
}

void do_waddstr(state *st) {
  int arity;
  long slot, strlen;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &strlen);
  char str[strlen];
  ei_decode_string(st->args, &(st->index), str);
  encode_ok_reply(st, waddnstr(st->win[slot], str, strlen));
}

void do_waddch(state *st) {
  int arity;
  long slot;
  char ch = 0;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_char(st->args, &(st->index), &ch);
  encode_ok_reply(st, waddch(st->win[slot], ch));
}

void do_mvwaddstr(state *st) {
  int arity;
  long slot, y, x, strlen;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &y);
  ei_decode_long(st->args, &(st->index), &x);
  ei_decode_long(st->args, &(st->index), &strlen);
  char str[strlen];
  ei_decode_string(st->args, &(st->index), str);
  encode_ok_reply(st, mvwaddnstr(st->win[slot], (int)y, (int)x, str, strlen));
}

void do_mvwaddch(state *st) {
  int arity;
  long slot, y, x;
  char ch = 0;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &y);
  ei_decode_long(st->args, &(st->index), &x);
  ei_decode_char(st->args, &(st->index), &ch);
  encode_ok_reply(st, mvwaddch(st->win[slot], (int)y, (int)x, ch));
}

void do_wrefresh(state *st) {
  long slot;
  ei_decode_long(st->args, &(st->index), &slot);
  encode_ok_reply(st, wrefresh(st->win[slot]));
}

void do_whline(state *st) {
  int arity;
  long slot, ch, max;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &ch);
  ei_decode_long(st->args, &(st->index), &max);
  encode_ok_reply(st, whline(st->win[slot], (chtype)ch, (int)max));
}

void do_wvline(state *st) {
  int arity;
  long slot, ch, max;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &ch);
  ei_decode_long(st->args, &(st->index), &max);
  encode_ok_reply(st, wvline(st->win[slot], (chtype)ch, (int)max));
}

void do_wborder(state *st) {
  int arity;
  long slot, ls, rs, ts, bs, tl, tr, bl, br;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &ls);
  ei_decode_long(st->args, &(st->index), &rs);
  ei_decode_long(st->args, &(st->index), &ts);
  ei_decode_long(st->args, &(st->index), &bs);
  ei_decode_long(st->args, &(st->index), &tl);
  ei_decode_long(st->args, &(st->index), &tr);
  ei_decode_long(st->args, &(st->index), &bl);
  ei_decode_long(st->args, &(st->index), &br);
  encode_ok_reply(st, wborder(st->win[slot], (chtype)ls, (chtype)rs, (chtype)ts,
			      (chtype)bs, (chtype)tl, (chtype)tr, (chtype)bl,
			      (chtype)br));
}

void do_box(state *st) {
  int arity;
  long slot, verch, horch;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &verch);
  ei_decode_long(st->args, &(st->index), &horch);
  encode_ok_reply(st, box(st->win[slot], (chtype)verch, (chtype)horch));
}

void do_keypad(state *st) {
  int arity, bf;
  long slot;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_boolean(st->args, &(st->index), &bf);
  encode_ok_reply(st, keypad(st->win[slot], bf));
}

// =============================================================================
// Utility functions
// =============================================================================
void init_state(state *st, char *args, int argslen) {
  st->index = 0;
  st->version = 0;
  st->args = args;
  st->argslen = argslen;
  ei_decode_version(st->args, &(st->index), &(st->version));
  assert(st->version != 0);
  assert(st->index != 0);
  ei_x_new_with_version(&(st->eixb));
}

void ok(state *st) {
  atom(&(st->eixb), "ok", 2);
}

void error_tuple(state *st, int code) {
  tuple(&(st->eixb), 2);
  atom(&(st->eixb), "error", 5);
  integer(&(st->eixb), code);
}

void boolean(state *st, int code) {
  if (code == TRUE)
    atom(&(st->eixb),"true",4);
  else
    atom(&(st->eixb),"false",5);
}

void tuple(ei_x_buff *eixb, int size) {
  ei_x_encode_tuple_header(eixb, size);
}

void atom(ei_x_buff *eixb, const char *str, int size) {
  ei_x_encode_atom_len(eixb, str, size);
}

void integer(ei_x_buff *eixb, int integer) {
  ei_x_encode_long(eixb, (long)integer);
}

void string(ei_x_buff *eixb, const char *str) {
  ei_x_encode_string(eixb, str);
}

void encode_ok_reply(state *st, int code) {
  if (code == OK) {
    ok(st);
  } else {
    error_tuple(st, code);
  }
}

int findfreewindowslot(state *st) {
  int i;
  for (i = 0; i < _MAXWINDOWS; i++)
    if (st->win[i] == NULL) return i;
  return -1;
}

// =============================================================================
// Erlang driver_entry Specification
// ===========================================================================
ErlDrvEntry driver_entry = {
  NULL,
  start,
  stop,
  NULL,
  do_getch,
  NULL,
  "cecho",
  NULL,
  NULL,
  control,
  NULL
};

// =============================================================================
// Erlang Driver Name
// =============================================================================
DRIVER_INIT(cecho) {
  return &driver_entry;
}
