#include <stdlib.h>
#include <ctype.h>
#include <curses.h>

#include "display.h"

int main() {
  WINDOW* mainwin;
  int ch;

  /*  Initialize ncurses  */
  if ( (mainwin = initscr()) == NULL ) {
    fprintf(stderr, "Error initializing ncurses.\n");
    exit(EXIT_FAILURE);
  }

  raw();
  keypad(stdscr, TRUE);
  noecho();                  /*  Turn off key echoing                 */
  keypad(mainwin, TRUE);     /*  Enable the keypad for non-char keys  */

  /*  Print a prompt and refresh() the screen  */
  mvaddstr(5, 10, "Press a key ('q' to quit)...");
  mvprintw(7, 10, "You pressed: ");
  refresh();

  /*  Loop until user presses 'q'  */
  while ( (ch = getch()) != 'q' ) {

    /*  Delete the old response line, and print a new one  */
    deleteln();
    mvprintw(7, 10, "You pressed: 0x%x", ch);
    refresh();
  }

  /*  Clean up after ourselves  */
  delwin(mainwin);
  endwin();
  refresh();

  return EXIT_SUCCESS;
}

