import sys

import os

import time

import termios

import tty

import select

esc = "\x1b"

csi = f"{esc}["

sgr_reset = f"{csi}0m"

block_char = "█"

blue = f"34"

_original_terminal_settings = None

_fd = None

def setup_terminal(  ):

    sys.stdout.write( f"{csi}?25l" )

    sys.stdout.write( f"{csi}2J" )

    sys.stdout.write( f"{csi}H" )

    sys.stdout.flush(  )



def set_terminal_raw_mode(  ):

    global _original_terminal_settings , _fd

    _fd = sys.stdin.fileno(  )

    new_settings = termios.tcgetattr( _fd )

    new_settings[ 6 ][ termios.VMIN ] = 0

    new_settings[ 6 ][ termios.VTIME ] = 0

    termios.tcsetattr( _fd , termios.TCSANOW , new_settings )



def restore_terminal( final_row = None , final_col = None ):

    if not final_row == None and not final_col == None:

        sys.stdout.write( f"{csi}{final_row};{final_col}H" )



    else:

        foo = 2 * os.linesep

        sys.stdout.write( f"{foo}" )



    sys.stdout.write( f"{csi}?25h" )

    sys.stdout.flush(  )



def get_char_non_blocking(  ):

    fd = sys.stdin.fileno(  )

    old_settings = termios.tcgetattr( fd )

    try:

        new_settings = termios.tcgetattr( fd )

        new_settings[ 3 ] = new_settings[ 3 ] & ~( termios.ICANON | termios.ECHO )

        termios.tcsetattr( fd , termios.TCSANOW , new_settings )

        char = None

        if select.select( [ sys.stdin ] , [  ] , [  ] , 0 ) == ( [ sys.stdin ] , [  ] , [  ] ):

            char = sys.stdin.read( 1 )





    finally:

        termios.tcsetattr( fd , termios.TCSADRAIN , old_settings )



    return char



def pixel( row , col , color_code ):

    sgr_color_fg = f"{csi}{color_code}m"

    cursor_pos = f"{csi}{row};{col}H"

    sys.stdout.write( f"{cursor_pos}{sgr_color_fg}{block_char}{sgr_reset}" )

    sys.stdout.flush(  )



def erase_pixel( row , col ):

    cursor_pos = f"{csi}{row};{col}H"

    sgr_default_fg_bg = f"{csi}39;49m"

    sys.stdout.write( f"{cursor_pos}{sgr_default_fg_bg} " )

    sys.stdout.flush(  )



def square( ya , xa , lx , ly , color ):

    xa = xa + 1

    ya = ya + 1

    xb = xa + lx

    yb = ya + ly

    for j in range( xb - xa + 1 ):

        for k in range( yb - ya + 1 ):

            pixel( xa + j , ya + k , color )







def erase_square( ya , xa , lx , ly ):

    xa = xa + 1

    ya = ya + 1

    xb = xa + lx

    yb = ya + ly

    for j in range( xb - xa + 1 ):

        for k in range( yb - ya + 1 ):

            erase_pixel( xa + j , ya + k )







def sprite( x , y , the_sprite ):

    for i in the_sprite:

        square( x + i[ 0 ] , y + i[ 1 ] , i[ 2 ] , i[ 3 ] , i[ 4 ] )





man = [ [ 0 , 1 , 0 , 0 , blue ] , [ 0 , 3 , 0 , 0 , blue ] , [ 1 , 2 , 0 , 0 , blue ] , [ 2 , 3 , 0 , 0 , blue ] , [ 2 , 1 , 0 , 0 , blue ] , [ 1 , 1 , 0 , 0 , blue ] , [ 1 , 0 , 0 , 0 , blue ] ]

x = [ [ 0 , 0 , 0 , 0 , blue ] , [ 0 , 2 , 0 , 0 , blue ] , [ 1 , 1 , 0 , 0 , blue ] , [ 2 , 2 , 0 , 0 , blue ] , [ 2 , 0 , 0 , 0 , blue ] ]

def erase_sprite( row , col , sprite ):

    for i in sprite:

        erase_square( row  + i[ 0 ] , col + i[ 1 ] , i[ 2 ] , i[ 3 ] )





sprite_foo = x

def main(  ):

    set_terminal_raw_mode(  )

    setup_terminal(  )

    square( 15 , 20 , 0 , 12 , blue )

    square( 28 , 20 , 8 , 0 , blue )

    square( 15 , 28 , 0 , 12 , blue )

    positionx = 0

    positiony = 0

    sprite( positionx , positiony , sprite_foo )

    continue2 = True

    while continue2:

        key = get_char_non_blocking(  )

        if key == "q":

            continue2 = False



        elif key == "w":

            erase_sprite( positionx , positiony , sprite_foo )

            positiony = positiony - 1

            sprite( positionx , positiony , sprite_foo )



        elif key == "a":

            erase_sprite( positionx , positiony , sprite_foo )

            positionx = positionx - 1

            sprite( positionx , positiony , sprite_foo )



        elif key == "s":

            erase_sprite( positionx , positiony , sprite_foo )

            positiony = positiony + 1

            sprite( positionx , positiony , sprite_foo )



        elif key == "d":

            erase_sprite( positionx , positiony , sprite_foo )

            positionx = positionx + 1

            sprite( positionx , positiony , sprite_foo )



        time.sleep( 0.01 )



    erase_square( 15 , 20 , 0 , 12 )

    erase_square( 28 , 20 , 8 , 0 )

    erase_square( 15 , 28 , 0 , 12 )

    restore_terminal(  )

    if not old_term_settings == None and not fd == None:

        restore_terminal_mode( old_term_settings , fd )



    sys.exit( 0 )

    pass



if __name__ == "__main__":

    try:

        main(  )



    except KeyboardInterrupt:

        print( 
Exiting )



    finally:

        restore_terminal(  )

        restore_terminal_mode(  )

        sys.exit( 0 )





