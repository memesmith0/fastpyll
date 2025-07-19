import sys

import os

import time

import termios

import tty

import select

_original_terminal_settings = None

_fd = None

esc = f"\x1b"

csi = f"{esc}["

sgr_reset = f"{csi}0m"

block_char = f"▀"

black = [ 0 , 0 , 0 ]

transparent = [ -1 , -1 , -1 ]

blue = [ 0 , 0 , 255 ]

wizard_blue = [ 33 , 47 , 73 ]

screen_width = 320

screen_height = 162

screen = [  ]

for r in range( screen_height ):

    row_data = [  ]

    for c in range( screen_width ):

        row_data.append( { f"occupant_pixels": {  } , f"last_color": black } )



    screen.append( row_data )



def main(  ):

    set_terminal_raw_mode(  )

    setup_terminal(  )

    player_id = construct_player(  )

    continue2 = True

    while continue2:

        for i in range( 100 ):

            key = get_char_non_blocking(  )

            if ( key == f"q" ):

                continue2 = False



            elif ( key == f"w" ):

                move( player_id , f"up" )



            elif ( key == f"a" ):

                move( player_id , f"left" )



            elif ( key == f"s" ):

                move( player_id , f"down" )



            elif ( key == f"d" ):

                move( player_id , f"right" )





        draw_screen(  )

        time.sleep( 0.05 )



    restore_terminal(  )

    pass



def setup_terminal(  ):

    sys.stdout.write( f"{csi}?25l" )

    sys.stdout.write( f"{csi}2J" )

    sys.stdout.write( f"{csi}H" )

    sys.stdout.flush(  )



def set_terminal_raw_mode(  ):

    global _original_terminal_settings , _fd

    _fd = sys.stdin.fileno(  )

    _original_terminal_settings = termios.tcgetattr( _fd )

    new_settings = termios.tcgetattr( _fd )

    new_settings[ 3 ] = ( new_settings[ 3 ] & ~( ( termios.ICANON | termios.ECHO ) ) )

    new_settings[ 6 ][ termios.VMIN ] = 0

    new_settings[ 6 ][ termios.VTIME ] = 0

    termios.tcsetattr( _fd , termios.TCSANOW , new_settings )



def restore_terminal_mode(  ):

    global _original_terminal_settings , _fd

    if ( ( not ( _fd == None ) ) and ( not ( _original_terminal_settings == None ) ) ):

        termios.tcsetattr( _fd , termios.TCSADRAIN , _original_terminal_settings )





def restore_terminal( final_row = None , final_col = None ):

    if ( ( not ( final_row == None ) ) and ( not ( final_col == None ) ) ):

        sys.stdout.write( f"{csi}{final_row};{final_col}H" )



    else:

        foo = ( 2 * os.linesep )

        sys.stdout.write( f"{foo}" )



    sys.stdout.write( f"{csi}?25h" )

    sys.stdout.flush(  )



def get_char_non_blocking(  ):

    char = None

    if ( select.select( [ sys.stdin ] , [  ] , [  ] , 0 ) == [ sys.stdin ] , [  ] , [  ] ):

        char = sys.stdin.read( 1 )



    return char



def pixels( col , row , top_color , bottom_colors ):

    row = ( row + 1 )

    col = ( col + 1 )

    set_top_color = f"{csi}38;2;{top_color[0]};{top_color[1]};{top_color[2]}m"

    set_bottom_color = f"{csi}48;2;{bottom_color[0]};{bottom_color[1]};{bottom_color[2]}m"

    cursor_position = f"{csi}{row};{col}H"

    sys.stdout.write( f"{cursor_position}{set_top_color}{set_bottom_color}{block_char}{sgr_reset}" )



def are_colors_equal( color_a , color_b ):

    return ( ( color_a[ 0 ] == color_b[ 0 ] ) and ( ( color_a[ 1 ] == color_b[ 1 ] ) and ( color_a[ 2 ] == color_b[ 2 ] ) ) )



def draw_screen(  ):

    for r in range( ( screen_height // 2 ) ):

        for c in range( screen_width ):

            screen_r = ( r * 2 )

            screen_c = c

            top_location = screen[ screen_r ][ screen_c ]

            bottom_location = screen[ ( 1 + screen_r ) ][ screen_c ]

            highest_top_occupant_depth = 0

            highest_top_occupant_color = black

            for foop , top_occupant in top_location[ f"occupant_pixels" ].items(  ):

                if ( top_occupant[ f"display_depth" ] > highest_top_occupant_depth ):

                    highest_top_occupant_color = top_occupant[ f"screen" ][ screen_r ][ screen_c ]





            highest_bottom_occupant_depth = 0

            highest_bottom_occupant_color = black

            for foop , bottom_occupant in bottom_location[ f"occupant_pixels" ].items(  ):

                if ( bottom_occupant[ f"display_depth" ] > highest_bottom_occupant_depth ):

                    highest_bottom_occupant_color = bottom_occupant[ f"screen" ][ ( 1 + screen_r ) ][ screen_c ]





            if ( ( highest_top_occupant_color != top_location[ f"last_color" ] ) or ( highest_bottom_occupant_color != bottom_location[ f"last_color" ] ) ):

                pixels( c , r , highest_top_occupant_color , highest_bottom_occupant_color )







    sys.stdout.flush(  )



last_enumerable = 0

def new_enumerable(  ):

    global last_enumerable

    last_enumerable = ( last_enumerable + 1 )

    return ( last_enumerable - 1 )



global_entity_list = {  }

def teleport_on_screen( id , x , y ):

    player_id = id

    player = global_entity_list[ player_id ]

    collision = global_entity_list[ player_id ][ f"collision" ]

    dimensions = player[ f"animations" ][ f"idle" ][ 0 ][ f"frame" ]

    if ( ( x > -1 ) and ( ( y > -1 ) and ( ( ( x + ( 1 - len( dimensions[ 0 ] ) ) ) < screen_width ) and ( ( y + ( 1 - len( dimensions ) ) ) < screen_height ) ) ) ):

        can_move = True

        for r in range( len( collision ) ):

            for c in range( len( collision[ r ] ) ):

                for foop , occupant in screen[ ( y + r ) ][ ( x + c ) ][ f"occupant_pixels" ].items(  ):

                    occupant_collision = occupant[ f"collision" ]

                    if ( ( collision[ r ][ c ] != f"no_collide" ) and ( collision[ r ][ c ] == occupant_collision[ r ][ c ] ) ):

                        can_move = False









        if can_move:

            player[ f"position_x" ] = x

            global_entity_list[ player_id ][ f"position_y" ] = y

            old_x = player[ f"position_x" ]

            old_y = player[ f"position_y" ]

            for x_unit in range( len( dimensions ) ):

                for y_unit in range( dimensions[ y_unit ] ):

                    new_color = dimensions[ y_unit ][ x_unit ]

                    if ( not are_colors_equal( new_color , transparent ) ):

                        del( screen[ ( old_y + y_unit ) ][ ( old_x + x_unit ) ][ f"occupant_pixels" ][ player_id ] )

                        screen[ ( y + y_unit ) ][ ( x + x_unit ) ][ f"occupant_pixels" ][ player_id ] = player

                        player[ f"screen" ][ ( y + y_unit ) ][ ( x + x_unit ) ] = new_color













def move( id , direction ):

    player_id = id

    player = global_entity_list[ player_id ]

    position_x = accessplayer( f"position_x" )

    position_y = player[ f"position_y" ]

    if ( direction == f"up" ):

        teleport_on_screen( id , position_x , ( position_y - 1 ) )



    elif ( direction == f"down" ):

        teleport_on_screen( id , position_x , ( position_y + 1 ) )



    elif ( direction == f"left" ):

        teleport_on_screen( id , ( position_x - 1 ) , position_y )



    elif ( direction == f"right" ):

        teleport_on_screen( id , ( position_x + 1 ) , position_y )





def construct_player(  ):

    player_id = new_enumerable(  )

    global_entity_list[ player_id ] = { f"animations": { f"idle": [ { f"time": 0 , f"frame": [ [ transparent , blue , transparent ] , [ blue , blue , blue ] , [ transparent , blue , transparent ] , [ blue , transparent , blue ] ] } ] } , f"collision": [ [ f"player" , f"player" , f"player" ] , [ f"player" , f"player" , f"player" ] , [ f"player" , f"player" , f"player" ] , [ f"player" , f"player" , f"player" ] ] , f"position_x": 0 , f"position_y": 0 , f"display_depth": 2 , f"screen": [  ] }

    for r in range( screen_height ):

        row_data = [  ]

        for c in range( screen_width ):

            row_data.append( transparent )



        global_entity_list[ player_id ][ f"screen" ].append( row_data )



    return player_id



if ( __name__ == f"__main__" ):

    try:

        main(  )



    except KeyboardInterrupt:

        print( f"Exiting" )



    finally:

        restore_terminal(  )

        restore_terminal_mode(  )

        sys.exit( 0 )





