#import matplotlib.pyplot as plt
import numpy as np
import random
import time
import pyglet

from mini_pacman import PacmanGame

env = PacmanGame(field_shape=(10,10), nmonsters=2,
                 ndiamonds=3, nwalls=4, monster_vision_range=1)
env.print_field()  # will print a picture in text symbols
#env.render()  # creates graphical rendering of the field



# Render random-action game
obs = env.reset()
while not obs['end_game']:
    action = random.choice(obs['possible_actions'])
    obs = env.make_action(action)
    env.render()
    time.sleep(0.5)              