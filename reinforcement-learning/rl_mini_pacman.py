import numpy as np
import random
import time
import os
import gc

from keras.models import Sequential, clone_model
from keras.layers import Dense, Flatten, Conv2D, InputLayer
from keras.callbacks import CSVLogger, TensorBoard
from keras.optimizers import Adam
import keras.backend as K

import gym
from collections import deque
from mini_pacman import PacmanGame


def get_state(obs):
    v = []
    x,y = obs['player']
    v.append(x)
    v.append(y)
    for x, y in obs['monsters']:
        v.append(x)
        v.append(y)
    for x, y in obs['diamonds']:
        v.append(x)
        v.append(y)
    for x, y in obs['walls']:
        v.append(x)
        v.append(y)
    return v

#env = gym.make("MsPacman-ram-v0")
env = PacmanGame(field_shape=(8, 8), nmonsters=2, ndiamonds=3, nwalls=10, monster_vision_range=2, max_moves=100, diamond_reward=100, survival_reward=1)
#env = PacmanGame(field_shape=(10,10), nmonsters=2,ndiamonds=3, nwalls=4, monster_vision_range=1)
obs = env.reset()
end_game = False
score = 0

def create_dqn_model(input_shape, nb_actions, dense_layers, dense_units):
    model = Sequential()
    model.add(InputLayer(input_shape=input_shape))
    for i in range(dense_layers):
        model.add(Dense(units=dense_units, activation='relu'))
    model.add(Dense(nb_actions, activation='linear'))
    return model

input_shape = (32,) #obs.shape
nb_actions = 9 #env.action_space.n  # 9
dense_layers = 5
dense_units = 256

online_network = create_dqn_model(input_shape, nb_actions, dense_layers, dense_units)


def epsilon_greedy(q_values, epsilon, n_outputs):
    if random.random() < epsilon:
        return random.randrange(1, n_outputs, 1)  # random action
    else:
        return np.argmax(q_values)          # q-optimal action





replay_memory_maxlen = 1000000
replay_memory = deque([], maxlen=replay_memory_maxlen)


target_network = clone_model(online_network)
target_network.set_weights(online_network.get_weights())


name = 'MiniPacman_DQN'  # used in naming files (weights, logs, etc)
n_steps = 100000        # total number of training steps (= n_epochs)
warmup = 1000          # start training after warmup iterations
training_interval = 6  # period (in actions) between training steps
save_steps = int(n_steps/10)  # period (in training steps) between storing weights to file
copy_steps = 500       # period (in training steps) between updating target_network weights
gamma = 0.9            # discount rate
skip_start = 90        # skip the start of every game (it's just freezing time before game starts)
batch_size = 128        # size of minibatch that is taken randomly from replay memory every training step
double_dqn = False     # whether to use Double-DQN approach or simple DQN (see above)
# eps-greedy parameters: we slowly decrease epsilon from eps_max to eps_min in eps_decay_steps
eps_max = 1.0
eps_min = 0.05
eps_decay_steps = int(n_steps/2)

learning_rate = 0.001

def mean_q(y_true, y_pred):
    return K.mean(K.max(y_pred, axis=-1))


online_network.compile(optimizer=Adam(learning_rate), loss='mse', metrics=[mean_q])



if not os.path.exists(name):
    os.makedirs(name)
    
weights_folder = os.path.join(name, 'weights')
if not os.path.exists(weights_folder):
    os.makedirs(weights_folder)


csv_logger = CSVLogger(os.path.join(name, 'log.csv'), append=True, separator=';')





# counters:
step = 0          # training step counter (= epoch counter)
iteration = 0     # frames counter
episodes = 0      # game episodes counter
end_game = True       # indicator that env needs to be reset

episode_scores = []  # collect total scores in this list and log it later

while step < n_steps:
    if end_game:  # game over, restart it
        obs = env.reset()
        score = 0  # reset score for current episode
        for skip in range(skip_start):  # skip the start of each game (it's just freezing time before game starts)
            try:
                obs = env.make_action(1) #env.step(0)
                reward = obs['reward']
                end_game = obs['end_game']
                score += reward
            except AssertionError:
                continue

        state = get_state(obs)
        episodes += 1

    # Online network evaluates what to do
    iteration += 1
    q_values = online_network.predict(np.array([state]))[0]  # calculate q-values using online network

    # select epsilon (which linearly decreases over training steps):
    epsilon = max(eps_min, eps_max - (eps_max-eps_min) * step/eps_decay_steps)
    action = epsilon_greedy(q_values, epsilon, nb_actions)
    
    # Play:
    try:
        obs = env.make_action(action)
        reward = obs['reward']
        end_game = obs['end_game']
    except AssertionError:
        continue

    score += reward

    if end_game:
        episode_scores.append(score)
    next_state = get_state(obs)
    # Let's memorize what just happened
    replay_memory.append((state, action, reward, next_state, end_game))
    state = next_state

    if iteration >= warmup and iteration % training_interval == 0:
        # learning branch
        step += 1
        #minibatch = random.sample(replay_memory, batch_size)

        minibatch = []
        selectcnt = 4
        batch_counter = 0
        batch_limit = batch_size/selectcnt
        batch_selection_complete = False
        batch_indices = []
        
        while(not batch_selection_complete):
            cstart = random.randint(0, len(replay_memory) - selectcnt)

            if(any(ind in batch_indices for ind in (cstart, cstart+1, cstart+2, cstart+3))):            
                continue
                
            batch_indices.append(cstart)
            
            minibatch = (minibatch + (list(itertools.islice(replay_memory, cstart, cstart + selectcnt))))
            
            batch_counter += 1
            
            if(batch_counter == batch_limit):
                batch_selection_complete = True

        replay_state = np.array([x[0] for x in minibatch])
        replay_action = np.array([x[1] for x in minibatch])
        replay_rewards = np.array([x[2] for x in minibatch])
        replay_next_state = np.array([x[3] for x in minibatch])
        replay_end_game = np.array([x[4] for x in minibatch], dtype=int)

        # calculate targets (see above for details)
        if double_dqn == False:
            # DQN
            target_for_action = replay_rewards + (1-replay_end_game) * gamma * \
                                    np.amax(target_network.predict(replay_next_state), axis=1)
        else:
            # Double DQN
            best_actions = np.argmax(online_network.predict(replay_next_state), axis=1)
            target_for_action = replay_rewards + (1-replay_end_game) * gamma * \
                                    target_network.predict(replay_next_state)[np.arange(batch_size), best_actions]

        target = online_network.predict(replay_state)  # targets coincide with predictions ...
        target[np.arange(batch_size), replay_action] = target_for_action  #...except for targets with actions from replay
        
        # Train online network
        online_network.fit(replay_state, target, epochs=step, verbose=2, initial_epoch=step-1,
                           callbacks=[csv_logger])

        # Periodically copy online network weights to target network
        if step % copy_steps == 0:
            target_network.set_weights(online_network.get_weights())
        # And save weights
        if step % save_steps == 0:
            online_network.save_weights(os.path.join(weights_folder, 'weights_{}.h5f'.format(step)))
            gc.collect()  # also clean the garbage




online_network.save_weights(os.path.join(weights_folder, 'weights_last.h5f'))
online_network.save("dqn_model.h5")

# Dump all scores to txt-file
with open(os.path.join(name, 'episode_scores.txt'), 'w') as file:
    for item in episode_scores:
        file.write("{}\n".format(item))

