from exp import exp
import pandas as pd
from psychopy import core
import numpy as np

# Initialize experiment
e = exp()
e.start_exp_handler(exp_info={'frame_rate': 240, 'participant': 999, 'session': 999, 'blocks_to_run': 2})

# Apply frame rate to timing
# e._set_debugging_time()
e.update_timing()

# create trials
# e.create_block_trials_list()
e.setup_total_trials()

print('exp started...')

# open window
e.open_window(monitor='vu', full_screen=True)

# simulate 
e._simulate_keys(True)

# ins
e.prime_instructions()

e._block_type = 'experiment'
# Prepare for new block. Increase block count and reset performance counters
e.reset_block()
e._trial_count = -1    

# Block settings
e._block_task = 'prime'   
e.run_block(20)    
e.show_performance()

e.prepare_new_block()
# second block
e._block_task = 'mask'   
e.reset_block()
e.run_block(20)    
e.show_performance()

# # third block
# e._block_task = 'prime'   
# e.reset_block()
# e.run_block()    
# e.show_performance()

# # second block
# e._block_task = 'mask'   
# e.reset_block()
# e.run_block()    
# e.show_performance()

# # third block
# e._block_task = 'prime'   
# e.reset_block()
# e.run_block()    
# e.show_performance()

# # second block
# e._block_task = 'mask'   
# e.reset_block()
# e.run_block()    
# e.show_performance()

# close window
e._win.close()

# ------------------------------------------------------------------

# stimuli duration

# filter data to keep only some keys of each list element
data = pd.DataFrame.from_records(e.exp_handler.entries)
# filter out trial_aborted rows
data = data[data['trial_aborted'] == False]

# average prime duration
avrg_prime_duration = (data['prime_time_duration'].mean() * 1000).round(1)
std_prime_duration =  np.round(np.std(data['prime_time_duration']) * 1000, 1)
# average prime duration
avrg_mask_fore_duration = (data['mask_fore_time_duration'].mean() * 1000).round(1)
std_mask_fore_duration =  np.round(np.std(data['mask_fore_time_duration']) * 1000, 1)

print('\n########################################',
      f'Average prime duration: {avrg_prime_duration} (SD={std_prime_duration})',
      f' -- Error on PRIME duration smaller than 1ms: {abs(avrg_prime_duration - 12.5) < 1}',
      f' -- PRIME SD smaller than 1ms: {abs(std_prime_duration) < 1}',
      f'Average mask duration: {avrg_mask_fore_duration} (SD={std_mask_fore_duration})', 
      f' -- Error on MASK duration smaller than 1ms: {abs(avrg_mask_fore_duration - 125) < 1}',
      f' -- MASK SD smaller than 1ms: {abs(std_mask_fore_duration) < 1}',
      '########################################', sep='\n')

print('########################################',
      f'Mask duration per task\n',
      (data.groupby(['task'])['mask_fore_time_duration'].mean() * 1000).round(1), 
      '########################################',
      sep='\n')

print('########################################',
      f'Mask duration per block\n',
      (data.groupby(['block_count'])['mask_fore_time_duration'].mean() * 1000).round(1), 
      '########################################',
      sep='\n')

# ------------------------------------------------------------------

# SOA duration

# filter data to keep only some keys of each list element
data = pd.DataFrame.from_records(e.exp_handler.entries)
# filter out trial_aborted rows
data = data[data['trial_aborted'] == False]
# mask_time_name = 'mask_fore_time_start_refresh'
mask_time_name = 'mask_back_time_start_refresh'
# calculate start time
data['mask_start_trial'] = data[mask_time_name] - data['trial_start']

# Print message

onset_diff = (data.groupby(['task'])['mask_fore_time_duration'].mean() * 1000).round(1)['mask'] - (data.groupby(['task'])['mask_fore_time_duration'].mean() * 1000).round(1)['prime']

print('########################################',
      f"Mask onset on MASK block: {(data.groupby(['task'])['mask_fore_time_duration'].mean() * 1000).round(1)['mask']}",
      f"Mask onset on PRIME block: {(data.groupby(['task'])['mask_fore_time_duration'].mean() * 1000).round(1)['prime']}",
      f" -- Onset difference smaller than 1ms: {abs(onset_diff) < 1}",
      "########################################",
      sep='\n')

# check SOAs
data = data[['task', 'block_count', 'soa', 'prime_time_start_refresh', mask_time_name]]
# calculate SOA
data['observed_SOA'] = data[mask_time_name] - data['prime_time_start_refresh']
# convert second to ms
data['observed_SOA'] = (data['observed_SOA'] * 1000).round(1)
data['soa'] = data['soa'] * 1000

# calcultate error
data['error_SOA'] = (data['observed_SOA'] - data['soa']) 
data['error_SOA'] = data['error_SOA'].round(1)

# SOA error per task
mask_SOA_error = data[['task', 'error_SOA']].groupby(['task'])['error_SOA'].mean().round(1)['mask']
prime_SOA_error = data[['task', 'error_SOA']].groupby(['task'])['error_SOA'].mean().round(1)['prime']

print('########################################',
      f"SOA error on MASK block: {mask_SOA_error}",
      f" -- Error smaller than 1ms: {abs(mask_SOA_error) < 1}",
      f"SOA error on PRIME block: {prime_SOA_error}",
      f" -- Error smaller than 1ms: {abs(prime_SOA_error) < 1}",
      "########################################",
      sep='\n')

print('########################################',
      f"SOA error smaller than 1ms in all blocks: {(data.groupby(['task', 'soa']).mean()['error_SOA'] < 1).all()}",
      "########################################",
      sep='\n')

print('########################################',
      f"SOA error in all blocks",
      (data.groupby(['task', 'soa']).mean()['error_SOA']).round(1),
      "########################################",
      sep='\n')

# ------------------------------------------------------------------


data = pd.DataFrame.from_records(e.exp_handler.entries)


data.groupby(['task', 'trial_aborted'])['trial_aborted'].count()

data = data[data['task'] == 'mask']

# number of aborted trials

data[data['trial_aborted'] == True]

aborted_trials = len()



len(data[data['trial_aborted'] == False])

# filter out trial_aborted rows
data = data[data['trial_aborted'] == False]

# ------------------------------------------------------------------

# filter data to keep only some keys of each list element
data = pd.DataFrame.from_records(e.exp_handler.entries)
data = data[data['task'] == 'mask']
# select only the relevant columns
data = data[['block_count', 'prime_direction', 'mask_direction', 'stim_position', 'soa', 'trial_aborted', 'mask_rt']]
# filter out trial_aborted rows
data = data[data['trial_aborted'] == False]
# check that no RT is slower then .7
data['rt_ok'] = data['mask_rt'] < .7
# check that all rows of rt_ok are True
data['rt_ok'].all()

# ------------------------------------------------------------------

# filter data to keep only some keys of each list element
data = pd.DataFrame.from_records(e.exp_handler.entries)
# filter out trial_aborted rows
data = data[data['trial_aborted'] == False]
# select only the relevant columns
data = data[['block_count', 'prime_presence', 'prime_direction', 'mask_direction', 'stim_position', 'soa', 'trial_aborted']]
# group by block_count, prime_direction, mask_direction, stim_position, soa and sum the count
data = data.groupby(['block_count', 'prime_direction', 'mask_direction', 'stim_position', 'soa']).value_counts().reset_index(name='unique_trials')
# check all is one
data.groupby(['block_count'])['unique_trials'].all()

# count total trials
data.groupby(['block_count']).count()

# core.quit()

