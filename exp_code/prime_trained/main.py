"""
~~ motor priming experiment

this script uses the class exp (from exp.py) to run the experiment with the appropriate settings.

@ nicolás sánchez-fuenzalida
may 10, 2024
"""

from exp import exp
from psychopy import core

def run_experiment(experiment_info):

    # Initialize experiment
    e = exp()
    e.start_exp_handler(exp_info=experiment_info)
    # Set up trials    
    e.create_block_trials_list(repeat_unique_trials=2)
    e.setup_total_trials()    

    # Update timing
    e.update_timing()
    # open window
    e.open_window(monitor='vu', full_screen=True, screen_index=0)
    
    # Adjust chin-rest
    e.adjust_chinrest()

    # Demographics
    if experiment_info['demographics']:
        e.run_demographic_questions()

    # Experiment welcome
    e.experiment_welcome()

    # Prime Instructions
    if experiment_info['prime_instructions']:
        e.prime_instructions()
        e.prime_practice()

    # Mask Instructions
    if experiment_info['mask_instructions']:
        e.mask_instructions()
        e.mask_practice()

    # Blocked task instructions
    e.show_message(text='The experiment is divided into blocks. In each block you will be asked to identify the direction of the first or second arrow. You will be informed of the task at the beginning of each block. Press SPACE to continue.',
                   color='black', height=e._default_text_height * .9, wait_keypress=['space'])
    core.wait(.1)

    # Warm up
    if experiment_info['warm_up']:
        e.warm_up()

    # Run the experiment
    e.show_message(text='The experiment is about to start.\n Press A or L to begin.',
                   color='black', height=e._default_text_height * .9, wait_keypress=['a', 'l'])

    # Reset trial count
    e._trial_count = -1
    e._valid_trial_count = -1
    e._block_count = -1
    e._block_type = 'experiment'

    # Run blocks
    for block in range(e._blocks_to_run):
        # reset block vars
        e.prepare_new_block()
        # run trials
        e.run_block()
        # performance
        if block+1 == 6: # forced break half way
            e.show_performance(have_break='forced')
        else:
            e.show_performance(have_break='short')

    # The experiment is over
    e.print_progress(f'Experiment done!')
    e.show_message(text='The experiment is over. Thanks for participating! The researcher will be with you shortly.',
                   color='black', height=e._default_text_height * .9, wait_keypress=['space'])
    
    # Save data
    e.win.close()
    e.save_csv()
    e.exp_handler.abort()
    core.quit()

if __name__ == '__main__':
        
    # Run experiment
    run_experiment(exp().get_experiment_info())
