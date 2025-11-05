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
    
    # create trials
    e.create_block_trials_list()
    e.setup_total_trials()

    # Apply frame rate to timing
    e._set_default_timing()
    e.update_timing()
    
    # open window
    e.open_window(monitor='vu', full_screen=True, screen_index=0)
    
    # Adjust chin-rest
    e.adjust_chinrest()

    # Demographics
    if experiment_info['demographics']:
        e.run_demographic_questions()
    
    # mask discrimination, prime detection instructions    
    e.mask_instructions()

    # Run the experiment
    e.show_message(text='Press A or L to start the experiment.',
                   color='black', height=e._default_text_height * .9, 
                   wait_keypress=['a', 'l'])

    # Reset trial count
    e._block_type = 'experiment'
    e._trial_count = -1
    e._valid_trial_count = -1
    e.reset_block()

    # Mask block
    e._block_task = 'mask'   
    for b in range(e._blocks_to_run):
        # Remind task after first block
        if b != 0:
            e.prepare_new_block()
        # Run trials
        e.run_block()    
        e.show_performance()

    
    # Prime task instruction
    e.prime_instructions()
    
    # Start second part of the experiment
    e.show_message(text='Press A or L to start.',
                   color='black', height=e._default_text_height * .9, 
                   wait_keypress=['a', 'l'])
    
    # Prime block
    e.reset_block()
    e._block_task = 'prime'
    for b in range(e._blocks_to_run):
        # Remind task after first block
        if b != 0:
            e.prepare_new_block()
        e.run_block()    
        e.show_performance()

    # Save data
    e.save_csv()

    # The experiment is over
    e.print_progress(f'Experiment done!')
    e.show_message(text='The experiment is over. Thanks for participating! The researcher will be with you shortly.',
                   color='black', height=e._default_text_height * .9, wait_keypress=['space'])
    
    # Save data
    e.win.close()
    e.exp_handler.abort()
    core.quit()

if __name__ == '__main__':
        
    # Run experiment
    run_experiment(exp().get_experiment_info())
