"""
~~ motor priming experiment

this script contains the class that runs the experiment. another script (main.py) uses the class exp to run the experiment with the appropriate settings.


@ nicolás sánchez-fuenzalida
apr 22, 2024
"""
script_version = '0.0.1'

import os
import warnings
import random
import string
import numpy as np
import psychopy
from psychopy import visual, monitors, event, data, core, gui
from psychopy.constants import NOT_STARTED, STARTED, STOPPED
from psychopy.hardware import keyboard
# this may be different in windows
from psychopy import prefs
prefs.hardware['audioLib'] = ['pygame']
from psychopy import sound
import psychtoolbox as ptb
import copy

# Set psychopy version
psychopyVersion = '2023.1.3'
psychopy.useVersion(psychopyVersion)
print(f'Using psychopy version {psychopy.__version__}')

# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)
# Experiment name for logging
experiment_name = 'prime_control'
# Clock for experiment time
exp_clock = core.Clock()

# This class contains the entire experiment and instruction
class exp:

    def __init__(self):

        print('Initializing experiment ...')
        
        # pre-create some vars ----------------

        # for general experiment handling
        self._this_dir = _thisDir
        self.exp_handler = None
        self._experiment_is_over = False
        self._experiment_info = None
        
        # for saving data
        self._filename = None

        # for trial tracking
        self._trial_count = None
        self._valid_trial_count = None
        self._block_count = None
        self._block_trial_count = None
        self._total_trials = None
        self._block_type = None
        self._last_block = None
        self._blocks_to_run = None
        self._block_trials = None
        self._block_task = None
        self.trials_in_block = None
        self._block_running = None

        # for trial settings
        self._trial_feedback = True

        # for current trial
        self._this_trial_correct_response = None
        self._this_trial_accuracy = None
        self._this_trial_answer = None
        self._this_trial_rt = None
        self._this_trial_task = None
        self._print_answer_info = None

        # for monitor
        self._win = None
        self._flip_it = None
        self._frame_rate = None

        # for stimuli timing
        self._fixation_duration_f = None
        self._prime_duration_f = None
        self._soa_duration_f = None
        self._mask_duration_f = None
        self._soa_f = None

        # for performance
        self._prime_correct_count = None
        self._prime_trial_count = None
        self._mask_correct_count = None
        self._mask_trial_count  = None
        self._mask_rt = None

        # for staircase
        self._stairs = None
        
        # for timing
        self._set_default_timing()

        # for tasks
        self._tasks = ['prime', 'mask']

        # for stimuli settings
        self._prime_directions = ['left', 'right']
        self._mask_directions = ['left', 'right']
        self._stim_positions = ['top', 'bottom']

        # for presenting text
        self._default_text_height = .7
        self._default_wrap_width = 35

        # for stim contrast (this is only used during the instructions)
        self._prime_contrast = 1
        self._mask_contrast = 1

        # others
        self.simulate_answers = None
        self._force_correct = None
        self._log_instructions = None
        self._log_file_name = None
        self._forced_break_recurrence = 4
        self._forced_break_duration = 180 # 3 minutes
    
        # for key capturing
        self.kb = keyboard.Keyboard()
        self._key_left = 'a'
        self._key_right = 'l'

        # for key simulation
        if self.simulate_answers:
            self.waitKeys = self._simulate_wait_keys
            self.getKeys = self._simulate_get_keys
        else:
            self.waitKeys = self.kb.waitKeys
            self.getKeys = self.kb.getKeys
        self._rt_mean_simulated = 0.7
        self._rt_sd_simulated = 0.1

    def start_exp_handler(self, exp_info=None):
        """
        Starts the experiment handler and sets up necessary information for data logging.

        :param exp_info: (dict) Optional. A dictionary containing experiment information.
                         Default values will be used if not provided.
                         The dictionary should include the following keys:
                         - 'frame_rate': The frame rate of the experiment (default: 60).
                         - 'participant': The participant identifier (default: '00').

        :return: None
        """

        if exp_info is None:
            # Default values for gui
            self._experiment_info = {'frame_rate': 60,
                                     'participant': '999',
                                     'blocks_to_run': '1',
                                     'first_task': 'prime'}
        else:
            # get experiment info from input
            self._experiment_info = exp_info
            # add default values if needed
            if not 'frame_rate' in self._experiment_info.keys():
                self._experiment_info['frame_rate'] = 60
            if not 'participant' in self._experiment_info.keys():
                self._experiment_info['participant'] = '999'
            if not 'blocks_to_run' in self._experiment_info.keys():
                self._experiment_info['blocks_to_run'] = '1'

        # add script version to _experiment_info for automatic logging
        self._experiment_info['script_version'] = script_version

        # Check if 'frame_rate' and 'participant' keys exist in exp_info
        assert 'frame_rate' in self._experiment_info, "'frame_rate' key does not exist in exp_info."
        assert 'participant' in self._experiment_info, "'participant' key does not exist in exp_info."
        assert 'blocks_to_run' in self._experiment_info, "'blocks_to_run' key does not exist in exp_info."

        # Pad subject number
        self._experiment_info['participant'] = str(self._experiment_info['participant']).zfill(3)

        # frame is used a lot throughout the experiment, so it's easier to set up a new var
        self._frame_rate = self._experiment_info['frame_rate']
        self._blocks_to_run = self._experiment_info['blocks_to_run']
                
        # Add some info to be logged in every row
        self._experiment_info['date'] = data.getDateStr()  # add a simple timestamp
        self._experiment_info['expName'] = experiment_name
        self._experiment_info['psychopyVersion'] = psychopyVersion
        self._experiment_info['script_version'] = script_version

        # Data file name
        self._filename = 'data_%s_%s_%s' % (
            self._experiment_info['participant'], 
            experiment_name, self._experiment_info['date'])
        # File name with fullpath
        self._filename_full_path = os.path.join(self._this_dir, 'data', self._filename)

        # An ExperimentHandler isn't essential but helps with data saving
        self.exp_handler = data.ExperimentHandler(name=experiment_name, version=psychopy.__version__,
                                                  extraInfo=self._experiment_info,
                                                  savePickle=False, saveWideText=True,
                                                  dataFileName=self._filename_full_path)    
        
        # set up progress file
        self.setup_progress_log()

    def update_timing(self):
        """
        Convert default timing values (ms) to frames using the frame rate of the experiment.

        :return: None
        """       
        self._fixation_duration_f = int(self._fixation_duration_s * self._frame_rate)
        self._prime_duration_f = int(self._default_prime_duration_s * self._frame_rate)
        self._mask_duration_f = int(self._default_mask_duration_s * self._frame_rate)


    def open_window(self, monitor=None, full_screen=False, screen_index=0, size=None, background_color='white'):
        """
        Opens a window for displaying visual stimuli.

        :param monitor: Optional. Specifies the monitor to use for the window. It can be provided as:
                        - A string representing the name of the monitor.
                        - An instance of `monitors.Monitor`.
                        - None, to use a generic monitor.

        :param full_screen: Optional. Specifies whether the window should be opened in full-screen mode (default: False).

        :param screen_index: Optional. Specifies the index of the screen to use when using multiple monitors (default: 0).

        :param size: Optional. Specifies the size of the window as a list containing the width and height in pixels.
                     If not provided, a default size of [900, 800] will be used.

        :raises InvalidMonitorType: If the provided monitor parameter is of an invalid type.

        :return: None
        """
        
        class InvalidMonitorType(Exception):
            pass

        # Default size for debugging
        if size is None:
            size = [900, 800]

        # Check monitor input
        if isinstance(monitor, str):
            print(f'looking for monitor {monitor} in monitor pocket')
            if monitor in self.monitor_pocket().keys():
                print('monitor found, opening window')
                self._win = visual.Window(monitor=self.monitor_pocket()[monitor], fullscr=full_screen,
                                          screen=screen_index, size=size, units='deg', color=background_color)
            else:
                warnings.warn(f'monitor {monitor} not found, opening generic window')
                warnings.warn(f'degrees cannot be used as window unit with generic monitor')
                self._win = visual.Window(fullscr=full_screen, screen=screen_index, size=size, color=background_color)

        elif isinstance(monitor, monitors.Monitor):
            print('opening window with input monitor')
            self._win = visual.Window(monitor=monitor, fullscr=full_screen, screen=screen_index, size=size,
                                      units='deg', color=background_color)
        elif monitor is None:
            warnings.warn(f'no monitor provided, opening generic window')
            warnings.warn(f'degrees cannot be used as window unit with generic monitor')
            self._win = visual.Window(fullscr=full_screen, screen=screen_index, size=size, color=background_color)
        else:
            raise InvalidMonitorType(
                "Invalid monitor type. Please provide a monitor name as a string, monitors.Monitor() instance, "
                "or None to get a generic monitor.")

        # Set flip function
        self._flip_it = self._win.flip
        self._record_frames = False
        self._win.mouseVisible = False
        self._mouse=event.Mouse(win=self._win ,visible=False)
        self._mouse.setVisible(False)
        self._mouse.setExclusive(True)

    def close_win(self):
        self._win.close()

    class timeTracker:
        def __init__(self):
            self.first_frame = None
            self.response_prompt = None

    def show_message(self, wait_keypress: list = None, max_wait=float('inf'), return_text=False, block_keypress=None, **kwargs):
        """
        Displays a message on the screen using the psychopy TextStim class and waits for a keypress if specified.

        :param wait_keypress: Optional. A list of keys to wait for. If None, the function returns immediately.

        :param max_wait: Optional. Time in seconds to wait for a keypress. This parameter is ignored if wait_keypress is None.

        :param return_text: Optional. If True, the function returns the TextStim object instead of drawing it on the screen.

        :param kwargs: Additional keyword arguments that can be passed to the `visual.TextStim` class constructor.
                   These arguments control the properties of the text stimulus, such as text content, font, color, etc.
                   Refer to the documentation of `visual.TextStim` for more details.

        :return: If return_text is True, returns the TextStim object. Otherwise, returns None.
        
        :notes: If the function `_set_log_instructions` is used, the text may be logged in a log file.
        """

        # set default response block
        if block_keypress is None:
            block_keypress = 0.2

        # Create a TextStim using the **kwargs dictionary
        text_stim = visual.TextStim(self._win, **kwargs)

        # Log instructions if enabled
        if self._log_instructions:
            # Try to write line to log file, if it fails, print warning
            try:
                with open(self._log_file_name, "a") as file:
                    file.write(f'{text_stim.text}\n\n')
            except Exception as e:
                warnings.warn(f'Failed to write to log file: {e}')

        # Return text or draw it
        if return_text:
            # if wait_keypress is not None, raise a warning indicating that it will be ignored
            if wait_keypress is not None:
                warnings.warn("wait_keypress parameter will be ignored when return_text is True.")
            
            return text_stim
        else:
            # draw text
            text_stim.draw()
            self._flip_it()
            core.wait(block_keypress)

        # If list of keys provided, wait for keys, otherwise text is shown until next win flip
        if isinstance(wait_keypress, list):
            if len(wait_keypress):
                # In Windows a key press before the waitKeys function is called is registered
                # and ends the routine immediately. This is most likely a bug as clearing key
                # presses using the keyboard module or the event module does not work. This is
                # a workaround that stops the code until no key press is registered 
                while True:
                    keys = self.kb.getKeys(keyList=wait_keypress)
                    if not keys:
                        break
                # Wait for key press
                self.waitKeys(keyList=wait_keypress, maxWait=max_wait)
                self._flip_it()

    def reset_block(self):
        # Set block count to -1 if None
        if self._block_count is None:
            self._block_count = -1
        # Increase block count
        self._block_count += 1
        # Reset performance counter
        self._prime_correct_count = 0
        self._prime_trial_count = 0
        self._mask_correct_count = 0
        self._mask_trial_count = 0
        self._mask_rt = []
        # Shuffle trial list
        np.random.shuffle(self._block_trials_mask)
        np.random.shuffle(self._block_trials_prime)

    def _simulate_wait_keys(self, maxWait=None, keyList=None, modifiers=None, timeStamped=False, clear=None):

        # Get force correct value
        force_correct = self._force_correct

        # Sample a number that on average is 0.4 and is never lower than 0
        rt = max(random.gauss(self._rt_mean_simulated, self._rt_sd_simulated), 0)
        
        # If force correct return correct key
        if force_correct:
            if self._this_trial_correct_response is None:
                warnings.warn('_simulate_wait_keys: Cannot force correct if correct response is None')
                if keyList is None:
                    name = random.choice(string.ascii_letters)
                else:
                    name = random.choice(keyList)
            else:
                if self._this_trial_correct_response == 'left':
                    name = self._key_left
                elif self._this_trial_correct_response == 'right':
                    name = self._key_right
        else:
            # Randomly pick an element of keyList, or a letter if keyList is None
            if keyList is None:
                name = random.choice(string.ascii_letters)
            else:
                name = random.choice(keyList)

        # Check if maxWait is not None and if rt is greater than maxWait
        if maxWait is not None and rt > maxWait:
            # If so, return None
            return None

        # Use psychopy.core.wait() to wait for rt seconds
        core.wait(rt)

        # Create a list with a single element, which is a list containing the name and rt variables
        response = self.custom_KeyPress(name, rt)

        # Return the response list
        return [response]

    def _simulate_get_keys(self, keyList=None, waitRelease=True, clear=True):
        
        # Get force correct value
        force_correct = self._force_correct

        # Sample a number that on average is 0.4 and is never lower than 0
        rt = max(random.gauss(self._rt_mean_simulated, self._rt_sd_simulated), 0)
        
        # If force correct return correct key
        if force_correct:
            if self._this_trial_correct_response is None:
                warnings.warn('_simulate_get_keys: Cannot force correct if correct response is None')
                if keyList is None:
                    name = random.choice(string.ascii_letters)
                else:
                    name = random.choice(keyList)
            else:
                if self._this_trial_correct_response == 'left':
                    name = self._key_left
                elif self._this_trial_correct_response == 'right':
                    name = self._key_right
        else:
            # Randomly pick an element of keyList, or a letter if keyList is None
            if keyList is None:
                name = random.choice(string.ascii_letters)
            else:
                name = random.choice(keyList)

        # Create a list with a single element, which is a list containing the name and rt variables
        response = self.custom_KeyPress(name, rt)

        # if rt is greater than time passed since keyboard reset, return response   
        if self.kb.clock.getTime() > rt:
            # Return the response list
            return [response]
        # otherwise return an empty list
        else:
            return []

    class custom_KeyPress:
        def __init__(self, name, rt):
            self.name = name
            self.rt = rt

    def _simulate_keys(self, simulate):
        # Set answer function
        if simulate:
            self.waitKeys = self._simulate_wait_keys
            self.getKeys = self._simulate_get_keys
        else:
            self.waitKeys = self.kb.waitKeys
            self.getKeys = self.kb.getKeys

    def play_feedback(self, correct=None, show=None):
        """
        Plays feedback sound on incorrect trials.

        Args:
            correct (bool, optional): The correctness of the trial. If None, the value from the current trial is used.
            show (bool, optional): Whether to show the feedback. If None, the value from the current trial is used.

        Raises:
            UserWarning: If the accuracy is None and feedback is requested.

        Returns:
            None
        """
        
        # remove feedback from prime detection
        if self._this_trial_task == 'prime':
            
            # set default values
            correct = correct if correct is not None else self._this_trial_prime_accuracy
            show = show if show is not None else self._trial_feedback

            if show:
                # check if correct is not None
                if correct is None:
                    # raise warning
                    warnings.warn('Accuracy is None, cannot give feedback')
                # there is only feedback if the trial is incorrect
                elif not correct:
                    # play wrong sound
                    wrong_sound = sound.Sound('A')
                    now = ptb.GetSecs()
                    wrong_sound.play(when=now+.1)
                    core.wait(.1)

    def display_feedback(self, correct=None, show=None):
        """
        Displays feedback on incorrect trials of mask task.

        Args:
            correct (bool, optional): The correctness of the trial. If None, the value from the current trial is used.
            show (bool, optional): Whether to show the feedback. If None, the value from the current trial is used.

        Raises:
            UserWarning: If the accuracy is None and feedback is requested.

        Returns:
            None
        """
        # set default values
        correct = correct if correct is not None else self._this_trial_mask_accuracy
        show = show if show is not None else self._trial_feedback

        if show:
            # check if correct is not None
            if correct is None:
                # raise warning
                warnings.warn('Accuracy is None, cannot give feedback')
            # there is only feedback if the trial is incorrect
            elif not correct:
                self.show_message(text='INCORRECT', color='red', pos=[0,1],
                                  height=self._default_text_height, wait_keypress=['+'], max_wait=.3)

    def flip_record(self):
        self._win.flip()
        self._win.getMovieFrame()

    def _record_flips(self, record):
        self._record_frames = record
        if record:
            self._flip_it = self.flip_record
        else:
            self._flip_it = self._win.flip
        warnings.warn(f'Record flips: {record}')

    def run_demographic_questions(self):
        # reset clock to get a better estimation of time
        exp_clock.reset()

        print('Running demographic questions...')

        # Add block type info
        self.exp_handler.addData('block_type', 'demographic_questions')
        self.exp_handler.addData('trial_type', 'demographic_questions')

        # For capturing numbers
        all_digits = list(string.digits) + ['num_' + x for x in list(string.digits)]

        # Age ==============================================================================

        age_limit = [18, 35]
        age_question = f'How old are you? Type your answer and press SPACE when you are ready.'
        text_stim = self.show_message(text=age_question + '\n\n ', height=self._default_text_height, wrapWidth=25, pos=[0, 0], anchorVert='center', return_text=True, color='black')

        text_stim.setAutoDraw(True)
        self._flip_it()
        # Ask for input
        age_text = ''
        # Get keys
        while True:
            # wait for keys
            keys = event.waitKeys(keyList=all_digits + ['space', 'backspace'])
            # add number
            if not keys[0] in ['space', 'backspace']:
                age_text += keys[0][-1]
            # delete last
            elif keys[0] == 'backspace':
                age_text = age_text[:-1]
            # submit only if age is correct
            elif keys[0] == 'space':
                if len(age_text):
                    if age_limit[0] <= int(age_text) <= age_limit[1]:
                        text_stim.setAutoDraw(False)
                        self._flip_it()
                        break

            # Update text with question and age
            text_stim.setText(age_question + f'\n\n{age_text}')
            self._flip_it()

        core.wait(.3)
        # Log answer
        self.exp_handler.addData('dem_age', age_text)

        # Gender ==========================================================================================
        
        # Show question
        self.show_message(text='What is your gender (press a number)?'
                               '\n\n1. Female\n2. Male\n3. Other\n4. '
                               'I prefer not to say', 
                               height=self._default_text_height, wrapWidth=25, pos=[0, 0], anchorVert='center', color='black')
        key_list=list(filter(lambda x: x[-1] in ['1', '2', '3', '4'], all_digits))
        # wait til buffer is clean
        while True:
            keys = self.kb.getKeys(keyList=key_list)
            if not keys:
                break
        # Capture keys
        keys = self.waitKeys(keyList=key_list)
        self._flip_it()
        core.wait(.3)
        
        # Log answer
        self.exp_handler.addData('dem_gender', {'1': 'female', '2': 'male', '3': 'other', '4': 'not_say'}[keys[-1].name[-1]])

        # Vision ==========================================================================================

        self.show_message(text='Are you wearing glasses or contact '
                               'lenses? If you usually wear any of '
                               'them when using a computer you should '
                               'put them on before continuing.\n\n1. Yes\n2. No',
                               height=self._default_text_height, wrapWidth=25, pos=[0, 0], anchorVert='center', color='black')
        key_list=list(filter(lambda x: x[-1] in ['1', '2'], all_digits))
        # wait til buffer is clean
        while True:
            keys = self.kb.getKeys(keyList=key_list)
            if not keys:
                break
        # Capture keys
        keys = self.waitKeys(keyList=key_list)
        self._flip_it()
        core.wait(.3)
        # Log answer
        self.exp_handler.addData('dem_vision', {'1': 'corrected', '2': 'uncorrected'}[keys[-1].name[-1]])
        self.exp_handler.nextEntry()
        print('Demographic questions done')
    
    @staticmethod
    def monitor_pocket():
        """
        List of monitors to open a window with

        """
        # Black Lenovo Think-pad E15 Gen 2
        mons = {'nostromo': monitors.Monitor(name='nostromo', width=34.4, distance=63)}
        mons['nostromo'].setSizePix((1910, 1000))
        # 4K Samsung screen
        mons['samsung4k'] = monitors.Monitor(name='samsung4k', width=60.7, distance=63)
        mons['samsung4k'].setSizePix((2560, 1040))
        # (allegedly) VU stim monitor
        mons['vu'] = monitors.Monitor(name='vu', width=52.6, distance=75)
        mons['vu'].setSizePix((1920, 1080))

        return mons

    @property
    def win(self):
        return self._win

    def save_csv(self):
        print('\n\n')

        data_saved_locally = False
        data_saved_exp_folder = False
        data_saved_project_folder = False

        # ONLINE: Attempt to save csv file in data folder in experiment folder
        try:
            self.exp_handler.saveAsWideText(self._filename_full_path + '.csv')
            data_saved_exp_folder = True
        except Exception as e:
            print("Can't save the file:", str(e))
        # ONLINE: Attempt to save csv file in project data folder
        try:
            self.exp_handler.saveAsWideText(os.path.join(os.path.dirname(_thisDir), 'Data', self._filename + '.csv'))
            data_saved_project_folder = True
        except OSError as e:
            print("Can't save the file:", str(e))

        # OFFLINE: Attemp to save csv file locally
        try:
            self.exp_handler.saveAsWideText(os.path.join(r'C:\Users\prime\DATA', self._filename + '.csv'))
            data_saved_locally = True
        except OSError as e:
            print("Can't save the file:", str(e))
        
        # Print 
        print('\n\n#############################\n')
        print(f'Data saved on Exp folder:       {data_saved_exp_folder}')
        print(f'Data saved on Project folder:   {data_saved_project_folder}')
        print(f'Data saved on Local folder:     {data_saved_locally}')
        print('\n#############################\n\n')

    def make_fixation(self):
        return visual.ShapeStim(self._win, vertices='cross', lineColor='black', fillColor='black', size=.3, units='deg')

    def make_prime(self, direction, vertical_position):
        """
        Create an arrow stimulus to be used as prime using Vorberg et al. (2003) settings.

        Parameters:
        - direction (str): The direction of the arrow. Can be either 'right' or 'left'.
        - vertical_position (str): The vertical position of the arrow. Can be 'top', 'bottom', or 'center'.

        Returns:
        - arrow (visual.ShapeStim): The visual arrow stimulus.

        """

        # prime length and height in degrees (from vorberg et al. 2003)
        length = 1.86
        height = 0.8

        # define the vertices of the arrow
        vertices = [
            (0.93,                  0),                 # 1. Tip of the arrow
            (0.53,                  -0.4),      # 2. Bottom-right vertex of the tip 
            (-0.93,                 -0.395),      # 3. 
            (-0.53,                 0),                 # 4. tip of the inner arrow
            (-0.93,                 0.4),         # 5. Bottom-left vertex of the tip
            (0.53,                  0.4)          # 6. 
        ]

        # check length and height
        # find leftmost and rightmost point in vertices
        leftmost_point = min([x[0] for x in vertices])
        rightmost_point = max([x[0] for x in vertices])
        vertices_length = rightmost_point - leftmost_point
        f'vertices length is {vertices_length} and height is {length}'
        # find topmost and bottommost point in vertices
        top_point = max([x[1] for x in vertices])
        bottom_point = min([x[1] for x in vertices])
        vertices_height = top_point - bottom_point
        f'vertices height is {vertices_height} and height is {height}'
        
        # adapt the arrow vertices to the direction
        if direction == 'right':
            oriented_vertices = vertices
        elif direction == 'left':
            oriented_vertices = [(x * -1, y) for x, y in vertices]

        # arrow position in degrees. the y coordinate indicates the position between
        # the center of the screen and the center of the arrow
        if vertical_position == 'top':
            prime_coords = (0, 1.375)
        elif vertical_position == 'bottom':
            prime_coords = (0, -1.375)
        elif vertical_position == 'center':
            prime_coords = (0, 0)

        # Draw the scaled arrow using polygon
        arrow = visual.ShapeStim(self.win, name='prime', vertices=oriented_vertices, pos=prime_coords, 
                                 lineColor='black', fillColor='black', size=1, units='deg', interpolate=False,
                                 contrast=self._prime_contrast)

        return self.create_stim_attributes(arrow)
    

    def make_mask_back(self, direction, vertical_position):
        """
        Create a mask stimulus to be used as the mask background using Vorberg et al. (2003) settings.

        Parameters:
        - direction (str): The direction of the arrow. Can be either 'right' or 'left'.
        - vertical_position (str): The vertical position of the arrow. Can be 'top', 'bottom', or 'center'.

        Returns:
        - mask_back (visual.ShapeStim): The visual mask stimulus.

        """

        # mask background length and height in degrees (from vorberg et al. 2003)
        length = 3.47
        height = 1.09

        # transform size into edges so the stimuli is centered
        arrow_tip_short_side = (length/2 + 1.19)/2
        leftmost_point = -(length/2 + 1.19)/2
        rightmost_point = arrow_tip_short_side + (1.19/2)
        top_point = height / 2
        bottom_point = -height / 2
        

        # define the vertices of the mask
        vertices = [
            (2.0325, 0),               # tip (moved left by 0.025)
            (1.4875, -0.545),          # bottom-right (moved left)
            (-1.4375, -0.545),         # bottom-left (moved right)
            (-1.4375, 0.545),          # top-left (moved right)
            (1.4875, 0.545)            # top-right (moved left)
        ]

        # check length and height
        # find leftmost and rightmost point in vertices
        leftmost_point = min([x[0] for x in vertices])
        rightmost_point = max([x[0] for x in vertices])
        vertices_length = rightmost_point - leftmost_point
        f'vertices length is {vertices_length} and height is {length}'
        # find topmost and bottommost point in vertices
        top_point = max([x[1] for x in vertices])
        bottom_point = min([x[1] for x in vertices])
        vertices_height = top_point - bottom_point
        f'vertices height is {vertices_height} and height is {height}'

        # adapt the arrow vertices to the direction
        if direction == 'right':
            oriented_vertices = vertices
        elif direction == 'left':
            oriented_vertices = [(x * -1, y) for x, y in vertices]

        # mask position in degrees. the y coordinate indicates the position between
        # the center of the screen and the center of the mask
        if vertical_position == 'top':
            mask_coords = (0, 1.38)
        elif vertical_position == 'bottom':
            mask_coords = (0, -1.38)
        elif vertical_position == 'center':
            mask_coords = (0, 0)

        # Draw mask
        mask_back = visual.ShapeStim(self.win, name='mask_back', vertices=oriented_vertices, pos=mask_coords, 
                                     lineColor='black', fillColor='black', size=1, units='deg',
                                     opacity=self._mask_contrast, interpolate=False)

        return self.create_stim_attributes(mask_back)

    def make_mask_front(self, vertical_position):
        """
        Create a mask stimulus to be used as the mask foreground using Vorberg et al. (2003) settings.

        Parameters:
        - direction (str): The direction of the arrow. Can be either 'right' or 'left'.
        - vertical_position (str): The vertical position of the arrow. Can be 'top', 'bottom', or 'center'.

        Returns:
        - mask_front (visual.ShapeStim): The visual mask stimulus.

        """

        # define the vertices of the mask        
        vertices = [
            
            # X,            Y
            ( .94,         0),                     # 1. center-right tip
            (0.73,          -.1975),                # 2.
            (0.94,          -.4),                 # 3. bottom-right half-tip
            (-.93,          -.395),                 # 4. bottom-left half-tip
            (-0.732,        -0.202),                # 5. bottom inner diagonal
            (-0.928,        0),                     # 6. center-left tip
            (-0.728,        0.198),                 # 7. top inner diagonal
            (-.93,          .395),                  # 8. 
            (.93,           .395),                  # 9. top-right half-tip
            (.73,           .1975)                  # 10.

        ]

        # mask position in degrees. the y coordinate indicates the position between
        # the center of the screen and the center of the mask
        if vertical_position == 'top':
            mask_coords = (0, 1.38)
        elif vertical_position == 'bottom':
            mask_coords = (0, -1.38)
        elif vertical_position == 'center':
            mask_coords = (0, 0)

        # Draw mask
        mask_front = visual.ShapeStim(self.win, name='mask_fore', vertices=vertices, pos=mask_coords, 
                                      lineColor='white', fillColor='white', size=1, units='deg',
                                     opacity=self._mask_contrast, interpolate=False)

        return self.create_stim_attributes(mask_front)
        
    def present_stimuli(self, task, prime_direction, mask_direction, position, soa, prime_presence='present'):
        """
        Run a mask or prime trial.
        Parameters:
        task (str): 'prime': prime discrimination trial. 'mask': mask discrimination followed by prime detection.
        prime_direction (str): The direction of the prime stimulus ('left' or 'right').
        mask_direction (str): The direction of the mask stimulus ('left' or 'right').
        position (tuple): Coodinates indicating the position of the stimuli (both prime and mask) on the screen.
        soa (float): The stimulus onset asynchrony (SOA) in seconds. The time will be converted to frames and it is assumed that the time in seconds can be achieved in frames. If it is not possible the time in frames is rounded using int().
        prime_presence (str): Whether the prime is present or not. Defaults to True.
        Returns:
        None
        """
        
        # Reset trial variables. There are different variables used for each task.
        # To prevent carry values from the previous trials they are reset here.
        self._this_trial_mask_answer = None
        self._this_trial_mask_rt = None
        self._this_trial_mask_accuracy = None
        self._this_trial_mask_correct_response = None
        self._this_trial_prime_answer = None
        self._this_trial_prime_rt = None
        self._this_trial_prime_accuracy = None
        self._this_trial_prime_correct_response = None

        # keys
        prime_discrimination_keys = {'a':'left', 'l':'right', None:None}
        prime_detection_keys = {'a':'absent', 'l':'present', None:None}
        mask_discrimination_keys = {'a':'left', 'l':'right', None:None}

        # trial types
        ## mask: mask discrimination and prime detection
        ## prime: prime discrimination
        
        # get task
        self._this_trial_task = task
        # get correct response
        if self._this_trial_task == 'prime':
            self._this_trial_prime_correct_response = prime_direction
        elif self._this_trial_task == 'mask':
            self._this_trial_mask_correct_response = mask_direction
            self._this_trial_prime_correct_response = prime_presence

        # make stimuli
        prime = self.make_prime(prime_direction, position)
        mask_back = self.make_mask_back(mask_direction, position)
        mask_fore = self.make_mask_front(position)

        # set prime presence
        if prime_presence == 'absent':
            prime.opacity = 0

        # make fixation
        fixation = self.make_fixation()
        fixation.setAutoDraw(True)
        fixation_gray = self.make_fixation()
        fixation_gray.setColor('lightgray')

        # transform soa into frames
        self._soa_f = int(soa * self._frame_rate)

        # Pre create some vars
        look_for_mask_response = False
        self.kb.clearEvents(eventType='keyboard')

        # trial duration clock
        trial_clock = core.Clock()
        self._win.callOnFlip(trial_clock.reset)
        trial_start_time = {'time': None}
        self._win.timeOnFlip(trial_start_time, 'time')

        # Run trial
        frame_number = -1
        continue_routine = True
        self.trial_aborted = False
        while continue_routine:
            # Get frame time
            t = core.getTime()
            
            # Capture mask response if mask trial ----------------
            
            # Look for keys
            if look_for_mask_response:
                
                # Abort trial if 700 ms after mask onset
                if frame_number >= (mask_back.frame_start + int(self._frame_rate * .7)):
                    # Draw fixation off
                    fixation_gray.setAutoDraw(False)
                    # End routine
                    continue_routine = False
                    self.trial_aborted = True
                else:
                    # search for keys
                    keys_mask = self.getKeys(keyList=['a', 'l'])
                    if len(keys_mask) and continue_routine:
                        # Draw fixation off
                        fixation_gray.setAutoDraw(False)
                        fixation.setAutoDraw(True)
                        # Draw mask off if it hasn't been drawn off
                        if mask_back.status == STARTED:
                            mask_back = self.draw_stim_off(mask_back, t, frame_number)
                            mask_fore = self.draw_stim_off(mask_fore, t, frame_number)
                        # End routine
                        continue_routine = False

            # Prime ----------------------------------------------
            
            if frame_number == self._fixation_duration_f:
                prime = self.draw_stim_on(prime, t, frame_number)
            elif frame_number == self._fixation_duration_f + self._prime_duration_f:
                prime = self.draw_stim_off(prime, t, frame_number)

            # Mask -----------------------------------------------

            if frame_number == self._fixation_duration_f + self._soa_f:
                # Draw mask on
                mask_back = self.draw_stim_on(mask_back, t, frame_number)
                mask_fore = self.draw_stim_on(mask_fore, t, frame_number)
                if self._this_trial_task == 'mask':
                    # Change fixation color to red to indicate response time
                    fixation.setAutoDraw(False)
                    fixation_gray.setAutoDraw(True)
                    # reset keyboard time and start looking for mask responses on the next frame
                    look_for_mask_response = True
                    self._win.callOnFlip(self.kb.clock.reset)
                    self._win.callOnFlip(self.kb.clearEvents, eventType='keyboard')
            elif frame_number == self._fixation_duration_f +  self._soa_f + self._mask_duration_f:
                # Draw mask off
                mask_back = self.draw_stim_off(mask_back, t, frame_number)
                mask_fore = self.draw_stim_off(mask_fore, t, frame_number)
                if task == 'prime':
                    continue_routine = False
            
            # Flip screen
            self._flip_it()
            frame_number += 1

            # check for escape and abort experiment
            if 'escape' in event.getKeys():
                self._win.close()
                core.quit()


        # Mask aborted trials ----------------
        
        if self.trial_aborted:
            # show message indicating that responses should be faster
            self.show_message(text='Too slow!\nPress A or L to continue to the next trial.', color='red', pos=[0,0], height=self._default_text_height, wait_keypress=['a', 'l'])
            self.exp_handler.addData('trial_aborted', self.trial_aborted)
            # set empty vars
            self._this_trial_mask_answer = None
            self._this_trial_mask_rt = None
            self._this_trial_mask_accuracy = None
        else:
            # Prime detection response (on mask trials) ----------------------------

            if self._this_trial_task == 'mask':
                # mask responses and rt only in mask trials
                self._this_trial_mask_answer = keys_mask[0].name
                self._this_trial_mask_rt = keys_mask[0].rt    
                self._this_trial_mask_accuracy = self._this_trial_mask_correct_response == mask_discrimination_keys[self._this_trial_mask_answer]

                # mask response feedback
                self.display_feedback()

                # wait before prompting to detect prime
                core.wait(.25)

                # Prompt for prime detection
                prime_detection_prompt = self.show_message(text='Was the prime present?\n\n  ABSENT               PRESENT', 
                                height=self._default_text_height, wrapWidth=25, pos=[0, 0.8], 
                                color='black', return_text=True)
                prime_detection_prompt.setAutoDraw(True)
                # Change fixation color to lightgray to indicate response time
                fixation.setAutoDraw(False)
                fixation_gray.setAutoDraw(True)
                # Reset keyboard on prompt
                self._win.callOnFlip(self.kb.clock.reset)
                self._win.callOnFlip(self.kb.clearEvents, eventType='keyboard')
                while self.kb.getKeys():
                    self.kb.clearEvents()
                # Update screen
                self._flip_it()
                # Get response        
                keys_prime = self.waitKeys(keyList=['a', 'l'], clear=True)
                # Draw prompt off
                prime_detection_prompt.setAutoDraw(False)
                self._flip_it()
                # draw fixation back to black
                fixation_gray.setAutoDraw(False)
            
            # Prime discrimination (on prime trials) ----------------------------

            elif self._this_trial_task == 'prime':
                # wait before prompting to discrimination prime
                core.wait(.6)

                # Change fixation color to red to indicate response time
                fixation.setAutoDraw(False)
                fixation_gray.setAutoDraw(True)

                # Reset keyboard on prompt
                self._win.callOnFlip(self.kb.clock.reset)
                self._win.callOnFlip(self.kb.clearEvents, eventType='keyboard')
                # Update screen
                self._flip_it()

                # Get response        
                keys_prime = self.waitKeys(keyList=['a', 'l'])
                # draw fixation off 
                fixation_gray.setAutoDraw(False)

            # Get responses --------------------------------------------------

            # prime response and rt in all trials
            self._this_trial_prime_answer = keys_prime[0].name
            self._this_trial_prime_rt = keys_prime[0].rt

            # Label response as correct or incorrect
            if self._this_trial_task == 'mask':
                
                # Check prime response in mask trial
                if self._this_trial_prime_correct_response == prime_detection_keys[self._this_trial_prime_answer]:
                    self._this_trial_prime_accuracy = True
                else:
                    self._this_trial_prime_accuracy = False
            elif self._this_trial_task == 'prime':
                # Check prime response in prime trial
                if self._this_trial_prime_correct_response == prime_discrimination_keys[self._this_trial_prime_answer]:
                    self._this_trial_prime_accuracy = True
                else:
                    self._this_trial_prime_accuracy = False

        # Log trial data ----------------
        
        self.exp_handler.addData('congruent', prime_direction == mask_direction)
        self.exp_handler.addData('trial_type', 'decision')
        self.exp_handler.addData('block_type', self._block_type)
        self.exp_handler.addData('task', self._this_trial_task)
        self.exp_handler.addData('prime_presence', prime_presence)
        self.exp_handler.addData('prime_direction', prime_direction)
        self.exp_handler.addData('mask_direction', mask_direction)
        self.exp_handler.addData('stim_position', position)
        self.exp_handler.addData('soa', soa)
        # log mask trial data. in prime trials, mask data is logged as None
        self.exp_handler.addData('mask_answer', mask_discrimination_keys[self._this_trial_mask_answer])
        self.exp_handler.addData('mask_answer_key', self._this_trial_mask_answer)    
        self.exp_handler.addData('mask_accuracy', self._this_trial_mask_accuracy)
        self.exp_handler.addData('mask_rt', self._this_trial_mask_rt)
        # log prime trial data
        if self._this_trial_task == 'mask':
            self.exp_handler.addData('prime_answer', prime_detection_keys[self._this_trial_prime_answer])
        else:
            self.exp_handler.addData('prime_answer', prime_discrimination_keys[self._this_trial_prime_answer])
        self.exp_handler.addData('prime_answer_key', self._this_trial_prime_answer)
        self.exp_handler.addData('prime_accuracy', self._this_trial_prime_accuracy)
        self.exp_handler.addData('prime_rt', self._this_trial_prime_rt)
        self.exp_handler.addData('exp_time', exp_clock.getTime())
        self.exp_handler.addData('trial_dur', trial_clock.getTime())
        self.exp_handler.addData('trial_aborted', self.trial_aborted)
        self.exp_handler.addData('trial_start', trial_start_time['time'])
        

        # Log stim timing ----------------
        self.log_stim_time(prime)
        self.log_stim_time(mask_back)
        self.log_stim_time(mask_fore)

        if self._print_answer_info:
            print('     direction: ', prime_direction)
            print('     prime-response:', self._this_trial_prime_answer)
            print('     prime-accuracy:', self._this_trial_prime_accuracy)
            print('     prime-rt (s):', round(self._this_trial_prime_rt, 4))
            print('     mask-response:', self._this_trial_mask_answer)
            print('     mask-accuracy:', self._this_trial_mask_accuracy)
            print('     mask-rt (s):', round(self._this_trial_mask_rt, 4) if self._this_trial_mask_rt else None)
    
    @staticmethod
    def create_stim_attributes(stim):
        """
        Create attributes for logging the start and stop times of a stimulus.

        Args:
            stim: The stimulus object for which to create the attributes.
            
        Returns:
            The stimulus object with the newly created attributes.
        """

        # for timing logging and status tracking
        stim.frame_start = None
        stim.frame_stop = None
        stim.time_start = None
        stim.time_stop = None
        stim.refresh_start = None
        stim.refresh_stop = None
        stim.status = NOT_STARTED

        return stim
    
    def log_stim_time(self, stim):
        """
        Logs the timing information of a stimulus.

        Parameters:
            stim (object): The stimulus object.

        Returns:
            None
        """
        
        # Stim name
        stim_name = stim.name
        
        # Time in seconds
        self.exp_handler.addData('{}_time_start'.format(stim_name), stim.time_start)
        self.exp_handler.addData('{}_time_stop'.format(stim_name), stim.time_stop)
        # Time in refresh
        self.exp_handler.addData('{}_time_start_refresh'.format(stim_name), stim.refresh_start)
        self.exp_handler.addData('{}_time_stop_refresh'.format(stim_name), stim.refresh_stop)
        # Frames
        self.exp_handler.addData('{}_frame_start'.format(stim_name), stim.frame_start)
        self.exp_handler.addData('{}_frame_stop'.format(stim_name), stim.frame_stop)
        # Duration in seconds
        stim_duration = None if None in [stim.time_stop, stim.time_start] else stim.time_stop - stim.time_start
        self.exp_handler.addData('{}_time_duration'.format(stim.name), stim_duration)
        # Duration in refresh
        stim_duration_refresh = None if None in [stim.refresh_stop, stim.refresh_start] else \
            stim.refresh_stop - stim.refresh_start
        self.exp_handler.addData('{}_time_refresh_dur'.format(stim.name), stim_duration_refresh)
        # Duration in frames
        stim_duration_frames = None if None in [stim.frame_stop, stim.frame_start] else \
            stim.frame_stop - stim.frame_start
        self.exp_handler.addData('{}_frame_duration'.format(stim.name), stim_duration_frames)
    
    def draw_stim_on(self, stim, t, f, log=True):
        """
        Draw a stimulus on the window and log its start time.

        Parameters:
            stim (object): The stimulus object to draw.
            t (float): The current time.
            f (int): The current frame.
            log (bool, optional): Whether to log the start time of the stimulus. Defaults to True.

        Returns:
            object: The stimulus object that was drawn.
        """
        
        # Draw the stimulus
        stim.setAutoDraw(True)
        stim.status = STARTED

        # Log the start time
        if log:
            stim.time_start = t
            stim.frame_start = f
            self._win.timeOnFlip(stim, 'refresh_start')

        return stim
    
    def draw_stim_off(self, stim, t, f, log=True):
        """
        Turn off a stimulus and log its stop time.

        Parameters:
            stim (object): The stimulus object to turn off.
            t (float): The current time.
            f (int): The current frame.
            log (bool, optional): Whether to log the stop time of the stimulus. Defaults to True.

        Returns:
            object: The stimulus object that was turned off.
        """
        
        # Turn off the stimulus
        stim.setAutoDraw(False)
        stim.status = STOPPED

        # Log the stop time
        if log:
            stim.time_stop = t
            stim.frame_stop = f
            self._win.timeOnFlip(stim, 'refresh_stop')

        return stim
    
    def create_block_trials_list(self):
        """
        Creates a list of block trials based on the given number of repetitions of unique trials.

        Args:
            repeat_unique_trials (int): The number of times to repeat the unique trials.
            return_list (bool, optional): Whether to return the block trial list. Defaults to False.

        Returns:
            None if return_list is False, otherwise a list of block trials.

        """
        # create list of dictionaries with all possible combinations
        # of prime and mask directions and stimulus positions
        unique_trials = self.get_unique_trials()

        # prime unique trials are the same as unique trials
        self._prime_task_unique_trials = unique_trials

        # mask unique trials are the same as unique trials but removing 
        # the prime direction field and adding prime absence trials
        
        # filter entries with prime_direction left
        mask_unique_trials = [{**x, 'prime_presence': 'present'} for x in unique_trials]
        # get all entries with prime_direction left and add prime_presence absent
        mask_unique_trials += [{**x, 'prime_presence': 'absent'} for x in unique_trials if x['prime_direction'] == 'left']
        # save unique trials
        self._mask_task_unique_trials = mask_unique_trials

        # create block trial list
        self._block_trials_mask = self._mask_task_unique_trials * 2
        self._block_trials_prime = self._prime_task_unique_trials * 3

    def get_unique_trials(self):

        ut = [{'prime_direction': x, 'mask_direction': y, 'stim_position': z, 'SOA': a, 'congruent': x == y} \
                            for x in self._prime_directions 
                            for y in self._mask_directions 
                            for z in self._stim_positions
                            for a in self._possible_SOAs_s]
        
        return ut

    def update_performance(self):
        """
        Update the performance counters for the current block.

        Returns:
            None
        """
        # Check if prime or mask trial and update the trial and correct counter
        if self._this_trial_task == 'prime':
            self._prime_correct_count += self._this_trial_prime_accuracy
            self._prime_trial_count += 1
        elif self._this_trial_task == 'mask':
            self._mask_correct_count += self._this_trial_mask_accuracy
            self._mask_trial_count += 1
            self._mask_rt.append(self.exp_handler.getAllEntries()[-1]['mask_rt'])
            self._prime_correct_count += self._this_trial_prime_accuracy
            self._prime_trial_count += 1

    def run_block(self, trials=None, task=None):
        """
        Run a block of trials.

        Returns:
            None
        """     
        
        # Task is input or taken from block task
        if task is None:
            if self._block_task is None:
                raise ValueError('task is not input and block task is not set. Please input task or set block task.')
            else:
                task = self._block_task
        # Trials list is input or taken from block trial list
        if trials is None:
            if task == 'prime':
                trials = copy.deepcopy(self._block_trials_prime)
            elif task == 'mask':
                trials = copy.deepcopy(self._block_trials_mask)
        elif isinstance(trials, int):
            if task == 'prime':
                trials = list(np.random.choice(self._block_trials_prime, trials, replace=False))
            elif task == 'mask':
                trials = list(np.random.choice(self._block_trials_mask, trials, replace=False))
        
        # set block status
        self._block_running = True

        # within block trial counter
        self._block_trial_count = -1

        # count trials in block
        self.trials_in_block = len(trials)

        # within block performance
        show_performance = True
        # run trials
        counter = -1
        while counter < self.trials_in_block-1:
            counter+=1

            # Update trial count
            self._trial_count += 1
            self._valid_trial_count += 1
            self._block_trial_count += 1

            # Log trial information
            self.exp_handler.addData('block_count', self._block_count)
            self.exp_handler.addData('trial_count', self._trial_count)
            self.exp_handler.addData('block_trial', self._block_trial_count)
            self.exp_handler.addData('feedback', self._trial_feedback)

            # Get trial info
            trial = trials.pop()

            # Parse trial information
            prime_direction = trial['prime_direction']
            mask_direction = trial['mask_direction']
            stim_position = trial['stim_position']
            soa = trial['SOA']
            if task == 'mask':
                prime_presence = trial['prime_presence']
            else:
                prime_presence = 'present'
            
            # Print trial information
            time_left = 90-round(exp_clock.getTime()/60)
            progress_text =  f'{time_left} min. --- Block ({self._block_count}): {task} - Trial {self._trial_count}'
            print(progress_text)
            self.print_progress(progress_text)

            # present stimuli 
            self.present_stimuli(task, prime_direction, mask_direction, stim_position, soa, prime_presence)
            
            # if trial is aborted, append current trial 
            # setting to trial list and shuffle list
            if self.trial_aborted:
                self._block_trial_count -= 1
                self._valid_trial_count -= 1
                counter-=1
                trials.append(trial)
                np.random.shuffle(trials)
            else:
                # feedback
                self.play_feedback()
                # update trial and correct counter
                self.update_performance()

            # End trial
            self.exp_handler.nextEntry()

            # Show performance
            if counter == int(self.trials_in_block/2)-1:
                if show_performance:
                    show_performance = False
                    self.show_performance()
                    # reset counters
                    self._prime_correct_count = 0 
                    self._prime_trial_count = 0
                    self._mask_correct_count = 0
                    self._mask_trial_count = 0
                    self._mask_rt = []

        # add wait to make transition more fluid
        core.wait(.3)

        # set block to end
        self._block_running = False

    def show_performance(self, experiment_progress=None, block_trials_number=None, have_break=None):
        # Vorberg: Trials were grouped in blocks of 50–72 trials, and summary feedback 
        # (mean RT, percentage correct) was given after each block
        """
        Show the performance of the last block.

        Parameters:
            correct_responses (int, optional): The number of correct responses. Defaults to None.
            experiment_progress (float, optional): The progress of the experiment. Defaults to None.
            block_trials_number (int, optional): The number of trials in the block. Defaults to None.

        Returns:
            None
        """

        # Print progress
        self.print_progress(f'Block number: {self._block_count} - Performance')

        # Check if experiment progress needs to be added
        if experiment_progress is not False:
            # Check if experiment_progress is provided, otherwise use the current value
            if experiment_progress is None:
                experiment_progress = round(self._valid_trial_count / self._total_trials * 100)
            # Make experiment progress text
            experiment_progress_text = f'You have completed {experiment_progress}% of the experiment.\n'
        else:
            experiment_progress_text = ''
        
        # Check if block_trials_number is provided, otherwise use the current value
        if block_trials_number is None:
            block_trials_number = self._block_trial_count+1

        # Check if a break is needed. Only in experiment blocks
        if self._block_type == 'experiment':
            if self._block_count != self._last_block:
                # Check if block number is multiple of forced break recurrence
                if not (self._block_count + 1) % self._forced_break_recurrence and self._block_running == False:
                    have_break = 'forced'   
                else:
                    have_break = 'short' 
            else:
                have_break = 'end'

        # Mask block performance: mask discrimination and prime detection
        if self._block_type == 'experiment' and self._block_task == 'mask':
            
            # check RT ------------------------------
            last_block_rt = np.mean(self._mask_rt)
            # Print progress
            self.print_progress(f'-- average rt is {round(last_block_rt, 4)}')
            # get percentage correct
            last_block_mask_percentage_correct = self._mask_correct_count / self._mask_trial_count if all([self._mask_correct_count, self._mask_trial_count]) else 0
            self.print_progress(f'-- average MASK percentage correct is {round(last_block_mask_percentage_correct, 4)}')
            last_block_prime_percentage_correct = self._prime_correct_count / self._prime_trial_count if all([self._prime_correct_count, self._prime_trial_count]) else 0
            self.print_progress(f'-- average PRIME percentage correct is {round(last_block_prime_percentage_correct, 4)}')
            
            # Check speed and accuracy
            if last_block_rt > .4:
                rt_slow_message='TRY TO BE FASTER WHEN YOU INDICATE THE DIRECTION OF THE MASK.\n\n'
            else:
                rt_slow_message='\n'

            if last_block_mask_percentage_correct < .9:
                performance_low_message='TRY TO BE MORE ACCURATE WHEN YOU INDICATE THE DIRECTION OF THE MASK.\n\n'
            else:
                performance_low_message=''
    
            # Make performance text
            performance_text = f'{experiment_progress_text}This was your performance in the last {self._mask_trial_count} trials.\n\n' \
                                f'Mask performance: {int(last_block_mask_percentage_correct*100)}% correct.\n{performance_low_message}' \
                                f'Mask response speed: {round(last_block_rt, 2)} seconds.\n{rt_slow_message}' \
                                f'Prime performance: {int(last_block_prime_percentage_correct*100)}% correct.'
        
        # Prime block: prime discrimination
        elif self._block_type == 'experiment' and self._block_task == 'prime':
            
            # get last block data
            last_block_data = self.exp_handler.getAllEntries()
            last_block_data = last_block_data[-self._prime_trial_count:]
            
            # get percentage correct
            last_block_prime_percentage_correct = self._prime_correct_count / self._prime_trial_count if all([self._prime_correct_count, self._prime_trial_count]) else 0
            self.print_progress(f'-- average PRIME percentage correct is {round(last_block_prime_percentage_correct, 4)}')
            
            # Make performance text
            performance_text = f'{experiment_progress_text}This was your performance in the last {self._prime_trial_count} trials.\n\n' \
                                f'Prime performance: {int(last_block_prime_percentage_correct*100)}% correct.'
            
        # Performance clock
        performance_clock = core.Clock()
        self._win.callOnFlip(performance_clock.reset)
        
        # Add break text
        if have_break == 'forced':
            # add break text
            performance_text += f'\n\nYou have to take a 3-minute break. A message on the screen will indicate when the break is over.'
            # Print performance to console
            self.print_progress(performance_text)
            # get message
            perf_mssg = self.show_message(text=performance_text, color='black', 
                                          height=self._default_text_height*.8, wrapWidth=15, return_text=True)
            # draw message and wait
            perf_mssg.setAutoDraw(True)
            self._flip_it()
            core.wait(self._forced_break_duration)
            # draw message off
            perf_mssg.setAutoDraw(False) 
            self._flip_it()
            # screen before starting new block
            self.show_message(text='Press A or L when you are ready to continue.', 
                              wait_keypress=['a', 'l'], color='black', height=self._default_text_height*.8, wrapWidth=15, block_keypress=.5)                
            
        else:
            # indicate a short break is possible
            if have_break == 'short':
                performance_text += f'\n\nYou may take a short break now, otherwise press A or L when you are ready to continue.'
            # the experiment is ending, no neeed for beak            
            elif have_break == 'end':
                performance_text += f'\n\nPress A or L to end the experiment.'
            # Print performance to console
            self.print_progress(performance_text)
            # Show performance
            self.show_message(text=performance_text, wait_keypress=['a', 'l'], color='black', height=self._default_text_height*.8, wrapWidth=15, block_keypress=.5)                
       
        # Log performance
        self.exp_handler.addData('block_type', self._block_type)
        self.exp_handler.addData('trial_type', 'performance')
        self.exp_handler.addData('block_count', self._block_count)
        self.exp_handler.addData('block_trials', block_trials_number)
        self.exp_handler.addData('trial_dur', performance_clock.getTime())
        self.exp_handler.addData('break', have_break)
        self.exp_handler.addData('trials_feedback', max([self._mask_trial_count,self._prime_trial_count]))
        self.exp_handler.addData('mask_correct_trials', self._mask_correct_count)
        self.exp_handler.addData('prime_correct_trials', self._prime_correct_count)

        self.exp_handler.nextEntry()

        # Reset performance counter
        self._prime_correct_count = 0
        self._prime_trial_count = 0
        self._mask_correct_count = 0
        self._mask_trial_count = 0
        self._mask_rt = []

    def setup_total_trials(self):
        """
        Set the total number of trials and the last block index.

        Returns:
            None
        """
        # Check if block trials is set
        if self._block_trials_mask is None:
            raise ValueError('Block trials MASK is not set. Please create block trials list.')
        if self._block_trials_prime is None:
            raise ValueError('Block trials PRIME is not set. Please create block trials list.')

        # Set total number of trials
        self._total_trials = len(self._block_trials_prime) * self._blocks_to_run + len(self._block_trials_mask) * self._blocks_to_run
        
        # Set last block index
        self._last_block = self._blocks_to_run * 2 - 1

    def _set_default_timing(self):
        # Timing default values. The values of the original paper can't
        # be achieved with the screen we have, so we will use the closest
        # prime duration possible and make the mask duration ten times that
        # value (as in Vorberg's paper)
        self._fixation_duration_s = (1/60) * 42
        self._default_prime_duration_s = (1/80)
        self._default_mask_duration_s = self._default_prime_duration_s * 10
        # In vorberg's paper the SOA values are 14, 28, 42, 56, 70 and 84.
        # These are values are the duration of the prime multiplied by 
        # 1, 2, 3, 4, 5 and 6. We can't achieve these durations with the 
        # screen we have so we will use the target duration we have and 
        # multiply that by the same values (except 6, to keep the experiment
        # short).
        self._possible_SOAs_s = [round(self._default_prime_duration_s * x, 4) for x in range(1, 7)] 

    def _set_debugging_time(self):
        # Set frame rate to 60 because most monitors can handle it
        self._frame_rate = 60
        # Update stimuli timing manually to values achievable by the frame rate
        self._fixation_duration_s = (1/60) * 42
        self._default_prime_duration_s = (1/60)
        self._default_mask_duration_s = self._default_prime_duration_s * 10
        self._possible_SOAs_s = [round(self._default_prime_duration_s * x, 4) for x in range(1, 7)]

    def _get_n_trials(self, n):
        # Get n trials from the block trials list
        return self._block_trials[:n]
    
    def experiment_welcome(self):
        
        # ---
        self.show_message(text='Welcome, and thank you for participating in this study. Before starting the experiment, you will receive instructions and some practice blocks. Press SPACE to continue.', 
                    color='black', height=self._default_text_height * .9, wait_keypress=['space'])

        # ---
        fixation = self.make_fixation()
        fixation.setAutoDraw(True)
        self.show_message(text='You can see a black cross in the center of the screen. This is a fixation cross. You must always look at the fixation cross during the experiment. Otherwise, we will not be able to use your data. Press SPACE to continue.', 
                    color='black', height=self._default_text_height * .9, wait_keypress=['space'], pos=(0, 4))
        fixation.setAutoDraw(False)
        self._flip_it()
    
    def _set_log_instructions(self):

        # Enable instructions log. show_message will dump the text to the log file
        self._log_instructions = True
        self._log_file_name = f'data/{self._experiment_info["participant"]}_{self._experiment_info["session"]}_instructions.log'
        
        # Try to open log file, if error raise warning
        try:
            with open(self._log_file_name, "w") as file:
                file.write("Instructions log\n\n")
        except Exception as e:
                warnings.warn(f'Failed to write to log file: {e}')

    def prepare_new_block(self):
        # Reset block and performance counters
        self.reset_block()

    @staticmethod
    def get_experiment_info():
        print('Getting experiment info')
        
        # Try to get cubicle
        if os.getenv('computername') is None:
            cubicle_id = 'unknown'
        else:
            cubicle_id = os.getenv('computername')

        # Set default settings
        experiment_info = {
            # may change on every run
            'participant': '',
            'blocks_to_run': 6,
            'frame_rate': 240,
            'cubicle': cubicle_id,
            'something_else': False}
        
        # Create dialog
        dialog = gui.DlgFromDict(title='Experiment Runner', dictionary=experiment_info, fixed=['cubicle'], show=False)

        while True:
            # Prompt researcher
            dialog.show()

            # If escape or cancel is pressed stop experiment
            if not dialog.OK:
                print('User cancelled')
                core.quit()

            # Check for number
            try:
                for key, new_val in experiment_info.items():
                    if key in ['participant', 'frame_rate', 'blocks_to_run']:
                        experiment_info[key] = int(new_val)
                    else:
                        experiment_info[key] = new_val

            except ValueError:
                print(f"ERROR: '{key}' has to be a valid number! Not '{new_val}'.")
                continue

            # Further check
            try:
                # Check the conditions for each field in experiment_info
                assert experiment_info['participant'] >= 0, "'participant' should be a positive non-zero integer."
                assert experiment_info['frame_rate'] > 0, "'frame_rate' should be a positive non-zero integer."
                assert experiment_info['blocks_to_run'] > 0, "'blocks_to_run' should be a positive non-zero integer."

            except AssertionError as e:
                print('ERROR:', str(e))
                continue

            break
        
        # Check if different part of the experiment need to be run
        something_else = {'demographics': True, 
                          'prime_instructions': True,
                          'mask_instructions': True}
            
        if experiment_info['something_else']:
            diag = gui.DlgFromDict(something_else)

        # Add extra settings
        experiment_info.update(something_else)
        print('Experiment info:', experiment_info)
        return experiment_info

    def adjust_chinrest(self):
        # Make fixation
        fixation = self.make_fixation()
        fixation.setAutoDraw(True)
        # Show instructions
        self.show_message(text='Align the chin-rest with the fixation cross.\nPress SPACE to continue.',
                            color='black', height=self._default_text_height * .9, wait_keypress=['space'], pos=(0, 4))
        fixation.setAutoDraw(False)
        self._flip_it()

    def setup_progress_log(self):
        # path to progress log folder 
        self._progress_folder = os.path.join(self._this_dir, 'log')
        # if folder doesn't exist, create it
        if not os.path.isdir(self._progress_folder):
            os.mkdir(self._progress_folder)

        # create file
        self._log_file_name = os.path.join(self._progress_folder, f'{self._experiment_info["participant"]}_progress.log')

        # try to open file, if not throw a warning
        try:
            progress_log = open(self._log_file_name, 'w')
            progress_log.close()
        except Exception as e:
                warnings.warn(f'Failed to open progress log file: {e}')

    def print_progress(self, text):
        try:
            # compose log text
            new_line = f"{self._experiment_info['participant']} {self._experiment_info['cubicle']}: {text} "
            # open and read file
            progress_log = open(self._log_file_name)
            old_lines = progress_log.read()
            # open and write new and old lines
            progress_log = open(self._log_file_name, 'w')
            progress_log.writelines((f'{new_line}\n', old_lines))
            progress_log.flush()
            progress_log.close()
        except Exception as e:
                warnings.warn(f'Failed to write to progress log file: {e}')

    def mask_instructions(self):
        
        # Indicate that on each trial two arrows will appear in a brief sequence.
        # Show both arrows and indicate which is the first and second arrow.
        # Indicate that the task of the participant is first to indicate the direction of the second arrow as soon as it appears
        # After that, the participant has to indicate if the first arrow appeared or not.

        # --- Fixation cross --- 

        # Make fixation
        fixation = self.make_fixation()
        fixation.setAutoDraw(True)

        # Show instruction
        self.show_message(
            text='At the center of the screen you can see a black cross. This is a fixation cross. '
            'You must always look at the fixation cross during the experiment. Otherwise, we will '
            'not be able to use your data. Press SPACE to continue.', 
            color='black', 
            height=self._default_text_height * .9, 
            wait_keypress=['space'], 
            pos=(0, 4), 
            wrapWidth=25
        )

        core.wait(.1)

        # --- Instructions for the mask-prime task --- 
       
        # Make prime and mask examples
        prime = self.make_prime(direction='left', vertical_position='center')
        prime.setPos([-5, 0])
        mask_back = self.make_mask_back(direction='right', vertical_position='center')
        mask_front = self.make_mask_front(vertical_position='center')
        mask_back.setPos([5, 0])
        mask_front.setPos([5, 0])
        # Make labels for text
        first_arrow_label = self.show_message(
            text='prime',
            color='black', 
            height=self._default_text_height * .9, 
            pos=(-5, -1), 
            wrapWidth=25,
            return_text=True
        )
        second_arrow_label = self.show_message(
            text='mask',
            color='black', 
            height=self._default_text_height * .9, 
            pos=(5, -1), 
            wrapWidth=25,
            return_text=True
        )
        
        # Draw examples
        prime.setAutoDraw(True)
        mask_back.setAutoDraw(True)
        mask_front.setAutoDraw(True)
        first_arrow_label.setAutoDraw(True)
        second_arrow_label.setAutoDraw(True)

        # Show instruction
        self.show_message(
            text='Two arrows can be presented on each trial. We refer to the first arrow as the '
            'prime and the second as the mask. Below, you can see an example of the prime and '
            'the mask. In some trials, the prime will appear quickly before the mask, whereas '
            'in other trials only the mask will be presented. Press SPACE to continue.',
            color='black', 
            height=self._default_text_height * .9, 
            wait_keypress=['space'], 
            pos=(0, 4), 
            wrapWidth=25
        )

        # Draw examples off and flip screen
        prime.setAutoDraw(False)
        mask_back.setAutoDraw(False)
        mask_front.setAutoDraw(False)
        first_arrow_label.setAutoDraw(False)
        second_arrow_label.setAutoDraw(False)
        self._flip_it()

        core.wait(.1)

        # ------------------

        # Draw examples
        prime.setAutoDraw(True)
        mask_back.setAutoDraw(True)
        mask_front.setAutoDraw(True)
        first_arrow_label.setAutoDraw(True)
        second_arrow_label.setAutoDraw(True)
        fixation.setAutoDraw(True)

        # Show instruction
        self.show_message(text='You have two tasks on each trial. First, you have to indicate the '
        'direction of the mask as quickly as possible—press A to answer LEFT or L to '
        'answer RIGHT. Then, a message will prompt you to indicate whether the prime was present—press '
        'A to answer ABSENT or L to answer PRESENT. When indicating whether the prime was present, '
        'it is important to be accurate. Press A or L to continue.', 
        color='black', height=self._default_text_height * .9, wait_keypress=['a', 'l'], pos=(0, 4), wrapWidth=28)

        # Draw examples off and flip screen
        prime.setAutoDraw(False)
        mask_back.setAutoDraw(False)
        mask_front.setAutoDraw(False)
        first_arrow_label.setAutoDraw(False)
        second_arrow_label.setAutoDraw(False)
        fixation.setAutoDraw(False)
        self._flip_it()

        core.wait(.1)

                # ------------------

        # Draw examples
        fixation.setAutoDraw(True)

        # Show instruction
        self.show_message(text='You will receive feedback after your mask responses if you are wrong but '
        'you will not receive feedback on your prime responses. Press A or L to continue.', 
        color='black', height=self._default_text_height * .9, wait_keypress=['a', 'l'], pos=(0, 4), wrapWidth=18)

        # Draw examples off and flip screen
        fixation.setAutoDraw(False)
        self._flip_it()

        core.wait(.1)

    def prime_instructions(self):
        
        # Indicate that both arrows are presented in all trials
        # The task is now to indicate the direction of the first arrow

        # --- Instructions for the prime task --- 

        # Make prime and mask examples
        prime = self.make_prime(direction='left', vertical_position='center')
        prime.setPos([-5, 0])
        mask_back = self.make_mask_back(direction='right', vertical_position='center')
        mask_front = self.make_mask_front(vertical_position='center')
        mask_back.setPos([5, 0])
        mask_front.setPos([5, 0])
        # Make labels for text
        first_arrow_label = self.show_message(
            text='prime',
            color='black', 
            height=self._default_text_height * .9, 
            pos=(-5, -1), 
            wrapWidth=25,
            return_text=True
        )
        second_arrow_label = self.show_message(
            text='mask',
            color='black', 
            height=self._default_text_height * .9, 
            pos=(5, -1), 
            wrapWidth=25,
            return_text=True
        )
        
        # Make fixation
        fixation = self.make_fixation()

        # ------------------
        
        # Flash text to prevent people continuing too fast

        # Make and draw text
        half_done = self.show_message(text='You have completed the first part of the experiment. You will '
                          'perform a new task now. Pay attention to the new instructions. Press A or L '
                          'to continue.', 
                          color='black', height=self._default_text_height * .9, pos=(0, 0), 
                          wrapWidth=17, return_text=True)
        half_done.setAutoDraw(True)
        self._flip_it()

        # Loop to change text color between black and gray
        frame_n = -1
        while True:
            frame_n += 1
            # switch color once every second
            color = frame_n % self._frame_rate
            if color > self._frame_rate / 2:
                half_done.setColor('black')
            else:
                half_done.setColor('gray')
            
            # check for key press
            if frame_n:
                if frame_n / self._frame_rate > 1:
                    keys = self.getKeys(keyList=['a', 'l'])
                    if len(keys):
                        half_done.setAutoDraw(False)
                        self._flip_it()    
                        break
            # flip screen
            self._flip_it()
        
        # Draw examples off and flip screen
        self._flip_it()

        core.wait(.1)

        # ------------------

        # Draw examples
        prime.setAutoDraw(True)
        mask_back.setAutoDraw(True)
        mask_front.setAutoDraw(True)
        first_arrow_label.setAutoDraw(True)
        second_arrow_label.setAutoDraw(True)
        fixation.setAutoDraw(True)
        self._flip_it()

        # Show instruction
        self.show_message(text='In this part of the experiment, the prime and mask will appear '
                        'in every trial. The prime will point either to the left or to the right. '
                        'Your task is to indicate the direction of the prime —press A to answer left '
                        'or L to answer right.\nPress A or L to continue. ',
                    color='black', height=self._default_text_height * .9, wait_keypress=['a', 'l'], 
                    pos=(0, 4), wrapWidth=25)

        # draw everything off
        prime.setAutoDraw(False)
        mask_back.setAutoDraw(False)
        mask_front.setAutoDraw(False)
        first_arrow_label.setAutoDraw(False)
        second_arrow_label.setAutoDraw(False)
        fixation.setAutoDraw(False)
        self._flip_it()

        core.wait(.1)

        # ------------------

        # Draw examples
        fixation.setAutoDraw(True)
        # Set fixation to light grey
        fixation.setColor('lightgray')
        self._flip_it()

        # Show instruction
        self.show_message(text='The fixation cross will turn light gray to let you know you can '
                        'answer. Below, you see an example of the fixation cross that will prompt '
                        'you to answer. If you answer incorrectly you will hear a sound. You should '
                        'try to be accurate, not fast.\nPress A or L to continue. ',
                    color='black', height=self._default_text_height * .9, wait_keypress=['a', 'l'], 
                    pos=(0, 4), wrapWidth=25)

        # draw everything off
        fixation.setAutoDraw(False)
        self._flip_it()

        core.wait(.1)

if __name__ == '__main__':
    '''
    this is an example of how to run this experiment. 
    to run the experiment use the script main.py

    '''
    
    # Initialize experiment
    e = exp()
    e.start_exp_handler(exp_info={'frame_rate': 60, 'participant': 999, 'blocks_to_run': 6})
    
    # Apply frame rate to timing
    e._set_debugging_time()
    e.update_timing()
    
    # create trials
    e.create_block_trials_list()
    e.setup_total_trials()

    print('exp started...')

    # open window
    e.open_window(monitor='nostromo', full_screen=True, size=[2560, 1440])
     
    # demographics
    e.run_demographic_questions()
    
    # mask discrimination, prime detection instructions    
    e.mask_instructions()

    # Run the experiment
    e.show_message(text='Press A or L to start the experiment.',
                   color='black', height=e._default_text_height * .9, wait_keypress=['a', 'l'])

    # Prepare for new block. Increase block count and reset performance counters
    e._block_type = 'experiment'
    e._trial_count = -1    
    e.reset_block()
 
    # Mask block
    e._block_task = 'mask'   
    for b in range(6):
        if b > 0:
            e.prepare_new_block()
        # Run trials 
        e.run_block() 
        e.show_performance()
    
    # # Prime block
    e.prime_instructions()

    # Run the experiment
    e.show_message(text='Press A or L to start a new block.',
                   color='black', height=e._default_text_height * .9, 
                   wait_keypress=['a', 'l'])
    e._block_task = 'prime'
    e.reset_block()

    for b in range(6):
        # Remind task after first block
        if b != 0:
            e.prepare_new_block()
        # e._simulate_keys(True)    
        e.run_block()    
        e.show_performance()

    # The experiment is over
    e.print_progress(f'Experiment done!')
    e.show_message(text='The experiment is over. Thanks for participating! The researcher will be with you shortly.',
                   color='black', height=e._default_text_height * .9, wait_keypress=['space'])
    
    # Save data
    e.win.close()
    e.save_csv()
    e.exp_handler.abort()
    core.quit()