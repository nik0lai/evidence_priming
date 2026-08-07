"""
~~ priming experiment

this script contains the class that runs the experiment. another script (main.py) uses the class exp to run the experiment with the appropriate settings.


@ nicolás sánchez-fuenzalida
apr 22, 2024
"""
script_version = '0.0.0'

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
import csv
import copy

# Set psychopy version
psychopyVersion = '2023.1.3'
psychopy.useVersion(psychopyVersion)
print(f'Using psychopy version {psychopy.__version__}')

# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)
# Experiment name for logging
experiment_name = 'prime'
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
        self._session = None
        
        # for saving data
        self._filename = None

        # for trial tracking
        self._trial_count = None
        self._block_count = None
        self._block_trial_count = None
        self._total_trials = None
        self._block_type = None
        self._last_block = None
        self._blocks_to_run = None
        self._block_trials = None
        self._block_task = None

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
                         - 'session': The session number (default: '1').

        :return: None
        """

        if exp_info is None:
            # Default values for gui
            self._experiment_info = {'frame_rate': 60,
                                     'participant': '999',
                                     'session': '999',
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
            if not 'session' in self._experiment_info.keys():
                self._experiment_info['session'] = '999'
            if not 'blocks_to_run' in self._experiment_info.keys():
                self._experiment_info['blocks_to_run'] = '1'
            if not 'first_task' in self._experiment_info.keys():
                self._experiment_info['first_task'] = 'prime'

        # add script version to _experiment_info for automatic logging
        self._experiment_info['script_version'] = script_version

        # Check if 'frame_rate' and 'participant' keys exist in exp_info
        assert 'frame_rate' in self._experiment_info, "'frame_rate' key does not exist in exp_info."
        assert 'participant' in self._experiment_info, "'participant' key does not exist in exp_info."
        if not 'session' in self._experiment_info:
            warnings.warn('"session" parameter not found in exp_info, setting session to 1.')
            self._experiment_info['session'] = 1
        assert 'blocks_to_run' in self._experiment_info, "'blocks_to_run' key does not exist in exp_info."
        assert 'first_task' in self._experiment_info, "'first_task' key does not exist in exp_info."

        # Pad subject number
        self._experiment_info['participant'] = str(self._experiment_info['participant']).zfill(3)
        self._experiment_info['session'] = str(self._experiment_info['session']).zfill(2)

        # frame is used a lot throughout the experiment, so it's easier to set up a new var
        self._frame_rate = self._experiment_info['frame_rate']
        self._session = int(self._experiment_info['session'])
        self._blocks_to_run = self._experiment_info['blocks_to_run']
        
        # set tasks
        self._first_task = self._experiment_info['first_task']
        self._tasks = [self._first_task] + [x for x in self._tasks if x != self._first_task]
        
        # Add some info to be logged in every row
        self._experiment_info['date'] = data.getDateStr()  # add a simple timestamp
        self._experiment_info['expName'] = experiment_name
        self._experiment_info['psychopyVersion'] = psychopyVersion
        self._experiment_info['script_version'] = script_version


        # Data file name
        self._filename = 'data_%s_%s_%s_%s' % (
            self._experiment_info['participant'], 
            self._experiment_info['session'], 
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
        np.random.shuffle(self._block_trials)

    def _simulate_wait_keys(self, maxWait=None, keyList=None, modifiers=None, timeStamped=False):

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
        # set default values
        correct = correct if correct is not None else self._this_trial_accuracy
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
    
    def present_stimuli(self, task, prime_direction, mask_direction, position, soa):
        
        # get task
        self._this_trial_task = task
        # get correct response
        if self._this_trial_task == 'prime':
            self._this_trial_correct_response = prime_direction
        elif self._this_trial_task == 'mask':
            self._this_trial_correct_response = mask_direction

        # make stimuli
        prime = self.make_prime(prime_direction, position)
        mask_back = self.make_mask_back(mask_direction, position)
        mask_fore = self.make_mask_front(position)

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
                    fixation.setAutoDraw(True)
                    # End routine
                    continue_routine = False
                    self.trial_aborted = True
                else:
                    # search for keys
                    keys = self.getKeys(keyList=['a', 'l'])
                    if len(keys) and continue_routine:
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
                # Draw fixation and look for responses
                if self._this_trial_task == 'mask':
                    # Change fixation color to red to indicate response time
                    fixation.setAutoDraw(False)
                    fixation_gray.setAutoDraw(True)
                    # reset keyboard time and start looking for mask responses on the next frame
                    look_for_mask_response = True
                    self._win.callOnFlip(self.kb.clock.reset)
                    self._win.callOnFlip(self.kb.clearEvents, eventType='keyboard')
            elif frame_number == self._fixation_duration_f +  self._soa_f + self._mask_duration_f:
                # Draw mask 
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

        # Prime response ---------------

        if self._this_trial_task == 'prime':
            # Wait 600ms 
            core.wait(.6)

            # Change fixation color to red to indicate response time
            fixation.setAutoDraw(False)
            fixation_gray.setAutoDraw(True)
            # Reset keyboard time
            self.kb.clock.reset()
            self.kb.clearEvents
            # Update screen
            self._flip_it()

            # Get response        
            keys = self.waitKeys(keyList=['a', 'l'])
    
        # Draw fixation off --------------

        # If there is another trial the fixation won't disappear, just change color
        # if there is no other trial, the fixation will disappear.

        fixation.setAutoDraw(False)
        fixation_gray.setAutoDraw(False)

        # Mask aborted trials ----------------
        
        if self.trial_aborted:
            # show message indicating that responses should be faster
            self.show_message(text='Too slow!\nPress A or L to continue to the next trial.', color='red', pos=[0,0], height=self._default_text_height, wait_keypress=['a', 'l'])
            self.exp_handler.addData('trial_aborted', self.trial_aborted)
            # no answer or rt
            self._this_trial_answer = None
            self._this_trial_rt = None
        else:

            # Get performance ----------------

            # Get key pressed and RT
            self._this_trial_answer = keys[0].name
            self._this_trial_rt = keys[0].rt

            # Label response as correct or incorrect
            if self._this_trial_answer == 'a' and self._this_trial_correct_response == 'left':
                self._this_trial_accuracy = True
            elif self._this_trial_answer == 'l' and self._this_trial_correct_response == 'right':
                self._this_trial_accuracy = True
            else:
                self._this_trial_accuracy = False

        # Log trial data ----------------
        
        self.exp_handler.addData('congruent', prime_direction == mask_direction)
        self.exp_handler.addData('trial_type', 'decision')
        self.exp_handler.addData('block_type', self._block_type)
        self.exp_handler.addData('task', self._this_trial_task)
        self.exp_handler.addData('prime_direction', prime_direction)
        self.exp_handler.addData('mask_direction', mask_direction)
        self.exp_handler.addData('stim_position', position)
        self.exp_handler.addData('soa', soa)
        self.exp_handler.addData('answer', 'left' if self._this_trial_answer == 'a' else 'right')
        self.exp_handler.addData('answer_key', self._this_trial_answer)
        self.exp_handler.addData('accuracy', self._this_trial_accuracy)
        self.exp_handler.addData('rt', self._this_trial_rt)
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
            print('     response:', self._this_trial_answer)
            print('     accuracy:', self._this_trial_accuracy)
            print('     rt (s):', self._this_trial_rt)

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
    
    def create_block_trials_list(self, repeat_unique_trials, return_list=False):
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

        # create congruent field
        for trial in unique_trials:
            trial['congruent'] = trial['prime_direction'] == trial['mask_direction']
        
        # display unique trials
        # print(*unique_trials,sep='\n')

        # create block trial list
        if return_list:
            return unique_trials * repeat_unique_trials
        else:
            self._block_trials = unique_trials * repeat_unique_trials

    def get_unique_trials(self):

        ut = [{'prime_direction': x, 'mask_direction': y, 'stim_position': z, 'SOA': a} \
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
            self._prime_correct_count += self._this_trial_accuracy
            self._prime_trial_count += 1
        elif self._this_trial_task == 'mask':
            self._mask_correct_count += self._this_trial_accuracy
            self._mask_trial_count += 1
            self._mask_rt.append(self.exp_handler.getAllEntries()[-1]['rt'])

    def run_block(self, trials=None, task=None):
        """
        Run a block of trials.

        Returns:
            None
        """     
        
        # Trials list is input or taken from block trial list
        if trials is None:
            trials = copy.deepcopy(self._block_trials)
        elif isinstance(trials, int):
            trials = list(np.random.choice(self._block_trials, trials, replace=False))
            
        # Task is input or taken from block task
        if task is None:
            if self._block_task is None:
                raise ValueError('task is not input and block task is not set. Please input task or set block task.')
            else:
                task = self._block_task
        
        # within block trial counter
        self._block_trial_count = -1

        # count trials in block
        trials_in_block = len(trials)
        
        # run trials
        counter = -1
        while counter < trials_in_block-1:
            counter+=1
            
            # Update trial count
            self._valid_trial_count += 1
            self._trial_count += 1
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
            
            # Print trial information
            time_left = 60-round(exp_clock.getTime()/60)
            progress_text =  f'{time_left} min. --- Block: {task} (no. {self._block_count}) Trial {self._trial_count}'
            print(progress_text)
            self.print_progress(progress_text)
            
            # present stimuli 
            self.present_stimuli(task, prime_direction, mask_direction, stim_position, soa)
            
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

        # add wait to make transition more fluid
        core.wait(.3)

    def show_performance(self, correct_responses=None, experiment_progress=None, block_trials_number=None, have_break=None):
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
        
        # Check if correct_responses is provided, otherwise use the current value
        if correct_responses is None:
            if self._block_task == 'prime':
                correct_responses = self._prime_correct_count
                trials_count = self._prime_trial_count
            elif self._block_task == 'mask':
                correct_responses = self._mask_correct_count
                trials_count = self._mask_trial_count
                rt = self._mask_rt

        # Check if block_trials_number is provided, otherwise use the current value
        if block_trials_number is None:
            block_trials_number = self._block_trial_count+1

        # Check if a break is needed
        if have_break == 'short':
            break_text = '\nYou may take a short break now. Press A or L when you are ready to continue.\n'
        elif have_break == 'forced':
            break_text = '\nYou have to take a 3-minute break. A message on the screen will indicate when the break is over.\n'
        else:
            break_text = ''

        # Make performance text
        percentage_correct = (correct_responses/trials_count) * 100
        percentage_correct = int(np.round(percentage_correct))
        
        # pre-create empty text
        performance_low_message=''
        rt_slow_message='\n '
        # If mask block, check for rt and performance
        if self._block_task == 'mask':
            last_block_rt = np.mean(rt)
            
            if self._block_type == 'experiment':

                # check number of trials matches number of RTs
                assert len(rt) == trials_count, "number of rt values does not match number of trials"
                
                # check RT ------------------------------
                if last_block_rt > .4:
                    # rt_message = 'In the last block your average response time was too slow. Please try to respond faster when you have to indicate the direction of the SECOND arrow.\n\n'         
                    rt_slow_message = 'TRY TO BE FASTER WHEN YOU INDICATE THE DIRECTION OF THE SECOND ARROW\n\n'
                    # Print progress
                    self.print_progress(f'-- average rt too slow!')
                # check performance ----------------------
                if percentage_correct < 90:
                    # rt_message += 'In the last block you gave less than 90% correct responses. Please try to respond more accurately.\n\n'
                    performance_low_message = 'TRY TO BE MORE ACCURATE WHEN YOU INDICATE THE DIRECTION OF THE SECOND ARROW\n\n'
                    
                    # Print progress
                    self.print_progress(f'-- performance too low!')

        # create message
        if self._block_task == 'mask':
            performance_text = f'{experiment_progress_text}\nIn the last {block_trials_number} trials ' \
                            f'you were correct in {percentage_correct}% of the trials.\n{performance_low_message}' \
                            f'Your response speed was: {round(last_block_rt, 2)} seconds\n{rt_slow_message}'\
                            f'{break_text}'
        elif self._block_task == 'prime':
            performance_text = f'{experiment_progress_text}\nIn the last {block_trials_number} trials ' \
                            f'you were correct in {percentage_correct}% of the trials.\n\n' \
                            f'{break_text}'
               

            
        # Performance clock
        performance_clock = core.Clock()
        self._win.callOnFlip(performance_clock.reset)

        # Add break text
        if have_break == 'forced':
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
            # Print performance to console
            self.print_progress(performance_text)
            # Show performance
            self.show_message(text=performance_text, wait_keypress=['a', 'l'], color='black', height=self._default_text_height*.8, wrapWidth=15, block_keypress=.5)                


        # Log performance
        self.exp_handler.addData('block_type', self._block_type)
        self.exp_handler.addData('trial_type', 'performance')
        self.exp_handler.addData('block_count', self._block_count)
        self.exp_handler.addData('block_correct_responses', correct_responses)
        self.exp_handler.addData('block_trials', block_trials_number)
        self.exp_handler.addData('trial_dur', performance_clock.getTime())

        self.exp_handler.nextEntry()

    def setup_total_trials(self):
        """
        Set the total number of trials and the last block index.

        Returns:
            None
        """
        # Check if block trials is set
        if self._block_trials is None:
            raise ValueError('Block trials is not set. Please create block trials list.')
        
        # Set total number of trials
        self._total_trials = len(self._block_trials) * self._blocks_to_run
        
        # Set last block index
        self._last_block = self._blocks_to_run - 1

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
        self._possible_SOAs_s = [round(self._default_prime_duration_s * x, 4) for x in range(1, 7)] + [.3]

    def _set_debugging_time(self):
        # Set frame rate to 60 because most monitors can handle it
        self._frame_rate = 60
        # Update stimuli timing manually to values achievable by the frame rate
        self._fixation_duration_s = (1/60) * 42
        self._default_prime_duration_s = (1/60)
        self._default_mask_duration_s = self._default_prime_duration_s * 10
        self._possible_SOAs_s = [round(self._default_prime_duration_s * x, 4) for x in range(1, 7)] + [.3]

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

    def mask_instructions(self):

        # --- Instructions for the mask task --- 

        # Show instruction
        self.show_message(text='You will receive instructions for a different task now. ' \
                                'As before, two arrows will appear on each trial, but ' \
                                'this time you will have to indicate the direction of ' \
                                'the second arrow. Press SPACE to continue.', 
                    color='black', height=self._default_text_height * .9, wait_keypress=['space'], pos=(0, 0))

        core.wait(.1)

        # --- 

        # Make fixation
        fixation = self.make_fixation()

        # Make prime examples and labels for what key to use
        mask_back_left = self.make_mask_back(direction='left', vertical_position='center')
        mask_front_left = self.make_mask_front(vertical_position='center')
        mask_back_right = self.make_mask_back(direction='right', vertical_position='center')
        mask_front_right = self.make_mask_front(vertical_position='center')
        mask_back_left.setPos([-5, -3])
        mask_front_left.setPos([-5, -3])
        mask_back_right.setPos([5, -3])
        mask_front_right.setPos([5, -3])

        # make answer help text
        mask_left_mssg = self.show_message(text='Press A', height=self._default_text_height * .9, pos=(-5, -4), wrapWidth=25, color='red', return_text=True)
        mask_right_mssg = self.show_message(text='Press L', height=self._default_text_height * .9, pos=(5, -4), wrapWidth=25, color='red', return_text=True)

        # Draw prime examples and key labels
        mask_back_left.setAutoDraw(True)
        mask_front_left.setAutoDraw(True)
        mask_back_right.setAutoDraw(True)
        mask_front_right.setAutoDraw(True)
        mask_left_mssg.setAutoDraw(True)
        mask_right_mssg.setAutoDraw(True)
        fixation.setAutoDraw(True)

        # Show instructions
        self.show_message(text='You have to press A to indicate that the second arrow is pointing to the left, or L to indicate that it is pointing to the right. In this case, you can answer as soon as the second arrow appears. Press SPACE to continue.', 
                    color='black', height=self._default_text_height * .9, wait_keypress=['space'], pos=(0, 4), wrapWidth=25)

        # Draw everything off
        mask_back_left.setAutoDraw(False)
        mask_front_left.setAutoDraw(False)
        mask_back_right.setAutoDraw(False)
        mask_front_right.setAutoDraw(False)
        mask_left_mssg.setAutoDraw(False)
        mask_right_mssg.setAutoDraw(False)
        fixation.setAutoDraw(False)
        self._flip_it()

        core.wait(.1)
        
        # --- 

    def mask_practice(self):

        # General settings
        self._block_type = 'instructions'
        self._block_task = 'mask'
        trials_to_run = 30
        
        # ~~ 
        # First practice block. Participant needs 10 correct trials in a row. 
        # The task is pretty easy so this is mostly to check that they are reading the instructions

        # Show instructions
        self.show_message(text='You will go over a practice block now. You have to indicate the direction ' \
                            'of the second arrow. Try to be as quick as possible. Place your left index ' \
                            'finger on the A key and your right index finger over the L key. Press A or L ' \
                            'to start the practice.', 
                    color='black', height=self._default_text_height * .9, wait_keypress=['a', 'l'], pos=(0, 0))

        while True:
            # First practice with near zero prime contrast
            self._trial_count = -1
            self._valid_trial_count = -1
            self.reset_block()
            practice_trial_list = self._get_n_trials(trials_to_run)
            
            # Run practice block
            self.run_block(practice_trial_list, task=self._block_task)

            # Show performance
            self.show_performance(block_trials_number=trials_to_run, experiment_progress=False, have_break=None)
            
            # Check if performance is at least 85%. If not give a message and repeat the block
            if self._mask_correct_count < trials_to_run * .85:
                self._simulate_keys(simulate=False)
                self.show_message(text=f'You need to get at least {round(trials_to_run * .85)} correct responses to continue. Press A or L to try again.',
                                  color='black', height=self._default_text_height * .9, wait_keypress=['a', 'l'], pos=(0, 0))
            else:
                break
                
        # Show instructions
        self.show_message(text='You did great! You will continue to the last part of the instructions now. Press SPACE to continue. ', 
                    color='black', height=self._default_text_height * .9, wait_keypress=['space'], pos=(0, 0))


    def prime_instructions(self):

        # --- Instructions for the prime task --- 
       
        # Make prime and mask examples
        prime = self.make_prime(direction='left', vertical_position='center')
        prime.setPos([-5, 0])
        mask_back = self.make_mask_back(direction='right', vertical_position='center')
        mask_front = self.make_mask_front(vertical_position='center')
        mask_back.setPos([5, 0])
        mask_front.setPos([5, 0])
        # Make fixation
        fixation = self.make_fixation()

        # Draw examples
        prime.setAutoDraw(True)
        mask_back.setAutoDraw(True)
        mask_front.setAutoDraw(True)
        fixation.setAutoDraw(True)

        # Show instruction
        self.show_message(text='On each trial two arrows will appear one after the other. Below you can see an example of the first arrow on the left, and the second arrow on the right. Press SPACE to continue.', 
                    color='black', height=self._default_text_height * .9, wait_keypress=['space'], pos=(0, 4))

        # Draw examples off and flip screen
        prime.setAutoDraw(False)
        mask_back.setAutoDraw(False)
        mask_front.setAutoDraw(False)
        self._flip_it()

        core.wait(.1)

        # --- 

        # Show instructions
        mssg = self.show_message(text='On each trial the arrows will quickly appear above or below the fixation cross. Sometimes both arrows will point in the same direction and sometimes they will point in opposite directions. You can see an example of the sequence being repeated in the center of the screen. In this case the sequence has been slowed down to make it easier for you to get an idea of how it looks. Press SPACE to continue.', 
                    color='black', height=self._default_text_height * .9, pos=(-9, 0), wrapWidth=9, return_text=True)
        mssg.setAutoDraw(True)

        # Run example sequence
        self.kb.clearEvents()
        while True:

            # Create stimuli, direction and position is random
            position = np.random.choice(['top', 'bottom'])
            prime = self.make_prime(direction=np.random.choice(['left', 'right']), vertical_position=position)
            mask_back = self.make_mask_back(direction=np.random.choice(['left', 'right']), vertical_position=position)
            mask_front = self.make_mask_front(vertical_position=position)
            
            # Pre-stim time ---
            core.wait(0.7)
            # Present prime ---
            prime.setAutoDraw(True)
            self._flip_it()
            core.wait(0.4)
            prime.setAutoDraw(False)
            self._flip_it()
            # Check for key press ---
            if self.getKeys(keyList=['space']):
                mssg.setAutoDraw(False)
                self._flip_it()
                break
            # SOA ---
            core.wait(0.2)
            # Present mask ---
            mask_back.setAutoDraw(True)
            mask_front.setAutoDraw(True)
            self._flip_it()
            core.wait(0.4)
            mask_back.setAutoDraw(False)
            mask_front.setAutoDraw(False)
            self._flip_it()
            # Check for key press ---
            if self.getKeys(keyList=['space']):
                mssg.setAutoDraw(False)
                self._flip_it()
                break

        # ---

        # Make prime examples and labels for what key to use
        prime_left = self.make_prime(direction='left', vertical_position='center')
        prime_right = self.make_prime(direction='right', vertical_position='center')
        prime_left.setPos([-5, -3])
        prime_right.setPos([5, -3])
        
        fixation.setColor('lightgray')

        # make answer help text
        prime_left_mssg = self.show_message(text='Press A', height=self._default_text_height * .9, pos=(-5, -4), wrapWidth=25, color='red', return_text=True)
        prime_right_mssg = self.show_message(text='Press L', height=self._default_text_height * .9, pos=(5, -4), wrapWidth=25, color='red', return_text=True)

        # Draw prime examples and key labels
        prime_left.setAutoDraw(True)
        prime_right.setAutoDraw(True)
        prime_left_mssg.setAutoDraw(True)
        prime_right_mssg.setAutoDraw(True)

        # Show instructions
        self.show_message(text='Your task is to indicate whether the first arrow is pointing to the left or the right. You have to press A to indicate that the first arrow was pointing to the left, or L to indicate that it was pointing to the right. You can answer as soon as the fixation cross turns gray, like the one you see below. Press SPACE to continue.', 
                    color='black', height=self._default_text_height * .9, wait_keypress=['space'], pos=(0, 4), wrapWidth=25)

        # Draw everything off
        prime_left.setAutoDraw(False)
        prime_right.setAutoDraw(False)
        prime_left_mssg.setAutoDraw(False)
        prime_right_mssg.setAutoDraw(False)
        self._flip_it()

        core.wait(.1)

        # ---

        self.show_message(text='If you answer before the fixation cross turns gray, your answer will not be recorded and the trial will not continue until you answer again. Press SPACE to continue.', 
                    color='black', height=self._default_text_height * .9, wait_keypress=['space'], pos=(0, 4), wrapWidth=25)
        fixation.setAutoDraw(False)
        self._flip_it()

        core.wait(.1)
    
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

    def prepare_new_block(self, task=None):
        # Reset block and performance counters
        self.reset_block()
        
        # Switch to next task
        if task is None:
            self._block_task = self._tasks[self._block_count % 2]
        else:
            self._block_task = task

        # Show prime instructions. Show a text indicating that in the next block the 
        # participant will have to indicate the direction of the first arrow. Show two 
        # arrows as an example, one pointing to the left and one pointing to the right.
        # Also show labels below indicating the keys.

        # Make fixation
        fixation = self.make_fixation()

        if self._block_task == 'prime':
            # Make prime examples and labels for what key to use
            prime_left = self.make_prime(direction='left', vertical_position='center')
            prime_right = self.make_prime(direction='right', vertical_position='center')
            prime_left.setPos([-5, -3])
            prime_right.setPos([5, -3])
            # Make answer help text
            prime_left_mssg = self.show_message(text='Press A', height=self._default_text_height * .9, pos=(-5, -4), wrapWidth=25, color='red', return_text=True)
            prime_right_mssg = self.show_message(text='Press L', height=self._default_text_height * .9, pos=(5, -4), wrapWidth=25, color='red', return_text=True)

            # Draw prime examples and key labels
            prime_left.setAutoDraw(True)
            prime_right.setAutoDraw(True)
            prime_left_mssg.setAutoDraw(True)
            prime_right_mssg.setAutoDraw(True)
            fixation.setAutoDraw(True)

            self.show_message(text=f'In the next block you will have to indicate the direction of the FIRST arrow. Press A to indicate that the FIRST arrow is pointing to the left or L to indicate that the FIRST arrow is pointing to the right. Below you can see an example. Put your left index finger over the A key and your right index finger over the L key. Remember to look at the fixation cross while doing the task.\nPress A/L to start the block. ', 
                        color='black', height=self._default_text_height * .9, wait_keypress=['a', 'l'], pos=(0, 4), wrapWidth=28)

            # Draw everything off
            prime_left.setAutoDraw(False)
            prime_right.setAutoDraw(False)
            prime_left_mssg.setAutoDraw(False)
            prime_right_mssg.setAutoDraw(False)
            fixation.setAutoDraw(False)
            self._flip_it()

            core.wait(.1)
        
        elif self._block_task == 'mask':
            # Make prime examples and labels for what key to use
            mask_back_left = self.make_mask_back(direction='left', vertical_position='center')
            mask_front_left = self.make_mask_front(vertical_position='center')
            mask_back_right = self.make_mask_back(direction='right', vertical_position='center')
            mask_front_right = self.make_mask_front(vertical_position='center')
            mask_back_left.setPos([-5, -3])
            mask_front_left.setPos([-5, -3])
            mask_back_right.setPos([5, -3])
            mask_front_right.setPos([5, -3])

            # make answer help text\
            mask_left_mssg = self.show_message(text='Press A', height=self._default_text_height * .9, pos=(-5, -4), wrapWidth=25, color='red', return_text=True)
            mask_right_mssg = self.show_message(text='Press L', height=self._default_text_height * .9, pos=(5, -4), wrapWidth=25, color='red', return_text=True)

            # Draw prime examples and key labels
            mask_back_left.setAutoDraw(True)
            mask_front_left.setAutoDraw(True)
            mask_back_right.setAutoDraw(True)
            mask_front_right.setAutoDraw(True)
            mask_left_mssg.setAutoDraw(True)
            mask_right_mssg.setAutoDraw(True)
            fixation.setAutoDraw(True)

            # you gotta be fast mssg
            be_quick_mssg = self.show_message(text='Remember you can answer as soon as the second arrow appears. Try to be as quick as possible. ', 
                                              height=self._default_text_height * .9, pos=(0, -5.5), wrapWidth=28, color='red', return_text=True)
            be_quick_mssg.setAutoDraw(True)

            self.show_message(text=f'In the next block you will have to indicate the direction of the SECOND arrow. Press A to indicate that the SECOND arrow is pointing to the left or L to indicate that the SECOND arrow is pointing to the right. Below you can see an example. Put your left index finger over the A key and your right index finger over the L key. Remember to look at the fixation cross while doing the task.\nPress A/L to start the block. ',
                        color='black', height=self._default_text_height * .9, wait_keypress=['a', 'l'], pos=(0, 4), wrapWidth=28)

            # Draw everything off
            mask_back_left.setAutoDraw(False)
            mask_front_left.setAutoDraw(False)
            mask_back_right.setAutoDraw(False)
            mask_front_right.setAutoDraw(False)
            mask_left_mssg.setAutoDraw(False)
            mask_right_mssg.setAutoDraw(False)
            fixation.setAutoDraw(False)
            be_quick_mssg.setAutoDraw(False)
            self._flip_it()

            core.wait(.1)

    def prime_practice(self):
        
        # ---

        # Make fixation
        fixation = self.make_fixation()
        fixation.setAutoDraw(True)

        self.show_message(text='You will go over a practice block now. In this first practice block the sequence has been slowed down to make the task easier. You will need ten correct responses in a row before continuing to the next part of the instructions. You will hear a sound after your response every time you make a mistake. Put your left-hand index finger over the A key and your right-hand index finger over the L key.\nPress A or L to start the practice block. ', 
                    color='black', height=self._default_text_height * .9, wait_keypress=['a', 'l'], pos=(0, 4), wrapWidth=25)
        fixation.setAutoDraw(False)
        self._flip_it()

        core.wait(.1)

        # ---

        # Practice settings
        trials_to_run = 30 
        practice_passed = False
        self._trial_feedback = True
        self._block_task = 'prime'
        self._block_type = 'instructions'
        
        # ~~ 
        # First practice block with longer SOA. Participant needs 10 correct 
        # responses in a row to pass the practice block
        
        # Reset block and performance counters
        self.reset_block()

        while not practice_passed:
            streak_counter = 0
            highest_streak = 0
            self._trial_count = -1
            practice_trial_list = self._get_n_trials(trials_to_run)

            for trial in practice_trial_list:
            
                # Update trial count
                self._trial_count += 1

                # Log trial information
                self.exp_handler.addData('block_count', self._block_count)
                self.exp_handler.addData('trial_count', self._trial_count)
                self.exp_handler.addData('task', self._block_task)
                self.exp_handler.addData('feedback', self._trial_feedback)
                
                # Parse trial information
                prime_direction = trial['prime_direction']
                mask_direction = trial['mask_direction']
                stim_position = trial['stim_position']
                soa = max(self._possible_SOAs_s)
                
                # Print trial information
                progress_text = f'Block: prime_instructions Trial {self._trial_count} Prime --> {prime_direction} // Mask --> {mask_direction} // Position --> {stim_position} // SOA --> {soa}'
                print(progress_text)
                self.print_progress(progress_text)

                # present stimuli 
                self.present_stimuli(self._block_task, prime_direction, mask_direction, stim_position, soa)
                # feedback
                self.play_feedback()
                # update trial and correct counter
                self.update_performance()

                # End trial
                self.exp_handler.nextEntry()

                # End block if 10 correct responses in a row
                streak_counter = (streak_counter + self._this_trial_accuracy) * self._this_trial_accuracy
                highest_streak = max(streak_counter, highest_streak)
                if streak_counter >= 10:
                    practice_passed = True
                    break
            
        # Check if practice is passed
        if practice_passed:
            core.wait(.1)
            self.show_message(text='You did great! You will go over another practice block now. Press SPACE to continue.', 
                        color='black', height=self._default_text_height * .9, wait_keypress=['space'])
        else:
            core.wait(.1)
            self.show_message(text=f'During the last {trials_to_run} trials you longest streak was {highest_streak} correct responses in a row. You need to get at least 10 correct in a row to pass the practice block. Press SPACE to try again.', 
                        color='black', height=self._default_text_height * .9, wait_keypress=['space'])
        
        # ~~ 
        # Second practice block with LONG and normal SOAs. Participant needs to get all the
        # long SOA trials correct to pass
        
        # Make fixation
        fixation = self.make_fixation()
        fixation.setAutoDraw(True)

        self.show_message(text='In the next block some of the trials will be easy, like the ones you just did, and some trials will be very difficult. You should be able to answer correctly in all the easy trials. Press A or L to start the practice block.  ', 
                    color='black', height=self._default_text_height * .9, wait_keypress=['a', 'l'], pos=(0, 4), wrapWidth=25)
        fixation.setAutoDraw(False)
        self._flip_it()

        core.wait(.1)

        # Reset block and performance counters
        self.reset_block()

        practice_passed = False
        while not practice_passed:
            easy_trial_correct_counter = 0
            self._trial_count = -1

            # Get 10 easy trials
            easy_trials = self.create_block_trials_list(repeat_unique_trials=1, return_list=True)
            easy_trials = list(filter(lambda x: x['SOA'] == max(self._possible_SOAs_s), easy_trials))
            easy_trials = random.sample(easy_trials*2, 10)
            # Get 20 hard trials
            hard_trials = self.create_block_trials_list(repeat_unique_trials=1, return_list=True)
            hard_trials = list(filter(lambda x: x['SOA'] != max(self._possible_SOAs_s), hard_trials))
            hard_trials = random.sample(hard_trials, 20)

            # Append easy and hard trials
            practice_trial_list = easy_trials + hard_trials
            np.random.shuffle(practice_trial_list)

            for trial in practice_trial_list:
            
                # Update trial count
                self._trial_count += 1

                # Log trial information
                self.exp_handler.addData('block_count', self._block_count)
                self.exp_handler.addData('trial_count', self._trial_count)
                self.exp_handler.addData('task', self._block_task)
                self.exp_handler.addData('feedback', self._trial_feedback)
                
                # Parse trial information
                prime_direction = trial['prime_direction']
                mask_direction = trial['mask_direction']
                stim_position = trial['stim_position']
                soa = trial['SOA']
                
                # Print trial information
                progress_text = f'Block: prime_instructions Trial {self._trial_count} Prime --> {prime_direction} // Mask --> {mask_direction} // Position --> {stim_position} // SOA --> {soa}'
                print(progress_text)
                self.print_progress(progress_text)

                # present stimuli 
                self.present_stimuli(self._block_task, prime_direction, mask_direction, stim_position, soa)
                # feedback
                self.play_feedback()
                # update trial and correct counter
                self.update_performance()

                # End trial
                self.exp_handler.nextEntry()

                # Check if easy trial is correct
                easy_trial_correct_counter += self._this_trial_accuracy * (soa == max(self._possible_SOAs_s))

            # End block if 10 correct responses in a row
            if easy_trial_correct_counter >= 9:
                practice_passed = True
            
            # Check if practice is passed
            if practice_passed:
                core.wait(.1)
                self.show_message(text='You did great! You will go over another practice block now. Press SPACE to continue.', 
                            color='black', height=self._default_text_height * .9, wait_keypress=['space'])
                break
            else:
                core.wait(.1)
                self.show_message(text=f'During the last {trials_to_run} trials you did not get most of the easy trials right. You will repeat the last practice block. You need to get at least most of the easy trials right before continuing. Press A or L to try again.', 
                            color='black', height=self._default_text_height * .9, wait_keypress=['a', 'l'])
            
        # ~~ 
        # Third practice block. There is no performance requirement
        self.show_message(text=f'In the next practice block all trials will be very difficult. Even though sometimes it will seem like the first arrow did not appear at all, both arrows appear in all trials. Try your best to indicate the direction of the first arrow.\nPress A or L to start the practice block. ', 
                                color='black', height=self._default_text_height * .9, wrapWidth=25, wait_keypress=['a', 'l'])
        
        practice_passed = False
        
        self.reset_block()
        self._trial_count = -1

        # Get 20 hard trials
        practice_trial_list = self.create_block_trials_list(repeat_unique_trials=1, return_list=True)
        practice_trial_list = list(filter(lambda x: x['SOA'] != max(self._possible_SOAs_s), practice_trial_list))
        practice_trial_list = random.sample(practice_trial_list, 30)

        for trial in practice_trial_list:
        
            # Update trial count
            self._trial_count += 1

            # Log trial information
            self.exp_handler.addData('block_count', self._block_count)
            self.exp_handler.addData('trial_count', self._trial_count)
            self.exp_handler.addData('task', self._block_task)
            self.exp_handler.addData('feedback', self._trial_feedback)
            
            # Parse trial information
            prime_direction = trial['prime_direction']
            mask_direction = trial['mask_direction']
            stim_position = trial['stim_position']
            soa = trial['SOA']
            
            # Print trial information
            progress_text = f'Block: prime_instructions Trial {self._trial_count} Prime --> {prime_direction} // Mask --> {mask_direction} // Position --> {stim_position} // SOA --> {soa}'
            print(progress_text)
            self.print_progress(progress_text)

            # present stimuli 
            self.present_stimuli(self._block_task, prime_direction, mask_direction, stim_position, soa)
            # feedback
            self.play_feedback()
            # update trial and correct counter
            self.update_performance()

            # End trial
            self.exp_handler.nextEntry()

        # Show block performance
        self.show_performance(block_trials_number=trials_to_run, experiment_progress=False, have_break=None)
 
        # Practice end message
        self.show_message(text=f'During the experiment some trials will be easy but most of them will be hard. Even if you feel you can\'t see the first arrow, it is important that you try your best to indicate its direction.\nPress SPACE to continue. ', 
                          color='black', height=self._default_text_height * .9, wrapWidth=25, wait_keypress=['space'])

    class staircaseHandle:

        # Initialize staircase
        def __init__(self,
                    start_value: float = .1,
                    target_performance: float = .75,
                    reversals: list = None,
                    step_sizes: list = None,
                    power_law: float = 1,
                    min_value_correction: float = None,
                    max_value_correction: float = None,
                    name: str = 'staircase',
                    siam: bool = True, 
                    custom_adjustment_matrix: dict = None):
            """

            :param start_value: Value for first trial.
            :param target_performance: Target performance (t value in Kaernbach 1991) is expressed as the hit-rate minus false alarm. The percentage stated in target_performance can be understood as the target_performance% of the distance between chance (.5) and perfect performance. For example, when target_performance is 0.5 the staircase aims at 50% between 0.5 and 1 ( 0.5 * 0.5 + 0.5 = 0.75), resulting in a 75% accuracy. More generally the aimed accuracy can be calculated for a given target_performance as follows: 0.5 * target_performance + 0.5.
            :param reversals: Number of reversals to run before staircase ends. First reversals are discarded
                                when calculating the threshold.
            :param step_sizes: Step sizes to use for each number of reversals.
            :param power_law: Transformation to apply on staircased value (staircase_value ** power_law). Set to 1 when
                                perceived and absolute value is equal.
            :param min_value_correction: staircase_value cannot go below this value
            :param max_value_correction: staircase_value cannot go above this value
            :param name: Staircase name for logging or for when you run multiple staircases
            :param siam: Use Kaernbach, C. (1990) single‐interval adjustment‐matrix (SIAM).
            :param custom_payoff_matrix: Alternative any contingency table can be fed to inform the step sizes. This
                                            should be a dictionary (e.g. {'hit': -1, 'miss': 4, 'fa': 5, 'cr': 0})
                                            indicating each step size. Feeding a custom payoff matrix will override
                                            the siam argument.
            """

            # Set defaults. These values are here for practical reasons. Different
            # tasks and stimuli will require particular reversals number and step
            # sizes. This defaults should not be assume to be sensible to your 
            # particular task and stim.
            if reversals is None:
                reversals = [5, 15]
            if step_sizes is None:
                step_sizes = [1, .5]

            # Save inputs
            self.name = name
            self.dv = start_value  # Staircase start value
            self.p = target_performance  # target performance (t value in Kaernbach paper)
            self.reversals = reversals  # Total reversals
            self.step_sizes = step_sizes  # Step sizes for first and second phase
            self.current_step_size = self.step_sizes[0]  # Assign first step size
            self.powers_law = power_law  # Power law to transform to perceived scale
            self.min_corr = min_value_correction
            self.max_corr = max_value_correction
            self.siam = siam
            # Trackers
            self.phase = 0  # Only reversal values in phase 2 are used to get threshold
            self.dvs = []  # Track all staircased values
            self.dvs_on_rev = []  # Track staircased values on reversals
            self.trial_number = 0  # Trial counter
            self.revn = 0  # Reversal counter
            self.reversal_on_trial = []  # Reversal trial
            self.is_correct_track = []  # Track corr/incorr booleans
            self.stim_track = []  # Track corr/incorr booleans
            # Indicators
            self.staircase_over = False  # Staircase over?
            self.first_trial = True  # Is first trial?

            # Last ans trackers
            self.previous_is_correct = None
            self.isRev = False

            # SIAM Adjustment matrix =====================================
            # These contingency tables are taken from Table 1 in Kaernbach, C. (1990).
            # A single‐interval adjustment‐matrix (SIAM) procedure for unbiased adaptive testing. The
            # Journal of the Acoustical Society of America, 88(6), 2645–2655. https://doi.org/10.1121/1.399985

            # Note that here the sign of hits and missess are inverted (compared to the values if 
            # Kaernback's paper). This is because in this particular experiment to increase the 
            # difficulty after a hit, the contrast of the mask is increased. 
            self.adjustment_matrices = {
                '25': {'hit': 3, 'miss': -1, 'fa': -4, 'cr': 0},   # 62.5%
                '33': {'hit': 2, 'miss': -1, 'fa': -3, 'cr': 0},   # 66.5%
                '50': {'hit': 1, 'miss': -1, 'fa': -2, 'cr': 0},   # 75%
                '66': {'hit': 1, 'miss': -2, 'fa': -3, 'cr': 0},   # 83%
                '75': {'hit': 1, 'miss': -3, 'fa': -4, 'cr': 0}    # 87.5%
            }

            # Either use SIAM payoff matrices defined above
            if siam:
                self.payoffs = self.adjustment_matrices[str(int(target_performance * 100))]
            else:
                # In principle this gets you proportionally the same values as in the
                # SIAM procedure above. The only difference seems to be that all values
                # are increase until there are no decimal step sizes. So, for example,
                # when the aimed hit-rate is .25 you multiply all values by 3 to get
                # the step sizes in the SIAM procedure
                self.payoffs = {'hit': 1, 
                                'miss': - target_performance / (1 - target_performance),
                                'fa': - 1 / (1 - target_performance), 
                                'cr': 0}

            # Set custom payoff matrix
            if isinstance(custom_adjustment_matrix, dict):
                self.payoffs = custom_adjustment_matrix
                import warnings
                warnings.warn('custom_payoff_matrix detected, overriding SIAM argument.')

        def new_trial(self, is_correct: bool, stim: bool):
            """

            :param is_correct: Was the current trial correct?
            :param stim: Was the target present? Alternatively this can be used for category A
                            and B in discrimination settings.
            """
            # If staircase not over -----------------------------
            if not self.staircase_over:

                # Update trial count and save current dv
                self.trial_number += 1  # trial number
                self.dvs.append(self.dv)  # track value of current trial

                # track is correct
                self.is_correct_track.append(is_correct)
                self.stim_track.append(stim)

                # Check if first trial -----------------------------

                # In the first trial we can't check for reversals
                # because there are no previous trials.
                if not self.first_trial:

                    # Check if reversal -----------------------
                    if is_correct != self.previous_is_correct:
                        self.isRev = True
                        # reversal counter
                        self.revn += 1
                        # log trial with reversal
                        self.reversal_on_trial.append(self.trial_number)
                        # Only record values when in the second phase
                        if self.phase == 1:
                            # append dev value on reversal
                            self.dvs_on_rev.append(self.dv)
                    else:
                        self.isRev = False

                # Confusion matrix label. This is used to get the step size
                if is_correct:
                    if stim:
                        conf_mat = 'hit'
                    else:
                        conf_mat = 'cr'
                else:
                    if stim:
                        conf_mat = 'miss'
                    else:
                        conf_mat = 'fa'

                # Update dv -----------------------------------------

                # Convert contrast to perceived contrast
                perceived = self.dv ** self.powers_law

                # Update value
                perceived_new = perceived + (self.payoffs[conf_mat] * self.current_step_size)

                # Prevent value going below 0. When transforming back
                # to non-perceived space a negative number will turn
                # into a complex number.
                if perceived_new < 0: perceived_new = 0 

                # Convert new perceived to contrast space
                self.dv = perceived_new ** (1 / self.powers_law)

                # Min/max corrections
                if self.min_corr or self.min_corr == 0:
                    if self.dv < self.min_corr: self.dv = self.min_corr
                if self.max_corr:
                    if self.dv > self.max_corr: self.dv = self.max_corr

                # if max. number of reversals end staircase
                if self.revn >= sum(self.reversals):
                    self.staircase_over = True

                # if first portion of reversals done continue to second phase
                if self.revn >= self.reversals[0]:
                    self.phase = 1
                    self.current_step_size = self.step_sizes[self.phase]

                # first trial over
                self.first_trial = False
                # store last correct/incorrect answer
                self.previous_is_correct = is_correct

        def get_threshold(self):
            """
            The staircase threshold can be calculated only when all the reversals are done. Alternatively, set
            staircase.staircase_over = True and then call staircase.get_threshold().

            :return: Staircase threshold.
            """

            if self.staircase_over:
                return np.mean(self.dvs_on_rev)
            else:
                return 'Staircase is not over.'

        def print_staircase(self):
            # Print staircase info to console, use after calling new_trial

            print('\n###############################\n')
            print(self.name)
            print('Trial number: ' + str(self.trial_number))
            print('Current trial is correct: ' + str(self.previous_is_correct))
            print('Current dv value: ' + str(round(self.dv, 5)))
            print('Reversal count: ' + str(self.revn))
            print('Phase: ' + str(self.phase))
            print('Staircase over? ' + str(self.staircase_over))
            print('\n###############################\n')

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
            'session': '',
            'first_task': ['', 'prime', 'mask'],
            'blocks_to_run': 12,
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
                    if key in ['participant', 'session', 'frame_rate', 'blocks_to_run']:
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
                assert experiment_info['session'] > 0, "'session' should be a positive non-zero integer."
                assert experiment_info['frame_rate'] > 0, "'frame_rate' should be a positive non-zero integer."
                assert experiment_info['blocks_to_run'] > 0, "'blocks_to_run' should be a positive non-zero integer."
                assert experiment_info['first_task'] in ['prime', 'mask'], "'first_task' should be either 'prime' or 'mask'."

            except AssertionError as e:
                print('ERROR:', str(e))
                continue

            break
        
        # Check if different part of the experiment need to be run
        if experiment_info['session'] > 1:
            something_else = {'demographics': False, 
                            'prime_instructions': False,
                            'mask_instructions': False,
                            'warm_up': True}
        else:
            something_else = {'demographics': True, 
                          'prime_instructions': True,
                          'mask_instructions': True,
                          'warm_up': False}
            
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

    def warm_up(self):
        self._block_type = 'warm_up'
        
        # Make fixation
        fixation = self.make_fixation()
        fixation.setAutoDraw(True)
        # Show instructions
        self.show_message(text='You will go over a warm-up block now. Press SPACE to continue.',
                            color='black', height=self._default_text_height * .9, wait_keypress=['space'], pos=(0, 4))
        fixation.setAutoDraw(False)
        self._flip_it()

        # Instructions for next block
        self.prepare_new_block()    
        self._trial_count = -1
        self._valid_trial_count =-1
        # Run 30 trials
        self.run_block(trials=self._get_n_trials(30))
        self.show_performance(block_trials_number=30, experiment_progress=False, have_break=None)

        # Instructions for next block
        self.prepare_new_block()
        self._trial_count = -1
        self._valid_trial_count =-1
        # Run 30 trials
        self.run_block(trials=self._get_n_trials(30))
        self.show_performance(block_trials_number=30, experiment_progress=False, have_break=None)
        
    def setup_progress_log(self):
        # path to progress log folder 
        self._progress_folder = os.path.join(self._this_dir, 'log')
        # if folder doesn't exist, create it
        if not os.path.isdir(self._progress_folder):
            os.mkdir(self._progress_folder)

        # create file
        self._log_file_name = os.path.join(self._progress_folder, f'{self._experiment_info["participant"]}_{self._experiment_info["session"]}_progress.log')

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


if __name__ == '__main__':
    '''
    this is an example of how to run this experiment. 
    to run the experiment use the script main.py

    '''
    
    # Initialize experiment
    e = exp()
    e.start_exp_handler(exp_info={'frame_rate': 60, 'participant': 999, 'session': 999, 'blocks_to_run': 4})
    e._set_debugging_time()
    
    # Apply frame rate to timing
    e.update_timing()
    # create trials
    e.create_block_trials_list(repeat_unique_trials=1)
    # print(*e._block_trials, sep='\n')    
    e.setup_total_trials()

    print('exp started...')

    # open window
    e.open_window(monitor='nostromo', full_screen=True, size=[1200, 1000])

    # Prepare for new block. Increase block count and reset performance counters
    e.reset_block()
    e._trial_count = -1    
    e._valid_trial_count = -1
    # Block settings
    e._block_task = 'mask'  
    e._block_type = 'experiment'

    e._simulate_keys(True)
    e.run_block(trials=50)   
    e.show_performance(have_break='short')
    e.reset_block()
