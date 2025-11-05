from exp import exp
import os

e = exp()

# open window
e.open_window(monitor='vu', full_screen=False, screen_index=1)

# Check meta-contrast

# make prime
prime_t = e.make_prime('left', 'top')
mask_fore_t = e.make_mask_front('top')

prime_b = e.make_prime('right', 'bottom')
mask_fore_b = e.make_mask_front('bottom')

# change colors for easy check
prime_t.setFillColor(None)
prime_b.setFillColor(None)
mask_fore_t.setColor('pink')
mask_fore_b.setColor('pink')

# ------------------------------------------------------

# draw masks
prime_t.draw()
prime_b.draw()
mask_fore_t.draw()
mask_fore_b.draw()

e._flip_it()
e._win.getMovieFrame()
e._win.saveMovieFrames(os.path.join(os.getcwd(), 'cutout_on_prime.png'))

e._win.flip()

# ------------------------------------------------------

# draw masks
mask_fore_t.draw()
mask_fore_b.draw()
prime_t.draw()
prime_b.draw()

e._flip_it()
e._win.getMovieFrame()
e._win.saveMovieFrames(os.path.join(os.getcwd(), 'prime_on_cutout.png'))

e._win.flip()


# ------------------------------------------------------

mask_back_t = e.make_mask_back('left', 'top')
mask_fore_t = e.make_mask_front('top')
mask_back_b = e.make_mask_back('right', 'bottom')
mask_fore_b = e.make_mask_front('bottom')

# draw masks
mask_back_t.draw()
mask_fore_t.draw()
mask_back_b.draw()
mask_fore_b.draw()

e._flip_it()
e._win.getMovieFrame()
e._win.saveMovieFrames(os.path.join(os.getcwd(), 'masks.png'))

# ------------------------------------------------------

prime_t = e.make_prime('left', 'top')
prime_b = e.make_prime('right', 'bottom')

# draw masks
prime_t.draw()
prime_b.draw()

e._flip_it()
e._win.getMovieFrame()
e._win.saveMovieFrames(os.path.join(os.getcwd(), 'primes.png'))
