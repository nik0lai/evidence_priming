from exp import exp
import os
from psychopy import visual

e = exp()

# open window
e.open_window(monitor='nostromo', full_screen=True, screen_index=1)

# make prime
prime_t = e.make_prime('left', 'top')
mask_back_t = e.make_mask_back('left', 'top')
mask_fore_t = e.make_mask_front('top')

prime_b = e.make_prime('right', 'bottom')
mask_back_b = e.make_mask_back('right', 'bottom')
mask_fore_b = e.make_mask_front('bottom')

# guide line
x_pos = .94
vert_line_r = visual.Line(
    win=e._win,
    start=(x_pos, -10),
    end=(x_pos, 10),
    lineColor='red',
    lineWidth=1,
)
x_pos = -.94
vert_line_l = visual.Line(
    win=e._win,
    start=(x_pos, -10),
    end=(x_pos, 10),
    lineColor='red',
    lineWidth=1,
)


# diag_line = visual.Line(
#     win=e._win,
#     size=1,
#     ori=180,
#     pos=(0,0),
#     lineColor='red',
#     lineWidth=1,
# )
# # diagonal line
# # diag_line = visual.Line(
# #     win=e._win,
# #     start=(-.93, 1.38),
# #     end=(-.73, 1.38+.192),
# #     lineColor='red',
# #     lineWidth=1,
# # )
# diag_line.pos += (-.93, 1.4)
# diag_line.pos += -.33

mask_back_t.draw()
mask_fore_t.draw()
prime_t.draw()

mask_back_b.draw()
mask_fore_b.draw()
prime_b.draw()

# vert_line_l.draw()
# vert_line_r.draw()
# diag_line.draw()

e._win.flip()
e._win.getMovieFrame()
e._win.saveMovieFrames(os.path.join(os.getcwd(), 'stim_test.png'))

