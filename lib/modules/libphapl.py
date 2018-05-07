# -*- coding: utf-8 -*-
# phapl: Phase Plane helper

import re
import string
import json

import sympy

# ######################################################################
# Solving part

e, u, v, x, y, l = sympy.symbols('e u v x y l')
# l is for lambda

# arctg = sympy.atan

def from_string(s, replace_e = True):
    if type(s) == str:
        s = re.sub(r'\barctg\b', 'atan', s)
        if replace_e:
            s = re.sub(r'\be\b', 'E', s)
    return sympy.simplify(s)

# print tasks

def solve_system(s_dotx, s_doty):
    # ** due to bug(?) in sympy, we don't replace e for E before solving
    dotx = from_string(s_dotx, False)
    doty = from_string(s_doty, False)
    r = sympy.solve([dotx, doty], [x, y], check = False)
    dotx = from_string(s_dotx)
    doty = from_string(s_doty)
    real, img = [], []
    if type(r) == dict:
        r = [[ r[x], r[y] ]]
    for sol in r:
        rx, ry = sol
        rx = rx.subs(e, sympy.E)
        ry = ry.subs(e, sympy.E)
        s = { x : rx, y : ry }
        # assert sympy.Eq(dotx.subs(s), 0) == True
        # assert sympy.Eq(doty.subs(s), 0) == True
        if rx.is_real and ry.is_real:
            real.append([rx, ry])
        else:
            img.append([rx, ry])
    return real, img

# real, img = solve_system(*tasks[1])
# real, img = solve_system(*tasks[0])

# print real, img

# Returns constant and linear parts
def linearize(f, *vs):
    assert len(vs) == 2
    # print f, vs
    # # %% \u0438\u0437-\u0437\u0430 \u0441\u043a\u043e\u0431\u043e\u043a \u043c\u043e\u0436\u0435\u0442 \u043d\u0435 \u0441\u0440\u0430\u0431\u043e\u0442\u0430\u0442\u044c \u0437\u0430\u043c\u0435\u043d\u0430 subs(vs[0] * vs[1], 0); \u043f\u0440\u0438\u043c\u0435\u0440 ((u + v - 1)**2 - 1)
    # # %% \u043c\u043e\u0436\u0435\u0442 \u0431\u044b\u0442\u044c \u043d\u0443\u0436\u0435\u043d expand \u0432 \u043a\u043e\u043d\u0446\u0435 from_string
    # r = from_string(f).series(vs[0], n = 2).removeO().series(vs[1], n = 2).removeO().subs(vs[0] * vs[1], 0)
    # c = r.subs({ vs[0] : 0, vs[1] : 0 })
    # nc = r - c
    # return c, nc
    f = from_string(f)
    c = f.subs({ vs[0] : 0, vs[1] : 0 })
    nc = (f.diff(vs[0]).subs({ vs[0] : 0, vs[1] : 0 }) * vs[0]
          + f.diff(vs[1]).subs({ vs[0] : 0, vs[1] : 0 }) * vs[1])
    return c, nc

# print linearize('sin(x * y)', x, y)

def get_abcd(dotx, doty, rx, ry):
    tx, ty = [ from_string(f).subs({ x : rx + u, y : ry + v })
               for f in (dotx, doty) ]
    # tx, ty = [sympy.simplify(f) for f in tx, ty]

    c, linear_x = linearize(tx, u, v)
    # %% \u0447\u0442\u043e \u0442\u0443\u0442 \u0434\u0435\u043b\u0430\u0442\u044c \u043f\u0440\u0438 \u043e\u0442\u043a\u043b\u043e\u043d\u0435\u043d\u0438\u044f\u0445?
    assert c == 0
    c, linear_y = linearize(ty, u, v)
    assert c == 0

    # print linear_x, '||', linear_y

    is_linear = False
    if (sympy.simplify(tx - linear_x) == 0
        and sympy.simplify(ty - linear_y) == 0
        ):
        is_linear = True

    (a, b), (c, d) = [[ f.subs({ u : i, v : j })
                        for i, j in ((1, 0), (0, 1)) ]
                      for f in (linear_x, linear_y) ]
    # print a, b, c, d
    return (a, b, c, d), is_linear

# abcd = get_abcd(*(tasks[0] + real[0]))

def characteristic_equation(a, b, c, d):
    eq = (a - l) * (d - l) - b * c
    ls = sympy.solve(eq, l)
    if len(ls) == 1:
        ls += ls
    return tuple(ls)

# ls = characteristic_equation(*abcd)

# \u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u043e\u0441\u0442\u044c:
#  \u041e\u0431\u043e\u0437\u043d\u0430\u0447\u0435\u043d\u0438\u044f: l1 - \lambda_1, l2 - \lambda_2,
#    \u041d - \u041d\u0435\u0443\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u043e, \u0423 - \u0410\u0441\u0438\u043c\u043f\u0442\u043e\u0442\u0438\u0447\u0435\u0441\u043a\u0438 \u0443\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u043e, \u041b - \u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u043e \u043f\u043e \u041b\u044f\u043f\u0443\u043d\u043e\u0432\u0443
# # |\u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u043e\u0441\u0442\u044c|                 \u0422\u0438\u043f| \u041f\u0440\u0438\u0437\u043d\u0430\u043a\u0438
# 1  \u041d               \u041d\u0435\u0443\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0443\u0437\u0435\u043b: l1 != l2, l1 > 0, l2 > 0
# 2  \u0423                 \u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0443\u0437\u0435\u043b: l1 != l2, l1 < 0, l2 < 0
# 3  \u041d                           \u0421\u0435\u0434\u043b\u043e: l1 != l2, l1 > 0, l2 < 0 (\u0438\u043b\u0438 l1 < 0, l2 > 0)
# 4  \u041b                           \u0426\u0435\u043d\u0442\u0440: Re(l1) = Re(l2), Re(l1) = 0,
#                                         Im(l1) = -Im(l2), Im(l1) != 0
# 5  \u041d              \u041d\u0435\u0443\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0444\u043e\u043a\u0443\u0441: Re(l1) = Re(l2), Re(l1) > 0,
#                                         Im(l1) = -Im(l2), Im(l1) != 0
# 6  \u0423                \u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0444\u043e\u043a\u0443\u0441: Re(l1) = Re(l2), Re(l1) < 0,
#                                         Im(l1) = -Im(l2), Im(l1) != 0
# 7  \u041d   \u041d\u0435\u0443\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0432\u044b\u0440\u043e\u0436\u0434\u0435\u043d\u043d\u044b\u0439 \u0443\u0437\u0435\u043b: l1 = l2, l1 > 0, \u0438\u0441\u043a\u043b\u044e\u0447\u0430\u044f 7\u0430
# 7a \u041d \u041d\u0435\u0443\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0434\u0438\u043a\u0440\u0438\u0442\u0438\u0447\u0435\u0441\u043a\u0438\u0439 \u0443\u0437\u0435\u043b: l1 = l2, l1 > 0, x' = ax, y' = ay, a > 0
# 8  \u0423     \u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0432\u044b\u0440\u043e\u0436\u0434\u0435\u043d\u043d\u044b\u0439 \u0443\u0437\u0435\u043b: l1 = l2, l1 < 0, \u0438\u0441\u043a\u043b\u044e\u0447\u0430\u044f 8\u0430
# 8a \u0423   \u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0434\u0438\u043a\u0440\u0438\u0442\u0438\u0447\u0435\u0441\u043a\u0438\u0439 \u0443\u0437\u0435\u043b: l1 = l2, l1 < 0, x' = ax, y' = ay, a < 0

def stability(l1, l2, a, b, c, d):
    r1, i1 = l1.as_real_imag()
    r2, i2 = l2.as_real_imag()
    if i1 == 0 and i2 == 0:
        if r1 == r2:
            if l1 > 0:
                if b == 0 and c == 0 and a == d and a > 0:
                    return '7a'
                else:
                    return '7'
            elif l1 < 0:
                if b == 0 and c == 0 and a == d and a < 0:
                    return '8a'
                else:
                    return '8'
            else:
                return 'err4'
        else:
            if r1 > 0 and r2 > 0:
                return '1'
            elif r1 < 0 and r2 < 0:
                return '2'
            elif (r1 < 0 and r2 > 0) or (r1 > 0 and r2 < 0):
                return '3'
            else:
                return 'err3'
    else:
        if r1 == r2 and i1 == -i2:
            if r1 == 0:
                return '4'
            elif r1 > 0:
                return '5'
            elif r1 < 0:
                return '6'
            else:
                return 'err2'
        else:
            return 'err1'

# s = stability(*(ls + abcd))

stability_names = [
    u"\u041e\u0441\u043e\u0431\u0430\u044f \u0442\u043e\u0447\u043a\u0430 \u043d\u0435 \u0434\u043e\u043f\u0443\u0441\u043a\u0430\u0435\u0442 \u043b\u0438\u043d\u0435\u0430\u0440\u0438\u0437\u0430\u0446\u0438\u044e",
    u"\u041d\u0435\u0443\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u043e",
    u"\u0410\u0441\u0438\u043c\u043f\u0442\u043e\u0442\u0438\u0447\u0435\u0441\u043a\u0438 \u0443\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u043e",
    u"\u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u043e \u043f\u043e \u041b\u044f\u043f\u0443\u043d\u043e\u0432\u0443"
]

error = 0
unstable = 1
stable = 2
lyapunov = 3

point_type_names = {

    "1"  : [ unstable, u"\u041d\u0435\u0443\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0443\u0437\u0435\u043b" ],
    "2"  : [   stable, u"\u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0443\u0437\u0435\u043b" ],
    "3"  : [ unstable, u"\u0421\u0435\u0434\u043b\u043e" ],
    "4"  : [ lyapunov, u"\u0426\u0435\u043d\u0442\u0440" ],
    "5"  : [ unstable, u"\u041d\u0435\u0443\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0444\u043e\u043a\u0443\u0441" ],
    "6"  : [   stable, u"\u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0444\u043e\u043a\u0443\u0441" ],
    "7"  : [ unstable, u"\u041d\u0435\u0443\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0432\u044b\u0440\u043e\u0436\u0434\u0435\u043d\u043d\u044b\u0439 \u0443\u0437\u0435\u043b" ],
    "7a" : [ unstable, u"\u041d\u0435\u0443\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0434\u0438\u043a\u0440\u0438\u0442\u0438\u0447\u0435\u0441\u043a\u0438\u0439 \u0443\u0437\u0435\u043b" ],
    "8"  : [   stable, u"\u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0432\u044b\u0440\u043e\u0436\u0434\u0435\u043d\u043d\u044b\u0439 \u0443\u0437\u0435\u043b" ],
    "8a" : [   stable, u"\u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0434\u0438\u043a\u0440\u0438\u0442\u0438\u0447\u0435\u0441\u043a\u0438\u0439 \u0443\u0437\u0435\u043b" ],

    "err1" : [ error, u"\u041e\u0441\u043e\u0431\u0430\u044f \u0442\u043e\u0447\u043a\u0430 \u043d\u0435 \u0434\u043e\u043f\u0443\u0441\u043a\u0430\u0435\u0442 \u043b\u0438\u043d\u0435\u0430\u0440\u0438\u0437\u0430\u0446\u0438\u044e (1)" ],
    "err2" : [ error, u"\u041e\u0441\u043e\u0431\u0430\u044f \u0442\u043e\u0447\u043a\u0430 \u043d\u0435 \u0434\u043e\u043f\u0443\u0441\u043a\u0430\u0435\u0442 \u043b\u0438\u043d\u0435\u0430\u0440\u0438\u0437\u0430\u0446\u0438\u044e (2)" ],
    "err3" : [ error, u"\u041e\u0441\u043e\u0431\u0430\u044f \u0442\u043e\u0447\u043a\u0430 \u043d\u0435 \u0434\u043e\u043f\u0443\u0441\u043a\u0430\u0435\u0442 \u043b\u0438\u043d\u0435\u0430\u0440\u0438\u0437\u0430\u0446\u0438\u044e (3)" ],
    "err4" : [ error, u"\u041e\u0441\u043e\u0431\u0430\u044f \u0442\u043e\u0447\u043a\u0430 \u043d\u0435 \u0434\u043e\u043f\u0443\u0441\u043a\u0430\u0435\u0442 \u043b\u0438\u043d\u0435\u0430\u0440\u0438\u0437\u0430\u0446\u0438\u044e (4)" ]
}

# print point_type_names[s][1]

def eigenvectors(a, b, c, d):
    t = sympy.Matrix([[a, b], [c, d]]).eigenvects()
    if len(t) == 2:
        assert t[0][1] == 1
        assert t[1][1] == 1
        return list(t[0][2][0]), list(t[1][2][0])
    if len(t) == 1:
        assert t[0][1] == 2
        return list(t[0][2][0]), None
    assert 0

# Returns: number of imaginary points, descriptions of real points
def get_points(dotx, doty):
    real, img = solve_system(dotx, doty)
    k_img = len(img)
    r = []
    g_is_linear = None
    for p in real:
        pr = { 'x' : p[0], 'y' : p[1] }
        abcd, is_linear = get_abcd(dotx, doty, *p)
        ls = characteristic_equation(*abcd)
        ss = stability(*(ls + abcd))
        if g_is_linear != None:
            assert g_is_linear == is_linear
        g_is_linear = is_linear
        # print p, ss
        s = point_type_names[ss]
        pr['is_linear'] = is_linear
        pr['l1'], pr['l2'] = ls
        pr['stability'] = stability_names[s[0]]
        pr['type'] = s[1]
        pr['ab'] = abcd[0] * u + abcd[1] * v
        pr['cd'] = abcd[2] * u + abcd[3] * v
        if pr['type'] in { u"\u041d\u0435\u0443\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0443\u0437\u0435\u043b", u"\u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0443\u0437\u0435\u043b", u"\u0421\u0435\u0434\u043b\u043e",
                           u"\u041d\u0435\u0443\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0432\u044b\u0440\u043e\u0436\u0434\u0435\u043d\u043d\u044b\u0439 \u0443\u0437\u0435\u043b",
                           u"\u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u044b\u0439 \u0432\u044b\u0440\u043e\u0436\u0434\u0435\u043d\u043d\u044b\u0439 \u0443\u0437\u0435\u043b"
                           }:
            # %% \u043c\u043e\u0436\u043d\u043e \u0431\u044b\u043b\u043e \u0431\u044b \u0432\u044b\u0431\u0440\u0430\u0442\u044c \u043f\u043e\u0440\u044f\u0434\u043e\u043a \u0442\u0430\u043a, \u0447\u0442\u043e\u0431\u044b \u0441\u043e\u0431\u0441\u0442\u0432\u0435\u043d\u043d\u044b\u0435
            #  % \u0432\u0435\u043a\u0442\u043e\u0440\u044b \u0441\u043e\u043e\u0442\u0432\u0435\u0442\u0441\u0442\u0432\u043e\u0432\u0430\u043b\u0438 \u0441\u043e\u0431\u0441\u0442\u0432\u0435\u043d\u043d\u044b\u043c \u0437\u043d\u0430\u0447\u0435\u043d\u0438\u044f\u043c
            ev1, ev2 = eigenvectors(*abcd)
            pr['ev1'] = ev1
            if ev2 != None:
                pr['ev2'] = ev2
        r.append(pr)
    return k_img, r

# k_img, ps = get_points(*tasks[0])

# # \u0412\u044b\u0432\u043e\u0434 \u0434\u0430\u043d\u043d\u044b\u0445 \u043f\u043e \u0437\u0430\u0434\u0430\u0447\u0430\u043c
# for t in tasks:
#     k_img, ps = get_points(*t)
#     print '"{0}", "{1}"'.format(*t)
#     print '>> special points: ', len(ps)
#     ll = sympy.latex
#     for p in ps:
#         # print
#         print '>out>', '$$({0}, {1})$$'.format(ll(p['x']), ll(p['y']))
#         print '>out>', '$$\\lambda_1={0}, \\lambda_2={1}$$'.format(ll(p['l1']), ll(p['l2']))
#         print '>type>', p['type']
#         print '>stability>', p['stability']

def latexify_point(d):
    for k in 'l1', 'l2', 'x', 'y':
        d[k + '_tex'] = sympy.latex(d[k])
    for k in 'ab', 'cd':
        d[k + '_tex'] = sympy.latex(d[k])
        d[k + '_code'] = sympy.jscode(d[k].subs({ u : x, v : y }))
        d[k] = ''
    for k in 'l1', 'l2':
        d[k] = ''
    for k in 'x', 'y':
        d[k] = float(d[k])
    for k in 'ev1', 'ev2':
        if k in d:
            d[k + '_tex'] = map(sympy.latex, d[k])
            d[k] = map(float, d[k])

def task_to_json(dotx, doty):
    # %% rename; it is not really _to_json, just returns dictionary
    k_img, ps = get_points(dotx, doty)
    print type(dotx)
    print from_string(dotx)
    # dot_x = sympy.simplify(dotx)
    # dot_y = sympy.simplify(doty)
    for p in ps:
        latexify_point(p)
    d = {
        'dot_x_code' : sympy.jscode(from_string(dotx)),
        'dot_y_code' : sympy.jscode(from_string(doty)),
        'dot_x_tex' : sympy.latex(sympy.simplify(dotx)),
        'dot_y_tex' : sympy.latex(sympy.simplify(doty)),
        'k_img' : k_img,
        'k_real' : len(ps),
        'points' : ps
    }
    # return json.dumps(d)
    return d

# ######################################################################
# html part

js_template = ur'''
function $name(
    ctx, direction, x, y, colored,
    color, color1, color2,
    max_length, height,
    min_x, mid_x, max_x,
    min_y, mid_y, max_y,
    height_dif_x, line_width)
{
    var t = 1.0;
    var p = 2;
    /*ctx.fillStyle = color;*/
    ctx.strokeStyle = color;
    ctx.beginPath();
    ctx.lineWidth = line_width;
    ctx.moveTo(x, mid_y - (y - mid_y));
    while (max_length-- > 0 && min_x < x && x < max_x && min_y < y && y < max_y && t > 0) {
        /*ctx.fillRect(x, mid_y - (y - mid_y), line_width, line_width * 2);*/
        ctx.lineTo(x, mid_y - (y - mid_y));
        var vx = $dot_x ;
        var vy = $dot_y ;
        if (colored) {
            var n = (mid_x - x) * vx + (mid_y - y) * vy > 0;
            if (n != p) {
                ctx.stroke();
                ctx.beginPath();
                ctx.moveTo(x, mid_y - (y - mid_y));
                ctx.strokeStyle = (n ? color1 : color2);
                /*ctx.fillStyle = (n ? color1 : color2);*/
                p = n;
            }
        }
        t = Math.sqrt(vx * vx + vy * vy) * height_dif_x;
        if (t > 0.0) {
            x += direction * (vx / t);
            y += direction * (vy / t);
        }
    }
    ctx.stroke();
}
'''
js_template = string.Template(js_template)

js_common = ur'''
function phapl_setup_canvases()
{
    var canvas_properties = {};
    var all_points = phapl_all_points;

    // draw phase planes
    for (j in phapl_canvases) {
        var cid = phapl_canvases[j][0];
        var hl_fun = phapl_canvases[j][1];
        var points = phapl_canvases[j][2];
        var c = document.getElementById(cid);
        var ctx = c.getContext('2d');
        ctx.lineJoin = 'bevel';

        var black_color = '#000000';
        var red_color = '#ff0000';
        var blue_color = '#0000ff';
        var fg_color = '#8b008b';
        var fg2_color = '#008000';

        var height = c.height;
        var single = (points.length == 1);

        // Calculate borders
        var pad = 1.0000001;
        // var pad = 1.0;
        // var pad = 2.0;
        var min_x = points[0][0];
        var max_x = points[0][0];
        var min_y = points[0][1];
        var max_y = points[0][1];
        for (i in points) {
            var x = points[i][0];
            var y = points[i][1];
            if (x > max_x) {
                max_x = x;
            }
            if (y > max_y) {
                max_y = y;
            }
            if (x < min_x) {
                min_x = x
            }
            if (y < min_y) {
                min_y = y;
            }
        }
        min_x -= pad;
        min_y -= pad;
        max_x += pad;
        max_y += pad;

        // Expand to square
        // ** After expansion assumption is used: max_x - min_x == max_y - min_y
        var c_width = max_x - min_x;
        var c_height = max_y - min_y;
        var half_dif = Math.abs(c_width - c_height) / 2.0;
        if (c_width > c_height) {
            min_y -= half_dif;
            max_y += half_dif;
        } else {
            min_x -= half_dif;
            max_x += half_dif;
        }
        var mid_x = (min_x + max_x) / 2.0;
        var mid_y = (min_y + max_y) / 2.0;
        var dif_x = max_x - min_x;
        var dif_y = max_y - min_y;
        var height_dif_x = height / dif_x;

        var is_linear_twin = false;
        var nn = cid.replace('_nonlinear_', '_linear_');
        if (nn === cid) {
            nn = cid.replace('_linear_', '_nonlinear_');
            if (nn != cid) {
                is_linear_twin = true;
            }
        }
        if (nn === cid) {
            nn = false;
        }

        if (is_linear_twin) {
            all_points = points;
        } else {
            all_points = phapl_all_points;
        }

        canvas_properties[cid] = {
            // function to draw half line
            'hl_fun' : hl_fun,
            'elem' : c,
            'twin_cid' : nn,
            'ctx' : ctx,
            'mid_x' : mid_x,
            'mid_y' : mid_y,
            'min_x' : min_x,
            'min_y' : min_y,
            'max_x' : max_x,
            'max_y' : max_y,
            'dif_x' : dif_x,
            'dif_y' : dif_y,
            'height_dif_x' : height_dif_x
        };

        var line_width = 1.0 * dif_x / height;


        // console.log(j, min_x, mid_x, max_x);
        // console.log(j, min_y, mid_y, max_y);

        // ctx.fillStyle = '#ffffff';
        // ctx.fillStyle = '#ff0000';
        // ctx.fillRect(0, 0, height, height);

        // Scale and translate
        ctx.scale(height / dif_x, height / dif_y);
        ctx.translate(-min_x, -min_y);

        // ctx.fillStyle = '#ffffff';
        // ctx.fillRect(min_x, min_y, dif_x, dif_y);

        var invert = function (y) { return mid_y - (y - mid_y); };
        // var invert = function (y) { return y; };

        for (j in all_points) {
            var px = all_points[j][0];
            var py = all_points[j][1];
            // Circles around special points
            ctx.strokeStyle = '#ff0000';
            ctx.lineWidth = line_width;
            ctx.beginPath();
            ctx.arc(px, invert(py), 10.0 * dif_x / height, 0, 2 * Math.PI);
            ctx.stroke();
        }


        var f = function (x, y) {

            // console.log(x, y);

            // ctx.fillStyle = '#ff0000';
            // ctx.fillRect(x, mid_y - (y - mid_y), 0.1, 0.1);

            var f2 = function (x, y, direction) {
                hl_fun(ctx, direction, x, y, single,
                       black_color, fg_color, fg2_color,
                       // %% height * 5, height,
                       height * 2, height,
                       min_x, mid_x, max_x,
                       min_y, mid_y, max_y,
                       height_dif_x, line_width / 2
                      );
            };
            f2(x, y, -1);
            f2(x, y, 1);
        };

        // Initial lines
        for (j in points) {
            var px = points[j][0];
            var py = points[j][1];

            // ctx.strokeStyle = '#ff0000';
            // ctx.lineWidth = line_width / 2;
            // ctx.strokeRect(px - 1, invert(py) - 1, 2, 2);

            var ji;

            // var parts = Math.floor(30 / points.length);
            // // var parts = 30;
            // for (ji = 0; ji < parts + 1; ji++) {
            //     f(px - 1 + ji * 2.0 / parts, py - 1);
            //     f(px - 1 + ji * 2.0 / parts, py + 1);
            //     f(px - 1, py - 1 + ji * 2.0 / parts);
            //     f(px + 1, py - 1 + ji * 2.0 / parts);
            // }

        }

        // for (ji = 0; ji < 40 + 1; ji++) {
        //     f(Math.random() * dif_x + min_x, Math.random() * dif_y + min_y);
        // }

        var w = dif_x / 10.0;
        for (var tx = min_x; tx <= max_x; tx += w) {
            for (var ty = min_y; ty <= max_y; ty += w) {
                f(tx + Math.random() * w, ty + Math.random() * w);
            }
        }

        var shift = 10.0 * dif_x / height;
        for (j in all_points) {
            var px = all_points[j][0];
            var py = all_points[j][1];
            f(px - shift, py);
            f(px, py - shift);
            f(px + shift, py);
            f(px, py + shift);
        }

        // Grid
        ctx.lineWidth = line_width;
        var line = function (ctx, x1, y1, x2, y2) {
            ctx.beginPath();
            ctx.moveTo(x1, y1);
            ctx.lineTo(x2, y2);
            ctx.stroke();
        };
        for (i = Math.floor(min_x) - 1; i < Math.floor(max_x) + 1; i += 1.0) {
            // %% \u0441\u0442\u043e\u0438\u0442 \u043f\u0440\u043e\u0432\u043e\u0434\u0438\u0442\u044c \u043e\u0441\u0438 \u043e\u0442\u0434\u0435\u043b\u044c\u043d\u043e
            ctx.strokeStyle = (i == 0.0 ? 'violet' : 'lightgrey');
            line(ctx, i, invert(min_y - 1), i, invert(max_y + 1));
        }
        for (i = Math.floor(min_y) - 1; i < Math.floor(max_y) + 1; i += 1.0) {
            // %% \u0441\u0442\u043e\u0438\u0442 \u043f\u0440\u043e\u0432\u043e\u0434\u0438\u0442\u044c \u043e\u0441\u0438 \u043e\u0442\u0434\u0435\u043b\u044c\u043d\u043e
            ctx.strokeStyle = (i == 0.0 ? 'violet' : 'lightgrey');
            line(ctx, min_x - 1, invert(i), max_x + 1, invert(i));
        }

        // Triangles to mark axis and special points
        var draw_triangle = function (x, y, color, orientation, size) {
            ctx.fillStyle = color;
            ctx.beginPath();
            if (orientation) {
                // vertical, like A
                ctx.moveTo(x - size / 2, invert(y));
                ctx.lineTo(x, invert(y + size));
                ctx.lineTo(x + size / 2, invert(y));
            } else {
                // horizontal, like |>
                ctx.moveTo(x, invert(y - size / 2));
                ctx.lineTo(x + size, invert(y));
                ctx.lineTo(x, invert(y + size / 2));
            }
            ctx.fill();
        };
        var axis_size = 15.0 * dif_x / height;
        draw_triangle(0.0, min_y, black_color, true, axis_size);
        draw_triangle(min_x, 0.0, black_color, false, axis_size);
        var point_size = 10.0 * dif_x / height;
        for (j in all_points) {
            var px = all_points[j][0];
            var py = all_points[j][1];
            draw_triangle(px, min_y, red_color, true, point_size);
            draw_triangle(min_x, py, red_color, false, point_size);
        }

        // Eigen vectors
        for (j in all_points) {
            if (all_points[j].length > 2) {
                var px = all_points[j][0];
                var py = all_points[j][1];
                var vv = [ 0 ]
                if (all_points[j].length > 4) {
                    vv.push(1);
                }
                for (var jj in vv) {
                    var vx = all_points[j][2 + 2 * jj];
                    var vy = all_points[j][3 + 2 * jj];
                    // normalize to 0.5 in length
                    var m = Math.sqrt(0.25 / (vx * vx + vy * vy));
                    vx *= m;
                    vy *= m;
                    ctx.strokeStyle = 'cyan';
                    line(ctx, px - vx, invert(py - vy), px + vx, invert(py + vy));
                }
            }
        }

        canvas_properties[cid].saved_canvas = ctx.getImageData(0, 0, c.width, c.height);

        // Actions for mouse
        // %% lift functions, don't create every time; is it important at all?
        c.addEventListener('mouseout', function (e) {
            var c = e.srcElement;
            if (!c) {
                c = e.originalTarget;
            }
            var d = canvas_properties[c.id];
            d.ctx.putImageData(d.saved_canvas, 0, 0);
            var nn = d.twin_cid;
            if (nn != false) {
                var d2 = canvas_properties[nn];
                d2.ctx.putImageData(d2.saved_canvas, 0, 0);
            }
        });
        c.onmousemove = (function (e) {
            var c = e.srcElement;
            if (!c) {
                c = e.originalTarget;
            }
            var d = canvas_properties[c.id];
            var rect = c.getBoundingClientRect();
            var mx = e.clientX - rect.left;
            var my = e.clientY - rect.top;
            mx = mx / rect.width * d.dif_x + d.min_x;
            my = d.mid_y - ((my / rect.height * d.dif_y + d.min_y) - d.mid_y);
            d.ctx.putImageData(d.saved_canvas, 0, 0);
            d.hl_fun(d.ctx, 1.0, mx, my, false,
                     red_color, fg_color, fg2_color,
                     // %% c.height * 3, c.height,
                     c.height * 1.5, c.height,
                     d.min_x, d.mid_x, d.max_x,
                     d.min_y, d.mid_y, d.max_y,
                     d.height_dif_x, line_width * 4
                    );
            d.hl_fun(d.ctx, -1.0, mx, my, false,
                     blue_color, fg_color, fg2_color,
                     // %% c.height * 3, c.height,
                     c.height * 1.5, c.height,
                     d.min_x, d.mid_x, d.max_x,
                     d.min_y, d.mid_y, d.max_y,
                     d.height_dif_x, line_width * 4
                    );
            var nn = d.twin_cid;
            if (nn != false) {
                var d2 = canvas_properties[nn];
                mx = e.clientX - rect.left;
                my = e.clientY - rect.top;
                mx = mx / rect.width * d2.dif_x + d2.min_x;
                my = d2.mid_y - ((my / rect.height * d2.dif_y + d2.min_y) - d2.mid_y);
                d2.ctx.putImageData(d2.saved_canvas, 0, 0);
                d2.hl_fun(d2.ctx, 1.0, mx, my, false,
                          red_color, fg_color, fg2_color,
                          // %% see similar above
                          d2.elem.height * 1.5, d2.elem.height,
                          d2.min_x, d2.mid_x, d2.max_x,
                          d2.min_y, d2.mid_y, d2.max_y,
                          d2.height_dif_x, line_width * 4
                         );
                d2.hl_fun(d2.ctx, -1.0, mx, my, false,
                          blue_color, fg_color, fg2_color,
                          // %% see similar above
                          d2.elem.height * 1.5, d2.elem.height,
                          d2.min_x, d2.mid_x, d2.max_x,
                          d2.min_y, d2.mid_y, d2.max_y,
                          d2.height_dif_x, line_width * 4
                         );
            }
        });
    }
}
'''

def solution_word(i, b):
    # %% remove b
    if i == 1 or (i >= 20 and i % 10 == 1):
        r = u'\u043e\u0435 \u0440\u0435\u0448\u0435\u043d\u0438\u0435'
        if b:
            r += u' (\u043f\u0440\u043e\u0438\u0433\u043d\u043e\u0440\u0438\u0440\u043e\u0432\u0430\u043d\u043e)'
        return r
    if 2 <= i <= 4 or (i >= 20 and 2 <= i % 10 <= 4):
        r = u'\u044b\u0445 \u0440\u0435\u0448\u0435\u043d\u0438\u044f'
    elif i == 0 or i >= 5:
        r = u'\u044b\u0445 \u0440\u0435\u0448\u0435\u043d\u0438\u0439'
    else:
        assert 0, 'not implemented'
    if b:
        r += u' (\u043f\u0440\u043e\u0438\u0433\u043d\u043e\u0440\u0438\u0440\u043e\u0432\u0430\u043d\u044b)'
    return r

# # original style with table for special points
# def make_html1(str_args):
#     d = json.loads(str_args)
#     s = ''
#     canvases = []
#     s += u'<h2>\u0418\u0441\u0441\u043b\u0435\u0434\u0443\u0435\u043c\u0430\u044f \u0441\u0438\u0441\u0442\u0435\u043c\u0430</h2>'
#     s += (r'<p>$$ \left\{ \begin{aligned}'
#           + r'\dot{x} &= ' + d['dot_x_tex'] + r' \\'
#           + r'\dot{y} &= ' + d['dot_y_tex'] + r' \\'
#           + r'\end{aligned}\right. $$</p>')
#     s += u'<h2>\u041e\u0441\u043e\u0431\u044b\u0435 \u0442\u043e\u0447\u043a\u0438</h2>'
#     s += (u'\u041f\u043e\u0438\u0441\u043a \u043e\u0441\u043e\u0431\u044b\u0445 \u0442\u043e\u0447\u0435\u043a \u043d\u0430\u0448\u0451\u043b {} \u0434\u0435\u0439\u0441\u0442\u0432\u0438\u0442\u0435\u043b\u044c\u043d{}.'
#           + u' \u0422\u0430\u043a\u0436\u0435 \u0431\u044b\u043b\u043e \u043d\u0430\u0439\u0434\u0435\u043d\u043e {} \u043a\u043e\u043c\u043f\u043b\u0435\u043a\u0441\u043d{}.').format(
#               d['k_real'], solution_word(d['k_real'], False),
#               d['k_img'], solution_word(d['k_img'], True))
#     s += '<table border="1" style="border-collapse: collapse;">'
#     headers = [
#         u"\u2116",
#         u"\u041a\u043e\u043e\u0440\u0434\u0438\u043d\u0430\u0442\u044b",
#         u"\u041a\u043e\u0440\u043d\u0438<br>\u0445\u0430\u0440\u0430\u043a\u0442\u0435\u0440\u0438\u0441\u0442\u0438\u0447\u0435\u0441\u043a\u043e\u0433\u043e<br>\u0443\u0440\u0430\u0432\u043d\u0435\u043d\u0438\u044f",
#         u"\u0422\u043e\u0447\u043a\u0430 \u043f\u043e\u043a\u043e\u044f",
#         u"\u0424\u0430\u0437\u043e\u0432\u044b\u0439 \u043f\u043e\u0440\u0442\u0440\u0435\u0442<br>\u0432 \u043e\u043a\u0440\u0435\u0441\u0442\u043d\u043e\u0441\u0442\u0438 \u0442\u043e\u0447\u043a\u0438",
#         u"\u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u043e\u0441\u0442\u044c<br>(\u043d\u0435\u0443\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u043e\u0441\u0442\u044c)<br>\u0442\u0440\u0438\u0432\u0438\u0430\u043b\u044c\u043d\u043e\u0433\u043e<br>\u0440\u0435\u0448\u0435\u043d\u0438\u044f"]
#     s += '<tr>'
#     for i in headers:
#         s += u'<th>{}</th>'.format(i)
#     s += '</tr>'
#     def td(s):
#         return u'<td style="padding: 5px;">{}</td>'.format(s)
#     all_points = []
#     for i, p in enumerate(d['points']):
#         s += '<tr>'
#         s += td(i + 1)
#         s += td(r'$$ \left( {}, {} \right) $$'.format(p['x_tex'], p['y_tex']))
#         s += td(r'$$ \lambda_1 = {}, \lambda_2 = {} $$'.format(
#             p['l1_tex'], p['l2_tex']))
#         s += td(p['type'])
#         cid = 'canvas' + str(i)
#         s += td('<canvas id="{}" width="300" height="300"></canvas>'.format(cid))
#         canvases.append([ cid, [ [ p['x'], p['y'] ] ] ])
#         all_points.append([ p['x'], p['y'] ])
#         s += td(p['stability'])
#         s += '</tr>'
#     s += '</table>'
#     s += u'<h2>\u041e\u0431\u0449\u0438\u0439 \u0444\u0430\u0437\u043e\u0432\u044b\u0439 \u043f\u043e\u0440\u0442\u0440\u0435\u0442</h2>'
#     s += '<canvas id="canvas_all" width="600" height="600" style="border: 1px solid black;"></canvas>'
#     if not all_points:
#         all_points.append([ 0, 0 ])
#     canvases.append([ 'canvas_all', all_points ])
#     # Some padding at the bottom
#     s += '<br><br><br><br><br><br><br><br>'
#     s += '<script type="text/javascript">'
#     s += 'phapl_canvases = ' + str(canvases) + ';'
#     s += 'phapl_all_points = ' + str(all_points) + ';'
#     s += js_template.safe_substitute(
#         dot_x = d['dot_x_code'],
#         dot_y = d['dot_y_code'])
#     s += 'phapl_setup_canvases();'
#     s += '</script>'
#     return s

# new style with linear layout
def make_html(d):
    # d = json.loads(str_args)
    s = ''
    canvases = []
    # s += u'<style> body { font-size: 140%; } </style>'
    s += u'<h2>\u0418\u0441\u0441\u043b\u0435\u0434\u0443\u0435\u043c\u0430\u044f \u0441\u0438\u0441\u0442\u0435\u043c\u0430</h2>'
    s += (r'<p>$$ \left\{ \begin{aligned}'
          + r'\dot{x} &= ' + d['dot_x_tex'] + r' \\'
          + r'\dot{y} &= ' + d['dot_y_tex']
          + r' \end{aligned}\right. $$</p>')
    s += u'<h2>\u041e\u0441\u043e\u0431\u044b\u0435 \u0442\u043e\u0447\u043a\u0438</h2>'
    s += (r'<p>$$ \left\{ \begin{aligned}'
          + d['dot_x_tex'] + r' = 0 \\'
          + d['dot_y_tex'] + r' = 0'
          + r' \end{aligned}\right. $$</p>')
    if d['k_real'] == 0:
        s += u'\u041d\u0430\u0439\u0434\u0435\u043d\u043e <b>0</b> \u0434\u0435\u0439\u0441\u0442\u0432\u0438\u0442\u0435\u043b\u044c\u043d\u044b\u0439 \u0440\u0435\u0448\u0435\u043d\u0438\u0439: \u043e\u0441\u043e\u0431\u044b\u0435 \u0442\u043e\u0447\u043a\u0438 \u043d\u0435 \u043d\u0430\u0439\u0434\u0435\u043d\u044b.'
    else:
        s += u'\u041d\u0430\u0439\u0434\u0435\u043d\u043e <b>{}</b> \u0434\u0435\u0439\u0441\u0442\u0432\u0438\u0442\u0435\u043b\u044c\u043d{}: \( {} \).'.format(
            d['k_real'],
            solution_word(d['k_real'], False),
            ur',\  '.join(ur'\left( {}, {} \right)'.format(
                              p['x_tex'], p['y_tex'])
                          for p in d['points']))
    s += '<br>'
    s += ur'\u041d\u0430\u0439\u0434\u0435\u043d\u043e \u0438 \u043f\u0440\u043e\u0438\u0433\u043d\u043e\u0440\u0438\u0440\u043e\u0432\u0430\u043d\u043e <b>{}</b> \u043a\u043e\u043c\u043f\u043b\u0435\u043a\u0441\u043d{}.'.format(
        d['k_img'], solution_word(d['k_img'], False))
    all_points = []
    hl_funcs = [
        ('phapl_draw_half_line_original', d['dot_x_code'], d['dot_y_code'])
    ]
    hl_funcs_k = 0
    for i, p in enumerate(d['points']):
        s += '<hr>'
        # %% use <li> ?
        s += u'<b>{}.</b> \u0422\u043e\u0447\u043a\u0430 '.format(i + 1)
        s += r'\( \left( {}, {} \right) \).'.format(
            p['x_tex'], p['y_tex'])
        s += '<br>'
        if not p['is_linear']:
            s += u'\u0421\u043e\u043e\u0442\u0432\u0435\u0442\u0441\u0442\u0432\u0443\u044e\u0449\u0430\u044f \u043b\u0438\u043d\u0435\u0439\u043d\u0430\u044f \u0441\u0438\u0441\u0442\u0435\u043c\u0430 \u043f\u043e\u0441\u043b\u0435 \u043f\u0435\u0440\u0435\u043d\u043e\u0441\u0430 \u043a\u043e\u043e\u0440\u0434\u0438\u043d\u0430\u0442:'
            s += (r'<p>$$ \left\{ \begin{aligned}'
                  + r'\dot{u} &= ' + p['ab_tex'] + r' \\'
                  + r'\dot{v} &= ' + p['cd_tex']
                  + r' \end{aligned}\right. $$</p>')
        s += u'\u041a\u043e\u0440\u043d\u0438 \u0445\u0430\u0440\u0430\u043a\u0442\u0435\u0440\u0438\u0441\u0442\u0438\u0447\u0435\u0441\u043a\u043e\u0433\u043e \u0443\u0440\u0430\u0432\u043d\u0435\u043d\u0438\u044f:'
        s += r'$$ \lambda_1 = {}, \\ \lambda_2 = {}. $$'.format(
            p['l1_tex'], p['l2_tex'])
        s += u'\u0422\u0438\u043f \u043e\u0441\u043e\u0431\u043e\u0439 \u0442\u043e\u0447\u043a\u0438: '
        if not p['is_linear'] and p['type'] == u'\u0426\u0435\u043d\u0442\u0440':
            s += u'<b>' + p['type'] + u'</b> \u0432 \u043b\u0438\u043d\u0435\u0439\u043d\u043e\u0439 \u0441\u0438\u0441\u0442\u0435\u043c\u0435. \u0412 \u043e\u0440\u0438\u0433\u0438\u043d\u0430\u043b\u044c\u043d\u043e\u0439 \u0441\u0438\u0441\u0442\u0435\u043c\u0435 \u0442\u043e\u0447\u043a\u0430 \u2014 \u0446\u0435\u043d\u0442\u0440 \u0438\u043b\u0438 \u0444\u043e\u043a\u0443\u0441. <b>\u0422\u0440\u0435\u0431\u0443\u044e\u0442\u0441\u044f \u0434\u043e\u043f\u043e\u043b\u043d\u0438\u0442\u0435\u043b\u044c\u043d\u044b\u0435 \u0438\u0441\u0441\u043b\u0435\u0434\u043e\u0432\u0430\u043d\u0438\u044f</b>.'
            s += '<br>'
            s += u'\u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u043e\u0441\u0442\u044c: <b>\u0442\u0440\u0435\u0431\u0443\u044e\u0442\u0441\u044f \u0434\u043e\u043f\u043e\u043b\u043d\u0438\u0442\u0435\u043b\u044c\u043d\u044b\u0435 \u0438\u0441\u0441\u043b\u0435\u0434\u043e\u0432\u0430\u043d\u0438\u044f</b>.'
        else:
            s += u'<b>' + p['type'] + u'</b>.'
            s += '<br>'
            s += u'\u0423\u0441\u0442\u043e\u0439\u0447\u0438\u0432\u043e\u0441\u0442\u044c: '
            s += u'<b>' + p['stability'] + u'</b>.'
        s += '<br>'
        pd = [ p['x'], p['y'] ]
        if 'ev1' in p and 'ev2' in p:
            pd += p['ev1'] + p['ev2']
            s += u'\u041f\u0430\u0440\u0430 \u0441\u043e\u0431\u0441\u0442\u0432\u0435\u043d\u043d\u044b\u0445 \u0432\u0435\u043a\u0442\u043e\u0440\u043e\u0432: '
            s += (ur'\[ \vec{\xi_1} = \left( \begin{array}{c} '
                  + p['ev1_tex'][0] + ur' \\ ' + p['ev1_tex'][1]
                  + ur' \end{array} \right),\ '
                  + ur' \vec{\xi_2} = \left( \begin{array}{c} '
                  + p['ev2_tex'][0] + ur' \\ ' + p['ev2_tex'][1]
                  + ur' \end{array} \right). \]')
        elif 'ev1' in p:
            pd += p['ev1']
            s += u'\u0421\u043e\u0431\u0441\u0442\u0432\u0435\u043d\u043d\u044b\u0439 \u0432\u0435\u043a\u0442\u043e\u0440: '
            s += (ur'\[ \vec{\xi_1} = \left( \begin{array}{c} '
                  + p['ev1_tex'][0] + ur' \\ ' + p['ev1_tex'][1]
                  + ur' \end{array} \right). \]')
        if p['is_linear']:
            s += u'\u0424\u0430\u0437\u043e\u0432\u044b\u0439 \u043f\u043e\u0440\u0442\u0440\u0435\u0442 \u0432 \u043e\u043a\u0440\u0435\u0441\u0442\u043d\u043e\u0441\u0442\u0438 \u0442\u043e\u0447\u043a\u0438:<br>'
            cid = 'phapl_canvas_only_' + str(i)
            s += '<canvas id="{}" width="300" height="300" style="border: 1px solid black;"></canvas>'.format(cid)
            canvases.append([ cid, hl_funcs[0][0], [ pd ] ])
        else:
            s += u'\u0424\u0430\u0437\u043e\u0432\u044b\u0435 \u043f\u043e\u0440\u0442\u0440\u0435\u0442\u044b \u043b\u0438\u043d\u0435\u0439\u043d\u043e\u0439 \u0438 \u043e\u0440\u0438\u0433\u0438\u043d\u0430\u043b\u044c\u043d\u043e\u0439 \u0441\u0438\u0441\u0442\u0435\u043c \u0432 \u043e\u043a\u0440\u0435\u0441\u0442\u043d\u043e\u0441\u0442\u0438 \u0442\u043e\u0447\u043a\u0438:<br>'
            cid = 'phapl_canvas_linear_' + str(i)
            s += '<canvas id="{}" width="300" height="300" style="border: 1px solid black;"></canvas>'.format(cid)
            hl_funcs_k += 1
            fn = 'phapl_draw_half_line_' + str(hl_funcs_k)
            hl_funcs.append( (fn, p['ab_code'], p['cd_code']) )
            canvases.append([ cid, fn, [ [ 0, 0 ] + pd[2 : ] ] ])
            s += '<span width=10px>&nbsp;</span>'
            cid = 'phapl_canvas_nonlinear_' + str(i)
            s += '<canvas id="{}" width="300" height="300" style="border: 1px solid black;"></canvas>'.format(cid)
            canvases.append([ cid, hl_funcs[0][0], [ pd ] ])
        all_points.append(pd)
    s += '<hr>'
    s += u'<h2>\u041e\u0431\u0449\u0438\u0439 \u0444\u0430\u0437\u043e\u0432\u044b\u0439 \u043f\u043e\u0440\u0442\u0440\u0435\u0442</h2>'
    s += '<canvas id="phapl_canvas_all" width="600" height="600" style="border: 1px solid black;"></canvas>'
    if not all_points:
        all_points.append([ 0, 0 ])
    canvases.append([ 'phapl_canvas_all', hl_funcs[0][0], all_points ])
    # Some padding at the bottom
    s += '<br><br><br><br><br><br><br><br>'
    # s += '<script type="text/javascript">'
    js = u''
    js += 'phapl_all_points = ' + str(all_points) + ';'
    for name, dx_code, dy_code in hl_funcs:
        js += js_template.safe_substitute(
            name = name,
            dot_x = dx_code,
            dot_y = dy_code)
    js += js_common
    js += ('phapl_canvases = ['
           + ', '.join(map("['{0[0]}', {0[1]}, {0[2]}]".format, canvases))
           + '];\n')
    js += 'phapl_setup_canvases();'
    # print js
    # s += '</script>'
    return [ s, js ]

def task_to_html(dotx, doty):
    j = task_to_json(dotx, doty)
    # print j
    r = make_html(j)
    return r

# from urllib import quote
# print '<table border="1" style="border-collapse: collapse;">'
# for i, t in enumerate(tasks):
#     if i % 3 == 0:
#         print '<tr>'
#     print '<td>{2}) <a href="phapl.html#{1}"><img src="{0}"></a></td>'.format(hex_name(*t), quote(task_to_json(*t)), i + 1)
#     if i % 3 == 2:
#         print '</tr>'
# print '</table>'
