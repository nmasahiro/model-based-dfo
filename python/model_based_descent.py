import numpy as np

class Sphere(object):
    def __init__(self, n, name):
        self.n = n
        self.name = name

    def evaluate(self, x):
        return np.sum(x**2)

def get_simplex_grad(y0, f0, Delta, f):
    Y = [y0]
    n = y0.shape[0]
    Y.extend([ np.array([np.random.rand()* 2 - 1 for i in range(n)]).reshape(n, 1) * Delta + y0 for i in range(1, n+1)])
    L = [Y[i] - Y[0] for i in range(1, n+1)]
    L = np.concatenate(L, axis=1)
    inv_L_T = np.linalg.inv(L.T)
    delta_fY = np.array([f.evaluate(Y[i]) - f0 for i in range(1, n+1)]).reshape(n, 1)
    alpha = np.dot(inv_L_T, delta_fY)
    return alpha

# forward-backward-tracking line search
def line_search(f, x, d, g, eta):
    success_flag = True

    # 0. Initialise:
    tau = 1.
    N_max = 1000
    ip_d_g = np.dot(d.T, g)[0][0]
    # 1. Forward-backward search:
    fx = f.evaluate(x)
    if f.evaluate(x + tau * d) < fx + eta * tau * ip_d_g:
        success_flag = True
        tau = 1.
        while tau <= pow(2, N_max) and f.evaluate(x + tau * d) < fx + eta * tau * ip_d_g:
            tau = 2 * tau
        tau = tau / 2.
    else:
        success_flag = False
        tau = 1.0
        while tau >= pow(2, - N_max) and f.evaluate(x + tau * d) >= fx + eta * tau * ip_d_g:
            tau = tau / 2.
        if f.evaluate(x + tau * d) < fx + eta * tau * ip_d_g:
            success_flag = True
    # 2. Output
    if success_flag:
        return success_flag, tau
    else:
        return success_flag, -1.


def main():
    # np.random.seed(100)
    # 0. Initialise
    # initial model accuracy parameter (0, \infty)
    Delta = 1.0
    # initial target accuracy parameter (0, \infty)
    mu = 1.0
    # an Armijo parameter
    eta = 0.05
    # minimum decrease angle parameter
    epsilon_d = 0.0 # not use now (because d = - approx_grad is alwayls descent direction)
    # stopping tolerance
    epsilon_stop = 1e-4
    # iteration counter
    K = 200

    dim = 2
    f = Sphere(dim, "s")
    # init point
    x = np.array([[1.0], [1.0]])

    for k in range(K):
        fx = f.evaluate(x)
        print("fx:{}".format(fx))
        # 1. Model:
        # use Delta^k and a finite number of points
        # to create controllably accurate gradient approximations:
        approx_grad = get_simplex_grad(x, fx, Delta, f)

        # 2. Model accuracy checks:
        if Delta < epsilon_stop and np.linalg.norm(approx_grad) < epsilon_stop:
            # declare algorithm success and stop
            print("k:{}, algorithm success and stop".format(k))
            break
        elif Delta > mu * np.linalg.norm(approx_grad):
            # declare the model inaccurate
            Delta = Delta / 2.
            continue

        # 3. Line search
        flag_line_search = True
        # select descent direction d^k
        d = - approx_grad

        # perform a line-search in the direction d^k to seek t^k with
        flag_line_search, t = line_search(f, x, d, approx_grad, eta)

        # # 4. Update:
        if flag_line_search:
            x = x + t * d
        else:
            mu = mu / 2.

if __name__ == '__main__':
    main()
