# --------------------------------------------------------------------
import contextlib as cl
import typing as tp

# ====================================================================
class Scope:
    def __init__(self):
        self.vars = [dict()]
        self.unhandled_exceptions = [dict()]

    def open(self):
        self.vars.append(dict())
        self.unhandled_exceptions.append(dict())

    def close(self):
        assert(len(self.vars) > 0)
        self.vars.pop()
        # Move unhandled exceptions to the parent scope
        self.unhandled_exceptions[-2].update(self.unhandled_exceptions[-1])
        self.unhandled_exceptions.pop()

    def push(self, name: str, data: tp.Any):
        assert(name not in self.vars[-1])
        self.vars[-1][name] = data

    def push_exception(self, name: str):
        assert(name not in self.unhandled_exceptions[-1])
        self.unhandled_exceptions[-1][name] = True

    def pop_exception(self, name: str):
        assert(name in self.unhandled_exceptions[-1])
        del self.unhandled_exceptions[-1][name]

    def islocal(self, name: str):
        return name in self.vars[-1]

    def __getitem__(self, name: str):
        for s in self.vars[::-1]:
            if name in s:
                return s[name]
        assert(False)

    def __contains__(self, name: str):
        return any(name in s for s in self.vars)

    @cl.contextmanager
    def in_subscope(self):
        self.open()
        try:
            yield self
        finally:
            self.close()
