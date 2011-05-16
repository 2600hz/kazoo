from erlport import Port, Protocol, String, Atom
from erlport.erlterms import decode
from django.template import Context, Template
from django.conf import settings
import types

# Inherit custom protocol from erlport.Protocol
class ErlydtlProtocol(Protocol):
    
    def handle_slice(self, command):
        list = command[0]
        slice = "%s" % String(command[1])
        slice_test1_string = "%s[%s]" % (list,slice)
        try:
            result_list = eval(slice_test1_string)
        except(IndexError):
            result_list = Atom("indexError")
        except:
            result_list = Atom("error")
        #print "result_list: %s" % (result_list)
        return result_list
    
    #@doc Start list with 'object' to pass in a python term along with import statement in the form:
    #@doc object|module to import|term (three strings delimited by "|"
    def handle_template(self, command):
        file = open("/tmp/debug.txt",'a')
        template_text = "%s" % String(command[0])
        #value = "%s" % String(command[1])
        value = "%s" % String(command[1])
        value_split = value.split("|")
        
        if value_split[0] == u"object":
            module = value_split[1]
            exec "import %s" % module
            value = eval(value_split[2])
            #term(((term.year, term.month, term.day), (term.hour, term.minute, term.second)))
        c = Context({"value":value})
        t = Template(template_text)
        result = String(t.render(c))
        file.close()
        return result


if __name__ == "__main__":
    settings.configure(DEBUG=True, TEMPLATE_DEBUG=True)
    proto = ErlydtlProtocol()
    # Run protocol with port open on STDIO
    proto.run(Port(use_stdio=True))
    
