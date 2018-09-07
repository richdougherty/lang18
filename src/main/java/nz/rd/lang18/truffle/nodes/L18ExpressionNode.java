package nz.rd.lang18.truffle.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;

abstract public class L18ExpressionNode extends Node {

    abstract public Object execute(VirtualFrame frame);

}
