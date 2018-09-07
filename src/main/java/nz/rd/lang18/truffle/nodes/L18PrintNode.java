package nz.rd.lang18.truffle.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;

public class L18PrintNode extends L18ExpressionNode {

    @Child private L18ExpressionNode child;

    public L18PrintNode(L18ExpressionNode child) {
        this.child = child;
    }

    @Override
    public Object execute(VirtualFrame frame) {
        Object childResult = child.execute(frame);
        System.out.println(""+childResult);
        return "<null>";
    }
}
