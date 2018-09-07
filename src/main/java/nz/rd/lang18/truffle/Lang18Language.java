package nz.rd.lang18.truffle;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.debug.DebuggerTags;
import com.oracle.truffle.api.instrumentation.ProvidedTags;
import com.oracle.truffle.api.instrumentation.StandardTags;
import nz.rd.lang18.truffle.nodes.L18RootNode;

@TruffleLanguage.Registration(id = Lang18Language.ID, name = "lang18", mimeType = Lang18Language.MIME_TYPE, contextPolicy = TruffleLanguage.ContextPolicy.SHARED)
@ProvidedTags({StandardTags.CallTag.class, StandardTags.StatementTag.class, StandardTags.RootTag.class, StandardTags.ExpressionTag.class, DebuggerTags.AlwaysHalt.class})
public class Lang18Language extends TruffleLanguage<Object> {

    public static final String ID = "lang18";
    public static final String MIME_TYPE = "application/x-lang18";

    public Lang18Language() {}

    @Override
    protected CallTarget parse(ParsingRequest request) throws Exception {
        L18RootNode rootNode = TruffleHelpers.parse(this, request.getSource());
        CallTarget callTarget = Truffle.getRuntime().createCallTarget(rootNode);
        return callTarget;
    }

    @Override
    protected Object createContext(Env env) {
        return null;
    }

    @Override
    protected boolean isObjectOfLanguage(Object object) {
        return false;
    }

}
