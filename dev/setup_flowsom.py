"""Build a FlowSOM dev step: rows = channel, columns = cell, y = value.

    TERCEN_TOKEN=… python dev/setup_flowsom.py <workflowId> <tableSchemaId> [prop=value …]

The projection mirrors the R `flowsom_operator`: channels on rows, cells on columns, the value
on y. No colour and no label — the clustering reads nothing but the matrix.
"""
import os, sys, uuid, json
import tercen.model.impl as m
from tercen.client.factory import TercenClient

tok = os.environ["TERCEN_TOKEN"]
wf_id, schema_id = sys.argv[1], sys.argv[2]
props = dict(a.split("=", 1) for a in sys.argv[3:])
ROW, COL, Y = "channel", "cell_id", "value"
# Extra column factors, e.g. EXTRA_COLS=type:string for the train-mode test.
EXTRA = [tuple(x.split(":")) for x in os.environ.get("EXTRA_COLS", "").split(",") if x]

c = TercenClient(os.environ.get("TERCEN_HTTP", "http://127.0.0.1:5402"))
c.userService.tercenClient.token = tok
c.httpClient.authorization = tok
wf = c.workflowService.get(wf_id)

def rect(x, y, w=200.0, h=55.0):
    r = m.Rectangle(); r.topLeft = m.Point(); r.topLeft.x = x; r.topLeft.y = y
    r.extent = m.Point(); r.extent.x = w; r.extent.y = h; return r
def factor(n, t):
    f = m.Factor(); f.name = n; f.type = t; return f
def gf(n, t):
    g = m.GraphicalFactor(); g.factor = factor(n, t)
    g.rectangle = rect(0.0, 0.0, max(len(n) * 10.0, 30.0), 30.0); return g
def ctable(fs):
    t = m.CrosstabTable(); t.cellSize = 250.0; t.offset = 0; t.nRows = 0
    t.graphicalFactors = fs; t.rectangleSelections = []; return t

ts = m.TableStep(); ts.id = str(uuid.uuid4()); ts.name = "flowsom fixture"
ts.groupId = ""; ts.description = ""
op = m.OutputPort(); op.id = str(uuid.uuid4()); op.name = "table"; op.linkType = "relation"
ts.inputs = []; ts.outputs = [op]; ts.rectangle = rect(100.0, 100.0)
ts.state = m.StepState(); ts.state.taskId = ""; ts.state.taskState = m.DoneState()
rel = m.SimpleRelation(); rel.id = schema_id
ts.model = m.TableStepModel(); ts.model.relation = rel; ts.model.filterSelector = ""

ds = m.DataStep(); ds.id = str(uuid.uuid4()); ds.name = "FlowSOM (Rust, dev)"
ds.groupId = ""; ds.description = ""; ds.parentDataStepId = ""
ip = m.InputPort(); ip.id = str(uuid.uuid4()); ip.name = "data"; ip.linkType = "relation"
dop = m.OutputPort(); dop.id = str(uuid.uuid4()); dop.name = "data"; dop.linkType = "relation"
ds.inputs = [ip]; ds.outputs = [dop]; ds.rectangle = rect(100.0, 250.0)
ds.state = m.StepState(); ds.state.taskId = ""; ds.state.taskState = m.InitState()
ct = m.Crosstab(); ct.taskId = ""
ct.axis = m.XYAxisList(); ct.axis.rectangleSelections = []; ct.axis.xyAxis = []
ct.columnTable = ctable([gf(COL, "double")] + [gf(n, t) for n, t in EXTRA])
ct.rowTable = ctable([gf(ROW, "string")])
ct.filters = m.Filters(); ct.filters.removeNaN = False; ct.filters.namedFilters = []
st = m.OperatorSettings(); st.namespace = "ds0"; st.environment = []
ref = m.OperatorRef(); ref.name = "flowsom_operator"; ref.version = "dev"
ref.operatorId = ""; ref.operatorKind = ""
ref.url = m.Url(); ref.url.uri = "https://github.com/tercen/flowsom_operator"
ref.propertyValues = []
for k, v in props.items():
    pv = m.PropertyValue(); pv.name = k; pv.value = v; ref.propertyValues.append(pv)
ref.operatorSpec = m.OperatorSpec(); ref.operatorSpec.inputSpecs = []; ref.operatorSpec.outputSpecs = []
st.operatorRef = ref
ct.operatorSettings = st
ds.model = ct

link = m.Link(); link.id = str(uuid.uuid4()); link.inputId = ip.id; link.outputId = op.id
wf.steps = list(wf.steps or []) + [ts, ds]
wf.links = list(wf.links or []) + [link]
c.workflowService.update(wf)
print("TABLE_STEP", ts.id); print("DATA_STEP", ds.id)

wf = c.workflowService.get(wf_id)
ds = next(s for s in wf.steps if s.id == ds.id)
q = m.CubeQuery()
q.relation = ts.model.relation
q.colColumns = [factor(COL, "double")] + [factor(n, t) for n, t in EXTRA]
q.rowColumns = [factor(ROW, "string")]
aq = m.CubeAxisQuery(); aq.chartType = "point"; aq.pointSize = 4
aq.xAxis = factor("", "string"); aq.yAxis = factor(Y, "double")
aq.colors = []
aq.labels = []
aq.errors = []; aq.preprocessors = []
aq.xAxisSettings = m.AxisSettings(); aq.xAxisSettings.meta = []
aq.yAxisSettings = m.AxisSettings(); aq.yAxisSettings.meta = []
q.axisQueries = [aq]
q.filters = ds.model.filters
q.operatorSettings = ds.model.operatorSettings
# A workflow created through the client can come back with an empty owner, and the server then
# rejects the task with User.get.not.found — which reaches the client as "failed to decode".
owner = wf.acl.owner or os.environ.get("TERCEN_USER", "admin")
task = m.CubeQueryTask(); task.state = m.InitState(); task.owner = owner
task.projectId = wf.projectId; task.query = q
task = c.taskService.create(task)
c.taskService.runTask(task.id)
task = c.taskService.waitDone(task.id)
print("CUBE_QUERY_TASK", task.id, type(task.state).__name__, getattr(task.state, "reason", ""))
if type(task.state).__name__ != "DoneState":
    sys.exit(1)
axis = json.loads(open(os.path.join(os.path.dirname(__file__), "axis_template.json")).read())
axis["xyAxis"][0]["taskId"] = task.id
axis["xyAxis"][0]["yAxis"]["graphicalFactor"]["factor"]["name"] = Y
wf = c.workflowService.get(wf_id)
ds2 = next(s for s in wf.steps if s.id == ds.id)
ds2.model.taskId = task.id
ds2.model.axis = m.XYAxisList(axis)
c.workflowService.update(wf)
print("READY", wf_id, ds.id, "schema ids", len(task.schemaIds))
