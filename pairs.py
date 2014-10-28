class Edge:
  def __init__(self, u, v, w):
    self.source = u
    self.target = v
    self.capacity = w
 
  def __repr__(self):
    return "{}--{}\\{}".format(self.source, self.target, self.capacity)
 
class Graph:
  def  __init__(self):
    self.adj = {}
    self.flow = {}
 
  def AddVertex(self, vertex):
    self.adj[vertex] = []
 
  def GetEdges(self, v):
    return self.adj[v]
 
  def AddEdge(self, u, v, w = 0):
    if u == v:
      raise ValueError("u == v")
    edge = Edge(u, v, w)
    redge = Edge(v, u, 0)
    edge.redge = redge
    redge.redge = edge
    self.adj[u].append(edge)
    self.adj[v].append(redge)
    self.flow[edge] = 0
    self.flow[redge] = 0
 
  def FindPath(self, source, target, path):
    if source == target:
      return path
    for edge in self.GetEdges(source):
      residual = edge.capacity - self.flow[edge]
      if residual > 0 and not (edge, residual) in path:
        result = self.FindPath(edge.target, target, path + [(edge, residual)])
        if result != None:
          return result
 
  def MaxFlow(self, source, target):
    path = self.FindPath(source, target, [])
    while path != None:
      flow = min(res for edge, res in path)
      for edge, res in path:
        self.flow[edge] += flow
        self.flow[edge.redge] -= flow
      path = self.FindPath(source, target, [])
    #print(sum(self.flow[edge] for edge in self.GetEdges(source)))
    return self.flow
 
def readFile(filename):
    with open(filename,'r') as fl:
        k,l = [int(x) for x in fl.readline().split()]
        N = int(fl.readline().replace('\n',''))
        return ((k,l, N),[int(x) for x in fl.read().strip().split()][:-1])
 
def writeFile(filename, string):
    with open(filename,'w') as fl:
        fl.write(string)
 
def prepareInput(lst, k):
    mainShift = lst[0]
    shift, adj = lst[:k], lst[mainShift-1:]
    return list(map(lambda x: x-mainShift, shift)), adj
 
def createAdj(shift, adj):
    shift.reverse()
    res = []
    for x in shift:
        if x < 0 :
            res.append([])
            continue
        else:
            res.append(adj[x:len(adj)])
        adj = adj[:x]
    res.reverse()
    return res
 
def fillGraph(adjLst, k, l):
    gr = Graph()
    for x in range(k+1):
        gr.AddVertex(str(x))
 
    for x in range(l+1):
        gr.AddVertex(str(x)+ '*')
 
    for pos, element in enumerate(adjLst):
        for element__ in element:
            gr.AddEdge(str(pos + 1), str(element__) + '*', 1)
 
    for x in range(1, k+1):
        gr.AddEdge('0', str(x), 1)
    for x in range(1, l+1):
        gr.AddEdge(str(x)+'*', '0*', 1)
 
    return gr
 
def parseReply(dct, k):
    result = ['0' for x in range(k+1)]
   
    validRange = [str(x) for x in range(1, k+1)]
    for x in dct:
        if dct[x] == 1 and x.source in validRange:
           result[int(x.source)] = x.target[0]
    return result
 
def createForFile(lst):
    res = ''
    for x in range(1, len(lst)):
        res += lst[x] + ' '
    return res
           
def main():
    tup, data = readFile('in.txt')
    k, l, N = tup
 
    shift, adj = prepareInput(data, k)
    print(shift, adj)
    adjLst = createAdj(shift, adj)
    print(adjLst)
    gr = fillGraph(adjLst, k, l)
    value = gr.MaxFlow('0', '0*')
    resultLst = parseReply(value, k)
    resultString = createForFile(resultLst)
    writeFile('out.txt', resultString)
    #print(resultString)
     
if __name__ == '__main__': main()