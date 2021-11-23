
// 未关闭的申购单
const getUnclosedRequisitionsBrief = {
  url: '/api/wms/requisitions/unclosed/brief',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '操作成功',
      data: {
        'content|10': [
          {
            'basicClass|1-16': 1,
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/,
            'projectId|+1': [undefined, 1, 2, 3, 4]
          }
        ]
      }
    }
  }
}

export default [
  getUnclosedRequisitionsBrief
]
