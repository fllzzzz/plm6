// 获取分类树
const getClassificationTree = {
  url: '/api/config/v1/classification/tree',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: []
      }
    }
  }
}

export default [
  getClassificationTree
]
