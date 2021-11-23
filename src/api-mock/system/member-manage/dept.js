
const getDept = {
  url: '/api/dept',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'totalElements': 1,
        'content': [{
          'id': 1,
          'pid': 0,
          'name': '公司',
          'introduction': '公司顶级部门，不可修改，不可删除',
          'createTime': '2021-09-18T08:43:54.719+00:00',
          'updateTime': null,
          'children': [{
            'id': 2,
            'pid': 1,
            'name': '生产部',
            'introduction': null,
            'createTime': '2021-09-18T09:50:49.502+00:00',
            'updateTime': null,
            'children': []
          }]
        }]
      }
    }
  }
}

const addDept = {
  url: '/api/dept',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': 10
    }
  }
}

const editDept = {
  url: '/api/dept',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': 10
    }
  }
}

const delDept = {
  url: '/api/dept',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}
const getDeptTree = {
  url: '/api/dept/all/simple',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': [{
        'id': 1,
        'name': '公司',
        'children': [{
          'id': 2,
          'name': '生产部1',
          'children': []
        }, {
          'id': 3,
          'name': '业务部',
          'children': []
        }, {
          'id': 6,
          'name': '销售部',
          'children': []
        }, {
          'id': 7,
          'name': '仓库',
          'children': []
        }, {
          'id': 8,
          'name': '财务部',
          'children': []
        }, {
          'id': 9,
          'name': '1',
          'children': []
        }]
      }]
    }
  }
}
export default [
  getDept,
  addDept,
  editDept,
  delDept,
  getDeptTree
]
