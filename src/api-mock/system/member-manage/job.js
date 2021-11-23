
const getJob = {
  url: '/api/job',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content': [{
          'createTime': 1592448346497,
          'deptId': 1,
          'deptName': '初鸣建科',
          'deptSuperiorName': null,
          'enabled': 1,
          'id': 1,
          'name': '超级管理员',
          'sort': 1,
          'updateTime': 1592448346497
        }, {
          'createTime': 1592455595391,
          'deptId': 3,
          'deptName': '技术部',
          'deptSuperiorName': null,
          'enabled': 1,
          'id': 30,
          'name': '部长',
          'sort': 1,
          'updateTime': 1592455595391
        }, {
          'createTime': 1599106518646,
          'deptId': 25,
          'deptName': '桥梁生产车间',
          'deptSuperiorName': null,
          'enabled': 1,
          'id': 33,
          'name': '箱体生产',
          'sort': 1,
          'updateTime': 1599106518646
        }, {
          'createTime': 1599106531369,
          'deptId': 25,
          'deptName': '桥梁生产车间',
          'deptSuperiorName': null,
          'enabled': 1,
          'id': 34,
          'name': '单元件生产',
          'sort': 1,
          'updateTime': 1599106531369
        }, {
          'createTime': 1599106790886,
          'deptId': 25,
          'deptName': '桥梁生产车间',
          'deptSuperiorName': null,
          'enabled': 1,
          'id': 35,
          'name': '零件生产',
          'sort': 1,
          'updateTime': 1599106790886
        }],
        'endRow': 5,
        'firstPage': true,
        'hasNextPage': false,
        'hasPreviousPage': false,
        'lastPage': true,
        'navigateFirstPage': 1,
        'navigateLastPage': 1,
        'navigatePageNums': [1],
        'navigatePages': 8,
        'nextPage': 0,
        'pageNum': 1,
        'pageSize': 50,
        'pages': 1,
        'prePage': 0,
        'size': 5,
        'startRow': 1,
        'totalElements': 5
      },
      'message': '操作成功'
    }
  }
}

const addJob = {
  url: '/api/job',
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

const editJob = {
  url: '/api/job',
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

const delJob = {
  url: '/api/job',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}
const editJobStatus = {
  url: '/api/job/enabled',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功'
    }
  }
}

const jobAll = {
  url: '/api/job/all',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content': [{
          'createTime': 1592448346497,
          'deptId': 1,
          'deptName': '初鸣建科',
          'deptSuperiorName': null,
          'enabled': 1,
          'id': 1,
          'name': '超级管理员',
          'sort': 1,
          'updateTime': 1592448346497
        }, {
          'createTime': 1592455595391,
          'deptId': 3,
          'deptName': '技术部',
          'deptSuperiorName': null,
          'enabled': 1,
          'id': 30,
          'name': '部长',
          'sort': 1,
          'updateTime': 1592455595391
        }, {
          'createTime': 1599106518646,
          'deptId': 25,
          'deptName': '桥梁生产车间',
          'deptSuperiorName': null,
          'enabled': 1,
          'id': 33,
          'name': '箱体生产',
          'sort': 1,
          'updateTime': 1599106518646
        }, {
          'createTime': 1599106531369,
          'deptId': 25,
          'deptName': '桥梁生产车间',
          'deptSuperiorName': null,
          'enabled': 1,
          'id': 34,
          'name': '单元件生产',
          'sort': 1,
          'updateTime': 1599106531369
        }, {
          'createTime': 1599106790886,
          'deptId': 25,
          'deptName': '桥梁生产车间',
          'deptSuperiorName': null,
          'enabled': 1,
          'id': 35,
          'name': '零件生产',
          'sort': 1,
          'updateTime': 1599106790886
        }],
        'totalElements': 5
      },
      'message': '操作成功'
    }
  }
}
export default [
  getJob,
  addJob,
  editJob,
  delJob,
  editJobStatus,
  jobAll
]
