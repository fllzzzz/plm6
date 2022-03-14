
const getUser = {
  url: '/api/user',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 6,
        'content': [{
          'id': 3,
          'username': 'A0002',
          'name': '李冰冰',
          'sex': 0,
          'phone': '13100000001',
          'email': '',
          'password': null,
          'createTime': '2020-06-18T09:48:11.000+00:00',
          'lastPasswordResetDate': '2020-06-18T09:48:10.819+00:00',
          'roles': [{
            'createTime': 1631790427000,
            'id': 1,
            'is_default': true,
            'name': '超级管理员',
            'permission': 'admin',
            'remark': null
          }],
          'dept': {
            'createTime': 1592448346484,
            'id': 1,
            'introduction': '公司顶级部门，不可修改，不可删除',
            'name': '初鸣建科',
            'pid': 0,
            'updateTime': 1615385877633
          },
          'deptId': 1,
          'job': {
            'createTime': 1592448346497,
            'deptId': 1,
            'enabled': 1,
            'id': 1,
            'isAdmin': 1,
            'name': '超级管理员',
            'sort': 1,
            'updateTime': 1592448346497
          },
          'enabled': true
        }, {
          'id': 4,
          'username': 'A0003',
          'name': '王中磊',
          'sex': 0,
          'phone': '13100000002',
          'email': null,
          'password': null,
          'createTime': '2020-06-18T09:49:35.000+00:00',
          'lastPasswordResetDate': '2020-06-18T09:49:34.853+00:00',
          'roles': [{
            'createTime': 1631790427000,
            'id': 1,
            'is_default': true,
            'name': '超级管理员',
            'permission': 'admin',
            'remark': null
          }],
          'dept': null,
          'deptId': 18,
          'job': null,
          'enabled': true
        }, {
          'id': 5,
          'username': 'A0004',
          'name': '冯小刚',
          'sex': 0,
          'phone': '13100000003',
          'email': null,
          'password': null,
          'createTime': '2020-06-18T09:50:22.000+00:00',
          'lastPasswordResetDate': '2020-06-18T09:50:21.820+00:00',
          'roles': [{
            'createTime': 1631790427000,
            'id': 1,
            'is_default': true,
            'name': '超级管理员',
            'permission': 'admin',
            'remark': null
          }],
          'dept': null,
          'deptId': 18,
          'job': null,
          'enabled': true
        }, {
          'id': 6,
          'username': 'A0005',
          'name': '川建国',
          'sex': 0,
          'phone': '13100000004',
          'email': '',
          'password': null,
          'createTime': '2020-06-18T09:51:27.000+00:00',
          'lastPasswordResetDate': '2020-06-18T09:51:26.726+00:00',
          'roles': [{
            'createTime': 1631790427000,
            'id': 1,
            'is_default': true,
            'name': '超级管理员',
            'permission': 'admin',
            'remark': null
          }],
          'dept': null,
          'deptId': 19,
          'job': null,
          'enabled': true
        }, {
          'id': 7,
          'username': 'A0006',
          'name': '小强',
          'sex': 0,
          'phone': '13100000005',
          'email': null,
          'password': null,
          'createTime': '2020-06-18T09:52:20.000+00:00',
          'lastPasswordResetDate': '2020-06-18T09:52:19.633+00:00',
          'roles': [{
            'createTime': 1631790427000,
            'id': 1,
            'is_default': true,
            'name': '超级管理员',
            'permission': 'admin',
            'remark': null
          }],
          'dept': null,
          'deptId': 19,
          'job': null,
          'enabled': true
        }, {
          'id': 8,
          'username': 'A007',
          'name': '小张',
          'sex': 0,
          'phone': '13100000006',
          'email': null,
          'password': null,
          'createTime': '2020-06-18T09:53:18.000+00:00',
          'lastPasswordResetDate': '2020-06-18T09:53:18.241+00:00',
          'roles': [{
            'createTime': 1631790427000,
            'id': 1,
            'is_default': true,
            'name': '超级管理员',
            'permission': 'admin',
            'remark': null
          }],
          'dept': null,
          'deptId': 19,
          'job': null,
          'enabled': true
        }]
      }
    }
  }
}

const addUser = {
  url: '/api/user',
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

const editUser = {
  url: '/api/user',
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

const delUser = {
  url: '/api/user',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editUserStatus = {
  url: '/api/user/enabled',
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
export default [
  getUser,
  addUser,
  editUser,
  delUser,
  editUserStatus
]
