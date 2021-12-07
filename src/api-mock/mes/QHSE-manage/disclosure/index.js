const getQHSE = {
  url: '/api/mes/building/qhse/page',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 2,
        'content|1-50': [{
          'project': {
            'id': 1,
            'name': '@cword(2,15)',
            'shortName': '@cword(2,9)',
            'contractNo': '@guid'
          },
          'type|1-10': 1,
          'applicantName': '@cname',
          'createTime': '@datetime',
          'description': '@cword(2,60)',
          'responsibleUserName': '@cname',
          'imgUrls|1-5': ['@image(`200x100`,`@color()`)'],
          'imgs|1-5': ['@image(`200x200`,`@color()`)'],
          'tinyImageUrl|1-5': ['@image(`200x50`,`@color()`)'],
          'tinyImgs|1-5': ['@image(`200x50`,`@color()`)'],
          'status|1': [1, 2]
        }]
      }
    }
  }
}

export default [
  getQHSE
]
