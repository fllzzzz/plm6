const getTeam = {
  url: '/api/mes/building/team/page',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content|1-100': [
          {
            'boolDeleteEnum|1-2': false,
            'createTime': '@datetime',
            'factoryId|1-10': 1,
            'id|+1': 1,
            'userLinkList': [
              {
                'boolDeleteEnum': false,
                'boolLeaderEnum': true,
                'createTime': '@datetime',
                'id': 1,
                'teamId': 1,
                'updateTime': '@datetime',
                'userId': 4,
                'userName': '@cname',
                'version': 1
              },
              {
                'boolDeleteEnum': false,
                'boolLeaderEnum': false,
                'createTime': '@datetime',
                'id': 2,
                'teamId': 1,
                'updateTime': '@datetime',
                'userId': 3,
                'userName': '@cname',
                'version': 1
              }
            ],
            'organizationType|1-2': true,
            'processId|1-10': 1,
            'processName': '@cword(2,5)',
            'teamId|1-10': 1,
            'updateTime': '@datetime',
            'userId': 1,
            'version': 1,
            'workshopId|1-10': 1
          }
        ],
        'hasNextPage': false,
        'hasPreviousPage': false,
        'totalElements': 5
      },
      'message': '成功'
    }
  }
}

const addTeam = {
  url: '/api/mes/building/team',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editTeam = {
  url: '/api/mes/building/team',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const delTeam = {
  url: '/api/mes/building/team',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

export default [
  getTeam,
  addTeam,
  editTeam,
  delTeam
]
