const getTeam = {
  url: '/api/mes/building/team/page',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content': [
          {
            'boolDeleteEnum': false,
            'createTime': '2021-11-01T10:38:09',
            'factoryId': 3,
            'id': 1,
            'mesBuildingTeamUserLinkList': [
              {
                'boolDeleteEnum': false,
                'boolLeaderEnum': true,
                'createTime': '2021-11-01T10:38:09',
                'id': 1,
                'teamId': 1,
                'updateTime': '2021-11-01T10:38:09',
                'userId': 1,
                'userName': '超级管理员',
                'version': 1
              },
              {
                'boolDeleteEnum': false,
                'boolLeaderEnum': false,
                'createTime': '2021-11-01T10:38:09',
                'id': 2,
                'teamId': 1,
                'updateTime': '2021-11-01T10:38:09',
                'userId': 3,
                'userName': '李冰冰',
                'version': 1
              }
            ],
            'organizationType': true,
            'processId': 1,
            'processName': '组立1',
            'teamId': 4,
            'updateTime': '2021-11-01T10:38:09',
            'userId': 1,
            'version': 1,
            'workshopId': 4
          },
          {
            'boolDeleteEnum': false,
            'createTime': '2021-11-01T10:38:23',
            'factoryId': 3,
            'id': 2,
            'mesBuildingTeamUserLinkList': [
              {
                'boolDeleteEnum': false,
                'boolLeaderEnum': true,
                'createTime': '2021-11-01T10:38:23',
                'id': 3,
                'teamId': 2,
                'updateTime': '2021-11-01T10:38:23',
                'userId': 1,
                'userName': '超级管理员',
                'version': 1
              },
              {
                'boolDeleteEnum': false,
                'boolLeaderEnum': false,
                'createTime': '2021-11-01T10:38:23',
                'id': 4,
                'teamId': 2,
                'updateTime': '2021-11-01T10:38:23',
                'userId': 6,
                'userName': '川建国',
                'version': 1
              }
            ],
            'organizationType': true,
            'processId': 2,
            'processName': '组立2',
            'teamId': 4,
            'updateTime': '2021-11-01T10:38:23',
            'userId': 1,
            'version': 1,
            'workshopId': 4
          },
          {
            'boolDeleteEnum': false,
            'createTime': '2021-11-01T10:38:36',
            'factoryId': 3,
            'id': 3,
            'mesBuildingTeamUserLinkList': [
              {
                'boolDeleteEnum': false,
                'boolLeaderEnum': true,
                'createTime': '2021-11-01T10:38:36',
                'id': 5,
                'teamId': 3,
                'updateTime': '2021-11-01T10:38:36',
                'userId': 1,
                'userName': '超级管理员',
                'version': 1
              },
              {
                'boolDeleteEnum': false,
                'boolLeaderEnum': false,
                'createTime': '2021-11-01T10:38:36',
                'id': 6,
                'teamId': 3,
                'updateTime': '2021-11-01T10:38:36',
                'userId': 7,
                'userName': '小强',
                'version': 1
              }
            ],
            'organizationType': true,
            'processId': 3,
            'processName': '组立3',
            'teamId': 4,
            'updateTime': '2021-11-01T10:38:36',
            'userId': 1,
            'version': 1,
            'workshopId': 4
          },
          {
            'boolDeleteEnum': false,
            'createTime': '2021-11-01T10:39:16',
            'factoryId': 3,
            'id': 6,
            'mesBuildingTeamUserLinkList': [
              {
                'boolDeleteEnum': false,
                'boolLeaderEnum': true,
                'createTime': '2021-11-01T10:39:16',
                'id': 9,
                'teamId': 6,
                'updateTime': '2021-11-01T10:39:16',
                'userId': 1,
                'userName': '超级管理员',
                'version': 1
              },
              {
                'boolDeleteEnum': false,
                'boolLeaderEnum': false,
                'createTime': '2021-11-01T10:39:16',
                'id': 10,
                'teamId': 6,
                'updateTime': '2021-11-01T10:39:16',
                'userId': 7,
                'userName': '小强',
                'version': 1
              }
            ],
            'organizationType': true,
            'processId': 5,
            'processName': '组立5',
            'teamId': 4,
            'updateTime': '2021-11-01T10:39:16',
            'userId': 1,
            'version': 1,
            'workshopId': 4
          },
          {
            'boolDeleteEnum': false,
            'createTime': '2021-11-01T10:39:32',
            'factoryId': 3,
            'id': 7,
            'mesBuildingTeamUserLinkList': [
              {
                'boolDeleteEnum': false,
                'boolLeaderEnum': true,
                'createTime': '2021-11-01T10:39:32',
                'id': 11,
                'teamId': 7,
                'updateTime': '2021-11-01T10:39:32',
                'userId': 1,
                'userName': '超级管理员',
                'version': 1
              },
              {
                'boolDeleteEnum': false,
                'boolLeaderEnum': false,
                'createTime': '2021-11-01T10:39:32',
                'id': 12,
                'teamId': 7,
                'updateTime': '2021-11-01T10:39:32',
                'userId': 8,
                'userName': '小张',
                'version': 1
              }
            ],
            'organizationType': true,
            'processId': 11,
            'processName': '构件1',
            'teamId': 4,
            'updateTime': '2021-11-01T10:39:32',
            'userId': 1,
            'version': 1,
            'workshopId': 4
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
