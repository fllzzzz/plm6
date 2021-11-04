const getInspection = {
  url: '/api/mes/building/inspectionTeam/page',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content': [
          {
            'boolDeleteEnum': false,
            'createTime': '2021-11-01T10:43:00',
            'factoryId': 3,
            'id': 1,
            'mesBuildingInspectionTeamUserLinkDTOList': [
              {
                'boolDeleteEnum': false,
                'createTime': '2021-11-01T10:43:00',
                'id': 1,
                'inspectionTeamId': 1,
                'updateTime': '2021-11-01T10:43:00',
                'userId': 1,
                'userName': '超级管理员',
                'version': 1
              }
            ],
            'processId': 1,
            'processName': '组立1',
            'productionLineId': 4,
            'updateTime': '2021-11-01T10:43:00',
            'userId': 1,
            'version': 1,
            'workshopId': 4
          },
          {
            'boolDeleteEnum': false,
            'createTime': '2021-11-01T10:44:31',
            'factoryId': 3,
            'id': 2,
            'mesBuildingInspectionTeamUserLinkDTOList': [
              {
                'boolDeleteEnum': false,
                'createTime': '2021-11-01T10:44:31',
                'id': 2,
                'inspectionTeamId': 2,
                'updateTime': '2021-11-01T10:44:31',
                'userId': 1,
                'userName': '超级管理员',
                'version': 1
              }
            ],
            'processId': 2,
            'processName': '组立2',
            'productionLineId': 4,
            'updateTime': '2021-11-01T10:44:31',
            'userId': 1,
            'version': 1,
            'workshopId': 4
          },
          {
            'boolDeleteEnum': false,
            'createTime': '2021-11-01T10:44:43',
            'factoryId': 3,
            'id': 3,
            'mesBuildingInspectionTeamUserLinkDTOList': [
              {
                'boolDeleteEnum': false,
                'createTime': '2021-11-01T10:44:43',
                'id': 3,
                'inspectionTeamId': 3,
                'updateTime': '2021-11-01T10:44:43',
                'userId': 69,
                'userName': '吕冰工人一',
                'version': 1
              },
              {
                'boolDeleteEnum': false,
                'createTime': '2021-11-01T10:44:43',
                'id': 4,
                'inspectionTeamId': 3,
                'updateTime': '2021-11-01T10:44:43',
                'userId': 1,
                'userName': '超级管理员',
                'version': 1
              }
            ],
            'processId': 3,
            'processName': '组立3',
            'productionLineId': 4,
            'updateTime': '2021-11-01T10:44:43',
            'userId': 1,
            'version': 1,
            'workshopId': 4
          },
          {
            'boolDeleteEnum': false,
            'createTime': '2021-11-01T10:44:50',
            'factoryId': 3,
            'id': 4,
            'mesBuildingInspectionTeamUserLinkDTOList': [
              {
                'boolDeleteEnum': false,
                'createTime': '2021-11-01T10:44:50',
                'id': 5,
                'inspectionTeamId': 4,
                'updateTime': '2021-11-01T10:44:50',
                'userId': 1,
                'userName': '超级管理员',
                'version': 1
              }
            ],
            'processId': 4,
            'processName': '组立4',
            'productionLineId': 4,
            'updateTime': '2021-11-01T10:44:50',
            'userId': 1,
            'version': 1,
            'workshopId': 4
          },
          {
            'boolDeleteEnum': false,
            'createTime': '2021-11-01T10:44:56',
            'factoryId': 3,
            'id': 5,
            'mesBuildingInspectionTeamUserLinkDTOList': [
              {
                'boolDeleteEnum': false,
                'createTime': '2021-11-01T10:44:56',
                'id': 6,
                'inspectionTeamId': 5,
                'updateTime': '2021-11-01T10:44:56',
                'userId': 1,
                'userName': '超级管理员',
                'version': 1
              }
            ],
            'processId': 5,
            'processName': '组立5',
            'productionLineId': 4,
            'updateTime': '2021-11-01T10:44:56',
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

const addInspection = {
  url: '/api/mes/building/inspectionTeam',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editInspection = {
  url: '/api/mes/building/inspectionTeam',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const delInspection = {
  url: '/api/mes/building/inspectionTeam',
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
  getInspection,
  addInspection,
  editInspection,
  delInspection
]
