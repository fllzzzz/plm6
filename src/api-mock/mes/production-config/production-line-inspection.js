const getInspection = {
  url: '/api/mes/building/inspectionTeam/page',
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
            'mesBuildingInspectionTeamUserLinkDTOList|1-3': [
              {
                'boolDeleteEnum|1-2': false,
                'createTime': '@datetime',
                'id|+1': 1,
                'inspectionTeamId': 1,
                'updateTime': '@datetime',
                'userId': 1,
                'userName': '@cname',
                'version': 1
              }
            ],
            'processId': 1,
            'processName': '@cword(2,5)',
            'productionLineId|1-10': 1,
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
