import { projectTypeEnum, projectStatusEnum } from '@/utils/enum/modules/contract'

// 获取所有项目（简要信息）
const allProjectSimple = {
  url: '/api/contract/v1/project/simple',
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'content': [
          {
            'contractNo': '@guid',
            'createTime': '@datetime(T)',
            'id': 1,
            'name': '牙牙乐生产工厂',
            'projectType': projectTypeEnum.STEEL.V,
            'shortName': '牙牙乐',
            'status': projectStatusEnum.PROCESS.V
          },
          {
            'contractNo': '@guid',
            'createTime': '@datetime(T)',
            'id': 2,
            'name': '洛阳市王城大道快速路与火车站北广场连接线工程施工第二标段钢箱梁',
            'projectType': projectTypeEnum.BRIDGE.V,
            'shortName': '北广场制作安装站',
            'status': projectStatusEnum.PROCESS.V
          },
          {
            'contractNo': '@guid',
            'createTime': '@datetime(T)',
            'id': 3,
            'name': '石家庄美的电器加工车间',
            'projectType': projectTypeEnum.ENCLOSURE.V,
            'shortName': '美的电器加工车间',
            'status': projectStatusEnum.PROCESS.V
          }
        ],
        totalElements: 3
      }
    }
  }
}

// 获取用户的所有项目（简要信息）
const projectSimple = {
  url: '/api/contract/v1/user/project/simple',
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'content': [
          {
            'contractNo': '@guid',
            'createTime': '@datetime(T)',
            'id': 1,
            'name': '牙牙乐生产工厂',
            'projectType': projectTypeEnum.STEEL.V,
            'shortName': '牙牙乐',
            'status': projectStatusEnum.PROCESS.V
          },
          {
            'contractNo': '@guid',
            'createTime': '@datetime(T)',
            'id': 2,
            'name': '洛阳市王城大道快速路与火车站北广场连接线工程施工第二标段钢箱梁',
            'projectType': projectTypeEnum.BRIDGE.V,
            'shortName': '北广场制作安装站',
            'status': projectStatusEnum.PROCESS.V
          },
          {
            'contractNo': '@guid',
            'createTime': '@datetime(T)',
            'id': 3,
            'name': '石家庄美的电器加工车间',
            'projectType': projectTypeEnum.ENCLOSURE.V,
            'shortName': '美的电器加工车间',
            'status': projectStatusEnum.SUSPEND.V
          }
        ],
        totalElements: 3
      }
    }
  }
}

export default [
  projectSimple,
  allProjectSimple
]
