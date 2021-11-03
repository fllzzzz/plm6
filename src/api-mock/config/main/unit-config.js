import { enabledEnum, unitTypeEnum } from '@enum-ms/common'

// 获取所有单位
const getAllUnit = {
  url: '/api/config/base-config/unit/all',
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: [
        { id: 1, 'name': '毫米', 'symbol': '㎜', 'type': unitTypeEnum.LENGTH.V },
        { id: 2, 'name': '米', 'symbol': 'm', 'type': unitTypeEnum.LENGTH.V },
        { id: 3, 'name': '平方米', 'symbol': '㎡', 'type': unitTypeEnum.AREA.V },
        { id: 4, 'name': '立方米', 'symbol': 'm³', 'type': unitTypeEnum.VOLUME.V },
        { id: 5, 'name': '千克', 'symbol': '㎏', 'type': unitTypeEnum.WEIGHT.V },
        { id: 6, 'name': '张', 'symbol': '', 'type': unitTypeEnum.DIGIT.V },
        { id: 7, 'name': '卷', 'symbol': '', 'type': unitTypeEnum.DIGIT.V },
        { id: 8, 'name': '桶', 'symbol': '', 'type': unitTypeEnum.DIGIT.V },
        { id: 9, 'name': '箱', 'symbol': '', 'type': unitTypeEnum.DIGIT.V }
      ]
    }
  }
}

const getUnit = {
  url: '/api/config/base-config/unit',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'content': [
          { id: 1, 'name': '毫米', 'symbol': '㎜', 'type': unitTypeEnum.LENGTH.V, 'enabled': enabledEnum.TRUE.V, boolSystem: true },
          { id: 2, 'name': '米', 'symbol': 'm', 'type': unitTypeEnum.LENGTH.V, 'enabled': enabledEnum.TRUE.V, boolSystem: true },
          { id: 3, 'name': '平方米', 'symbol': '㎡', 'type': unitTypeEnum.AREA.V, 'enabled': enabledEnum.TRUE.V, boolSystem: true },
          { id: 4, 'name': '立方米', 'symbol': 'm³', 'type': unitTypeEnum.VOLUME.V, 'enabled': enabledEnum.FALSE.V, boolSystem: true },
          { id: 5, 'name': '千克', 'symbol': '㎏', 'type': unitTypeEnum.WEIGHT.V, 'enabled': enabledEnum.TRUE.V, boolSystem: true },
          { id: 6, 'name': '张', 'symbol': '', 'type': unitTypeEnum.DIGIT.V, 'enabled': enabledEnum.TRUE.V, boolSystem: false },
          { id: 7, 'name': '卷', 'symbol': '', 'type': unitTypeEnum.DIGIT.V, 'enabled': enabledEnum.TRUE.V, boolSystem: false },
          { id: 8, 'name': '桶', 'symbol': '', 'type': unitTypeEnum.DIGIT.V, 'enabled': enabledEnum.TRUE.V, boolSystem: false },
          { id: 9, 'name': '箱', 'symbol': '', 'type': unitTypeEnum.DIGIT.V, 'enabled': enabledEnum.TRUE.V, boolSystem: false }
        ],
        totalElements: 9
      }
    }
  }
}

// 批量添加单位
const batchAddUnit = {
  url: '/api/config/base-config/unit/batch',
  method: 'post',
  timeout: 5000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 修改单位
const editEnabled = {
  url: '/api/config/base-config/unit/enabled',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 删除单位
const delUnit = {
  url: '/api/config/base-config/unit',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [
  getUnit,
  getAllUnit,
  batchAddUnit,
  editEnabled,
  delUnit
]
