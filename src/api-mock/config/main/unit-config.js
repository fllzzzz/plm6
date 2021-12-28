import { enabledEnum, unitTypeEnum } from '@enum-ms/common'

// 获取所有单位
const getAllUnit = {
  url: '/api/config/base-config/unit/all',
  timeout: 500,
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: [
        { id: 1, name: '毫米', symbol: '㎜', type: unitTypeEnum.LENGTH.V, enabled: true },
        { id: 2, name: '米', symbol: 'm', type: unitTypeEnum.LENGTH.V, enabled: true },
        { id: 3, name: '平方米', symbol: '㎡', type: unitTypeEnum.AREA.V, enabled: true },
        { id: 4, name: '立方米', symbol: 'm³', type: unitTypeEnum.VOLUME.V, enabled: true },
        { id: 5, name: '千克', symbol: 'kg', type: unitTypeEnum.WEIGHT.V, enabled: true },
        { id: 6, name: '张', symbol: '', type: unitTypeEnum.DIGIT.V, enabled: true },
        { id: 7, name: '卷', symbol: '', type: unitTypeEnum.DIGIT.V, enabled: true },
        { id: 8, name: '桶', symbol: '', type: unitTypeEnum.DIGIT.V, enabled: true },
        { id: 9, name: '箱', symbol: '', type: unitTypeEnum.DIGIT.V, enabled: false }
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
        content: [
          { id: 1, name: '毫米', symbol: '㎜', type: unitTypeEnum.LENGTH.V, enabled: enabledEnum.TRUE.V, boolSystem: true },
          { id: 2, name: '米', symbol: 'm', type: unitTypeEnum.LENGTH.V, enabled: enabledEnum.TRUE.V, boolSystem: true },
          { id: 3, name: '平方米', symbol: '㎡', type: unitTypeEnum.AREA.V, enabled: enabledEnum.TRUE.V, boolSystem: true },
          { id: 4, name: '立方米', symbol: 'm³', type: unitTypeEnum.VOLUME.V, enabled: enabledEnum.FALSE.V, boolSystem: true },
          { id: 5, name: '千克', symbol: '㎏', type: unitTypeEnum.WEIGHT.V, enabled: enabledEnum.TRUE.V, boolSystem: true },
          { id: 6, name: '张', symbol: '', type: unitTypeEnum.DIGIT.V, enabled: enabledEnum.TRUE.V, boolSystem: false },
          { id: 7, name: '卷', symbol: '', type: unitTypeEnum.DIGIT.V, enabled: enabledEnum.TRUE.V, boolSystem: false },
          { id: 8, name: '桶', symbol: '', type: unitTypeEnum.DIGIT.V, enabled: enabledEnum.TRUE.V, boolSystem: false },
          { id: 9, name: '箱', symbol: '', type: unitTypeEnum.DIGIT.V, enabled: enabledEnum.TRUE.V, boolSystem: false }
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
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 修改启用状态
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

export default [getUnit, getAllUnit, batchAddUnit, editEnabled, delUnit]
