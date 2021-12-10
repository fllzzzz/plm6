import {
  validatorPhone
} from '@/utils/validate/pattern'
// 获取过磅超标短信接收人
const getOverweightSMSRecipient = {
  url: '/api/config/mes/overweightSMSRecipient',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'id|1-10': 1,
        'name': '@cname',
        'phone': validatorPhone,
        'maxWeight': null
      }
    }
  }
}

// 设置过磅超标短信接收人
const setOverweightSMSRecipient = {
  url: '/api/config/mes/overweightSMSRecipient',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}
// 获取司机信息填写配置
const getDriverConfig = {
  url: '/api/config/driver',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'configure|1-2': false
      }
    }
  }
}

// 获取司机信息填写配置
const setDriverConfig = {
  url: '/api/config/driver',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 获取车型配置
const getCarModelConfig = {
  url: '/api/config/car/model',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'carModels|1-5': ['string1', 'string2']
      }
    }
  }
}

// 设置车型配置
const setCarModelConfig = {
  url: '/api/config/car/model',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [
  getOverweightSMSRecipient,
  setOverweightSMSRecipient,
  getDriverConfig,
  setDriverConfig,
  getCarModelConfig,
  setCarModelConfig
]
