import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: 'artifact/list-specAmend',
    method: 'get',
    params
  })
}

export function edit(data) {
  return request({
    module: 'plan',
    url: 'artifact/save-specAmend',
    method: 'put',
    data: data.list
  })
}

// 获取规格前缀配置
export function specConfig(params) {
  return request({
    module: '',
    url: '/api/system/structure-classification/list-specPrefix',
    method: 'get',
    params
  })
}

// 获取规格列表
export function getSpecList(params) {
  return request({
    module: '',
    url: '/api/plan/artifact/list-spec',
    method: 'get',
    params
  })
}
export default { get, edit }
