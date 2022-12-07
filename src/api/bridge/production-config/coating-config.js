import request from '@/utils/request'

// 涂装配置列表
export function get(params) {
  return request({
    module: 'bridge',
    url: 'coating',
    method: 'get',
    params
  })
}

export function edit(data) {
  return request({
    module: 'bridge',
    url: 'coating',
    method: 'put',
    data
  })
}

export default { get, edit }
