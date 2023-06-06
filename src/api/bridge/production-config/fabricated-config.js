import request from '@/utils/request'

// 涂装配置列表
export function get(params) {
  return request({
    module: 'bridge',
    url: 'fabricated',
    method: 'get',
    params
  })
}

export function edit(data) {
  return request({
    module: 'bridge',
    url: 'fabricated',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'bridge',
    url: 'fabricated',
    method: 'delete',
    data: {
      auxiliaryId: ids[0]
    }
  })
}

export default { get, edit, del }
