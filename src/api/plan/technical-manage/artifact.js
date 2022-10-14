import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: 'artifact/listByCondition',
    method: 'get',
    params
  })
}

// 获取构件清单汇总
export function summary(params) {
  return request({
    module: 'plan',
    url: 'artifact/listByCondition/summary',
    method: 'get',
    params
  })
}

// 构件清单下载
export function downLoad(params) {
  return request({
    module: 'plan',
    url: `artifact/listByCondition/downLoad`,
    method: 'get',
    responseType: 'blob',
    timeout: 60000000,
    params
  })
}

export default { get, downLoad }
