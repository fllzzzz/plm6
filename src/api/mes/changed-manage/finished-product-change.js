import request from '@/utils/request'

export function get(params) {
  return request({
    url: 'api/plan/artifact/change/warehousing',
    method: 'get',
    params
  })
}

export function componentChangeDownload(params) {
  return request({
    url: '/api/plan/artifact/change/warehousing/download',
    method: 'get',
    responseType: 'blob',
    params
  })
}

export default { get }
