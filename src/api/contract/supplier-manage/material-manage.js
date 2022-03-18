import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'project/pageProjectLedger',
    method: 'get',
    params
  })
}

export default { get }
