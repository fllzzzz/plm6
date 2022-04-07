import request from '@/utils/request'

// 按项目查排产
export function get(params) {
  return request({
    url: `/api/cut/getNestingList`,
    method: 'get',
    params
  })
}

export default {
  get
}
