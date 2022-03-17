import request from '@/utils/request'

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