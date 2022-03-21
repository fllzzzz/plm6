import request from '@/utils/request'

// 查看任务包
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
