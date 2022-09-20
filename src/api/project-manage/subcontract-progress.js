import request from '@/utils/request'

// 获取分包商具体任务
export function get(params) {
  return request({
    module: 'project',
    url: `install-plan/list-supplier-install-product-sum`,
    method: 'get',
    params
  })
}

export default { get }
