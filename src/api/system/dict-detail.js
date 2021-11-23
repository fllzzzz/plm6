import request from '@/utils/request'

/**
 * 根据字典名称获取对应所有内容
 *
 * @export
 * @param {*} name
 * @returns
 */
export function getAll(name) {
  const params = {
    name
  }
  return request({
    module: 'system',
    url: 'dictDetail/all',
    method: 'get',
    params
  })
}

export function get(params) {
  return request({
    module: 'system',
    url: 'dictDetail',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'system',
    url: 'dictDetail',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'system',
    url: 'dictDetail',
    method: 'put',
    data
  })
}

export function del(id) {
  return request({
    module: 'system',
    url: 'dictDetail/' + id,
    method: 'delete'
  })
}

export default { add, edit, del, get }
