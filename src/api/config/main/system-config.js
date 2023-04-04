import request from '@/utils/request'

/**
 * 获取公司信息
 */
export function getCompanyConfig(params) {
  return request({
    module: 'config',
    url: 'company',
    method: 'get',
    params
  })
}

/**
 * 设置公司信息
 */
export function setCompanyConfig(data) {
  return request({
    module: 'config',
    url: 'company',
    method: 'put',
    data
  })
}

/**
 * 获取项目信息
 */
export function getProjectConfig(params) {
  return request({
    module: 'config',
    url: 'project',
    method: 'get',
    params
  })
}

/**
 * 设置项目信息
 */
export function setProjectConfig(data) {
  return request({
    module: 'config',
    url: 'project',
    method: 'put',
    data
  })
}

/**
 * 获取公司logo列表
 */
export function getLogoConfig(params) {
  return request({
    module: 'config',
    url: 'company/logo',
    method: 'get',
    params
  })
}

/**
 * 获取公司logo列表（公共的，无需权限）
 */
export function getLogoConfigAll(params) {
  return request({
    module: 'config',
    url: 'company/logo/all',
    method: 'get',
    params
  })
}

/**
 * 设置公司默认logo
 */
export function setLogoConfig(id) {
  return request({
    module: 'config',
    url: `company/logo/default/${id}`,
    method: 'put'
  })
}

/**
 * 删除公司logo
 */
export function delLogoConfig(id) {
  return request({
    module: 'config',
    url: `company/logo/${id}`,
    method: 'delete'
  })
}
