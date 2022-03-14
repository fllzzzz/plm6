// --------------------------- 公共权限 start ------------------------------

export const commonPM = {
}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 人员管理 start ------------------------------

// 用户配置
export const userConfigPM = {
  get: ['user:get'], // 列表
  add: ['user:add'], // 添加
  edit: ['user:edit'], // 编辑
  del: ['user:del'] // 删除
}

// 部门配置
export const deptConfigPM = {
  get: ['dept:get'], // 列表
  add: ['dept:add'], // 添加
  edit: ['dept:edit'], // 编辑
  del: ['dept:del'] // 删除
}

// 岗位配置
export const jobConfigPM = {
  get: ['job:get'], // 列表
  add: ['job:add'], // 添加
  edit: ['job:edit'], // 编辑
  del: ['job:del'] // 删除
}

// 角色配置
export const roleConfigPM = {
  get: ['role:get'], // 列表
  add: ['role:add'], // 添加
  edit: ['role:edit'], // 编辑
  del: ['role:del'] // 删除
}

// --------------------------- 人员管理 end --------------------------------
