import { constantize } from '../base'

// 系统-菜单-类别
// const systemMenusCategoryEnum = {
//   PC: 0, // PC端
//   APP: 1 // 移动端
// }
const systemMenusCategoryEnum = {
  PC: { L: 'PC', K: 'PC', V: 0 },
  APP: { L: 'APP', K: 'APP', V: 1 }
}
constantize(systemMenusCategoryEnum)

// 使用状态
const systemEnabledEnum = {
  TRUE: { L: '启用', K: 'TRUE', V: 1 },
  FALSE: { L: '禁用', K: 'FALSE', V: 0 }
}
constantize(systemEnabledEnum)

// 性别
const userSexEnum = {
  MALE: { L: '男', K: 'MALE', V: 0 },
  FEMALE: { L: '女', K: 'FEMALE', V: 1 }
}
constantize(userSexEnum)

// 系统-菜单-类型
const systemMenusTypeEnum = {
  MODULE: { L: '模块', K: 'MODULE', V: 0 },
  MENU: { L: '菜单', K: 'MENU', V: 1 },
  BUTTON: { L: '按钮', K: 'BUTTON', V: 2 }
}
constantize(systemMenusTypeEnum)

export {
  userSexEnum,
  systemEnabledEnum,
  systemMenusCategoryEnum, // 系统-菜单-类别
  systemMenusTypeEnum // 系统-菜单-类型
}

export default {
  userSexEnum,
  systemEnabledEnum,
  systemMenusCategoryEnum, // 系统-菜单-类别
  systemMenusTypeEnum // 系统-菜单-类型
}
