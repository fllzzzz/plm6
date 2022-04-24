import { constantize } from '../base'

// 性别
const userSexEnum = {
  MALE: { L: '男', K: 'MALE', V: 0 },
  FEMALE: { L: '女', K: 'FEMALE', V: 1 }
}
constantize(userSexEnum)

export {
  userSexEnum
}

export default {
  userSexEnum
}
