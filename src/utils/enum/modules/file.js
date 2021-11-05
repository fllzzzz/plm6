import { constantize } from '../base'

// 文件分类
const fileClassificationEnum = {
  NORMAL: { L: '公共文件', K: 'OTHER', V: 1 },
  SECTION_ATT: { L: '型材导入附件', K: 'SECTION_ATT', V: 10 }
}

constantize(fileClassificationEnum)

export {
  fileClassificationEnum
}

export default {
  fileClassificationEnum
}
