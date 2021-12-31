// 注意modules文件夹下的文件（不相同的两个js）的export不能出现相同的名称

// 批量引入文件
const modulesFiles = import.meta.globEager('./modules/*.js')

const modules = {}

// 遍历文件
for (const key in modulesFiles) {
  modules[key.replace(/(\.\/modules\/|\.js)/g, '')] = modulesFiles[key].default
}

export default modules

