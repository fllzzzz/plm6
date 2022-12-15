# Vue 3 + Vite

This template should help get you started developing with Vue 3 in Vite. The template uses Vue 3 `<script setup>` SFCs, check out the [script setup docs](https://v3.vuejs.org/api/sfc-script-setup.html#sfc-script-setup) to learn more.

## Recommended IDE Setup

- [VSCode](https://code.visualstudio.com/) + [Volar](https://marketplace.visualstudio.com/items?itemName=johnsoncodehk.volar)

## author
柠檬泡泡冰

## 注意事项
Vue 3 中函数式组件的性能提升很小，可以直接使用常规组件

## scoped-styles-changes
/* deep selectors */
::v-deep(.foo) {}

/* targeting slot content */
::v-slotted(.foo) {}

/* one-off global rule */
::v-global(.foo) {}

## 系统待优化

## 未完成需求
1.钢材损耗率  总装第一道工序之后与出库做比较

## 可能需求
1.工厂选择，默认为当前用户所属工厂（涉及：用户工厂无工厂/有多个工厂的情况下的问题）

## 服务端

## WMS
1.物料信息由后端传递，而不由前端自行获取，即不再需要调用 setSpecInfoToList这类的方法
  （目前由于服务端暂无时间处理且没有缓存，因此将该功能坐在前端中）
  （若该功能后期不由后端处理，修改物料分类单位精度后，各个页面的物料单位精度会统一变化，而实际应照原来的显示。）

## 前端

## wms
1.领料提醒通知
2.完善物流订单

## 极端BUG 
代码build后，生产环境部署时入库办理时通过“采购合同编号选择组件” 查看 采购合同详情，导致"分配重量按钮"消失（异常报错，insertBefore）
PS: 
1.开发环境（本地运行）正常
2.两个组件之间无任何关联

原因：采购合同详情中使用了"上传列表组件（uploadList）",上传列表组件中的export-button组件的attr中的params使用了“props.files[index].id”,
      代码如下 ：
      <export-button
        v-show="props.showDownload"
        v-permission="props.downloadPerm"
        :params="{ ...props.downloadParams, id: props.files[scope.$index].id }"
        :fn="props.downloadFn"
      />
      错误1: props.files 可能为null
      错误2：element-plus的^1.1.0-beta.19版本中，table没数据时，也会调用此处代码，且index可能为-1，可运行查看

原因1 导致了vue插入dom出现问题（可能是diff算法问题）（具体为什么只有"分配重量按钮"出现问题，原因未知）
PS:
1.若分配重量按钮不使用v-if（即不插入），而使用v-show不会出现这样的问题
2.在上面情景的基础上，在“分配重量”所在的盒子中用v-if插入（兄弟节点插入），插入的顺序也会有问题）

如果想探知该问题，请切换到git的 cc60af13518a4a2bb58d426b56b24c62657c54f7 版本， 时间： 2021/12/29, author：DH

## PDF预览
1. 使用pdfjs-dist@2.5.207（vue-3.0中使用过高或者过低的版本，会报错）
2. `/public/assets/pdf` 内文件从 `/node_modules/pdfjs-dist/es5` 内拷贝过来
3. `/public/assets/pdf/build/pdf.js`做过如下修改：
   1. 24698行 `xhr.open("GET", this.url);` => `xhr.open("GET", this.url,true);`
4. PDF预览封装在`/src/components/PDF` 

## 打包内存溢出
1. 安装increase-memory-limit：npm install -g increase-memory-limit
2. package.json的同级目录下，执行：increase-memory-limit
3. npm run build：
  若报错`'"node --max-old-space-size=10240"' 不是内部或外部命令,也不是可运行的程序`,需要将`./node_modules/.bin/*.cmd`内所有 `"%_prog%"` 替换成 `%_prog%`后再打包


