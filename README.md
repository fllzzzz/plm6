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

## 服务端

## WMS
1.物料信息由后端传递，而不由前端自行获取，即不再需要调用 setSpecInfoToList这类的方法
  （目前由于服务端暂无时间处理且没有缓存，因此将该功能坐在前端中）
  （若该功能后期不由后端处理，修改物料分类单位精度后，各个页面的物料单位精度会统一变化，而实际应照原来的显示。）
