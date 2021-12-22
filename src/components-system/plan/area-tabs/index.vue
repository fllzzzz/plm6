<!-- 区域：tab选择 -->
<template>
  <el-tabs v-model="id" :tab-position="props.tabPosition" @tab-click="tabClick">
    <el-tab-pane v-for="item in props.areaInfo" :key="item.id" :label="`${showType==1?item.name+'('+item.axis+')':item.name+'('+(item.type===1?'外包':'自制')+')'}`" :name="`${item.id}`" />
  </el-tabs>
</template>

<script setup>
import { ref, defineProps, defineEmits, watch } from 'vue'
import { ElTabs, ElTabPane } from 'element-plus'
import { isNotBlank } from '@data-type/index'

const id = ref()
const oldValue = ref()
const emit = defineEmits(['update:modelValue', 'tab-click'])

const props = defineProps({
  modelValue: {
    type: [Number, String],
    default: undefined
  },
  tabPosition: {
    type: String,
    default: 'top'
  },
  areaInfo: {
    type: Array,
    default: () => []
  },
  default: {
    type: Boolean,
    default: false
  },
  defaultTab: {
    type: Object,
    default: () => {}
  },
  showType: {
    type: Number,
    default: 1
  }
})

watch(
  () => props.defaultTab,
  (val) => {
    if (val) {
      id.value = val.id
      tabClick('default')
    }
  },
  { deep: true, immediate: true }
)

function tabClick(type) {
  let val = {}
  if (type === 'default') {
    val = props.defaultTab
  } else {
    val = props.areaInfo.find(v => v.id === Number(id.value))
  }
  if (isNotBlank(val) && oldValue.value !== val.id) {
    oldValue.value = val.id
    emit('update:modelValue', val.id)
    emit('tab-click', {
      name: val.id,
      label: val.name
    })
  }
}
</script>

<style lang="scss" scoped>
 .filter-item {
    ::v-deep(.el-tabs__header){
      margin: 0;
      user-select:none;
      .el-tabs__item {
        line-height: 20px;
        height: 28px;
        padding: 0 14px;
        font-size: 13px;
      }
      .el-tabs__nav-next, .el-tabs__nav-prev {
        line-height: 20px;
      }
      .is-top,.is-bottom,.is-top,.is-bottom{
        :nth-child(2){
          padding-left: 0;
        }
        :last-child {
          padding-right: 0;
        }
      }
    }
 }
</style>
