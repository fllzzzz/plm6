<!-- 车间：tab选择 -->
<template>
  <el-skeleton :loading="!loaded" :rows="1" animated>
    <template #template>
      <el-skeleton-item variant="rect" style="margin-top: 10px; height: 30px; margin-bottom: 10px" />
    </template>
    <template #default>
      <el-tabs v-if="isNotBlank(workshops)" v-model="workshopId" :tab-position="props.tabPosition" :type="props.type" @tab-click="tabClick">
        <el-tab-pane v-for="item in workshops" :key="item.id" :label="`${item.name}`" :name="`${item.id}`" />
        <slot name="content" />
      </el-tabs>
      <el-tag v-else type="danger" style="margin:15px 0">* 暂无车间, 请添加车间后再添加仓库</el-tag>
    </template>
  </el-skeleton>
</template>

<script setup>
import { defineEmits, defineProps, ref } from 'vue'
import { isNotBlank } from '@data-type/index'

import useWorkshop from '@compos/store/use-workshops'
import { ElTabs, ElTabPane } from 'element-plus'

const emit = defineEmits(['update:modelValue', 'tab-click'])

const props = defineProps({
  modelValue: {
    type: Number
  },
  tabPosition: {
    type: String,
    default: 'top'
  },
  type: {
    type: String,
    default: ''
  }
})

const workshopId = ref()
let workshopsMap = new Map()

const { loaded, workshops } = useWorkshop(dataFormat)

function tabClick() {
  const workshop = workshopsMap.get(workshopId.value)
  emit('update:modelValue', workshop.id)
  emit('tab-click', {
    id: workshop.id,
    name: workshop.name
  })
}

function dataFormat() {
  workshopsMap = new Map()
  try {
    if (isNotBlank(workshops.value)) {
      // key 使用字符串，因为tab的v-model是字符串类型
      workshops.value.forEach(v => {
        workshopsMap.set(`${v.id}`, v)
      })
      workshopId.value = `${workshops.value[0].id}`

      tabClick({
        id: workshopId.value,
        name: `${workshops.value[0].name}`
      })
    }
  } catch (error) {
    console.log('获取车间', error)
  }
}
</script>
